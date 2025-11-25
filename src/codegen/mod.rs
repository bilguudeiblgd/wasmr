mod type_mapping;
mod local_context;
mod wasi;
mod var_collector;
mod binary_ops;
mod expressions;
mod builtins;
mod statements;
pub mod io;

// Re-export public API
pub use io::{compile_to_wasm, compile_to_wasm_ir, wasm_to_wat, write_wasm_file, write_wat_file, compile_and_write, compile_and_write_ir};

use crate::ast::{BinaryOp, Param, ParamKind, Stmt as AstStmt, Type};
use local_context::LocalContext;
use crate::ir::{BuiltinKind, IR, IRExpr, IRExprKind, IRStmt as Stmt, TypeResolver};
use std::collections::HashMap;
use wasm_encoder::{BlockType, CodeSection, DataSection, ExportKind, ExportSection, Function, FunctionSection, HeapType, Instruction, ImportSection, MemorySection, MemoryType, Module, RefType, StorageType, TypeSection, ValType, EntityType};
use wasm_encoder::Instruction::BrIf;
use wasmtime::StorageType::ValType as StoreValType;
use crate::ir::IRExprKind::{Binary, VectorLiteral};

pub struct WasmGenerator {
    module: Module,
    pub(crate) types: TypeSection,
    imports: ImportSection,
    pub(crate) functions: FunctionSection,
    memory: MemorySection,
    exports: ExportSection,
    data: DataSection,
    pub(crate) code: CodeSection,
    pub(crate) func_indices: HashMap<String, u32>,
    pub(crate) func_count: u32,
    fd_write_idx: Option<u32>,
    array_type_i32: Option<u32>,
    array_type_f32: Option<u32>,
    array_type_f64: Option<u32>,
    array_type_anyref: Option<u32>,
}

impl WasmGenerator {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            types: TypeSection::new(),
            imports: ImportSection::new(),
            functions: FunctionSection::new(),
            memory: MemorySection::new(),
            exports: ExportSection::new(),
            data: DataSection::new(),
            code: CodeSection::new(),
            func_indices: HashMap::new(),
            func_count: 0,
            fd_write_idx: None,
            array_type_i32: None,
            array_type_f32: None,
            array_type_f64: None,
            array_type_anyref: None,
        }
    }

    pub fn compile_program(&mut self, program: Vec<Stmt>) -> Vec<u8> {
        // Setup WASI imports (fd_write) and memory
        self.setup_wasi_imports();

        // Split into function defs and top-level statements
        self.include_builtins();

        let mut functions: Vec<(String, Vec<Param>, Type, Vec<Stmt>)> = Vec::new();
        let mut top_level: Vec<Stmt> = Vec::new();
        for s in &program {
            if let Stmt::FunctionDef {
                name,
                params,
                return_type,
                body,
            } = s
            {
                functions.push((
                    name.clone(),
                    params.clone(),
                    return_type.clone(),
                    body.clone(),
                ));
            } else {
                top_level.push(s.clone());
            }
        }

        // First pass: declare all function types
        for (name, params, ret_ty, _body) in &functions {
            let param_tys: Vec<ValType> = params.iter().map( |p|self.wasm_param_valtype(p)).collect();
            let result_tys: Vec<ValType> = if *ret_ty != Type::Void {
                vec![self.wasm_valtype(ret_ty)]
            } else {
                vec![]
            };
            let type_index = self.types.len();
            {
                let ty = self.types.ty();
                ty.function(param_tys, result_tys);
            }

            self.functions.function(type_index as u32);
            let idx = self.func_count;
            self.func_indices.insert(name.clone(), idx);
            self.func_count += 1;
        }

        // If there are top-level statements, synthesize a main: () -> i32
        let mut synthetic_main_index: Option<u32> = None;
        if !top_level.is_empty() {
            let type_index = self.types.len();
            {
                let ty = self.types.ty();
                ty.function(vec![], vec![]);
            }
            self.functions.function(type_index as u32);
            let idx = self.func_count;
            self.func_indices.insert("_start".to_string(), idx);
            self.func_count += 1;
            synthetic_main_index = Some(idx);
        }

        // Second pass: define function bodies
        for (_name, params, ret_ty, body) in &functions {
            let ret_has_value = *ret_ty != Type::Void;

            // Collect local variables
            let vars = self.collect_vars(body);
            let local_types: Vec<(u32, ValType)> = vars
                .iter()
                .map(|(_, ty)| (1, self.wasm_valtype(ty)))
                .collect();

            let mut f = Function::new(local_types);

            // Build local context
            let mut ctx = LocalContext::new(params.clone());
            let param_count = params.len() as u32;
            for (i, (var_name, _)) in vars.iter().enumerate() {
                ctx.add_local(var_name.clone(), param_count + i as u32);
            }

            // Generate statements
            for stmt in body {
                self.gen_stmt(&mut f, &ctx, stmt, ret_has_value);
            }

            f.instruction(&Instruction::End);
            self.code.function(&f);
        }

        // Generate synthetic main if needed
        if let Some(_idx) = synthetic_main_index {
            // Collect variables from top-level statements
            let vars = self.collect_vars(&top_level);
            let local_types: Vec<(u32, ValType)> = vars
                .iter()
                .map(|(_, ty)| (1, self.wasm_valtype(ty)))
                .collect();

            let mut f = Function::new(local_types);

            // Build local context (no params for main)
            let mut ctx = LocalContext::new(vec![]);
            for (i, (var_name, _)) in vars.iter().enumerate() {
                ctx.add_local(var_name.clone(), i as u32);
            }

            let last_idx = top_level.len().saturating_sub(1);
            for (i, stmt) in top_level.iter().enumerate() {
                self.gen_stmt(&mut f, &ctx, stmt, true);
            }

            if top_level.is_empty() {
                f.instruction(&Instruction::I32Const(0));
            }
            f.instruction(&Instruction::End);
            self.code.function(&f);
        }

        // Export memory for WASI
        self.exports.export("memory", ExportKind::Memory, 0);

        // Export functions
        for (name, _p, _r, _b) in &functions {
            if let Some(&idx) = self.func_indices.get(name) {
                self.exports.export(name, ExportKind::Func, idx);
            }
        }
        if let Some(idx) = synthetic_main_index {
            self.exports.export("_start", ExportKind::Func, idx);
        }

        // Build module (order matters: types, imports, functions, memory, exports, code)
        self.module.section(&self.types);
        self.module.section(&self.imports);
        self.module.section(&self.functions);
        self.module.section(&self.memory);
        self.module.section(&self.exports);
        self.module.section(&self.code);
        self.module.clone().finish()
    }
}
