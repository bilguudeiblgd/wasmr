mod type_mapping;
mod local_context;
mod wasi;
mod binary_ops;
mod expressions;
mod builtins;
mod statements;
pub mod io;
pub mod codegen_builtins;

// Re-export public API
pub use io::{compile_and_write, compile_and_write_ir, compile_to_wasm, compile_to_wasm_ir, wasm_to_wat, write_wasm_file, write_wat_file};

use crate::ast::{Param, Type};
use local_context::LocalContext;
use crate::ir::{IRProgram, IRStmt as Stmt};
use std::collections::HashMap;
use wasm_encoder::{CodeSection, DataSection, ExportKind, ExportSection, Function, FunctionSection, ImportSection, Instruction, MemorySection, Module, TypeSection, ValType};
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
    pub(crate) fd_write_idx: Option<u32>,
    array_type_i32: Option<u32>,
    array_type_f32: Option<u32>,
    array_type_f64: Option<u32>,
    array_type_anyref: Option<u32>,
    // Vector struct types: struct { data: array<T>, length: i32 }
    vector_struct_i32: Option<u32>,
    vector_struct_f32: Option<u32>,
    vector_struct_f64: Option<u32>,
    vector_struct_anyref: Option<u32>,
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
            vector_struct_i32: None,
            vector_struct_f32: None,
            vector_struct_f64: None,
            vector_struct_anyref: None,
        }
    }

    pub fn compile_program(&mut self, program: IRProgram) -> Vec<u8> {
        // Setup WASI imports (fd_write) and memory
        self.setup_wasi_imports();

        // Split into function defs and top-level statements
        self.include_builtins();

        let program_stmts = &program.statements;
        let mut functions: Vec<(String, Vec<Param>, Type, Vec<Stmt>, Box<crate::ir::FunctionMetadata>)> = Vec::new();
        let mut top_level: Vec<Stmt> = Vec::new();
        for s in program_stmts {
            if let Stmt::FunctionDef {
                name,
                params,
                return_type,
                body,
                metadata,
            } = s
            {
                // Metadata must be present at this point (populated by passes)
                let meta = metadata.as_ref().expect("Function metadata must be populated by IR passes");
                functions.push((
                    name.clone(),
                    params.clone(),
                    return_type.clone(),
                    body.clone(),
                    meta.clone(),
                ));
            } else {
                top_level.push(s.clone());
            }
        }

        // First pass: declare all function types
        for (name, params, ret_ty, _body, _metadata) in &functions {
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
        for (_name, params, ret_ty, body, metadata) in &functions {
            let ret_has_value = *ret_ty != Type::Void;

            // Use precomputed metadata instead of collecting vars
            let param_count = params.len() as u32;
            let non_param_vars = &metadata.local_vars[param_count as usize..];

            let local_types: Vec<(u32, ValType)> = non_param_vars
                .iter()
                .map(|var_info| (1, self.wasm_valtype(&var_info.ty)))
                .collect();

            let mut f = Function::new(local_types);

            // Build local context from metadata
            let ctx = LocalContext::from_metadata(metadata);

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
        for (name, _p, _r, _b, _m) in &functions {
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

    /// Simple variable collector for top-level statements (without metadata)
    /// This is only used for synthetic main functions
    fn collect_vars(&self, stmts: &[Stmt]) -> Vec<(String, Type)> {
        use std::collections::HashSet;
        let mut vars = Vec::new();
        let mut seen = HashSet::new();

        fn collect_from_stmts(
            stmts: &[Stmt],
            vars: &mut Vec<(String, Type)>,
            seen: &mut HashSet<String>,
        ) {
            for stmt in stmts {
                match stmt {
                    Stmt::VarAssign { name, ty, .. } => {
                        if !seen.contains(name) {
                            vars.push((name.clone(), ty.clone()));
                            seen.insert(name.clone());
                        }
                    }
                    Stmt::Block(body) => collect_from_stmts(body, vars, seen),
                    Stmt::If { then_branch, else_branch, .. } => {
                        collect_from_stmts(then_branch, vars, seen);
                        if let Some(else_stmts) = else_branch {
                            collect_from_stmts(else_stmts, vars, seen);
                        }
                    }
                    Stmt::For { iter_var, body, .. } => {
                        let (iter_name, iter_ty) = iter_var;
                        if !seen.contains(iter_name) {
                            vars.push((iter_name.clone(), iter_ty.clone()));
                            seen.insert(iter_name.clone());
                        }
                        // Add compiler temporaries for loops
                        let system_vars = vec![
                            ("system_iter".to_string(), Type::Int),
                            (format!("__for_vec_{}", iter_name), Type::Vector(Box::new(iter_ty.clone()))),
                            ("__for_len".to_string(), Type::Int),
                        ];
                        for (name, ty) in system_vars {
                            if !seen.contains(&name) {
                                vars.push((name.clone(), ty));
                                seen.insert(name);
                            }
                        }
                        collect_from_stmts(body, vars, seen);
                    }
                    _ => {}
                }
            }
        }

        collect_from_stmts(stmts, &mut vars, &mut seen);
        vars
    }
}
