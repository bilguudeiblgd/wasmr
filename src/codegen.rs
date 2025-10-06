use std::collections::HashMap;
use wasm_encoder::{Module, CodeSection, Function, Instruction, ValType, FunctionSection, TypeSection, ExportSection, ExportKind};

use crate::ast::{Stmt, Expr, BinaryOp, Type, Param};

// Simple codegen that emits a single function named "main" for a block of statements
// and returns i32 (for now). This is a minimal scaffold to start integrating wasm-encoder.

pub struct WasmGenerator {
    module: Module,
    types: TypeSection,
    functions: FunctionSection,
    exports: ExportSection,
    code: CodeSection,
    // function registry
    func_indices: HashMap<String, u32>,
    func_count: u32,
}

impl WasmGenerator {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            types: TypeSection::new(),
            functions: FunctionSection::new(),
            exports: ExportSection::new(),
            code: CodeSection::new(),
            func_indices: HashMap::new(),
            func_count: 0,
        }
    }

    fn wasm_valtype(t: &Type) -> ValType {
        match t {
            Type::Int | Type::Bool | Type::Char => ValType::I32,
            Type::Float => ValType::F32,
            Type::Double => ValType::F64,
            // For now map others to i32 pointer placeholder
            _ => ValType::I32,
        }
    }

    fn gen_expr(&self, func: &mut Function, params: Vec<Param>, expr: &Expr) {
        match expr {
            Expr::Number(n) => {
                let v: i32 = n.parse().unwrap_or(0);
                func.instruction(&Instruction::I32Const(v));
            }
            Expr::Binary { left, op, right } => {
                self.gen_expr(func, params.clone(), left);
                self.gen_expr(func, params.clone(), right);
                match op {
                    BinaryOp::Plus => { func.instruction(&Instruction::I32Add); },
                    BinaryOp::Minus => { func.instruction(&Instruction::I32Sub); },
                    BinaryOp::Mul => { func.instruction(&Instruction::I32Mul); },
                    BinaryOp::Div => { func.instruction(&Instruction::I32DivS); },
                    BinaryOp::Less => { func.instruction(&Instruction::I32LtS); },
                    BinaryOp::LessEqual => { func.instruction(&Instruction::I32LeS); },
                    BinaryOp::Range => {
                        func.instruction(&Instruction::Drop);
                        func.instruction(&Instruction::I32Const(0));
                    }
                }
            }
            // need to
            Expr::Identifier(str) => {
                let index = params.into_iter().position(|p| p.name == *str);
                match index {
                    Some(num) => {
                        func.instruction(&Instruction::LocalGet(num as u32));
                    }
                    _ => {}
                }
                // locals/vars not implemented yet: push 0
                // func.instruction(&Instruction::I32Const(0));
            }
            Expr::Call { callee, args } => {
                // Only support calling a named function for now
                if let Expr::Identifier(name) = &**callee {
                    // evaluate arguments
                    for a in args {
                        self.gen_expr(func, vec![], a);
                    }
                    if let Some(&idx) = self.func_indices.get(name) {
                        func.instruction(&Instruction::Call(idx));
                    } else {
                        // unknown function: drop args and push 0
                        for _ in args { func.instruction(&Instruction::Drop); }
                        func.instruction(&Instruction::I32Const(0));
                    }
                } else {
                    // unsupported callee expression
                    func.instruction(&Instruction::I32Const(0));
                }
            }
            Expr::XString(_) => {
                func.instruction(&Instruction::I32Const(0));
            }
            Expr::Grouping(inner) => self.gen_expr(func, vec![], inner),
            
        }
    }

    fn gen_stmt(&self, func: &mut Function, stmt: &Stmt, params: Vec<Param>, ret_has_value: bool) {
        match stmt {
            Stmt::ExprStmt(e) => {
                self.gen_expr(func, params.clone(), e);
                func.instruction(&Instruction::Drop);
            }
            Stmt::VarAssign { .. } => {
                // locals not yet supported: ignore
            }
            Stmt::Return(opt) => {
                if let Some(e) = opt {
                    self.gen_expr(func, params.clone(), e);
                } else if ret_has_value {
                    func.instruction(&Instruction::I32Const(0));
                }
                func.instruction(&Instruction::Return);
            }
            Stmt::FunctionDef { .. } => {
                // handled at top-level
            }
            Stmt::Block(stmts) => {
                for s in stmts { self.gen_stmt(func, s, vec![], ret_has_value); }
            }
        }
    }

    pub fn compile_program(&mut self, program: Vec<Stmt>) -> Vec<u8> {
        // Split into function defs and top-level statements
        let mut functions: Vec<(String, Vec<Param>, Option<Type>, Vec<Stmt>)> = Vec::new();
        let mut top_level: Vec<Stmt> = Vec::new();
        for s in &program {
            if let Stmt::FunctionDef { name, params, return_type, body } = s {
                functions.push((name.clone(), params.clone(), return_type.clone(), body.clone()));
            } else {
                top_level.push(s.clone());
            }
        }

        // First pass: declare all function types and functions to get indices
        for (name, params, ret_ty, _body) in &functions {
            let param_tys: Vec<ValType> = params.iter().map(|p| Self::wasm_valtype(&p.ty)).collect();
            let result_tys: Vec<ValType> = match ret_ty {
                Some(t) => {
                    if *t != Type::Void {

                        vec![Self::wasm_valtype(t)]
                    } else { vec![] }
                }
                None => vec![],
            };
            let type_index = self.types.len();
            {
                let mut ty = self.types.ty();
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
                let mut ty = self.types.ty();
                ty.function(vec![], vec![ValType::I32]);
            }
            self.functions.function(type_index as u32);
            let idx = self.func_count;
            self.func_indices.insert("main".to_string(), idx);
            self.func_count += 1;
            synthetic_main_index = Some(idx);
        }

        // Second pass: define function bodies in the same order as declared
        for (_name, _params, ret_ty, body) in &functions {
            let ret_has_value = match ret_ty {
                Some(t) if *t != Type::Void => true,
                _ => false,
            };
            let mut f = Function::new(vec![]);
            for stmt in body {
                self.gen_stmt(&mut f, stmt, _params.clone(), ret_has_value);
            }
            if ret_has_value {
                // ensure something on the stack for fallthrough
                // f.instruction(&Instruction::I32Const(0));
            }
            f.instruction(&Instruction::End);
            self.code.function(&f);
        }

        if let Some(_idx) = synthetic_main_index {
            let mut f = Function::new(vec![]);
            let last_idx = top_level.len().saturating_sub(1);
            for (i, stmt) in top_level.iter().enumerate() {
                if i == last_idx {
                    // For the last statement, evaluate it and leave result on stack
                    match stmt {
                        Stmt::ExprStmt(e) | Stmt::VarAssign { value: e, .. } => {
                            self.gen_expr(&mut f, vec![], e);
                        }
                        Stmt::Return(opt) => {
                            if let Some(e) = opt {
                                self.gen_expr(&mut f, vec![], e);
                            } else {
                                f.instruction(&Instruction::I32Const(0));
                            }
                            f.instruction(&Instruction::Return);
                        }
                        _ => {
                            self.gen_stmt(&mut f, stmt, vec![], true);
                            f.instruction(&Instruction::I32Const(0));
                        }
                    }
                } else {
                    // For non-last statements, generate normally
                    self.gen_stmt(&mut f, stmt, vec![], true);
                }
            }
            // If empty top_level, ensure default return
            if top_level.is_empty() {
                f.instruction(&Instruction::I32Const(0));
            }
            f.instruction(&Instruction::End);
            self.code.function(&f);
        }

        // Export all user-defined functions and synthetic main if present
        for (name, _p, _r, _b) in &functions {
            if let Some(&idx) = self.func_indices.get(name) {
                self.exports.export(name, ExportKind::Func, idx);
            }
        }
        if let Some(idx) = synthetic_main_index {
            self.exports.export("main", ExportKind::Func, idx);
        }

        // Build module
        self.module.section(&self.types);
        self.module.section(&self.functions);
        self.module.section(&self.exports);
        self.module.section(&self.code);
        self.module.clone().finish()
    }
}

pub fn compile_to_wasm(program: Vec<Stmt>) -> Vec<u8> {
    let mut wg = WasmGenerator::new();
    wg.compile_program(program)
}

/// Ensure data/wasm_out exists relative to the project root (or current working directory)
fn ensure_wasm_out_dir() -> std::io::Result<std::path::PathBuf> {
    use std::path::PathBuf;
    use std::fs;
    let mut path = PathBuf::from("data");
    path.push("wasm_out");
    if !path.exists() {
        fs::create_dir_all(&path)?;
    }
    Ok(path)
}

/// Write the provided wasm bytes to data/wasm_out/<filename>.wasm
/// Returns the full path on success.
pub fn write_wasm_file<S: AsRef<str>>(filename_stem: S, bytes: &[u8]) -> std::io::Result<std::path::PathBuf> {
    use std::fs;
    use std::path::PathBuf;
    let out_dir = ensure_wasm_out_dir()?;
    let mut path = PathBuf::from(out_dir);
    let stem = filename_stem.as_ref();
    // Append .wasm if not already provided
    let file_name = if stem.ends_with(".wasm") { stem.to_string() } else { format!("{}.wasm", stem) };
    path.push(file_name);
    fs::write(&path, bytes)?;
    Ok(path)
}

/// Convenience function: compile the given program and write to data/wasm_out/<filename>.wasm
pub fn compile_and_write<S: AsRef<str>>(program: Vec<Stmt>, filename_stem: S) -> std::io::Result<std::path::PathBuf> {
    let bytes = compile_to_wasm(program);
    write_wasm_file(filename_stem, &bytes)
}
