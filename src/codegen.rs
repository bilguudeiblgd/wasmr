use wasm_encoder::{Module, CodeSection, Function, Instruction, ValType, FunctionSection, TypeSection, ExportSection, ExportKind, FuncType};

use crate::ast::{Stmt, Expr, BinaryOp, Type, Param};

// Simple codegen that emits a single function named "main" for a block of statements
// and returns i32 (for now). This is a minimal scaffold to start integrating wasm-encoder.

pub struct WasmGenerator {
    module: Module,
    types: TypeSection,
    functions: FunctionSection,
    exports: ExportSection,
    code: CodeSection,
    // map for locals could be added later
}

impl WasmGenerator {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            types: TypeSection::new(),
            functions: FunctionSection::new(),
            exports: ExportSection::new(),
            code: CodeSection::new(),
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

    fn gen_expr(&self, func: &mut Function, expr: &Expr) {
        match expr {
            Expr::Number(n) => {
                // naive: parse as i32
                let v: i32 = n.parse().unwrap_or(0);
                func.instruction(&Instruction::I32Const(v));
            }
            Expr::Binary { left, op, right } => {
                self.gen_expr(func, left);
                self.gen_expr(func, right);
                match op {
                    BinaryOp::Plus => { func.instruction(&Instruction::I32Add); },
                    BinaryOp::Minus => { func.instruction(&Instruction::I32Sub); },
                    BinaryOp::Mul => { func.instruction(&Instruction::I32Mul); },
                    BinaryOp::Div => { func.instruction(&Instruction::I32DivS); },
                    BinaryOp::Less => { func.instruction(&Instruction::I32LtS); },
                    BinaryOp::LessEqual => { func.instruction(&Instruction::I32LeS); },
                    BinaryOp::Range => {
                        // Not implemented: leave 0
                        func.instruction(&Instruction::Drop);
                        func.instruction(&Instruction::I32Const(0));
                    }
                }
            }
            Expr::Identifier(_) => {
                // locals/vars not implemented yet: push 0
                func.instruction(&Instruction::I32Const(0));
            }
            Expr::Call { .. } => {
                // calls not yet supported
                func.instruction(&Instruction::I32Const(0));
            }
            Expr::XString(s) => {
                func.instruction(&Instruction::I32Const(0));
            }
            Expr::Grouping(inner) => self.gen_expr(func, inner),
        }
    }

    fn gen_stmt(&self, func: &mut Function, stmt: &Stmt) {
        match stmt {
            Stmt::ExprStmt(e) => {
                self.gen_expr(func, e);
                func.instruction(&Instruction::Drop);
            }
            Stmt::VarAssign { .. } => {
                // locals not yet supported: ignore
            }
            Stmt::Return(opt) => {
                if let Some(e) = opt { self.gen_expr(func, e); }
                else { func.instruction(&Instruction::I32Const(0)); }
                func.instruction(&Instruction::Return);
            }
            Stmt::FunctionDef { .. } => {
                // multi-function not supported in this minimal skeleton
            }
            Stmt::Block(stmts) => {
                for s in stmts { self.gen_stmt(func, s); }
            }
        }
    }

    pub fn compile_main(&mut self, body: &[Stmt], ret_type: Type) -> Vec<u8> {
        // create type for function: no params, one result
        let result_ty = Self::wasm_valtype(&ret_type);
        let type_index = self.types.len();
        // register function type index 0
        {
            let mut ty = self.types.ty();
            ty.function(vec![], vec![result_ty]);
        }

        // function section declares it
        self.functions.function(type_index as u32);

        // code section body
        let mut f = Function::new(vec![]); // no locals yet
        for stmt in body {
            self.gen_stmt(&mut f, stmt);
        }
        // ensure something is on stack: default 0
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::End);
        self.code.function(&f);

        // export as "main"
        self.exports.export("main", ExportKind::Func, 0);

        // build module
        self.module.section(&self.types);
        self.module.section(&self.functions);
        self.module.section(&self.exports);
        self.module.section(&self.code);
        self.module.clone().finish()
    }
}

pub fn compile_to_wasm(program: Vec<Stmt>) -> Vec<u8> {
    // For now, treat the entire program as a block for a single main that returns i32
    let mut wg = WasmGenerator::new();
    wg.compile_main(&program, Type::Int)
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
