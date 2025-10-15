use std::collections::HashMap;
use wasm_encoder::{Module, CodeSection, Function, Instruction, ValType, FunctionSection, TypeSection, ExportSection, ExportKind, ImportSection, EntityType};

use crate::ast::{BinaryOp, Type, Param, Stmt as AstStmt};
use crate::ir::{IRStmt as Stmt, IRExpr as Expr, IRExprKind, TypeResolver, IR};

pub struct WasmGenerator {
    module: Module,
    types: TypeSection,
    functions: FunctionSection,
    exports: ExportSection,
    code: CodeSection,
    func_indices: HashMap<String, u32>,
    func_count: u32,
}

/// Context for tracking locals within a function
struct LocalContext {
    /// Map variable names to local indices
    locals: HashMap<String, u32>,
    /// Function parameters (also accessible as locals, starting from index 0)
    params: Vec<Param>,
}

impl LocalContext {
    fn new(params: Vec<Param>) -> Self {
        let mut locals = HashMap::new();
        // Parameters are locals 0..param_count
        for (i, param) in params.iter().enumerate() {
            locals.insert(param.name.clone(), i as u32);
        }
        Self { locals, params }
    }

    fn get_local(&self, name: &str) -> Option<u32> {
        self.locals.get(name).copied()
    }

    fn add_local(&mut self, name: String, index: u32) {
        self.locals.insert(name, index);
    }
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
            _ => ValType::I32,
        }
    }

    /// Collect all variable declarations from statements
    fn collect_vars(&self, stmts: &[Stmt]) -> Vec<(String, Type)> {
        let mut vars = Vec::new();
        for stmt in stmts {
            self.collect_vars_from_stmt(stmt, &mut vars);
        }
        vars
    }

    fn collect_vars_from_stmt(&self, stmt: &Stmt, vars: &mut Vec<(String, Type)>) {
        match stmt {
            Stmt::VarAssign { name, ty, .. } => {
                let ty = ty.clone();
                if !vars.iter().any(|(n, _)| n == name) {
                    vars.push((name.clone(), ty));
                }
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.collect_vars_from_stmt(s, vars);
                }
            }
            _ => {}
        }
    }

    fn gen_binary_op(&self, func: &mut Function, ctx: &LocalContext, op: &BinaryOp, left: &Expr, right: &Expr) {
        self.gen_expr(func, ctx, left);
        self.gen_expr(func, ctx, right);

        // Verify both operands have the same type (sanity check)
        // IR should guarantee this, but we check defensively
        if left.ty != right.ty {
            eprintln!("Warning: Binary operation type mismatch: left={:?}, right={:?}", left.ty, right.ty);
        }

        // Use the left operand's type to determine which instruction to emit
        let ty = &left.ty;

        match op {
            BinaryOp::Plus => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Add),
                    Type::Float => func.instruction(&Instruction::F32Add),
                    Type::Double => func.instruction(&Instruction::F64Add),
                    _ => func.instruction(&Instruction::I32Add), // fallback
                };
            },
            BinaryOp::Minus => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Sub),
                    Type::Float => func.instruction(&Instruction::F32Sub),
                    Type::Double => func.instruction(&Instruction::F64Sub),
                    _ => func.instruction(&Instruction::I32Sub),
                };
            },
            BinaryOp::Mul => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Mul),
                    Type::Float => func.instruction(&Instruction::F32Mul),
                    Type::Double => func.instruction(&Instruction::F64Mul),
                    _ => func.instruction(&Instruction::I32Mul),
                };
            },
            BinaryOp::Div => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32DivS),
                    Type::Float => func.instruction(&Instruction::F32Div),
                    Type::Double => func.instruction(&Instruction::F64Div),
                    _ => func.instruction(&Instruction::I32DivS),
                };
            },
            BinaryOp::Less => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32LtS),
                    Type::Float => func.instruction(&Instruction::F32Lt),
                    Type::Double => func.instruction(&Instruction::F64Lt),
                    _ => func.instruction(&Instruction::I32LtS),
                };
            },
            BinaryOp::LessEqual => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32LeS),
                    Type::Float => func.instruction(&Instruction::F32Le),
                    Type::Double => func.instruction(&Instruction::F64Le),
                    _ => func.instruction(&Instruction::I32LeS),
                };
            },
            BinaryOp::Or => {
                // booleans are represented as i32 0/1
                func.instruction(&Instruction::I32Or);
            },
            BinaryOp::And => {
                func.instruction(&Instruction::I32And);
            },
            BinaryOp::Range => {
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I32Const(0));
            }
        }

    }

    fn gen_expr(&self, func: &mut Function, ctx: &LocalContext, expr: &Expr) {
        match &expr.kind {
            IRExprKind::Number(n) => {
                let v: i32 = n.parse().unwrap_or(0);
                func.instruction(&Instruction::I32Const(v));
            }
            IRExprKind::Binary { left, op, right } => {
                self.gen_binary_op(func, ctx, op, left, right)
            }
            IRExprKind::Identifier(name) => {
                if let Some(idx) = ctx.get_local(name) {
                    func.instruction(&Instruction::LocalGet(idx));
                } else {
                    // Unknown variable, push 0 as fallback
                    func.instruction(&Instruction::I32Const(0));
                }
            }
            IRExprKind::Call { callee, args } => {
                if let IRExprKind::Identifier(name) = &callee.kind {
                    for a in args {
                        self.gen_expr(func, ctx, a);
                    }
                    if let Some(&idx) = self.func_indices.get(name) {
                        func.instruction(&Instruction::Call(idx));
                    } else {
                        for _ in args { func.instruction(&Instruction::Drop); }
                        func.instruction(&Instruction::I32Const(0));
                    }
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }
            }
            IRExprKind::XString(_s) => {
                func.instruction(&Instruction::I32Const(0));
            }
            IRExprKind::Unit => {
                // nothing to push for void; use 0 by convention if a value is required, handled by caller
            }
        }
    }

    fn gen_stmt(&self, func: &mut Function, ctx: &LocalContext, stmt: &Stmt, ret_has_value: bool) {
        match stmt {
            Stmt::ExprStmt(e) => {
                self.gen_expr(func, ctx, e);
                func.instruction(&Instruction::Drop);
            }
            Stmt::VarAssign { name, ty, value, .. } => {
                self.gen_expr(func, ctx, value);
                // value.ty when does type checking work?
                if let Some(idx) = ctx.get_local(name) {
                    func.instruction(&Instruction::LocalSet(idx));
                } else {
                    func.instruction(&Instruction::Drop);
                }
            }
            Stmt::Return(e) => {
                // IR guarantees return carries an expression; use it.
                self.gen_expr(func, ctx, e);
                func.instruction(&Instruction::Return);
            }
            Stmt::FunctionDef { .. } => {
                // handled at top-level
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.gen_stmt(func, ctx, s, ret_has_value);
                }
            }
        }
    }

    pub fn compile_program(&mut self, program: Vec<Stmt>) -> Vec<u8> {

        // Split into function defs and top-level statements
        let mut functions: Vec<(String, Vec<Param>, Type, Vec<Stmt>)> = Vec::new();
        let mut top_level: Vec<Stmt> = Vec::new();
        for s in &program {
            if let Stmt::FunctionDef { name, params, return_type, body } = s {
                functions.push((name.clone(), params.clone(), return_type.clone(), body.clone()));
            } else {
                top_level.push(s.clone());
            }
        }

        // First pass: declare all function types
        for (name, params, ret_ty, _body) in &functions {
            let param_tys: Vec<ValType> = params.iter().map(|p| Self::wasm_valtype(&p.ty)).collect();
            let result_tys: Vec<ValType> = if *ret_ty != Type::Void {
                vec![Self::wasm_valtype(ret_ty)]
            } else {
                vec![]
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

        // Second pass: define function bodies
        for (name, params, ret_ty, body) in &functions {
            let ret_has_value = *ret_ty != Type::Void;
            
            // Collect local variables
            let vars = self.collect_vars(body);
            let local_types: Vec<(u32, ValType)> = vars
                .iter()
                .map(|(_, ty)| (1, Self::wasm_valtype(ty)))
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
                .map(|(_, ty)| (1, Self::wasm_valtype(ty)))
                .collect();
            
            let mut f = Function::new(local_types);
            
            // Build local context (no params for main)
            let mut ctx = LocalContext::new(vec![]);
            for (i, (var_name, _)) in vars.iter().enumerate() {
                ctx.add_local(var_name.clone(), i as u32);
            }
            
            let last_idx = top_level.len().saturating_sub(1);
            for (i, stmt) in top_level.iter().enumerate() {
                if i == last_idx {
                    // For the last statement, evaluate and leave on stack
                    match stmt {
                        Stmt::ExprStmt(e) => {
                            self.gen_expr(&mut f, &ctx, e);
                        }
                        Stmt::VarAssign { name, value, .. } => {
                            self.gen_expr(&mut f, &ctx, value);
                            if let Some(idx) = ctx.get_local(name) {
                                f.instruction(&Instruction::LocalTee(idx)); // Tee: set local AND keep value on stack
                            }
                        }
                        Stmt::Return(e) => {
                            self.gen_expr(&mut f, &ctx, e);
                            f.instruction(&Instruction::Return);
                        }
                        _ => {
                            self.gen_stmt(&mut f, &ctx, stmt, true);
                            f.instruction(&Instruction::I32Const(0));
                        }
                    }
                } else {
                    self.gen_stmt(&mut f, &ctx, stmt, true);
                }
            }
            
            if top_level.is_empty() {
                f.instruction(&Instruction::I32Const(0));
            }
            f.instruction(&Instruction::End);
            self.code.function(&f);
        }

        // Export functions
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

pub fn compile_to_wasm_ir(program: Vec<Stmt>) -> Vec<u8> {
    let mut wg = WasmGenerator::new();
    wg.compile_program(program)
}

pub fn compile_to_wasm(program: Vec<AstStmt>) -> Vec<u8> {
    let mut resolver = TypeResolver::new();
    let ir_program = match IR::from_ast(program, &mut resolver) {
        Ok(ir) => ir,
        Err(e) => {
            eprintln!("Type error during lowering: {:?}", e);
            Vec::new()
        }
    };
    compile_to_wasm_ir(ir_program)
}

pub fn wasm_to_wat(wasm_bytes: &[u8]) -> Result<String, String> {
    wasmprinter::print_bytes(wasm_bytes)
        .map_err(|e| format!("Failed to convert WASM to WAT: {}", e))
}

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

pub fn write_wasm_file<S: AsRef<str>>(filename_stem: S, bytes: &[u8]) -> std::io::Result<std::path::PathBuf> {
    use std::fs;
    use std::path::PathBuf;
    let out_dir = ensure_wasm_out_dir()?;
    let mut path = PathBuf::from(out_dir);
    let stem = filename_stem.as_ref();
    let file_name = if stem.ends_with(".wasm") { stem.to_string() } else { format!("{}.wasm", stem) };
    path.push(file_name);
    fs::write(&path, bytes)?;
    Ok(path)
}

pub fn write_wat_file<S: AsRef<str>>(filename_stem: S, wat_text: &str) -> std::io::Result<std::path::PathBuf> {
    use std::fs;
    use std::path::PathBuf;
    let out_dir = ensure_wasm_out_dir()?;
    let mut path = PathBuf::from(out_dir);
    let stem = filename_stem.as_ref();
    let file_name = if stem.ends_with(".wat") { stem.to_string() } else { format!("{}.wat", stem) };
    path.push(file_name);
    fs::write(&path, wat_text)?;
    Ok(path)
}

pub fn compile_and_write_ir<S: AsRef<str>>(program: Vec<Stmt>, filename_stem: S) -> std::io::Result<std::path::PathBuf> {
    let bytes = compile_to_wasm_ir(program);
    let stem = filename_stem.as_ref();
    
    let wasm_path = write_wasm_file(stem, &bytes)?;
    
    match wasm_to_wat(&bytes) {
        Ok(wat_text) => {
            if let Err(e) = write_wat_file(stem, &wat_text) {
                eprintln!("Warning: Failed to write WAT file: {}", e);
            }
        }
        Err(e) => {
            eprintln!("Warning: Failed to convert to WAT: {}", e);
        }
    }
    
    Ok(wasm_path)
}

pub fn compile_and_write<S: AsRef<str>>(program: Vec<AstStmt>, filename_stem: S) -> std::io::Result<std::path::PathBuf> {
    let bytes = compile_to_wasm(program);
    let stem = filename_stem.as_ref();
    
    let wasm_path = write_wasm_file(stem, &bytes)?;
    
    match wasm_to_wat(&bytes) {
        Ok(wat_text) => {
            if let Err(e) = write_wat_file(stem, &wat_text) {
                eprintln!("Warning: Failed to write WAT file: {}", e);
            }
        }
        Err(e) => {
            eprintln!("Warning: Failed to convert to WAT: {}", e);
        }
    }
    
    Ok(wasm_path)
}
