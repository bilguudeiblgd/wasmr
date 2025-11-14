use crate::ast::{BinaryOp, Param, ParamKind, Stmt as AstStmt, Type};
use crate::ir::{BuiltinKind, IR, IRExpr, IRExprKind, IRStmt as Stmt, TypeResolver};
use std::collections::HashMap;
use wasm_encoder::{BlockType, CodeSection, ExportKind, ExportSection, Function, FunctionSection, HeapType, Instruction, Module, RefType, StorageType, TypeSection, ValType};
use wasmtime::StorageType::ValType as StoreValType;

pub struct WasmGenerator {
    module: Module,
    pub(crate) types: TypeSection,
    pub(crate) functions: FunctionSection,
    exports: ExportSection,
    pub(crate) code: CodeSection,
    pub(crate) func_indices: HashMap<String, u32>,
    pub(crate) func_count: u32,
    array_type_i32: Option<u32>,
    array_type_f32: Option<u32>,
    array_type_f64: Option<u32>,
    array_type_anyref: Option<u32>,
}

/// Context for tracking locals within a function
struct LocalContext {
    /// Map variable names to local indices
    locals: HashMap<String, u32>,
    /// Local index for the packed varargs parameter, if present.
    varargs_local: Option<u32>,
}

impl LocalContext {
    fn new(params: Vec<Param>) -> Self {
        let mut locals = HashMap::new();
        let mut varargs_local = None;
        // Parameters are locals 0..param_count
        for (i, param) in params.iter().enumerate() {
            locals.insert(param.name.clone(), i as u32);
            if matches!(param.kind, ParamKind::VarArgs) {
                varargs_local = Some(i as u32);
            }
        }
        Self {
            locals,
            varargs_local,
        }
    }

    fn get_local(&self, name: &str) -> Option<u32> {
        self.locals.get(name).copied()
    }

    fn add_local(&mut self, name: String, index: u32) {
        self.locals.insert(name, index);
    }

    fn varargs_local(&self) -> Option<u32> {
        self.varargs_local
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
            array_type_i32: None,
            array_type_f32: None,
            array_type_f64: None,
            array_type_anyref: None,
        }
    }

    fn wasm_valtype(&mut self, t: &Type) -> ValType {
        match t {
            Type::Int | Type::Bool | Type::Char => ValType::I32,
            Type::Float => ValType::F32,
            Type::Double => ValType::F64,
            // #TODO: Shouldn't be ANYREF
            Type::List | Type::VarArgs | Type::Any | Type::FunctionRef => {
                ValType::Ref(RefType::ANYREF)
            }
            Type::Vector(inner_ty) => {
                // Get the storage type for the inner type

                let storage = self.storage_type_for(inner_ty);
                // Ensure the array type exists and get its index
                let array_type_idx = self.ensure_array_type(&storage);
                // Return a concrete reference to that specific array type
                ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Concrete(array_type_idx),
                })
            }

            _ => ValType::I32,
        }
    }

    fn wasm_param_valtype(&mut self, param: &Param) -> ValType {
        match &param.kind {
            ParamKind::Normal(ty) => self.wasm_valtype(ty),
            ParamKind::VarArgs => ValType::Ref(RefType::ANYREF),
        }
    }

    fn storage_type_for(&self, ty: &Type) -> StorageType {
        match ty {
            Type::Int | Type::Bool | Type::Char | Type::String => StorageType::Val(ValType::I32),
            Type::Float => StorageType::Val(ValType::F32),
            Type::Double => StorageType::Val(ValType::F64),
            Type::Vector(_) | Type::List | Type::VarArgs | Type::Any | Type::FunctionRef => {
                StorageType::Val(ValType::Ref(RefType::ANYREF))
            }
            _ => StorageType::Val(ValType::I32),
        }
    }

    pub(crate) fn ensure_array_type(&mut self, storage: &StorageType) -> u32 {
        match storage {
            StorageType::Val(ValType::I32) => {
                if let Some(idx) = self.array_type_i32 {
                    idx
                } else {
                    let index = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.array_type_i32 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::F32) => {
                if let Some(idx) = self.array_type_f32 {
                    idx
                } else {
                    let index = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.array_type_f32 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::F64) => {
                if let Some(idx) = self.array_type_f64 {
                    idx
                } else {
                    let index = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.array_type_f64 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::Ref(rt)) if *rt == RefType::ANYREF => {
                if let Some(idx) = self.array_type_anyref {
                    idx
                } else {
                    let index = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.array_type_anyref = Some(index);
                    index
                }
            }
            _ => {
                // Fallback: treat as i32 storage
                self.ensure_array_type(&StorageType::Val(ValType::I32))
            }
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

    fn gen_vector_binary_op(&mut self,
                            func: &mut Function,
                            ctx: &LocalContext,
                            op: &BinaryOp,
                            left: &IRExpr,
                            right: &IRExpr) {
        // allowed
        // Type -> Vector[Type] op Type
        // Or -> Vector[Type] op Type
        // func.instruction(&Instruction::ArrayGet())

        match op {
            BinaryOp::Plus => {
                let callee = IRExpr {
                    kind: IRExprKind::Identifier("vector_add".to_string()),
                    ty: Type::Vector(Box::new(Type::Int)),
                };
                let args = vec![left.clone(), right.clone()];
                self.gen_call(func, ctx, &callee, &args);
            }
            _ => {}
        }
    }

    fn gen_binary_op(
        &mut self,
        func: &mut Function,
        ctx: &LocalContext,
        op: &BinaryOp,
        left: &IRExpr,
        right: &IRExpr,
    ) {
        if(matches!(left.ty, Type::Vector(_)) || matches!(right.ty, Type::Vector(_))) {
            self.gen_vector_binary_op(func, ctx, op, left, right);
            return;
        }
        // func.instruction(&)
        self.gen_expr(func, ctx, left);
        self.gen_expr(func, ctx, right);
        // func.instruction(&Instruction::Call())
        // Verify both operands have the same type (sanity check)
        // IR should guarantee this, but we check defensively
        if left.ty != right.ty {
            eprintln!(
                "Warning: Binary operation type mismatch: left={:?}, right={:?}",
                left.ty, right.ty
            );
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
            }
            BinaryOp::Minus => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Sub),
                    Type::Float => func.instruction(&Instruction::F32Sub),
                    Type::Double => func.instruction(&Instruction::F64Sub),
                    _ => func.instruction(&Instruction::I32Sub),
                };
            }
            BinaryOp::Mul => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Mul),
                    Type::Float => func.instruction(&Instruction::F32Mul),
                    Type::Double => func.instruction(&Instruction::F64Mul),
                    _ => func.instruction(&Instruction::I32Mul),
                };
            }
            BinaryOp::Div => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32DivS),
                    Type::Float => func.instruction(&Instruction::F32Div),
                    Type::Double => func.instruction(&Instruction::F64Div),
                    _ => func.instruction(&Instruction::I32DivS),
                };
            }
            BinaryOp::Less => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32LtS),
                    Type::Float => func.instruction(&Instruction::F32Lt),
                    Type::Double => func.instruction(&Instruction::F64Lt),
                    _ => func.instruction(&Instruction::I32LtS),
                };
            }
            BinaryOp::LessEqual => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32LeS),
                    Type::Float => func.instruction(&Instruction::F32Le),
                    Type::Double => func.instruction(&Instruction::F64Le),
                    _ => func.instruction(&Instruction::I32LeS),
                };
            }
            BinaryOp::Or => {
                // booleans are represented as i32 0/1
                func.instruction(&Instruction::I32Or);
            }
            BinaryOp::And => {
                func.instruction(&Instruction::I32And);
            }
            BinaryOp::Range => {
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I32Const(0));
            }
        }
    }

    fn gen_expr(&mut self, func: &mut Function, ctx: &LocalContext, expr: &IRExpr) {
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
            IRExprKind::VectorLiteral(vec) => {
                self.gen_vector_literal(func, ctx, vec);
            }
            IRExprKind::Call { callee, args } => {
                    self.gen_call(func, ctx, callee, args);
            }
            IRExprKind::VarArgs => {
                self.gen_varargs_expr(func, ctx);
            }
            IRExprKind::XString(_s) => {
                func.instruction(&Instruction::I32Const(0));
            }
            IRExprKind::Unit | _ => {
                // nothing to push for void; use 0 by convention if a value is required, handled by caller
            }
        }
    }

    fn gen_vector_literal(&mut self, func: &mut Function, ctx: &LocalContext, vec: &Vec<IRExpr>) {
        vec.iter().for_each(|e| self.gen_expr(func, ctx, e));
        let element_ty = match vec.iter().find(|a| !matches!(a.ty, Type::VarArgs)) {
            Some(expr) => &expr.ty,
            None => panic!("Vector literal must have at least one element"),
        };
        let storage = self.storage_type_for(element_ty);
        let array_type_index = self.ensure_array_type(&storage);
        func.instruction(&Instruction::ArrayNewFixed {
            array_type_index,
            array_size: vec.len() as u32,
        });
    }

    fn gen_call(
        &mut self,
        func: &mut Function,
        ctx: &LocalContext,
        callee: &IRExpr,
        args: &[IRExpr],
    ) {
        // Generate arguments first (they need to be on the stack in order)
        for arg in args {
            self.gen_expr(func, ctx, arg);
        }

        // Determine what kind of call this is
        match &callee.kind {
            IRExprKind::Identifier(name) => {
                // Direct function call by name
                if let Some(&func_idx) = self.func_indices.get(name) {
                    func.instruction(&Instruction::Call(func_idx));
                } else {
                    // Unknown function - this should have been caught by type checking
                    eprintln!("Warning: Call to unknown function '{}'", name);
                    // Drop all arguments from stack
                    for _ in args {
                        func.instruction(&Instruction::Drop);
                    }
                    // Push a default value (0) as a placeholder
                    func.instruction(&Instruction::I32Const(0));
                }
            }
            _ => {
                // Indirect call (function stored in variable or returned from expression)
                // This would require function tables (call_indirect)
                // For now, we'll just emit a warning and provide a fallback
                eprintln!("Warning: Indirect function calls not yet fully implemented");

                // Drop all arguments
                for _ in args {
                    func.instruction(&Instruction::Drop);
                }

                // Try to evaluate the callee expression
                self.gen_expr(func, ctx, callee);
                func.instruction(&Instruction::Drop);

                // Push default value
                func.instruction(&Instruction::I32Const(0));
            }
        }
    }


    fn gen_varargs_expr(&mut self, func: &mut Function, ctx: &LocalContext) {
        if let Some(idx) = ctx.varargs_local() {
            func.instruction(&Instruction::LocalGet(idx));
        } else {
            func.instruction(&Instruction::RefNull(HeapType::ANY));
        }
    }

    fn gen_stmt(
        &mut self,
        func: &mut Function,
        ctx: &LocalContext,
        stmt: &Stmt,
        ret_has_value: bool,
    ) {
        match stmt {
            Stmt::ExprStmt(e) => {
                self.gen_expr(func, ctx, e);
                func.instruction(&Instruction::Drop);
            }
            Stmt::VarAssign {
                name, ty, value, ..
            } => {
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

        // Export functions
        for (name, _p, _r, _b) in &functions {
            if let Some(&idx) = self.func_indices.get(name) {
                self.exports.export(name, ExportKind::Func, idx);
            }
        }
        if let Some(idx) = synthetic_main_index {
            self.exports.export("_start", ExportKind::Func, idx);
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
    use std::fs;
    use std::path::PathBuf;
    let mut path = PathBuf::from("data");
    path.push("wasm_out");
    if !path.exists() {
        fs::create_dir_all(&path)?;
    }
    Ok(path)
}

pub fn write_wasm_file<S: AsRef<str>>(
    filename_stem: S,
    bytes: &[u8],
) -> std::io::Result<std::path::PathBuf> {
    use std::fs;
    use std::path::PathBuf;
    let out_dir = ensure_wasm_out_dir()?;
    let mut path = PathBuf::from(out_dir);
    let stem = filename_stem.as_ref();
    let file_name = if stem.ends_with(".wasm") {
        stem.to_string()
    } else {
        format!("{}.wasm", stem)
    };
    path.push(file_name);
    fs::write(&path, bytes)?;
    Ok(path)
}

pub fn write_wat_file<S: AsRef<str>>(
    filename_stem: S,
    wat_text: &str,
) -> std::io::Result<std::path::PathBuf> {
    use std::fs;
    use std::path::PathBuf;
    let out_dir = ensure_wasm_out_dir()?;
    let mut path = PathBuf::from(out_dir);
    let stem = filename_stem.as_ref();
    let file_name = if stem.ends_with(".wat") {
        stem.to_string()
    } else {
        format!("{}.wat", stem)
    };
    path.push(file_name);
    fs::write(&path, wat_text)?;
    Ok(path)
}

pub fn compile_and_write_ir<S: AsRef<str>>(
    program: Vec<Stmt>,
    filename_stem: S,
) -> std::io::Result<std::path::PathBuf> {
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

pub fn compile_and_write<S: AsRef<str>>(
    program: Vec<AstStmt>,
    filename_stem: S,
) -> std::io::Result<std::path::PathBuf> {
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
