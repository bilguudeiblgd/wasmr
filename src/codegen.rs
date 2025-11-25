use crate::ast::{BinaryOp, Param, ParamKind, Stmt as AstStmt, Type};
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

    fn setup_wasi_imports(&mut self) {
        // Define the fd_write function type: (i32, i32, i32, i32) -> i32
        let fd_write_type_idx = self.types.len() as u32;
        {
            let ty = self.types.ty();
            ty.function(
                vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
                vec![ValType::I32],
            );
        }

        // Import fd_write from wasi_snapshot_preview1
        self.imports.import(
            "wasi_snapshot_preview1",
            "fd_write",
            EntityType::Function(fd_write_type_idx),
        );

        // Store the function index (imports come before other functions)
        self.fd_write_idx = Some(self.func_count);
        self.func_count += 1;

        // Add 1 page of memory (64KB)
        self.memory.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
    }

    /// Collect all variable declarations from statements
    /// #TODO: move variable collection to IR
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
            Stmt::For { iter_var, iter_expr, body, .. } => {
                let name = iter_var.0.clone();
                let ty = iter_var.1.clone();
                if !vars.iter().any(|(n, _)| *n == name) {
                    vars.push((name.clone(), ty.clone()));
                }

                // Add system iterator variable
                if !vars.iter().any(|(n, _)| *n == "system_iter") {
                    vars.push(("system_iter".to_string(), Type::Int));
                }

                // Add temporary vector variable to hold the iterable
                let vector_local_name = format!("__for_vec_{}", iter_var.0);
                if !vars.iter().any(|(n, _)| n == &vector_local_name) {
                    vars.push((vector_local_name, Type::Vector(Box::new(ty))));
                }

                // Add length variable
                if !vars.iter().any(|(n, _)| *n == "__for_len") {
                    vars.push(("__for_len".to_string(), Type::Int));
                }

                // Collect vars from loop body
                for s in body {
                    self.collect_vars_from_stmt(s, vars);
                }
            }
            Stmt::ExprStmt(expr) => {
                self.collect_print_temps_from_expr(expr, vars);
            }

            _ => {}
        }
    }

    fn collect_print_temps_from_expr(&self, expr: &IRExpr, vars: &mut Vec<(String, Type)>) {
        match &expr.kind {
            IRExprKind::BuiltinCall { builtin, args } => {
                if matches!(builtin, BuiltinKind::Print) {
                    // Add temporary variables for print function
                    let temps = vec![
                        ("__print_num", Type::Int),
                        ("__print_is_negative", Type::Int),
                        ("__print_write_pos", Type::Int),
                        ("__print_digit_count", Type::Int),
                    ];
                    for (name, ty) in temps {
                        if !vars.iter().any(|(n, _)| n == name) {
                            vars.push((name.to_string(), ty));
                        }
                    }
                }
                // Recursively check arguments
                for arg in args {
                    self.collect_print_temps_from_expr(arg, vars);
                }
            }
            IRExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_print_temps_from_expr(arg, vars);
                }
            }
            IRExprKind::Binary { left, right, .. } => {
                self.collect_print_temps_from_expr(left, vars);
                self.collect_print_temps_from_expr(right, vars);
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

        // sign of bad design
        if(matches!(op, BinaryOp::Range)) {
            self.gen_range(func, ctx, left, right);
            return;
        }

        self.gen_expr(func, ctx, left);
        self.gen_expr(func, ctx, right);


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
            BinaryOp::Greater => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32GtS),
                    Type::Float => func.instruction(&Instruction::F32Gt),
                    Type::Double => func.instruction(&Instruction::F64Gt),
                    _ => func.instruction(&Instruction::I32GtS),
                };
            },
            BinaryOp::GreaterEqual => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32GeS),
                    Type::Float => func.instruction(&Instruction::F32Ge),
                    Type::Double => func.instruction(&Instruction::F64Ge),
                    _ => func.instruction(&Instruction::I32GeS),
                };
            },
            BinaryOp::Equality => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Eq),
                    Type::Float => func.instruction(&Instruction::F32Eq),
                    Type::Double => func.instruction(&Instruction::F64Eq),
                    _ => func.instruction(&Instruction::I32Eq),
                };
            }
            BinaryOp::Or => {
                // booleans are represented as i32 0/1
                func.instruction(&Instruction::I32Or);
            }
            BinaryOp::And => {
                func.instruction(&Instruction::I32And);
            }

            // #TODO: add if statement
            _ => {}
        }
    }

    fn gen_range(&mut self, func: &mut Function, ctx: &LocalContext, left: &IRExpr, right: &IRExpr) {
        let start = match &left.kind {
            IRExprKind::Number(num) => {
                 num.parse::<i32>().unwrap()
            }
            _ => panic!("Range start must be a number")
        };
        let end = match &right.kind {
            IRExprKind::Number(num) => {
                num.parse::<i32>().unwrap()
            }
            _ => panic!("Range end must be a number")
        };
        for i in start..(end + 1) {
            func.instruction(&Instruction::I32Const(i));
        }
        let storage = self.storage_type_for(&Type::Int);
        let array_type_index = self.ensure_array_type(&storage);
        let array_size = (end - start + 1) as u32;
        func.instruction(&Instruction::ArrayNewFixed { array_type_index, array_size });

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
            IRExprKind::BuiltinCall { builtin, args } => {
                self.compile_builtin_call(func, ctx, builtin, args);
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

    fn compile_builtin_call(
        &mut self,
        func: &mut Function,
        ctx: &LocalContext,
        builtin: &BuiltinKind,
        args: &[IRExpr],
    ) {
        match builtin {
            BuiltinKind::Print => {
                // Only support printing integers for now
                if args.len() != 1 {
                    return;
                }

                // Memory layout:
                // 0-11: buffer for integer-to-string conversion (max 11 chars for i32)
                // 12: newline character
                // 16-23: iovec[0] structure (ptr, len) for number
                // 24-31: iovec[1] structure (ptr, len) for newline
                // 32-35: nwritten return value

                const BUFFER_PTR: i32 = 0;
                const NEWLINE_PTR: i32 = 12;
                const IOVEC_PTR: i32 = 16;
                const NWRITTEN_PTR: i32 = 32;

                // Store newline character at position 12
                func.instruction(&Instruction::I32Const(NEWLINE_PTR));
                func.instruction(&Instruction::I32Const(10)); // '\n'
                func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));

                // Evaluate the argument (puts integer on stack)
                self.gen_expr(func, ctx, &args[0]);

                // Convert integer to string and store in memory
                // We'll use a simple algorithm: repeatedly divide by 10
                // Get local indices for temporary variables
                let num_local = ctx.get_local("__print_num").expect("__print_num local not found");
                let is_negative_local = ctx.get_local("__print_is_negative").expect("__print_is_negative local not found");
                let write_pos_local = ctx.get_local("__print_write_pos").expect("__print_write_pos local not found");
                let digit_count_local = ctx.get_local("__print_digit_count").expect("__print_digit_count local not found");

                // num = value from stack
                func.instruction(&Instruction::LocalSet(num_local));

                // Check if negative
                func.instruction(&Instruction::LocalGet(num_local));
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32LtS);
                func.instruction(&Instruction::LocalSet(is_negative_local));

                // If negative, negate the number
                func.instruction(&Instruction::Block(BlockType::Empty));
                func.instruction(&Instruction::LocalGet(is_negative_local));
                func.instruction(&Instruction::I32Eqz);
                func.instruction(&Instruction::BrIf(0));
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::LocalGet(num_local));
                func.instruction(&Instruction::I32Sub);
                func.instruction(&Instruction::LocalSet(num_local));
                func.instruction(&Instruction::End);

                // Initialize write_pos to end of buffer (BUFFER_PTR + 11)
                func.instruction(&Instruction::I32Const(BUFFER_PTR + 11));
                func.instruction(&Instruction::LocalSet(write_pos_local));

                // Initialize digit_count to 0
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::LocalSet(digit_count_local));

                // Convert digits (right to left)
                func.instruction(&Instruction::Block(BlockType::Empty));
                func.instruction(&Instruction::Loop(BlockType::Empty));

                // write_pos--
                func.instruction(&Instruction::LocalGet(write_pos_local));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Sub);
                func.instruction(&Instruction::LocalTee(write_pos_local));

                // digit = num % 10
                func.instruction(&Instruction::LocalGet(num_local));
                func.instruction(&Instruction::I32Const(10));
                func.instruction(&Instruction::I32RemS);

                // digit_char = '0' + digit
                func.instruction(&Instruction::I32Const(48)); // '0'
                func.instruction(&Instruction::I32Add);

                // memory[write_pos] = digit_char
                func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));

                // digit_count++
                func.instruction(&Instruction::LocalGet(digit_count_local));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(digit_count_local));

                // num = num / 10
                func.instruction(&Instruction::LocalGet(num_local));
                func.instruction(&Instruction::I32Const(10));
                func.instruction(&Instruction::I32DivS);
                func.instruction(&Instruction::LocalTee(num_local));

                // Continue loop if num != 0 (br 0 continues the loop)
                func.instruction(&Instruction::BrIf(0));
                func.instruction(&Instruction::End); // end loop
                func.instruction(&Instruction::End); // end block

                // If negative, add '-' sign
                func.instruction(&Instruction::Block(BlockType::Empty));
                func.instruction(&Instruction::LocalGet(is_negative_local));
                func.instruction(&Instruction::I32Eqz);
                func.instruction(&Instruction::BrIf(0));

                // write_pos--
                func.instruction(&Instruction::LocalGet(write_pos_local));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Sub);
                func.instruction(&Instruction::LocalTee(write_pos_local));

                // memory[write_pos] = '-'
                func.instruction(&Instruction::I32Const(45)); // '-'
                func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));

                // digit_count++
                func.instruction(&Instruction::LocalGet(digit_count_local));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(digit_count_local));

                func.instruction(&Instruction::End);

                // Setup first iovec (number string)
                // iovec[0].ptr = write_pos
                func.instruction(&Instruction::I32Const(IOVEC_PTR));
                func.instruction(&Instruction::LocalGet(write_pos_local));
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                // iovec[0].len = digit_count
                func.instruction(&Instruction::I32Const(IOVEC_PTR + 4));
                func.instruction(&Instruction::LocalGet(digit_count_local));
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                // Call fd_write for the number (1 iovec)
                func.instruction(&Instruction::I32Const(1)); // stdout
                func.instruction(&Instruction::I32Const(IOVEC_PTR));
                func.instruction(&Instruction::I32Const(1)); // 1 iovec (just the number)
                func.instruction(&Instruction::I32Const(NWRITTEN_PTR));

                if let Some(fd_write_idx) = self.fd_write_idx {
                    func.instruction(&Instruction::Call(fd_write_idx));
                    func.instruction(&Instruction::Drop);
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }

                // Setup second iovec (newline)
                func.instruction(&Instruction::I32Const(IOVEC_PTR));
                func.instruction(&Instruction::I32Const(NEWLINE_PTR));
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                func.instruction(&Instruction::I32Const(IOVEC_PTR + 4));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                // Call fd_write for the newline (1 iovec)
                func.instruction(&Instruction::I32Const(1)); // stdout
                func.instruction(&Instruction::I32Const(IOVEC_PTR));
                func.instruction(&Instruction::I32Const(1)); // 1 iovec (just the newline)
                func.instruction(&Instruction::I32Const(NWRITTEN_PTR));

                if let Some(fd_write_idx) = self.fd_write_idx {
                    func.instruction(&Instruction::Call(fd_write_idx));
                    func.instruction(&Instruction::Drop);
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }

                // Push a dummy value so ExprStmt can drop it
                func.instruction(&Instruction::I32Const(0));
            }
            BuiltinKind::C | BuiltinKind::List => {
                // These are handled by creating vector literals
                // Generate a vector from the arguments
                for arg in args {
                    self.gen_expr(func, ctx, arg);
                }
                if !args.is_empty() {
                    let element_ty = &args[0].ty;
                    let storage = self.storage_type_for(element_ty);
                    let array_type_index = self.ensure_array_type(&storage);
                    func.instruction(&Instruction::ArrayNewFixed {
                        array_type_index,
                        array_size: args.len() as u32,
                    });
                } else {
                    // Empty vector - push null reference
                    func.instruction(&Instruction::RefNull(HeapType::ANY));
                }
            }
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
            },
            Stmt::If { condition, then_branch, else_branch } => {
                self.gen_expr(func, ctx, condition);
                func.instruction(&Instruction::If(BlockType::Empty));
                for stmt in then_branch {
                    self.gen_stmt(func, ctx, stmt, ret_has_value);
                }

                if(else_branch.is_some()) {
                    func.instruction(&Instruction::Else);
                    for stmt in else_branch.as_ref().unwrap() {
                        self.gen_stmt(func, ctx, stmt, ret_has_value);
                    }
                }
                func.instruction(&Instruction::End);

            }


            Stmt::For { iter_var, iter_expr, body } => {
                let var_name = iter_var.0.clone();
                let var_ty = iter_var.1.clone();
                let system_iter = ctx.get_local("system_iter").unwrap();
                let iter_object = ctx.get_local(&var_name).unwrap();

                // Create a local variable to hold the vector we're iterating over
                // We need to add this to the context if it doesn't exist
                let vector_local_name = format!("__for_vec_{}", var_name);
                let vector_local = ctx.get_local(&vector_local_name)
                    .expect("Vector local should have been collected");

                // Generate the vector to iterate over
                self.gen_expr(func, ctx, iter_expr);
                func.instruction(&Instruction::LocalSet(vector_local));

                // Get the length of the vector
                func.instruction(&Instruction::LocalGet(vector_local));
                func.instruction(&Instruction::ArrayLen);
                let total_iter_local = ctx.get_local("__for_len")
                    .expect("Length local should have been collected");
                func.instruction(&Instruction::LocalSet(total_iter_local));

                // Initialize loop counter to 0
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::LocalSet(system_iter));

                // Block for break
                func.instruction(&Instruction::Block(BlockType::Empty));
                // Loop
                func.instruction(&Instruction::Loop(BlockType::Empty));

                // Get current element from vector: vector[system_iter]
                func.instruction(&Instruction::LocalGet(vector_local));
                func.instruction(&Instruction::LocalGet(system_iter));
                func.instruction(&Instruction::ArrayGet(self.ensure_array_type(&self.storage_type_for(&var_ty))));
                func.instruction(&Instruction::LocalSet(iter_object));

                // Execute loop body
                for stmt in body {
                    self.gen_stmt(func, ctx, stmt, ret_has_value);
                }

                // Increment counter
                func.instruction(&Instruction::LocalGet(system_iter));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalTee(system_iter));

                // Check if we should continue (counter < total_iter)
                func.instruction(&Instruction::LocalGet(total_iter_local));
                func.instruction(&Instruction::I32LtS);
                func.instruction(&Instruction::BrIf(0)); // Continue loop

                func.instruction(&Instruction::End); // End loop
                func.instruction(&Instruction::End); // End block
            }
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
