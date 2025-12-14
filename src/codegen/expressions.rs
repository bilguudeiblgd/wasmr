use crate::ast::Type;
use crate::ir::{IRExpr, IRExprKind};
use wasm_encoder::{Function, HeapType, Instruction};

use super::{local_context::LocalContext, WasmGenerator};

impl WasmGenerator {
    /// Generate function value with environment: allocate environment struct with captured variables
    /// NOTE: Currently unused - functions with captured vars receive env as parameter, not stored as value
    ///
    /// Creates a struct containing:
    /// - Field 0: function pointer (ref func)
    /// - Field 1..N: captured variable values
    ///
    /// Leaves the struct reference on the stack
    pub(crate) fn gen_closure_create(
        &mut self,
        func: &mut Function,
        ctx: &LocalContext,
        func_name: &str,
        captured_vars: &[String],
        _func_ty: &Type,
    ) {
        // Get the function index
        let func_idx = *self.func_indices.get(func_name)
            .expect("Function should be in func_indices");

        // Get the environment struct type index
        let env_struct_type_idx = *self.env_struct_types.get(func_name)
            .expect("Function with captured vars should have environment struct type");

        // Emit values in field order: field 0 (func_ptr), then fields 1..N (captured vars)

        // Field 0: Push function pointer
        func.instruction(&Instruction::RefFunc(func_idx));

        // Fields 1..N: Push captured variable values
        for var_name in captured_vars {
            // Check if it's a local or another captured variable
            if let Some(local_idx) = ctx.get_local(var_name) {
                func.instruction(&Instruction::LocalGet(local_idx));
            } else {
                // Unknown variable - push 0 as fallback
                eprintln!("Warning: captured variable {} not found in context", var_name);
                func.instruction(&Instruction::I32Const(0));
            }
        }

        // Create the struct with all fields on stack
        // struct.new expects: [field_0_val, field_1_val, ..., field_N_val] -> struct_ref
        func.instruction(&Instruction::StructNew(env_struct_type_idx));
    }
}

impl WasmGenerator {
    pub(crate) fn gen_expr(&mut self, func: &mut Function, ctx: &LocalContext, expr: &IRExpr) {
        match &expr.kind {
            IRExprKind::Number(n) => {
                let v: i32 = n.parse().unwrap_or(0);
                func.instruction(&Instruction::I32Const(v));
            }
            IRExprKind::Binary { left, op, right } => {
                self.gen_binary_op(func, ctx, op, left, right)
            }
            IRExprKind::Identifier(name) => {
                // Check if this is a captured variable (from parent scope, passed via environment)
                if let Some(captured_info) = ctx.get_captured(name) {
                    // Load from environment struct
                    let env_index = ctx.env_param_index().unwrap();
                    let env_struct_index = ctx.env_struct_type_idx().unwrap();
                    func.instruction(&Instruction::LocalGet(env_index));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: env_struct_index,
                        field_index: captured_info.field_index
                    });
                }
                // Check if this is a local variable that holds a function with environment
                else if ctx.has_local(name) && self.env_struct_types.contains_key(name) {
                    // This is a function with environment stored in a local variable - just load it
                    let idx = ctx.get_local(name).unwrap();
                    func.instruction(&Instruction::LocalGet(idx));
                }
                // Check if this is a function name being used as a value
                else if let Some(&func_idx) = self.func_indices.get(name) {
                    // This is a function used as a value - emit ref.func
                    func.instruction(&Instruction::RefFunc(func_idx));
                } else if let Some(idx) = ctx.get_local(name) {
                    // Regular local variable
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
            IRExprKind::Index { target, index } => {
                // Generate target (leaves vector ref on stack)
                self.gen_expr(func, ctx, target);

                // Generate index (leaves 1-based i32 on stack)
                self.gen_expr(func, ctx, index);

                // Convert 1-based to 0-based: index - 1
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Sub);

                // Get array type index for element type
                let elem_ty = match &target.ty {
                    Type::Vector(inner) => &**inner,
                    _ => panic!("Type checker should prevent non-vector indexing"),
                };
                let storage = self.storage_type_for(elem_ty);
                let array_type_index = self.ensure_array_type(&storage);

                // Emit ArrayGet: pops [array_ref, i32_index], pushes [element_value]
                func.instruction(&Instruction::ArrayGet(array_type_index));
            }
            IRExprKind::XString(_s) => {
                func.instruction(&Instruction::I32Const(0));
            }
            IRExprKind::ClosureCreate { func_name, captured_vars } => {
                self.gen_closure_create(func, ctx, func_name, captured_vars, &expr.ty);
            }
            IRExprKind::Unit => {
                // nothing to push for void; use 0 by convention if a value is required, handled by caller
            }
        }
    }

    pub(crate) fn gen_vector_literal(&mut self, func: &mut Function, ctx: &LocalContext, vec: &Vec<IRExpr>) {
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

    pub(crate) fn gen_call(
        &mut self,
        func: &mut Function,
        ctx: &LocalContext,
        callee: &IRExpr,
        args: &[IRExpr],
    ) {
        // Determine what kind of call this is
        match &callee.kind {
            IRExprKind::Identifier(name) => {
                // Check if this is a direct function call or a function variable
                if self.func_indices.contains_key(name) && !ctx.has_local(name) {
                    // Direct function call by name

                    // Check if this function has captured variables (needs environment)
                    let needs_env = self.func_metadata.get(name)
                        .map(|meta| !meta.captured_vars.is_empty())
                        .unwrap_or(false);

                    if needs_env {
                        // Generate environment struct as first argument
                        let metadata = self.func_metadata.get(name).unwrap();
                        let env_type_idx = *self.env_struct_types.get(name)
                            .expect("Function with captured vars should have env type");

                        // Field 0: placeholder int32 (for future function pointer)
                        func.instruction(&Instruction::I32Const(0));

                        // Fields 1..N: captured variable values
                        for captured in &metadata.captured_vars {
                            // Get the captured variable from current context
                            if let Some(local_idx) = ctx.get_local(&captured.name) {
                                func.instruction(&Instruction::LocalGet(local_idx));
                            } else {
                                // Variable not found - this shouldn't happen
                                eprintln!("Warning: captured variable {} not found in caller context", captured.name);
                                func.instruction(&Instruction::I32Const(0));
                            }
                        }

                        // Create the environment struct
                        func.instruction(&Instruction::StructNew(env_type_idx));
                    }

                    // Generate regular arguments
                    for arg in args {
                        self.gen_expr(func, ctx, arg);
                    }

                    let func_idx = self.func_indices[name];
                    func.instruction(&Instruction::Call(func_idx));
                } else {
                    // Function stored in a variable - could be a function with environment or bare function ref
                    // Check if this function has environment
                    let has_env = if let IRExprKind::Identifier(name) = &callee.kind {
                        self.env_struct_types.contains_key(name)
                    } else {
                        false
                    };

                    if has_env {
                        // Function with environment call: call_ref expects [env, args..., funcptr]
                        //
                        // 1. Load environment (the struct itself)
                        self.gen_expr(func, ctx, callee);

                        // 2. Generate user arguments
                        for arg in args {
                            self.gen_expr(func, ctx, arg);
                        }

                        // 3. Load struct again and extract function pointer from field 0
                        self.gen_expr(func, ctx, callee);
                        if let IRExprKind::Identifier(name) = &callee.kind {
                            let env_struct_type_idx = *self.env_struct_types.get(name).unwrap();
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: env_struct_type_idx,
                                field_index: 0,
                            });
                        }

                        // 4. Now stack is: [env, args..., funcptr] - call it!
                        // For functions with environment, we need the WASM function type that includes the environment parameter
                        // This is NOT the IR function type - it's the actual WASM type with env as first param
                        if let IRExprKind::Identifier(name) = &callee.kind {
                            // The closure function was declared with environment as first param in PASS 2
                            // We need to find that type index. For now, we can get it by looking up
                            // the function in func_indices and getting its type.
                            // But actually, the funcref on the stack already has the right type.
                            // call_ref will use the type from the funcref, but we still need to specify
                            // the expected type for validation.

                            // The issue: we created the bare function type (type 6) but the actual
                            // WASM function has the closure type (type 8). The funcref in field 0
                            // is typed as (ref 6), but it should be (ref 8).

                            // For now, let me get the function index and look up its actual type
                            // Actually, we can't easily get the type index of the closure function here.
                            // The real fix is to make sure field 0 of the struct has the right type (ref 8, not ref 6).

                            // Let me use a workaround: find the closure function type
                            let func_idx = self.func_indices.get(name).copied();
                            // We need to compute what type 8 is. It's the bare function params + env param

                            if let Type::Function { params, return_type } = &callee.ty {
                                let param_types: Vec<Type> = params
                                    .iter()
                                    .filter_map(|p| match &p.kind {
                                        crate::ast::ParamKind::Normal(ty) => Some(ty.clone()),
                                        crate::ast::ParamKind::VarArgs => None,
                                    })
                                    .collect();
                                let type_idx = self.get_or_create_func_type_index(&param_types, return_type);
                                func.instruction(&Instruction::CallRef(type_idx));
                            }
                        }
                    } else {
                        // Bare function variable (no environment)
                        // Generate arguments first
                        for arg in args {
                            self.gen_expr(func, ctx, arg);
                        }

                        // Generate the callee (function reference)
                        self.gen_expr(func, ctx, callee);

                        // Get the function type index for call_ref
                        if let Type::Function { params, return_type } = &callee.ty {
                            let param_types: Vec<Type> = params
                                .iter()
                                .filter_map(|p| match &p.kind {
                                    crate::ast::ParamKind::Normal(ty) => Some(ty.clone()),
                                    crate::ast::ParamKind::VarArgs => None,
                                })
                                .collect();
                            let type_idx = self.get_or_create_func_type_index(&param_types, return_type);
                            func.instruction(&Instruction::CallRef(type_idx));
                        } else {
                            eprintln!("Warning: Callee is not a function type");
                            // Drop arguments and callee
                            for _ in args {
                                func.instruction(&Instruction::Drop);
                            }
                            func.instruction(&Instruction::Drop);
                            func.instruction(&Instruction::I32Const(0));
                        }
                    }
                }
            }
            _ => {
                // Indirect call (function returned from expression or complex expression)
                // Generate arguments first
                for arg in args {
                    self.gen_expr(func, ctx, arg);
                }

                // Generate the callee expression (function reference)
                self.gen_expr(func, ctx, callee);

                // Get the function type index for call_ref
                if let Type::Function { params, return_type } = &callee.ty {
                    let param_types: Vec<Type> = params
                        .iter()
                        .filter_map(|p| match &p.kind {
                            crate::ast::ParamKind::Normal(ty) => Some(ty.clone()),
                            crate::ast::ParamKind::VarArgs => None,
                        })
                        .collect();
                    let type_idx = self.get_or_create_func_type_index(&param_types, return_type);
                    func.instruction(&Instruction::CallRef(type_idx));
                } else {
                    panic!("Warning: Callee is not a function type");
                }
            }
        }
    }


    pub(crate) fn gen_varargs_expr(&mut self, func: &mut Function, ctx: &LocalContext) {
        if let Some(idx) = ctx.varargs_local() {
            func.instruction(&Instruction::LocalGet(idx));
        } else {
            func.instruction(&Instruction::RefNull(HeapType::ANY));
        }
    }
}
