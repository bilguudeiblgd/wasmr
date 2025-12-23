use crate::types::Type;
use crate::ir::{IRExpr, IRExprKind};
use wasm_encoder::{Function, HeapType, Ieee32, Ieee64, Instruction};

use super::super::{context::LocalContext, WasmGenerator};

impl WasmGenerator {
    pub(crate) fn gen_expr(&mut self, func: &mut Function, ctx: &LocalContext, expr: &IRExpr) {
        match &expr.kind {
            IRExprKind::Number(n) => {
                self.gen_number(func, ctx, &expr.ty, n);
            }
            IRExprKind::Binary { left, op, right } => {
                self.gen_binary_op(func, ctx, op, left, right)
            }
            IRExprKind::Identifier(name) => {
                // Check if this is a captured variable (from parent scope, passed via environment)
                if let Some(captured_info) = ctx.get_captured(name) {
                    // Load from downcasted typed environment struct
                    // Use typed_env_local (downcasted) and concrete type for field access
                    let typed_env_local = ctx.typed_env_local()
                        .expect("Function with captured vars should have typed env local");
                    let concrete_env_type = ctx.concrete_env_type_idx()
                        .expect("Function with captured vars should have concrete env type");

                    func.instruction(&Instruction::LocalGet(typed_env_local));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: concrete_env_type,
                        field_index: captured_info.field_index
                    });

                    // If it's a mutable variable (super-assigned), it's stored as a ref cell
                    // Extract the actual value from the ref cell
                    if captured_info.is_mutable {
                        let ref_cell_type_idx = self.get_or_create_ref_cell_type(&captured_info.ty);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: ref_cell_type_idx,
                            field_index: 0,  // Ref cell has the value at field 0
                        });
                    }
                }
                // Check if this is a local variable that holds a function with environment
                else if ctx.has_local(name) && self.env_struct_types.contains_key(name) {
                    // This is a function with environment stored in a local variable - just load it
                    let idx = ctx.get_local(name).unwrap();
                    func.instruction(&Instruction::LocalGet(idx));
                }
                // Check if this is a function name being used as a value
                else if let Some(&func_idx) = self.func_indices.get(name) {
                    // Check if this function has captured variables (is a closure)
                    let has_captured_vars = self.func_metadata.get(name)
                        .map(|meta| !meta.captured_vars.is_empty())
                        .unwrap_or(false);

                    if has_captured_vars {
                        // This is a closure - create environment struct
                        let metadata = self.func_metadata.get(name).unwrap();
                        let (_closure_func_type_idx, _base_type_idx, concrete_type_idx) = *self.env_struct_types.get(name)
                            .expect("Function with captured vars should have env type");

                        // Field 0: function pointer
                        func.instruction(&Instruction::RefFunc(func_idx));

                        // Fields 1..N: captured variable values
                        for captured in &metadata.captured_vars {
                            // Get the captured variable from current context
                            if let Some(local_idx) = ctx.get_local(&captured.name) {
                                // Variable is in our local variables
                                func.instruction(&Instruction::LocalGet(local_idx));
                            } else if let Some(caller_captured) = ctx.get_captured(&captured.name) {
                                // Variable is in our captured vars (transitive capture)
                                // Use the downcasted typed environment to access concrete fields
                                let typed_env_local = ctx.typed_env_local()
                                    .expect("Function with captured vars should have typed env local");
                                let concrete_env_type = ctx.concrete_env_type_idx()
                                    .expect("Function with captured vars should have concrete env type");

                                // Load downcasted environment and extract the field
                                func.instruction(&Instruction::LocalGet(typed_env_local));
                                func.instruction(&Instruction::StructGet {
                                    struct_type_index: concrete_env_type,
                                    field_index: caller_captured.field_index,
                                });
                            } else {
                                // Variable not found - shouldn't happen
                                eprintln!("Warning: captured variable {} not found when creating closure", captured.name);
                                func.instruction(&Instruction::I32Const(0));
                            }
                        }

                        // Create the closure struct (automatically upcasted to base type)
                        func.instruction(&Instruction::StructNew(concrete_type_idx));
                    } else {
                        // Regular function without captures - just emit ref.func
                        func.instruction(&Instruction::RefFunc(func_idx));
                    }
                } else if let Some(idx) = ctx.get_local(name) {
                    // Local variable - might be in a reference cell
                    if ctx.needs_ref_cell(name) {
                        // Variable is in a reference cell - load the ref cell and extract the value
                        let ref_cell_type_idx = self.get_or_create_ref_cell_type(&expr.ty);
                        func.instruction(&Instruction::LocalGet(idx));
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: ref_cell_type_idx,
                            field_index: 0,
                        });
                    } else {
                        // Regular local variable
                        func.instruction(&Instruction::LocalGet(idx));
                    }
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
                // Generate target (leaves vector struct ref on stack)
                self.gen_expr(func, ctx, target);

                // Extract data field (field 0) from vector struct
                let elem_ty = match &target.ty {
                    Type::Vector(inner) => &**inner,
                    _ => panic!("Type checker should prevent non-vector indexing"),
                };
                let vector_struct_index = self.ensure_vector_struct_type(elem_ty);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: vector_struct_index,
                    field_index: 0,  // data field
                });

                // Generate index (leaves 1-based i32 on stack)
                self.gen_expr(func, ctx, index);

                // Convert 1-based to 0-based: index - 1
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Sub);

                // Get array type index for element type
                let storage = self.storage_type_for(elem_ty);
                let array_type_index = self.ensure_array_type(&storage);

                // Emit ArrayGet: pops [array_ref, i32_index], pushes [element_value]
                func.instruction(&Instruction::ArrayGet(array_type_index));
            }
            IRExprKind::XString(_s) => {
                func.instruction(&Instruction::I32Const(0));
            }
            IRExprKind::If { condition, then_branch, else_branch } => {
                // Generate condition
                self.gen_expr(func, ctx, condition);

                // Determine result type for WASM if instruction
                let block_type = if expr.ty != Type::Void {
                    wasm_encoder::BlockType::Result(self.wasm_valtype(&expr.ty))
                } else {
                    wasm_encoder::BlockType::Empty
                };

                // WASM if instruction with result type
                func.instruction(&Instruction::If(block_type));

                // Generate then branch
                for stmt in &then_branch.stmts {
                    self.gen_stmt(func, ctx, stmt, false);
                }
                // Generate tail expression if present
                if let Some(tail) = &then_branch.tail_expr {
                    self.gen_expr(func, ctx, tail);
                }

                // Generate else branch if present
                if let Some(else_block) = else_branch {
                    func.instruction(&Instruction::Else);
                    for stmt in &else_block.stmts {
                        self.gen_stmt(func, ctx, stmt, false);
                    }
                    // Generate tail expression if present
                    if let Some(tail) = &else_block.tail_expr {
                        self.gen_expr(func, ctx, tail);
                    }
                }

                func.instruction(&Instruction::End);
            }
            IRExprKind::Unit => {
                // nothing to push for void; use 0 by convention if a value is required, handled by caller
            }
        }
    }

    pub(crate) fn gen_vector_literal(&mut self, func: &mut Function, ctx: &LocalContext, vec: &Vec<IRExpr>) {
        let element_ty = match vec.iter().find(|a| !matches!(a.ty, Type::VarArgs)) {
            Some(expr) => &expr.ty,
            None => panic!("Vector literal must have at least one element"),
        };
        let storage = self.storage_type_for(element_ty);
        let array_type_index = self.ensure_array_type(&storage);

        // Create the data array
        vec.iter().for_each(|e| self.gen_expr(func, ctx, e));
        func.instruction(&Instruction::ArrayNewFixed {
            array_type_index,
            array_size: vec.len() as u32,
        });

        // Push length
        func.instruction(&Instruction::I32Const(vec.len() as i32));

        // Create vector struct: (struct (field data (array)) (field length i32))
        let vector_struct_index = self.ensure_vector_struct_type(element_ty);
        func.instruction(&Instruction::StructNew(vector_struct_index));
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
                        let (_closure_func_type_idx, _base_type_idx, concrete_type_idx) = *self.env_struct_types.get(name)
                            .expect("Function with captured vars should have env type");

                        // Field 0: function pointer (ref.func for the closure implementation)
                        let func_idx = self.func_indices[name];
                        func.instruction(&Instruction::RefFunc(func_idx));

                        // Fields 1..N: captured variable values
                        for captured in &metadata.captured_vars {
                            // Get the captured variable from current context
                            // It can be in either locals OR in our own captured vars (transitive)
                            if let Some(local_idx) = ctx.get_local(&captured.name) {
                                // Variable is in our local variables
                                func.instruction(&Instruction::LocalGet(local_idx));
                            } else if let Some(caller_captured) = ctx.get_captured(&captured.name) {
                                // Variable is in our captured vars (we also capture it transitively)
                                // Use the downcasted typed environment to access concrete fields
                                let typed_env_local = ctx.typed_env_local()
                                    .expect("Function with captured vars should have typed env local");
                                let concrete_env_type = ctx.concrete_env_type_idx()
                                    .expect("Function with captured vars should have concrete env type");

                                // Load downcasted environment and extract the field
                                func.instruction(&Instruction::LocalGet(typed_env_local));
                                func.instruction(&Instruction::StructGet {
                                    struct_type_index: concrete_env_type,
                                    field_index: caller_captured.field_index,
                                });
                            } else {
                                // Variable not found - this shouldn't happen
                                eprintln!("Warning: captured variable {} not found in caller context", captured.name);
                                func.instruction(&Instruction::I32Const(0));
                            }
                        }

                        // Create the environment struct (use concrete type)
                        func.instruction(&Instruction::StructNew(concrete_type_idx));
                    }

                    // Generate regular arguments
                    for arg in args {
                        self.gen_expr(func, ctx, arg);
                    }

                    let func_idx = self.func_indices[name];
                    func.instruction(&Instruction::Call(func_idx));
                } else {
                    // Function stored in a variable - could be a function with environment or bare function ref
                    // Check if this function signature corresponds to a closure
                    let closure_info = if let Type::Function { params, return_type } = &callee.ty {
                        let param_types: Vec<Type> = params
                            .iter()
                            .filter_map(|p| match &p.kind {
                                crate::types::ParamKind::Normal(ty) => Some(ty.clone()),
                                crate::types::ParamKind::VarArgs => None,
                            })
                            .collect();
                        self.get_closure_info_for_signature(&param_types, return_type)
                    } else {
                        None
                    };

                    if let Some((closure_func_type_idx, base_type_idx)) = closure_info {
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
                        // Use base type to extract field 0 (function pointer is in base type)
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: base_type_idx,
                            field_index: 0,
                        });

                        // 4. Now stack is: [env, args..., funcptr] - call it!
                        // Use the closure function type which includes the environment parameter as the first param
                        func.instruction(&Instruction::CallRef(closure_func_type_idx));
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
                                    crate::types::ParamKind::Normal(ty) => Some(ty.clone()),
                                    crate::types::ParamKind::VarArgs => None,
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
                            crate::types::ParamKind::Normal(ty) => Some(ty.clone()),
                            crate::types::ParamKind::VarArgs => None,
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

    pub(crate) fn gen_number(&mut self, func: &mut Function, ctx: &LocalContext, ty: &Type, numeric_string: &String) {
        match &ty {
            &Type::Int => {
                let num = numeric_string.parse::<i32>().unwrap();
                func.instruction(&Instruction::I32Const(num))
            }
        ,
            &Type::Float => {
                let ie_num = Ieee32::from(numeric_string.parse::<f32>().unwrap());
                func.instruction(&Instruction::F32Const(ie_num))
            }
            &Type::Double => {
                let ie_num = Ieee64::from(numeric_string.parse::<f64>().unwrap());
                func.instruction(&Instruction::F64Const(ie_num))
            }
            &Type::Logical => {
                // Boolean literals are represented as "0" (false) or "1" (true)
                let num = numeric_string.parse::<i32>().unwrap();
                func.instruction(&Instruction::I32Const(num))
            }
            _ => panic!("Type checker should prevent non-numeric expressions"),
        };
    }


    pub(crate) fn gen_varargs_expr(&mut self, func: &mut Function, ctx: &LocalContext) {
        if let Some(idx) = ctx.varargs_local() {
            func.instruction(&Instruction::LocalGet(idx));
        } else {
            func.instruction(&Instruction::RefNull(HeapType::ANY));
        }
    }
}
