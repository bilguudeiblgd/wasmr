use crate::ast::Type;
use crate::ir::{IRExpr, IRExprKind};
use wasm_encoder::{Function, HeapType, Instruction};

use super::{local_context::LocalContext, WasmGenerator};

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
                // Check if this is a function name being used as a value
                if let Some(&func_idx) = self.func_indices.get(name) {
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
            IRExprKind::Unit | _ => {
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
                    // Generate arguments first
                    for arg in args {
                        self.gen_expr(func, ctx, arg);
                    }
                    let func_idx = self.func_indices[name];
                    func.instruction(&Instruction::Call(func_idx));
                } else {
                    // Function stored in a variable - indirect call via call_ref
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
