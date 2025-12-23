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
            IRExprKind::BoolLiteral(b) => {
                // Booleans are represented as i32: 1 for true, 0 for false
                func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
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
            IRExprKind::Index { target, index } => {
                // Generate target (leaves vector struct ref on stack)
                self.gen_expr(func, ctx, target);

                // Extract data field (array ref) from the struct
                let elem_ty = match &target.ty {
                    Type::Vector(inner) => &**inner,
                    _ => panic!("Type checker should prevent non-vector indexing"),
                };
                let storage = self.storage_type_for(elem_ty);
                let struct_type_idx = self.ensure_vector_struct_type(&storage);

                // Stack: [struct_ref]
                // Extract field 0 (data - the array ref)
                func.instruction(&Instruction::StructGet {
                    struct_type_index: struct_type_idx,
                    field_index: 0,
                });

                // Stack: [array_ref]

                // Generate index (leaves 1-based i32 on stack)
                self.gen_expr(func, ctx, index);

                // Convert 1-based to 0-based: index - 1
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Sub);

                // Stack: [array_ref, i32_index]

                // Get array type index for element type
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
        // Generate all elements on the stack
        vec.iter().for_each(|e| self.gen_expr(func, ctx, e));

        let element_ty = match vec.iter().find(|a| !matches!(a.ty, Type::VarArgs)) {
            Some(expr) => &expr.ty,
            None => panic!("Vector literal must have at least one element"),
        };
        let storage = self.storage_type_for(element_ty);
        let array_type_index = self.ensure_array_type(&storage);
        let vec_len = vec.len() as u32;

        // Create the array
        func.instruction(&Instruction::ArrayNewFixed {
            array_type_index,
            array_size: vec_len,
        });

        // Stack: [array_ref]
        // Now create the vector struct: (struct (field data array) (field length i32))

        // Push length onto stack
        func.instruction(&Instruction::I32Const(vec_len as i32));

        // Stack: [array_ref, length]
        // Create the struct
        let struct_type_idx = self.ensure_vector_struct_type(&storage);
        func.instruction(&Instruction::StructNew(struct_type_idx));

        // Stack: [struct_ref]
    }

    pub(crate) fn gen_call(
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


    pub(crate) fn gen_varargs_expr(&mut self, func: &mut Function, ctx: &LocalContext) {
        if let Some(idx) = ctx.varargs_local() {
            func.instruction(&Instruction::LocalGet(idx));
        } else {
            func.instruction(&Instruction::RefNull(HeapType::ANY));
        }
    }
}
