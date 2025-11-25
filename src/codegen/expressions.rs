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
