use crate::ir::{IRStmt as Stmt};
use wasm_encoder::{BlockType, Function, Instruction};

use super::{local_context::LocalContext, WasmGenerator};

impl WasmGenerator {
    pub(crate) fn gen_stmt(
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
                name, ty: _, value, ..
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
            Stmt::IndexAssign { target, index, value } => {
                // Generate target (vector ref)
                self.gen_expr(func, ctx, target);

                // Generate index (1-based i32)
                self.gen_expr(func, ctx, index);

                // Convert 1-based to 0-based: index - 1
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Sub);

                // Generate value
                self.gen_expr(func, ctx, value);

                // Get array type index
                use crate::ast::Type;
                let elem_ty = match &target.ty {
                    Type::Vector(inner) => &**inner,
                    _ => panic!("Type checker should prevent non-vector indexing"),
                };
                let storage = self.storage_type_for(elem_ty);
                let array_type_index = self.ensure_array_type(&storage);

                // Emit ArraySet: pops [array_ref, i32_index, value], pushes nothing
                func.instruction(&Instruction::ArraySet(array_type_index));
            }
        }
    }
}
