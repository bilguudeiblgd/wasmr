use crate::ir::{IRStmt as Stmt};
use wasm_encoder::{BlockType, Function, Instruction};

use super::super::{context::LocalContext, WasmGenerator};

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
                // Only drop if the expression returns a value (not void)
                if !matches!(e.ty, crate::types::Type::Void) {
                    func.instruction(&Instruction::Drop);
                }
            }
            Stmt::VarAssign {
                name, ty, value, is_super_assign,
            } => {
                // Check if it's a captured variable from parent scope
                if let Some(captured) = ctx.get_captured(name) {
                    if *is_super_assign {
                        // Super-assignment to captured variable
                        // Use typed (downcasted) environment for field access
                        let typed_env_local = ctx.typed_env_local()
                            .expect("Function with captured vars should have typed env local");
                        let concrete_env_type = ctx.concrete_env_type_idx()
                            .expect("Function with captured vars should have concrete env type");

                        if captured.is_mutable {
                            // The captured variable is a ref cell
                            // 1. Load typed env and extract ref cell
                            func.instruction(&Instruction::LocalGet(typed_env_local));
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: concrete_env_type,
                                field_index: captured.field_index,
                            });

                            // 2. Generate value
                            self.gen_expr(func, ctx, value);

                            // 3. Set the ref cell's field
                            let ref_cell_type_idx = self.get_or_create_ref_cell_type(ty);
                            func.instruction(&Instruction::StructSet {
                                struct_type_index: ref_cell_type_idx,
                                field_index: 0,
                            });
                        } else {
                            // Direct field in environment struct (not wrapped in ref cell)
                            // 1. Load typed env
                            func.instruction(&Instruction::LocalGet(typed_env_local));

                            // 2. Generate value
                            self.gen_expr(func, ctx, value);

                            // 3. Set the field
                            func.instruction(&Instruction::StructSet {
                                struct_type_index: concrete_env_type,
                                field_index: captured.field_index,
                            });
                        }
                    } else {
                        // Initial assignment to captured variable - this shouldn't happen
                        // (captured vars are already defined in parent scope)
                        eprintln!("Warning: initial assignment to captured variable {} - this shouldn't happen", name);
                        self.gen_expr(func, ctx, value);
                        func.instruction(&Instruction::Drop);
                    }
                } else if let Some(idx) = ctx.get_local(name) {
                    if ctx.needs_ref_cell(name) {
                        // Variable is in a reference cell
                        if *is_super_assign {
                            // Super-assignment: update existing ref cell
                            // Load ref cell, push new value, update field 0
                            func.instruction(&Instruction::LocalGet(idx));
                            self.gen_expr(func, ctx, value);
                            let ref_cell_type_idx = self.get_or_create_ref_cell_type(ty);
                            func.instruction(&Instruction::StructSet {
                                struct_type_index: ref_cell_type_idx,
                                field_index: 0,
                            });
                        } else {
                            // Initial assignment: create new ref cell
                            self.gen_expr(func, ctx, value);
                            let ref_cell_type_idx = self.get_or_create_ref_cell_type(ty);
                            func.instruction(&Instruction::StructNew(ref_cell_type_idx));
                            func.instruction(&Instruction::LocalSet(idx));
                        }
                    } else {
                        // Regular variable
                        self.gen_expr(func, ctx, value);
                        func.instruction(&Instruction::LocalSet(idx));
                    }
                } else {
                    // Variable not found - drop the value
                    self.gen_expr(func, ctx, value);
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
            Stmt::Block(block) => {
                // Generate statements
                for s in &block.stmts {
                    self.gen_stmt(func, ctx, s, ret_has_value);
                }
                // Generate tail expression if present
                if let Some(tail) = &block.tail_expr {
                    self.gen_expr(func, ctx, tail);
                    // If the block has a non-void tail expression but we don't need the value, drop it
                    if !matches!(tail.ty, crate::types::Type::Void) && !ret_has_value {
                        func.instruction(&Instruction::Drop);
                    }
                }
            },
            Stmt::If { condition, then_branch, else_branch, result_ty } => {
                self.gen_expr(func, ctx, condition);

                // Determine if the if expression produces a value
                let has_result = !matches!(result_ty, crate::types::Type::Void);
                let block_type = if has_result {
                    // Map result type to WASM block type
                    BlockType::Result(self.wasm_valtype(result_ty))
                } else {
                    BlockType::Empty
                };

                func.instruction(&Instruction::If(block_type));

                // Generate then branch
                for stmt in &then_branch.stmts {
                    self.gen_stmt(func, ctx, stmt, ret_has_value);
                }
                // Generate tail expression if present
                if let Some(tail) = &then_branch.tail_expr {
                    self.gen_expr(func, ctx, tail);
                }

                if let Some(else_block) = else_branch {
                    func.instruction(&Instruction::Else);
                    for stmt in &else_block.stmts {
                        self.gen_stmt(func, ctx, stmt, ret_has_value);
                    }
                    // Generate tail expression if present
                    if let Some(tail) = &else_block.tail_expr {
                        self.gen_expr(func, ctx, tail);
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

                // Generate the vector struct to iterate over
                self.gen_expr(func, ctx, iter_expr);
                func.instruction(&Instruction::LocalSet(vector_local));

                // Get the length of the vector from struct field 1
                func.instruction(&Instruction::LocalGet(vector_local));
                let vector_struct_index = self.ensure_vector_struct_type(&var_ty);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: vector_struct_index,
                    field_index: 1,  // length field
                });
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

                // Get current element from vector: extract data array (field 0), then index into it
                func.instruction(&Instruction::LocalGet(vector_local));
                func.instruction(&Instruction::StructGet {
                    struct_type_index: vector_struct_index,
                    field_index: 0,  // data field
                });
                func.instruction(&Instruction::LocalGet(system_iter));
                func.instruction(&Instruction::ArrayGet(self.ensure_array_type(&self.storage_type_for(&var_ty))));
                func.instruction(&Instruction::LocalSet(iter_object));

                // Execute loop body
                for stmt in &body.stmts {
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

            Stmt::While { condition, body } => {
                // Block for break
                func.instruction(&Instruction::Block(BlockType::Empty));
                // Loop
                func.instruction(&Instruction::Loop(BlockType::Empty));

                // Evaluate condition
                self.gen_expr(func, ctx, condition);

                // If condition is false (0), break out of loop (br 1 breaks to outer block)
                func.instruction(&Instruction::I32Eqz);
                func.instruction(&Instruction::BrIf(1));

                // Execute loop body
                for stmt in &body.stmts {
                    self.gen_stmt(func, ctx, stmt, ret_has_value);
                }

                // Continue to next iteration (br 0 continues loop)
                func.instruction(&Instruction::Br(0));

                func.instruction(&Instruction::End); // End loop
                func.instruction(&Instruction::End); // End block
            }
            Stmt::IndexAssign { target, index, value } => {
                // Generate target (vector struct ref)
                self.gen_expr(func, ctx, target);

                // Extract data field (field 0) from vector struct
                use crate::types::Type;
                let elem_ty = match &target.ty {
                    Type::Vector(inner) => &**inner,
                    _ => panic!("Type checker should prevent non-vector indexing"),
                };
                let vector_struct_index = self.ensure_vector_struct_type(elem_ty);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: vector_struct_index,
                    field_index: 0,  // data field
                });

                // Generate index (1-based i32)
                self.gen_expr(func, ctx, index);

                // Convert 1-based to 0-based: index - 1
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Sub);

                // Generate value
                self.gen_expr(func, ctx, value);

                // Get array type index
                let storage = self.storage_type_for(elem_ty);
                let array_type_index = self.ensure_array_type(&storage);

                // Emit ArraySet: pops [array_ref, i32_index, value], pushes nothing
                func.instruction(&Instruction::ArraySet(array_type_index));
            }
        }
    }
}
