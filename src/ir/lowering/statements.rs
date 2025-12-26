use super::super::type_resolver::FunctionCtx;
use super::super::types::{IRBlock, IRExpr, IRExprKind, IRStmt, TyResult, TypeError};
use super::utils::{ensure_ty, types_compatible};
use crate::ast::{Block, Expr as AstExpr, Stmt as AstStmt};
use crate::types::{Param, ParamKind, Type};
use crate::types::Type::Vector;

impl<'a> super::context::LowerCtx<'a> {
    pub(super) fn lower_stmt(&mut self, stmt: AstStmt) -> TyResult<IRStmt> {
        match stmt {
            AstStmt::ExprStmt(e) => {
                let e = self.lower_expr(e)?;
                Ok(IRStmt::ExprStmt(e))
            }
            AstStmt::VarAssign {
                name,
                x_type,
                value,
                is_super_assign,
            } => {
                // Special-case: function literal on RHS defines a named function
                if let AstExpr::FunctionDef {
                    params,
                    return_type,
                    body,
                } = value
                {
                    // Use Type::Any as placeholder for inference if no return type specified
                    let declared_ret_ty = return_type;
                    let placeholder_ret_ty = declared_ret_ty.clone().unwrap_or(Type::Any);

                    // Extract base Param for type signature (without defaults)
                    let type_params: Vec<Param> = params.iter().map(|pd| pd.param.clone()).collect();

                    // Create function type with placeholder
                    let func_ty = Type::Function {
                        params: type_params.clone(),
                        return_type: Box::new(placeholder_ret_ty.clone()),
                    };

                    // Store function in current scope (before processing body)
                    // This enables: 1) recursive calls, 2) nested function access
                    if is_super_assign {
                        self.tr.super_assign(&name, func_ty)?;
                    } else {
                        self.tr.define_var(name.clone(), func_ty);
                    }

                    // Store parameter definitions for named argument resolution
                    self.tr.function_param_defs.insert(name.clone(), params.clone());

                    // Enter NEW function scope
                    self.tr.enter_scope();

                    // Add parameters to new function scope
                    for p in &params {
                        if let ParamKind::Normal(param_ty) = &p.param.kind {
                            self.tr.define_var(p.param.name.clone(), param_ty.clone());
                        }
                    }

                    // Set function context
                    let saved_fn = self.tr.current_function.clone();
                    let varargs_name = params.iter().find_map(|p| {
                        if matches!(p.param.kind, ParamKind::VarArgs) {
                            Some(p.param.name.clone())
                        } else {
                            None
                        }
                    });
                    self.tr.current_function = Some(FunctionCtx {
                        name: name.clone(),
                        return_type: placeholder_ret_ty.clone(),
                        varargs_name,
                    });

                    // Lower body (can access parent function variables via scope stack)
                    let body_ir = self.lower_ast_block(body)?;

                    // Infer return type from body if not explicitly declared
                    let final_ret_ty = if let Some(explicit_ty) = declared_ret_ty {
                        // Explicit return type: validate tail expression matches
                        if explicit_ty != Type::Void && body_ir.tail_expr.is_some() {
                            if !types_compatible(&body_ir.ty, &explicit_ty) {
                                return Err(TypeError::TypeMismatch {
                                    expected: explicit_ty.clone(),
                                    found: body_ir.ty.clone(),
                                    context: format!("function '{}' body", name),
                                });
                            }
                        }
                        explicit_ty
                    } else {
                        // No explicit return type: infer from tail expression
                        // If no tail expression, function returns nothing (Void)
                        if body_ir.tail_expr.is_some() {
                            body_ir.ty.clone()
                        } else {
                            Type::Void
                        }
                    };

                    // Exit function scope
                    self.tr.exit_scope();
                    self.tr.current_function = saved_fn;

                    // Update the function's type in the environment with the final inferred return type
                    // This ensures that calls to this function see the correct return type
                    let final_func_ty = Type::Function {
                        params: type_params.clone(),
                        return_type: Box::new(final_ret_ty.clone()),
                    };
                    if is_super_assign {
                        self.tr.super_assign(&name, final_func_ty)?;
                    } else {
                        self.tr.define_var(name.clone(), final_func_ty);
                    }

                    return Ok(IRStmt::FunctionDef {
                        name,
                        params: type_params,
                        return_type: final_ret_ty,
                        body: body_ir,
                        metadata: None,
                    });
                }
                let val = self.lower_expr(value)?;
                let inferred = val.ty.clone();
                let final_ty = match x_type {
                    Some(t) => {
                        // allow simple numeric promotions into declared type
                        let promoted = self.tr.promote_numeric(&inferred, &t);
                        if types_compatible(&promoted, &t) {
                            t
                        } else {
                            return Err(TypeError::TypeMismatch {
                                expected: t,
                                found: inferred,
                                context: format!("let {} = <expr>", name),
                            });
                        }
                    }
                    None => inferred,
                };

                // Regular variable assignment - handle super assignment
                if is_super_assign {
                    // Superassignment: find variable in parent function scopes
                    self.tr.super_assign(&name, final_ty.clone())?;
                } else {
                    // Regular assignment: define in current function scope
                    self.tr.define_var(name.clone(), final_ty.clone());
                }

                Ok(IRStmt::VarAssign {
                    name,
                    ty: final_ty.clone(),
                    value: ensure_ty(val, final_ty),
                    is_super_assign,
                })
            }
            AstStmt::Return(opt) => {
                let (func_name, func_ret) = match &self.tr.current_function {
                    Some(ctx) => (ctx.name.clone(), ctx.return_type.clone()),
                    None => ("<top-level>".to_string(), Type::Void),
                };
                let ir_expr = if let Some(e) = opt {
                    let e = self.lower_expr(e)?;
                    if func_ret == Type::Void && e.ty != Type::Void {
                        return Err(TypeError::TypeMismatch {
                            expected: Type::Void,
                            found: e.ty,
                            context: format!("return in {}", func_name),
                        });
                    }
                    let e2 = ensure_ty(e, func_ret.clone());
                    e2
                } else {
                    if func_ret != Type::Void {
                        return Err(TypeError::MissingReturnValue {
                            function: func_name,
                            expected: func_ret,
                        });
                    }
                    IRExpr {
                        kind: IRExprKind::Unit,
                        ty: Type::Void,
                    }
                };
                Ok(IRStmt::Return(ir_expr))
            }
            AstStmt::Block(block) => {
                let ir_block = self.lower_ast_block(block)?;
                Ok(IRStmt::Block(ir_block))
            }
            AstStmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_ir = self.lower_expr(condition)?;
                let then_ir = self.lower_ast_block(then_branch)?;
                let else_ir = else_branch.map(|b| self.lower_ast_block(b)).transpose()?;

                // Determine the result type of the if expression
                let result_ty = match &else_ir {
                    Some(else_block) => {
                        // Both branches exist - check if they have compatible types
                        if types_compatible(&then_ir.ty, &else_block.ty) {
                            then_ir.ty.clone()
                        } else {
                            // Branches have incompatible types - if expression is Void
                            Type::Void
                        }
                    }
                    None => {
                        // No else branch - if expression is always Void
                        Type::Void
                    }
                };

                Ok(IRStmt::If {
                    condition: cond_ir,
                    then_branch: then_ir,
                    else_branch: else_ir,
                    result_ty,
                })
            }

            AstStmt::For {
                iter_name,
                iter_vector,
                body,
            } => {
                let iter_expr = self.lower_expr(iter_vector)?;
                if let Vector(inner_box) = &iter_expr.ty {
                    let inner_ty = (*inner_box).clone();
                    // Loop iterator is function-scoped (visible throughout function)
                    self.tr.define_var(iter_name.clone(), *inner_ty.clone());
                    // Lower the block (tail expression will be dropped or converted to ExprStmt during codegen)
                    let mut body_block = self.lower_ast_block(body)?;
                    // If there's a tail expression, convert it to an ExprStmt
                    if let Some(tail) = body_block.tail_expr.take() {
                        body_block.stmts.push(IRStmt::ExprStmt(*tail));
                    }
                    return Ok(IRStmt::For {
                        iter_var: (iter_name, *inner_ty),
                        iter_expr,
                        body: body_block,
                    });
                }
                if matches!(&iter_expr.ty, Vector(_)) {}

                Err(TypeError::TypeMismatch {
                    expected: Vector(Type::Int.into()),
                    found: iter_expr.ty,
                    context: format!("for loop iterator {}", iter_name),
                })
            }

            AstStmt::While { condition, body } => {
                let cond_ir = self.lower_expr(condition)?;
                // Lower the block (tail expression will be dropped or converted to ExprStmt during codegen)
                let mut body_block = self.lower_ast_block(body)?;
                // If there's a tail expression, convert it to an ExprStmt
                if let Some(tail) = body_block.tail_expr.take() {
                    body_block.stmts.push(IRStmt::ExprStmt(*tail));
                }
                Ok(IRStmt::While {
                    condition: cond_ir,
                    body: body_block,
                })
            }
            AstStmt::IndexAssign {
                target,
                index,
                value,
            } => {
                let target_ir = self.lower_expr(target)?;
                let index_ir = self.lower_expr(index)?;
                let value_ir = self.lower_expr(value)?;

                match &target_ir.ty {
                    Type::Vector(elem_ty_box) => {
                        let elem_ty = (**elem_ty_box).clone();

                        // Validate index is Int
                        if index_ir.ty != Type::Int {
                            return Err(TypeError::InvalidIndexType {
                                index_type: index_ir.ty,
                                context: "vector index assignment".to_string(),
                            });
                        }

                        // Validate value matches element type
                        let value_ir2 = ensure_ty(value_ir, elem_ty.clone());
                        if value_ir2.ty != elem_ty {
                            return Err(TypeError::TypeMismatch {
                                expected: elem_ty,
                                found: value_ir2.ty,
                                context: "index assignment value".to_string(),
                            });
                        }

                        Ok(IRStmt::IndexAssign {
                            target: target_ir,
                            index: index_ir,
                            value: value_ir2,
                        })
                    }
                    other => Err(TypeError::InvalidIndexTarget {
                        target_type: other.clone(),
                        context: "index assignment target must be vector".to_string(),
                    }),
                }
            }
        }
    }

}
