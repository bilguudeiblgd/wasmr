use super::types::types_compatible;
use super::LowerCtx;
use crate::ast::{Block, Expr as AstExpr, Stmt as AstStmt};
use crate::ir::type_resolver::FunctionCtx;
use crate::ir::types::{IRBlock, IRExpr, IRExprKind, IRStmt, TyResult, TypeError};
use crate::types::Type::Vector;
use crate::types::{Param, ParamKind, Type};

/// Type coercion helper - creates Cast nodes for implicit type conversions
/// The actual conversion logic will be implemented in codegen (using runtime functions for vectors)
pub(super) fn ensure_ty(e: IRExpr, want: Type) -> IRExpr {
    if e.ty == want {
        return e;
    }

    // Scalar numeric conversions
    match (&e.ty, &want) {
        // Int <-> Double
        (Type::Int, Type::Double) | (Type::Double, Type::Int) => {
            return IRExpr {
                kind: IRExprKind::Cast {
                    expr: Box::new(e.clone()),
                    from: e.ty.clone(),
                    to: want.clone(),
                },
                ty: want,
            };
        }
        // Double -> Logical (boolean casting from double)
        (Type::Double, Type::Logical) => {
            return IRExpr {
                kind: IRExprKind::Cast {
                    expr: Box::new(e.clone()),
                    from: e.ty.clone(),
                    to: Type::Logical,
                },
                ty: Type::Logical,
            };
        }
        // Int -> Logical (boolean casting from int)
        (Type::Int, Type::Logical) => {
            return IRExpr {
                kind: IRExprKind::Cast {
                    expr: Box::new(e.clone()),
                    from: e.ty.clone(),
                    to: Type::Logical,
                },
                ty: Type::Logical,
            };
        }
        // Logical -> Int (numeric promotion)
        (Type::Logical, Type::Int) => {
            return IRExpr {
                kind: IRExprKind::Cast {
                    expr: Box::new(e.clone()),
                    from: e.ty.clone(),
                    to: Type::Int,
                },
                ty: Type::Int,
            };
        }
        // Logical -> Double (numeric promotion)
        (Type::Logical, Type::Double) => {
            return IRExpr {
                kind: IRExprKind::Cast {
                    expr: Box::new(e.clone()),
                    from: e.ty.clone(),
                    to: Type::Double,
                },
                ty: Type::Double,
            };
        }
        // Vector conversions (element-wise casting)
        // The codegen will generate calls to runtime functions that loop through elements
        (Type::Vector(from_elem), Type::Vector(to_elem)) if from_elem != to_elem => {
            // Allow vector<int> <-> vector<double> and logical promotions
            match (from_elem.as_ref(), to_elem.as_ref()) {
                (Type::Int, Type::Double) | (Type::Double, Type::Int)
                | (Type::Logical, Type::Int) | (Type::Int, Type::Logical)
                | (Type::Logical, Type::Double) | (Type::Double, Type::Logical) => {
                    return IRExpr {
                        kind: IRExprKind::Cast {
                            expr: Box::new(e.clone()),
                            from: e.ty.clone(),
                            to: want.clone(),
                        },
                        ty: want,
                    };
                }
                // Could extend to other vector conversions in the future
                _ => {}
            }
        }
        _ => {}
    }

    // No conversion available - return as-is
    e
}

impl<'a> LowerCtx<'a> {
    /// Lower a list of AST statements to IR statements
    pub(super) fn lower_block(&mut self, stmts: Vec<AstStmt>) -> TyResult<Vec<IRStmt>> {
        let mut out = Vec::with_capacity(stmts.len());
        for s in stmts {
            out.push(self.lower_stmt(s)?);
        }
        Ok(out)
    }

    /// Lower a Block (statements + optional tail expression) to IRBlock
    pub(super) fn lower_ast_block(&mut self, block: Block) -> TyResult<IRBlock> {
        // Lower all statements
        let mut stmts = Vec::with_capacity(block.stmts.len());
        for s in block.stmts {
            stmts.push(self.lower_stmt(s)?);
        }

        // Lower tail expression if present
        let (tail_expr, ty) = if let Some(expr) = block.tail_expr {
            let ir_expr = self.lower_expr(*expr)?;
            let ty = ir_expr.ty.clone();
            (Some(Box::new(ir_expr)), ty)
        } else {
            (None, Type::Void)
        };

        Ok(IRBlock {
            stmts,
            tail_expr,
            ty,
        })
    }

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
                    let mut body_ir = self.lower_ast_block(body)?;

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

                    // Convert tail expression to explicit Return statement (or ExprStmt for void)
                    if let Some(tail) = body_ir.tail_expr.take() {
                        if final_ret_ty != Type::Void {
                            // Non-void return: wrap tail in Return statement
                            body_ir.stmts.push(IRStmt::Return(*tail));
                        } else {
                            // Void return: convert tail to ExprStmt if it produces a value
                            if tail.ty != Type::Void {
                                body_ir.stmts.push(IRStmt::ExprStmt(*tail));
                            }
                        }
                    }

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
                    None => {
                        // If variable already exists, preserve its existing type (reassignment)
                        // Otherwise use inferred type (initial assignment)
                        if let Some(existing_ty) = self.tr.lookup_var(&name) {
                            existing_ty
                        } else {
                            inferred
                        }
                    }
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

                        // Allow Double indices (R behavior) - cast to Int automatically
                        let index_ir = if index_ir.ty == Type::Double || index_ir.ty == Type::Int {
                            ensure_ty(index_ir, Type::Int)
                        } else {
                            return Err(TypeError::InvalidIndexType {
                                index_type: index_ir.ty,
                                context: "vector index assignment requires numeric type".to_string(),
                            });
                        };

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
