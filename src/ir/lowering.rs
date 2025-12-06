use crate::ast::Type::Vector;
use crate::ast::{BinaryOp, Expr as AstExpr, Expr, ParamKind, Stmt as AstStmt, Stmt, Type};
use super::types::{BuiltinKind, IRExpr, IRExprKind, IRStmt, TyResult, TypeError};
use super::type_resolver::{TypeResolver, FunctionCtx, BuiltinDescriptor};

/// Public IR facade that owns the lowering API. TypeResolver is injected as a dependency.
pub struct IR;

impl IR {
    pub fn from_ast(program: Vec<AstStmt>, resolver: &mut TypeResolver) -> TyResult<Vec<IRStmt>> {
        let mut lower = LowerCtx { tr: resolver };
        lower.lower_program(program)
    }
}

/// Internal lowering context that uses a borrowed TypeResolver for environments and type ops.
pub(crate) struct LowerCtx<'a> {
    pub(crate) tr: &'a mut TypeResolver,
}

impl<'a> LowerCtx<'a> {
    /// Lower a whole program (list of AST statements) to typed IR statements.
    fn lower_program(&mut self, program: Vec<AstStmt>) -> TyResult<Vec<IRStmt>> {
        // collect function signatures first so calls can be validated
        for s in &program {
            if let AstStmt::VarAssign { name, value, .. } = s {
                if let AstExpr::FunctionDef {
                    params,
                    return_type,
                    ..
                } = value
                {
                    let ret_ty = return_type.clone().unwrap_or(Type::Void);
                    self.tr.funcs.insert(name.clone(), (params.clone(), ret_ty));
                }
            }
        }

        let mut out = Vec::with_capacity(program.len());
        for s in program {
            out.push(self.lower_stmt(s)?);
        }
        Ok(out)
    }

    fn lower_block(&mut self, stmts: Vec<AstStmt>) -> TyResult<Vec<IRStmt>> {
        let mut out = Vec::with_capacity(stmts.len());
        for s in stmts {
            out.push(self.lower_stmt(s)?);
        }
        Ok(out)
    }

    fn lower_stmt(&mut self, stmt: AstStmt) -> TyResult<IRStmt> {
        match stmt {
            AstStmt::ExprStmt(e) => {
                let e = self.lower_expr(e)?;
                Ok(IRStmt::ExprStmt(e))
            }
            AstStmt::VarAssign {
                name,
                x_type,
                value,
            } => {
                // Special-case: function literal on RHS defines a named function
                if let AstExpr::FunctionDef {
                    params,
                    return_type,
                    body,
                } = value
                {
                    let ret_ty = return_type.unwrap_or(Type::Void);
                    // Set up new scope for function variables
                    let saved_vars = self.tr.vars.clone();
                    for p in &params {
                        if let ParamKind::Normal(param_ty) = &p.kind {
                            self.tr.vars.insert(p.name.clone(), param_ty.clone());
                        }
                    }
                    let saved_fn = self.tr.current_function.clone();
                    let varargs_name = params.iter().find_map(|p| {
                        if matches!(p.kind, ParamKind::VarArgs) {
                            Some(p.name.clone())
                        } else {
                            None
                        }
                    });
                    self.tr.current_function = Some(FunctionCtx {
                        name: name.clone(),
                        return_type: ret_ty.clone(),
                        varargs_name,
                    });

                    let body_ir = self.lower_block(body)?;

                    // Restore scope
                    self.tr.vars = saved_vars;
                    self.tr.current_function = saved_fn;

                    return Ok(IRStmt::FunctionDef {
                        name,
                        params,
                        return_type: ret_ty,
                        body: body_ir,
                    });
                }
                let val = self.lower_expr(value)?;
                let inferred = val.ty.clone();
                let final_ty = match x_type {
                    Some(t) => {
                        // allow simple numeric promotions into declared type
                        let promoted = self.tr.promote_numeric(&inferred, &t);
                        if promoted == t {
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
                self.tr.vars.insert(name.clone(), final_ty.clone());
                Ok(IRStmt::VarAssign {
                    name,
                    ty: final_ty.clone(),
                    value: ensure_ty(val, final_ty),
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
            AstStmt::Block(stmts) => {
                let body = self.lower_block(stmts)?;
                Ok(IRStmt::Block(body))
            }
            AstStmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                Ok(IRStmt::If {
                    condition: self.lower_expr(condition)?,
                    then_branch: self.lower_block(then_branch)?,
                    else_branch: else_branch.map(|b| self.lower_block(b)).transpose()? ,
                })
            },

            AstStmt::For {
                iter_name,
                iter_vector,
                body
            } => {
                let iter_expr = self.lower_expr(iter_vector)?;
                if let Vector(inner_box) = &iter_expr.ty {
                    let inner_ty = (*inner_box).clone();
                    self.tr.vars.insert(iter_name.clone(), *inner_ty.clone() );
                    return Ok(IRStmt::For {
                        iter_var: (iter_name, *inner_ty),
                        iter_expr,
                        body: self.lower_block(body)?,
                    });
                }
                if matches!(&iter_expr.ty, Vector(some)) {

                }

                Err(TypeError::TypeMismatch {
                    expected: Vector(Type::Int.into()),
                    found: iter_expr.ty,
                    context: format!("for loop iterator {}", iter_name),
                })
            }

            AstStmt::While { condition, body } => {
                let cond_ir = self.lower_expr(condition)?;
                let body_ir = self.lower_block(body)?;
                Ok(IRStmt::While {
                    condition: cond_ir,
                    body: body_ir,
                })
            }
            AstStmt::IndexAssign { target, index, value } => {
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

    fn lower_expr(&mut self, expr: AstExpr) -> TyResult<IRExpr> {
        match expr {
            AstExpr::Number(s) => {
                // Heuristic: integers have no dot; otherwise Double
                let ty = if s.contains('.') {
                    Type::Double
                } else {
                    Type::Int
                };
                Ok(IRExpr {
                    kind: IRExprKind::Number(s),
                    ty,
                })
            }
            AstExpr::Identifier(name) => {
                if let Some(t) = self.tr.vars.get(&name).cloned() {
                    Ok(IRExpr {
                        kind: IRExprKind::Identifier(name),
                        ty: t,
                    })
                } else if let Some((_, ret)) = self.tr.funcs.get(&name).cloned() {
                    Ok(IRExpr {
                        kind: IRExprKind::Identifier(name),
                        ty: ret,
                    })
                } else {
                    Err(TypeError::UnknownVariable(name))
                }
            }
            AstExpr::VarArgs => match &self.tr.current_function {
                Some(ctx) if ctx.varargs_name.is_some() => Ok(IRExpr {
                    kind: IRExprKind::VarArgs,
                    ty: Type::VarArgs,
                }),
                _ => Err(TypeError::UnknownVariable("...".to_string())),
            },
            AstExpr::XString(s) => Ok(IRExpr {
                kind: IRExprKind::XString(s),
                ty: Type::String,
            }),
            AstExpr::FunctionDef { .. } => Ok(IRExpr {
                // We currently do not materialize function references at runtime; treat as a value of FunctionRef type.
                kind: IRExprKind::Unit,
                ty: Type::FunctionRef,
            }),
            AstExpr::Grouping(inner) => self.lower_expr(*inner),
            AstExpr::Binary { left, op, right } => {
                let l = self.lower_expr(*left)?;
                let r = self.lower_expr(*right)?;
                match op {
                    BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => {
                        // #TODO: promotion for numerics inside vector or composite types
                        if (matches!(l.ty, Vector(_)) || matches!(r.ty, Vector(_))) {
                            return Ok(IRExpr {
                                kind: IRExprKind::Binary {
                                    left: Box::new(l.clone()),
                                    op,
                                    right: Box::new(r.clone()),
                                },
                                ty: l.ty,
                            });
                        }
                        let res_ty = self.tr.unify_numeric(&l.ty, &r.ty)?;
                        let l2 = ensure_ty(l, res_ty.clone());
                        let r2 = ensure_ty(r, res_ty.clone());
                        Ok(IRExpr {
                            kind: IRExprKind::Binary {
                                left: Box::new(l2),
                                op,
                                right: Box::new(r2),
                            },
                            ty: res_ty,
                        })
                    }
                    BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual | BinaryOp::Equality => {
                        let _ = self.tr.unify_numeric(&l.ty, &r.ty)?;
                        Ok(IRExpr {
                            kind: IRExprKind::Binary {
                                left: Box::new(l),
                                op,
                                right: Box::new(r),
                            },
                            ty: Type::Bool,
                        })
                    }
                    BinaryOp::Or | BinaryOp::And => {
                        if l.ty == Type::Bool && r.ty == Type::Bool {
                            Ok(IRExpr {
                                kind: IRExprKind::Binary {
                                    left: Box::new(l),
                                    op,
                                    right: Box::new(r),
                                },
                                ty: Type::Bool,
                            })
                        } else {
                            Err(TypeError::UnsupportedOperation {
                                op: format!("{:?}", op),
                                left: l.ty,
                                right: r.ty,
                            })
                        }
                    }
                    BinaryOp::Range => {
                        if l.ty == Type::Int && r.ty == Type::Int {
                            Ok(IRExpr {
                                kind: IRExprKind::Binary {
                                    left: Box::new(l),
                                    op,
                                    right: Box::new(r),
                                },
                                ty: Vector(Type::Int.into()),
                            })
                        } else {
                            Err(TypeError::UnsupportedOperation {
                                op: format!("{:?}", op),
                                left: l.ty,
                                right: r.ty,
                            })
                        }
                    }
                    _ => Err(TypeError::UnsupportedOperation {
                        op: format!("{:?}", op),
                        left: l.ty,
                        right: r.ty,
                    }),
                }
            }
            AstExpr::Call { callee, args } => match *callee {
                AstExpr::Identifier(name) => {
                    if let Some(descriptor) = self.tr.builtins.get(&name).cloned() {
                        let mut ir_args = Vec::with_capacity(args.len());
                        for arg in args {
                            ir_args.push(self.lower_expr(arg)?);
                        }
                        return self.lower_builtin_call(&descriptor, ir_args);
                    }

                    let (params, ret_ty) = self
                        .tr
                        .funcs
                        .get(&name)
                        .cloned()
                        .ok_or_else(|| TypeError::UnknownFunction(name.clone()))?;

                    if params.iter().any(|p| matches!(p.kind, ParamKind::VarArgs)) {
                        return Err(TypeError::UnsupportedOperation {
                            op: format!("calling variadic function {}", name),
                            left: Type::VarArgs,
                            right: Type::VarArgs,
                        });
                    }

                    if params.len() != args.len() {
                        return Err(TypeError::ArityMismatch {
                            func: name,
                            expected: params.len(),
                            found: args.len(),
                        });
                    }

                    let mut ir_args = Vec::with_capacity(args.len());
                    for (i, (arg_ast, param)) in
                        args.into_iter().zip(params.into_iter()).enumerate()
                    {
                        let a_ir = self.lower_expr(arg_ast)?;
                        let expected_ty = match param.kind {
                            ParamKind::Normal(ty) => ty,
                            ParamKind::VarArgs => unreachable!("variadic params already rejected"),
                        };
                        let a_ir2 = ensure_ty(a_ir, expected_ty.clone());
                        if a_ir2.ty != expected_ty {
                            return Err(TypeError::TypeMismatch {
                                expected: expected_ty,
                                found: a_ir2.ty,
                                context: format!("argument {} for call", i),
                            });
                        }
                        ir_args.push(a_ir2);
                    }

                    Ok(IRExpr {
                        kind: IRExprKind::Call {
                            callee: Box::new(IRExpr {
                                kind: IRExprKind::Identifier(name),
                                ty: ret_ty.clone(),
                            }),
                            args: ir_args,
                        },
                        ty: ret_ty,
                    })
                }
                other => {
                    let callee_ir = self.lower_expr(other)?;
                    Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        found: callee_ir.ty,
                        context: "callable expression (identifier function)".to_string(),
                    })
                }
            },
            AstExpr::Index { target, index } => {
                let target_ir = self.lower_expr(*target)?;
                let index_ir = self.lower_expr(*index)?;

                // Extract element type from Vector<T>
                match &target_ir.ty {
                    Type::Vector(elem_ty_box) => {
                        let elem_ty = (**elem_ty_box).clone();

                        // Validate index is Int
                        if index_ir.ty != Type::Int {
                            return Err(TypeError::InvalidIndexType {
                                index_type: index_ir.ty,
                                context: "vector indexing".to_string(),
                            });
                        }

                        Ok(IRExpr {
                            kind: IRExprKind::Index {
                                target: Box::new(target_ir),
                                index: Box::new(index_ir),
                            },
                            ty: elem_ty, // Result type is element type!
                        })
                    }
                    other => Err(TypeError::InvalidIndexTarget {
                        target_type: other.clone(),
                        context: "only vectors can be indexed".to_string(),
                    }),
                }
            }
        }
    }

    fn lower_builtin_call(
        &mut self,
        descriptor: &BuiltinDescriptor,
        args: Vec<IRExpr>,
    ) -> TyResult<IRExpr> {
        let kind = descriptor.kind;
        let return_ty = descriptor.return_type.clone();
        let name = match kind {
            BuiltinKind::C => "c",
            BuiltinKind::List => "list",
            BuiltinKind::Print => "print",
        };

        if args.is_empty() {
            return Err(TypeError::ArityMismatch {
                func: name.to_string(),
                expected: 1,
                found: 0,
            });
        }

        let has_varargs = args.iter().any(|a| matches!(a.ty, Type::VarArgs));
        if has_varargs && args.len() != 1 {
            return Err(TypeError::UnsupportedOperation {
                op: format!("{}: mixing ... with positional arguments", name),
                left: Type::VarArgs,
                right: Type::Any,
            });
        }

        match kind {
            BuiltinKind::C => {
                if has_varargs {
                    return Ok(IRExpr {
                        kind: IRExprKind::VectorLiteral(args),
                        ty: return_ty,
                    });
                }

                let mut numeric_target: Option<Type> = None;
                for arg in &args {
                    numeric_target = Some(match &numeric_target {
                        None => arg.ty.clone(),
                        Some(acc) => self.tr.unify_numeric(acc, &arg.ty)?,
                    });
                }

                let target = numeric_target.unwrap_or(Type::Int);
                let coerced_args: Vec<IRExpr> = args
                    .into_iter()
                    .map(|arg| ensure_ty(arg, target.clone()))
                    .collect();
                // #TODO: this makes program not able to initialize C vectors with non-numeric types. We'll extend it with Any types soon as soon as codegen supports.
                let vector_type = coerced_args.first().unwrap().ty.clone();

                Ok(IRExpr {
                    kind: IRExprKind::VectorLiteral(coerced_args),
                    ty: Vector(vector_type.into()),
                })
            }
            BuiltinKind::List => {
                if has_varargs {
                    return Ok(IRExpr {
                        kind: IRExprKind::BuiltinCall {
                            builtin: kind,
                            args,
                        },
                        ty: return_ty,
                    });
                }

                let mut element_ty: Option<Type> = None;
                for (idx, arg) in args.iter().enumerate() {
                    if let Some(expected) = &element_ty {
                        if expected != &arg.ty {
                            return Err(TypeError::TypeMismatch {
                                expected: expected.clone(),
                                found: arg.ty.clone(),
                                context: format!("argument {} for {}", idx, name),
                            });
                        }
                    } else {
                        element_ty = Some(arg.ty.clone());
                    }
                }

                Ok(IRExpr {
                    kind: IRExprKind::BuiltinCall {
                        builtin: kind,
                        args,
                    },
                    ty: return_ty,
                })
            }
            BuiltinKind::Print => {
                if args.len() != 1 {
                    return Err(TypeError::ArityMismatch {
                        func: name.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }

                if args[0].ty != Type::Int {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        found: args[0].ty.clone(),
                        context: format!("print argument"),
                    });
                }

                Ok(IRExpr {
                    kind: IRExprKind::BuiltinCall {
                        builtin: kind,
                        args,
                    },
                    ty: return_ty,
                })
            }
        }
    }
}

/// Helper: ensure an expression has a desired type, applying implicit numeric promotions virtually.
fn ensure_ty(mut e: IRExpr, want: Type) -> IRExpr {
    if e.ty == want {
        return e;
    }
    // Allow implicit numeric promotions by just changing the expression's result type.
    // Real codegen can insert casts if needed later.
    match (&e.ty, &want) {
        (Type::Int, Type::Float | Type::Double) => {
            e.ty = want;
            e
        }
        (Type::Float, Type::Double) => {
            e.ty = want;
            e
        }
        // No other implicit conversions for now
        _ => e,
    }
}

/// Convenience function for callers: lower a program without creating a resolver explicitly.
pub fn lower_program(program: Vec<AstStmt>) -> TyResult<Vec<IRStmt>> {
    let mut r = TypeResolver::new();
    IR::from_ast(program, &mut r)
}
