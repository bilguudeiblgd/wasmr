use crate::types::{Type, Param, ParamKind};
use crate::types::Type::Vector;
use crate::ast::{Block, BinaryOp, Expr as AstExpr, Expr, Stmt as AstStmt, Stmt};
use super::types::{BuiltinKind, IRBlock, IRExpr, IRExprKind, IRProgram, IRStmt, TyResult, TypeError};
use super::type_resolver::{TypeResolver, FunctionCtx, BuiltinDescriptor};

/// Compare two types for compatibility, ignoring parameter names in function types
fn types_compatible(t1: &Type, t2: &Type) -> bool {
    // Type::Any is compatible with any type (acts as a wildcard)
    if matches!(t1, Type::Any) || matches!(t2, Type::Any) {
        return true;
    }

    match (t1, t2) {
        (Type::Function { params: p1, return_type: r1 }, Type::Function { params: p2, return_type: r2 }) => {
            // Check same number of parameters
            if p1.len() != p2.len() {
                return false;
            }
            // Check parameter types match (ignoring names)
            for (param1, param2) in p1.iter().zip(p2.iter()) {
                if !param_kinds_compatible(&param1.kind, &param2.kind) {
                    return false;
                }
            }
            // Check return types match
            types_compatible(r1, r2)
        }
        (Type::Vector(inner1), Type::Vector(inner2)) => types_compatible(inner1, inner2),
        (Type::Reference(inner1), Type::Reference(inner2)) => types_compatible(inner1, inner2),
        _ => t1 == t2, // For non-function types, use regular equality
    }
}

/// Compare parameter kinds for compatibility (recursive for nested function types)
fn param_kinds_compatible(k1: &ParamKind, k2: &ParamKind) -> bool {
    match (k1, k2) {
        (ParamKind::Normal(ty1), ParamKind::Normal(ty2)) => types_compatible(ty1, ty2),
        (ParamKind::VarArgs, ParamKind::VarArgs) => true,
        _ => false,
    }
}

/// Public IR facade that owns the lowering API. TypeResolver is injected as a dependency.
pub struct IR;

impl IR {
    pub fn from_ast(program: Vec<AstStmt>, resolver: &mut TypeResolver) -> TyResult<IRProgram> {
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
    fn lower_program(&mut self, program: Vec<AstStmt>) -> TyResult<IRProgram> {
        // Functions are now stored in scope_stack during normal statement processing
        // No need for separate function registration pass

        let mut out = Vec::with_capacity(program.len());
        for s in program {
            out.push(self.lower_stmt(s)?);
        }
        Ok(IRProgram::new(out))
    }

    fn lower_block(&mut self, stmts: Vec<AstStmt>) -> TyResult<Vec<IRStmt>> {
        let mut out = Vec::with_capacity(stmts.len());
        for s in stmts {
            out.push(self.lower_stmt(s)?);
        }
        Ok(out)
    }

    /// Lower a Block (statements + optional tail expression) to IRBlock
    fn lower_ast_block(&mut self, block: Block) -> TyResult<IRBlock> {
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
                is_super_assign
            } => {
                // Special-case: function literal on RHS defines a named function
                if let AstExpr::FunctionDef {
                    params,
                    return_type,
                    body,
                } = value
                {
                    let ret_ty = return_type.unwrap_or(Type::Void);

                    // Create function type
                    let func_ty = Type::Function {
                        params: params.clone(),
                        return_type: Box::new(ret_ty.clone()),
                    };

                    // Store function in current scope (before processing body)
                    // This enables: 1) recursive calls, 2) nested function access
                    if is_super_assign {
                        self.tr.super_assign(&name, func_ty)?;
                    } else {
                        self.tr.define_var(name.clone(), func_ty);
                    }

                    // Enter NEW function scope
                    self.tr.enter_scope();

                    // Add parameters to new function scope
                    for p in &params {
                        if let ParamKind::Normal(param_ty) = &p.kind {
                            self.tr.define_var(p.name.clone(), param_ty.clone());
                        }
                    }

                    // Set function context
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

                    // Lower body (can access parent function variables via scope stack)
                    let body_ir = self.lower_ast_block(body)?;

                    // Check that block's tail expression type matches function return type
                    // Only enforce this check if the function expects a non-Void return
                    // AND the block has a tail expression (not relying on explicit returns)
                    if ret_ty != Type::Void && body_ir.tail_expr.is_some() {
                        if !types_compatible(&body_ir.ty, &ret_ty) {
                            return Err(TypeError::TypeMismatch {
                                expected: ret_ty.clone(),
                                found: body_ir.ty.clone(),
                                context: format!("function '{}' body", name),
                            });
                        }
                    }

                    // Exit function scope
                    self.tr.exit_scope();
                    self.tr.current_function = saved_fn;

                    return Ok(IRStmt::FunctionDef {
                        name,
                        params,
                        return_type: ret_ty,
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
            },

            AstStmt::For {
                iter_name,
                iter_vector,
                body
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
                // Heuristic: integers have no dot; otherwise Float
                let ty = if s.contains('.') {
                    Type::Float
                } else {
                    Type::Int
                };
                Ok(IRExpr {
                    kind: IRExprKind::Number(s),
                    ty,
                })
            }
            AstExpr::Identifier(name) => {
                // Unified lookup - works for both variables and functions
                if let Some(t) = self.tr.lookup_var(&name) {
                    Ok(IRExpr {
                        kind: IRExprKind::Identifier(name),
                        ty: t,
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
            AstExpr::FunctionDef { params, return_type, .. } => {
                // Function definition as expression (shouldn't normally happen, but handle it)
                let ret_ty = return_type.unwrap_or(Type::Void);
                Ok(IRExpr {
                    kind: IRExprKind::Unit,
                    ty: Type::Function {
                        params,
                        return_type: Box::new(ret_ty),
                    },
                })
            }
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
                    // Direct function call: name(args)
                    if let Some(descriptor) = self.tr.builtins.get(&name).cloned() {
                        let mut ir_args = Vec::with_capacity(args.len());
                        for arg in args {
                            ir_args.push(self.lower_expr(arg)?);
                        }
                        return self.lower_builtin_call(&descriptor, ir_args);
                    }

                    // Look up function type from scope
                    let func_type = self.tr.lookup_var(&name)
                        .ok_or_else(|| TypeError::UnknownFunction(name.clone()))?;

                    // Extract params and return type from Function type
                    let (params, ret_ty) = match &func_type {
                        Type::Function { params, return_type } => (params.clone(), (**return_type).clone()),
                        other => {
                            return Err(TypeError::TypeMismatch {
                                expected: Type::Void,  // placeholder
                                found: other.clone(),
                                context: format!("{} is not a function", name),
                            });
                        }
                    };

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
                        if !types_compatible(&a_ir2.ty, &expected_ty) {
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
                                ty: func_type.clone(),  // Callee should have Function type
                            }),
                            args: ir_args,
                        },
                        ty: ret_ty,
                    })
                }
                AstExpr::Call { callee: inner_callee, args: inner_args } => {
                    // Recursive case: callee is itself a call expression
                    // e.g., outer(1.0)(2.0) where outer(1.0) returns a function
                    // Here: inner_callee = Identifier("outer"), inner_args = [1.0]
                    //       outer args = [2.0]
                    // We need to first evaluate Call{inner_callee(inner_args)}, then call result with outer args

                    let inner_call = AstExpr::Call {
                        callee: inner_callee,
                        args: inner_args,
                    };
                    let callee_ir = self.lower_expr(inner_call)?;

                    // The callee must evaluate to a function type
                    let (params, ret_ty) = match &callee_ir.ty {
                        Type::Function { params, return_type } => (params.clone(), (**return_type).clone()),
                        other => {
                            return Err(TypeError::TypeMismatch {
                                expected: Type::Void, // placeholder
                                found: other.clone(),
                                context: "callee must be a function type".to_string(),
                            });
                        }
                    };

                    if params.iter().any(|p| matches!(p.kind, ParamKind::VarArgs)) {
                        return Err(TypeError::UnsupportedOperation {
                            op: "calling function expression with varargs".to_string(),
                            left: Type::VarArgs,
                            right: Type::VarArgs,
                        });
                    }

                    if params.len() != args.len() {
                        return Err(TypeError::ArityMismatch {
                            func: "<expression>".to_string(),
                            expected: params.len(),
                            found: args.len(),
                        });
                    }

                    let mut ir_args = Vec::with_capacity(args.len());
                    for (i, (arg_ast, param)) in args.into_iter().zip(params.into_iter()).enumerate() {
                        let a_ir = self.lower_expr(arg_ast)?;
                        let expected_ty = match param.kind {
                            ParamKind::Normal(ty) => ty,
                            ParamKind::VarArgs => unreachable!("variadic params already rejected"),
                        };
                        let a_ir2 = ensure_ty(a_ir, expected_ty.clone());
                        if !types_compatible(&a_ir2.ty, &expected_ty) {
                            return Err(TypeError::TypeMismatch {
                                expected: expected_ty,
                                found: a_ir2.ty,
                                context: format!("argument {} for call expression", i),
                            });
                        }
                        ir_args.push(a_ir2);
                    }

                    Ok(IRExpr {
                        kind: IRExprKind::Call {
                            callee: Box::new(callee_ir),
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
            },
            AstExpr::If { condition, then_branch, else_branch } => {
                // Lower condition
                let cond_ir = self.lower_expr(*condition)?;

                // Check condition is Bool
                if cond_ir.ty != Type::Bool {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Bool,
                        found: cond_ir.ty,
                        context: "if expression condition".to_string(),
                    });
                }

                // Lower then branch
                let then_ir = self.lower_ast_block(then_branch)?;

                // Lower else branch if present
                let (else_ir, result_ty) = if let Some(else_block) = else_branch {
                    let else_ir = self.lower_ast_block(else_block)?;

                    // Both branches exist - determine common type
                    if types_compatible(&then_ir.ty, &else_ir.ty) {
                        (Some(else_ir.clone()), then_ir.ty.clone())
                    } else if types_compatible(&else_ir.ty, &then_ir.ty) {
                        (Some(else_ir.clone()), else_ir.ty.clone())
                    } else {
                        return Err(TypeError::TypeMismatch {
                            expected: then_ir.ty.clone(),
                            found: else_ir.ty.clone(),
                            context: "if expression branches must have compatible types".to_string(),
                        });
                    }
                } else {
                    // No else branch - defaults to Void
                    (None, Type::Void)
                };

                Ok(IRExpr {
                    kind: IRExprKind::If {
                        condition: Box::new(cond_ir),
                        then_branch: then_ir,
                        else_branch: else_ir,
                    },
                    ty: result_ty,
                })
            },
            Expr::Logical(_) => todo!()
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
            BuiltinKind::Length => "length",
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

                // Accept Int, Float, or Double for printing
                if !matches!(args[0].ty, Type::Int | Type::Float | Type::Double) {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        found: args[0].ty.clone(),
                        context: format!("print argument (expected int, float, or double)"),
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
            BuiltinKind::Length => {
                if args.len() != 1 {
                    return Err(TypeError::ArityMismatch {
                        func: name.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }

                // Accept only vector types
                if !matches!(args[0].ty, Type::Vector(_)) {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Vector(Box::new(Type::Any)),
                        found: args[0].ty.clone(),
                        context: format!("length() argument (expected vector)"),
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
pub fn lower_program(program: Vec<AstStmt>) -> TyResult<IRProgram> {
    let mut r = TypeResolver::new();
    IR::from_ast(program, &mut r)
}
