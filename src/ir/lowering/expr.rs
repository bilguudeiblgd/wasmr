use super::args::{extract_arg_expr, match_arguments_to_params};
use super::mangling::mangle_function_name;
use super::stmt::ensure_ty;
use super::types::types_compatible;
use super::LowerCtx;
use crate::ast::{Argument, BinaryOp, Expr as AstExpr};
use crate::ir::types::{IRExpr, IRExprKind, TyResult, TypeError};
use crate::ir::TypeResolver;
use crate::types::Type::Vector;
use crate::types::{ParamKind, Type};

impl<'a> LowerCtx<'a> {
    pub(super) fn lower_expr(&mut self, expr: AstExpr) -> TyResult<IRExpr> {
        match expr {
            AstExpr::Number(s) => {
                // Heuristic: integers have no dot; otherwise Double (matches R's default)
                let ty = if s.contains('L') {
                    Type::Int
                } else {
                    Type::Double
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
            AstExpr::FunctionDef {
                params,
                return_type,
                ..
            } => {
                // For function expressions, default to Type::Any if no return type specified
                // (Note: body is not lowered here - only when assigned to a variable)
                // Extract base Param for type signature (without defaults)
                let type_params: Vec<crate::types::Param> = params.iter().map(|pd| pd.param.clone()).collect();
                let ret_ty = return_type.unwrap_or(Type::Any);
                Ok(IRExpr {
                    kind: IRExprKind::Unit,
                    ty: Type::Function {
                        params: type_params,
                        return_type: Box::new(ret_ty),
                    },
                })
            }
            AstExpr::Grouping(inner) => self.lower_expr(*inner),
            AstExpr::Unary { op, operand } => {
                use crate::ast::UnaryOp;
                let operand_ir = self.lower_expr(*operand)?;
                match op {
                    UnaryOp::LogicalNot => {
                        // Logical not requires boolean operand
                        if operand_ir.ty != Type::Logical {
                            return Err(TypeError::TypeMismatch {
                                expected: Type::Logical,
                                found: operand_ir.ty,
                                context: "logical not operator".to_string(),
                            });
                        }
                        Ok(IRExpr {
                            kind: IRExprKind::Unary {
                                op,
                                operand: Box::new(operand_ir),
                            },
                            ty: Type::Logical,
                        })
                    }
                    UnaryOp::Minus => {
                        // Unary minus works on numeric types
                        match &operand_ir.ty {
                            Type::Int | Type::Double => {
                                Ok(IRExpr {
                                    kind: IRExprKind::Unary {
                                        op,
                                        operand: Box::new(operand_ir.clone()),
                                    },
                                    ty: operand_ir.ty.clone(),
                                })
                            }
                            _ => Err(TypeError::TypeMismatch {
                                expected: Type::Int, // Or Double
                                found: operand_ir.ty,
                                context: "unary minus operator".to_string(),
                            }),
                        }
                    }
                    UnaryOp::Plus => {
                        // Unary plus works on numeric types (essentially a no-op)
                        match &operand_ir.ty {
                            Type::Int | Type::Double => {
                                Ok(IRExpr {
                                    kind: IRExprKind::Unary {
                                        op,
                                        operand: Box::new(operand_ir.clone()),
                                    },
                                    ty: operand_ir.ty.clone(),
                                })
                            }
                            _ => Err(TypeError::TypeMismatch {
                                expected: Type::Int, // Or Double
                                found: operand_ir.ty,
                                context: "unary plus operator".to_string(),
                            }),
                        }
                    }
                }
            }
            AstExpr::Binary { left, op, right } => {
                // Special case: desugar seq operator (from:to) into function call
                // This allows dynamic ranges to work by calling the runtime function
                if matches!(op, BinaryOp::Seq) {
                    // Desugar: from:to  →  system_seq___int___int___int(from, to, 1)
                    let seq_call = AstExpr::Call {
                        callee: Box::new(AstExpr::Identifier("system_seq___int___int___int".to_string())),
                        args: vec![
                            Argument::Positional(*left),
                            Argument::Positional(*right),
                            Argument::Positional(AstExpr::Number("1".to_string())), // by = 1
                        ],
                    };
                    return self.lower_expr(seq_call);
                }

                let l = self.lower_expr(*left)?;
                let r = self.lower_expr(*right)?;
                match op {
                    BinaryOp::Mod => {
                        // Modulo only works on integers (no F64 mod in WASM)
                        // Force both operands to Int
                        let l2 = ensure_ty(l, Type::Int);
                        let r2 = ensure_ty(r, Type::Int);
                        Ok(IRExpr {
                            kind: IRExprKind::Binary {
                                left: Box::new(l2),
                                op,
                                right: Box::new(r2),
                            },
                            ty: Type::Int,
                        })
                    }
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
                    BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual
                    | BinaryOp::Equality
                    | BinaryOp::NotEqual => {
                        // Check if we're comparing vectors (vector-vector, vector-scalar, or scalar-vector)
                        match (&l.ty, &r.ty) {
                            (Type::Vector(l_elem), Type::Vector(r_elem)) => {
                                // Vector comparison - call runtime function
                                let op_name = match op {
                                    BinaryOp::Less => "less",
                                    BinaryOp::LessEqual => "less_equal",
                                    BinaryOp::Greater => "greater",
                                    BinaryOp::GreaterEqual => "greater_equal",
                                    BinaryOp::Equality => "equal",
                                    BinaryOp::NotEqual => "not_equal",
                                    _ => unreachable!(),
                                };

                                let elem_type_name = match (l_elem.as_ref(), r_elem.as_ref()) {
                                    (Type::Int, Type::Int) | (Type::Logical, Type::Logical) | (Type::Int, Type::Logical) | (Type::Logical, Type::Int) => "int",
                                    (Type::Double, Type::Double) => "double",
                                    // For mixed types with double, promote to double
                                    (Type::Int, Type::Double) | (Type::Double, Type::Int)
                                    | (Type::Logical, Type::Double) | (Type::Double, Type::Logical) => "double",
                                    _ => {
                                        return Err(TypeError::UnsupportedOperation {
                                            op: format!("vector comparison {:?}", op),
                                            left: l.ty.clone(),
                                            right: r.ty.clone(),
                                        });
                                    }
                                };

                                // Build the function name: system_vector_{op}___vec_{type}__vec_{type}
                                let func_name = format!("system_vector_{}___vec_{}__vec_{}", op_name, elem_type_name, elem_type_name);

                                // If types don't match, we need to cast
                                let l2 = if elem_type_name == "double" && (l_elem.as_ref() == &Type::Int || l_elem.as_ref() == &Type::Logical) {
                                    ensure_ty(l, Type::Vector(Box::new(Type::Double)))
                                } else if elem_type_name == "int" && l_elem.as_ref() == &Type::Logical {
                                    ensure_ty(l, Type::Vector(Box::new(Type::Int)))
                                } else {
                                    l
                                };

                                let r2 = if elem_type_name == "double" && (r_elem.as_ref() == &Type::Int || r_elem.as_ref() == &Type::Logical) {
                                    ensure_ty(r, Type::Vector(Box::new(Type::Double)))
                                } else if elem_type_name == "int" && r_elem.as_ref() == &Type::Logical {
                                    ensure_ty(r, Type::Vector(Box::new(Type::Int)))
                                } else {
                                    r
                                };

                                // Generate a function call
                                Ok(IRExpr {
                                    kind: IRExprKind::Call {
                                        callee: Box::new(IRExpr {
                                            kind: IRExprKind::Identifier(func_name.clone()),
                                            ty: Type::Function {
                                                params: vec![
                                                    crate::types::Param {
                                                        name: "a".to_string(),
                                                        kind: ParamKind::Normal(Type::Vector(Box::new(if elem_type_name == "int" { Type::Int } else { Type::Double }))),
                                                    },
                                                    crate::types::Param {
                                                        name: "b".to_string(),
                                                        kind: ParamKind::Normal(Type::Vector(Box::new(if elem_type_name == "int" { Type::Int } else { Type::Double }))),
                                                    },
                                                ],
                                                return_type: Box::new(Type::Vector(Box::new(Type::Logical))),
                                            },
                                        }),
                                        args: vec![l2, r2],
                                    },
                                    ty: Type::Vector(Box::new(Type::Logical)),
                                })
                            }
                            // Vector-scalar comparison: vector < scalar
                            (Type::Vector(l_elem), r_ty) if TypeResolver::is_numeric(r_ty) => {
                                let op_name = match op {
                                    BinaryOp::Less => "less",
                                    BinaryOp::LessEqual => "less_equal",
                                    BinaryOp::Greater => "greater",
                                    BinaryOp::GreaterEqual => "greater_equal",
                                    BinaryOp::Equality => "equal",
                                    BinaryOp::NotEqual => "not_equal",
                                    _ => unreachable!(),
                                };

                                // Determine the unified element type
                                let elem_type_name = match (l_elem.as_ref(), r_ty) {
                                    (Type::Int, Type::Int) | (Type::Logical, Type::Logical) | (Type::Int, Type::Logical) | (Type::Logical, Type::Int) => "int",
                                    (Type::Double, Type::Double) => "double",
                                    (Type::Int, Type::Double) | (Type::Double, Type::Int)
                                    | (Type::Logical, Type::Double) | (Type::Double, Type::Logical) => "double",
                                    _ => {
                                        return Err(TypeError::UnsupportedOperation {
                                            op: format!("vector-scalar comparison {:?}", op),
                                            left: l.ty.clone(),
                                            right: r.ty.clone(),
                                        });
                                    }
                                };

                                // Build function name for vector-vector comparison
                                let func_name = format!("system_vector_{}___vec_{}__vec_{}", op_name, elem_type_name, elem_type_name);

                                // Cast left vector if needed
                                let l2 = if elem_type_name == "double" && (l_elem.as_ref() == &Type::Int || l_elem.as_ref() == &Type::Logical) {
                                    ensure_ty(l, Type::Vector(Box::new(Type::Double)))
                                } else if elem_type_name == "int" && l_elem.as_ref() == &Type::Logical {
                                    ensure_ty(l, Type::Vector(Box::new(Type::Int)))
                                } else {
                                    l
                                };

                                // Convert scalar to vector using c()
                                let r_vec_ty = Type::Vector(Box::new(if elem_type_name == "int" { Type::Int } else { Type::Double }));
                                let r_scalar = if elem_type_name == "double" && (r_ty == &Type::Int || r_ty == &Type::Logical) {
                                    ensure_ty(r, Type::Double)
                                } else if elem_type_name == "int" && r_ty == &Type::Logical {
                                    ensure_ty(r, Type::Int)
                                } else {
                                    r
                                };

                                // Wrap scalar in c() to make it a vector
                                let r2 = IRExpr {
                                    kind: IRExprKind::VectorLiteral(vec![r_scalar]),
                                    ty: r_vec_ty.clone(),
                                };

                                Ok(IRExpr {
                                    kind: IRExprKind::Call {
                                        callee: Box::new(IRExpr {
                                            kind: IRExprKind::Identifier(func_name.clone()),
                                            ty: Type::Function {
                                                params: vec![
                                                    crate::types::Param {
                                                        name: "a".to_string(),
                                                        kind: ParamKind::Normal(r_vec_ty.clone()),
                                                    },
                                                    crate::types::Param {
                                                        name: "b".to_string(),
                                                        kind: ParamKind::Normal(r_vec_ty.clone()),
                                                    },
                                                ],
                                                return_type: Box::new(Type::Vector(Box::new(Type::Logical))),
                                            },
                                        }),
                                        args: vec![l2, r2],
                                    },
                                    ty: Type::Vector(Box::new(Type::Logical)),
                                })
                            }
                            // Scalar-vector comparison: scalar < vector
                            (l_ty, Type::Vector(r_elem)) if TypeResolver::is_numeric(l_ty) => {
                                let op_name = match op {
                                    BinaryOp::Less => "less",
                                    BinaryOp::LessEqual => "less_equal",
                                    BinaryOp::Greater => "greater",
                                    BinaryOp::GreaterEqual => "greater_equal",
                                    BinaryOp::Equality => "equal",
                                    BinaryOp::NotEqual => "not_equal",
                                    _ => unreachable!(),
                                };

                                // Determine the unified element type
                                let elem_type_name = match (l_ty, r_elem.as_ref()) {
                                    (Type::Int, Type::Int) | (Type::Logical, Type::Logical) | (Type::Int, Type::Logical) | (Type::Logical, Type::Int) => "int",
                                    (Type::Double, Type::Double) => "double",
                                    (Type::Int, Type::Double) | (Type::Double, Type::Int)
                                    | (Type::Logical, Type::Double) | (Type::Double, Type::Logical) => "double",
                                    _ => {
                                        return Err(TypeError::UnsupportedOperation {
                                            op: format!("scalar-vector comparison {:?}", op),
                                            left: l.ty.clone(),
                                            right: r.ty.clone(),
                                        });
                                    }
                                };

                                // Build function name for vector-vector comparison
                                let func_name = format!("system_vector_{}___vec_{}__vec_{}", op_name, elem_type_name, elem_type_name);

                                // Convert scalar to vector using c()
                                let l_vec_ty = Type::Vector(Box::new(if elem_type_name == "int" { Type::Int } else { Type::Double }));
                                let l_scalar = if elem_type_name == "double" && (l_ty == &Type::Int || l_ty == &Type::Logical) {
                                    ensure_ty(l, Type::Double)
                                } else if elem_type_name == "int" && l_ty == &Type::Logical {
                                    ensure_ty(l, Type::Int)
                                } else {
                                    l
                                };

                                // Wrap scalar in c() to make it a vector
                                let l2 = IRExpr {
                                    kind: IRExprKind::VectorLiteral(vec![l_scalar]),
                                    ty: l_vec_ty.clone(),
                                };

                                // Cast right vector if needed
                                let r2 = if elem_type_name == "double" && (r_elem.as_ref() == &Type::Int || r_elem.as_ref() == &Type::Logical) {
                                    ensure_ty(r, Type::Vector(Box::new(Type::Double)))
                                } else if elem_type_name == "int" && r_elem.as_ref() == &Type::Logical {
                                    ensure_ty(r, Type::Vector(Box::new(Type::Int)))
                                } else {
                                    r
                                };

                                Ok(IRExpr {
                                    kind: IRExprKind::Call {
                                        callee: Box::new(IRExpr {
                                            kind: IRExprKind::Identifier(func_name.clone()),
                                            ty: Type::Function {
                                                params: vec![
                                                    crate::types::Param {
                                                        name: "a".to_string(),
                                                        kind: ParamKind::Normal(l_vec_ty.clone()),
                                                    },
                                                    crate::types::Param {
                                                        name: "b".to_string(),
                                                        kind: ParamKind::Normal(l_vec_ty.clone()),
                                                    },
                                                ],
                                                return_type: Box::new(Type::Vector(Box::new(Type::Logical))),
                                            },
                                        }),
                                        args: vec![l2, r2],
                                    },
                                    ty: Type::Vector(Box::new(Type::Logical)),
                                })
                            }
                            _ => {
                                // Scalar comparison (existing logic)
                                let res_ty = self.tr.unify_numeric(&l.ty, &r.ty)?;
                                let l2 = ensure_ty(l, res_ty.clone());
                                let r2 = ensure_ty(r, res_ty);
                                Ok(IRExpr {
                                    kind: IRExprKind::Binary {
                                        left: Box::new(l2),
                                        op,
                                        right: Box::new(r2),
                                    },
                                    ty: Type::Logical,
                                })
                            }
                        }
                    }
                    BinaryOp::Or | BinaryOp::And => {
                        if l.ty == Type::Logical && r.ty == Type::Logical {
                            Ok(IRExpr {
                                kind: IRExprKind::Binary {
                                    left: Box::new(l),
                                    op,
                                    right: Box::new(r),
                                },
                                ty: Type::Logical,
                            })
                        } else {
                            Err(TypeError::UnsupportedOperation {
                                op: format!("{:?}", op),
                                left: l.ty,
                                right: r.ty,
                            })
                        }
                    }
                    BinaryOp::Seq => {
                        // This case is handled above by desugaring into a function call
                        // We should never reach here
                        unreachable!("BinaryOp::Seq should be desugared before lowering")
                    }
                }
            }
            AstExpr::Call { callee, args } => match *callee {
                AstExpr::Identifier(name) => {
                    // Direct function call: name(args)
                    if let Some(descriptor) = self.tr.builtins.get(&name).cloned() {
                        // Check if this built-in has parameter definitions for named argument support
                        let param_defs = self.tr.function_param_defs.get(&name);

                        // Match arguments to parameters if definitions exist
                        let matched_exprs = if let Some(param_defs) = param_defs {
                            match_arguments_to_params(args.clone(), param_defs, &name)?
                        } else {
                            // No param_defs - use simple positional extraction
                            args.into_iter().map(extract_arg_expr).collect()
                        };

                        // Lower all matched arguments to IR
                        let mut ir_args = Vec::with_capacity(matched_exprs.len());
                        for expr in matched_exprs {
                            ir_args.push(self.lower_expr(expr)?);
                        }

                        return self.lower_builtin_call(&descriptor, ir_args, &name);
                    }

                    // Look up parameter definitions for named argument matching
                    let param_defs = self.tr.function_param_defs.get(&name);

                    // Match arguments to parameters (handles named args, defaults, reordering)
                    let matched_exprs = if let Some(param_defs) = param_defs {
                        match_arguments_to_params(args.clone(), param_defs, &name)?
                    } else {
                        // No param_defs found - fall back to simple extraction
                        // (This can happen for functions without definitions, like imported functions)
                        args.into_iter().map(extract_arg_expr).collect()
                    };

                    // Lower all matched arguments to get their types for overload resolution
                    let mut lowered_args = Vec::with_capacity(matched_exprs.len());
                    for expr in matched_exprs {
                        lowered_args.push(self.lower_expr(expr)?);
                    }

                    // Try function overloading: generate mangled name based on argument types
                    let mangled_name = mangle_function_name(&name, &lowered_args);

                    // Look up function type from scope (try mangled name first, then original)
                    let (resolved_name, func_type) = if let Some(ty) = self.tr.lookup_var(&mangled_name) {
                        (mangled_name, ty)
                    } else if let Some(ty) = self.tr.lookup_var(&name) {
                        (name.clone(), ty)
                    } else {
                        return Err(TypeError::UnknownFunction(name.clone()));
                    };

                    // Extract params and return type from Function type
                    let (params, ret_ty) = match &func_type {
                        Type::Function {
                            params,
                            return_type,
                        } => (params.clone(), (**return_type).clone()),
                        other => {
                            return Err(TypeError::TypeMismatch {
                                expected: Type::Void, // placeholder
                                found: other.clone(),
                                context: format!("{} is not a function", name),
                            });
                        }
                    };

                    if params.iter().any(|p| matches!(p.kind, ParamKind::VarArgs)) {
                        return Err(TypeError::UnsupportedOperation {
                            op: format!("calling variadic function {}", resolved_name),
                            left: Type::VarArgs,
                            right: Type::VarArgs,
                        });
                    }

                    // Arity check is now handled by match_arguments_to_params (with default support)

                    // Type-check and coerce arguments (they're already lowered)
                    let mut ir_args = Vec::with_capacity(lowered_args.len());
                    for (i, (arg_ir, param)) in
                        lowered_args.into_iter().zip(params.into_iter()).enumerate()
                    {
                        let expected_ty = match param.kind {
                            ParamKind::Normal(ty) => ty,
                            ParamKind::VarArgs => unreachable!("variadic params already rejected"),
                        };
                        let a_ir2 = ensure_ty(arg_ir, expected_ty.clone());
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
                                kind: IRExprKind::Identifier(resolved_name),
                                ty: func_type.clone(), // Callee should have Function type
                            }),
                            args: ir_args,
                        },
                        ty: ret_ty,
                    })
                }
                AstExpr::Call {
                    callee: inner_callee,
                    args: inner_args,
                } => {
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
                        Type::Function {
                            params,
                            return_type,
                        } => (params.clone(), (**return_type).clone()),
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
                    for (i, (arg_ast, param)) in
                        args.into_iter().zip(params.into_iter()).enumerate()
                    {
                        let a_ir = self.lower_expr(extract_arg_expr(arg_ast))?;
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

                        // Allow Double indices (R behavior) - cast to Int automatically
                        let index_ir = if index_ir.ty == Type::Double || index_ir.ty == Type::Int {
                            ensure_ty(index_ir, Type::Int)
                        } else {
                            return Err(TypeError::InvalidIndexType {
                                index_type: index_ir.ty,
                                context: "vector indexing requires numeric type".to_string(),
                            });
                        };

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
            AstExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Lower condition
                let cond_ir = self.lower_expr(*condition)?;

                // Check condition is Logical
                if cond_ir.ty != Type::Logical {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Logical,
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
                            context: "if expression branches must have compatible types"
                                .to_string(),
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
            }
            AstExpr::Logical(val) => Ok(IRExpr {
                kind: IRExprKind::Number(if val { "1" } else { "0" }.to_string()),
                ty: Type::Logical,
            }),
        }
    }
}
