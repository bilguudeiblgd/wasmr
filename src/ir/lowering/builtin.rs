use super::stmt::ensure_ty;
use super::LowerCtx;
use crate::ir::type_resolver::BuiltinDescriptor;
use crate::ir::types::{BuiltinKind, IRExpr, IRExprKind, TyResult, TypeError};
use crate::types::Type;
use crate::types::Type::Vector;

impl<'a> LowerCtx<'a> {
    pub(super) fn lower_builtin_call(
        &mut self,
        descriptor: &BuiltinDescriptor,
        args: Vec<IRExpr>,
        name: &str,
    ) -> TyResult<IRExpr> {
        let kind = descriptor.kind;
        let return_ty = descriptor.return_type.clone();

        // c() and list() can be called with 0 args to create empty collections
        // Other builtins require at least 1 argument
        if args.is_empty() && !matches!(kind, BuiltinKind::C | BuiltinKind::List) {
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

                // Handle empty c() - create empty int vector
                if args.is_empty() {
                    return Ok(IRExpr {
                        kind: IRExprKind::VectorLiteral(vec![]),
                        ty: Vector(Box::new(Type::Int)),
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
                    .map(|arg| ensure_ty(arg, target.clone(), true))
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

                // Accept Int, Double, or Logical for printing
                if !matches!(
                    args[0].ty,
                    Type::Int | Type::Double | Type::Logical
                ) {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        found: args[0].ty.clone(),
                        context: format!(
                            "print argument (expected int, double, or logical)"
                        ),
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
            BuiltinKind::Stop => {
                if args.len() != 1 {
                    return Err(TypeError::ArityMismatch {
                        func: name.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }

                // Accept string messages for now
                // In the future, we might support other types
                // For now, we'll just check that there's an argument
                // The actual error message handling will be done in codegen

                Ok(IRExpr {
                    kind: IRExprKind::BuiltinCall {
                        builtin: kind,
                        args,
                    },
                    ty: return_ty,
                })
            }
            BuiltinKind::Vector => {
                // vec(length: int, mode: string) creates a vector of given length and type
                if args.len() != 2 {
                    return Err(TypeError::ArityMismatch {
                        func: name.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }

                // Accept both Int and Double for length parameter (R allows numeric)
                // Cast to Int if needed
                let length_arg = if args[0].ty == Type::Double || args[0].ty == Type::Int {
                    ensure_ty(args[0].clone(), Type::Int, true)
                } else {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        found: args[0].ty.clone(),
                        context: format!("vec() length parameter must be numeric"),
                    });
                };

                // Check that mode argument is a string literal
                if !matches!(args[1].ty, Type::String) {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::String,
                        found: args[1].ty.clone(),
                        context: format!("vec() mode parameter"),
                    });
                }

                // Extract mode string literal to determine vector type
                let mode_str = match &args[1].kind {
                    IRExprKind::XString(s) => s.as_str(),
                    _ => {
                        return Err(TypeError::ArgumentError {
                            func: name.to_string(),
                            message: "vec() mode parameter must be a string literal".to_string(),
                        });
                    }
                };

                // Map mode string to Type
                let element_type = match mode_str {
                    "logical" => Type::Logical,
                    "int" | "integer" => Type::Int,
                    "double" | "numeric" => Type::Double,
                    _ => {
                        return Err(TypeError::ArgumentError {
                            func: name.to_string(),
                            message: format!(
                                "vec() mode must be 'logical', 'int', or 'double', got '{}'",
                                mode_str
                            ),
                        });
                    }
                };

                let vector_type = Type::Vector(Box::new(element_type));

                Ok(IRExpr {
                    kind: IRExprKind::BuiltinCall {
                        builtin: kind,
                        args: vec![length_arg, args[1].clone()],
                    },
                    ty: vector_type,
                })
            }
            BuiltinKind::AsInt => {
                // as.integer(x) casts x to int (works for scalars and vectors)
                if args.len() != 1 {
                    return Err(TypeError::ArityMismatch {
                        func: name.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }

                let arg = &args[0];
                let result_type = match &arg.ty {
                    Type::Int => Type::Int,  // Already int
                    Type::Double | Type::Logical => Type::Int,  // Scalar cast
                    Type::Vector(elem_ty) => {
                        // Vector cast
                        match elem_ty.as_ref() {
                            Type::Int | Type::Double | Type::Logical => {
                                Type::Vector(Box::new(Type::Int))
                            }
                            _ => {
                                return Err(TypeError::TypeMismatch {
                                    expected: Type::Vector(Box::new(Type::Int)),
                                    found: arg.ty.clone(),
                                    context: format!("as.integer() requires numeric scalar or vector"),
                                });
                            }
                        }
                    }
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            expected: Type::Int,
                            found: arg.ty.clone(),
                            context: format!("as.integer() requires numeric scalar or vector"),
                        });
                    }
                };

                Ok(IRExpr {
                    kind: IRExprKind::BuiltinCall {
                        builtin: kind,
                        args,
                    },
                    ty: result_type,
                })
            }
            BuiltinKind::AsDouble => {
                // as.double(x) casts x to double (works for scalars and vectors)
                if args.len() != 1 {
                    return Err(TypeError::ArityMismatch {
                        func: name.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }

                let arg = &args[0];
                let result_type = match &arg.ty {
                    Type::Double => Type::Double,  // Already double
                    Type::Int | Type::Logical => Type::Double,  // Scalar cast
                    Type::Vector(elem_ty) => {
                        // Vector cast
                        match elem_ty.as_ref() {
                            Type::Int | Type::Double | Type::Logical => {
                                Type::Vector(Box::new(Type::Double))
                            }
                            _ => {
                                return Err(TypeError::TypeMismatch {
                                    expected: Type::Vector(Box::new(Type::Double)),
                                    found: arg.ty.clone(),
                                    context: format!("as.double() requires numeric scalar or vector"),
                                });
                            }
                        }
                    }
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            expected: Type::Double,
                            found: arg.ty.clone(),
                            context: format!("as.double() requires numeric scalar or vector"),
                        });
                    }
                };

                Ok(IRExpr {
                    kind: IRExprKind::BuiltinCall {
                        builtin: kind,
                        args,
                    },
                    ty: result_type,
                })
            }
            BuiltinKind::AsLogical => {
                // as.logical(x) casts x to logical (works for scalars and vectors)
                if args.len() != 1 {
                    return Err(TypeError::ArityMismatch {
                        func: name.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }

                let arg = &args[0];
                let result_type = match &arg.ty {
                    Type::Logical => Type::Logical,  // Already logical
                    Type::Int | Type::Double => Type::Logical,  // Scalar cast
                    Type::Vector(elem_ty) => {
                        // Vector cast
                        match elem_ty.as_ref() {
                            Type::Int | Type::Double | Type::Logical => {
                                Type::Vector(Box::new(Type::Logical))
                            }
                            _ => {
                                return Err(TypeError::TypeMismatch {
                                    expected: Type::Vector(Box::new(Type::Logical)),
                                    found: arg.ty.clone(),
                                    context: format!("as.logical() requires numeric scalar or vector"),
                                });
                            }
                        }
                    }
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            expected: Type::Logical,
                            found: arg.ty.clone(),
                            context: format!("as.logical() requires numeric scalar or vector"),
                        });
                    }
                };

                Ok(IRExpr {
                    kind: IRExprKind::BuiltinCall {
                        builtin: kind,
                        args,
                    },
                    ty: result_type,
                })
            }
        }
    }
}
