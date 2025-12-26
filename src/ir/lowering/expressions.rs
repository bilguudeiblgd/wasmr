use super::super::types::{IRExpr, IRExprKind, TyResult, TypeError};
use super::builtins::lower_builtin_call;
use super::utils::{ensure_ty, extract_arg_expr, mangle_function_name, match_arguments_to_params, types_compatible};
use crate::ast::{Argument, BinaryOp, Expr as AstExpr};
use crate::types::Type;
use crate::types::Type::Vector;

impl<'a> super::context::LowerCtx<'a> {
    /// Lower an AST expression to a typed IR expression
    pub(super) fn lower_expr(&mut self, expr: AstExpr) -> TyResult<IRExpr> {
        match expr {
            AstExpr::Number(s) => {
                // Heuristic: integers have no dot; otherwise Double (matches R's default)
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
            AstExpr::Unary { op, operand } => self.lower_unary(op, *operand),
            AstExpr::Binary { left, op, right } => self.lower_binary(*left, op, *right),
            AstExpr::Call { callee, args } => self.lower_call(*callee, args),
            AstExpr::Index { target, index } => self.lower_index(*target, *index),
            AstExpr::Logical(_b) => {
                // TODO: Boolean literals are not yet supported in IR
                // For now, treat them as Unit
                Ok(IRExpr {
                    kind: IRExprKind::Unit,
                    ty: Type::Logical,
                })
            }
            AstExpr::If {
                condition,
                then_branch,
                else_branch,
            } => self.lower_if_expr(*condition, then_branch, else_branch),
        }
    }

    fn lower_unary(&mut self, op: crate::ast::UnaryOp, operand: AstExpr) -> TyResult<IRExpr> {
        use crate::ast::UnaryOp;
        let operand_ir = self.lower_expr(operand)?;
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
        }
    }

    fn lower_binary(&mut self, left: AstExpr, op: BinaryOp, right: AstExpr) -> TyResult<IRExpr> {
        // Special case: desugar seq operator (from:to) into function call
        // This allows dynamic ranges to work by calling the runtime function
        if matches!(op, BinaryOp::Seq) {
            // Desugar: from:to  →  system_seq___int___int___int(from, to, 1)
            let seq_call = AstExpr::Call {
                callee: Box::new(AstExpr::Identifier("system_seq___int___int___int".to_string())),
                args: vec![
                    Argument::Positional(left),
                    Argument::Positional(right),
                    Argument::Positional(AstExpr::Number("1".to_string())), // by = 1
                ],
            };
            return self.lower_expr(seq_call);
        }

        let l = self.lower_expr(left)?;
        let r = self.lower_expr(right)?;
        
        match op {
            BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                self.lower_arithmetic(l, op, r)
            }
            BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::Equality
            | BinaryOp::NotEqual => {
                self.lower_comparison(l, op, r)
            }
            BinaryOp::Or | BinaryOp::And => {
                self.lower_logical(l, op, r)
            }
            BinaryOp::Seq => {
                // This case is handled above by desugaring into a function call
                // We should never reach here
                unreachable!("BinaryOp::Seq should be desugared before lowering")
            }
        }
    }

    fn lower_arithmetic(&mut self, l: IRExpr, op: BinaryOp, r: IRExpr) -> TyResult<IRExpr> {
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

    fn lower_comparison(&mut self, l: IRExpr, op: BinaryOp, r: IRExpr) -> TyResult<IRExpr> {
        let _ = self.tr.unify_numeric(&l.ty, &r.ty)?;
        Ok(IRExpr {
            kind: IRExprKind::Binary {
                left: Box::new(l),
                op,
                right: Box::new(r),
            },
            ty: Type::Logical,
        })
    }

    fn lower_logical(&mut self, l: IRExpr, op: BinaryOp, r: IRExpr) -> TyResult<IRExpr> {
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

    fn lower_call(&mut self, callee: AstExpr, args: Vec<Argument>) -> TyResult<IRExpr> {
        match callee {
            AstExpr::Identifier(name) => self.lower_direct_call(name, args),
            other_callee => self.lower_indirect_call(other_callee, args),
        }
    }

    fn lower_direct_call(&mut self, name: String, args: Vec<Argument>) -> TyResult<IRExpr> {
        // Direct function call: name(args)
        if let Some(descriptor) = self.tr.builtins.get(&name).cloned() {
            return self.lower_builtin_direct_call(name, descriptor, args);
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
        let ir_args: Vec<IRExpr> = matched_exprs
            .into_iter()
            .map(|e| self.lower_expr(e))
            .collect::<TyResult<Vec<_>>>()?;

        // Build mangled name for overload resolution
        let mangled = mangle_function_name(&name, &ir_args);

        // Look up the function in the environment
        let func_ty = self.tr.lookup_var(&name)
            .or_else(|| self.tr.lookup_var(&mangled))
            .ok_or_else(|| TypeError::UnknownVariable(name.clone()))?;

        // Extract expected parameter types and return type
        match &func_ty {
            Type::Function { params, return_type } => {
                self.validate_call_args(&ir_args, params, &name)?;
                Ok(IRExpr {
                    kind: IRExprKind::Call {
                        callee: Box::new(IRExpr {
                            kind: IRExprKind::Identifier(name.clone()),
                            ty: func_ty.clone(),
                        }),
                        args: ir_args,
                    },
                    ty: (**return_type).clone(),
                })
            }
            _ => Err(TypeError::TypeMismatch {
                expected: Type::Function {
                    params: vec![],
                    return_type: Box::new(Type::Any),
                },
                found: func_ty.clone(),
                context: format!("calling {}", name),
            }),
        }
    }

    fn lower_builtin_direct_call(
        &mut self,
        name: String,
        descriptor: super::super::type_resolver::BuiltinDescriptor,
        args: Vec<Argument>,
    ) -> TyResult<IRExpr> {
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

        lower_builtin_call(self.tr, &descriptor, ir_args, &name)
    }

    fn lower_indirect_call(&mut self, callee: AstExpr, args: Vec<Argument>) -> TyResult<IRExpr> {
        // Indirect call: expr(args) where expr is not an identifier
        // e.g., (if (x) f else g)(10)
        let callee_ir = self.lower_expr(callee)?;

        // Extract expressions from arguments (no named argument support for indirect calls)
        let arg_exprs: Vec<AstExpr> = args.into_iter().map(extract_arg_expr).collect();

        // Lower argument expressions
        let ir_args: Vec<IRExpr> = arg_exprs
            .into_iter()
            .map(|e| self.lower_expr(e))
            .collect::<TyResult<Vec<_>>>()?;

        // Validate callee type
        let return_ty = match &callee_ir.ty {
            Type::Function { params, return_type } => {
                self.validate_call_args(&ir_args, params, "<anonymous function>")?;
                (**return_type).clone()
            }
            other => {
                return Err(TypeError::TypeMismatch {
                    expected: Type::Function {
                        params: vec![],
                        return_type: Box::new(Type::Any),
                    },
                    found: other.clone(),
                    context: "indirect call".to_string(),
                });
            }
        };

        Ok(IRExpr {
            kind: IRExprKind::Call {
                callee: Box::new(callee_ir),
                args: ir_args,
            },
            ty: return_ty,
        })
    }

    fn validate_call_args(&self, ir_args: &[IRExpr], params: &[crate::types::Param], func_name: &str) -> TyResult<()> {
        // Check arity (allowing for variadic functions)
        let has_varargs = params.iter().any(|p| matches!(p.kind, crate::types::ParamKind::VarArgs));
        
        if !has_varargs && ir_args.len() != params.len() {
            return Err(TypeError::ArityMismatch {
                func: func_name.to_string(),
                expected: params.len(),
                found: ir_args.len(),
            });
        }

        // Type-check arguments
        for (i, arg) in ir_args.iter().enumerate() {
            if i >= params.len() {
                if !has_varargs {
                    return Err(TypeError::ArityMismatch {
                        func: func_name.to_string(),
                        expected: params.len(),
                        found: ir_args.len(),
                    });
                }
                break;
            }

            let param = &params[i];
            match &param.kind {
                crate::types::ParamKind::Normal(expected_ty) => {
                    if !types_compatible(&arg.ty, expected_ty) {
                        return Err(TypeError::TypeMismatch {
                            expected: expected_ty.clone(),
                            found: arg.ty.clone(),
                            context: format!("argument {} for {}", i + 1, func_name),
                        });
                    }
                }
                crate::types::ParamKind::VarArgs => {
                    // VarArgs accepts any remaining arguments
                    break;
                }
            }
        }

        Ok(())
    }

    fn lower_index(&mut self, target: AstExpr, index: AstExpr) -> TyResult<IRExpr> {
        let target_ir = self.lower_expr(target)?;
        let index_ir = self.lower_expr(index)?;

        // Verify target is a vector
        let elem_ty = match &target_ir.ty {
            Type::Vector(elem_ty) => {
                // Index must be Int
                if index_ir.ty != Type::Int {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        found: index_ir.ty,
                        context: "vector index".to_string(),
                    });
                }
                (**elem_ty).clone()
            }
            other => {
                return Err(TypeError::InvalidIndexTarget {
                    target_type: other.clone(),
                    context: "vector indexing".to_string(),
                });
            }
        };

        Ok(IRExpr {
            kind: IRExprKind::Index {
                target: Box::new(target_ir),
                index: Box::new(index_ir),
            },
            ty: elem_ty,
        })
    }

    fn lower_if_expr(
        &mut self,
        condition: AstExpr,
        then_branch: crate::ast::Block,
        else_branch: Option<crate::ast::Block>,
    ) -> TyResult<IRExpr> {
        let cond_ir = self.lower_expr(condition)?;

        // Condition must be logical
        if cond_ir.ty != Type::Logical {
            return Err(TypeError::TypeMismatch {
                expected: Type::Logical,
                found: cond_ir.ty,
                context: "if expression condition".to_string(),
            });
        }

        let then_ir = self.lower_ast_block(then_branch)?;
        let else_ir = else_branch.map(|b| self.lower_ast_block(b)).transpose()?;

        // Determine result type based on branches
        let result_ty = if let Some(else_block) = &else_ir {
            if types_compatible(&then_ir.ty, &else_block.ty) {
                then_ir.ty.clone()
            } else {
                Type::Void
            }
        } else {
            Type::Void
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
}
