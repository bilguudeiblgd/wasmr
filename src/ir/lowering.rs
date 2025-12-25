use super::type_resolver::{BuiltinDescriptor, FunctionCtx, TypeResolver};
use super::types::{
    BuiltinKind, IRBlock, IRExpr, IRExprKind, IRProgram, IRStmt, TyResult, TypeError,
};
use crate::ast::{Argument, BinaryOp, Block, Expr as AstExpr, Expr, Stmt as AstStmt};
use crate::types::Type::Vector;
use crate::types::{Param, ParamKind, Type};

/// Extract expression from an Argument (handles both Positional and Named)
/// TODO: Remove this once all call sites use match_arguments_to_params
fn extract_arg_expr(arg: Argument) -> AstExpr {
    match arg {
        Argument::Positional(expr) => expr,
        Argument::Named { value, .. } => value,
    }
}

/// Match function call arguments to function parameters, handling:
/// - Positional arguments
/// - Named arguments
/// - Default parameter values
/// - Argument reordering
///
/// Returns expressions in parameter order, with defaults filled in.
fn match_arguments_to_params(
    args: Vec<Argument>,
    param_defs: &[crate::ast::ParamDef],
    func_name: &str,
) -> Result<Vec<AstExpr>, TypeError> {
    // Separate positional and named arguments
    let mut positional_args = Vec::new();
    let mut named_args = Vec::new();
    let mut seen_named = false;

    for arg in args {
        match arg {
            Argument::Positional(expr) => {
                if seen_named {
                    return Err(TypeError::ArgumentError {
                        func: func_name.to_string(),
                        message: "positional arguments must come before named arguments".to_string(),
                    });
                }
                positional_args.push(expr);
            }
            Argument::Named { name, value } => {
                seen_named = true;
                named_args.push((name, value));
            }
        }
    }

    // Result: expressions in parameter order
    let mut result_exprs: Vec<Option<AstExpr>> = vec![None; param_defs.len()];

    // Step 1: Match positional arguments to first N parameters
    if positional_args.len() > param_defs.len() {
        return Err(TypeError::ArityMismatch {
            func: func_name.to_string(),
            expected: param_defs.len(),
            found: positional_args.len(),
        });
    }

    for (i, expr) in positional_args.into_iter().enumerate() {
        result_exprs[i] = Some(expr);
    }

    // Step 2: Match named arguments to parameters by name
    for (arg_name, arg_value) in named_args {
        // Find parameter with this name
        let param_idx = param_defs.iter().position(|pd| pd.param.name == arg_name);

        match param_idx {
            None => {
                return Err(TypeError::ArgumentError {
                    func: func_name.to_string(),
                    message: format!("unknown parameter name: '{}'", arg_name),
                });
            }
            Some(idx) => {
                // Check if already provided (via positional)
                if result_exprs[idx].is_some() {
                    return Err(TypeError::ArgumentError {
                        func: func_name.to_string(),
                        message: format!(
                            "parameter '{}' provided both positionally and by name",
                            arg_name
                        ),
                    });
                }
                result_exprs[idx] = Some(arg_value);
            }
        }
    }

    // Step 3: Fill in defaults for missing parameters
    for (i, param_def) in param_defs.iter().enumerate() {
        if result_exprs[i].is_none() {
            // Check if parameter has a default value
            if let Some(default_expr) = &param_def.default_value {
                result_exprs[i] = Some((**default_expr).clone());
            } else {
                // Required parameter not provided
                return Err(TypeError::ArgumentError {
                    func: func_name.to_string(),
                    message: format!("missing required parameter: '{}'", param_def.param.name),
                });
            }
        }
    }

    // Step 4: Extract all expressions (they should all be Some now)
    Ok(result_exprs.into_iter().map(|opt| opt.unwrap()).collect())
}

/// Compare two types for compatibility, ignoring parameter names in function types
fn types_compatible(t1: &Type, t2: &Type) -> bool {
    // Type::Any is compatible with any type (acts as a wildcard)
    if matches!(t1, Type::Any) || matches!(t2, Type::Any) {
        return true;
    }

    match (t1, t2) {
        (
            Type::Function {
                params: p1,
                return_type: r1,
            },
            Type::Function {
                params: p2,
                return_type: r2,
            },
        ) => {
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

    /// Generate a mangled function name for overload resolution
    /// Follows the scheme: system_name___arg1__arg2__arg3
    /// Examples:
    ///   max(int, int) -> system_max___int__int
    ///   sum(vec<int>) -> system_sum___vec_int
    fn mangle_function_name(&self, base_name: &str, args: &[IRExpr]) -> String {
        if args.is_empty() {
            return base_name.to_string();
        }

        let mut mangled = format!("system_{}", base_name);
        for (i, arg) in args.iter().enumerate() {
            if i == 0 {
                mangled.push_str("___");  // THREE underscores before first arg
            } else {
                mangled.push_str("__");   // TWO underscores between args
            }
            mangled.push_str(&Self::mangle_type(&arg.ty));
        }
        mangled
    }

    /// Convert a type to its mangled string representation
    /// Examples: Int -> "int", Vector(Int) -> "vec_int", Double -> "double"
    fn mangle_type(ty: &Type) -> String {
        match ty {
            Type::Int => "int".to_string(),
            Type::Double => "double".to_string(),
            Type::Logical => "logical".to_string(),
            Type::Char => "char".to_string(),
            Type::String => "string".to_string(),
            Type::Vector(inner) => format!("vec_{}", Self::mangle_type(inner)),
            Type::Function { .. } => "func".to_string(),
            _ => "any".to_string(),
        }
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

    fn lower_expr(&mut self, expr: AstExpr) -> TyResult<IRExpr> {
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
                }
            }
            AstExpr::Binary { left, op, right } => {
                let l = self.lower_expr(*left)?;
                let r = self.lower_expr(*right)?;
                match op {
                    BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
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
                    // _ => Err(TypeError::UnsupportedOperation {
                    //     op: format!("{:?}", op),
                    //     left: l.ty,
                    //     right: r.ty,
                    // }),
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
                    let mangled_name = self.mangle_function_name(&name, &lowered_args);

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
            Expr::Logical(val) => Ok(IRExpr {
                kind: IRExprKind::Number(if val { "1" } else { "0" }.to_string()),
                ty: Type::Logical,
            }),
        }
    }

    fn lower_builtin_call(
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
                // vec(length: int) creates a vector of given length
                if args.len() != 1 {
                    return Err(TypeError::ArityMismatch {
                        func: name.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }

                // Check that length argument is an integer
                if !matches!(args[0].ty, Type::Int) {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        found: args[0].ty.clone(),
                        context: format!("vec() length parameter"),
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
        (Type::Int, Type::Double) => {
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
