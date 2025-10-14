use std::collections::HashMap;

use crate::ast::{self, BinaryOp, Expr as AstExpr, Param, Stmt as AstStmt, Type};

/// A typed intermediate representation (IR) used by code generation.
/// All expressions and declarations in IR have concrete, non-optional types.
#[derive(Debug, Clone, PartialEq)]
pub enum IRExprKind {
    Number(String),
    Identifier(String),
    XString(String),
    Binary { left: Box<IRExpr>, op: BinaryOp, right: Box<IRExpr> },
    Call { callee: Box<IRExpr>, args: Vec<IRExpr> },
    /// Represents no value (used for void returns). Always has Type::Void.
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRExpr {
    pub kind: IRExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRStmt {
    ExprStmt(IRExpr),
    VarAssign { name: String, ty: Type, value: IRExpr },
    /// Return always carries an expression. Use an `IRExpr { kind: Unit, ty: Type::Void }` for `return;`.
    Return(IRExpr),
    FunctionDef {
        name: String,
        params: Vec<Param>,
        return_type: Type,
        body: Vec<IRStmt>,
    },
    Block(Vec<IRStmt>),
}

/// Errors that can occur during type resolution / lowering.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UnknownVariable(String),
    UnknownFunction(String),
    ArityMismatch { func: String, expected: usize, found: usize },
    TypeMismatch { expected: Type, found: Type, context: String },
    UnsupportedOperation { op: String, left: Type, right: Type },
    MissingReturnValue { function: String, expected: Type },
}

/// Result alias for type resolution.
pub type TyResult<T> = Result<T, TypeError>;

#[derive(Default)]
pub struct TypeResolver {
    /// Variable environment
    vars: HashMap<String, Type>,
    /// Function environment: name -> (param_types, return_type)
    funcs: HashMap<String, (Vec<Type>, Type)>,
    /// For better error messages in returns
    current_function: Option<(String, Type)>,
}

impl TypeResolver {
    pub fn new() -> Self { Self::default() }

    fn is_numeric(t: &Type) -> bool {
        matches!(t, Type::Int | Type::Float | Type::Double)
    }

    pub(crate) fn promote_numeric(&self, a: &Type, target: &Type) -> Type {
        // Only promote if both numeric and target is at least as wide
        if Self::is_numeric(a) && Self::is_numeric(target) { target.clone() } else { a.clone() }
    }

    pub(crate) fn unify_numeric(&self, l: &Type, r: &Type) -> TyResult<Type> {
        use Type::*;
        if !Self::is_numeric(l) || !Self::is_numeric(r) {
            return Err(TypeError::UnsupportedOperation { op: "numeric op".to_string(), left: l.clone(), right: r.clone() });
        }
        let rank = |t: &Type| match t { Int => 0, Float => 1, Double => 2, _ => -1 };
        Ok(match (rank(l), rank(r)) {
            (2, _) | (_, 2) => Double,
            (1, _) | (_, 1) => Float,
            _ => Int,
        })
    }
}

/// Public IR facade that owns the lowering API. TypeResolver is injected as a dependency.
pub struct IR;

impl IR {
    pub fn from_ast(program: Vec<AstStmt>, resolver: &mut TypeResolver) -> TyResult<Vec<IRStmt>> {
        let mut lower = LowerCtx { tr: resolver };
        lower.lower_program(program)
    }
}

/// Internal lowering context that uses a borrowed TypeResolver for environments and type ops.
struct LowerCtx<'a> { tr: &'a mut TypeResolver }

impl<'a> LowerCtx<'a> {
    /// Lower a whole program (list of AST statements) to typed IR statements.
    fn lower_program(&mut self, program: Vec<AstStmt>) -> TyResult<Vec<IRStmt>> {
        // collect function signatures first so calls can be validated
        for s in &program {
            if let AstStmt::FunctionDef { name, params, return_type, .. } = s {
                let param_tys: Vec<Type> = params.iter().map(|p| p.ty.clone()).collect();
                let ret_ty = return_type.clone().unwrap_or(Type::Void);
                self.tr.funcs.insert(name.clone(), (param_tys, ret_ty));
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
        for s in stmts { out.push(self.lower_stmt(s)?); }
        Ok(out)
    }

    fn lower_stmt(&mut self, stmt: AstStmt) -> TyResult<IRStmt> {
        match stmt {
            AstStmt::ExprStmt(e) => {
                let e = self.lower_expr(e)?;
                Ok(IRStmt::ExprStmt(e))
            }
            AstStmt::VarAssign { name, x_type, value } => {
                let val = self.lower_expr(value)?;
                let inferred = val.ty.clone();
                let final_ty = match x_type {
                    Some(t) => {
                        // allow simple numeric promotions into declared type
                        let promoted = self.tr.promote_numeric(&inferred, &t);
                        if promoted == t { t } else { return Err(TypeError::TypeMismatch { expected: t, found: inferred, context: format!("let {} = <expr>", name) }); }
                    }
                    None => inferred,
                };
                self.tr.vars.insert(name.clone(), final_ty.clone());
                Ok(IRStmt::VarAssign { name, ty: final_ty.clone(), value: ensure_ty(val, final_ty) })
            }
            AstStmt::Return(opt) => {
                let (func_name, func_ret) = self.tr.current_function.clone().unwrap_or_else(|| ("<top-level>".to_string(), Type::Void));
                let ir_expr = if let Some(e) = opt {
                    let e = self.lower_expr(e)?;
                    if func_ret == Type::Void && e.ty != Type::Void {
                        return Err(TypeError::TypeMismatch { expected: Type::Void, found: e.ty, context: format!("return in {}", func_name) });
                    }
                    let e2 = ensure_ty(e, func_ret.clone());
                    e2
                } else {
                    if func_ret != Type::Void {
                        return Err(TypeError::MissingReturnValue { function: func_name, expected: func_ret });
                    }
                    IRExpr { kind: IRExprKind::Unit, ty: Type::Void }
                };
                Ok(IRStmt::Return(ir_expr))
            }
            AstStmt::FunctionDef { name, params, return_type, body } => {
                let ret_ty = return_type.unwrap_or(Type::Void);
                // Set up new scope for function variables (shadowing supported by saving old map)
                let saved_vars = self.tr.vars.clone();
                for p in &params { self.tr.vars.insert(p.name.clone(), p.ty.clone()); }
                let saved_fn = self.tr.current_function.clone();
                self.tr.current_function = Some((name.clone(), ret_ty.clone()));

                let body_ir = self.lower_block(body)?;

                // Restore scope
                self.tr.vars = saved_vars;
                self.tr.current_function = saved_fn;

                Ok(IRStmt::FunctionDef { name, params, return_type: ret_ty, body: body_ir })
            }
            AstStmt::Block(stmts) => {
                let body = self.lower_block(stmts)?;
                Ok(IRStmt::Block(body))
            }
        }
    }

    fn lower_expr(&mut self, expr: AstExpr) -> TyResult<IRExpr> {
        match expr {
            AstExpr::Number(s) => {
                // Heuristic: integers have no dot; otherwise Double
                let ty = if s.contains('.') { Type::Double } else { Type::Int };
                Ok(IRExpr { kind: IRExprKind::Number(s), ty })
            }
            AstExpr::Identifier(name) => {
                if let Some(t) = self.tr.vars.get(&name).cloned() {
                    Ok(IRExpr { kind: IRExprKind::Identifier(name), ty: t })
                } else if let Some((_, ret)) = self.tr.funcs.get(&name).cloned() {
                    Ok(IRExpr { kind: IRExprKind::Identifier(name), ty: ret })
                } else {
                    Err(TypeError::UnknownVariable(name))
                }
            }
            AstExpr::XString(s) => Ok(IRExpr { kind: IRExprKind::XString(s), ty: Type::String }),
            AstExpr::Grouping(inner) => self.lower_expr(*inner),
            AstExpr::Binary { left, op, right } => {
                let l = self.lower_expr(*left)?;
                let r = self.lower_expr(*right)?;
                match op {
                    BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => {
                        let res_ty = self.tr.unify_numeric(&l.ty, &r.ty)?;
                        let l2 = ensure_ty(l, res_ty.clone());
                        let r2 = ensure_ty(r, res_ty.clone());
                        Ok(IRExpr { kind: IRExprKind::Binary { left: Box::new(l2), op, right: Box::new(r2) }, ty: res_ty })
                    }
                    BinaryOp::Less | BinaryOp::LessEqual => {
                        let _ = self.tr.unify_numeric(&l.ty, &r.ty)?;
                        Ok(IRExpr { kind: IRExprKind::Binary { left: Box::new(l), op, right: Box::new(r) }, ty: Type::Bool })
                    }
                    BinaryOp::Range => {
                        let _ = self.tr.unify_numeric(&l.ty, &r.ty)?;
                        Ok(IRExpr { kind: IRExprKind::Binary { left: Box::new(l), op, right: Box::new(r) }, ty: Type::Vector })
                    }
                }
            }
            AstExpr::Call { callee, args } => {
                match *callee {
                    AstExpr::Identifier(name) => {
                        let (param_tys, ret_ty) = self
                            .tr
                            .funcs
                            .get(&name)
                            .cloned()
                            .ok_or_else(|| TypeError::UnknownFunction(name.clone()))?;
                        if param_tys.len() != args.len() {
                            return Err(TypeError::ArityMismatch { func: name, expected: param_tys.len(), found: args.len() });
                        }
                        let mut ir_args = Vec::with_capacity(args.len());
                        for (i, (a, expected)) in args.into_iter().zip(param_tys.into_iter()).enumerate() {
                            let a_ir = self.lower_expr(a)?;
                            let a_ir2 = ensure_ty(a_ir, expected.clone());
                            if a_ir2.ty != expected {
                                return Err(TypeError::TypeMismatch { expected, found: a_ir2.ty, context: format!("argument {} for call", i) });
                            }
                            ir_args.push(a_ir2);
                        }
                        Ok(IRExpr { kind: IRExprKind::Call { callee: Box::new(IRExpr { kind: IRExprKind::Identifier(name), ty: ret_ty.clone() }), args: ir_args }, ty: ret_ty })
                    }
                    other => {
                        let callee_ir = self.lower_expr(other)?;
                        Err(TypeError::TypeMismatch { expected: Type::Int, found: callee_ir.ty, context: "callable expression (identifier function)".to_string() })
                    }
                }
            }
        }
    }
}

/// Helper: ensure an expression has a desired type, applying implicit numeric promotions virtually.
fn ensure_ty(mut e: IRExpr, want: Type) -> IRExpr {
    if e.ty == want { return e; }
    // Allow implicit numeric promotions by just changing the expression's result type.
    // Real codegen can insert casts if needed later.
    match (&e.ty, &want) {
        (Type::Int, Type::Float | Type::Double) => { e.ty = want; e }
        (Type::Float, Type::Double) => { e.ty = want; e }
        // No other implicit conversions for now
        _ => e,
    }
}

/// Convenience function for callers: lower a program without creating a resolver explicitly.
pub fn lower_program(program: Vec<AstStmt>) -> TyResult<Vec<IRStmt>> {
    let mut r = TypeResolver::new();
    IR::from_ast(program, &mut r)
}
