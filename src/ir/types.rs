use crate::ast::{BinaryOp, Param, Type};

/// A typed intermediate representation (IR) used by code generation.
/// All expressions and declarations in IR have concrete, non-optional types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinKind {
    C,
    List,
    Print
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRExprKind {
    Number(String),
    Identifier(String),
    XString(String),
    VectorLiteral(Vec<IRExpr>),
    Binary {
        left: Box<IRExpr>,
        op: BinaryOp,
        right: Box<IRExpr>,
    },
    Call {
        callee: Box<IRExpr>,
        args: Vec<IRExpr>,
    },
    BuiltinCall {
        builtin: BuiltinKind,
        args: Vec<IRExpr>,
    },
    Index {
        target: Box<IRExpr>,
        index: Box<IRExpr>,
    },
    /// Represents the packed `...` array/local in functions with varargs.
    VarArgs,
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
    VarAssign {
        name: String,
        ty: Type,
        value: IRExpr,
    },
    If {
        condition: IRExpr,
        then_branch: Vec<IRStmt>,
        else_branch: Option<Vec<IRStmt>>,
    },
    /// Return always carries an expression. Use an `IRExpr { kind: Unit, ty: Type::Void }` for `return;`.
    Return(IRExpr),
    FunctionDef {
        name: String,
        params: Vec<Param>,
        return_type: Type,
        body: Vec<IRStmt>,
    },
    For {
        iter_var: (String, Type),
        iter_expr: IRExpr,
        body: Vec<IRStmt>,
    },
    IndexAssign {
        target: IRExpr,
        index: IRExpr,
        value: IRExpr,
    },
    Block(Vec<IRStmt>),
}

/// Errors that can occur during type resolution / lowering.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UnknownVariable(String),
    UnknownFunction(String),
    ArityMismatch {
        func: String,
        expected: usize,
        found: usize,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        context: String,
    },
    UnsupportedOperation {
        op: String,
        left: Type,
        right: Type,
    },
    InvalidIndexTarget {
        target_type: Type,
        context: String,
    },
    InvalidIndexType {
        index_type: Type,
        context: String,
    },
    MissingReturnValue {
        function: String,
        expected: Type,
    },
}

/// Result alias for type resolution.
pub type TyResult<T> = Result<T, TypeError>;
