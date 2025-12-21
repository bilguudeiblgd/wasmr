use crate::types::{Type, Param, ParamKind};

#[derive(Debug, Clone, PartialEq)]
// #TODO: add more maths operations + functions.
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Equality,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Range, // for ':' operator
    Or,    // '|'
    And,   // '&'
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(String),
    Identifier(String),
    XString(String),
    /// Represents the `...` placeholder inside function bodies.
    VarArgs,
    Logical(bool),
    // Anonymous function literal that evaluates to a reference to that function.
    // In practice this is typically used on the right-hand side of an assignment,
    // e.g., `f <- function(x: int): int { return(x) }`.
    FunctionDef {
        params: Vec<Param>,
        return_type: Option<Type>,
        body: Vec<Stmt>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    Grouping(Box<Expr>),
}

// -------------------- Statements & Program --------------------

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    VarAssign {
        name: String,
        x_type: Option<Type>,
        value: Expr,
        is_super_assign: bool, // true for <<-, false for <-
    },
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    For {
        iter_name: String,
        // either range exp or vector exp
        iter_vector: Expr,
        body: Vec<Stmt>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    IndexAssign {
        target: Expr,
        index: Expr,
        value: Expr,
    },
    Return(Option<Expr>),
    Block(Vec<Stmt>),
}
