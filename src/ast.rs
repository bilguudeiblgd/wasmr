use crate::types::{Type, Param, ParamKind};

/// A block of statements with an optional tail expression.
/// The tail expression, if present, represents the value of the block.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// Statements in the block
    pub stmts: Vec<Stmt>,
    /// Optional tail expression (the value of the block)
    /// If present, this is the last expression without a semicolon
    pub tail_expr: Option<Box<Expr>>,
}

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
        body: Block,
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
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
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
        then_branch: Block,
        else_branch: Option<Block>,
    },
    For {
        iter_name: String,
        // either range exp or vector exp
        iter_vector: Expr,
        body: Block,
    },
    While {
        condition: Expr,
        body: Block,
    },
    IndexAssign {
        target: Expr,
        index: Expr,
        value: Expr,
    },
    Return(Option<Expr>),
    Block(Block),
}
