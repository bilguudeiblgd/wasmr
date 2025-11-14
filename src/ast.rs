#[derive(Debug, Clone, PartialEq)]
// #TODO: add more maths operations + functions.
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Less,
    LessEqual,
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
    Grouping(Box<Expr>),
}

// -------------------- Statements & Program --------------------

#[derive(Debug, Clone, PartialEq)]
pub enum ParamKind {
    Normal(Type),
    VarArgs,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub kind: ParamKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    VarAssign {
        name: String,
        x_type: Option<Type>,
        value: Expr,
    },
    Return(Option<Expr>),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Double,
    String,
    Vector(Box<Type>),
    List,
    Char,
    Void,
    Bool,
    Any,
    /// Internal type used to represent packed `...` values.
    VarArgs,
    // Represents a reference/value of a function.
    FunctionRef,
}
