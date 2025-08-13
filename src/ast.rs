#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Less,
    LessEqual,
    Range, // for ':' operator
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(String),
    Identifier(String),
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
pub struct Param {
    pub name: String,
    pub ty: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    VarAssign { name: String, value: Expr },
    Return(Option<Expr>),
    FunctionDef {
        name: String,
        params: Vec<Param>,
        return_type: Option<String>,
        body: Vec<Stmt>,
    },
    Block(Vec<Stmt>),
}
