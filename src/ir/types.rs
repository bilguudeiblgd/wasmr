use crate::ast::{BinaryOp, Param, Type};

/// Top-level IR representation of a program
///
/// Contains IR statements and provides access to function metadata
/// computed by IR passes.
#[derive(Debug, Clone, PartialEq)]
pub struct IRProgram {
    /// Top-level IR statements
    pub statements: Vec<IRStmt>,
}

impl IRProgram {
    /// Create a new IR program from statements
    pub fn new(statements: Vec<IRStmt>) -> Self {
        Self { statements }
    }
}

/// A typed intermediate representation (IR) used by code generation.
/// All expressions and declarations in IR have concrete, non-optional types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinKind {
    C,
    List,
    Print,
    Length,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRExprKind {
    Number(String),
    Identifier(String),
    XString(String),
    BoolLiteral(bool),
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
        /// Function metadata computed by IR passes
        /// Initially None, populated by variable collection pass
        metadata: Option<Box<FunctionMetadata>>,
    },
    For {
        iter_var: (String, Type),
        iter_expr: IRExpr,
        body: Vec<IRStmt>,
    },
    While {
        condition: IRExpr,
        body: Vec<IRStmt>,
    },
    IndexAssign {
        target: IRExpr,
        index: IRExpr,
        value: IRExpr,
    },
    Block(Vec<IRStmt>),
}

/// Complete metadata about a function, computed during IR passes
///
/// This includes all local variables (parameters + user variables + compiler temps)
/// with their WASM local indices assigned.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionMetadata {
    /// All local variables (parameters come first, then user vars, then temps)
    pub local_vars: Vec<LocalVarInfo>,

    /// Varargs parameter information, if present
    pub varargs_param: Option<VarArgsInfo>,

    /// Total number of locals (params + user vars + temps)
    pub local_count: u32,
}

/// Information about a single local variable
#[derive(Debug, Clone, PartialEq)]
pub struct LocalVarInfo {
    /// Variable name
    pub name: String,

    /// Variable type
    pub ty: Type,

    /// WASM local index (0-based, parameters come first)
    pub index: u32,

    /// How this variable was introduced
    pub origin: VarOrigin,
}

/// Describes how a variable was introduced into the function
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarOrigin {
    /// Function parameter
    Parameter,

    /// User-declared variable (VarAssign statement)
    UserDeclared,

    /// Compiler-generated for loop iterator variable
    LoopIterator,

    /// Compiler-generated for loop index counter (system_iter)
    LoopIndex,

    /// Compiler-generated for loop vector storage (__for_vec_X)
    LoopVector,

    /// Compiler-generated for loop length (__for_len)
    LoopLength,

    /// Compiler-generated temporary for print operations
    PrintTemp,
}

/// Information about varargs parameter
#[derive(Debug, Clone, PartialEq)]
pub struct VarArgsInfo {
    /// Name of the varargs parameter
    pub param_name: String,

    /// Local index where varargs array is stored
    pub local_index: u32,
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
