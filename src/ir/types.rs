use crate::ast::{BinaryOp, UnaryOp};
use crate::types::{Param, Type};

/// A typed block of statements with an optional tail expression.
/// The tail expression, if present, represents the value of the block.
#[derive(Debug, Clone, PartialEq)]
pub struct IRBlock {
    /// Statements in the block
    pub stmts: Vec<IRStmt>,
    /// Optional tail expression (the value of the block)
    /// If present, this is the last expression and determines the block's type
    pub tail_expr: Option<Box<IRExpr>>,
    /// The type of the block (either the tail_expr's type or Void if no tail)
    pub ty: Type,
}

/// Top-level IR representation of a program
///
/// Contains the main function and all flattened functions.
/// Nested functions are extracted to top-level by the function flattening pass.
#[derive(Debug, Clone, PartialEq)]
pub struct IRProgram {
    /// Main entry point function
    pub main_function: IRStmt,

    /// All functions (including nested functions, flattened to top-level)
    /// Populated by the function flattening pass
    pub functions: Vec<IRStmt>,
}

impl IRProgram {
    /// Create a new IR program from statements
    pub fn new(statements: Vec<IRStmt>) -> Self {
        Self {
            main_function: IRStmt::FunctionDef {
                name: "main".to_string(),
                params: vec![],
                return_type: Type::Void,
                body: IRBlock {
                    stmts: statements,
                    tail_expr: None,
                    ty: Type::Void,
                },
                metadata: None,
            },
            functions: vec![],
        }
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
    Stop,
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
    Unary {
        op: UnaryOp,
        operand: Box<IRExpr>,
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
    /// If expression that evaluates to a value
    If {
        condition: Box<IRExpr>,
        then_branch: IRBlock,
        else_branch: Option<IRBlock>,
    },
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
        is_super_assign: bool,
    },
    If {
        condition: IRExpr,
        then_branch: IRBlock,
        else_branch: Option<IRBlock>,
        /// The type of the if expression (if both branches exist and have same type)
        /// Otherwise Type::Void
        result_ty: Type,
    },
    /// Return always carries an expression. Use an `IRExpr { kind: Unit, ty: Type::Void }` for `return;`.
    Return(IRExpr),
    FunctionDef {
        name: String,
        params: Vec<Param>,
        return_type: Type,
        body: IRBlock,
        /// Function metadata computed by IR passes
        /// Initially None, populated by variable collection pass
        metadata: Option<Box<FunctionMetadata>>,
    },
    For {
        iter_var: (String, Type),
        iter_expr: IRExpr,
        body: IRBlock,
    },
    While {
        condition: IRExpr,
        body: IRBlock,
    },
    IndexAssign {
        target: IRExpr,
        index: IRExpr,
        value: IRExpr,
    },
    Block(IRBlock),
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

    pub captured_vars: Vec<CapturedVarInfo>,  // Empty if no captures
    pub is_closure: bool,  // true if needs env struct
}

/// Information about a variable captured from parent scope (for closures)
#[derive(Debug, Clone, PartialEq)]
pub struct CapturedVarInfo {
    /// Name of the captured variable
    pub name: String,

    /// Type of the captured variable
    pub ty: Type,

    /// Field index in the environment struct
    /// Note: Field 0 is always the function code pointer (ref func)
    /// Field 1..N are the captured variables
    pub field_index: u32,

    /// Whether this variable can be mutated via super-assignment (<<-)
    /// If true, the struct field should be mutable or wrapped in a ref
    pub is_mutable: bool,
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
    
    pub need_reference: bool,

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
