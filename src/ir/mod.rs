mod types;
mod type_resolver;
mod lowering;

// Pass infrastructure
pub mod passes;

// Re-export public types
pub use types::{
    BuiltinKind, IRExpr, IRExprKind, IRStmt, TypeError, TyResult,
    // New types for pass-based architecture
    IRProgram, FunctionMetadata, LocalVarInfo, VarOrigin, VarArgsInfo,
};
pub use type_resolver::TypeResolver;
pub use lowering::{lower_program, IR};

// Re-export pass infrastructure
pub use passes::{Pass, PassError, IRPassManager};
