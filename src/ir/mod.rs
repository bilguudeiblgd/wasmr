mod types;
mod type_resolver;
mod lowering;

// Re-export public types
pub use types::{BuiltinKind, IRExpr, IRExprKind, IRStmt, TypeError, TyResult};
pub use type_resolver::TypeResolver;
pub use lowering::{lower_program, IR};
