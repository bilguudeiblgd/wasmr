/// Argument matching and extraction for function calls
mod args;
/// Builtin function call lowering
mod builtin;
/// Expression lowering (lower_expr)
mod expr;
/// Function name mangling for overload resolution
mod mangling;
/// Statement lowering (lower_stmt, lower_ast_block, lower_block)
mod stmt;
/// Type compatibility checking
mod types;

use super::type_resolver::TypeResolver;
use super::types::{IRProgram, TyResult};
use crate::ast::Stmt as AstStmt;

/// Public IR facade that owns the lowering API. TypeResolver is injected as a dependency.
pub struct IR;

impl IR {
    pub fn from_ast(program: Vec<AstStmt>, resolver: &mut TypeResolver) -> TyResult<IRProgram> {
        let mut lower = LowerCtx { tr: resolver };
        lower.lower_program(program)
    }
}

/// Internal lowering context that uses a borrowed TypeResolver for environments and type ops.
pub(crate) struct LowerCtx<'a> {
    pub(crate) tr: &'a mut TypeResolver,
}

impl<'a> LowerCtx<'a> {
    /// Lower a whole program (list of AST statements) to typed IR statements.
    fn lower_program(&mut self, program: Vec<AstStmt>) -> TyResult<IRProgram> {
        // Functions are now stored in scope_stack during normal statement processing
        // No need for separate function registration pass

        let mut out = Vec::with_capacity(program.len());
        for s in program {
            out.push(self.lower_stmt(s)?);
        }
        Ok(IRProgram::new(out))
    }
}

/// Convenience function for callers: lower a program without creating a resolver explicitly.
pub fn lower_program(program: Vec<AstStmt>) -> TyResult<IRProgram> {
    let mut r = TypeResolver::new();
    IR::from_ast(program, &mut r)
}
