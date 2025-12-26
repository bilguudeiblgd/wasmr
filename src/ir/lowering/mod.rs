//! AST to IR lowering
//!
//! This module handles the conversion from untyped AST to typed IR,
//! performing type checking and inference along the way.
//!
//! ## Module Structure
//!
//! - `context`: Contains the `LowerCtx` struct that holds the lowering state
//! - `utils`: Helper functions for argument matching, type compatibility, etc.
//! - `statements`: Statement lowering logic
//! - `expressions`: Expression lowering logic
//! - `builtins`: Builtin function lowering logic

mod builtins;
mod context;
mod expressions;
mod statements;
mod utils;

use super::type_resolver::TypeResolver;
use super::types::{IRProgram, TyResult};
use crate::ast::Stmt as AstStmt;
use context::LowerCtx;

/// Public IR struct that provides the main entry point for lowering
pub struct IR;

impl IR {
    /// Convert an AST program to typed IR
    ///
    /// This is the main entry point for the lowering phase.
    /// It takes a list of AST statements and a type resolver,
    /// and returns a typed IR program or a type error.
    pub fn from_ast(program: Vec<AstStmt>, resolver: &mut TypeResolver) -> TyResult<IRProgram> {
        let mut lower = LowerCtx::new(resolver);
        lower.lower_program(program)
    }
}

/// Legacy function for backwards compatibility
pub fn lower_program(program: Vec<AstStmt>) -> TyResult<IRProgram> {
    let mut resolver = TypeResolver::new();
    IR::from_ast(program, &mut resolver)
}
