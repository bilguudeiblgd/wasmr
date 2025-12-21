//! Cross-cutting type system for the Rty compiler
//!
//! This module contains type definitions and utilities used across all compilation phases.
//! By extracting types from the AST module, we eliminate false coupling and make it clear
//! that types are a fundamental, cross-cutting concern.

mod ast_types;
mod builtins;

// Re-export all public types
pub use ast_types::{Type, Param, ParamKind};
pub use builtins::{is_builtin_type_name, map_builtin_type};
