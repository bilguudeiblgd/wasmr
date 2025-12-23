//! Type system definitions for the Rty compiler
//!
//! This module contains the core type definitions used across all compilation phases:
//! lexer, parser, IR, and code generation.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Double,
    String,
    Vector(Box<Type>),
    List,
    Char,
    Void,
    Logical,
    Any,
    Reference(Box<Type>),
    /// Internal type used to represent packed `...` values.
    VarArgs,
    /// Function type with signature: parameters and return type
    /// May represent either a bare function or a closure (determined at codegen)
    Function {
        params: Vec<Param>,
        return_type: Box<Type>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParamKind {
    Normal(Type),
    VarArgs,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub name: String,
    pub kind: ParamKind,
}
