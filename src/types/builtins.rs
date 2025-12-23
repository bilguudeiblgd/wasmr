//! Built-in type utilities for recognizing and mapping type names

use super::Type;

/// Return true if identifier is a built-in type name the lexer should tag as Token::Type.
pub fn is_builtin_type_name(name: &str) -> bool {
    matches!(
        name,
        "int" | "double" | "string" | "char"
        | "void" | "logical" | "any" | "vector" | "list"
    )
}

/// Map a built-in type name to the AST Type enum.
/// Returns None for unknown names or names that don't have a corresponding Type variant (e.g., "bool").
pub fn map_builtin_type(name: &str) -> Option<Type> {
    match name {
        "int" => Some(Type::Int),
        "double" => Some(Type::Double),
        "string" => Some(Type::String),
        "vector" => Some(Type::Vector(Box::new(Type::Any))),
        "list" => Some(Type::List),
        "char" => Some(Type::Char),
        "void" => Some(Type::Void),
        "logical" => Some(Type::Logical),
        _ => None,
    }
}
