use ast::Type;

pub mod lexer;
pub mod ast;
pub mod parser;
pub mod codegen;
pub mod runtime;

/// Return true if identifier is a built-in type name the lexer should tag as Token::Type.
pub fn is_builtin_type_name(name: &str) -> bool {
    matches!(
        name,
        "bool" | "int" | "float" | "double" | "list" | "vector" | "string" | "char" | "void"
    )
}

/// Map a built-in type name to the AST Type enum.
/// Returns None for unknown names or names that don't have a corresponding Type variant (e.g., "bool").
pub fn map_builtin_type(name: &str) -> Option<Type> {
    match name {
        "int" => Some(Type::Int),
        "float" => Some(Type::Float),
        "double" => Some(Type::Double),
        "string" => Some(Type::String),
        "vector" => Some(Type::Vector),
        "list" => Some(Type::List),
        "char" => Some(Type::Char),
        "void" => Some(Type::Void),
        // No Bool variant in Type enum; keep as None
        "bool" => Some(Type::Bool),
        _ => None,
    }
}

