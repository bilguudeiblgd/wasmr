use crate::ir::types::IRExpr;
use crate::types::Type;

/// Generate a mangled function name for overload resolution
/// Follows the scheme: system_name___arg1__arg2__arg3
/// Examples:
///   max(int, int) -> system_max___int__int
///   sum(vec<int>) -> system_sum___vec_int
pub(crate) fn mangle_function_name(base_name: &str, args: &[IRExpr]) -> String {
    if args.is_empty() {
        return base_name.to_string();
    }

    let mut mangled = format!("system_{}", base_name);
    for (i, arg) in args.iter().enumerate() {
        if i == 0 {
            mangled.push_str("___");  // THREE underscores before first arg
        } else {
            mangled.push_str("__");   // TWO underscores between args
        }
        mangled.push_str(&mangle_type(&arg.ty));
    }
    mangled
}

/// Convert a type to its mangled string representation
/// Examples: Int -> "int", Vector(Int) -> "vec_int", Double -> "double"
pub(crate) fn mangle_type(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Double => "double".to_string(),
        Type::Logical => "logical".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::Vector(inner) => format!("vec_{}", mangle_type(inner)),
        Type::Function { .. } => "func".to_string(),
        _ => "any".to_string(),
    }
}
