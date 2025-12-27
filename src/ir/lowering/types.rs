use crate::types::{ParamKind, Type};

/// Compare two types for compatibility, ignoring parameter names in function types
pub(crate) fn types_compatible(t1: &Type, t2: &Type) -> bool {
    // Type::Any is compatible with any type (acts as a wildcard)
    if matches!(t1, Type::Any) || matches!(t2, Type::Any) {
        return true;
    }

    match (t1, t2) {
        (
            Type::Function {
                params: p1,
                return_type: r1,
            },
            Type::Function {
                params: p2,
                return_type: r2,
            },
        ) => {
            // Check same number of parameters
            if p1.len() != p2.len() {
                return false;
            }
            // Check parameter types match (ignoring names)
            for (param1, param2) in p1.iter().zip(p2.iter()) {
                if !param_kinds_compatible(&param1.kind, &param2.kind) {
                    return false;
                }
            }
            // Check return types match
            types_compatible(r1, r2)
        }
        (Type::Vector(inner1), Type::Vector(inner2)) => types_compatible(inner1, inner2),
        (Type::Reference(inner1), Type::Reference(inner2)) => types_compatible(inner1, inner2),
        _ => t1 == t2, // For non-function types, use regular equality
    }
}

/// Compare parameter kinds for compatibility (recursive for nested function types)
fn param_kinds_compatible(k1: &ParamKind, k2: &ParamKind) -> bool {
    match (k1, k2) {
        (ParamKind::Normal(ty1), ParamKind::Normal(ty2)) => types_compatible(ty1, ty2),
        (ParamKind::VarArgs, ParamKind::VarArgs) => true,
        _ => false,
    }
}
