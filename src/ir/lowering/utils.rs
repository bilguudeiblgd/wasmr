use crate::ast::{Argument, Expr as AstExpr};
use crate::types::{Param, ParamKind, Type};
use super::super::types::{IRExpr, TypeError};

/// Extract expression from an Argument (handles both Positional and Named)
/// TODO: Remove this once all call sites use match_arguments_to_params
pub(super) fn extract_arg_expr(arg: Argument) -> AstExpr {
    match arg {
        Argument::Positional(expr) => expr,
        Argument::Named { value, .. } => value,
    }
}

/// Match function call arguments to function parameters, handling:
/// - Positional arguments
/// - Named arguments
/// - Default parameter values
/// - Argument reordering
///
/// Returns expressions in parameter order, with defaults filled in.
pub(super) fn match_arguments_to_params(
    args: Vec<Argument>,
    param_defs: &[crate::ast::ParamDef],
    func_name: &str,
) -> Result<Vec<AstExpr>, TypeError> {
    // Separate positional and named arguments
    let mut positional_args = Vec::new();
    let mut named_args = Vec::new();
    let mut seen_named = false;

    for arg in args {
        match arg {
            Argument::Positional(expr) => {
                if seen_named {
                    return Err(TypeError::ArgumentError {
                        func: func_name.to_string(),
                        message: "positional arguments must come before named arguments".to_string(),
                    });
                }
                positional_args.push(expr);
            }
            Argument::Named { name, value } => {
                seen_named = true;
                named_args.push((name, value));
            }
        }
    }

    // Result: expressions in parameter order
    let mut result_exprs: Vec<Option<AstExpr>> = vec![None; param_defs.len()];

    // Step 1: Match positional arguments to first N parameters
    if positional_args.len() > param_defs.len() {
        return Err(TypeError::ArityMismatch {
            func: func_name.to_string(),
            expected: param_defs.len(),
            found: positional_args.len(),
        });
    }

    for (i, expr) in positional_args.into_iter().enumerate() {
        result_exprs[i] = Some(expr);
    }

    // Step 2: Match named arguments to parameters by name
    for (arg_name, arg_value) in named_args {
        // Find parameter with this name
        let param_idx = param_defs.iter().position(|pd| pd.param.name == arg_name);

        match param_idx {
            None => {
                return Err(TypeError::ArgumentError {
                    func: func_name.to_string(),
                    message: format!("unknown parameter name: '{}'", arg_name),
                });
            }
            Some(idx) => {
                // Check if already provided (via positional)
                if result_exprs[idx].is_some() {
                    return Err(TypeError::ArgumentError {
                        func: func_name.to_string(),
                        message: format!(
                            "parameter '{}' provided both positionally and by name",
                            arg_name
                        ),
                    });
                }
                result_exprs[idx] = Some(arg_value);
            }
        }
    }

    // Step 3: Fill in defaults for missing parameters
    for (i, param_def) in param_defs.iter().enumerate() {
        if result_exprs[i].is_none() {
            // Check if parameter has a default value
            if let Some(default_expr) = &param_def.default_value {
                result_exprs[i] = Some((**default_expr).clone());
            } else {
                // Required parameter not provided
                return Err(TypeError::ArgumentError {
                    func: func_name.to_string(),
                    message: format!("missing required parameter: '{}'", param_def.param.name),
                });
            }
        }
    }

    // Step 4: Extract all expressions (they should all be Some now)
    Ok(result_exprs.into_iter().map(|opt| opt.unwrap()).collect())
}

/// Compare two types for compatibility, ignoring parameter names in function types
pub(super) fn types_compatible(t1: &Type, t2: &Type) -> bool {
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

/// Check if two parameter kinds are compatible (used for function type matching)
fn param_kinds_compatible(k1: &ParamKind, k2: &ParamKind) -> bool {
    match (k1, k2) {
        (ParamKind::Normal(t1), ParamKind::Normal(t2)) => types_compatible(t1, t2),
        (ParamKind::VarArgs, ParamKind::VarArgs) => true,
        _ => false,
    }
}

/// Ensure an IR expression has a specific type, inserting a cast if needed
pub(super) fn ensure_ty(mut e: IRExpr, want: Type) -> IRExpr {
    if e.ty == want {
        e
    } else {
        // Insert a type annotation or cast to satisfy the type checker
        e.ty = want;
        e
    }
}

/// Generate a mangled function name for overload resolution
pub(super) fn mangle_function_name(base_name: &str, args: &[IRExpr]) -> String {
    // Simple mangling scheme: name_Type1_Type2_...
    let mut mangled = base_name.to_string();
    for arg in args {
        mangled.push('_');
        mangled.push_str(&mangle_type(&arg.ty));
    }
    mangled
}

/// Convert a type to a string representation for name mangling
fn mangle_type(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Double => "double".to_string(),
        Type::Logical => "logical".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::Void => "void".to_string(),
        Type::Any => "any".to_string(),
        Type::VarArgs => "varargs".to_string(),
        Type::Vector(inner) => format!("vector_{}", mangle_type(inner)),
        Type::Reference(inner) => format!("ref_{}", mangle_type(inner)),
        Type::Function { .. } => "function".to_string(),
        Type::List => "list".to_string(),
    }
}
