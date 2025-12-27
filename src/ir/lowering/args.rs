use crate::ast::{Argument, Expr as AstExpr};
use crate::ir::types::TypeError;

/// Extract expression from an Argument (handles both Positional and Named)
/// TODO: Remove this once all call sites use match_arguments_to_params
pub(crate) fn extract_arg_expr(arg: Argument) -> AstExpr {
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
pub(crate) fn match_arguments_to_params(
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
