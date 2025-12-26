use super::super::type_resolver::BuiltinDescriptor;
use super::super::types::{BuiltinKind, IRExpr, IRExprKind, TyResult, TypeError};
use super::utils::ensure_ty;
use crate::types::Type;
use crate::types::Type::Vector;

/// Lower a builtin function call to IR
pub(super) fn lower_builtin_call(
    tr: &mut super::super::type_resolver::TypeResolver,
    descriptor: &BuiltinDescriptor,
    args: Vec<IRExpr>,
    name: &str,
) -> TyResult<IRExpr> {
    let kind = descriptor.kind;
    let return_ty = descriptor.return_type.clone();

    // c() and list() can be called with 0 args to create empty collections
    // Other builtins require at least 1 argument
    if args.is_empty() && !matches!(kind, BuiltinKind::C | BuiltinKind::List) {
        return Err(TypeError::ArityMismatch {
            func: name.to_string(),
            expected: 1,
            found: 0,
        });
    }

    let has_varargs = args.iter().any(|a| matches!(a.ty, Type::VarArgs));
    if has_varargs && args.len() != 1 {
        return Err(TypeError::UnsupportedOperation {
            op: format!("{}: mixing ... with positional arguments", name),
            left: Type::VarArgs,
            right: Type::Any,
        });
    }

    match kind {
        BuiltinKind::C => lower_builtin_c(tr, args, has_varargs, return_ty),
        BuiltinKind::List => lower_builtin_list(args, has_varargs, kind, return_ty, name),
        BuiltinKind::Print => lower_builtin_print(args, kind, return_ty, name),
        BuiltinKind::Length => lower_builtin_length(args, kind, return_ty, name),
        BuiltinKind::Stop => lower_builtin_stop(args, kind, return_ty, name),
        BuiltinKind::Vector => lower_builtin_vector(args, kind, return_ty, name),
    }
}

/// Lower the builtin c() function (vector concatenation)
fn lower_builtin_c(
    tr: &mut super::super::type_resolver::TypeResolver,
    args: Vec<IRExpr>,
    has_varargs: bool,
    return_ty: Type,
) -> TyResult<IRExpr> {
    if has_varargs {
        return Ok(IRExpr {
            kind: IRExprKind::VectorLiteral(args),
            ty: return_ty,
        });
    }

    // Handle empty c() - create empty int vector
    if args.is_empty() {
        return Ok(IRExpr {
            kind: IRExprKind::VectorLiteral(vec![]),
            ty: Vector(Box::new(Type::Int)),
        });
    }

    let mut numeric_target: Option<Type> = None;
    for arg in &args {
        numeric_target = Some(match &numeric_target {
            None => arg.ty.clone(),
            Some(acc) => tr.unify_numeric(acc, &arg.ty)?,
        });
    }

    let target = numeric_target.unwrap_or(Type::Int);
    let coerced_args: Vec<IRExpr> = args
        .into_iter()
        .map(|arg| ensure_ty(arg, target.clone()))
        .collect();
    // #TODO: this makes program not able to initialize C vectors with non-numeric types. We'll extend it with Any types soon as soon as codegen supports.
    let vector_type = coerced_args.first().unwrap().ty.clone();

    Ok(IRExpr {
        kind: IRExprKind::VectorLiteral(coerced_args),
        ty: Vector(vector_type.into()),
    })
}

/// Lower the builtin list() function
fn lower_builtin_list(
    args: Vec<IRExpr>,
    has_varargs: bool,
    kind: BuiltinKind,
    return_ty: Type,
    name: &str,
) -> TyResult<IRExpr> {
    if has_varargs {
        return Ok(IRExpr {
            kind: IRExprKind::BuiltinCall {
                builtin: kind,
                args,
            },
            ty: return_ty,
        });
    }

    let mut element_ty: Option<Type> = None;
    for (idx, arg) in args.iter().enumerate() {
        if let Some(expected) = &element_ty {
            if expected != &arg.ty {
                return Err(TypeError::TypeMismatch {
                    expected: expected.clone(),
                    found: arg.ty.clone(),
                    context: format!("argument {} for {}", idx, name),
                });
            }
        } else {
            element_ty = Some(arg.ty.clone());
        }
    }

    Ok(IRExpr {
        kind: IRExprKind::BuiltinCall {
            builtin: kind,
            args,
        },
        ty: return_ty,
    })
}

/// Lower the builtin print() function
fn lower_builtin_print(
    args: Vec<IRExpr>,
    kind: BuiltinKind,
    return_ty: Type,
    name: &str,
) -> TyResult<IRExpr> {
    if args.len() != 1 {
        return Err(TypeError::ArityMismatch {
            func: name.to_string(),
            expected: 1,
            found: args.len(),
        });
    }

    // Accept Int, Double, or Logical for printing
    if !matches!(
        args[0].ty,
        Type::Int | Type::Double | Type::Logical
    ) {
        return Err(TypeError::TypeMismatch {
            expected: Type::Int,
            found: args[0].ty.clone(),
            context: format!(
                "print argument (expected int, double, or logical)"
            ),
        });
    }

    Ok(IRExpr {
        kind: IRExprKind::BuiltinCall {
            builtin: kind,
            args,
        },
        ty: return_ty,
    })
}

/// Lower the builtin length() function
fn lower_builtin_length(
    args: Vec<IRExpr>,
    kind: BuiltinKind,
    return_ty: Type,
    name: &str,
) -> TyResult<IRExpr> {
    if args.len() != 1 {
        return Err(TypeError::ArityMismatch {
            func: name.to_string(),
            expected: 1,
            found: args.len(),
        });
    }

    // Accept only vector types
    if !matches!(args[0].ty, Type::Vector(_)) {
        return Err(TypeError::TypeMismatch {
            expected: Type::Vector(Box::new(Type::Any)),
            found: args[0].ty.clone(),
            context: format!("length() argument (expected vector)"),
        });
    }

    Ok(IRExpr {
        kind: IRExprKind::BuiltinCall {
            builtin: kind,
            args,
        },
        ty: return_ty,
    })
}

/// Lower the builtin stop() function
fn lower_builtin_stop(
    args: Vec<IRExpr>,
    kind: BuiltinKind,
    return_ty: Type,
    name: &str,
) -> TyResult<IRExpr> {
    if args.len() != 1 {
        return Err(TypeError::ArityMismatch {
            func: name.to_string(),
            expected: 1,
            found: args.len(),
        });
    }

    // Accept string messages for now
    // In the future, we might support other types
    // For now, we'll just check that there's an argument
    // The actual error message handling will be done in codegen

    Ok(IRExpr {
        kind: IRExprKind::BuiltinCall {
            builtin: kind,
            args,
        },
        ty: return_ty,
    })
}

/// Lower the builtin vec() function (creates a vector of given length)
fn lower_builtin_vector(
    args: Vec<IRExpr>,
    kind: BuiltinKind,
    return_ty: Type,
    name: &str,
) -> TyResult<IRExpr> {
    // vec(length: int) creates a vector of given length
    if args.len() != 1 {
        return Err(TypeError::ArityMismatch {
            func: name.to_string(),
            expected: 1,
            found: args.len(),
        });
    }

    // Check that length argument is an integer
    if !matches!(args[0].ty, Type::Int) {
        return Err(TypeError::TypeMismatch {
            expected: Type::Int,
            found: args[0].ty.clone(),
            context: format!("vec() length parameter"),
        });
    }

    Ok(IRExpr {
        kind: IRExprKind::BuiltinCall {
            builtin: kind,
            args,
        },
        ty: return_ty,
    })
}
