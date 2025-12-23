use std::collections::HashMap;
use crate::ast::Type;
use super::types::{BuiltinKind, TyResult, TypeError};

pub struct TypeResolver {
    /// Variable environment
    pub vars: HashMap<String, Type>,
    /// Function environment: name -> (param_types, return_type)
    pub(crate) funcs: HashMap<String, (Vec<crate::ast::Param>, Type)>,
    /// Builtin function metadata
    pub(crate) builtins: HashMap<String, BuiltinDescriptor>,
    /// For better error messages in returns and handling of varargs
    pub(crate) current_function: Option<FunctionCtx>,
}

#[derive(Clone)]
pub(crate) struct FunctionCtx {
    pub(crate) name: String,
    pub(crate) return_type: Type,
    pub(crate) varargs_name: Option<String>,
}

#[derive(Clone)]
#[derive(Debug)]
pub(crate) struct BuiltinDescriptor {
    pub(crate) kind: BuiltinKind,
    pub(crate) return_type: Type,
}

impl TypeResolver {
    pub fn new() -> Self {
        let mut builtins = HashMap::new();
        builtins.insert(
            "c".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::C,
                return_type: Type::Vector(Type::Any.into()),
            },
        );
        builtins.insert(
            "list".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::List,
                return_type: Type::List,
            },
        );
        builtins.insert(
            "print".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::Print,
                return_type: Type::Void,
            }
        );
        builtins.insert(
            "length".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::Length,
                return_type: Type::Int,
            }
        );
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            builtins,
            current_function: None,
        }
    }

    pub(crate) fn is_numeric(t: &Type) -> bool {
        matches!(t, Type::Int | Type::Float | Type::Double)
    }

    pub(crate) fn promote_numeric(&self, a: &Type, target: &Type) -> Type {
        // Only promote if both numeric and target is at least as wide
        if Self::is_numeric(a) && Self::is_numeric(target) {
            target.clone()
        } else {
            a.clone()
        }
    }

    pub(crate) fn unify_numeric(&self, l: &Type, r: &Type) -> TyResult<Type> {
        use Type::*;
        if !Self::is_numeric(l) || !Self::is_numeric(r) {
            return Err(TypeError::UnsupportedOperation {
                op: "numeric op".to_string(),
                left: l.clone(),
                right: r.clone(),
            });
        }
        let rank = |t: &Type| match t {
            Int => 0,
            Float => 1,
            Double => 2,
            _ => -1,
        };
        Ok(match (rank(l), rank(r)) {
            (2, _) | (_, 2) => Double,
            (1, _) | (_, 1) => Float,
            _ => Int,
        })
    }
}
