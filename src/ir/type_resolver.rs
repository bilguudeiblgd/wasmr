use std::collections::HashMap;
use crate::types::{Param, ParamKind, Type};
use super::types::{BuiltinKind, TyResult, TypeError};

pub struct TypeResolver {
    /// Unified scope stack for both variables and functions
    /// Only functions create new scopes - blocks/if/while/for do NOT
    /// Stack: [global scope, function1 scope, nested_function scope, ...]
    pub(crate) scope_stack: Vec<HashMap<String, Type>>,
    /// Builtin function metadata
    pub(crate) builtins: HashMap<String, BuiltinDescriptor>,
    /// For better error messages in returns and handling of varargs
    pub(crate) current_function: Option<FunctionCtx>,
    /// Function parameter definitions (with defaults) for named argument resolution
    /// Maps function name to its parameter definitions
    pub(crate) function_param_defs: HashMap<String, Vec<crate::ast::ParamDef>>,
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
        builtins.insert(
            "stop".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::Stop,
                return_type: Type::Void,
            }
        );
        builtins.insert(
            "vec".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::Vector,
                return_type: Type::Vector(Type::Double.into()),
            }
        );
        builtins.insert(
            "as.integer".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::AsInt,
                return_type: Type::Any, // Actual return type depends on input (scalar or vector)
            }
        );
        builtins.insert(
            "as.double".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::AsDouble,
                return_type: Type::Any,
            }
        );
        builtins.insert(
            "as.logical".to_string(),
            BuiltinDescriptor {
                kind: BuiltinKind::AsLogical,
                return_type: Type::Any,
            }
        );

        let mut function_param_defs = HashMap::new();

        // Register parameter definitions for vec() built-in
        // vec(length: int = 0, mode: string = "double") creates a vector of given length
        function_param_defs.insert(
            "vec".to_string(),
            vec![
                crate::ast::ParamDef {
                    param: Param {
                        name: "length".to_string(),
                        kind: ParamKind::Normal(Type::Int),
                    },
                    default_value: Some(Box::new(crate::ast::Expr::Number("0".to_string()))),
                },
                crate::ast::ParamDef {
                    param: Param {
                        name: "mode".to_string(),
                        kind: ParamKind::Normal(Type::String),
                    },
                    default_value: Some(Box::new(crate::ast::Expr::XString("double".to_string()))),
                },
            ],
        );

        Self {
            scope_stack: vec![HashMap::new()],  // Start with one global scope
            builtins,
            current_function: None,
            function_param_defs,
        }
    }

    pub(crate) fn is_numeric(t: &Type) -> bool {
        matches!(t, Type::Int | Type::Double | Type::Logical)
    }

    pub(crate) fn promote_numeric(&self, a: &Type, target: &Type) -> Type {
        // Handle scalar numeric promotions only
        if Self::is_numeric(a) && Self::is_numeric(target) {
            return target.clone();
        }

        // Handle numeric to logical conversions
        if Self::is_numeric(a) && matches!(target, Type::Logical) {
            return target.clone();
        }

        // NO vector numeric promotions - users must use explicit as.integer(), as.double(), etc.

        // No promotion available
        a.clone()
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
            Logical => 0,
            Int => 1,
            Double => 2,
            _ => -1,
        };
        Ok(match (rank(l), rank(r)) {
            (0, 0) => Logical,
            (1, 0) | (0, 1) => Int,
            (2, _) | (_, 2) => Double,
            _ => Int,
        })
    }

    /// Enter a new scope (push empty HashMap onto stack)
    /// Only called for function definitions
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    /// Exit current scope (pop from stack)
    /// Only called when exiting function definitions
    pub fn exit_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
        // Don't pop the last scope (global scope)
    }

    /// Look up variable in scope stack (innermost to outermost)
    /// Searches all function scopes from current outward
    pub fn lookup_var(&self, name: &str) -> Option<Type> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    /// Define variable in current (innermost) scope
    /// Variables go into the current function scope
    pub fn define_var(&mut self, name: String, ty: Type) {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope.insert(name, ty);
        }
    }

    /// Assign to variable in parent scopes (for <<-)
    /// Skips current scope and searches outward
    pub fn super_assign(&mut self, name: &str, ty: Type) -> Result<(), TypeError> {
        let len = self.scope_stack.len();
        if len <= 1 {
            // At global scope, no parent to assign to
            return Err(TypeError::UnknownVariable(name.to_string()));
        }

        // Search from second-to-top scope outward (skip index len-1, which is current)
        for scope in self.scope_stack[..len-1].iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), ty);
                return Ok(());
            }
        }

        // Variable not found in any parent scope
        Err(TypeError::UnknownVariable(name.to_string()))
    }
}
