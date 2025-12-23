use std::collections::HashMap;
use crate::types::Type;
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
            scope_stack: vec![HashMap::new()],  // Start with one global scope
            builtins,
            current_function: None,
        }
    }

    pub(crate) fn is_numeric(t: &Type) -> bool {
        matches!(t, Type::Int | Type::Double)
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
            Double => 1,
            _ => -1,
        };
        Ok(match (rank(l), rank(r)) {
            (1, _) | (_, 1) => Double,
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
