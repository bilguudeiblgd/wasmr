use crate::ast::{Param, ParamKind};
use std::collections::HashMap;

/// Context for tracking locals within a function
pub(crate) struct LocalContext {
    /// Map variable names to local indices
    locals: HashMap<String, u32>,
    /// Local index for the packed varargs parameter, if present.
    varargs_local: Option<u32>,
}

impl LocalContext {
    pub(crate) fn new(params: Vec<Param>) -> Self {
        let mut locals = HashMap::new();
        let mut varargs_local = None;
        // Parameters are locals 0..param_count
        for (i, param) in params.iter().enumerate() {
            locals.insert(param.name.clone(), i as u32);
            if matches!(param.kind, ParamKind::VarArgs) {
                varargs_local = Some(i as u32);
            }
        }
        Self {
            locals,
            varargs_local,
        }
    }

    pub(crate) fn get_local(&self, name: &str) -> Option<u32> {
        self.locals.get(name).copied()
    }

    pub(crate) fn add_local(&mut self, name: String, index: u32) {
        self.locals.insert(name, index);
    }

    pub(crate) fn varargs_local(&self) -> Option<u32> {
        self.varargs_local
    }
}
