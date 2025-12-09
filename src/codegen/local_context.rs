use crate::ast::{Param, ParamKind};
use crate::ir::FunctionMetadata;
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

    /// Create LocalContext from precomputed FunctionMetadata
    pub(crate) fn from_metadata(metadata: &FunctionMetadata) -> Self {
        let mut locals = HashMap::new();

        // Add all local variables from metadata
        for var_info in &metadata.local_vars {
            locals.insert(var_info.name.clone(), var_info.index);
        }

        // Extract varargs local if present
        let varargs_local = metadata.varargs_param.as_ref().map(|v| v.local_index);

        Self {
            locals,
            varargs_local,
        }
    }

    pub(crate) fn get_local(&self, name: &str) -> Option<u32> {
        self.locals.get(name).copied()
    }

    pub(crate) fn has_local(&self, name: &str) -> bool {
        self.locals.contains_key(name)
    }

    pub(crate) fn add_local(&mut self, name: String, index: u32) {
        self.locals.insert(name, index);
    }

    pub(crate) fn varargs_local(&self) -> Option<u32> {
        self.varargs_local
    }
}
