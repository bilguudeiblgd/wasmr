use crate::ast::{Param, ParamKind};
use crate::ir::{CapturedVarInfo, FunctionMetadata};
use std::collections::HashMap;

/// Context for tracking locals within a function
pub(crate) struct LocalContext {
    /// Map variable names to local indices
    locals: HashMap<String, u32>,
    /// Local index for the packed varargs parameter, if present.
    varargs_local: Option<u32>,
    /// Captured variables (for closures) - maps name to field index in environment struct
    captured_vars: HashMap<String, CapturedVarInfo>,
    /// Local index of the environment parameter (always 0 for closures)
    env_param_index: Option<u32>,
    /// Environment struct type index (for closures)
    env_struct_type_idx: Option<u32>,
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
            captured_vars: HashMap::new(),
            env_param_index: None,
            env_struct_type_idx: None,
        }
    }

    /// Create LocalContext from precomputed FunctionMetadata
    /// If is_closure is true, assumes environment is at local index 0
    pub(crate) fn from_metadata(metadata: &FunctionMetadata) -> Self {
        Self::from_metadata_with_env(metadata, None, None)
    }

    /// Create LocalContext from precomputed FunctionMetadata with closure environment
    pub(crate) fn from_metadata_with_env(
        metadata: &FunctionMetadata,
        env_param_index: Option<u32>,
        env_struct_type_idx: Option<u32>
    ) -> Self {
        let mut locals = HashMap::new();

        // If there's an environment parameter, all other locals shift by 1
        let index_offset = if env_param_index.is_some() { 1 } else { 0 };

        // Add all local variables from metadata with adjusted indices
        for var_info in &metadata.local_vars {
            locals.insert(var_info.name.clone(), var_info.index + index_offset);
        }

        // Extract varargs local if present (also needs offset adjustment)
        let varargs_local = metadata.varargs_param.as_ref().map(|v| v.local_index + index_offset);

        // Build captured vars map
        let mut captured_vars = HashMap::new();
        for captured in &metadata.captured_vars {
            captured_vars.insert(captured.name.clone(), captured.clone());
        }

        Self {
            locals,
            varargs_local,
            captured_vars,
            env_param_index,
            env_struct_type_idx,
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

    /// Get captured variable info
    pub(crate) fn get_captured(&self, name: &str) -> Option<&CapturedVarInfo> {
        self.captured_vars.get(name)
    }

    /// Get environment parameter index
    pub(crate) fn env_param_index(&self) -> Option<u32> {
        self.env_param_index
    }

    /// Get environment struct type index
    pub(crate) fn env_struct_type_idx(&self) -> Option<u32> {
        self.env_struct_type_idx
    }
}
