use crate::types::{Param, ParamKind};
use crate::ir::{CapturedVarInfo, FunctionMetadata};
use std::collections::HashMap;

/// Context for tracking locals within a function
pub(crate) struct LocalContext {
    /// Map variable names to local indices
    locals: HashMap<String, u32>,
    /// Local index for the packed varargs parameter, if present.
    varargs_local: Option<u32>,
    /// Captured variables - maps name to field index in environment struct
    captured_vars: HashMap<String, CapturedVarInfo>,
    /// Local index of the environment parameter (always 0 for functions with captured vars)
    env_param_index: Option<u32>,
    /// Base environment struct type index (for parameter type)
    env_struct_type_idx: Option<u32>,
    /// Concrete environment struct type index (for downcasting)
    concrete_env_type_idx: Option<u32>,
    /// Local index of the downcasted typed environment (for accessing captured vars)
    typed_env_local: Option<u32>,
    /// Variables that need reference cells (for super-assignment)
    ref_cell_vars: HashMap<String, ()>,
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
            concrete_env_type_idx: None,
            typed_env_local: None,
            ref_cell_vars: HashMap::new(),
        }
    }

    /// Create LocalContext from precomputed FunctionMetadata
    /// If is_closure is true, assumes environment is at local index 0
    pub(crate) fn from_metadata(metadata: &FunctionMetadata) -> Self {
        Self::from_metadata_with_env(metadata, None, None, None, None)
    }

    /// Create LocalContext from precomputed FunctionMetadata with closure environment
    /// env_struct_type_idx is the base type, concrete_env_type_idx is for downcasting
    pub(crate) fn from_metadata_with_env(
        metadata: &FunctionMetadata,
        env_param_index: Option<u32>,
        env_struct_type_idx: Option<u32>,
        concrete_env_type_idx: Option<u32>,
        typed_env_local: Option<u32>
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

        // Build ref cell vars map
        let mut ref_cell_vars = HashMap::new();
        for var_info in &metadata.local_vars {
            if var_info.need_reference {
                ref_cell_vars.insert(var_info.name.clone(), ());
            }
        }

        Self {
            locals,
            varargs_local,
            captured_vars,
            env_param_index,
            env_struct_type_idx,
            concrete_env_type_idx,
            typed_env_local,
            ref_cell_vars,
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

    /// Check if a variable needs a reference cell (for super-assignment)
    pub(crate) fn needs_ref_cell(&self, name: &str) -> bool {
        self.ref_cell_vars.contains_key(name)
    }

    /// Get concrete environment struct type index (for downcasting)
    pub(crate) fn concrete_env_type_idx(&self) -> Option<u32> {
        self.concrete_env_type_idx
    }

    /// Get typed environment local index (downcasted environment)
    pub(crate) fn typed_env_local(&self) -> Option<u32> {
        self.typed_env_local
    }
}
