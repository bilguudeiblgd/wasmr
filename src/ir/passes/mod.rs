/// IR Pass infrastructure for the Rty compiler
///
/// Passes transform or analyze IR, computing metadata or optimizing code.
/// Passes run sequentially via PassManager after initial IR generation.

use super::types::IRProgram;
use super::TypeError;

pub mod manager;
pub mod variable_collection;
pub mod captured_vars;
pub mod function_flattening;

// Re-export for convenience
pub use manager::IRPassManager;

/// A pass that analyzes or transforms IR
pub trait Pass {
    /// Human-readable name for debugging/logging
    fn name(&self) -> &'static str;

    /// Execute the pass on an IR program
    ///
    /// Passes can:
    /// - Add metadata to IR nodes
    /// - Transform/optimize IR structure
    /// - Perform semantic analysis
    ///
    /// Returns an error if the pass fails
    fn run(&mut self, program: &mut IRProgram) -> Result<(), PassError>;
}

/// Errors that can occur during pass execution
#[derive(Debug, Clone)]
pub enum PassError {
    /// Type error detected during pass
    TypeError(TypeError),

    /// Internal error in pass implementation
    InternalError(String),

    /// Pass precondition not met (e.g., required metadata missing)
    PreconditionFailed {
        pass_name: String,
        message: String,
    },
}

impl From<TypeError> for PassError {
    fn from(err: TypeError) -> Self {
        PassError::TypeError(err)
    }
}

impl std::fmt::Display for PassError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PassError::TypeError(e) => write!(f, "Type error: {:?}", e),
            PassError::InternalError(msg) => write!(f, "Internal error: {}", msg),
            PassError::PreconditionFailed { pass_name, message } => {
                write!(f, "Pass '{}' precondition failed: {}", pass_name, message)
            }
        }
    }
}

impl std::error::Error for PassError {}
