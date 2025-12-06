/// Pass Manager for orchestrating IR passes

use super::{Pass, PassError};
use crate::ir::passes::variable_collection::VariableCollectionPass;
use crate::ir::types::IRProgram;

/// Manages and executes IR passes in sequence
pub struct IRPassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl IRPassManager {
    /// Create an empty pass manager
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    /// Add a pass to the pipeline
    ///
    /// Passes are executed in the order they are added
    pub fn add_pass<P: Pass + 'static>(&mut self, pass: P) {
        self.passes.push(Box::new(pass));
    }

    /// Execute all passes in sequence
    ///
    /// If any pass fails, execution stops and the error is returned
    pub fn run(&mut self, program: &mut IRProgram) -> Result<(), PassError> {
        for pass in &mut self.passes {
            pass.run(program)?;
        }
        Ok(())
    }

    /// Create a default pass pipeline with standard passes
    ///
    /// The default pipeline includes:
    /// 1. Variable collection - collects all variables and assigns local indices
    /// 2. (Future) Constant folding, dead code elimination, etc.
    pub fn default_pipeline() -> Self {
        let mut manager = Self::new();
        manager.add_pass(VariableCollectionPass::new());
        // Future passes can be added here:
        // manager.add_pass(ConstantFoldingPass::new());
        // manager.add_pass(DeadCodeEliminationPass::new());
        manager
    }
}

impl Default for IRPassManager {
    fn default() -> Self {
        Self::new()
    }
}
