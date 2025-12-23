/// Function Flattening Pass
///
/// Extracts all nested function definitions to the top-level IRProgram.functions vector.
/// This prepares the IR for WASM codegen, which doesn't support nested functions.
///
/// The pass:
/// 1. Recursively traverses all function bodies
/// 2. Extracts nested FunctionDef statements
/// 3. Adds them to IRProgram.functions
/// 4. Leaves function references in place (function names are still accessible)

use crate::ir::{IRBlock, IRProgram, IRStmt, IRExpr, IRExprKind};
use super::{Pass, PassError};

pub struct FunctionFlatteningPass;

impl FunctionFlatteningPass {
    pub fn new() -> Self {
        Self
    }

    /// Recursively extract nested functions from statements
    fn extract_functions(&self, block: &mut IRBlock, functions: &mut Vec<IRStmt>) {
        let mut i = 0;
        while i < block.stmts.len() {
            // Need to check type before borrowing mutably
            let is_function_def = matches!(&block.stmts[i], IRStmt::FunctionDef { .. });

            if is_function_def {
                if let IRStmt::FunctionDef { name, params, return_type, body, metadata, .. } = &mut block.stmts[i] {
                    // Extract nested functions from this function's body first
                    self.extract_functions(body, functions);

                    // Collect info needed for closure creation
                    let func_name = name.clone();
                    let func_params = params.clone();
                    let func_return_type = return_type.clone();
                    let is_closure = metadata.as_ref()
                        .map(|m| m.is_closure)
                        .unwrap_or(false);
                    let captured_var_names: Vec<String> = metadata.as_ref()
                        .map(|m| m.captured_vars.iter().map(|c| c.name.clone()).collect())
                        .unwrap_or_default();

                    // Remove this function from parent and add to top-level
                    let func = block.stmts.remove(i);
                    functions.push(func);

                    // Functions with captured variables get env as first parameter
                    // Closure detection is based on whether the function returns a function type

                    // Don't increment i (removed element, so next is at same index)
                } else {
                    unreachable!()
                }
            } else {
                // Handle other statement types
                match &mut block.stmts[i] {
                    IRStmt::If { then_branch, else_branch, .. } => {
                        self.extract_functions(then_branch, functions);
                        if let Some(else_stmts) = else_branch {
                            self.extract_functions(else_stmts, functions);
                        }
                    }
                    IRStmt::For { body, .. } | IRStmt::While { body, .. } => {
                        self.extract_functions(body, functions);
                    }
                    IRStmt::Block(inner_body) => {
                        self.extract_functions(inner_body, functions);
                    }
                    _ => {}
                }
                i += 1;
            }
        }
    }
}

impl Pass for FunctionFlatteningPass {
    fn name(&self) -> &'static str {
        "function_flattening"
    }

    fn run(&mut self, program: &mut IRProgram) -> Result<(), PassError> {
        // Extract functions from main function's body
        if let IRStmt::FunctionDef { body, .. } = &mut program.main_function {
            let mut extracted_functions = Vec::new();
            self.extract_functions(body, &mut extracted_functions);
            program.functions = extracted_functions;
        }

        Ok(())
    }
}
