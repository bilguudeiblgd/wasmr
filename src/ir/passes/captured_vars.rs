use crate::types::{Param, ParamKind, Type};
use crate::ir::{IRBlock, IRExpr, IRExprKind, IRProgram, IRStmt, Pass, PassError};
use crate::ir::types::{CapturedVarInfo, FunctionMetadata, LocalVarInfo};
use std::collections::{HashMap, HashSet};

/// Captured variables analysis pass
///
/// Analyzes which variables are captured from parent scopes and populates
/// the `captured_vars` field in `FunctionMetadata`.
///
/// This pass identifies variables that a function references but doesn't define locally,
/// meaning they come from a parent function scope. These variables need to be passed
/// through an environment struct.
///
/// Algorithm:
/// 1. Build a map of which variables are defined in which function scope
/// 2. For each function, find all variables it references
/// 3. If a referenced variable is defined in a parent function, it's captured
/// 4. Store captured variables in function metadata with their types and mutability
/// 5. Mark functions that have captured variables with `is_closure = true`
pub struct CapturedVarsPass;

impl CapturedVarsPass {
    pub fn new() -> Self {
        Self
    }

    /// Find captured variables for a function, tracking which are super-assigned
    /// Returns: (captured_vars, super_assigned_vars, transitively_needed_vars)
    ///
    /// The transitively_needed_vars includes both:
    /// - Variables this function directly references
    /// - Variables that nested functions need (to be passed through)
    fn find_captured_variables(
        &self,
        params: &[Param],
        body: &[IRStmt],
        parent_vars: &HashMap<String, Type>,
    ) -> Result<(Vec<CapturedVarInfo>, HashSet<String>, HashSet<String>), PassError> {
        // Track local variables (params + local declarations)
        let mut local_vars = HashSet::new();
        for param in params {
            if let ParamKind::Normal(_) = &param.kind {
                local_vars.insert(param.name.clone());
            }
        }
        self.collect_local_declarations(body, &mut local_vars);

        // Track variable references and mutations
        let mut referenced_vars = HashSet::new();
        let mut super_assigned_vars = HashSet::new();
        self.collect_references_and_mutations(
            body,
            &mut referenced_vars,
            &mut super_assigned_vars
        );

        // NEW: Collect variables needed by nested functions (transitive captures)
        let nested_function_needs = self.collect_nested_function_needs(body);

        // Combine direct references with transitive needs from nested functions
        let mut transitively_needed = referenced_vars.clone();
        for var_name in &nested_function_needs {
            // Only include if not a local variable (locals don't need to be captured)
            if !local_vars.contains(var_name) {
                transitively_needed.insert(var_name.clone());
            }
        }

        // Find captures: transitively needed, not local, and exists in parent
        let mut captured_info = Vec::new();
        let mut field_index = 1u32;  // Field 0 is function code pointer

        for var_name in &transitively_needed {
            if !local_vars.contains(var_name) {
                if let Some(var_type) = parent_vars.get(var_name) {
                    // This is a captured variable (either direct or transitive)
                    let is_mutable = super_assigned_vars.contains(var_name);

                    captured_info.push(CapturedVarInfo {
                        name: var_name.clone(),
                        ty: var_type.clone(),
                        field_index,
                        is_mutable,
                    });
                    field_index += 1;
                }
            }
        }

        // Return captured vars, super-assigned vars, and transitive needs
        Ok((captured_info, super_assigned_vars, transitively_needed))
    }

    /// Check if a function is returned from the given statements
    fn is_function_returned(&self, stmts: &[IRStmt], func_name: &str) -> bool {
        for stmt in stmts {
            match stmt {
                IRStmt::Return(expr) => {
                    if self.expr_references_function(expr, func_name) {
                        return true;
                    }
                }
                IRStmt::If { then_branch, else_branch, .. } => {
                    if self.is_function_returned(&then_branch.stmts, func_name) {
                        return true;
                    }
                    if let Some(else_block) = else_branch {
                        if self.is_function_returned(&else_block.stmts, func_name) {
                            return true;
                        }
                    }
                }
                IRStmt::For { body, .. } | IRStmt::While { body, .. } => {
                    if self.is_function_returned(&body.stmts, func_name) {
                        return true;
                    }
                }
                IRStmt::Block(block) => {
                    if self.is_function_returned(&block.stmts, func_name) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Check if an expression references a function name
    fn expr_references_function(&self, expr: &IRExpr, func_name: &str) -> bool {
        match &expr.kind {
            IRExprKind::Identifier(name) => name == func_name,
            IRExprKind::Call { callee, .. } => self.expr_references_function(callee, func_name),
            _ => false,
        }
    }

    /// Collect all variables needed by nested functions (for transitive capture propagation)
    ///
    /// This method looks at all nested function definitions and collects the variables
    /// they capture. These variables need to be passed through this function's environment
    /// even if this function doesn't directly use them.
    ///
    /// NOTE: This assumes nested functions have already been analyzed and have their
    /// captured_vars populated in metadata. The analysis must be done bottom-up.
    fn collect_nested_function_needs(&self, stmts: &[IRStmt]) -> HashSet<String> {
        let mut needs = HashSet::new();

        for stmt in stmts {
            match stmt {
                IRStmt::FunctionDef { metadata, body, .. } => {
                    // Collect what this nested function captures
                    if let Some(func_metadata) = metadata {
                        for captured in &func_metadata.captured_vars {
                            needs.insert(captured.name.clone());
                        }
                    }

                    // Recursively collect from deeper nested functions
                    let deeper_needs = self.collect_nested_function_needs(&body.stmts);
                    needs.extend(deeper_needs);
                }
                IRStmt::If { then_branch, else_branch, .. } => {
                    needs.extend(self.collect_nested_function_needs(&then_branch.stmts));
                    if let Some(else_block) = else_branch {
                        needs.extend(self.collect_nested_function_needs(&else_block.stmts));
                    }
                }
                IRStmt::For { body, .. } | IRStmt::While { body, .. } => {
                    needs.extend(self.collect_nested_function_needs(&body.stmts));
                }
                IRStmt::Block(block) => {
                    needs.extend(self.collect_nested_function_needs(&block.stmts));
                }
                _ => {}
            }
        }

        needs
    }

    /// Propagate mutability information from nested functions upward
    ///
    /// If a nested function has a mutable captured variable (due to super-assignment),
    /// we need to mark that same variable in the parent function:
    /// - If it's in parent's local_vars -> mark need_reference = true
    /// - If it's in parent's captured_vars -> mark is_mutable = true
    ///
    /// This ensures the reference chain is maintained through all levels.
    fn propagate_mutability_from_nested_functions(
        &self,
        stmts: &[IRStmt],
        local_vars: &mut Vec<LocalVarInfo>,
        captured_vars: &mut Vec<CapturedVarInfo>,
    ) -> Result<(), PassError> {
        for stmt in stmts {
            match stmt {
                IRStmt::FunctionDef { metadata, body, .. } => {
                    // Check this nested function's captured variables
                    if let Some(func_metadata) = metadata {
                        for captured in &func_metadata.captured_vars {
                            if captured.is_mutable {
                                // This variable is mutated in the nested function
                                // Mark it in our scope accordingly

                                // Check if it's in our local_vars (we define it)
                                let mut found_in_locals = false;
                                for local_var in local_vars.iter_mut() {
                                    if local_var.name == captured.name {
                                        local_var.need_reference = true;
                                        found_in_locals = true;
                                        break;
                                    }
                                }

                                // If not in locals, check if it's in our captured_vars (we also capture it)
                                if !found_in_locals {
                                    for our_captured in captured_vars.iter_mut() {
                                        if our_captured.name == captured.name {
                                            our_captured.is_mutable = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Recursively check deeper nested functions
                    self.propagate_mutability_from_nested_functions(&body.stmts, local_vars, captured_vars)?;
                }
                IRStmt::If { then_branch, else_branch, .. } => {
                    self.propagate_mutability_from_nested_functions(&then_branch.stmts, local_vars, captured_vars)?;
                    if let Some(else_block) = else_branch {
                        self.propagate_mutability_from_nested_functions(&else_block.stmts, local_vars, captured_vars)?;
                    }
                }
                IRStmt::For { body, .. } | IRStmt::While { body, .. } => {
                    self.propagate_mutability_from_nested_functions(&body.stmts, local_vars, captured_vars)?;
                }
                IRStmt::Block(block) => {
                    self.propagate_mutability_from_nested_functions(&block.stmts, local_vars, captured_vars)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Recursively analyze nested functions and populate closure metadata
    ///
    /// This performs a BOTTOM-UP analysis:
    /// 1. First, recursively analyze deeper nested functions
    /// 2. Then, compute captures for current level functions (which can now see what nested functions need)
    ///
    /// This ensures transitive capture propagation works correctly.
    fn analyze_nested_functions(
        &mut self,
        block: &mut IRBlock,
        parent_vars: &HashMap<String, Type>,
        parent_metadata: &mut FunctionMetadata,
    ) -> Result<(), PassError> {
        // First pass: check which functions are returned
        let mut returned_functions = std::collections::HashSet::new();
        for stmt in block.stmts.iter() {
            if let IRStmt::FunctionDef { name: func_name, .. } = stmt {
                if self.is_function_returned(&block.stmts, func_name) {
                    returned_functions.insert(func_name.clone());
                }
            }
        }

        // BOTTOM-UP ANALYSIS: Process in two phases

        // Phase 1: Recursively analyze deeper nested functions FIRST
        // This ensures nested functions have their captured_vars populated
        // before we try to compute transitive captures for parent functions
        for stmt in block.stmts.iter_mut() {
            match stmt {
                IRStmt::FunctionDef { params, body, metadata, .. } => {
                    // Ensure metadata exists
                    let func_metadata = metadata.as_mut()
                        .ok_or_else(|| PassError::PreconditionFailed {
                            pass_name: "closure_analysis".to_string(),
                            message: "Nested function missing metadata".to_string(),
                        })?;

                    // Build complete parent variable map (params + locals from parent)
                    let mut all_parent_vars = parent_vars.clone();
                    for local_var in &parent_metadata.local_vars {
                        all_parent_vars.insert(local_var.name.clone(), local_var.ty.clone());
                    }

                    // Add this function's own parameters and locals to the map for deeper nesting
                    for param in params.iter() {
                        if let ParamKind::Normal(ty) = &param.kind {
                            all_parent_vars.insert(param.name.clone(), ty.clone());
                        }
                    }
                    for local_var in &func_metadata.local_vars {
                        all_parent_vars.insert(local_var.name.clone(), local_var.ty.clone());
                    }

                    // Recursively analyze deeper nested functions FIRST
                    self.analyze_nested_functions(body, &all_parent_vars, func_metadata)?;
                }
                // Also recurse into control flow structures
                IRStmt::If { then_branch, else_branch, .. } => {
                    self.analyze_nested_functions(then_branch, parent_vars, parent_metadata)?;
                    if let Some(else_stmts) = else_branch {
                        self.analyze_nested_functions(else_stmts, parent_vars, parent_metadata)?;
                    }
                }
                IRStmt::For { body, .. } | IRStmt::While { body, .. } => {
                    self.analyze_nested_functions(body, parent_vars, parent_metadata)?;
                }
                IRStmt::Block(inner_block) => {
                    self.analyze_nested_functions(inner_block, parent_vars, parent_metadata)?;
                }
                _ => {}
            }
        }

        // Phase 2: Now compute captures for current level functions
        // At this point, all nested functions have their captured_vars populated,
        // so collect_nested_function_needs will work correctly
        for stmt in block.stmts.iter_mut() {
            if let IRStmt::FunctionDef { params, body, metadata, .. } = stmt {
                let func_metadata = metadata.as_mut().unwrap();

                // Build complete parent variable map
                let mut all_parent_vars = parent_vars.clone();
                for local_var in &parent_metadata.local_vars {
                    all_parent_vars.insert(local_var.name.clone(), local_var.ty.clone());
                }

                // Compute this function's captures (including transitive captures)
                let (mut captured_vars, super_assigned_vars, _transitive_needs) =
                    self.find_captured_variables(params, &body.stmts, &all_parent_vars)?;

                // Mark variables that need references due to super-assignment
                for var_name in &super_assigned_vars {
                    // First check if the variable is in parent's local_vars (defined in parent)
                    if let Some(local_var) = parent_metadata.local_vars
                        .iter_mut()
                        .find(|lv| lv.name == *var_name)
                    {
                        local_var.need_reference = true;
                    }
                    // If not in parent's locals, it must be captured from a higher scope
                    // Mark it as mutable in this function's captured_vars so we receive/pass a reference
                    else {
                        for captured_var in &mut captured_vars {
                            if captured_var.name == *var_name {
                                captured_var.is_mutable = true;
                            }
                        }

                        // Also mark it in parent's captured_vars if parent also captures it
                        for parent_captured in &mut parent_metadata.captured_vars {
                            if parent_captured.name == *var_name {
                                parent_captured.is_mutable = true;
                            }
                        }
                    }
                }

                // NEW: Propagate mutability from nested functions
                // If a nested function has a mutable captured variable, we need to mark it in our scope
                self.propagate_mutability_from_nested_functions(
                    &body.stmts,
                    &mut func_metadata.local_vars,
                    &mut captured_vars,
                )?;

                // Populate this function's metadata
                func_metadata.captured_vars = captured_vars;

                // Functions with captured variables need environment parameter
                func_metadata.is_closure = !func_metadata.captured_vars.is_empty();
            }
        }

        Ok(())
    }

    /// Collect all local variable declarations in statements
    fn collect_local_declarations(&self, stmts: &[IRStmt], locals: &mut HashSet<String>) {
        for stmt in stmts {
            match stmt {
                IRStmt::VarAssign { name, is_super_assign, .. } => {
                    if *is_super_assign == false {
                        locals.insert(name.clone());
                    }
                }
                IRStmt::FunctionDef { name, .. } => {
                    locals.insert(name.clone());
                }
                IRStmt::For { iter_var, body, .. } => {
                    locals.insert(iter_var.0.clone());
                    self.collect_local_declarations(&body.stmts, locals);
                }
                IRStmt::If { then_branch, else_branch, .. } => {
                    self.collect_local_declarations(&then_branch.stmts, locals);
                    if let Some(else_body) = else_branch {
                        self.collect_local_declarations(&else_body.stmts, locals);
                    }
                }
                IRStmt::While { body, .. } => {
                    self.collect_local_declarations(&body.stmts, locals);
                }
                IRStmt::Block(block) => {
                    self.collect_local_declarations(&block.stmts, locals);
                }
                IRStmt::ExprStmt(_) | IRStmt::Return(_) | IRStmt::IndexAssign { .. } => {}
            }
        }
    }

    /// Collect all variable references and track mutations (super-assignments)
    fn collect_references_and_mutations(
        &self,
        stmts: &[IRStmt],
        refs: &mut HashSet<String>,
        mutations: &mut HashSet<String>,
    ) {
        for stmt in stmts {
            match stmt {
                IRStmt::VarAssign { name, value, is_super_assign, .. } => {
                    // Collect references from RHS
                    self.collect_references_from_expr(value, refs, mutations);

                    // If super-assign, mark as mutated
                    if *is_super_assign {
                        mutations.insert(name.clone());
                        // Also add to refs since we're reading it
                        refs.insert(name.clone());
                    }
                }
                IRStmt::ExprStmt(expr) => {
                    self.collect_references_from_expr(expr, refs, mutations);
                }
                IRStmt::Return(expr) => {
                    self.collect_references_from_expr(expr, refs, mutations);
                }
                IRStmt::If { condition, then_branch, else_branch, result_ty: _ } => {
                    self.collect_references_from_expr(condition, refs, mutations);
                    self.collect_references_and_mutations(&then_branch.stmts, refs, mutations);
                    if let Some(else_stmts) = else_branch {
                        self.collect_references_and_mutations(&else_stmts.stmts, refs, mutations);
                    }
                }
                IRStmt::For { iter_expr, body, .. } => {
                    self.collect_references_from_expr(iter_expr, refs, mutations);
                    self.collect_references_and_mutations(&body.stmts, refs, mutations);
                }
                IRStmt::While { condition, body } => {
                    self.collect_references_from_expr(condition, refs, mutations);
                    self.collect_references_and_mutations(&body.stmts, refs, mutations);
                }
                IRStmt::IndexAssign { target, index, value } => {
                    self.collect_references_from_expr(target, refs, mutations);
                    self.collect_references_from_expr(index, refs, mutations);
                    self.collect_references_from_expr(value, refs, mutations);
                }
                IRStmt::Block(block) => {
                    self.collect_references_and_mutations(&block.stmts, refs, mutations);
                }
                IRStmt::FunctionDef { .. } => {
                    // Skip - nested functions analyzed separately
                }
            }
        }
    }

    /// Collect variable references and mutations from an expression
    fn collect_references_from_expr(&self, expr: &IRExpr, refs: &mut HashSet<String>, mutations: &mut HashSet<String>) {
        match &expr.kind {
            IRExprKind::Identifier(name) => {
                refs.insert(name.clone());
            }
            IRExprKind::Binary { left, right, .. } => {
                self.collect_references_from_expr(left, refs, mutations);
                self.collect_references_from_expr(right, refs, mutations);
            }
            IRExprKind::Unary { operand, .. } => {
                self.collect_references_from_expr(operand, refs, mutations);
            }
            IRExprKind::Call { callee, args } => {
                self.collect_references_from_expr(callee, refs, mutations);
                for arg in args {
                    self.collect_references_from_expr(arg, refs, mutations);
                }
            }
            IRExprKind::BuiltinCall { args, .. } => {
                for arg in args {
                    self.collect_references_from_expr(arg, refs, mutations);
                }
            }
            IRExprKind::Index { target, index } => {
                self.collect_references_from_expr(target, refs, mutations);
                self.collect_references_from_expr(index, refs, mutations);
            }
            IRExprKind::VectorLiteral(exprs) => {
                for e in exprs {
                    self.collect_references_from_expr(e, refs, mutations);
                }
            }
            IRExprKind::Cast { expr, .. } => {
                self.collect_references_from_expr(expr, refs, mutations);
            }
            IRExprKind::If { condition, then_branch, else_branch } => {
                // Collect from condition
                self.collect_references_from_expr(condition, refs, mutations);

                // Properly collect from then_branch using the statement collection method
                self.collect_references_and_mutations(&then_branch.stmts, refs, mutations);
                if let Some(tail) = &then_branch.tail_expr {
                    self.collect_references_from_expr(tail, refs, mutations);
                }

                // Properly collect from else_branch if present
                if let Some(else_block) = else_branch {
                    self.collect_references_and_mutations(&else_block.stmts, refs, mutations);
                    if let Some(tail) = &else_block.tail_expr {
                        self.collect_references_from_expr(tail, refs, mutations);
                    }
                }
            }
            IRExprKind::Number(_) | IRExprKind::XString(_) | IRExprKind::VarArgs | IRExprKind::Unit => {}
        }
    }
}

impl Pass for CapturedVarsPass {
    fn name(&self) -> &'static str {
        "captured_vars"
    }

    fn run(&mut self, program: &mut IRProgram) -> Result<(), PassError> {
        // Process the main function
        if let IRStmt::FunctionDef { params: _, body, metadata, .. } = &mut program.main_function {
            // Ensure metadata exists (from variable collection pass)
            let metadata = metadata.as_mut()
                .ok_or_else(|| PassError::PreconditionFailed {
                    pass_name: "captured_vars".to_string(),
                    message: "Variable collection pass must run first".to_string(),
                })?;

            // Start with empty parent variable map (main has no parent)
            let parent_vars = HashMap::new();

            // Recursively analyze nested functions
            self.analyze_nested_functions(body, &parent_vars, metadata)?;

            // IMPORTANT: After analyzing nested functions, propagate mutability to main's locals
            // If nested functions super-assign variables, mark them in main function's metadata
            let mut empty_captured_vars = Vec::new(); // Main doesn't capture anything (it's the top level)
            self.propagate_mutability_from_nested_functions(
                &body.stmts,
                &mut metadata.local_vars,
                &mut empty_captured_vars,
            )?;
        }

        Ok(())
    }
}

