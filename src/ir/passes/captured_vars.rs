use crate::ast::{Param, ParamKind, Type};
use crate::ir::{IRExpr, IRExprKind, IRProgram, IRStmt, Pass, PassError};
use crate::ir::types::{CapturedVarInfo, FunctionMetadata};
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
    fn find_captured_variables(
        &self,
        params: &[Param],
        body: &[IRStmt],
        parent_vars: &HashMap<String, Type>,
    ) -> Result<(Vec<CapturedVarInfo>, HashSet<String>), PassError> {
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

        // Find captures: referenced but not local, and exists in parent
        let mut captured_info = Vec::new();
        let mut field_index = 1u32;  // Field 0 is function code pointer

        for var_name in &referenced_vars {
            if !local_vars.contains(var_name) {
                if let Some(var_type) = parent_vars.get(var_name) {
                    // This is a captured variable
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

        // Return captured vars for this function AND super-assigned vars for parent marking
        Ok((captured_info, super_assigned_vars))
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
                    if self.is_function_returned(then_branch, func_name) {
                        return true;
                    }
                    if let Some(else_stmts) = else_branch {
                        if self.is_function_returned(else_stmts, func_name) {
                            return true;
                        }
                    }
                }
                IRStmt::For { body, .. } | IRStmt::While { body, .. } => {
                    if self.is_function_returned(body, func_name) {
                        return true;
                    }
                }
                IRStmt::Block(body) => {
                    if self.is_function_returned(body, func_name) {
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

    /// Recursively analyze nested functions and populate closure metadata
    fn analyze_nested_functions(
        &mut self,
        stmts: &mut [IRStmt],
        parent_vars: &HashMap<String, Type>,
        parent_metadata: &mut FunctionMetadata,
    ) -> Result<(), PassError> {
        // First pass: check which functions are returned
        let mut returned_functions = std::collections::HashSet::new();
        for stmt in stmts.iter() {
            if let IRStmt::FunctionDef { name: func_name, .. } = stmt {
                if self.is_function_returned(stmts, func_name) {
                    returned_functions.insert(func_name.clone());
                }
            }
        }

        // Second pass: analyze functions
        for stmt in stmts {
            match stmt {
                IRStmt::FunctionDef { name: func_name, params, body, metadata, .. } => {
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

                    // Analyze this function's captures
                    let (captured_vars, super_assigned_vars) =
                        self.find_captured_variables(params, body, &all_parent_vars)?;

                    // Mark parent variables with need_reference if super-assigned
                    for var_name in &super_assigned_vars {
                        if let Some(local_var) = parent_metadata.local_vars
                            .iter_mut()
                            .find(|lv| lv.name == *var_name)
                        {
                            local_var.need_reference = true;
                        }
                    }

                    // Populate this function's metadata
                    func_metadata.captured_vars = captured_vars;

                    // Functions with captured variables need environment parameter
                    func_metadata.is_closure = !func_metadata.captured_vars.is_empty();

                    // Recursively analyze deeper nesting
                    self.analyze_nested_functions(body, &all_parent_vars, func_metadata)?;
                }

                // Recurse into control flow structures
                IRStmt::If { then_branch, else_branch, .. } => {
                    self.analyze_nested_functions(then_branch, parent_vars, parent_metadata)?;
                    if let Some(else_stmts) = else_branch {
                        self.analyze_nested_functions(else_stmts, parent_vars, parent_metadata)?;
                    }
                }
                IRStmt::For { body, .. } | IRStmt::While { body, .. } => {
                    self.analyze_nested_functions(body, parent_vars, parent_metadata)?;
                }
                IRStmt::Block(stmts) => {
                    self.analyze_nested_functions(stmts, parent_vars, parent_metadata)?;
                }
                _ => {}
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
                    self.collect_local_declarations(body, locals);
                }
                IRStmt::If { then_branch, else_branch, .. } => {
                    self.collect_local_declarations(then_branch, locals);
                    if let Some(else_body) = else_branch {
                        self.collect_local_declarations(else_body, locals);
                    }
                }
                IRStmt::While { body, .. } => {
                    self.collect_local_declarations(body, locals);
                }
                IRStmt::Block(stmts) => {
                    self.collect_local_declarations(stmts, locals);
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
                    self.collect_references_from_expr(value, refs);

                    // If super-assign, mark as mutated
                    if *is_super_assign {
                        mutations.insert(name.clone());
                        // Also add to refs since we're reading it
                        refs.insert(name.clone());
                    }
                }
                IRStmt::ExprStmt(expr) => {
                    self.collect_references_from_expr(expr, refs);
                }
                IRStmt::Return(expr) => {
                    self.collect_references_from_expr(expr, refs);
                }
                IRStmt::If { condition, then_branch, else_branch } => {
                    self.collect_references_from_expr(condition, refs);
                    self.collect_references_and_mutations(then_branch, refs, mutations);
                    if let Some(else_stmts) = else_branch {
                        self.collect_references_and_mutations(else_stmts, refs, mutations);
                    }
                }
                IRStmt::For { iter_expr, body, .. } => {
                    self.collect_references_from_expr(iter_expr, refs);
                    self.collect_references_and_mutations(body, refs, mutations);
                }
                IRStmt::While { condition, body } => {
                    self.collect_references_from_expr(condition, refs);
                    self.collect_references_and_mutations(body, refs, mutations);
                }
                IRStmt::IndexAssign { target, index, value } => {
                    self.collect_references_from_expr(target, refs);
                    self.collect_references_from_expr(index, refs);
                    self.collect_references_from_expr(value, refs);
                }
                IRStmt::Block(stmts) => {
                    self.collect_references_and_mutations(stmts, refs, mutations);
                }
                IRStmt::FunctionDef { .. } => {
                    // Skip - nested functions analyzed separately
                }
            }
        }
    }

    /// Collect variable references from an expression
    fn collect_references_from_expr(&self, expr: &IRExpr, refs: &mut HashSet<String>) {
        match &expr.kind {
            IRExprKind::Identifier(name) => {
                refs.insert(name.clone());
            }
            IRExprKind::Binary { left, right, .. } => {
                self.collect_references_from_expr(left, refs);
                self.collect_references_from_expr(right, refs);
            }
            IRExprKind::Call { callee, args } => {
                self.collect_references_from_expr(callee, refs);
                for arg in args {
                    self.collect_references_from_expr(arg, refs);
                }
            }
            IRExprKind::BuiltinCall { args, .. } => {
                for arg in args {
                    self.collect_references_from_expr(arg, refs);
                }
            }
            IRExprKind::Index { target, index } => {
                self.collect_references_from_expr(target, refs);
                self.collect_references_from_expr(index, refs);
            }
            IRExprKind::VectorLiteral(exprs) => {
                for e in exprs {
                    self.collect_references_from_expr(e, refs);
                }
            }
            IRExprKind::ClosureCreate { captured_vars, .. } => {
                // Add captured variables to references
                for var_name in captured_vars {
                    refs.insert(var_name.clone());
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
        }

        Ok(())
    }
}

