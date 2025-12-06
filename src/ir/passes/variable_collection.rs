/// Variable Collection Pass
///
/// Collects all variables in functions and assigns WASM local indices.
/// This includes parameters, user-declared variables, and compiler-generated temporaries.

use std::collections::HashSet;

use crate::ast::{Param, ParamKind, Type};
use crate::ir::{
    BuiltinKind, FunctionMetadata, IRExpr, IRExprKind, IRProgram, IRStmt, LocalVarInfo,
    VarArgsInfo, VarOrigin,
};

use super::{Pass, PassError};

/// Pass that collects variables and assigns local indices
pub struct VariableCollectionPass;

impl VariableCollectionPass {
    pub fn new() -> Self {
        Self
    }

    /// Collect all variables for a function and create metadata
    fn collect_function_vars(
        &self,
        params: &[Param],
        body: &[IRStmt],
    ) -> FunctionMetadata {
        let mut local_vars = Vec::new();
        let mut varargs_param = None;

        // First, add parameters (they occupy indices 0..param_count)
        for (idx, param) in params.iter().enumerate() {
            let origin = VarOrigin::Parameter;
            let ty = match &param.kind {
                ParamKind::Normal(ty) => ty.clone(),
                ParamKind::VarArgs => {
                    varargs_param = Some(VarArgsInfo {
                        param_name: param.name.clone(),
                        local_index: idx as u32,
                    });
                    Type::Vector(Box::new(Type::Any)) // Varargs is an array
                }
            };

            local_vars.push(LocalVarInfo {
                name: param.name.clone(),
                ty,
                index: idx as u32,
                origin,
            });
        }

        let param_count = params.len() as u32;
        let mut next_index = param_count;
        let mut seen = HashSet::new();

        // Track params as already seen
        for param in params {
            seen.insert(param.name.clone());
        }

        // Collect user variables and compiler temporaries
        self.collect_vars_from_stmts(body, &mut local_vars, &mut next_index, &mut seen);

        FunctionMetadata {
            local_vars: local_vars.clone(),
            varargs_param,
            local_count: local_vars.len() as u32,
        }
    }

    /// Recursively collect variables from statements
    fn collect_vars_from_stmts(
        &self,
        stmts: &[IRStmt],
        vars: &mut Vec<LocalVarInfo>,
        next_index: &mut u32,
        seen: &mut HashSet<String>,
    ) {
        for stmt in stmts {
            self.collect_vars_from_stmt(stmt, vars, next_index, seen);
        }
    }

    /// Collect variables from a single statement
    fn collect_vars_from_stmt(
        &self,
        stmt: &IRStmt,
        vars: &mut Vec<LocalVarInfo>,
        next_index: &mut u32,
        seen: &mut HashSet<String>,
    ) {
        match stmt {
            IRStmt::VarAssign { name, ty, value: _ } => {
                // Add user variable if not seen
                if !seen.contains(name) {
                    vars.push(LocalVarInfo {
                        name: name.clone(),
                        ty: ty.clone(),
                        index: *next_index,
                        origin: VarOrigin::UserDeclared,
                    });
                    seen.insert(name.clone());
                    *next_index += 1;
                }
            }

            IRStmt::For {
                iter_var,
                iter_expr: _,
                body,
            } => {
                let (iter_name, iter_ty) = iter_var;

                // Add iterator variable
                if !seen.contains(iter_name) {
                    vars.push(LocalVarInfo {
                        name: iter_name.clone(),
                        ty: iter_ty.clone(),
                        index: *next_index,
                        origin: VarOrigin::LoopIterator,
                    });
                    seen.insert(iter_name.clone());
                    *next_index += 1;
                }

                // Add system iterator index
                let system_iter_name = "system_iter".to_string();
                if !seen.contains(&system_iter_name) {
                    vars.push(LocalVarInfo {
                        name: system_iter_name.clone(),
                        ty: Type::Int,
                        index: *next_index,
                        origin: VarOrigin::LoopIndex,
                    });
                    seen.insert(system_iter_name);
                    *next_index += 1;
                }

                // Add vector storage for iterable
                let vec_name = format!("__for_vec_{}", iter_name);
                if !seen.contains(&vec_name) {
                    vars.push(LocalVarInfo {
                        name: vec_name.clone(),
                        ty: Type::Vector(Box::new(iter_ty.clone())),
                        index: *next_index,
                        origin: VarOrigin::LoopVector,
                    });
                    seen.insert(vec_name);
                    *next_index += 1;
                }

                // Add length variable
                let len_name = "__for_len".to_string();
                if !seen.contains(&len_name) {
                    vars.push(LocalVarInfo {
                        name: len_name.clone(),
                        ty: Type::Int,
                        index: *next_index,
                        origin: VarOrigin::LoopLength,
                    });
                    seen.insert(len_name);
                    *next_index += 1;
                }

                // Recursively collect from loop body
                self.collect_vars_from_stmts(body, vars, next_index, seen);
            }

            IRStmt::Block(stmts) => {
                self.collect_vars_from_stmts(stmts, vars, next_index, seen);
            }

            IRStmt::If {
                condition: _,
                then_branch,
                else_branch,
            } => {
                self.collect_vars_from_stmts(then_branch, vars, next_index, seen);
                if let Some(else_stmts) = else_branch {
                    self.collect_vars_from_stmts(else_stmts, vars, next_index, seen);
                }
            }

            IRStmt::ExprStmt(_expr) => {
                // No special handling needed
            }

            IRStmt::Return(_expr) => {
                // No special handling needed
            }

            IRStmt::IndexAssign {
                target: _,
                index: _,
                value: _,
            } => {
                // No special handling needed
            }

            IRStmt::FunctionDef { .. } => {
                // Nested functions not supported, skip
            }

            IRStmt::While { condition: _, body } => {
                self.collect_vars_from_stmts(body, vars, next_index, seen);
            }
        }
    }

}

impl Pass for VariableCollectionPass {
    fn name(&self) -> &'static str {
        "variable_collection"
    }

    fn run(&mut self, program: &mut IRProgram) -> Result<(), PassError> {
        // Iterate through all statements and collect variables for each function
        for stmt in &mut program.statements {
            if let IRStmt::FunctionDef {
                params,
                body,
                metadata,
                ..
            } = stmt
            {
                let func_metadata = self.collect_function_vars(params, body);
                *metadata = Some(Box::new(func_metadata));
            }
        }

        Ok(())
    }
}
