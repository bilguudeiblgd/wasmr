use crate::ast::Type;
use crate::ir::{BuiltinKind, IRExpr, IRExprKind, IRStmt as Stmt};

use super::WasmGenerator;

impl WasmGenerator {
    /// Collect all variable declarations from statements
    /// #TODO: move variable collection to IR
    pub(crate) fn collect_vars(&self, stmts: &[Stmt]) -> Vec<(String, Type)> {
        let mut vars = Vec::new();
        for stmt in stmts {
            self.collect_vars_from_stmt(stmt, &mut vars);
        }
        vars
    }

    fn collect_vars_from_stmt(&self, stmt: &Stmt, vars: &mut Vec<(String, Type)>) {
        match stmt {
            Stmt::VarAssign { name, ty, .. } => {
                let ty = ty.clone();
                if !vars.iter().any(|(n, _)| n == name) {
                    vars.push((name.clone(), ty));
                }
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.collect_vars_from_stmt(s, vars);
                }
            }
            Stmt::For { iter_var, iter_expr, body, .. } => {
                let name = iter_var.0.clone();
                let ty = iter_var.1.clone();
                if !vars.iter().any(|(n, _)| *n == name) {
                    vars.push((name.clone(), ty.clone()));
                }

                // Add system iterator variable
                if !vars.iter().any(|(n, _)| *n == "system_iter") {
                    vars.push(("system_iter".to_string(), Type::Int));
                }

                // Add temporary vector variable to hold the iterable
                let vector_local_name = format!("__for_vec_{}", iter_var.0);
                if !vars.iter().any(|(n, _)| n == &vector_local_name) {
                    vars.push((vector_local_name, Type::Vector(Box::new(ty))));
                }

                // Add length variable
                if !vars.iter().any(|(n, _)| *n == "__for_len") {
                    vars.push(("__for_len".to_string(), Type::Int));
                }

                // Collect vars from loop body
                for s in body {
                    self.collect_vars_from_stmt(s, vars);
                }
            }
            Stmt::ExprStmt(expr) => {
                self.collect_print_temps_from_expr(expr, vars);
            }

            _ => {}
        }
    }

    fn collect_print_temps_from_expr(&self, expr: &IRExpr, vars: &mut Vec<(String, Type)>) {
        match &expr.kind {
            IRExprKind::BuiltinCall { builtin, args } => {
                if matches!(builtin, BuiltinKind::Print) {
                    // Add temporary variables for print function
                    let temps = vec![
                        ("__print_num", Type::Int),
                        ("__print_is_negative", Type::Int),
                        ("__print_write_pos", Type::Int),
                        ("__print_digit_count", Type::Int),
                    ];
                    for (name, ty) in temps {
                        if !vars.iter().any(|(n, _)| n == name) {
                            vars.push((name.to_string(), ty));
                        }
                    }
                }
                // Recursively check arguments
                for arg in args {
                    self.collect_print_temps_from_expr(arg, vars);
                }
            }
            IRExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_print_temps_from_expr(arg, vars);
                }
            }
            IRExprKind::Binary { left, right, .. } => {
                self.collect_print_temps_from_expr(left, vars);
                self.collect_print_temps_from_expr(right, vars);
            }
            _ => {}
        }
    }
}
