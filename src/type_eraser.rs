use crate::ast::{BinaryOp, Expr, Stmt};

pub fn to_r_code(stmts: &[Stmt]) -> String {
    let mut code = String::new();
    for stmt in stmts {
        code.push_str(&stmt_to_r(stmt, 0));
        code.push('\n');
    }
    code
}

fn stmt_to_r(stmt: &Stmt, indent: usize) -> String {
    let mut s = "    ".repeat(indent);
    match stmt {
        Stmt::ExprStmt(expr) => {
            s.push_str(&expr_to_r(expr));
        }
        Stmt::VarAssign {
            name,
            x_type: _, // Erase types
            value,
            is_super_assign,
        } => {
            let arrow = if *is_super_assign { "<<-" } else { "<-" };
            s.push_str(&format!("{} {} {}", name, arrow, expr_to_r(value)));
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            s.push_str(&format!("if ({}) {{\n", expr_to_r(condition)));
            for stmt in &then_branch.stmts {
                s.push_str(&stmt_to_r(stmt, indent + 1));
                s.push('\n');
            }
            // Add tail expression if present
            if let Some(tail) = &then_branch.tail_expr {
                s.push_str(&"    ".repeat(indent + 1));
                s.push_str(&expr_to_r(tail));
                s.push('\n');
            }
            s.push_str(&"    ".repeat(indent));
            s.push('}');
            if let Some(else_b) = else_branch {
                s.push_str(" else {\n");
                for stmt in &else_b.stmts {
                    s.push_str(&stmt_to_r(stmt, indent + 1));
                    s.push('\n');
                }
                // Add tail expression if present
                if let Some(tail) = &else_b.tail_expr {
                    s.push_str(&"    ".repeat(indent + 1));
                    s.push_str(&expr_to_r(tail));
                    s.push('\n');
                }
                s.push_str(&"    ".repeat(indent));
                s.push('}');
            }
        }
        Stmt::For {
            iter_name,
            iter_vector,
            body,
        } => {
            s.push_str(&format!("for ({} in {}) {{\n", iter_name, expr_to_r(iter_vector)));
            for stmt in &body.stmts {
                s.push_str(&stmt_to_r(stmt, indent + 1));
                s.push('\n');
            }
            // Add tail expression if present (though uncommon in loops)
            if let Some(tail) = &body.tail_expr {
                s.push_str(&"    ".repeat(indent + 1));
                s.push_str(&expr_to_r(tail));
                s.push('\n');
            }
            s.push_str(&"    ".repeat(indent));
            s.push('}');
        }
        Stmt::While { condition, body } => {
            s.push_str(&format!("while ({}) {{\n", expr_to_r(condition)));
            for stmt in &body.stmts {
                s.push_str(&stmt_to_r(stmt, indent + 1));
                s.push('\n');
            }
            // Add tail expression if present (though uncommon in loops)
            if let Some(tail) = &body.tail_expr {
                s.push_str(&"    ".repeat(indent + 1));
                s.push_str(&expr_to_r(tail));
                s.push('\n');
            }
            s.push_str(&"    ".repeat(indent));
            s.push('}');
        }
        Stmt::IndexAssign {
            target,
            index,
            value,
        } => {
            s.push_str(&format!(
                "{}[{}] <- {}",
                expr_to_r(target),
                expr_to_r(index),
                expr_to_r(value)
            ));
        }
        Stmt::Return(expr_opt) => {
            s.push_str("return");
            if let Some(expr) = expr_opt {
                s.push_str(&format!("({})", expr_to_r(expr)));
            } else {
                s.push_str("()");
            }
        }
        Stmt::Block(block) => {
            s.push_str("{\n");
            for stmt in &block.stmts {
                s.push_str(&stmt_to_r(stmt, indent + 1));
                s.push('\n');
            }
            // Add tail expression if present
            if let Some(tail) = &block.tail_expr {
                s.push_str(&"    ".repeat(indent + 1));
                s.push_str(&expr_to_r(tail));
                s.push('\n');
            }
            s.push_str(&"    ".repeat(indent));
            s.push('}');
        }
    }
    s
}

fn expr_to_r(expr: &Expr) -> String {
    match expr {
        Expr::Number(n) => n.clone(),
        Expr::Identifier(id) => id.clone(),
        Expr::XString(s) => format!("\"{}\"", s), // #TODO: escape quotes?
        Expr::VarArgs => "...".to_string(),
        Expr::Logical(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        Expr::FunctionDef {
            params,
            return_type: _, // Erase types
            body,
        } => {
            let mut s = "function(".to_string();
            let param_strs: Vec<String> = params
                .iter()
                .map(|p| p.param.name.clone()) // Erase types
                .collect();
            s.push_str(&param_strs.join(", "));
            s.push_str(") {\n");
            for stmt in &body.stmts {
                s.push_str(&stmt_to_r(stmt, 1));
                s.push('\n');
            }
            // Add tail expression if present
            if let Some(tail) = &body.tail_expr {
                s.push_str(&"    ".repeat(1));
                s.push_str(&expr_to_r(tail));
                s.push('\n');
            }
            s.push('}');
            s
        }
        Expr::Unary { op, operand } => {
            use crate::ast::UnaryOp;
            let op_str = match op {
                UnaryOp::LogicalNot => "!",
                UnaryOp::Minus => "-",
                UnaryOp::Plus => "+",
            };
            format!("{}{}", op_str, expr_to_r(operand))
        }
        Expr::Binary { left, op, right } => {
            let op_str = match op {
                BinaryOp::Plus => "+",
                BinaryOp::Minus => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Mod => "%%",
                BinaryOp::Equality => "==",
                BinaryOp::NotEqual => "!=",
                BinaryOp::Less => "<",
                BinaryOp::LessEqual => "<=",
                BinaryOp::Greater => ">",
                BinaryOp::GreaterEqual => ">=",
                BinaryOp::Seq => ":",
                BinaryOp::Or => "|",
                BinaryOp::And => "&",
            };
            format!("({} {} {})", expr_to_r(left), op_str, expr_to_r(right))
        }
        Expr::Call { callee, args } => {
            use crate::ast::Argument;
            let arg_strs: Vec<String> = args.iter().map(|a| {
                match a {
                    Argument::Positional(expr) => expr_to_r(expr),
                    Argument::Named { name, value } => format!("{} = {}", name, expr_to_r(value)),
                }
            }).collect();
            format!("{}({})", expr_to_r(callee), arg_strs.join(", "))
        }
        Expr::Index { target, index } => {
            format!("{}[{}]", expr_to_r(target), expr_to_r(index))
        }
        Expr::Grouping(expr) => format!("({})", expr_to_r(expr)),
        Expr::If { condition, then_branch, else_branch } => {
            let mut s = format!("if ({}) ", expr_to_r(condition));

            // If then_branch has only a tail expression and no statements, use single expression form
            if then_branch.stmts.is_empty() && then_branch.tail_expr.is_some() {
                s.push_str(&expr_to_r(then_branch.tail_expr.as_ref().unwrap()));
            } else {
                s.push_str("{\n");
                for stmt in &then_branch.stmts {
                    s.push_str(&stmt_to_r(stmt, 1));
                    s.push('\n');
                }
                if let Some(tail) = &then_branch.tail_expr {
                    s.push_str("    ");
                    s.push_str(&expr_to_r(tail));
                    s.push('\n');
                }
                s.push('}');
            }

            if let Some(else_b) = else_branch {
                s.push_str(" else ");
                // If else_branch has only a tail expression and no statements, use single expression form
                if else_b.stmts.is_empty() && else_b.tail_expr.is_some() {
                    s.push_str(&expr_to_r(else_b.tail_expr.as_ref().unwrap()));
                } else {
                    s.push_str("{\n");
                    for stmt in &else_b.stmts {
                        s.push_str(&stmt_to_r(stmt, 1));
                        s.push('\n');
                    }
                    if let Some(tail) = &else_b.tail_expr {
                        s.push_str("    ");
                        s.push_str(&expr_to_r(tail));
                        s.push('\n');
                    }
                    s.push('}');
                }
            }
            s
        }
    }
}
