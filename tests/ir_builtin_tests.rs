use rty_compiler::ast::{BinaryOp, Expr, Param, ParamKind, Stmt, Type};
use rty_compiler::ir::{BuiltinKind, IRExprKind, IRProgram, IRStmt, TypeError, TypeResolver, IR};
use rty_compiler::lexer::{Lexer, Token};
use rty_compiler::parser::Parser;

fn lex_tokens(src: &str) -> Vec<Token> {
    let lexer = Lexer::new();
    lexer.lex(&src.to_string())
}

fn parse_program(src: &str) -> Vec<Stmt> {
    let mut tokens = lex_tokens(src);
    if *tokens.last().unwrap() != Token::EOF {
        tokens.push(Token::EOF);
    }
    let mut parser = Parser::new(tokens);
    parser.parse_program().expect("parse_program failed")
}

fn lower(src: &str) -> Result<IRProgram, TypeError> {
    let ast = parse_program(src);
    let mut resolver = TypeResolver::new();
    IR::from_ast(ast, &mut resolver)
}

#[test]
fn lowers_builtin_c_numeric() {
    let ir = lower("a <- c(1, 2, 3)").expect("lower failed");
    match &ir.statements[0] {
        IRStmt::VarAssign { name, value, .. } => {
            assert_eq!(name, "a");
            match &value.kind {
                IRExprKind::VectorLiteral(args) => {
                    assert_eq!(args.len(), 3);
                    assert!(args.iter().all(|arg| arg.ty == Type::Int));
                }
                other => panic!("expected vector literal, got {:?}", other),
            }
        }
        other => panic!("expected VarAssign, got {:?}", other),
    }
}

#[test]
fn lowers_builtin_list_of_vectors() {
    let program = vec![Stmt::VarAssign {
        name: "a".into(),
        x_type: None,
        value: Expr::Call {
            callee: Box::new(Expr::Identifier("list".into())),
            args: vec![
                Expr::Call {
                    callee: Box::new(Expr::Identifier("c".into())),
                    args: vec![Expr::Number("1".into()), Expr::Number("2".into())],
                },
                Expr::Call {
                    callee: Box::new(Expr::Identifier("c".into())),
                    args: vec![Expr::Number("3".into()), Expr::Number("4".into())],
                },
            ],
        },
    }];
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(program, &mut resolver).expect("lower failed");
    match &ir.statements[0] {
        IRStmt::VarAssign { value, .. } => match &value.kind {
            IRExprKind::BuiltinCall { builtin, args } => {
                assert_eq!(*builtin, BuiltinKind::List);
                assert_eq!(args.len(), 2);
                assert!(args.iter().all(|arg| arg.ty == Type::Vector(Box::new(Type::Int))));
            }
            other => panic!("expected list builtin, got {:?}", other),
        },
        other => panic!("expected VarAssign with builtin list, got {:?}", other),
    }
}

#[test]
fn builtin_list_rejects_mixed_types() {
    let program = vec![Stmt::VarAssign {
        name: "a".into(),
        x_type: None,
        value: Expr::Call {
            callee: Box::new(Expr::Identifier("list".into())),
            args: vec![Expr::Number("1".into()), Expr::XString("two".into())],
        },
    }];
    let mut resolver = TypeResolver::new();
    let err = IR::from_ast(program, &mut resolver).expect_err("expected type error");
    match err {
        TypeError::TypeMismatch { context, .. } => {
            assert!(
                context.contains("list"),
                "expected context to mention list, got {context}"
            );
        }
        other => panic!("expected TypeMismatch, got {:?}", other),
    }
}

#[test]
fn builtin_c_varargs_forwarding() {
    let program = vec![Stmt::VarAssign {
        name: "my".into(),
        x_type: None,
        value: Expr::FunctionDef {
            params: vec![Param {
                name: "...".into(),
                kind: ParamKind::VarArgs,
            }],
            return_type: Some(Type::Vector(Type::Any.into())),
            body: vec![Stmt::Return(Some(Expr::Call {
                callee: Box::new(Expr::Identifier("c".into())),
                args: vec![Expr::VarArgs],
            }))],
        },
    }];
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(program, &mut resolver).expect("lower failed");
    match &ir.statements[0] {
        IRStmt::FunctionDef { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                IRStmt::Return(expr) => match &expr.kind {
                    IRExprKind::VectorLiteral(args) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(args[0].ty, Type::VarArgs);
                        assert!(matches!(args[0].kind, IRExprKind::VarArgs));
                    }
                    other => panic!("expected vector literal for c(...) call, got {:?}", other),
                },
                other => panic!("expected return, got {:?}", other),
            }
        }
        other => panic!("expected function def, got {:?}", other),
    }
}


#[test]
fn lower_simple_if_without_else() {
    let prog = parse_program("if (x > 5) { y <- 10 }");
    let mut resolver = TypeResolver::new();
    // Need to declare x first since it's used in condition
    resolver.vars.insert("x".into(), Type::Int);

    let ir = IR::from_ast(prog, &mut resolver).expect("lower failed");
    assert_eq!(ir.statements.len(),1);

    match &ir.statements[0] {
        IRStmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert_eq!(condition.ty, Type::Bool);
            assert_eq!(then_branch.len(), 1);
            match &then_branch[0] {
                IRStmt::VarAssign { name, ty, .. } => {
                    assert_eq!(name, "y");
                    assert_eq!(ty, &Type::Int);
                }
                _ => panic!("expected var assign in then branch"),
            }
            assert!(else_branch.is_none());
        }
        _ => panic!("expected if statement"),
    }
}

#[test]
fn lower_if_with_else() {
    let prog = parse_program("if (a == b) { x <- 1 } else { x <- 2 }");
    let mut resolver = TypeResolver::new();
    // Declare a and b as integers
    resolver.vars.insert("a".into(), Type::Int);
    resolver.vars.insert("b".into(), Type::Int);

    let ir = IR::from_ast(prog, &mut resolver).expect("lower failed");
    assert_eq!(ir.statements.len(),1);

    match &ir.statements[0] {
        IRStmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert_eq!(condition.ty, Type::Bool);
            assert_eq!(then_branch.len(), 1);
            match &then_branch[0] {
                IRStmt::VarAssign { name, ty, value, .. } => {
                    assert_eq!(name, "x");
                    assert_eq!(ty, &Type::Int);
                    assert_eq!(value.ty, Type::Int);
                }
                _ => panic!("expected var assign in then branch"),
            }

            assert!(else_branch.is_some());
            let else_stmts = else_branch.as_ref().unwrap();
            assert_eq!(else_stmts.len(), 1);
            match &else_stmts[0] {
                IRStmt::VarAssign { name, ty, value, .. } => {
                    assert_eq!(name, "x");
                    assert_eq!(ty, &Type::Int);
                    assert_eq!(value.ty, Type::Int);
                }
                _ => panic!("expected var assign in else branch"),
            }
        }
        _ => panic!("expected if statement"),
    }
}


#[test]
fn lower_for_loop_with_range() {
    let prog = parse_program("sum <- 0\nfor (i in 1:10) { sum <- sum + i }");
    let mut resolver = TypeResolver::new();

    let ir = IR::from_ast(prog, &mut resolver).expect("lower failed");
    assert_eq!(ir.statements.len(),2);

    // First statement should be sum initialization
    match &ir.statements[0] {
        IRStmt::VarAssign { name, ty, .. } => {
            assert_eq!(name, "sum");
            assert_eq!(ty, &Type::Int);
        }
        _ => panic!("expected var assign"),
    }

    // Second statement should be the for loop
    match &ir.statements[1] {
        IRStmt::For {
            iter_var: iter_name,
            iter_expr,
            body,
        } => {
            assert_eq!(iter_name.0, "i");
            assert_eq!(iter_name.1, Type::Int);
            assert_eq!(iter_expr.ty, Type::Vector(Box::new(Type::Int)));

            // Check that the range expression is properly typed
            match &iter_expr.kind {
                IRExprKind::Binary { left, op, right } => {
                    assert_eq!(*op, BinaryOp::Range);
                    assert_eq!(left.ty, Type::Int);
                    assert_eq!(right.ty, Type::Int);
                }
                _ => panic!("expected binary range expression"),
            }

            // Check the loop body
            assert_eq!(body.len(), 1);
            match &body[0] {
                IRStmt::VarAssign { name, ty, value } => {
                    assert_eq!(name, "sum");
                    assert_eq!(ty, &Type::Int);
                    assert_eq!(value.ty, Type::Int);
                }
                _ => panic!("expected var assign in loop body"),
            }
        }
        _ => panic!("expected for statement"),
    }
}

#[test]
fn lower_for_loop_with_vector() {
    let prog = parse_program("my_vec <- c(1, 2, 3)\nfor (x in my_vec) { print(x) }");
    let mut resolver = TypeResolver::new();

    let ir = IR::from_ast(prog, &mut resolver).expect("lower failed");
    assert_eq!(ir.statements.len(),2);

    // First statement should be vector initialization
    match &ir.statements[0] {
        IRStmt::VarAssign { name, ty, .. } => {
            assert_eq!(name, "my_vec");
            assert_eq!(ty, &Type::Vector(Box::new(Type::Int)));
        }
        _ => panic!("expected var assign"),
    }

    // Second statement should be the for loop
    match &ir.statements[1] {
        IRStmt::For {
            iter_var: iter_name,
            iter_expr,
            body,
        } => {
            assert_eq!(iter_name.0, "x");
            assert_eq!(iter_name.1, Type::Int);
            assert_eq!(iter_expr.ty, Type::Vector(Box::new(Type::Int)));

            // Check that we're iterating over the identifier
            match &iter_expr.kind {
                IRExprKind::Identifier(name) => {
                    assert_eq!(name, "my_vec");
                }
                _ => panic!("expected identifier expression"),
            }

            // Check the loop body contains expression statement with builtin call
            assert_eq!(body.len(), 1);
            match &body[0] {
                IRStmt::ExprStmt(expr) => {
                    match &expr.kind {
                        IRExprKind::BuiltinCall { builtin, args } => {
                            assert_eq!(*builtin, BuiltinKind::Print);
                            assert_eq!(args.len(), 1);
                        }
                        _ => panic!("expected builtin call expression"),
                    }
                }
                _ => panic!("expected expr stmt in loop body"),
            }
        }
        _ => panic!("expected for statement"),
    }
}

#[test]
fn lower_vector_index_read() {
    let prog = parse_program("v <- c(10, 20, 30)\nx <- v[2]");
    let mut resolver = TypeResolver::new();

    let ir = IR::from_ast(prog, &mut resolver).expect("lower failed");
    assert_eq!(ir.statements.len(),2);

    // Second statement should be the index read
    match &ir.statements[1] {
        IRStmt::VarAssign { name, ty, value } => {
            assert_eq!(name, "x");
            assert_eq!(ty, &Type::Int); // Element type should be extracted

            // Check the index expression
            match &value.kind {
                IRExprKind::Index { target, index } => {
                    assert_eq!(target.ty, Type::Vector(Box::new(Type::Int)));
                    assert_eq!(index.ty, Type::Int);
                    assert_eq!(value.ty, Type::Int); // Result type is element type
                }
                other => panic!("expected index expression, got {:?}", other),
            }
        }
        other => panic!("expected var assign, got {:?}", other),
    }
}

#[test]
fn lower_vector_index_assign() {
    let prog = parse_program("v <- c(1, 2, 3)\nv[2] <- 99");
    let mut resolver = TypeResolver::new();

    let ir = IR::from_ast(prog, &mut resolver).expect("lower failed");
    assert_eq!(ir.statements.len(),2);

    // Second statement should be the index assignment
    match &ir.statements[1] {
        IRStmt::IndexAssign { target, index, value } => {
            assert_eq!(target.ty, Type::Vector(Box::new(Type::Int)));
            assert_eq!(index.ty, Type::Int);
            assert_eq!(value.ty, Type::Int);
        }
        other => panic!("expected index assign, got {:?}", other),
    }
}

#[test]
fn lower_vector_index_rejects_non_vector() {
    let prog = parse_program("x <- 42\ny <- x[1]");
    let mut resolver = TypeResolver::new();

    let err = IR::from_ast(prog, &mut resolver).expect_err("expected type error");
    match err {
        TypeError::InvalidIndexTarget { target_type, .. } => {
            assert_eq!(target_type, Type::Int);
        }
        other => panic!("expected InvalidIndexTarget, got {:?}", other),
    }
}

#[test]
fn lower_while_loop() {
    let prog = parse_program("x <- 0\nwhile (x < 10) { x <- x + 1 }");
    let mut resolver = TypeResolver::new();

    let ir = IR::from_ast(prog, &mut resolver).expect("lower failed");
    assert_eq!(ir.statements.len(), 2);

    // First statement should be x initialization
    match &ir.statements[0] {
        IRStmt::VarAssign { name, ty, .. } => {
            assert_eq!(name, "x");
            assert_eq!(ty, &Type::Int);
        }
        _ => panic!("expected var assign"),
    }

    // Second statement should be the while loop
    match &ir.statements[1] {
        IRStmt::While { condition, body } => {
            // Check condition is typed as Bool
            assert_eq!(condition.ty, Type::Bool);

            // Check the condition is a less-than comparison
            match &condition.kind {
                IRExprKind::Binary { left, op, right } => {
                    assert_eq!(*op, BinaryOp::Less);
                    assert_eq!(left.ty, Type::Int);
                    assert_eq!(right.ty, Type::Int);
                }
                _ => panic!("expected binary comparison expression"),
            }

            // Check the loop body
            assert_eq!(body.len(), 1);
            match &body[0] {
                IRStmt::VarAssign { name, ty, value } => {
                    assert_eq!(name, "x");
                    assert_eq!(ty, &Type::Int);
                    assert_eq!(value.ty, Type::Int);
                }
                _ => panic!("expected var assign in loop body"),
            }
        }
        _ => panic!("expected while statement"),
    }
}

