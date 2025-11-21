use rty_compiler::ast::{BinaryOp, Expr, Param, ParamKind, Stmt, Type};
use rty_compiler::lexer::{Lexer, Token};
use rty_compiler::parser::Parser;

fn lex_tokens(s: &str) -> Vec<Token> {
    let lexer = Lexer::new();
    lexer.lex(&s.to_string())
}

fn parse_expr(s: &str) -> Expr {
    let mut tokens = lex_tokens(s);
    if *tokens.last().unwrap() != Token::EOF {
        tokens.push(Token::EOF);
    }
    let mut p = Parser::new(tokens);
    p.parse_expression().expect("parse_expression failed")
}

fn parse_stmt(s: &str) -> Stmt {
    let mut tokens = lex_tokens(s);
    if *tokens.last().unwrap() != Token::EOF {
        tokens.push(Token::EOF);
    }
    let mut p = Parser::new(tokens);
    p.parse_statement().expect("parse_statement failed")
}

fn parse_program(s: &str) -> Vec<Stmt> {
    let mut tokens = lex_tokens(s);
    if *tokens.last().unwrap() != Token::EOF {
        tokens.push(Token::EOF);
    }
    let mut p = Parser::new(tokens);
    p.parse_program().expect("parse_program failed")
}

#[test]
fn parse_expression_arithmetic_and_precedence() {
    // 1 + 2 * 3 => 1 + (2 * 3)
    let expr = parse_expr("1 + 2 * 3");
    let expected = Expr::Binary {
        left: Box::new(Expr::Number("1".into())),
        op: BinaryOp::Plus,
        right: Box::new(Expr::Binary {
            left: Box::new(Expr::Number("2".into())),
            op: BinaryOp::Mul,
            right: Box::new(Expr::Number("3".into())),
        }),
    };
    assert_eq!(expr, expected);
}

#[test]
fn parse_function_call() {
    let expr = parse_expr("f(1, 2)");
    let expected = Expr::Call {
        callee: Box::new(Expr::Identifier("f".into())),
        args: vec![Expr::Number("1".into()), Expr::Number("2".into())],
    };
    assert_eq!(expr, expected);
}

#[test]
fn parse_hello_world() {
    let expr = parse_expr("print(\"Hello world!\")");
    let expected = Expr::Call {
        callee: Box::new(Expr::Identifier("print".into())),
        args: vec![Expr::XString("Hello world!".into())],
    };
    assert_eq!(expr, expected);
}

#[test]
fn parse_expression_comparison_and_range() {
    // 1:3 < 10
    let expr = parse_expr("1:3 < 10");
    let expected = Expr::Binary {
        left: Box::new(Expr::Binary {
            left: Box::new(Expr::Number("1".into())),
            op: BinaryOp::Range,
            right: Box::new(Expr::Number("3".into())),
        }),
        op: BinaryOp::Less,
        right: Box::new(Expr::Number("10".into())),
    };
    assert_eq!(expr, expected);
}

#[test]
fn parse_call_and_grouping() {
    let expr = parse_expr("f(1, x + 2)");
    let expected = Expr::Call {
        callee: Box::new(Expr::Identifier("f".into())),
        args: vec![
            Expr::Number("1".into()),
            Expr::Binary {
                left: Box::new(Expr::Identifier("x".into())),
                op: BinaryOp::Plus,
                right: Box::new(Expr::Number("2".into())),
            },
        ],
    };
    assert_eq!(expr, expected);
}

#[test]
fn parse_assignment_statement() {
    let stmt = parse_stmt("x: int <- 1:3");
    let expected = Stmt::VarAssign {
        name: "x".into(),
        x_type: Some(Type::Int),
        value: Expr::Binary {
            left: Box::new(Expr::Number("1".into())),
            op: BinaryOp::Range,
            right: Box::new(Expr::Number("3".into())),
        },
    };
    assert_eq!(stmt, expected);
}

#[test]
fn parse_return_with_and_without_parens() {
    let stmt1 = parse_stmt("return(x + x)");
    let expected1 = Stmt::Return(Some(Expr::Binary {
        left: Box::new(Expr::Identifier("x".into())),
        op: BinaryOp::Plus,
        right: Box::new(Expr::Identifier("x".into())),
    }));
    assert_eq!(stmt1, expected1);

    let stmt2 = parse_stmt("return x");
    let expected2 = Stmt::Return(Some(Expr::Identifier("x".into())));
    assert_eq!(stmt2, expected2);
}

#[test]
fn parse_block_statement() {
    // No separators required based on our parser rules.
    let stmt = parse_stmt("{ x: int <- 1; y: int <- 2 }");
    match stmt {
        Stmt::Block(body) => {
            assert_eq!(body.len(), 2);
            assert_eq!(
                body[0],
                Stmt::VarAssign {
                    name: "x".into(),
                    x_type: Some(Type::Int),
                    value: Expr::Number("1".into())
                }
            );
            assert_eq!(
                body[1],
                Stmt::VarAssign {
                    name: "y".into(),
                    x_type: Some(Type::Int),
                    value: Expr::Number("2".into())
                }
            );
        }
        _ => panic!("expected block"),
    }
}

#[test]
fn parse_function_definition_full() {
    let prog = parse_program("f <- function(x: int): int { return(x + x) }");
    assert_eq!(prog.len(), 1);
    match &prog[0] {
        Stmt::VarAssign {
            name,
            x_type,
            value,
        } => {
            assert_eq!(name, "f");
            assert!(x_type.is_none());
            match value {
                Expr::FunctionDef {
                    params,
                    return_type,
                    body,
                } => {
                    assert_eq!(
                        params,
                        &vec![Param {
                            name: "x".into(),
                            kind: ParamKind::Normal(Type::Int)
                        }]
                    );
                    assert_eq!(return_type, &Some(Type::Int));
                    assert_eq!(body.len(), 1);
                    assert_eq!(
                        body[0],
                        Stmt::Return(Some(Expr::Binary {
                            left: Box::new(Expr::Identifier("x".into())),
                            op: BinaryOp::Plus,
                            right: Box::new(Expr::Identifier("x".into())),
                        }))
                    );
                }
                _ => panic!("expected function expression"),
            }
        }
        _ => panic!("expected var assign of function expr"),
    }
}

#[test]
fn parse_program_multiple_statements() {
    let prog = parse_program("a: int <- 1\nb: int <- 2");
    assert_eq!(prog.len(), 2);
    assert_eq!(
        prog[0],
        Stmt::VarAssign {
            name: "a".into(),
            x_type: Some(Type::Int),
            value: Expr::Number("1".into())
        }
    );
    assert_eq!(
        prog[1],
        Stmt::VarAssign {
            name: "b".into(),
            x_type: Some(Type::Int),
            value: Expr::Number("2".into())
        }
    );
}

#[test]
fn parse_vector() {
    let prog = parse_program("a <- c(1,2,3)");
    assert_eq!(prog.len(), 1);
    assert_eq!(
        prog[0],
        Stmt::VarAssign {
            name: "a".into(),
            x_type: None,
            value: Expr::Call {
                callee: Box::new(Expr::Identifier("c".into())),
                args: vec![
                    Expr::Number("1".into()),
                    Expr::Number("2".into()),
                    Expr::Number("3".into())
                ]
            }
        }
    );
}

#[test]
fn parse_variable_arguments() {
    let prog = parse_program("my_func <- function(...) { return(c(...)) }");
    assert_eq!(prog.len(), 1);
    match &prog[0] {
        Stmt::VarAssign {
            name,
            x_type,
            value,
        } => {
            assert_eq!(name, "my_func");
            assert!(x_type.is_none());
            match value {
                Expr::FunctionDef {
                    params,
                    return_type,
                    body,
                } => {
                    assert_eq!(
                        params,
                        &vec![Param {
                            name: "...".into(),
                            kind: ParamKind::VarArgs
                        }]
                    );
                    assert_eq!(return_type, &None);
                    assert_eq!(
                        body,
                        &vec![Stmt::Return(Some(Expr::Call {
                            callee: Box::new(Expr::Identifier("c".into())),
                            args: vec![Expr::VarArgs]
                        }))]
                    );
                }
                _ => panic!("expected function expression"),
            }
        }
        _ => panic!("expected var assign of function expr"),
    }
}

#[test]
fn parse_complex_function_with_vectors() {
    let prog = parse_program(
        "add_vec <- function(a: vector<int>, b: vector<int>): vector<int> { \
            result: vector<int> <- a + b\n\
            return result \
        }"
    );
    assert_eq!(prog.len(), 1);
    match &prog[0] {
        Stmt::VarAssign {
            name,
            x_type,
            value,
        } => {
            assert_eq!(name, "add_vec");
            assert!(x_type.is_none());
            match value {
                Expr::FunctionDef {
                    params,
                    return_type,
                    body,
                } => {
                    assert_eq!(
                        params,
                        &vec![
                            Param {
                                name: "a".into(),
                                kind: ParamKind::Normal(Type::Vector(Box::new(Type::Int)))
                            },
                            Param {
                                name: "b".into(),
                                kind: ParamKind::Normal(Type::Vector(Box::new(Type::Int)))
                            }
                        ]
                    );
                    assert_eq!(
                        return_type,
                        &Some(Type::Vector(Box::new(Type::Int)))
                    );
                    assert_eq!(body.len(), 2);
                    // Check the first statement is a variable assignment
                    match &body[0] {
                        Stmt::VarAssign { name, x_type, .. } => {
                            assert_eq!(name, "result");
                            assert_eq!(
                                x_type,
                                &Some(Type::Vector(Box::new(Type::Int)))
                            );
                        }
                        _ => panic!("expected var assign"),
                    }
                }
                _ => panic!("expected function expression"),
            }
        }
        _ => panic!("expected var assign of function expr"),
    }
}

#[test]
fn parse_if_statement() {
    let prog = parse_program(
        " x <- 15
        result <- 0

        if (x > 20) {
          result <- 1
        } else if (x == 20) {
          result <- 2
        } else {
          result <- 3
        }

        result"
    );  
    assert_eq!(prog.len(), 4);
    assert_eq!(prog[0], Stmt::VarAssign { name: "x".into(), x_type: None, value: Expr::Number("15".into()) });
    assert_eq!(prog[1], Stmt::VarAssign { name: "result".into(), x_type: None, value: Expr::Number("0".into()) });
    assert_eq!(prog[2], Stmt::If { condition: Expr::Binary { left: Box::new(Expr::Identifier("x".into())),op: BinaryOp::Greater, right: Box::new(Expr::Number("20".into())) }, 
        then_branch: vec![Stmt::VarAssign { name: "result".into(), x_type: None, value: Expr::Number("1".into()) }], 
        else_branch: Some(
            vec![Stmt::If { condition: Expr::Binary { left: Box::new(Expr::Identifier("x".into())), op: BinaryOp::Equality, right: Box::new(Expr::Number("20".into())) },
            then_branch: vec![Stmt::VarAssign { name: "result".into(), x_type: None, value: Expr::Number("2".into()) }],
            else_branch: Some(vec![Stmt::VarAssign { name: "result".into(), x_type: None, value: Expr::Number("3".into()) }])
            }]
        ) });
    assert_eq!(prog[3], Stmt::ExprStmt(Expr::Identifier("result".into())));
    
}


#[test]
fn parse_simple_if_without_else() {
    let stmt = parse_stmt("if (x > 5) { y <- 10 }");
    match stmt {
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert_eq!(
                condition,
                Expr::Binary {
                    left: Box::new(Expr::Identifier("x".into())),
                    op: BinaryOp::Greater,
                    right: Box::new(Expr::Number("5".into())),
                }
            );
            assert_eq!(then_branch.len(), 1);
            assert_eq!(
                then_branch[0],
                Stmt::VarAssign {
                    name: "y".into(),
                    x_type: None,
                    value: Expr::Number("10".into())
                }
            );
            assert!(else_branch.is_none());
        }
        _ => panic!("expected if statement"),
    }
}

#[test]
fn parse_if_with_else() {
    let stmt = parse_stmt("if (a == b) { x <- 1 } else { x <- 2 }");
    match stmt {
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert_eq!(
                condition,
                Expr::Binary {
                    left: Box::new(Expr::Identifier("a".into())),
                    op: BinaryOp::Equality,
                    right: Box::new(Expr::Identifier("b".into())),
                }
            );
            assert_eq!(then_branch.len(), 1);
            assert_eq!(
                then_branch[0],
                Stmt::VarAssign {
                    name: "x".into(),
                    x_type: None,
                    value: Expr::Number("1".into())
                }
            );
            assert!(else_branch.is_some());
            let else_stmts = else_branch.unwrap();
            assert_eq!(else_stmts.len(), 1);
            assert_eq!(
                else_stmts[0],
                Stmt::VarAssign {
                    name: "x".into(),
                    x_type: None,
                    value: Expr::Number("2".into())
                }
            );
        }
        _ => panic!("expected if statement"),
    }
}


#[test]
fn parse_for_loop_with_range() {
    let stmt = parse_stmt("for (i in 1:10) { sum <- sum + i }");
    match stmt {
        Stmt::For {
            iter_name,
            iter_vector,
            body,
        } => {
            assert_eq!(iter_name, "i");
            assert_eq!(
                iter_vector,
                Expr::Binary {
                    left: Box::new(Expr::Number("1".into())),
                    op: BinaryOp::Range,
                    right: Box::new(Expr::Number("10".into())),
                }
            );
            assert_eq!(body.len(), 1);
            assert_eq!(
                body[0],
                Stmt::VarAssign {
                    name: "sum".into(),
                    x_type: None,
                    value: Expr::Binary {
                        left: Box::new(Expr::Identifier("sum".into())),
                        op: BinaryOp::Plus,
                        right: Box::new(Expr::Identifier("i".into())),
                    }
                }
            );
        }
        _ => panic!("expected for statement"),
    }
}

#[test]
fn parse_for_loop_with_vector() {
    let stmt = parse_stmt("for (x in my_vec) { print(x) }");
    match stmt {
        Stmt::For {
            iter_name,
            iter_vector,
            body,
        } => {
            assert_eq!(iter_name, "x");
            assert_eq!(iter_vector, Expr::Identifier("my_vec".into()));
            assert_eq!(body.len(), 1);
            assert_eq!(
                body[0],
                Stmt::ExprStmt(Expr::Call {
                    callee: Box::new(Expr::Identifier("print".into())),
                    args: vec![Expr::Identifier("x".into())]
                })
            );
        }
        _ => panic!("expected for statement"),
    }
}