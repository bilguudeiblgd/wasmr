use rty_compiler::ast::{Expr, Param, ParamKind, Stmt, Type};
use rty_compiler::ir::{BuiltinKind, IRExprKind, IRStmt, TypeError, TypeResolver, IR};
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

fn lower(src: &str) -> Result<Vec<IRStmt>, TypeError> {
    let ast = parse_program(src);
    let mut resolver = TypeResolver::new();
    IR::from_ast(ast, &mut resolver)
}

#[test]
fn lowers_builtin_c_numeric() {
    let ir = lower("a <- c(1, 2, 3)").expect("lower failed");
    match &ir[0] {
        IRStmt::VarAssign { name, value, .. } => {
            assert_eq!(name, "a");
            match &value.kind {
                IRExprKind::BuiltinCall { builtin, args } => {
                    assert_eq!(*builtin, BuiltinKind::C);
                    assert_eq!(args.len(), 3);
                    assert!(args.iter().all(|arg| arg.ty == Type::Int));
                }
                other => panic!("expected builtin call, got {:?}", other),
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
    match &ir[0] {
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
    match &ir[0] {
        IRStmt::FunctionDef { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                IRStmt::Return(expr) => match &expr.kind {
                    IRExprKind::BuiltinCall { builtin, args } => {
                        assert_eq!(*builtin, BuiltinKind::C);
                        assert_eq!(args.len(), 1);
                        assert_eq!(args[0].ty, Type::VarArgs);
                        assert!(matches!(args[0].kind, IRExprKind::VarArgs));
                    }
                    other => panic!("expected builtin c call, got {:?}", other),
                },
                other => panic!("expected return, got {:?}", other),
            }
        }
        other => panic!("expected function def, got {:?}", other),
    }
}
