use rty_compiler::lexer::{Lexer, Token};
use rty_compiler::parser::Parser;
use rty_compiler::ast::{Expr, Stmt, BinaryOp, Param};

fn lex_tokens(s: &str) -> Vec<Token> {
    let lexer = Lexer::new();
    lexer.lex(&s.to_string())
}

fn parse_expr(s: &str) -> Expr {
    let mut tokens = lex_tokens(s);
    if *tokens.last().unwrap() != Token::EOF { tokens.push(Token::EOF); }
    let mut p = Parser::new(tokens);
    p.parse_expression().expect("parse_expression failed")
}

fn parse_stmt(s: &str) -> Stmt {
    let mut tokens = lex_tokens(s);
    if *tokens.last().unwrap() != Token::EOF { tokens.push(Token::EOF); }
    let mut p = Parser::new(tokens);
    p.parse_statement().expect("parse_statement failed")
}

fn parse_program(s: &str) -> Vec<Stmt> {
    let mut tokens = lex_tokens(s);
    if *tokens.last().unwrap() != Token::EOF { tokens.push(Token::EOF); }
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
            }
        ],
    };
    assert_eq!(expr, expected);
}

#[test]
fn parse_assignment_statement() {
    let stmt = parse_stmt("x <- 1:3");
    let expected = Stmt::VarAssign {
        name: "x".into(),
        value: Expr::Binary {
            left: Box::new(Expr::Number("1".into())),
            op: BinaryOp::Range,
            right: Box::new(Expr::Number("3".into())),
        }
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
    let stmt = parse_stmt("{ x <- 1 y <- 2 }");
    match stmt {
        Stmt::Block(body) => {
            assert_eq!(body.len(), 2);
            assert_eq!(body[0], Stmt::VarAssign { name: "x".into(), value: Expr::Number("1".into())});
            assert_eq!(body[1], Stmt::VarAssign { name: "y".into(), value: Expr::Number("2".into())});
        }
        _ => panic!("expected block"),
    }
}

#[test]
fn parse_function_definition_full() {
    let prog = parse_program("f <- function(x: int): int { return(x + x) }");
    assert_eq!(prog.len(), 1);
    match &prog[0] {
        Stmt::FunctionDef { name, params, return_type, body } => {
            assert_eq!(name, "f");
            assert_eq!(params, &vec![Param { name: "x".into(), ty: Some("int".into()) }]);
            assert_eq!(return_type, &Some("int".into()));
            assert_eq!(body.len(), 1);
            assert_eq!(body[0], Stmt::Return(Some(Expr::Binary {
                left: Box::new(Expr::Identifier("x".into())),
                op: BinaryOp::Plus,
                right: Box::new(Expr::Identifier("x".into())),
            })));
        }
        _ => panic!("expected function def"),
    }
}

#[test]
fn parse_program_multiple_statements() {
    let prog = parse_program("a <- 1 b <- 2");
    assert_eq!(prog.len(), 2);
    assert_eq!(prog[0], Stmt::VarAssign { name: "a".into(), value: Expr::Number("1".into())});
    assert_eq!(prog[1], Stmt::VarAssign { name: "b".into(), value: Expr::Number("2".into())});
}
