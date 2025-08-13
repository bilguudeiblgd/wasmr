use rty_compiler::lexer::{Lexer, Token};

fn lex_str(s: &str) -> Vec<Token> {
    let lexer = Lexer::new();
    lexer.lex(&s.to_string())
}

#[test]
fn function_def_tokens() {
    let code = "f <- function(x: int): int { return(x + x) }";
    let tokens = lex_str(code);
    let expected = vec![
        Token::Identifier("f".into()),
        Token::AssignArrow,
        Token::Function,
        Token::LParen,
        Token::Identifier("x".into()),
        Token::Colon,
        Token::Type("int".into()),
        Token::RParen,
        Token::Colon,
        Token::Type("int".into()),
        Token::LBrace,
        Token::Return,
        Token::LParen,
        Token::Identifier("x".into()),
        Token::Plus,
        Token::Identifier("x".into()),
        Token::RParen,
        Token::RBrace,
        Token::EOF,
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn call_tokens() {
    let code = "f()";
    let tokens = lex_str(code);
    let expected = vec![
        Token::Identifier("f".into()),
        Token::LParen,
        Token::RParen,
        Token::EOF,
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn less_and_less_equal_tokens() {
    let tokens1 = lex_str("a <- 1 < 2");
    let expected1 = vec![
        Token::Identifier("a".into()),
        Token::AssignArrow,
        Token::Number("1".into()),
        Token::Less,
        Token::Number("2".into()),
        Token::EOF,
    ];
    assert_eq!(tokens1, expected1);

    let tokens2 = lex_str("b <- 3 <= 4");
    let expected2 = vec![
        Token::Identifier("b".into()),
        Token::AssignArrow,
        Token::Number("3".into()),
        Token::LessEqual,
        Token::Number("4".into()),
        Token::EOF,
    ];
    assert_eq!(tokens2, expected2);
}

#[test]
fn c_constructor_and_assign() {
    let tokens = lex_str("x <- c(1, 2, 3)");
    let expected = vec![
        Token::Identifier("x".into()),
        Token::AssignArrow,
        Token::Identifier("c".into()),
        Token::LParen,
        Token::Number("1".into()),
        Token::Comma,
        Token::Number("2".into()),
        Token::Comma,
        Token::Number("3".into()),
        Token::RParen,
        Token::EOF,
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn range_tokens() {
    let tokens = lex_str("1:3");
    let expected = vec![
        Token::Number("1".into()),
        Token::Colon,
        Token::Number("3".into()),
        Token::EOF,
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn builtin_sum_call() {
    let tokens = lex_str("s <- sum(x)");
    let expected = vec![
        Token::Identifier("s".into()),
        Token::AssignArrow,
        Token::Identifier("sum".into()),
        Token::LParen,
        Token::Identifier("x".into()),
        Token::RParen,
        Token::EOF,
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn constructors_seq_and_rep() {
    let seq_tokens = lex_str("b <- seq(1, 5)");
    let seq_expected = vec![
        Token::Identifier("b".into()),
        Token::AssignArrow,
        Token::Identifier("seq".into()),
        Token::LParen,
        Token::Number("1".into()),
        Token::Comma,
        Token::Number("5".into()),
        Token::RParen,
        Token::EOF,
    ];
    assert_eq!(seq_tokens, seq_expected);

    let rep_tokens = lex_str("c <- rep(7, 3)");
    let rep_expected = vec![
        Token::Identifier("c".into()),
        Token::AssignArrow,
        Token::Identifier("rep".into()),
        Token::LParen,
        Token::Number("7".into()),
        Token::Comma,
        Token::Number("3".into()),
        Token::RParen,
        Token::EOF,
    ];
    assert_eq!(rep_tokens, rep_expected);
}

#[test]
fn vector_scalar_ops() {
    let a = lex_str("a <- x + 10");
    let a_expected = vec![
        Token::Identifier("a".into()),
        Token::AssignArrow,
        Token::Identifier("x".into()),
        Token::Plus,
        Token::Number("10".into()),
        Token::EOF,
    ];
    assert_eq!(a, a_expected);

    let b = lex_str("b <- x * 2");
    let b_expected = vec![
        Token::Identifier("b".into()),
        Token::AssignArrow,
        Token::Identifier("x".into()),
        Token::Mul,
        Token::Number("2".into()),
        Token::EOF,
    ];
    assert_eq!(b, b_expected);

    let c = lex_str("c <- 5 - x");
    let c_expected = vec![
        Token::Identifier("c".into()),
        Token::AssignArrow,
        Token::Number("5".into()),
        Token::Minus,
        Token::Identifier("x".into()),
        Token::EOF,
    ];
    assert_eq!(c, c_expected);
}
