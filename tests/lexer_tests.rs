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

#[test]
fn simple_string() {
    let a = lex_str("\"bla bla\"");
    let a_expected = vec![Token::XString("bla bla".into()), Token::EOF];
    assert_eq!(a, a_expected);
}

#[test]
fn lex_vector_type_in_function_signature() {
    let tokens = lex_str("function(x: vector<int>): vector<double>");
    assert_eq!(
        tokens,
        vec![
            Token::Function,
            Token::LParen,
            Token::Identifier("x".into()),
            Token::Colon,
            Token::Type("vector".into()),
            Token::Less,
            Token::Type("int".into()),
            Token::Greater,
            Token::RParen,
            Token::Colon,
            Token::Type("vector".into()),
            Token::Less,
            Token::Type("double".into()),
            Token::Greater,
            Token::EOF
        ]
    );
}

#[test]
fn lex_if_statement() {
    let tokens = lex_str(
        "\
            x <- 15
        result <- 0

        if (x > 20) {
          result <- 1
        } else if (x == 20) {
          result <- 2
        } else {
          result <- 3
        }

        result",
    );
    let token_without_newlines = tokens.iter().filter(|t| *t != &Token::Newline).map(|t| t.clone()) .collect::<Vec<_>>();
    assert_eq!(
        token_without_newlines,
        vec![
            Token::Identifier("x".into()),
            Token::AssignArrow,
            Token::Number("15".into()),
            Token::Identifier("result".into()),
            Token::AssignArrow,
            Token::Number("0".into()),
            Token::If,
            Token::LParen,
            Token::Identifier("x".into()),
            Token::Greater,
            Token::Number("20".into()),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("result".into()),
            Token::AssignArrow,
            Token::Number("1".into()),
            Token::RBrace,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Identifier("x".into()),
            Token::Equality,
            Token::Number("20".into()),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("result".into()),
            Token::AssignArrow,
            Token::Number("2".into()),
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Identifier("result".into()),
            Token::AssignArrow,
            Token::Number("3".into()),
            Token::RBrace,
            Token::Identifier("result".into()),
            Token::EOF
        ]
    );
}

#[test]
fn super_assign_token() {
    // Test <<- (super-assignment) is distinguished from <- and <
    let tokens = lex_str("count <<- count + 1");
    let expected = vec![
        Token::Identifier("count".into()),
        Token::SuperAssignArrow,
        Token::Identifier("count".into()),
        Token::Plus,
        Token::Number("1".into()),
        Token::EOF,
    ];
    assert_eq!(tokens, expected);

    // Test that <-, <<-, and < all lex correctly in same context
    let tokens2 = lex_str("x <- 5\ny <<- 10\nif (x < y) { }");
    let token_without_newlines: Vec<Token> = tokens2
        .iter()
        .filter(|t| *t != &Token::Newline)
        .cloned()
        .collect();
    let expected2 = vec![
        Token::Identifier("x".into()),
        Token::AssignArrow,
        Token::Number("5".into()),
        Token::Identifier("y".into()),
        Token::SuperAssignArrow,
        Token::Number("10".into()),
        Token::If,
        Token::LParen,
        Token::Identifier("x".into()),
        Token::Less,
        Token::Identifier("y".into()),
        Token::RParen,
        Token::LBrace,
        Token::RBrace,
        Token::EOF,
    ];
    assert_eq!(token_without_newlines, expected2);
}

#[test]
fn arrow_token() {
    // Test -> token for function types
    let code = "float -> float";
    let tokens = lex_str(code);
    let expected = vec![
        Token::Type("float".into()),
        Token::Arrow,
        Token::Type("float".into()),
        Token::EOF,
    ];
    assert_eq!(tokens, expected);

    // Test that -> and - are distinguished
    let code2 = "x - y -> z";
    let tokens2 = lex_str(code2);
    let expected2 = vec![
        Token::Identifier("x".into()),
        Token::Minus,
        Token::Identifier("y".into()),
        Token::Arrow,
        Token::Identifier("z".into()),
        Token::EOF,
    ];
    assert_eq!(tokens2, expected2);
}
