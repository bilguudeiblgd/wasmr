use rty_compiler::lexer::Lexer;

#[test]
fn test_greater_equal_token() {
    let source = "x >= y".to_string();
    let lexer = Lexer::new();
    let tokens = lexer.lex(&source);

    println!("Tokens: {:?}", tokens);

    // We expect: Identifier(x), GreaterEqual, Identifier(y), EOF
    assert_eq!(tokens.len(), 4);
}
