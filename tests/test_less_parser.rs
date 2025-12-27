use rty_compiler::lexer::Lexer;
use rty_compiler::parser::Parser;

#[test]
fn test_parse_less_than() {
    let source = "x <- 5\ny <- 3\nresult <- x < y".to_string();
    let lexer = Lexer::new();
    let tokens = lexer.lex(&source);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program();

    match ast {
        Ok(statements) => {
            println!("Successfully parsed {} statements", statements.len());
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
            panic!("Failed to parse");
        }
    }
}
