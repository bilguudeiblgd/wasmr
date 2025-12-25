use Rty_compiler::lexer::{Lexer, Token};

fn main() {
    let lexer = Lexer::new();
    let input = "5 % 2".to_string();
    let tokens = lexer.lex(&input);

    println!("Input: {}", input);
    println!("Tokens: {:#?}", tokens);
}
