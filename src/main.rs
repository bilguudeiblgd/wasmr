use std::fs::File;
use std::io;
use std::io::Read;
use rty_compiler::lexer;

fn main() {
    let lexer = lexer::Lexer::new();
    let file_path = String::from("data/bla.R");
    let code = parse_file(file_path).expect("File not found");
    let tokens = lexer.lex(&code);

    for token in tokens {
        println!("{:?}", token)
    }
}

fn parse_file(file_path: String) -> io::Result<String> {
    let mut f = File::open(file_path)?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;
    Ok(contents)
}