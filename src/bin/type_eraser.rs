use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
use rty_compiler::{lexer, parser::Parser, type_eraser};

fn main() -> io::Result<()> {
    let lexer = lexer::Lexer::new();
    let data_dir = Path::new("data");
    let data_r_dir = Path::new("data_R");

    if !data_r_dir.exists() {
        fs::create_dir_all(data_r_dir)?;
    }

    let entries = fs::read_dir(data_dir)?;
    for entry in entries {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("R") {
            let mut src = String::new();
            fs::File::open(&path)?.read_to_string(&mut src)?;

            let tokens = lexer.lex(&src);
            let mut parser = Parser::new(tokens);
            let program = match parser.parse_program() {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("Failed to parse {:?}: {:?}", path, e);
                    continue;
                }
            };

            let erased_code = type_eraser::to_r_code(&program);
            let out_path = data_r_dir.join(path.file_name().unwrap());
            let mut out_file = fs::File::create(out_path)?;
            out_file.write_all(erased_code.as_bytes())?;
        }
    }

    println!("Type erasure complete. Files written to data_R/");
    Ok(())
}
