use std::fs;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use rty_compiler::{lexer, parser::Parser, codegen};

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {e}");
        std::process::exit(1);
    }
}

fn run() -> io::Result<()> {
    let data_dir = Path::new("data");
    let lexer = lexer::Lexer::new();

    let entries = fs::read_dir(data_dir)?;
    let mut processed = 0usize;
    let mut failed = 0usize;

    for entry in entries {
        let entry = match entry { Ok(e) => e, Err(_) => { failed += 1; continue } };
        let path = entry.path();
        if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("R") {
            match process_file(&lexer, &path) {
                Ok(out_path) => {
                    println!("Compiled {:?} -> {:?}", path.file_name().unwrap_or_default(), out_path);
                    processed += 1;
                }
                Err(err) => {
                    eprintln!("Failed {:?}: {err}", path.file_name().unwrap_or_default());
                    failed += 1;
                }
            }
        }
    }

    println!("Summary: processed = {processed}, failed = {failed}");
    Ok(())
}

fn process_file(lexer: &lexer::Lexer, path: &Path) -> io::Result<PathBuf> {
    // read source
    let mut src = String::new();
    File::open(path)?.read_to_string(&mut src)?;

    // lex
    let tokens = lexer.lex(&src);

    // parse
    let mut parser = Parser::new(tokens);
    let program = match parser.parse_program() {
        Ok(p) => p,
        Err(_e) => {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "parse error"));
        }
    };

    // codegen + write
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
    codegen::compile_and_write(program, stem)
}