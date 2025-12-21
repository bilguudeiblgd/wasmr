use std::env;
use std::fs;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use rty_compiler::{driver, lexer, parser::Parser};

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {e}");
        std::process::exit(1);
    }
}

fn run() -> io::Result<()> {
    let lexer = lexer::Lexer::new();

    // Check if MY_FILE environment variable is set
    if let Ok(file_arg) = env::var("MY_FILE") {
        // Compile single file
        let file_path = Path::new(&file_arg);

        if !file_path.exists() {
            eprintln!("File not found: {}", file_arg);
            return Err(io::Error::new(io::ErrorKind::NotFound, "File not found"));
        }

        match process_file(&lexer, file_path) {
            Ok(out_path) => {
                println!("Compiled {:?} -> {:?}", file_path, out_path);
                Ok(())
            }
            Err(err) => {
                eprintln!("Failed to compile {:?}: {err}", file_path);
                Err(err)
            }
        }
    } else {
        // Default: compile all .R files in data/ directory (recursively)
        let data_dir = Path::new("data");
        let mut processed = 0usize;
        let mut failed = 0usize;

        // Recursively walk data/ directory
        walk_dir(data_dir, &lexer, &mut processed, &mut failed)?;

        println!("Summary: processed = {processed}, failed = {failed}");
        Ok(())
    }
}

/// Recursively walk a directory and compile all .R files
fn walk_dir(
    dir: &Path,
    lexer: &lexer::Lexer,
    processed: &mut usize,
    failed: &mut usize,
) -> io::Result<()> {
    let entries = fs::read_dir(dir)?;

    for entry in entries {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => {
                *failed += 1;
                continue;
            }
        };

        let path = entry.path();

        if path.is_dir() {
            // Recursively process subdirectories
            walk_dir(&path, lexer, processed, failed)?;
        } else if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("R") {
            match process_file(&lexer, &path) {
                Ok(out_path) => {
                    println!("Compiled {:?} -> {:?}", path, out_path);
                    *processed += 1;
                }
                Err(err) => {
                    eprintln!("Failed {:?}: {err}", path);
                    *failed += 1;
                }
            }
        }
    }

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
        Err(e) => {
            println!("{:#?}", e);
            return Err(io::Error::new(io::ErrorKind::InvalidData, "parse error:"));
        }
    };

    // Compute relative path from data/ to preserve directory structure
    // e.g., "data/closures/simple.R" -> "closures/simple"
    let data_dir = Path::new("data");
    let relative_path = path
        .strip_prefix(data_dir)
        .unwrap_or(path)
        .with_extension("");

    let relative_path_str = relative_path.to_str().unwrap_or("out");

    // driver + write
    driver::compile_and_write(program, relative_path_str)
}
