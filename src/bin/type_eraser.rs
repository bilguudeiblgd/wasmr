use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path};
use rty_compiler::{lexer, parser::Parser, type_eraser};

fn main() -> io::Result<()> {
    let lexer = lexer::Lexer::new();
    let data_dir = Path::new("data");
    let data_r_dir = Path::new("data_R");

    if !data_r_dir.exists() {
        fs::create_dir_all(data_r_dir)?;
    }

    // Recursively process all .R files in data/
    walk_and_erase(data_dir, data_r_dir, &lexer)?;

    println!("Type erasure complete. Files written to data_R/");
    Ok(())
}

/// Recursively walk data/ directory and erase types
fn walk_and_erase(
    data_dir: &Path,
    data_r_dir: &Path,
    lexer: &lexer::Lexer,
) -> io::Result<()> {
    let entries = fs::read_dir(data_dir)?;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            // Get relative path from data/ root
            let rel_path = path.strip_prefix("data").unwrap_or(&path);
            let out_subdir = data_r_dir.join(rel_path);

            // Create corresponding subdirectory in data_R/
            if !out_subdir.exists() {
                fs::create_dir_all(&out_subdir)?;
            }

            // Recursively process subdirectory
            walk_and_erase(&path, data_r_dir, lexer)?;
        } else if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("R") {
            // Process R file
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

            // Compute output path preserving directory structure
            let rel_path = path.strip_prefix("data").unwrap_or(&path);
            let out_path = data_r_dir.join(rel_path);

            let mut out_file = fs::File::create(out_path)?;
            out_file.write_all(erased_code.as_bytes())?;
        }
    }

    Ok(())
}
