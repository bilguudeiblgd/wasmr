//! Runtime library compilation and embedding
//!
//! This module handles compilation of built-in R functions from the
//! `runtime_embed/` directory. These functions are compiled to IR and
//! merged with user code before WASM generation.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use crate::lexer::Lexer;
use crate::parser::Parser;

/// Load all runtime R files from the runtime_embed directory
///
/// Returns AST statements containing all runtime function definitions,
/// or None if the directory doesn't exist or no valid files were found.
pub fn load_runtime_ast() -> Option<Vec<crate::ast::Stmt>> {
    let runtime_dir = Path::new("../../runtime_embed");

    if !runtime_dir.exists() {
        eprintln!("Warning: runtime_embed directory not found");
        return None;
    }

    let mut all_runtime_stmts = Vec::new();
    let lexer = Lexer::new();

    // Collect all .R files from runtime_embed/
    let r_files = match collect_runtime_files(runtime_dir) {
        Ok(files) => files,
        Err(e) => {
            eprintln!("Warning: Failed to read runtime_embed directory: {}", e);
            return None;
        }
    };

    if r_files.is_empty() {
        return None;
    }

    // Compile each runtime file to AST
    for file_path in r_files {
        eprintln!("Loading runtime file: {:?}", file_path);
        match compile_runtime_file(&lexer, &file_path) {
            Ok(stmts) => {
                eprintln!("  Successfully loaded {} statements", stmts.len());
                all_runtime_stmts.extend(stmts);
            }
            Err(e) => {
                eprintln!("Warning: Failed to compile runtime file {:?}: {}", file_path, e);
                // Continue with other files even if one fails
            }
        }
    }

    if all_runtime_stmts.is_empty() {
        return None;
    }

    // Count function definitions (VarAssign with FunctionDef expression)
    let func_count = all_runtime_stmts.iter().filter(|s| {
        matches!(s, crate::ast::Stmt::VarAssign { value: crate::ast::Expr::FunctionDef { .. }, .. })
    }).count();
    println!("Successfully loaded {} runtime functions", func_count);
    Some(all_runtime_stmts)
}

/// Collect all .R files from the runtime directory (non-recursive)
fn collect_runtime_files(dir: &Path) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        // Only process .R files (not subdirectories)
        if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("R") {
            files.push(path);
        }
    }

    // Sort for deterministic compilation order
    files.sort();
    Ok(files)
}

/// Compile a single runtime R file to AST statements
fn compile_runtime_file(lexer: &Lexer, path: &Path) -> io::Result<Vec<crate::ast::Stmt>> {
    use std::fs::File;
    use std::io::Read;

    // Read source file
    let mut src = String::new();
    File::open(path)?.read_to_string(&mut src)?;

    // Lex
    let tokens = lexer.lex(&src);

    // Parse
    let mut parser = Parser::new(tokens);
    parser.parse_program().map_err(|e| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Parse error in {:?}: {:?}", path, e),
        )
    })
}

/// Merge runtime AST with user AST
///
/// Runtime function definitions are prepended to user code so they're
/// available during type resolution and before user code executes.
pub fn merge_runtime_ast_with_user(
    runtime: Vec<crate::ast::Stmt>,
    user: Vec<crate::ast::Stmt>,
) -> Vec<crate::ast::Stmt> {
    // Prepend runtime statements to user statements
    let mut merged = runtime;
    merged.extend(user);
    merged
}