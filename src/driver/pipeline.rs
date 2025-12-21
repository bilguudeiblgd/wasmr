//! Compilation pipeline orchestration
//!
//! Coordinates the full compilation pipeline from source AST/IR through
//! code generation, format conversion, and file output.

use std::io;
use std::path::PathBuf;

use crate::ast::Stmt as AstStmt;
use crate::backend::{compile_to_wasm, compile_to_wasm_ir};
use crate::ir::IRProgram;

use super::conversion::wasm_to_wat;
use super::io::{write_wasm_file, write_wat_file};

/// Compile IR to WASM and write both .wasm and .wat files
///
/// This is the high-level orchestration function that:
/// 1. Compiles IR to WASM bytes
/// 2. Writes the WASM binary file
/// 3. Converts to WAT text format
/// 4. Writes the WAT file (with warnings on failure)
pub fn compile_and_write_ir<S: AsRef<str>>(
    program: IRProgram,
    filename_stem: S,
) -> io::Result<PathBuf> {
    let bytes = compile_to_wasm_ir(program);
    let stem = filename_stem.as_ref();

    let wasm_path = write_wasm_file(stem, &bytes)?;

    match wasm_to_wat(&bytes) {
        Ok(wat_text) => {
            if let Err(e) = write_wat_file(stem, &wat_text) {
                eprintln!("Warning: Failed to write WAT file: {}", e);
            }
        }
        Err(e) => {
            eprintln!("Warning: Failed to convert to WAT: {}", e);
        }
    }

    Ok(wasm_path)
}

/// Compile AST to WASM and write both .wasm and .wat files
///
/// This is the high-level orchestration function that:
/// 1. Compiles AST to WASM bytes (via IR)
/// 2. Writes the WASM binary file
/// 3. Converts to WAT text format
/// 4. Writes the WAT file (with warnings on failure)
pub fn compile_and_write<S: AsRef<str>>(
    program: Vec<AstStmt>,
    filename_stem: S,
) -> io::Result<PathBuf> {
    let bytes = compile_to_wasm(program);
    let stem = filename_stem.as_ref();

    let wasm_path = write_wasm_file(stem, &bytes)?;

    match wasm_to_wat(&bytes) {
        Ok(wat_text) => {
            if let Err(e) = write_wat_file(stem, &wat_text) {
                eprintln!("Warning: Failed to write WAT file: {}", e);
            }
        }
        Err(e) => {
            eprintln!("Warning: Failed to convert to WAT: {}", e);
        }
    }

    Ok(wasm_path)
}
