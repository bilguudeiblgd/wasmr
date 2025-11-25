use crate::ast::Stmt as AstStmt;
use crate::ir::{IRStmt as Stmt, TypeResolver, IR};

use super::WasmGenerator;

pub fn compile_to_wasm_ir(program: Vec<Stmt>) -> Vec<u8> {
    let mut wg = WasmGenerator::new();
    wg.compile_program(program)
}

pub fn compile_to_wasm(program: Vec<AstStmt>) -> Vec<u8> {
    let mut resolver = TypeResolver::new();
    let ir_program = match IR::from_ast(program, &mut resolver) {
        Ok(ir) => ir,
        Err(e) => {
            eprintln!("Type error during lowering: {:?}", e);
            Vec::new()
        }
    };
    compile_to_wasm_ir(ir_program)
}

pub fn wasm_to_wat(wasm_bytes: &[u8]) -> Result<String, String> {
    wasmprinter::print_bytes(wasm_bytes)
        .map_err(|e| format!("Failed to convert WASM to WAT: {}", e))
}

fn ensure_wasm_out_dir() -> std::io::Result<std::path::PathBuf> {
    use std::fs;
    use std::path::PathBuf;
    let mut path = PathBuf::from("data");
    path.push("wasm_out");
    if !path.exists() {
        fs::create_dir_all(&path)?;
    }
    Ok(path)
}

pub fn write_wasm_file<S: AsRef<str>>(
    filename_stem: S,
    bytes: &[u8],
) -> std::io::Result<std::path::PathBuf> {
    use std::fs;
    use std::path::PathBuf;
    let out_dir = ensure_wasm_out_dir()?;
    let mut path = PathBuf::from(out_dir);
    let stem = filename_stem.as_ref();
    let file_name = if stem.ends_with(".wasm") {
        stem.to_string()
    } else {
        format!("{}.wasm", stem)
    };
    path.push(file_name);
    fs::write(&path, bytes)?;
    Ok(path)
}

pub fn write_wat_file<S: AsRef<str>>(
    filename_stem: S,
    wat_text: &str,
) -> std::io::Result<std::path::PathBuf> {
    use std::fs;
    use std::path::PathBuf;
    let out_dir = ensure_wasm_out_dir()?;
    let mut path = PathBuf::from(out_dir);
    let stem = filename_stem.as_ref();
    let file_name = if stem.ends_with(".wat") {
        stem.to_string()
    } else {
        format!("{}.wat", stem)
    };
    path.push(file_name);
    fs::write(&path, wat_text)?;
    Ok(path)
}

pub fn compile_and_write_ir<S: AsRef<str>>(
    program: Vec<Stmt>,
    filename_stem: S,
) -> std::io::Result<std::path::PathBuf> {
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

pub fn compile_and_write<S: AsRef<str>>(
    program: Vec<AstStmt>,
    filename_stem: S,
) -> std::io::Result<std::path::PathBuf> {
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
