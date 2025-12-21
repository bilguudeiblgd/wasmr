//! File I/O operations
//!
//! Handles writing compiled output to disk, including WASM binaries
//! and WAT text files.

use std::fs;
use std::io;
use std::path::PathBuf;

/// Ensure the output directory exists
fn ensure_wasm_out_dir() -> io::Result<PathBuf> {
    let mut path = PathBuf::from("data");
    path.push("wasm_out");
    if !path.exists() {
        fs::create_dir_all(&path)?;
    }
    Ok(path)
}

/// Write WASM binary to a file
pub fn write_wasm_file<S: AsRef<str>>(
    filename_stem: S,
    bytes: &[u8],
) -> io::Result<PathBuf> {
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

/// Write WAT text to a file
pub fn write_wat_file<S: AsRef<str>>(
    filename_stem: S,
    wat_text: &str,
) -> io::Result<PathBuf> {
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
