//! File I/O operations
//!
//! Handles writing compiled output to disk, including WASM binaries
//! and WAT text files.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Ensure the output directory exists, preserving subdirectory structure
///
/// If relative_path is provided (e.g., "closures/simple"), creates "out/closures/"
/// If relative_path is None, creates "out/"
fn ensure_out_dir(relative_path: Option<&str>) -> io::Result<PathBuf> {
    let mut path = PathBuf::from("out");

    if let Some(rel_path) = relative_path {
        // Extract directory part from relative path (if any)
        if let Some(parent) = Path::new(rel_path).parent() {
            if parent != Path::new("") {
                path.push(parent);
            }
        }
    }

    if !path.exists() {
        fs::create_dir_all(&path)?;
    }
    Ok(path)
}

/// Write WASM binary to a file
///
/// The filename_stem can include subdirectory path (e.g., "closures/simple")
/// Output will be written to "out/closures/simple.wasm"
pub fn write_wasm_file<S: AsRef<str>>(
    filename_stem: S,
    bytes: &[u8],
) -> io::Result<PathBuf> {
    let stem = filename_stem.as_ref();
    let out_dir = ensure_out_dir(Some(stem))?;

    // Get just the filename part (no directory)
    let file_name = Path::new(stem)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(stem);

    let file_name = if file_name.ends_with(".wasm") {
        file_name.to_string()
    } else {
        format!("{}.wasm", file_name)
    };

    let mut path = out_dir;
    path.push(file_name);
    fs::write(&path, bytes)?;
    Ok(path)
}

/// Write WAT text to a file
///
/// The filename_stem can include subdirectory path (e.g., "closures/simple")
/// Output will be written to "out/closures/simple.wat"
pub fn write_wat_file<S: AsRef<str>>(
    filename_stem: S,
    wat_text: &str,
) -> io::Result<PathBuf> {
    let stem = filename_stem.as_ref();
    let out_dir = ensure_out_dir(Some(stem))?;

    // Get just the filename part (no directory)
    let file_name = Path::new(stem)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(stem);

    let file_name = if file_name.ends_with(".wat") {
        file_name.to_string()
    } else {
        format!("{}.wat", file_name)
    };

    let mut path = out_dir;
    path.push(file_name);
    fs::write(&path, wat_text)?;
    Ok(path)
}
