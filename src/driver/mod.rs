//! Compilation driver and orchestration
//!
//! This module handles high-level compilation orchestration, separating concerns:
//! - **Pipeline**: Full compilation flow from source to output files
//! - **I/O**: File system operations for reading/writing artifacts
//! - **Conversion**: Format transformations (e.g., WASM → WAT)
//!
//! The driver layer sits above the backend, coordinating compilation stages
//! and managing external concerns like file I/O that don't belong in pure
//! code generation.

pub mod conversion;
pub mod io;
pub mod pipeline;

// Re-export commonly used functions
pub use conversion::wasm_to_wat;
pub use io::{write_wasm_file, write_wat_file};
pub use pipeline::{compile_and_write, compile_and_write_ir};
