//! Rty_compiler: A typed R-like language compiler targeting WebAssembly
//!
//! # Architecture
//! - `types`: Cross-cutting type system used across all compilation phases
//! - `lexer`: Tokenization
//! - `ast`: Abstract syntax tree (untyped)
//! - `parser`: Token stream → syntax tree
//! - `ir`: Type resolution, semantic analysis, optimization passes
//! - `backend`: Code generation (currently WASM only)
//! - `driver`: Compilation orchestration, I/O, and format conversions

// Core compiler modules
pub mod types;
pub mod ast;
pub mod lexer;
pub mod parser;
pub mod ir;
pub mod backend;
pub mod driver;
