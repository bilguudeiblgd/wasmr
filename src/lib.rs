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
pub mod type_eraser;
mod name_mangling;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile_string_to_wat(source: &str) -> Result<String, String> {
    // Set up panic hook for better error messages in browser
    #[cfg(target_arch = "wasm32")]
    console_error_panic_hook::set_once();

    // Step 1: Lexing
    let lexer = lexer::Lexer::new();
    let tokens = lexer.lex(&source.to_string());

    // Step 2: Parsing
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse_program()
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Step 3: Merge with runtime AST if available
    let ast_with_runtime = if let Some(runtime_ast) = driver::runtime::load_runtime_ast() {
        driver::runtime::merge_runtime_ast_with_user(runtime_ast, ast)
    } else {
        ast
    };

    // Step 4: Type resolution and IR generation
    let mut resolver = ir::TypeResolver::new();
    let ir_program = ir::IR::from_ast(ast_with_runtime, &mut resolver)
        .map_err(|e| format!("Type error: {:?}", e))?;

    // Step 5: Code generation (IR → WASM bytes)
    let wasm_bytes = backend::compile_to_wasm_ir(ir_program);

    // Step 6: Convert WASM bytes to WAT text format
    driver::conversion::wasm_to_wat(&wasm_bytes)
}