//! Pure code generation functions
//!
//! This module contains pure code generation functions that transform
//! AST or IR into WASM bytes without any I/O or orchestration concerns.

use crate::ast::Stmt as AstStmt;
use crate::ir::{IRProgram, IRPassManager, TypeResolver, IR};

use super::WasmGenerator;

/// Compile IR to WASM bytes
///
/// Takes a typed IR program, runs necessary passes, and generates WASM binary.
/// This is a pure transformation with no I/O side effects.
pub fn compile_to_wasm_ir(mut program: IRProgram) -> Vec<u8> {
    // Run IR passes to populate metadata
    let mut pass_manager = IRPassManager::default_pipeline();
    if let Err(e) = pass_manager.run(&mut program) {
        eprintln!("IR pass error: {}", e);
        return Vec::new();
    }

    let mut wg = WasmGenerator::new();
    wg.compile_program(program)
}

/// Compile AST to WASM bytes
///
/// Takes an untyped AST, performs type resolution and IR lowering,
/// then generates WASM binary. This is a pure transformation with no I/O.
pub fn compile_to_wasm(program: Vec<AstStmt>) -> Vec<u8> {
    let mut resolver = TypeResolver::new();
    let ir_program = match IR::from_ast(program, &mut resolver) {
        Ok(ir) => ir,
        Err(e) => {
            eprintln!("Type error during lowering: {:?}", e);
            return Vec::new();
        }
    };
    compile_to_wasm_ir(ir_program)
}
