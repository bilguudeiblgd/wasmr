//! WASM-specific code generation utilities
//!
//! This module contains WASM-specific concerns:
//! - Type mapping between Rty types and WASM types
//! - Memory management and WASI integration
//! - Runtime helper functions

pub(super) mod types;
pub(super) mod memory;
pub(super) mod runtime;
