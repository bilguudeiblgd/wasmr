//! Code emission for IR constructs
//!
//! This module handles the translation of typed IR nodes into WASM instructions.
//! Each submodule corresponds to a major IR construct category.

pub(super) mod expressions;
pub(super) mod statements;
pub(super) mod builtins;
pub(super) mod binary_ops;
pub(super) mod functions;
pub(super) mod ref_cell;
