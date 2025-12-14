/// Reference Cell Handling
///
/// This module manages reference cell struct types for variables that need
/// to be captured mutably from nested functions (super-assignment).
///
/// When a variable is super-assigned in a nested function, it needs to be
/// stored in a mutable struct (reference cell) so that all functions can
/// share access to the same mutable location.
///
/// Reference cell struct layout:
/// - Single field: the variable value (mutable)

use crate::ast::Type;
use wasm_encoder::{FieldType, HeapType, RefType, StorageType, ValType};
use std::collections::HashMap;

use super::WasmGenerator;

impl WasmGenerator {
    /// Get or create a reference cell struct type for a given value type
    ///
    /// Reference cells are mutable single-field structs used for variables
    /// that need to be captured mutably (super-assigned) from nested functions.
    ///
    /// The struct has a single mutable field containing the actual value.
    ///
    /// Returns the type index of the reference cell struct
    pub(crate) fn get_or_create_ref_cell_type(&mut self, value_type: &Type) -> u32 {
        // Check cache first
        if let Some(&type_idx) = self.ref_cell_types.get(value_type) {
            return type_idx;
        }

        let val_type = self.wasm_valtype(value_type);

        // Create a struct with single mutable field
        let field = FieldType {
            element_type: StorageType::Val(val_type),
            mutable: true,
        };

        let type_idx = self.types.len() as u32;
        self.types.ty().struct_(vec![field]);

        // Cache it
        self.ref_cell_types.insert(value_type.clone(), type_idx);

        type_idx
    }

    /// Get the ValType for a reference cell struct reference
    pub(crate) fn ref_cell_valtype(&self, struct_type_idx: u32) -> ValType {
        ValType::Ref(RefType {
            nullable: false,
            heap_type: HeapType::Concrete(struct_type_idx),
        })
    }
}
