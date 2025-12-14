/// Function Environment Handling
///
/// This module manages environment struct types for functions that capture
/// variables from parent scopes.
///
/// When a function references variables from its parent scope, those variables
/// are passed through an environment struct as the first parameter.
///
/// Environment struct layout:
/// - Field 0: placeholder (i32) - reserved for future function pointer support
/// - Field 1..N: captured variables from parent scope

use crate::ir::CapturedVarInfo;
use wasm_encoder::{FieldType, HeapType, RefType, StorageType, StructType, ValType};

use super::WasmGenerator;

impl WasmGenerator {
    /// Get or create an environment struct type for a function with captured variables
    ///
    /// The struct has the layout:
    /// - Field 0: i32 placeholder (reserved for future function pointer support)
    /// - Field 1..N: captured variables with their actual types
    ///
    /// Returns the type index of the environment struct
    pub(crate) fn get_or_create_env_struct_type(
        &mut self,
        func_name: &str,
        captured_vars: &[CapturedVarInfo],
        _func_type_idx: u32, // Reserved for future use
    ) -> u32 {
        // Check cache - reuse existing env type if already created
        if let Some(&type_idx) = self.env_struct_types.get(func_name) {
            return type_idx;
        }

        // Build struct fields
        let mut fields = Vec::new();

        // Field 0: placeholder (reserved for future function pointer)
        fields.push(FieldType {
            element_type: StorageType::Val(ValType::I32),
            mutable: false
        });

        // Field 1..N: captured variables
        for captured in captured_vars {
            let val_type = self.wasm_valtype(&captured.ty);
            fields.push(FieldType {
                element_type: StorageType::Val(val_type),
                mutable: captured.is_mutable,
            });
        }

        // Create struct type
        let type_idx = self.types.len() as u32;
        self.types.ty().struct_(fields);

        // Cache it
        self.env_struct_types.insert(func_name.to_string(), type_idx);

        type_idx
    }

    /// Get the ValType for an environment struct reference
    pub(crate) fn env_struct_valtype(&self, struct_type_idx: u32) -> ValType {
        ValType::Ref(RefType {
            nullable: false,
            heap_type: HeapType::Concrete(struct_type_idx),
        })
    }
}
