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

use super::super::WasmGenerator;

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
        // Generate a canonical key based on the structure, not the function name
        // This allows reusing the same type for different functions with identical environments
        let struct_key = self.make_env_struct_key(captured_vars);

        // Check if we already have this exact struct type
        if let Some(&type_idx) = self.env_struct_type_cache.get(&struct_key) {
            // Cache the mapping for this function name too
            self.env_struct_types.insert(func_name.to_string(), type_idx);
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
            // If the captured variable is mutable (via super-assignment),
            // it's stored as a ref cell in the caller's scope.
            // We need to pass the ref cell itself (not extract its value).
            let val_type = if captured.is_mutable {
                let ref_cell_type_idx = self.get_or_create_ref_cell_type(&captured.ty);
                self.ref_cell_valtype(ref_cell_type_idx)
            } else {
                self.wasm_valtype(&captured.ty)
            };
            fields.push(FieldType {
                element_type: StorageType::Val(val_type),
                mutable: false,  // The ref cell itself is immutable; the value inside is mutable
            });
        }

        // Create struct type
        let type_idx = self.types.len() as u32;
        self.types.ty().struct_(fields);

        // Cache by structure key (for reuse)
        self.env_struct_type_cache.insert(struct_key, type_idx);
        // Cache by function name (for lookup)
        self.env_struct_types.insert(func_name.to_string(), type_idx);

        type_idx
    }

    /// Generate a canonical key for an environment struct based on its field types
    /// This allows different functions with the same environment structure to share types
    fn make_env_struct_key(&mut self, captured_vars: &[CapturedVarInfo]) -> String {
        let mut key = String::from("env:");
        for captured in captured_vars {
            key.push_str(&format!("{:?}:{},", captured.ty, captured.is_mutable));
        }
        key
    }

    /// Get the ValType for an environment struct reference
    pub(crate) fn env_struct_valtype(&self, struct_type_idx: u32) -> ValType {
        ValType::Ref(RefType {
            nullable: false,
            heap_type: HeapType::Concrete(struct_type_idx),
        })
    }
}
