/// Function Environment Handling with WASM GC Subtyping
///
/// This module manages environment struct types for closures using WASM GC subtyping.
///
/// Closure representation:
/// - Base struct type: Contains only the function pointer (field 0)
/// - Concrete struct types: Extend base and add captured variables (fields 1..N)
///
/// Environment struct layout:
/// - Field 0: function pointer (ref to function type) - enables indirect calls
/// - Fields 1..N: captured variables from parent scope
///
/// The base type enables type abstraction while concrete types provide access
/// to specific captured variables through downcasting.

use crate::ir::CapturedVarInfo;
use crate::types::Type;
use wasm_encoder::{CompositeInnerType, CompositeType, FieldType, HeapType, RefType, StorageType, StructType, SubType, ValType};

use super::super::WasmGenerator;

impl WasmGenerator {
    /// Get or create environment struct types for a function with captured variables
    ///
    /// Uses WASM GC subtyping:
    /// - Base struct contains only the function pointer (field 0)
    /// - Concrete struct extends base and adds captured variables (fields 1..N)
    ///
    /// Returns (base_type_idx, concrete_type_idx)
    pub(crate) fn get_or_create_env_struct_type(
        &mut self,
        func_name: &str,
        param_types: &[Type],
        return_type: &Type,
        captured_vars: &[CapturedVarInfo],
        _: u32,
    ) -> (u32, u32) {
        // Generate a canonical key based on the structure, not the function name
        // This allows reusing the same type for different functions with identical environments
        let struct_key = self.make_env_struct_key(captured_vars);

        // Check if we already have this exact struct type
        if let Some(&(closure_func_type_idx, base_type_idx, concrete_type_idx)) = self.env_struct_type_cache.get(&struct_key) {
            // Cache the mapping for this function name too
            self.env_struct_types.insert(func_name.to_string(), (closure_func_type_idx, base_type_idx, concrete_type_idx));
            return (base_type_idx, concrete_type_idx);
        }

        // Get or create the base environment type and closure function type
        // These are created together as mutually recursive types
        let (closure_func_type_idx, base_type_idx) = self.get_or_create_base_env_type(param_types, return_type);

        // Build struct fields for concrete type
        let mut fields = Vec::new();

        // Field 0: function pointer (ref to closure function type WITH environment)
        let func_ref_type = ValType::Ref(RefType {
            nullable: false,
            heap_type: HeapType::Concrete(closure_func_type_idx),
        });
        fields.push(FieldType {
            element_type: StorageType::Val(func_ref_type),
            mutable: false,
        });

        // Fields 1..N: captured variables
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

        // Create concrete struct type as subtype of base
        let concrete_type_idx = self.type_count;
        self.types.ty().subtype(&SubType {
            is_final: false,
            supertype_idx: Some(base_type_idx),
            composite_type: CompositeType {
                inner: CompositeInnerType::Struct(StructType {
                    fields: fields.into_boxed_slice(),
                }),
                shared: false,
            },
        });
        self.type_count += 1;

        // Cache by structure key (for reuse) - store all three type indices
        self.env_struct_type_cache.insert(struct_key, (closure_func_type_idx, base_type_idx, concrete_type_idx));
        // Cache by function name (for lookup)
        self.env_struct_types.insert(func_name.to_string(), (closure_func_type_idx, base_type_idx, concrete_type_idx));

        (base_type_idx, concrete_type_idx)
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
