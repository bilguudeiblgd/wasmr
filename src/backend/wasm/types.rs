use crate::types::{Param, ParamKind, Type};
use wasm_encoder::{HeapType, RefType, StorageType, ValType};

use super::super::WasmGenerator;

impl WasmGenerator {
    pub(crate) fn wasm_valtype(&mut self, t: &Type) -> ValType {
        match t {
            Type::Int | Type::Bool | Type::Char => ValType::I32,
            Type::Float => ValType::F32,
            Type::Double => ValType::F64,
            // For function types, check if it's a closure or bare function
            Type::Function { params, return_type } => {
                // Extract parameter types from Param structs
                let param_types: Vec<Type> = params
                    .iter()
                    .filter_map(|p| match &p.kind {
                        ParamKind::Normal(ty) => Some(ty.clone()),
                        ParamKind::VarArgs => None, // Skip varargs for now
                    })
                    .collect();

                // Check if this signature corresponds to a closure
                if let Some((_closure_func_idx, base_type_idx)) =
                    self.get_closure_info_for_signature(&param_types, return_type)
                {
                    // This is a closure - return environment struct type
                    ValType::Ref(RefType {
                        nullable: false,
                        heap_type: HeapType::Concrete(base_type_idx),
                    })
                } else {
                    // Bare function - return funcref
                    let type_idx = self.get_or_create_func_type_index(&param_types, return_type);
                    ValType::Ref(RefType {
                        nullable: false,
                        heap_type: HeapType::Concrete(type_idx),
                    })
                }
            }
            // #TODO: Shouldn't be ANYREF
            Type::List | Type::VarArgs | Type::Any => {
                ValType::Ref(RefType::ANYREF)
            }
            Type::Vector(inner_ty) => {
                // Get the storage type for the inner type

                let storage = self.storage_type_for(inner_ty);
                // Ensure the array type exists and get its index
                let array_type_idx = self.ensure_array_type(&storage);
                // Return a concrete reference to that specific array type
                ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Concrete(array_type_idx),
                })
            }

            _ => ValType::I32,
        }
    }

    pub(crate) fn wasm_param_valtype(&mut self, param: &Param) -> ValType {
        match &param.kind {
            ParamKind::Normal(ty) => self.wasm_valtype(ty),
            ParamKind::VarArgs => ValType::Ref(RefType::ANYREF),
        }
    }

    pub(crate) fn storage_type_for(&self, ty: &Type) -> StorageType {
        match ty {
            Type::Int | Type::Bool | Type::Char | Type::String => StorageType::Val(ValType::I32),
            Type::Float => StorageType::Val(ValType::F32),
            Type::Double => StorageType::Val(ValType::F64),
            Type::Vector(_) | Type::List | Type::VarArgs | Type::Any | Type::Function { .. } => {
                StorageType::Val(ValType::Ref(RefType::ANYREF))
            }
            _ => StorageType::Val(ValType::I32),
        }
    }

    pub(crate) fn ensure_array_type(&mut self, storage: &StorageType) -> u32 {
        match storage {
            StorageType::Val(ValType::I32) => {
                if let Some(idx) = self.array_type_i32 {
                    idx
                } else {
                    let index = self.type_count;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.type_count += 1;
                    self.array_type_i32 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::F32) => {
                if let Some(idx) = self.array_type_f32 {
                    idx
                } else {
                    let index = self.type_count;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.type_count += 1;
                    self.array_type_f32 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::F64) => {
                if let Some(idx) = self.array_type_f64 {
                    idx
                } else {
                    let index = self.type_count;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.type_count += 1;
                    self.array_type_f64 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::Ref(rt)) if *rt == RefType::ANYREF => {
                if let Some(idx) = self.array_type_anyref {
                    idx
                } else {
                    let index = self.type_count;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.type_count += 1;
                    self.array_type_anyref = Some(index);
                    index
                }
            }
            _ => {
                // Fallback: treat as i32 storage
                self.ensure_array_type(&StorageType::Val(ValType::I32))
            }
        }
    }
}
