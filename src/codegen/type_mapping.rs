use crate::ast::{Param, ParamKind, Type};
use wasm_encoder::{HeapType, RefType, StorageType, ValType};

use super::WasmGenerator;

impl WasmGenerator {
    pub(crate) fn wasm_valtype(&mut self, t: &Type) -> ValType {
        match t {
            Type::Int | Type::Bool | Type::Char => ValType::I32,
            Type::Float => ValType::F32,
            Type::Double => ValType::F64,
            // #TODO: Shouldn't be ANYREF
            Type::List | Type::VarArgs | Type::Any | Type::Function { .. } => {
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
                    let index = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.array_type_i32 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::F32) => {
                if let Some(idx) = self.array_type_f32 {
                    idx
                } else {
                    let index = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.array_type_f32 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::F64) => {
                if let Some(idx) = self.array_type_f64 {
                    idx
                } else {
                    let index = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
                    self.array_type_f64 = Some(index);
                    index
                }
            }
            StorageType::Val(ValType::Ref(rt)) if *rt == RefType::ANYREF => {
                if let Some(idx) = self.array_type_anyref {
                    idx
                } else {
                    let index = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        ty.array(storage, true);
                    }
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
