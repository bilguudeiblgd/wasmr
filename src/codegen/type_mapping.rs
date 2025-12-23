use crate::ast::{Param, ParamKind, Type};
use wasm_encoder::{HeapType, RefType, StorageType, ValType, FieldType};

use super::WasmGenerator;

impl WasmGenerator {
    pub(crate) fn wasm_valtype(&mut self, t: &Type) -> ValType {
        match t {
            Type::Int | Type::Bool | Type::Char => ValType::I32,
            Type::Float => ValType::F32,
            Type::Double => ValType::F64,
            // #TODO: Shouldn't be ANYREF
            Type::List | Type::VarArgs | Type::Any | Type::FunctionRef => {
                ValType::Ref(RefType::ANYREF)
            }
            Type::Vector(inner_ty) => {
                // Get the storage type for the inner type
                let storage = self.storage_type_for(inner_ty);
                // Ensure the vector struct type exists and get its index
                let struct_type_idx = self.ensure_vector_struct_type(&storage);
                // Return a concrete reference to that struct type
                ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Concrete(struct_type_idx),
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
            Type::Vector(_) | Type::List | Type::VarArgs | Type::Any | Type::FunctionRef => {
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

    /// Ensure a vector struct type exists for the given element storage type.
    /// Returns the type index of the struct: (struct (field data (array T)) (field length i32))
    pub(crate) fn ensure_vector_struct_type(&mut self, storage: &StorageType) -> u32 {
        match storage {
            StorageType::Val(ValType::I32) => {
                if let Some(idx) = self.vector_struct_i32 {
                    idx
                } else {
                    let array_type_idx = self.ensure_array_type(storage);
                    let struct_idx = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        let fields = vec![
                            // Field 0: data - array of elements
                            FieldType {
                                element_type: StorageType::Val(ValType::Ref(RefType {
                                    nullable: false,
                                    heap_type: HeapType::Concrete(array_type_idx),
                                })),
                                mutable: false,
                            },
                            // Field 1: length - i32
                            FieldType {
                                element_type: StorageType::Val(ValType::I32),
                                mutable: false,
                            },
                        ];
                        ty.struct_(fields);
                    }
                    self.vector_struct_i32 = Some(struct_idx);
                    struct_idx
                }
            }
            StorageType::Val(ValType::F32) => {
                if let Some(idx) = self.vector_struct_f32 {
                    idx
                } else {
                    let array_type_idx = self.ensure_array_type(storage);
                    let struct_idx = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        let fields = vec![
                            FieldType {
                                element_type: StorageType::Val(ValType::Ref(RefType {
                                    nullable: false,
                                    heap_type: HeapType::Concrete(array_type_idx),
                                })),
                                mutable: false,
                            },
                            FieldType {
                                element_type: StorageType::Val(ValType::I32),
                                mutable: false,
                            },
                        ];
                        ty.struct_(fields);
                    }
                    self.vector_struct_f32 = Some(struct_idx);
                    struct_idx
                }
            }
            StorageType::Val(ValType::F64) => {
                if let Some(idx) = self.vector_struct_f64 {
                    idx
                } else {
                    let array_type_idx = self.ensure_array_type(storage);
                    let struct_idx = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        let fields = vec![
                            FieldType {
                                element_type: StorageType::Val(ValType::Ref(RefType {
                                    nullable: false,
                                    heap_type: HeapType::Concrete(array_type_idx),
                                })),
                                mutable: false,
                            },
                            FieldType {
                                element_type: StorageType::Val(ValType::I32),
                                mutable: false,
                            },
                        ];
                        ty.struct_(fields);
                    }
                    self.vector_struct_f64 = Some(struct_idx);
                    struct_idx
                }
            }
            StorageType::Val(ValType::Ref(rt)) if *rt == RefType::ANYREF => {
                if let Some(idx) = self.vector_struct_anyref {
                    idx
                } else {
                    let array_type_idx = self.ensure_array_type(storage);
                    let struct_idx = self.types.len() as u32;
                    {
                        let ty = self.types.ty();
                        let fields = vec![
                            FieldType {
                                element_type: StorageType::Val(ValType::Ref(RefType {
                                    nullable: false,
                                    heap_type: HeapType::Concrete(array_type_idx),
                                })),
                                mutable: false,
                            },
                            FieldType {
                                element_type: StorageType::Val(ValType::I32),
                                mutable: false,
                            },
                        ];
                        ty.struct_(fields);
                    }
                    self.vector_struct_anyref = Some(struct_idx);
                    struct_idx
                }
            }
            _ => {
                // Fallback: treat as i32 storage
                self.ensure_vector_struct_type(&StorageType::Val(ValType::I32))
            }
        }
    }
}
