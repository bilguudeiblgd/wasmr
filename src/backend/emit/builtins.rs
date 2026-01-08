//! IR builtin call emission
//!
//! Handles code generation for built-in function calls from the IR,
//! such as c(), print(), and list(). These are user-facing built-in
//! functions that appear in the source code.

use crate::ir::{BuiltinKind, IRExpr};
use wasm_encoder::{Function, HeapType, Instruction, StorageType, ValType};

use super::super::{context::LocalContext, WasmGenerator};

impl WasmGenerator {
    pub(crate) fn compile_builtin_call(
        &mut self,
        func: &mut Function,
        ctx: &LocalContext,
        builtin: &BuiltinKind,
        args: &[IRExpr],
    ) {
        match builtin {
            BuiltinKind::Print => {
                // Support printing int, float, and double
                if args.len() != 1 {
                    return;
                }

                // Evaluate the argument (puts value on stack)
                self.gen_expr(func, ctx, &args[0]);

                // Call appropriate to_string helper based on type
                use crate::types::Type;
                let to_string_idx = match &args[0].ty {
                    Type::Int => self
                        .func_indices
                        .get("__int_to_string")
                        .expect("__int_to_string helper not found"),
                    Type::Double => self
                        .func_indices
                        .get("__double_to_string")
                        .expect("__double_to_string helper not found"),
                    Type::Logical => self
                        .func_indices
                        .get("__bool_to_string")
                        .expect("__bool_to_string helper not found"),
                    _ => {
                        // Unsupported type - just drop and return
                        func.instruction(&Instruction::Drop);
                        return;
                    }
                };

                func.instruction(&Instruction::Call(*to_string_idx));

                // Stack now has: [ptr, len]
                // Call print_string helper: (ptr: i32, len: i32) -> void
                let print_string_idx = self
                    .func_indices
                    .get("__print_string")
                    .expect("__print_string helper not found");
                func.instruction(&Instruction::Call(*print_string_idx));

                // Print newline
                // Store newline at a known location (offset 12)
                const NEWLINE_PTR: i32 = 12;
                func.instruction(&Instruction::I32Const(NEWLINE_PTR));
                func.instruction(&Instruction::I32Const(10)); // '\n'
                func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));

                // Call print_string for newline
                func.instruction(&Instruction::I32Const(NEWLINE_PTR));
                func.instruction(&Instruction::I32Const(1)); // length = 1
                func.instruction(&Instruction::Call(*print_string_idx));

                // print() is void - no return value needed
            }
            BuiltinKind::Length => {
                // length() extracts the length field (field 1) from the vector struct
                if args.len() != 1 {
                    return;
                }

                // Generate the vector struct argument
                self.gen_expr(func, ctx, &args[0]);

                // Extract the length field (field 1) from the vector struct
                use crate::types::Type;
                let elem_ty = match &args[0].ty {
                    Type::Vector(inner) => &**inner,
                    _ => panic!("Type checker should ensure length() receives a vector"),
                };
                let vector_struct_index = self.ensure_vector_struct_type(elem_ty);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: vector_struct_index,
                    field_index: 1,  // length field
                });
            }
            BuiltinKind::C | BuiltinKind::List => {
                // These are handled by creating vector struct: (struct (field data) (field length))
                for arg in args {
                    self.gen_expr(func, ctx, arg);
                }
                if !args.is_empty() {
                    let element_ty = &args[0].ty;
                    let storage = self.storage_type_for(element_ty);
                    let array_type_index = self.ensure_array_type(&storage);

                    // Create the data array
                    func.instruction(&Instruction::ArrayNewFixed {
                        array_type_index,
                        array_size: args.len() as u32,
                    });

                    // Push length
                    func.instruction(&Instruction::I32Const(args.len() as i32));

                    // Create vector struct
                    let vector_struct_index = self.ensure_vector_struct_type(element_ty);
                    func.instruction(&Instruction::StructNew(vector_struct_index));
                } else {
                    // Empty vector - push null reference
                    func.instruction(&Instruction::RefNull(HeapType::ANY));
                }
            }
            BuiltinKind::Stop => {
                // stop() immediately halts execution with an error
                // We evaluate the argument (typically an error message string)
                // then use the unreachable instruction to trap

                if !args.is_empty() {
                    // Generate the argument (typically a string message)
                    self.gen_expr(func, ctx, &args[0]);
                    // Drop the message (we can't easily print it before trapping)
                    // In a more sophisticated implementation, we'd call an error handler
                    func.instruction(&Instruction::Drop);
                }

                // Trap execution immediately
                // This will cause the WASM runtime to halt with an error
                func.instruction(&Instruction::Unreachable);
            }
            BuiltinKind::Vector => {
                // vec(length: int, mode: string) creates a vector of given length and type
                // Returns a vector struct: (struct (field array_ref) (field length))

                if args.len() != 2 {
                    return;
                }

                // Extract element type from the mode parameter (second argument)
                // The mode is a string literal that was validated during type resolution
                use crate::types::Type;
                let elem_ty = match &args[1].kind {
                    crate::ir::IRExprKind::XString(mode) => match mode.as_str() {
                        "logical" => Type::Logical,
                        "int" | "integer" => Type::Int,
                        "double" | "numeric" => Type::Double,
                        _ => Type::Double, // Default fallback (should not happen due to validation)
                    },
                    _ => Type::Double, // Default fallback
                };

                // Get the vector struct type index
                let vector_struct_idx = self.ensure_vector_struct_type(&elem_ty);

                // Map element type to WASM storage type
                let storage = match elem_ty {
                    Type::Logical => StorageType::Val(ValType::I32),
                    Type::Int => StorageType::Val(ValType::I32),
                    Type::Double => StorageType::Val(ValType::F64),
                    _ => StorageType::Val(ValType::F64), // Fallback
                };
                let array_type_idx = self.ensure_array_type(&storage);

                // Step 1: Evaluate length and create the array
                // Stack: [] -> [array_ref]
                self.gen_expr(func, ctx, &args[0]);
                func.instruction(&Instruction::ArrayNewDefault(array_type_idx));

                // Step 2: Evaluate length again for the struct field
                // Stack: [array_ref] -> [array_ref, length]
                self.gen_expr(func, ctx, &args[0]);

                // Step 3: Create vector struct
                // Stack: [array_ref, length] -> [vector_struct_ref]
                func.instruction(&Instruction::StructNew(vector_struct_idx));
            }
            BuiltinKind::AsInt => {
                // as.integer(x) casts x to int (scalars emit instructions directly, vectors call runtime)
                if args.is_empty() {
                    return;
                }

                let arg = &args[0];
                use crate::types::Type;

                match &arg.ty {
                    Type::Int => {
                        // Already int - just generate the expression
                        self.gen_expr(func, ctx, arg);
                    }
                    Type::Double => {
                        // Scalar cast: double -> int
                        self.gen_expr(func, ctx, arg);
                        func.instruction(&Instruction::I32TruncSatF64S);
                    }
                    Type::Logical => {
                        // Logical -> int: already i32, just pass through
                        self.gen_expr(func, ctx, arg);
                    }
                    Type::Vector(elem_ty) => {
                        // Vector cast - call runtime function
                        let func_name = match elem_ty.as_ref() {
                            Type::Int => {
                                // Already vector<int> - just pass through
                                self.gen_expr(func, ctx, arg);
                                return;
                            }
                            Type::Double => "system_cast_vec_double_to_vec_int",
                            Type::Logical => "system_cast_vec_logical_to_vec_int",
                            _ => {
                                // Unsupported - should not happen due to type checking
                                self.gen_expr(func, ctx, arg);
                                return;
                            }
                        };

                        self.gen_expr(func, ctx, arg);
                        let func_idx = *self.func_indices.get(func_name)
                            .unwrap_or_else(|| panic!("Cast function {} not found in runtime", func_name));
                        func.instruction(&Instruction::Call(func_idx));
                    }
                    _ => {
                        // Unsupported type - should not happen due to type checking
                        self.gen_expr(func, ctx, arg);
                    }
                }
            }
            BuiltinKind::AsDouble => {
                // as.double(x) casts x to double (scalars emit instructions directly, vectors call runtime)
                if args.is_empty() {
                    return;
                }

                let arg = &args[0];
                use crate::types::Type;

                match &arg.ty {
                    Type::Double => {
                        // Already double - just generate the expression
                        self.gen_expr(func, ctx, arg);
                    }
                    Type::Int | Type::Logical => {
                        // Scalar cast: int/logical -> double
                        self.gen_expr(func, ctx, arg);
                        func.instruction(&Instruction::F64ConvertI32S);
                    }
                    Type::Vector(elem_ty) => {
                        // Vector cast - call runtime function
                        let func_name = match elem_ty.as_ref() {
                            Type::Double => {
                                // Already vector<double> - just pass through
                                self.gen_expr(func, ctx, arg);
                                return;
                            }
                            Type::Int => "system_cast_vec_int_to_vec_double",
                            Type::Logical => "system_cast_vec_logical_to_vec_double",
                            _ => {
                                // Unsupported - should not happen due to type checking
                                self.gen_expr(func, ctx, arg);
                                return;
                            }
                        };

                        self.gen_expr(func, ctx, arg);
                        let func_idx = *self.func_indices.get(func_name)
                            .unwrap_or_else(|| panic!("Cast function {} not found in runtime", func_name));
                        func.instruction(&Instruction::Call(func_idx));
                    }
                    _ => {
                        // Unsupported type - should not happen due to type checking
                        self.gen_expr(func, ctx, arg);
                    }
                }
            }
            BuiltinKind::AsLogical => {
                // as.logical(x) casts x to logical (scalars emit instructions directly, vectors call runtime)
                if args.is_empty() {
                    return;
                }

                let arg = &args[0];
                use crate::types::Type;

                match &arg.ty {
                    Type::Logical => {
                        // Already logical - just generate the expression
                        self.gen_expr(func, ctx, arg);
                    }
                    Type::Double => {
                        // Scalar cast: double -> logical (boolean)
                        self.gen_expr(func, ctx, arg);
                        func.instruction(&Instruction::F64Const(wasm_encoder::Ieee64::from(0.0)));
                        func.instruction(&Instruction::F64Ne);  // Compare not equal
                    }
                    Type::Int => {
                        // Scalar cast: int -> logical (boolean)
                        self.gen_expr(func, ctx, arg);
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Ne);  // Compare not equal
                    }
                    Type::Vector(elem_ty) => {
                        // Vector cast - call runtime function
                        let func_name = match elem_ty.as_ref() {
                            Type::Logical => {
                                // Already vector<logical> - just pass through
                                self.gen_expr(func, ctx, arg);
                                return;
                            }
                            Type::Int => "system_cast_vec_int_to_vec_logical",
                            Type::Double => "system_cast_vec_double_to_vec_logical",
                            _ => {
                                // Unsupported - should not happen due to type checking
                                self.gen_expr(func, ctx, arg);
                                return;
                            }
                        };

                        self.gen_expr(func, ctx, arg);
                        let func_idx = *self.func_indices.get(func_name)
                            .unwrap_or_else(|| panic!("Cast function {} not found in runtime", func_name));
                        func.instruction(&Instruction::Call(func_idx));
                    }
                    _ => {
                        // Unsupported type - should not happen due to type checking
                        self.gen_expr(func, ctx, arg);
                    }
                }
            }
            BuiltinKind::Sqrt => {
                // sqrt(x) computes square root using WASM f64.sqrt instruction
                // The lowering pass ensures the argument is already converted to Double
                if args.len() != 1 {
                    return;
                }

                // Generate the argument (already converted to Double by lowering)
                self.gen_expr(func, ctx, &args[0]);

                // Emit WASM f64.sqrt instruction
                func.instruction(&Instruction::F64Sqrt);
            }
        }
    }
}
