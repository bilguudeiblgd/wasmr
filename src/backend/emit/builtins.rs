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
                // vec(length: int) creates a vector of given length with default values
                // Returns a vector struct: (struct (field array_ref) (field length))

                if args.len() != 1 {
                    return;
                }

                // vec() returns Vector(Double) by default
                use crate::types::Type;
                let elem_ty = Type::Double;

                // Get the vector struct type index
                let vector_struct_idx = self.ensure_vector_struct_type(&elem_ty);

                // Get the array type index for the data field
                let storage = StorageType::Val(ValType::F64);
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
        }
    }
}
