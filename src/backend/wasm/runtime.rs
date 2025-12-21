//! WASM runtime helper functions
//!
//! Generates runtime helper functions that support the compiled WASM code,
//! such as __print_string, __int_to_string, and vector operation helpers.
//! These are internal runtime functions, not directly callable from source code.

use wasm_encoder::{BlockType, Function, HeapType, Instruction, RefType, StorageType, ValType};
use super::super::WasmGenerator;

impl WasmGenerator {
    /// Include built-in runtime functions that can be used throughout the compiled WASM
    pub fn include_builtins(&mut self) {
        // Print/string helpers
        self.gen_print_string_helper();
        self.gen_int_to_string_helper();

        // Vector operation builtins
        let storage = StorageType::Val(ValType::I32);
        let int_vec_type = self.ensure_array_type(&storage);
        self.gen_vector_add_builtin(int_vec_type);

        // Add more built-in functions as needed
        // self.gen_vector_sub_builtin(int_vec_type);
        // self.gen_vector_mul_builtin(int_vec_type);
    }

    /// Generate the print_string helper: (ptr: i32, len: i32) -> void
    /// Prints a string to stdout using WASI fd_write
    fn gen_print_string_helper(&mut self) {
        // Function signature: (ptr: i32, len: i32) -> void
        let type_idx = self.type_count;
        self.types
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![]);
        self.type_count += 1;

        self.functions.function(type_idx as u32);
        let func_idx = self.func_count;
        self.func_indices.insert("__print_string".to_string(), func_idx);
        self.func_count += 1;

        // Memory layout:
        // 1024-1031: iovec structure (ptr, len)
        // 1032-1035: nwritten return value
        const IOVEC_PTR: i32 = 1024;
        const NWRITTEN_PTR: i32 = 1032;

        let mut func = Function::new(vec![]);

        // Setup iovec structure
        // iovec.ptr = param[0] (ptr)
        func.instruction(&Instruction::I32Const(IOVEC_PTR));
        func.instruction(&Instruction::LocalGet(0)); // ptr parameter
        func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));

        // iovec.len = param[1] (len)
        func.instruction(&Instruction::I32Const(IOVEC_PTR + 4));
        func.instruction(&Instruction::LocalGet(1)); // len parameter
        func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));

        // Call fd_write(1, iovec_ptr, 1, nwritten_ptr)
        func.instruction(&Instruction::I32Const(1)); // stdout
        func.instruction(&Instruction::I32Const(IOVEC_PTR));
        func.instruction(&Instruction::I32Const(1)); // 1 iovec
        func.instruction(&Instruction::I32Const(NWRITTEN_PTR));

        if let Some(fd_write_idx) = self.fd_write_idx {
            func.instruction(&Instruction::Call(fd_write_idx));
            func.instruction(&Instruction::Drop); // Drop fd_write return value
        }

        func.instruction(&Instruction::End);
        self.code.function(&func);
    }

    /// Generate the int_to_string helper: (num: i32) -> (ptr: i32, len: i32)
    /// Converts an integer to a string in memory and returns pointer + length
    fn gen_int_to_string_helper(&mut self) {
        // Function signature: (num: i32) -> (ptr: i32, len: i32)
        let type_idx = self.type_count;
        self.types
            .ty()
            .function(vec![ValType::I32], vec![ValType::I32, ValType::I32]);
        self.type_count += 1;

        self.functions.function(type_idx as u32);
        let func_idx = self.func_count;
        self.func_indices
            .insert("__int_to_string".to_string(), func_idx);
        self.func_count += 1;

        // Memory layout:
        // 0-11: string buffer (max 11 chars for i32: "-2147483648")
        const BUFFER_START: i32 = 0;
        const BUFFER_END: i32 = 11;

        // Local variables:
        // 0: num (parameter)
        // 1: is_negative (i32)
        // 2: write_pos (i32)
        // 3: digit_count (i32)
        // 4: working_num (i32)
        let mut func = Function::new(vec![
            (1, ValType::I32), // is_negative
            (1, ValType::I32), // write_pos
            (1, ValType::I32), // digit_count
            (1, ValType::I32), // working_num
        ]);

        let num_local = 0;
        let is_negative_local = 1;
        let write_pos_local = 2;
        let digit_count_local = 3;
        let working_num_local = 4;

        // working_num = num
        func.instruction(&Instruction::LocalGet(num_local));
        func.instruction(&Instruction::LocalSet(working_num_local));

        // Check if negative
        func.instruction(&Instruction::LocalGet(num_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::I32LtS);
        func.instruction(&Instruction::LocalSet(is_negative_local));

        // If negative, negate the number
        func.instruction(&Instruction::Block(BlockType::Empty));
        func.instruction(&Instruction::LocalGet(is_negative_local));
        func.instruction(&Instruction::I32Eqz);
        func.instruction(&Instruction::BrIf(0));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalGet(working_num_local));
        func.instruction(&Instruction::I32Sub);
        func.instruction(&Instruction::LocalSet(working_num_local));
        func.instruction(&Instruction::End);

        // Initialize write_pos to end of buffer
        func.instruction(&Instruction::I32Const(BUFFER_END));
        func.instruction(&Instruction::LocalSet(write_pos_local));

        // Initialize digit_count to 0
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(digit_count_local));

        // Convert digits (right to left)
        func.instruction(&Instruction::Block(BlockType::Empty));
        func.instruction(&Instruction::Loop(BlockType::Empty));

        // write_pos--
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Sub);
        func.instruction(&Instruction::LocalTee(write_pos_local));

        // digit = working_num % 10
        func.instruction(&Instruction::LocalGet(working_num_local));
        func.instruction(&Instruction::I32Const(10));
        func.instruction(&Instruction::I32RemS);

        // digit_char = '0' + digit
        func.instruction(&Instruction::I32Const(48)); // '0'
        func.instruction(&Instruction::I32Add);

        // memory[write_pos] = digit_char
        func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));

        // digit_count++
        func.instruction(&Instruction::LocalGet(digit_count_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(digit_count_local));

        // working_num = working_num / 10
        func.instruction(&Instruction::LocalGet(working_num_local));
        func.instruction(&Instruction::I32Const(10));
        func.instruction(&Instruction::I32DivS);
        func.instruction(&Instruction::LocalTee(working_num_local));

        // Continue loop if working_num != 0
        func.instruction(&Instruction::BrIf(0));
        func.instruction(&Instruction::End); // end loop
        func.instruction(&Instruction::End); // end block

        // If negative, add '-' sign
        func.instruction(&Instruction::Block(BlockType::Empty));
        func.instruction(&Instruction::LocalGet(is_negative_local));
        func.instruction(&Instruction::I32Eqz);
        func.instruction(&Instruction::BrIf(0));

        // write_pos--
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Sub);
        func.instruction(&Instruction::LocalTee(write_pos_local));

        // memory[write_pos] = '-'
        func.instruction(&Instruction::I32Const(45)); // '-'
        func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));

        // digit_count++
        func.instruction(&Instruction::LocalGet(digit_count_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(digit_count_local));

        func.instruction(&Instruction::End);

        // Return (ptr, len)
        // Push write_pos (start of string)
        func.instruction(&Instruction::LocalGet(write_pos_local));

        // Push digit_count (length of string)
        func.instruction(&Instruction::LocalGet(digit_count_local));

        func.instruction(&Instruction::End);
        self.code.function(&func);
    }

    fn gen_vector_add_builtin(&mut self, int_vec_type: u32) {
        // Define the function type: (ref array, ref array) -> ref array
        let vector_val_type = ValType::Ref(RefType {
            nullable: false,
            heap_type: HeapType::Concrete(int_vec_type),
        });
        let type_index = self.type_count;
        {
            let ty = self.types.ty();
            ty.function(
                vec![
                    vector_val_type,
                    vector_val_type,
                ],
                vec![vector_val_type],
            );
        }
        self.type_count += 1;

        // Register the function
        self.functions.function(type_index);
        let func_idx = self.func_count;
        self.func_indices.insert("vector_add".to_string(), func_idx);
        self.func_count += 1;

        // Define local variables:
        // 0, 1 = params (arr1, arr2)
        // 2 = i (counter)
        // 3 = n (length)
        // 4 = tmp (temporary for addition result)
        // 5 = result_arr
        let locals = vec![
            (1, ValType::I32), // i
            (1, ValType::I32), // n
            (1, ValType::I32), // tmp
            (1, vector_val_type), // result_arr
        ];

        let mut func = Function::new(locals);

        // Get array length from arr1
        func.instruction(&Instruction::LocalGet(0)); // arr1
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(3)); // n = arr1.length

        // Allocate result array: array.new_default $int_vec n
        func.instruction(&Instruction::LocalGet(3)); // n
        func.instruction(&Instruction::ArrayNewDefault(int_vec_type));
        func.instruction(&Instruction::LocalSet(5)); // result_arr

        // Initialize i = 0
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(2)); // i = 0

        // block $exit
        func.instruction(&Instruction::Block(BlockType::Empty));
        {
            // loop $loop
            func.instruction(&Instruction::Loop(BlockType::Empty));
            {
                // if i >= n: br $exit
                func.instruction(&Instruction::LocalGet(2)); // i
                func.instruction(&Instruction::LocalGet(3)); // n
                func.instruction(&Instruction::I32GeU);
                func.instruction(&Instruction::BrIf(1)); // break to $exit

                // tmp = arr1[i] + arr2[i]
                func.instruction(&Instruction::LocalGet(0)); // arr1
                func.instruction(&Instruction::LocalGet(2)); // i
                func.instruction(&Instruction::ArrayGet(int_vec_type)); // arr1[i]

                func.instruction(&Instruction::LocalGet(1)); // arr2
                func.instruction(&Instruction::LocalGet(2)); // i
                func.instruction(&Instruction::ArrayGet(int_vec_type)); // arr2[i]

                func.instruction(&Instruction::I32Add); // arr1[i] + arr2[i]
                func.instruction(&Instruction::LocalSet(4)); // tmp

                // result_arr[i] = tmp
                func.instruction(&Instruction::LocalGet(5)); // result_arr
                func.instruction(&Instruction::LocalGet(2)); // i
                func.instruction(&Instruction::LocalGet(4)); // tmp
                func.instruction(&Instruction::ArraySet(int_vec_type));

                // i++
                func.instruction(&Instruction::LocalGet(2)); // i
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(2)); // i = i + 1

                // Continue loop
                func.instruction(&Instruction::Br(0)); // br $loop
            }
            func.instruction(&Instruction::End); // end loop
        }
        func.instruction(&Instruction::End); // end block

        // Return result_arr
        func.instruction(&Instruction::LocalGet(5));
        func.instruction(&Instruction::End);

        self.code.function(&func);
    }

   
    fn gen_vector_mul_builtin(&mut self, int_vec_type: u32) {
        let type_index = self.type_count;
        {
            let ty = self.types.ty();
            ty.function(
                vec![
                    ValType::Ref(RefType::ARRAYREF),
                    ValType::Ref(RefType::ARRAYREF),
                ],
                vec![ValType::Ref(RefType::ARRAYREF)],
            );
        }

        self.functions.function(type_index);
        let func_idx = self.func_count;
        self.func_indices.insert("vector_mul".to_string(), func_idx);
        self.func_count += 1;

        let locals = vec![
            (1, ValType::I32),
            (1, ValType::I32),
            (1, ValType::I32),
            (1, ValType::Ref(RefType::ARRAYREF)),
        ];

        let mut func = Function::new(locals);

        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(3));

        func.instruction(&Instruction::LocalGet(3));
        func.instruction(&Instruction::ArrayNewDefault(int_vec_type));
        func.instruction(&Instruction::LocalSet(5));

        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(2));

        func.instruction(&Instruction::Block(BlockType::Empty));
        func.instruction(&Instruction::Loop(BlockType::Empty));

        func.instruction(&Instruction::LocalGet(2));
        func.instruction(&Instruction::LocalGet(3));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));

        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::LocalGet(2));
        func.instruction(&Instruction::ArrayGet(int_vec_type));

        func.instruction(&Instruction::LocalGet(1));
        func.instruction(&Instruction::LocalGet(2));
        func.instruction(&Instruction::ArrayGet(int_vec_type));

        func.instruction(&Instruction::I32Mul); // Changed to Mul
        func.instruction(&Instruction::LocalSet(4));

        func.instruction(&Instruction::LocalGet(5));
        func.instruction(&Instruction::LocalGet(2));
        func.instruction(&Instruction::LocalGet(4));
        func.instruction(&Instruction::ArraySet(int_vec_type));

        func.instruction(&Instruction::LocalGet(2));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(2));

        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End);
        func.instruction(&Instruction::End);

        func.instruction(&Instruction::LocalGet(5));
        func.instruction(&Instruction::End);

        self.code.function(&func);
    }
}