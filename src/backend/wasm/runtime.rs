//! WASM runtime helper functions
//!
//! Generates runtime helper functions that support the compiled WASM code,
//! such as __print_string, __int_to_string, and vector operation helpers.
//! These are internal runtime functions, not directly callable from source code.

use wasm_encoder::{BlockType, Function,  Instruction, StorageType, ValType};
use super::super::WasmGenerator;

impl WasmGenerator {
    /// Include built-in runtime functions that can be used throughout the compiled WASM
    pub fn include_builtins(&mut self) {
        // Print/string helpers
        self.gen_print_string_helper();
        self.gen_int_to_string_helper();
        self.gen_float_to_string_helper();
        self.gen_double_to_string_helper();
        self.gen_bool_to_string_helper();

        // Vector operation builtins
        let storage = StorageType::Val(ValType::I32);
        let _ = self.ensure_array_type(&storage);

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
        const _BUFFER_START: i32 = 0;
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

    /// Generate the float_to_string helper: (num: f32) -> (ptr: i32, len: i32)
    /// Converts a float to a string in memory and returns pointer + length
    fn gen_float_to_string_helper(&mut self) {
        // Function signature: (num: f32) -> (ptr: i32, len: i32)
        let type_idx = self.type_count;
        self.types
            .ty()
            .function(vec![ValType::F32], vec![ValType::I32, ValType::I32]);
        self.type_count += 1;

        self.functions.function(type_idx as u32);
        let func_idx = self.func_count;
        self.func_indices
            .insert("__float_to_string".to_string(), func_idx);
        self.func_count += 1;

        // Memory layout: 50-100 for float string buffer (50 bytes should be enough)
        const BUFFER_START: i32 = 50;

        // Locals: integer_part(i32), decimal_part(i32), int_ptr(i32), int_len(i32),
        //         write_pos(i32), total_len(i32)
        let mut func = Function::new(vec![
            (1, ValType::I32), // integer_part
            (1, ValType::I32), // decimal_part (0-99)
            (1, ValType::I32), // int_ptr
            (1, ValType::I32), // int_len
            (1, ValType::I32), // write_pos
            (1, ValType::I32), // total_len
        ]);

        let num_local = 0;
        let integer_part_local = 1;
        let decimal_part_local = 2;
        let int_ptr_local = 3;
        let int_len_local = 4;
        let write_pos_local = 5;
        let total_len_local = 6;

        // Convert float to i32 (truncate) for integer part
        func.instruction(&Instruction::LocalGet(num_local));
        func.instruction(&Instruction::I32TruncF32S);
        func.instruction(&Instruction::LocalSet(integer_part_local));

        // Get decimal part: abs(num - integer_part) * 100, truncate to i32
        func.instruction(&Instruction::LocalGet(num_local));
        func.instruction(&Instruction::LocalGet(integer_part_local));
        func.instruction(&Instruction::F32ConvertI32S);
        func.instruction(&Instruction::F32Sub);
        func.instruction(&Instruction::F32Abs);  // Take absolute value
        func.instruction(&Instruction::F32Const(wasm_encoder::Ieee32::from(100.0)));
        func.instruction(&Instruction::F32Mul);
        func.instruction(&Instruction::I32TruncF32S);
        func.instruction(&Instruction::LocalSet(decimal_part_local));

        // Call __int_to_string for integer part
        func.instruction(&Instruction::LocalGet(integer_part_local));
        let int_to_str_idx = self.func_indices["__int_to_string"];
        func.instruction(&Instruction::Call(int_to_str_idx));

        // Stack has [ptr, len] - store them
        func.instruction(&Instruction::LocalSet(int_len_local));
        func.instruction(&Instruction::LocalSet(int_ptr_local));

        // Copy integer part to buffer at BUFFER_START
        // Use memory.copy: dest, src, len
        func.instruction(&Instruction::I32Const(BUFFER_START));
        func.instruction(&Instruction::LocalGet(int_ptr_local));
        func.instruction(&Instruction::LocalGet(int_len_local));
        func.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });

        // write_pos = BUFFER_START + int_len
        func.instruction(&Instruction::I32Const(BUFFER_START));
        func.instruction(&Instruction::LocalGet(int_len_local));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(write_pos_local));

        // Write decimal point '.'
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::I32Const(46)); // '.'
        func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));

        // write_pos++
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(write_pos_local));

        // Write first decimal digit (decimal_part / 10)
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::LocalGet(decimal_part_local));
        func.instruction(&Instruction::I32Const(10));
        func.instruction(&Instruction::I32DivU);
        func.instruction(&Instruction::I32Const(48)); // '0'
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));

        // write_pos++
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(write_pos_local));

        // Write second decimal digit (decimal_part % 10)
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::LocalGet(decimal_part_local));
        func.instruction(&Instruction::I32Const(10));
        func.instruction(&Instruction::I32RemU);
        func.instruction(&Instruction::I32Const(48)); // '0'
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));

        // total_len = int_len + 3 (decimal point + 2 digits)
        func.instruction(&Instruction::LocalGet(int_len_local));
        func.instruction(&Instruction::I32Const(3));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(total_len_local));

        // Return BUFFER_START and total_len
        func.instruction(&Instruction::I32Const(BUFFER_START));
        func.instruction(&Instruction::LocalGet(total_len_local));

        func.instruction(&Instruction::End);
        self.code.function(&func);
    }

    /// Generate the double_to_string helper: (num: f64) -> (ptr: i32, len: i32)
    /// Converts a double to a string in memory and returns pointer + length
    fn gen_double_to_string_helper(&mut self) {
        // Function signature: (num: f64) -> (ptr: i32, len: i32)
        let type_idx = self.type_count;
        self.types
            .ty()
            .function(vec![ValType::F64], vec![ValType::I32, ValType::I32]);
        self.type_count += 1;

        self.functions.function(type_idx as u32);
        let func_idx = self.func_count;
        self.func_indices
            .insert("__double_to_string".to_string(), func_idx);
        self.func_count += 1;

        // Memory layout: 100-150 for double string buffer
        const BUFFER_START: i32 = 100;

        // Locals: integer_part(i32), decimal_part(i32), ptr(i32), len(i32)
        let mut func = Function::new(vec![
            (1, ValType::I32), // integer_part
            (1, ValType::I32), // decimal_part
            (1, ValType::I32), // write_pos
            (1, ValType::I32), // len
        ]);

        let num_local = 0;
        let integer_part_local = 1;
        let decimal_part_local = 2;
        let write_pos_local = 3;
        let len_local = 4;

        // Convert double to i32 (truncate) for integer part
        func.instruction(&Instruction::LocalGet(num_local));
        func.instruction(&Instruction::I32TruncF64S);
        func.instruction(&Instruction::LocalSet(integer_part_local));

        // Get decimal part: (num - integer_part) * 100, truncate to i32
        func.instruction(&Instruction::LocalGet(num_local));
        func.instruction(&Instruction::LocalGet(integer_part_local));
        func.instruction(&Instruction::F64ConvertI32S);
        func.instruction(&Instruction::F64Sub);
        func.instruction(&Instruction::F64Const(wasm_encoder::Ieee64::from(100.0)));
        func.instruction(&Instruction::F64Mul);
        func.instruction(&Instruction::I32TruncF64S);
        func.instruction(&Instruction::LocalSet(decimal_part_local));

        // Call __int_to_string for integer part
        func.instruction(&Instruction::LocalGet(integer_part_local));
        let int_to_str_idx = self.func_indices["__int_to_string"];
        func.instruction(&Instruction::Call(int_to_str_idx));

        // Stack now has [ptr, len] from int_to_string
        // Store len temporarily
        func.instruction(&Instruction::LocalSet(len_local));
        // Store ptr as our write position (source ptr from int_to_string)
        func.instruction(&Instruction::LocalSet(write_pos_local));

        // Copy integer part to our buffer using memory.copy
        // Dest: BUFFER_START, Source: write_pos, Length: len
        func.instruction(&Instruction::I32Const(BUFFER_START));
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });

        // Calculate position for decimal point: BUFFER_START + len
        func.instruction(&Instruction::I32Const(BUFFER_START));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalTee(write_pos_local)); // Store and keep on stack

        // Write '.' (ASCII 46)
        func.instruction(&Instruction::I32Const(46));
        func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));

        // Write first decimal digit: (abs(decimal_part) / 10) + '0'
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(write_pos_local));

        // Get abs(decimal_part)
        func.instruction(&Instruction::LocalGet(decimal_part_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::I32LtS); // decimal_part < 0?
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        // If negative, negate it
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalGet(decimal_part_local));
        func.instruction(&Instruction::I32Sub);
        func.instruction(&Instruction::LocalSet(decimal_part_local));
        func.instruction(&Instruction::End);

        // First digit: (decimal_part / 10) + '0'
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::LocalGet(decimal_part_local));
        func.instruction(&Instruction::I32Const(10));
        func.instruction(&Instruction::I32DivS);
        func.instruction(&Instruction::I32Const(48)); // '0'
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));

        // Write second decimal digit: (decimal_part % 10) + '0'
        func.instruction(&Instruction::LocalGet(write_pos_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);

        func.instruction(&Instruction::LocalGet(decimal_part_local));
        func.instruction(&Instruction::I32Const(10));
        func.instruction(&Instruction::I32RemS);
        func.instruction(&Instruction::I32Const(48)); // '0'
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));

        // Return BUFFER_START and total length (int_len + 3 for ".xx")
        func.instruction(&Instruction::I32Const(BUFFER_START));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::I32Const(3)); // "." + 2 decimal digits
        func.instruction(&Instruction::I32Add);

        func.instruction(&Instruction::End);
        self.code.function(&func);
    }

    fn gen_bool_to_string_helper(&mut self) {
        // Function signature: (val: i32) -> (ptr: i32, len: i32)
        let type_idx = self.type_count;
        self.types
            .ty()
            .function(vec![ValType::I32], vec![ValType::I32, ValType::I32]);
        self.type_count += 1;

        self.functions.function(type_idx as u32);
        let func_idx = self.func_count;
        self.func_indices
            .insert("__bool_to_string".to_string(), func_idx);
        self.func_count += 1;

        // Memory layout:
        // offset 16-19: "TRUE" (4 bytes)
        // offset 20-24: "FALSE" (5 bytes)
        const TRUE_OFFSET: i32 = 16;
        const FALSE_OFFSET: i32 = 20;
        const TRUE_LEN: i32 = 4;
        const FALSE_LEN: i32 = 5;

        let mut func = Function::new(vec![]);

        let val_param = 0;

        // if (val == 0) then "FALSE" else "TRUE"
        // First, push the pointer
        func.instruction(&Instruction::LocalGet(val_param));
        func.instruction(&Instruction::I32Eqz); // Check if 0 (false)

        func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
        // False branch: return FALSE_OFFSET
        func.instruction(&Instruction::I32Const(FALSE_OFFSET));
        func.instruction(&Instruction::Else);
        // True branch: return TRUE_OFFSET
        func.instruction(&Instruction::I32Const(TRUE_OFFSET));
        func.instruction(&Instruction::End);

        // Now push the length based on the same condition
        func.instruction(&Instruction::LocalGet(val_param));
        func.instruction(&Instruction::I32Eqz);

        func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(ValType::I32)));
        // False branch: return FALSE_LEN
        func.instruction(&Instruction::I32Const(FALSE_LEN));
        func.instruction(&Instruction::Else);
        // True branch: return TRUE_LEN
        func.instruction(&Instruction::I32Const(TRUE_LEN));
        func.instruction(&Instruction::End);

        func.instruction(&Instruction::End);
        self.code.function(&func);
    }

}