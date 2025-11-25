use crate::ir::{BuiltinKind, IRExpr};
use wasm_encoder::{BlockType, Function, HeapType, Instruction};

use super::{local_context::LocalContext, WasmGenerator};

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
                // Only support printing integers for now
                if args.len() != 1 {
                    return;
                }

                // Memory layout:
                // 0-11: buffer for integer-to-string conversion (max 11 chars for i32)
                // 12: newline character
                // 16-23: iovec[0] structure (ptr, len) for number
                // 24-31: iovec[1] structure (ptr, len) for newline
                // 32-35: nwritten return value

                const BUFFER_PTR: i32 = 0;
                const NEWLINE_PTR: i32 = 12;
                const IOVEC_PTR: i32 = 16;
                const NWRITTEN_PTR: i32 = 32;

                // Store newline character at position 12
                func.instruction(&Instruction::I32Const(NEWLINE_PTR));
                func.instruction(&Instruction::I32Const(10)); // '\n'
                func.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));

                // Evaluate the argument (puts integer on stack)
                self.gen_expr(func, ctx, &args[0]);

                // Convert integer to string and store in memory
                // We'll use a simple algorithm: repeatedly divide by 10
                // Get local indices for temporary variables
                let num_local = ctx.get_local("__print_num").expect("__print_num local not found");
                let is_negative_local = ctx.get_local("__print_is_negative").expect("__print_is_negative local not found");
                let write_pos_local = ctx.get_local("__print_write_pos").expect("__print_write_pos local not found");
                let digit_count_local = ctx.get_local("__print_digit_count").expect("__print_digit_count local not found");

                // num = value from stack
                func.instruction(&Instruction::LocalSet(num_local));

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
                func.instruction(&Instruction::LocalGet(num_local));
                func.instruction(&Instruction::I32Sub);
                func.instruction(&Instruction::LocalSet(num_local));
                func.instruction(&Instruction::End);

                // Initialize write_pos to end of buffer (BUFFER_PTR + 11)
                func.instruction(&Instruction::I32Const(BUFFER_PTR + 11));
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

                // digit = num % 10
                func.instruction(&Instruction::LocalGet(num_local));
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

                // num = num / 10
                func.instruction(&Instruction::LocalGet(num_local));
                func.instruction(&Instruction::I32Const(10));
                func.instruction(&Instruction::I32DivS);
                func.instruction(&Instruction::LocalTee(num_local));

                // Continue loop if num != 0 (br 0 continues the loop)
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

                // Setup first iovec (number string)
                // iovec[0].ptr = write_pos
                func.instruction(&Instruction::I32Const(IOVEC_PTR));
                func.instruction(&Instruction::LocalGet(write_pos_local));
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                // iovec[0].len = digit_count
                func.instruction(&Instruction::I32Const(IOVEC_PTR + 4));
                func.instruction(&Instruction::LocalGet(digit_count_local));
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                // Call fd_write for the number (1 iovec)
                func.instruction(&Instruction::I32Const(1)); // stdout
                func.instruction(&Instruction::I32Const(IOVEC_PTR));
                func.instruction(&Instruction::I32Const(1)); // 1 iovec (just the number)
                func.instruction(&Instruction::I32Const(NWRITTEN_PTR));

                if let Some(fd_write_idx) = self.fd_write_idx {
                    func.instruction(&Instruction::Call(fd_write_idx));
                    func.instruction(&Instruction::Drop);
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }

                // Setup second iovec (newline)
                func.instruction(&Instruction::I32Const(IOVEC_PTR));
                func.instruction(&Instruction::I32Const(NEWLINE_PTR));
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                func.instruction(&Instruction::I32Const(IOVEC_PTR + 4));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));

                // Call fd_write for the newline (1 iovec)
                func.instruction(&Instruction::I32Const(1)); // stdout
                func.instruction(&Instruction::I32Const(IOVEC_PTR));
                func.instruction(&Instruction::I32Const(1)); // 1 iovec (just the newline)
                func.instruction(&Instruction::I32Const(NWRITTEN_PTR));

                if let Some(fd_write_idx) = self.fd_write_idx {
                    func.instruction(&Instruction::Call(fd_write_idx));
                    func.instruction(&Instruction::Drop);
                } else {
                    func.instruction(&Instruction::I32Const(0));
                }

                // Push a dummy value so ExprStmt can drop it
                func.instruction(&Instruction::I32Const(0));
            }
            BuiltinKind::C | BuiltinKind::List => {
                // These are handled by creating vector literals
                // Generate a vector from the arguments
                for arg in args {
                    self.gen_expr(func, ctx, arg);
                }
                if !args.is_empty() {
                    let element_ty = &args[0].ty;
                    let storage = self.storage_type_for(element_ty);
                    let array_type_index = self.ensure_array_type(&storage);
                    func.instruction(&Instruction::ArrayNewFixed {
                        array_type_index,
                        array_size: args.len() as u32,
                    });
                } else {
                    // Empty vector - push null reference
                    func.instruction(&Instruction::RefNull(HeapType::ANY));
                }
            }
        }
    }
}
