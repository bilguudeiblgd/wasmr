use wasm_encoder::{BlockType, Function, HeapType, Instruction, RefType, StorageType, ValType};
use crate::codegen::WasmGenerator;

impl WasmGenerator {
    /// Include built-in runtime functions that can be used throughout the compiled WASM
    pub fn include_builtins(&mut self) {
        // Ensure we have the i32 array type defined
        let storage = StorageType::Val(ValType::I32);
        let int_vec_type = self.ensure_array_type(&storage);

        // Generate vector_add function: (ref $int_vec, ref $int_vec) -> (ref $int_vec)
        self.gen_vector_add_builtin(int_vec_type);

        // Add more built-in functions as needed
        // self.gen_vector_sub_builtin(int_vec_type);
        // self.gen_vector_mul_builtin(int_vec_type);
    }

    fn gen_vector_add_builtin(&mut self, int_vec_type: u32) {
        // Define the function type: (ref array, ref array) -> ref array
        let vector_val_type = ValType::Ref(RefType {
            nullable: false,
            heap_type: HeapType::Concrete(int_vec_type),
        });
        let type_index = self.types.len() as u32;
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

    // Similar functions for other operations
    fn gen_vector_sub_builtin(&mut self, int_vec_type: u32) {
        let type_index = self.types.len() as u32;
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
        self.func_indices.insert("vector_sub".to_string(), func_idx);
        self.func_count += 1;

        let locals = vec![
            (1, ValType::I32),
            (1, ValType::I32),
            (1, ValType::I32),
            (1, ValType::Ref(RefType::ARRAYREF)),
        ];

        let mut func = Function::new(locals);

        // Similar to vector_add but use I32Sub instead of I32Add
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

        func.instruction(&Instruction::I32Sub); // Changed to Sub
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

    fn gen_vector_mul_builtin(&mut self, int_vec_type: u32) {
        let type_index = self.types.len() as u32;
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