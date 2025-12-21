use wasm_encoder::{EntityType, MemoryType, ValType};

use super::super::WasmGenerator;

impl WasmGenerator {
    pub(crate) fn setup_wasi_imports(&mut self) {
        // Define the fd_write function type: (i32, i32, i32, i32) -> i32
        let fd_write_type_idx = self.types.len() as u32;
        {
            let ty = self.types.ty();
            ty.function(
                vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
                vec![ValType::I32],
            );
        }

        // Import fd_write from wasi_snapshot_preview1
        self.imports.import(
            "wasi_snapshot_preview1",
            "fd_write",
            EntityType::Function(fd_write_type_idx),
        );

        // Store the function index (imports come before other functions)
        self.fd_write_idx = Some(self.func_count);
        self.func_count += 1;

        // Add 1 page of memory (64KB)
        self.memory.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
    }
}
