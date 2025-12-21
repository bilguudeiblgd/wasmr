//! Format conversion utilities
//!
//! Provides conversion between different intermediate formats,
//! such as WASM binary to WAT text format.

/// Convert WASM binary bytes to WAT (WebAssembly Text) format
pub fn wasm_to_wat(wasm_bytes: &[u8]) -> Result<String, String> {
    wasmprinter::print_bytes(wasm_bytes)
        .map_err(|e| format!("Failed to convert WASM to WAT: {}", e))
}
