mod type_mapping;
mod local_context;
mod wasi;
mod binary_ops;
mod expressions;
mod builtins;
mod statements;
mod function_env;
mod ref_cell;
pub mod io;
pub mod codegen_builtins;

// Re-export public API
pub use io::{compile_and_write, compile_and_write_ir, compile_to_wasm, compile_to_wasm_ir, wasm_to_wat, write_wasm_file, write_wat_file};

use crate::ast::Type;
use local_context::LocalContext;
use crate::ir::{FunctionMetadata, IRProgram, IRStmt as Stmt};
use std::collections::HashMap;
use wasm_encoder::{CodeSection, DataSection, ExportKind, ExportSection, FieldType, Function, FunctionSection, ImportSection, Instruction, MemorySection, Module, RefType, StorageType, StructType, TypeSection, ValType};

// Function signature for tracking unique function types
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct FunctionSignature {
    param_types: Vec<Type>,
    return_type: Type,
}

pub struct WasmGenerator {
    module: Module,
    pub(crate) types: TypeSection,
    imports: ImportSection,
    pub(crate) functions: FunctionSection,
    memory: MemorySection,
    exports: ExportSection,
    data: DataSection,
    pub(crate) code: CodeSection,
    pub(crate) func_indices: HashMap<String, u32>,
    pub(crate) func_count: u32,
    pub(crate) fd_write_idx: Option<u32>,
    array_type_i32: Option<u32>,
    array_type_f32: Option<u32>,
    array_type_f64: Option<u32>,
    array_type_anyref: Option<u32>,
    // Map function signatures to type indices for typed funcrefs
    func_type_indices: HashMap<FunctionSignature, u32>,
    // Map function name to environment struct type index (for functions with captured vars)
    env_struct_types: HashMap<String, u32>,
    // Map function name to metadata (for accessing captured variables in gen_call)
    func_metadata: HashMap<String, Box<FunctionMetadata>>,
    // Map type to reference cell struct type index (for super-assignment)
    ref_cell_types: HashMap<Type, u32>,
}

impl WasmGenerator {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            types: TypeSection::new(),
            imports: ImportSection::new(),
            functions: FunctionSection::new(),
            memory: MemorySection::new(),
            exports: ExportSection::new(),
            data: DataSection::new(),
            code: CodeSection::new(),
            func_indices: HashMap::new(),
            func_count: 0,
            fd_write_idx: None,
            array_type_i32: None,
            array_type_f32: None,
            array_type_f64: None,
            array_type_anyref: None,
            func_type_indices: HashMap::new(),
            env_struct_types: HashMap::new(),
            func_metadata: HashMap::new(),
            ref_cell_types: HashMap::new(),
        }
    }

    pub fn compile_program(&mut self, program: IRProgram) -> Vec<u8> {
        // Setup WASI imports (fd_write) and memory
        self.setup_wasi_imports();

        // Include builtin functions
        self.include_builtins();

        // Extract the main function from the program
        // According to IRProgram::new(), main_function is always a FunctionDef
        let (main_name, main_params, main_ret_ty, main_body, main_metadata) = match program.main_function {
            Stmt::FunctionDef { name, params, return_type, body, metadata } => {
                let metadata = metadata.expect("IRProgram main function should have metadata from IR passes");
                (name, params, return_type, body, metadata)
            }
            _ => panic!("IRProgram.main_function should always be a FunctionDef"),
        };

        // Declare the main function type: (params) -> return_type
        let param_tys: Vec<ValType> = main_params.iter().map(|p| self.wasm_param_valtype(p)).collect();
        let result_tys: Vec<ValType> = if main_ret_ty != Type::Void {
            vec![self.wasm_valtype(&main_ret_ty)]
        } else {
            vec![]
        };

        let main_type_index = self.types.len() as u32;
        {
            let ty = self.types.ty();
            ty.function(param_tys, result_tys);
        }
        self.functions.function(main_type_index);

        let main_func_index = self.func_count;
        self.func_indices.insert(main_name.clone(), main_func_index);
        self.func_count += 1;

        // PASS 1: Register all function indices for all functions
        for func_stmt in &program.functions {
            if let Stmt::FunctionDef { name, .. } = func_stmt {
                let func_index = self.func_count;
                self.func_indices.insert(name.clone(), func_index);
                self.func_count += 1;
            }
        }

        // PASS 2: Declare function types and create environment struct types for functions with captured vars
        // Reset func_count to start from where we left off after main
        let mut current_func_idx = main_func_index + 1;

        for func_stmt in &program.functions {
            if let Stmt::FunctionDef { name, params, return_type, metadata, .. } = func_stmt {
                let metadata = metadata.as_ref().expect("All functions should have metadata after passes");

                // Store metadata for use in gen_call
                self.func_metadata.insert(name.clone(), metadata.clone());

                // Declare function type
                let mut param_tys: Vec<ValType> = params.iter().map(|p| self.wasm_param_valtype(p)).collect();

                // If there's captured variables, create an environment struct type
                if metadata.captured_vars.len() > 0 {
                    // First, get the function type index for the bare function (without env)
                    let bare_param_types: Vec<Type> = params.iter()
                        .filter_map(|p| match &p.kind {
                            crate::ast::ParamKind::Normal(ty) => Some(ty.clone()),
                            crate::ast::ParamKind::VarArgs => None,
                        })
                        .collect();
                    let bare_func_type_idx = self.get_or_create_func_type_index(&bare_param_types, return_type);
                    // Create environment struct type
                    let _env_type_idx = self.get_or_create_env_struct_type(
                        name,
                        &metadata.captured_vars,
                        bare_func_type_idx
                    );

                    // Add environment as first parameter
                    let env_valtype = self.env_struct_valtype(_env_type_idx);
                    param_tys.insert(0, env_valtype);
                }

                let result_tys: Vec<ValType> = if *return_type != Type::Void {
                    vec![self.wasm_valtype(return_type)]
                } else {
                    vec![]
                };

                let type_index = self.types.len() as u32;
                {
                    let ty = self.types.ty();
                    ty.function(param_tys, result_tys);
                }
                self.functions.function(type_index);

                current_func_idx += 1;
            }
        }

        // PASS 3: Define the main function body
        let ret_has_value = main_ret_ty != Type::Void;
        let param_count = main_params.len() as u32;
        let non_param_vars = &main_metadata.local_vars[param_count as usize..];

        let local_types: Vec<(u32, ValType)> = non_param_vars
            .iter()
            .map(|var_info| {
                // Determine the WASM type for this local variable
                let val_type = if var_info.need_reference {
                    // Variable needs to be in a reference cell (for super-assignment)
                    let ref_cell_type_idx = self.get_or_create_ref_cell_type(&var_info.ty);
                    self.ref_cell_valtype(ref_cell_type_idx)
                } else if let Some(&env_type_idx) = self.env_struct_types.get(&var_info.name) {
                    // This variable holds a function with environment - use the environment struct type
                    self.env_struct_valtype(env_type_idx)
                } else {
                    // Regular variable
                    self.wasm_valtype(&var_info.ty)
                };
                (1, val_type)
            })
            .collect();

        let mut main_func = Function::new(local_types);
        let ctx = LocalContext::from_metadata(&main_metadata);

        // Generate main function statements
        for stmt in &main_body {
            self.gen_stmt(&mut main_func, &ctx, stmt, ret_has_value);
        }

        main_func.instruction(&Instruction::End);
        self.code.function(&main_func);

        // PASS 4: Compile all flattened function bodies
        for func_stmt in &program.functions {
            if let Stmt::FunctionDef { name, params, return_type, body, metadata } = func_stmt {
                let metadata = metadata.as_ref().expect("All functions should have metadata after passes");

                // Get environment struct type if this function has captured variables
                let env_struct_type_idx = if metadata.captured_vars.len() > 0 {
                    self.env_struct_types.get(name).copied()
                } else {
                    None
                };

                // Get function index (already registered in PASS 1)
                let func_index = *self.func_indices.get(name).expect("Function should be registered");

                // Define function body
                let ret_has_value = *return_type != Type::Void;
                let param_count = params.len() as u32;
                let non_param_vars = &metadata.local_vars[param_count as usize..];

                let local_types: Vec<(u32, ValType)> = non_param_vars
                    .iter()
                    .map(|var_info| {
                        // Determine the WASM type for this local variable
                        let val_type = if var_info.need_reference {
                            // Variable needs to be in a reference cell (for super-assignment)
                            let ref_cell_type_idx = self.get_or_create_ref_cell_type(&var_info.ty);
                            self.ref_cell_valtype(ref_cell_type_idx)
                        } else if let Some(&env_type_idx) = self.env_struct_types.get(&var_info.name) {
                            self.env_struct_valtype(env_type_idx)
                        } else {
                            self.wasm_valtype(&var_info.ty)
                        };
                        (1, val_type)
                    })
                    .collect();

                let mut func = Function::new(local_types);

                // Create context with environment info for functions with captured variables
                let ctx = if metadata.captured_vars.len() > 0 {
                    LocalContext::from_metadata_with_env(metadata, Some(0), env_struct_type_idx) // Environment is always param 0
                } else {
                    LocalContext::from_metadata(metadata)
                };

                // Generate function statements
                for stmt in body {
                    self.gen_stmt(&mut func, &ctx, stmt, ret_has_value);
                }

                func.instruction(&Instruction::End);
                self.code.function(&func);

                // Export the function (optional - for debugging)
                self.exports.export(name, ExportKind::Func, func_index);
            }
        }

        // Create _start function: () -> ()
        // This function calls main and is the entry point
        let start_type_index = self.types.len() as u32;
        {
            let ty = self.types.ty();
            ty.function(vec![], vec![]);
        }
        self.functions.function(start_type_index);

        let start_func_idx = self.func_count;
        self.func_indices.insert("_start".to_string(), start_func_idx);
        self.func_count += 1;

        // Generate _start function body
        let mut start_func = Function::new(vec![]);

        // Call main function
        start_func.instruction(&Instruction::Call(main_func_index));

        // If main returns a value, we need to drop it since _start returns void
        if ret_has_value {
            start_func.instruction(&Instruction::Drop);
        }

        start_func.instruction(&Instruction::End);
        self.code.function(&start_func);

        // Export memory for WASI
        self.exports.export("memory", ExportKind::Memory, 0);

        // Export main function
        self.exports.export(&main_name, ExportKind::Func, main_func_index);

        // Export _start function
        self.exports.export("_start", ExportKind::Func, start_func_idx);

        // Build module (order matters: types, imports, functions, memory, exports, code)
        self.module.section(&self.types);
        self.module.section(&self.imports);
        self.module.section(&self.functions);
        self.module.section(&self.memory);
        self.module.section(&self.exports);
        self.module.section(&self.code);
        self.module.clone().finish()
    }

    /// Get or create a function type index for typed function references
    /// This enables reusing the same type index for functions with the same signature
    pub(crate) fn get_or_create_func_type_index(
        &mut self,
        param_types: &[Type],
        return_type: &Type,
    ) -> u32 {
        let sig = FunctionSignature {
            param_types: param_types.to_vec(),
            return_type: return_type.clone(),
        };

        if let Some(&idx) = self.func_type_indices.get(&sig) {
            return idx;
        }

        // Create new type
        let param_vals: Vec<ValType> = param_types
            .iter()
            .map(|t| self.wasm_valtype(t))
            .collect();

        let result_vals = if *return_type == Type::Void {
            vec![]
        } else {
            vec![self.wasm_valtype(return_type)]
        };

        let type_idx = self.types.len() as u32;
        self.types.ty().function(param_vals, result_vals);
        self.func_type_indices.insert(sig, type_idx);

        type_idx
    }
}
