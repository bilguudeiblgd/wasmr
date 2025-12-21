// WASM-specific utilities
mod wasm;

// Code emission
mod emit;

// Core backend components
mod context;
pub mod io;

// Re-export public API (pure code generation functions only)
pub use io::{compile_to_wasm, compile_to_wasm_ir};

use crate::types::Type;
use context::LocalContext;
use crate::ir::{FunctionMetadata, IRProgram, IRStmt as Stmt};
use std::collections::HashMap;
use wasm_encoder::{CodeSection, CompositeInnerType, CompositeType, DataSection, ExportKind, ExportSection, FieldType, FuncType, Function, FunctionSection, HeapType, ImportSection, Instruction, MemorySection, Module, RefType, StorageType, StructType, SubType, TypeSection, ValType};

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
    type_count: u32,  // Manual tracking for recursion groups
    pub(crate) fd_write_idx: Option<u32>,
    array_type_i32: Option<u32>,
    array_type_f32: Option<u32>,
    array_type_f64: Option<u32>,
    array_type_anyref: Option<u32>,
    // Map function signatures to type indices for typed funcrefs
    func_type_indices: HashMap<FunctionSignature, u32>,
    // Map function signature to (closure_func_type_idx, base_env_type_idx) for subtyping
    base_env_types: HashMap<FunctionSignature, (u32, u32)>,
    // Map function name to (closure_func_type_idx, base_type_idx, concrete_env_type_idx) for functions with captured vars
    pub(crate) env_struct_types: HashMap<String, (u32, u32, u32)>,
    // Map environment structure to (closure_func_type_idx, base_type_idx, concrete_type_idx)
    pub(crate) env_struct_type_cache: HashMap<String, (u32, u32, u32)>,
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
            type_count: 0,
            fd_write_idx: None,
            array_type_i32: None,
            array_type_f32: None,
            array_type_f64: None,
            array_type_anyref: None,
            func_type_indices: HashMap::new(),
            base_env_types: HashMap::new(),
            env_struct_types: HashMap::new(),
            env_struct_type_cache: HashMap::new(),
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

        let main_type_index = self.type_count;
        {
            let ty = self.types.ty();
            ty.function(param_tys, result_tys);
        }
        self.type_count += 1;
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

        // PASS 2A: First pass - create environment struct types for all closures
        // This populates base_env_types before we process any function return types
        for func_stmt in &program.functions {
            if let Stmt::FunctionDef { name, params, return_type, metadata, .. } = func_stmt {
                let metadata = metadata.as_ref().expect("All functions should have metadata after passes");

                // Store metadata for use in gen_call
                self.func_metadata.insert(name.clone(), metadata.clone());

                eprintln!("Function {}: {} captured vars", name, metadata.captured_vars.len());
                for cap in &metadata.captured_vars {
                    eprintln!("  - {} (mutable: {})", cap.name, cap.is_mutable);
                }

                // If this function has captured variables, create environment struct types now
                if metadata.captured_vars.len() > 0 {
                    let bare_param_types: Vec<Type> = params.iter()
                        .filter_map(|p| match &p.kind {
                            crate::types::ParamKind::Normal(ty) => Some(ty.clone()),
                            crate::types::ParamKind::VarArgs => None,
                        })
                        .collect();

                    let func_idx = *self.func_indices.get(name).expect("Function should be registered");

                    // Create base and concrete environment struct types
                    // This populates base_env_types for this function signature
                    self.get_or_create_env_struct_type(
                        name,
                        &bare_param_types,
                        return_type,
                        &metadata.captured_vars,
                        func_idx
                    );
                }
            }
        }

        // PASS 2B: Second pass - declare function types
        // Now base_env_types is populated, so wasm_valtype can correctly handle closure return types
        let mut current_func_idx = main_func_index + 1;

        for func_stmt in &program.functions {
            if let Stmt::FunctionDef { name, params, return_type, metadata, .. } = func_stmt {
                let metadata = metadata.as_ref().expect("All functions should have metadata after passes");

                // Declare function type
                let mut param_tys: Vec<ValType> = params.iter().map(|p| self.wasm_param_valtype(p)).collect();

                // Determine which type index to use
                let type_index = if metadata.captured_vars.len() > 0 {
                    // Closure - use the closure function type created in PASS 2A
                    let bare_param_types: Vec<Type> = params.iter()
                        .filter_map(|p| match &p.kind {
                            crate::types::ParamKind::Normal(ty) => Some(ty.clone()),
                            crate::types::ParamKind::VarArgs => None,
                        })
                        .collect();

                    let (closure_func_type_idx, base_env_type_idx) =
                        self.get_closure_info_for_signature(&bare_param_types, return_type)
                        .expect("Closure should have been registered in PASS 2A");

                    // The closure function type index
                    closure_func_type_idx
                } else {
                    // Bare function - create regular function type
                    let result_tys: Vec<ValType> = if *return_type != Type::Void {
                        vec![self.wasm_valtype(return_type)]
                    } else {
                        vec![]
                    };

                    let type_idx = self.type_count;
                    {
                        let ty = self.types.ty();
                        ty.function(param_tys, result_tys);
                    }
                    self.type_count += 1;
                    type_idx
                };

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
                } else if let Some(&(_closure_func_type_idx, base_type_idx, _concrete_type_idx)) = self.env_struct_types.get(&var_info.name) {
                    // This variable holds a function with environment - use the base environment struct type
                    self.env_struct_valtype(base_type_idx)
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
                // Use the base type index for the function parameter
                let env_struct_type_idx = if metadata.captured_vars.len() > 0 {
                    self.env_struct_types.get(name).map(|(_closure_func_idx, base_idx, _)| *base_idx)
                } else {
                    None
                };

                // Get function index (already registered in PASS 1)
                let func_index = *self.func_indices.get(name).expect("Function should be registered");

                // Define function body
                let ret_has_value = *return_type != Type::Void;
                let ir_param_count = params.len() as u32;  // Number of params in IR (before adding env)
                let has_captured_vars = metadata.captured_vars.len() > 0;
                let wasm_param_count = if has_captured_vars {
                    ir_param_count + 1  // Add 1 for environment parameter
                } else {
                    ir_param_count
                };
                let non_param_vars = &metadata.local_vars[ir_param_count as usize..];

                let mut local_types: Vec<(u32, ValType)> = non_param_vars
                    .iter()
                    .map(|var_info| {
                        // Determine the WASM type for this local variable
                        let val_type = if var_info.need_reference {
                            // Variable needs to be in a reference cell (for super-assignment)
                            let ref_cell_type_idx = self.get_or_create_ref_cell_type(&var_info.ty);
                            self.ref_cell_valtype(ref_cell_type_idx)
                        } else if let Some(&(_closure_func_type_idx, base_type_idx, _concrete_type_idx)) = self.env_struct_types.get(&var_info.name) {
                            // This variable holds a function with environment - use the base environment struct type
                            self.env_struct_valtype(base_type_idx)
                        } else {
                            self.wasm_valtype(&var_info.ty)
                        };
                        (1, val_type)
                    })
                    .collect();

                // If this function has captured variables, add a local for the downcasted typed environment
                let (typed_env_local, concrete_env_type_idx) = if has_captured_vars {
                    let (base_idx, concrete_idx) = env_struct_type_idx
                        .and_then(|base_idx| {
                            self.env_struct_types.get(name).map(|(_closure_func_idx, b, c)| (*b, *c))
                        })
                        .unwrap_or_else(|| {
                            // This shouldn't happen, but provide defaults
                            (0, 0)
                        });

                    // Add typed environment local: (ref $concrete_type)
                    let typed_env_valtype = self.env_struct_valtype(concrete_idx);
                    local_types.push((1, typed_env_valtype));

                    // Calculate local index: WASM parameters (including env) + existing locals
                    let typed_env_idx = wasm_param_count + (local_types.len() as u32) - 1;
                    (Some(typed_env_idx), Some(concrete_idx))
                } else {
                    (None, None)
                };

                let mut func = Function::new(local_types);

                // Create context with environment info for functions with captured variables
                let ctx = if metadata.captured_vars.len() > 0 {
                    LocalContext::from_metadata_with_env(
                        metadata,
                        Some(0),  // Environment is always param 0
                        env_struct_type_idx,  // Base type
                        concrete_env_type_idx,  // Concrete type for downcasting
                        typed_env_local  // Local index of downcasted environment
                    )
                } else {
                    LocalContext::from_metadata(metadata)
                };

                // If this is a closure, emit downcast at start of function
                if let (Some(concrete_idx), Some(typed_local)) = (concrete_env_type_idx, typed_env_local) {
                    // Downcast: (local.set $typed_env (ref.cast (ref $concrete) (local.get 0)))
                    func.instruction(&Instruction::LocalGet(0)); // Load environment parameter (base type)
                    func.instruction(&Instruction::RefCastNonNull(HeapType::Concrete(concrete_idx))); // Downcast to concrete type
                    func.instruction(&Instruction::LocalSet(typed_local)); // Store in typed local
                }

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
        let start_type_index = self.type_count;
        {
            let ty = self.types.ty();
            ty.function(vec![], vec![]);
        }
        self.type_count += 1;
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

        let type_idx = self.type_count;
        self.types.ty().function(param_vals, result_vals);
        self.type_count += 1;
        self.func_type_indices.insert(sig, type_idx);

        type_idx
    }

    /// Get or create a function type for closures (includes environment parameter)
    /// This is the type of the actual WASM function implementation
    pub(crate) fn get_closure_func_type_index(
        &mut self,
        param_types: &[Type],
        return_type: &Type,
        base_env_type_idx: u32,
    ) -> u32 {
        // For closures, prepend the environment parameter to the signature
        let mut closure_params = vec![Type::Void]; // Placeholder for env (we'll use base_env_type_idx)
        closure_params.extend_from_slice(param_types);

        let sig = FunctionSignature {
            param_types: closure_params,
            return_type: return_type.clone(),
        };

        if let Some(&idx) = self.func_type_indices.get(&sig) {
            return idx;
        }

        // Create function type with environment as first parameter
        let mut param_vals: Vec<ValType> = vec![self.env_struct_valtype(base_env_type_idx)];
        param_vals.extend(param_types.iter().map(|t| self.wasm_valtype(t)));

        let result_vals = if *return_type == Type::Void {
            vec![]
        } else {
            vec![self.wasm_valtype(return_type)]
        };

        let type_idx = self.type_count;
        self.types.ty().function(param_vals, result_vals);
        self.type_count += 1;
        self.func_type_indices.insert(sig, type_idx);

        type_idx
    }

    /// Get or create a base environment struct type for closures
    /// The base type contains only the function pointer and is open for subtyping
    /// This enables type abstraction for closures with different captured variables
    ///
    /// This creates mutually recursive types in a recursion group:
    /// - Closure function type: (func (param (ref $base)) ...)
    /// - Base struct type: (struct (field (ref $closure_func)))
    ///
    /// Returns (closure_func_type_idx, base_type_idx)
    pub(crate) fn get_or_create_base_env_type(
        &mut self,
        param_types: &[Type],
        return_type: &Type,
    ) -> (u32, u32) {
        let sig = FunctionSignature {
            param_types: param_types.to_vec(),
            return_type: return_type.clone(),
        };

        // Check cache first
        if let Some(&indices) = self.base_env_types.get(&sig) {
            return indices;
        }

        // We need to create mutually recursive types in a recursion group:
        // type[N]: (func (param (ref N+1)) ...) - closure function type
        // type[N+1]: (struct (field (ref N)))    - base struct type
        //
        // In WASM GC, mutually recursive types must be in a recursion group

        // Recursion groups create multiple types with consecutive indices
        // We need to manually track the indices since .rec() is a single operation
        let type_count_before = self.type_count;
        let closure_func_type_idx = type_count_before;
        let base_type_idx = type_count_before + 1;

        // Prepare closure function type parameters: (ref $base), <user params...>
        let mut param_vals: Vec<ValType> = vec![self.env_struct_valtype(base_type_idx)];
        param_vals.extend(param_types.iter().map(|t| self.wasm_valtype(t)));

        let result_vals = if *return_type == Type::Void {
            vec![]
        } else {
            vec![self.wasm_valtype(return_type)]
        };

        // Prepare base struct field: (ref $closure_func_type)
        let func_ref_type = ValType::Ref(RefType {
            nullable: false,
            heap_type: HeapType::Concrete(closure_func_type_idx),
        });

        let field = FieldType {
            element_type: StorageType::Val(func_ref_type),
            mutable: false,
        };

        // Create both types in a recursion group
        self.types.ty().rec(vec![
            // Type N: closure function type
            SubType {
                is_final: false,
                supertype_idx: None,
                composite_type: CompositeType {
                    inner: CompositeInnerType::Func(wasm_encoder::FuncType::new(param_vals, result_vals)),
                    shared: false,
                },
            },
            // Type N+1: base struct type
            SubType {
                is_final: false,
                supertype_idx: None,
                composite_type: CompositeType {
                    inner: CompositeInnerType::Struct(StructType {
                        fields: Box::new([field]),
                    }),
                    shared: false,
                },
            },
        ]);

        // Recursion group creates 2 types
        self.type_count += 2;

        // Cache both indices
        self.base_env_types.insert(sig, (closure_func_type_idx, base_type_idx));

        (closure_func_type_idx, base_type_idx)
    }

    /// Check if a function signature corresponds to a closure (has environment)
    /// Returns Some((closure_func_type_idx, base_type_idx)) if it's a closure, None if bare function
    pub(crate) fn get_closure_info_for_signature(
        &self,
        param_types: &[Type],
        return_type: &Type,
    ) -> Option<(u32, u32)> {
        let sig = FunctionSignature {
            param_types: param_types.to_vec(),
            return_type: return_type.clone(),
        };
        self.base_env_types.get(&sig).copied()
    }
}
