# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Rty_compiler is a compiler for a typed R-like language that compiles to WebAssembly. The project implements a multi-stage compilation pipeline: lexing → parsing → IR (intermediate representation with type resolution) → WASM code generation.

## Build and Test Commands

### Build the project
```bash
cargo build
```

### Run the compiler
The compiler processes all `.R` files in the `data/` directory:
```bash
cargo run
```
This will compile each `.R` file to a corresponding `.wasm` file.


### Run the generated assembly
After cargo run, outputs are inside out folder. To run them, we use wasmtime, but it's important to note that we need to enable certain features
```bash
wasmtime -W gc=y -W function-references=y <path to wasm file>
```
This will compile each `.R` file to a corresponding `.wasm` file.


### Run tests
```bash
cargo test
```

Also important to run our end-to-end tests:
```bash
./test.sh
```

Below code takes files from `data/` and runs them through the compiler to generate wasm, and takes in R data files from data_R and compares the output
```bash
./translate_and_test.sh
```

### Run specific test file
```bash
cargo test --test <test_name>
# Examples:
cargo test --test lexer_tests
cargo test --test parser_tests
cargo test --test ir_builtin_tests
```

### Run tests with output
```bash
cargo test -- --nocapture
```

## Architecture

### Compilation Pipeline

1. **Lexer** (`src/lexer.rs`): Tokenizes source code into tokens
   - Handles R-like syntax including `<-` assignment operator
   - Recognizes built-in type names (int, float, double, string, vector, etc.)
   - Produces `Vec<Token>` for the parser

2. **Parser** (`src/parser.rs`): Builds an Abstract Syntax Tree (AST)
   - Recursive descent parser
   - Produces untyped AST nodes (`Stmt` and `Expr` from `src/ast.rs`)
   - Functions are first-class values (expressions, not just statements)
   - Supports type annotations with `:` (e.g., `x: int`)

3. **Type Resolution / IR Generation** (`src/ir.rs`): Converts AST to typed IR
   - The `TypeResolver` performs type inference and checking
   - Converts AST nodes to IR nodes where all types are concrete
   - Detects type errors (arity mismatches, unknown variables, etc.)
   - Handles built-in functions like `c()`, `list()`, `print()`
   - Returns `IR` struct containing typed statements (`Vec<IRStmt>`)

4. **Backend** (`src/backend/`): Generates WebAssembly from typed IR
   - `WasmGenerator` in `mod.rs` produces WASM modules
   - Uses `wasm-encoder` crate for WASM construction
   - Code emission organized in `emit/` submodule (expressions, statements, builtins, binary ops, functions, ref cells)
   - WASM-specific utilities in `wasm/` submodule (type mapping, memory management, runtime helpers)
   - Public API: `compile_to_wasm()` and `compile_to_wasm_ir()` in `io.rs`

5. **Driver** (`src/driver/`): Orchestration and I/O
   - Separates compilation orchestration from pure code generation
   - `pipeline.rs`: Full compilation flows (`compile_and_write`, `compile_and_write_ir`)
   - `io.rs`: File system operations for writing WASM/WAT files
   - `conversion.rs`: Format transformations (WASM → WAT using wasmprinter)

### Types Module

The type system lives in a dedicated cross-cutting module used by all compilation phases:

- **Types** (`src/types/`): Cross-cutting type definitions
  - `ast_types.rs`: Core type definitions (`Type`, `Param`, `ParamKind` enums)
  - `builtins.rs`: Built-in type utilities (`is_builtin_type_name()`, `map_builtin_type()`)
  - Used by lexer, parser, IR, and backend - not coupled to any single phase

### Key Data Structures

- **AST** (`src/ast.rs`): Untyped syntax tree
  - `Stmt`: Statements (assignments, returns, if/for, blocks)
  - `Expr`: Expressions (numbers, identifiers, function defs, calls, binary ops)
  - Uses types from `src/types/` module (not defined here)

- **IR** (`src/ir.rs`): Typed intermediate representation
  - `IRStmt` and `IRExpr`: Similar to AST but all nodes have concrete types
  - `IRExpr` contains both `kind: IRExprKind` and `ty: Type`
  - `TypeResolver`: Type checking and inference engine
  - `TypeError`: Comprehensive error types for type checking failures

- **Backend** (`src/backend/`):
  - `WasmGenerator` (in `mod.rs`): Main codegen struct with type/function/code sections
  - `LocalContext` (in `context.rs`): Tracks local variable indices and captured variables within functions
  - Code emission split by construct (`emit/`): expressions, statements, builtins, binary ops, functions, ref cells
  - WASM utilities (`wasm/`): type mapping, memory management, runtime helpers
  - Maps R types to WASM types (Int→I32, Float→F32, Double→F64, Vector→array refs)

### Type System

The language supports:
- Primitive types: `int`, `float`, `double`, `bool`, `char`, `string`, `void`
- Generic types: `vector<T>` (homogeneous arrays)
- Special types: `any`, `VarArgs`, `FunctionRef`
- Type inference where possible, with explicit annotations using `:` syntax

### Language Features

- R-style assignment: `x <- value` or with type: `x: int <- value`
- Superassignment: `x <<- value` for modifying parent scope variables
- Function definitions as expressions: `f <- function(x: int): int { return(x * 2) }`
- **First-class functions**: Functions as values with arrow type syntax
  - Arrow types: `int -> int`, `float, float -> int`, `(int -> int) -> int`
  - Pass functions as arguments, return from functions, store in variables
  - Uses WASM typed function references (`ref.func` and `call_ref`)
  - Parentheses required for multi-param types in parameters: `f: (int, int -> int)`
- Varargs support: `function(...) { }` with `...` forwarding
- Control flow: `if/else`, `for (x in vec) { }`, `while (cond) { }`
- Binary operations: arithmetic, comparison, logical (& and |)
- Range operator: `1:10` creates a sequence
- Built-in functions: `c()` (concatenate), `list()`, `print()`
- Vector operations (componentwise arithmetic)
- Lexical scoping: Only functions create scopes (not blocks/if/while/for)

## Important Implementation Details

### Built-in Type Recognition
Built-in type names are recognized in `src/types/builtins.rs`:
- `is_builtin_type_name()`: Checks if a string is a built-in type
- `map_builtin_type()`: Maps type name strings to `Type` enum variants

### Type Resolution Flow
1. AST is passed to `TypeResolver::new(ast_stmts)`
2. `TypeResolver::resolve()` processes all statements
3. Top-level function definitions are collected first
4. Statement-by-statement type checking with environment tracking
5. Returns `Result<IR, TypeError>` with fully typed IR or errors

### WASM Type Mapping
- Vectors use WASM GC `array` types with concrete storage types
- `ensure_array_type()` manages array type indices (cached per element type)
- Function types are created dynamically and registered in `TypeSection`
- Local variables tracked by index in `LocalContext`
- **Function references**: Uses typed funcrefs (WASM function references proposal)
  - `get_or_create_func_type_index()` creates/reuses type indices for function signatures
  - Functions represented as `ref (type $idx)` at runtime (not table indices)
  - Direct calls use `call`, indirect calls use `call_ref`
  - No element section or function tables needed

### Type Comparison for Functions
When comparing function types (e.g., in type checking), parameter names are ignored:
- `types_compatible()` in `src/ir/lowering.rs` compares types structurally
- Function types match based on parameter types and return type only
- Example: `int -> int` annotation matches function with parameter named `x: int`
- This is critical for first-class function type checking

### IR Passes and Metadata
The IR system uses a pass-based architecture:
- `IRPassManager` in `src/ir/passes/mod.rs` coordinates passes
- **Variable collection pass**: Analyzes functions to collect all local variables
  - Assigns WASM local indices to parameters, user variables, and compiler temps
  - Populates `FunctionMetadata` with `LocalVarInfo` for each variable
  - Tracks loop iterators, loop indices, and temporary variables
- **Scoping**: Lexical scoping where only functions create new scopes
  - `ScopeStack` in `TypeResolver` manages nested function scopes
  - Superassignment (`<<-`) searches parent function scopes

### Testing Strategy
- Lexer tests: Token stream validation (`tests/lexer_tests.rs`, `tests/parser_function_type_tests.rs`)
- Parser tests: AST structure validation (`tests/parser_tests.rs`)
- IR tests: Type resolution and scoping (`tests/ir_builtin_tests.rs`, `tests/ir_scoping_tests.rs`)
- First-class functions: Type checking and codegen (`tests/first_class_function_tests.rs`)
- WASM tests: Code generation and execution (`tests/wasm_codegen_smoke.rs`, `tests/wasm_write_out.rs`)

## Common Development Patterns

### Adding a New Built-in Function
1. Add to `BuiltinKind` enum in `src/ir.rs`
2. Handle in `TypeResolver::resolve_builtin_call()` for type checking
3. Implement codegen in `WasmGenerator::compile_builtin_call()` in `src/backend/emit/builtins.rs`

### Adding a New Binary Operator
1. Add token to `Token` enum in `src/lexer.rs` and lexing logic
2. Add to `BinaryOp` enum in `src/ast.rs`
3. Handle parsing in `Parser::match_comparison_or_range()` or similar
4. Add type rules in `TypeResolver::resolve_binary_expr()`
5. Implement WASM emission in `WasmGenerator::gen_binary_op()` in `src/backend/emit/binary_ops.rs`

### Adding a New Statement Type
1. Add variant to `Stmt` enum in `src/ast.rs`
2. Add parsing logic in `Parser::parse_statement()`
3. Add to `IRStmt` enum in `src/ir.rs`
4. Implement type resolution in `TypeResolver::resolve_stmt()`
5. Add codegen in `WasmGenerator::gen_stmt()` in `src/backend/emit/statements.rs`

## Test Data
Example R source files are in `data/*.R`. These are compiled when running `cargo run` and serve as integration test cases.
