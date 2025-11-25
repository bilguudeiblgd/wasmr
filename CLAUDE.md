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

### Run tests
```bash
cargo test
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

4. **Code Generation** (`src/codegen.rs`): Generates WebAssembly
   - `WasmGenerator` produces WASM modules from typed IR
   - Uses `wasm-encoder` crate for WASM construction
   - Tracks function indices and generates proper WASM types
   - Handles vector operations using WASM GC array types
   - Built-in runtime functions in `src/codegen_builtins.rs`

### Key Data Structures

- **AST** (`src/ast.rs`): Untyped syntax tree
  - `Stmt`: Statements (assignments, returns, if/for, blocks)
  - `Expr`: Expressions (numbers, identifiers, function defs, calls, binary ops)
  - `Type`: Type annotations (optional in AST, required after type resolution)
  - `Param` and `ParamKind`: Function parameters including varargs support

- **IR** (`src/ir.rs`): Typed intermediate representation
  - `IRStmt` and `IRExpr`: Similar to AST but all nodes have concrete types
  - `IRExpr` contains both `kind: IRExprKind` and `ty: Type`
  - `TypeResolver`: Type checking and inference engine
  - `TypeError`: Comprehensive error types for type checking failures

- **WASM Generator** (`src/codegen.rs`):
  - `WasmGenerator`: Main codegen struct with type/function/code sections
  - `LocalContext`: Tracks local variable indices within functions
  - Maps R types to WASM types (Int→I32, Float→F32, Double→F64, Vector→array refs)

### Type System

The language supports:
- Primitive types: `int`, `float`, `double`, `bool`, `char`, `string`, `void`
- Generic types: `vector<T>` (homogeneous arrays)
- Special types: `any`, `VarArgs`, `FunctionRef`
- Type inference where possible, with explicit annotations using `:` syntax

### Language Features

- R-style assignment: `x <- value` or with type: `x: int <- value`
- Function definitions as expressions: `f <- function(x: int): int { return(x * 2) }`
- Varargs support: `function(...) { }` with `...` forwarding
- Control flow: `if/else`, `for (x in vec) { }`
- Binary operations: arithmetic, comparison, logical (& and |)
- Range operator: `1:10` creates a sequence
- Built-in functions: `c()` (concatenate), `list()`, `print()`
- Vector operations (componentwise arithmetic)

## Important Implementation Details

### Built-in Type Recognition
Built-in type names are recognized in `src/lib.rs`:
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

### Testing Strategy
- Lexer tests: Token stream validation (`tests/lexer_tests.rs`)
- Parser tests: AST structure validation (`tests/parser_tests.rs`)
- IR tests: Type resolution correctness (`tests/ir_builtin_tests.rs`)
- WASM tests: Code generation and execution (`tests/wasm_codegen_smoke.rs`, `tests/wasm_write_out.rs`)

## Common Development Patterns

### Adding a New Built-in Function
1. Add to `BuiltinKind` enum in `src/ir.rs`
2. Handle in `TypeResolver::resolve_builtin_call()` for type checking
3. Implement codegen in `WasmGenerator::compile_builtin_call()` in `src/codegen.rs`

### Adding a New Binary Operator
1. Add token to `Token` enum in `src/lexer.rs` and lexing logic
2. Add to `BinaryOp` enum in `src/ast.rs`
3. Handle parsing in `Parser::match_comparison_or_range()` or similar
4. Add type rules in `TypeResolver::resolve_binary_expr()`
5. Implement WASM emission in `WasmGenerator::compile_expr()` match arm

### Adding a New Statement Type
1. Add variant to `Stmt` enum in `src/ast.rs`
2. Add parsing logic in `Parser::parse_statement()`
3. Add to `IRStmt` enum in `src/ir.rs`
4. Implement type resolution in `TypeResolver::resolve_stmt()`
5. Add codegen in `WasmGenerator::compile_stmt()`

## Test Data
Example R source files are in `data/*.R`. These are compiled when running `cargo run` and serve as integration test cases.
