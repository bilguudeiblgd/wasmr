use rty_compiler::ast::Stmt;
use rty_compiler::ir::{TypeResolver, IR};
use rty_compiler::lexer::Lexer;
use rty_compiler::parser::Parser;
use rty_compiler::codegen::compile_to_wasm;

fn parse_and_lower(s: &str) -> Result<Vec<Stmt>, String> {
    let lexer = Lexer::new();
    let tokens = lexer.lex(&s.to_string());
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;

    let mut resolver = TypeResolver::new();
    let _ir = IR::from_ast(ast.clone(), &mut resolver)
        .map_err(|e| format!("Type error: {:?}", e))?;

    Ok(ast)
}

#[test]
fn test_function_as_argument() {
    let code = r#"
        twice <- function(x: int): int {
            return(x * 2)
        }

        apply <- function(f: int -> int, x: int): int {
            return(f(x))
        }

        result <- apply(twice, 5)
    "#;

    let ast = parse_and_lower(code).expect("Should compile");
    assert_eq!(ast.len(), 3); // twice, apply, result assignment

    // Try to generate WASM
    let wasm_bytes = compile_to_wasm(ast);
    assert!(wasm_bytes.len() > 0, "WASM generation should succeed");
}

#[test]
fn test_function_stored_in_variable() {
    let code = r#"
        add <- function(x: int, y: int): int {
            return(x + y)
        }

        f: int, int -> int <- add
        result <- f(3, 4)
    "#;

    let ast = parse_and_lower(code).expect("Should compile");
    assert_eq!(ast.len(), 3); // add, f assignment, result

    let wasm_bytes = compile_to_wasm(ast);
    assert!(wasm_bytes.len() > 0);
}

#[test]
fn test_higher_order_function() {
    let code = r#"
        apply_twice <- function(f: int -> int, x: int): int {
            return(f(f(x)))
        }

        inc <- function(n: int): int {
            return(n + 1)
        }

        result <- apply_twice(inc, 0)
    "#;

    let ast = parse_and_lower(code).expect("Should compile");
    assert_eq!(ast.len(), 3);

    let wasm_bytes = compile_to_wasm(ast);
    assert!(wasm_bytes.len() > 0);
}

#[test]
fn test_multi_param_function_type() {
    let code = r#"
        operate <- function(f: (int, int -> int), a: int, b: int): int {
            return(f(a, b))
        }

        multiply <- function(x: int, y: int): int {
            return(x * y)
        }

        result <- operate(multiply, 3, 4)
    "#;

    let ast = parse_and_lower(code).expect("Should compile");
    assert_eq!(ast.len(), 3);

    let wasm_bytes = compile_to_wasm(ast);
    assert!(wasm_bytes.len() > 0);
}
