use rty_compiler::ast::Stmt;
use rty_compiler::ir::{TypeResolver, IR};
use rty_compiler::lexer::Lexer;
use rty_compiler::parser::Parser;

fn parse_program(s: &str) -> Vec<Stmt> {
    let lexer = Lexer::new();
    let tokens = lexer.lex(&s.to_string());
    let mut parser = Parser::new(tokens);
    parser.parse_program().expect("parse failed")
}

#[test]
fn test_function_scoped_variables() {
    // Variables declared in if blocks are visible throughout the function
    let code = r#"
        f <- function(): int {
            x: int <- 5
            if (x > 3) {
                y: int <- 10
            }
            return(y)
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    // Should succeed - y is visible throughout function
    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_nested_function_scoping() {
    // Nested functions can access parent function variables
    let code = r#"
        outer <- function(): int {
            x: int <- 5

            inner <- function(): int {
                return(x)
            }

            return(inner())
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_nested_function_not_visible_outside() {
    // Functions defined in a function are not visible outside
    let code = r#"
        outer <- function(): int {
            helper <- function(x: int): int {
                return(x * 2)
            }
            return(helper(5))
        }
        y <- helper(3)
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let result = IR::from_ast(prog, &mut resolver);

    // Should fail - helper not visible outside outer function
    assert!(result.is_err());
}

#[test]
fn test_superassignment_to_parent_function() {
    // Super assignment modifies parent function scope
    let code = r#"
        outer <- function(): int {
            x: int <- 5

            inner <- function(): void {
                x <<- 10
            }

            inner()
            return(x)
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_superassignment_variable_not_found() {
    // Super assignment fails if variable not in parent scope
    let code = r#"
        f <- function(): void {
            y <<- 10
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let result = IR::from_ast(prog, &mut resolver);

    // Should fail - y not in parent scope
    assert!(result.is_err());
}

#[test]
fn test_loop_variables_function_scoped() {
    // Loop iterator is function-scoped (visible after loop)
    let code = r#"
        f <- function(): int {
            sum: int <- 0
            for (i in 1:5) {
                sum <- sum + i
            }
            return(i)
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    // Should succeed - i is visible throughout function
    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_recursive_functions() {
    // Functions can call themselves (name visible during body)
    let code = r#"
        factorial <- function(n: int): int {
            if (n <= 1) {
                return(1)
            }
            return(n * factorial(n - 1))
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_function_shadowing_variable() {
    // Functions and variables share same namespace - shadowing allowed
    let code = r#"
        x: int <- 5
        x <- function(): int {
            return(10)
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 2);
}

#[test]
fn test_nested_function_variable_independence() {
    // Variables in nested function don't affect parent
    let code = r#"
        outer <- function(): int {
            x: int <- 5

            inner <- function(): int {
                x: int <- 10
                return(x)
            }

            y <- inner()
            return(x)
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_closure_with_superassignment() {
    // Nested functions can capture and modify parent variables via <<-
    let code = r#"
        make_counter <- function(): function {
            count: int <- 0

            increment <- function(): int {
                count <<- count + 1
                return(count)
            }

            return(increment)
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_function_type_stored_in_scope() {
    // Functions are stored with their type signature
    let code = r#"
        f <- function(x: int, y: double): int {
            return(x)
        }
        z <- f(5, 3.14)
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 2);
}

#[test]
fn test_multiple_nested_levels() {
    // Multiple levels of nested functions
    let code = r#"
        level1 <- function(): int {
            x: int <- 1

            level2 <- function(): int {
                y: int <- 2

                level3 <- function(): int {
                    return(x + y)
                }

                return(level3())
            }

            return(level2())
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_superassignment_to_global() {
    // Super assignment can reach global scope
    let code = r#"
        global_var: int <- 0

        f <- function(): void {
            global_var <<- 10
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 2);
}

#[test]
fn test_function_redefinition_same_scope() {
    // Functions can be redefined in the same scope
    let code = r#"
        f <- function(): int { return(1) }
        f <- function(): int { return(2) }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 2);
}

#[test]
fn test_while_loop_variables_function_scoped() {
    // Variables declared in while loops are function-scoped
    let code = r#"
        f <- function(): int {
            x: int <- 0
            while (x < 5) {
                y: int <- x * 2
                x <- x + 1
            }
            return(y)
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    // Should succeed - y is visible throughout function
    assert_eq!(ir.statements.len(), 1);
}

#[test]
fn test_superassignment_with_function() {
    // Super assignment works for function values too
    let code = r#"
        f <- function(): int { return(1) }

        outer <- function(): void {
            f <<- function(): int { return(2) }
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 2);
}

#[test]
fn test_nested_function_parameter_shadows_parent() {
    // Nested function parameters shadow parent variables
    let code = r#"
        outer <- function(): int {
            x: int <- 5

            inner <- function(x: int): int {
                return(x * 2)
            }

            return(inner(10))
        }
    "#;

    let prog = parse_program(code);
    let mut resolver = TypeResolver::new();
    let ir = IR::from_ast(prog, &mut resolver).expect("IR lowering should succeed");

    assert_eq!(ir.statements.len(), 1);
}
