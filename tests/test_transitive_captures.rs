use rty_compiler::ast::Stmt;
use rty_compiler::ir::{IRPassManager, IRProgram, IRStmt, TypeResolver, IR};
use rty_compiler::lexer::Lexer;
use rty_compiler::parser::Parser;

fn parse_program(s: &str) -> Vec<Stmt> {
    let lexer = Lexer::new();
    let tokens = lexer.lex(&s.to_string());
    let mut parser = Parser::new(tokens);
    parser.parse_program().expect("parse failed")
}

fn parse_and_lower_with_passes(s: &str) -> IRProgram {
    let prog = parse_program(s);
    let mut resolver = TypeResolver::new();
    let mut ir_program = IR::from_ast(prog, &mut resolver).expect("IR lowering failed");

    // Run IR passes to flatten functions and collect metadata
    let mut pass_manager = IRPassManager::default_pipeline();
    pass_manager.run(&mut ir_program).expect("IR passes failed");

    ir_program
}

#[test]
fn test_transitive_capture_propagation() {
    // Test that middle functions capture variables needed by inner functions
    let source = r#"
        f <- function(): int {
            x: int <- 1

            g <- function(): int {
                # g doesn't use x directly

                h <- function(): int {
                    # h needs x from f
                    x <<- x + 1
                    return(x)
                }

                result: int <- h()
                return(result)
            }

            result: int <- g()
            return(result)
        }

        result: int <- f()
    "#;

    // Parse, resolve types, and run IR passes
    let ir_program = parse_and_lower_with_passes(source);

    // Find function g in the IR
    let g_func = ir_program.functions.iter()
        .find(|f| {
            if let IRStmt::FunctionDef { name, .. } = f {
                name == "g"
            } else {
                false
            }
        })
        .expect("Function g not found");

    // Verify g has metadata with captured variables
    if let IRStmt::FunctionDef { metadata, .. } = g_func {
        let meta = metadata.as_ref().expect("g should have metadata");

        // g should capture x transitively (even though it doesn't use it directly)
        assert!(!meta.captured_vars.is_empty(),
            "g should capture x transitively for h to use");

        let captured_x = meta.captured_vars.iter()
            .find(|c| c.name == "x")
            .expect("g should capture variable x");

        // x should be marked as mutable because h does super-assignment on it
        assert!(captured_x.is_mutable,
            "x should be mutable because h uses super-assignment");

        println!("✓ Transitive capture working: g captures x for h");
        println!("  Captured vars in g: {:?}",
            meta.captured_vars.iter().map(|c| &c.name).collect::<Vec<_>>());
    } else {
        panic!("g should be a function definition");
    }
}

#[test]
fn test_direct_vs_transitive_captures() {
    // Test distinguishing between direct and transitive captures
    let source = r#"
        outer <- function(): int {
            x: int <- 1
            y: int <- 2

            middle <- function(): int {
                # middle uses y directly but not x
                # but inner needs x, so middle must capture both

                inner <- function(): int {
                    # inner uses x
                    return(x + y)
                }

                result: int <- inner() + y
                return(result)
            }

            result: int <- middle()
            return(result)
        }

        result: int <- outer()
    "#;

    let ir_program = parse_and_lower_with_passes(source);

    // Find middle function
    let middle_func = ir_program.functions.iter()
        .find(|f| {
            if let IRStmt::FunctionDef { name, .. } = f {
                name == "middle"
            } else {
                false
            }
        })
        .expect("Function middle not found");

    if let IRStmt::FunctionDef { metadata, .. } = middle_func {
        let meta = metadata.as_ref().expect("middle should have metadata");

        // middle should capture both x (transitive) and y (direct)
        assert_eq!(meta.captured_vars.len(), 2,
            "middle should capture both x and y");

        let has_x = meta.captured_vars.iter().any(|c| c.name == "x");
        let has_y = meta.captured_vars.iter().any(|c| c.name == "y");

        assert!(has_x, "middle should capture x transitively");
        assert!(has_y, "middle should capture y directly");

        println!("✓ Direct and transitive captures working");
        println!("  Captured vars in middle: {:?}",
            meta.captured_vars.iter().map(|c| &c.name).collect::<Vec<_>>());
    }
}
