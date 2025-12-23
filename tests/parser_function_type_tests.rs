use rty_compiler::ast::Stmt;
use rty_compiler::types::{Param, ParamKind, Type};
use rty_compiler::lexer::Lexer;
use rty_compiler::parser::Parser;

fn parse_program(s: &str) -> Vec<Stmt> {
    let lexer = Lexer::new();
    let tokens = lexer.lex(&s.to_string());
    let mut parser = Parser::new(tokens);
    parser.parse_program().expect("parse failed")
}

#[test]
fn test_simple_function_type() {
    // double -> double
    let code = "f: double -> double <- function(x: double): double { return(x) }";
    let stmts = parse_program(code);

    assert_eq!(stmts.len(), 1);
    if let Stmt::VarAssign { name, x_type, .. } = &stmts[0] {
        assert_eq!(name, "f");
        assert!(x_type.is_some());

        if let Some(Type::Function {
            params,
            return_type,
        }) = x_type
        {
            assert_eq!(params.len(), 1);
            assert_eq!(
                params[0],
                Param {
                    name: String::new(),
                    kind: ParamKind::Normal(Type::Double)
                }
            );
            assert_eq!(**return_type, Type::Double);
        } else {
            panic!("Expected Function type");
        }
    } else {
        panic!("Expected VarAssign");
    }
}

#[test]
fn test_multi_param_function_type() {
    // double, double -> int
    let code = "add: double, double -> int <- function(x: double, y: double): int { return(0) }";
    let stmts = parse_program(code);

    assert_eq!(stmts.len(), 1);
    if let Stmt::VarAssign { name, x_type, .. } = &stmts[0] {
        assert_eq!(name, "add");
        assert!(x_type.is_some());

        if let Some(Type::Function {
            params,
            return_type,
        }) = x_type
        {
            assert_eq!(params.len(), 2);
            assert_eq!(
                params[0],
                Param {
                    name: String::new(),
                    kind: ParamKind::Normal(Type::Double)
                }
            );
            assert_eq!(
                params[1],
                Param {
                    name: String::new(),
                    kind: ParamKind::Normal(Type::Double)
                }
            );
            assert_eq!(**return_type, Type::Int);
        } else {
            panic!("Expected Function type");
        }
    } else {
        panic!("Expected VarAssign");
    }
}

#[test]
fn test_higher_order_function_type() {
    // (double -> double) -> double
    let code = "apply: (double -> double) -> double <- function(f: function): double { return(0.0) }";
    let stmts = parse_program(code);

    assert_eq!(stmts.len(), 1);
    if let Stmt::VarAssign { name, x_type, .. } = &stmts[0] {
        assert_eq!(name, "apply");
        assert!(x_type.is_some());

        if let Some(Type::Function {
            params,
            return_type,
        }) = x_type
        {
            assert_eq!(params.len(), 1);

            // First param should be a function type (double -> double)
            if let Param {
                kind: ParamKind::Normal(Type::Function {
                    params: inner_params,
                    return_type: inner_return,
                }),
                ..
            } = &params[0]
            {
                assert_eq!(inner_params.len(), 1);
                assert_eq!(
                    inner_params[0],
                    Param {
                        name: String::new(),
                        kind: ParamKind::Normal(Type::Double)
                    }
                );
                assert_eq!(**inner_return, Type::Double);
            } else {
                panic!("Expected first parameter to be Function type");
            }

            assert_eq!(**return_type, Type::Double);
        } else {
            panic!("Expected Function type");
        }
    } else {
        panic!("Expected VarAssign");
    }
}

#[test]
fn test_function_returning_function() {
    // double -> (double -> double)
    let code =
        "make_adder: double -> (double -> double) <- function(x: double): function { return(0) }";
    let stmts = parse_program(code);

    assert_eq!(stmts.len(), 1);
    if let Stmt::VarAssign { name, x_type, .. } = &stmts[0] {
        assert_eq!(name, "make_adder");
        assert!(x_type.is_some());

        if let Some(Type::Function {
            params,
            return_type,
        }) = x_type
        {
            assert_eq!(params.len(), 1);
            assert_eq!(
                params[0],
                Param {
                    name: String::new(),
                    kind: ParamKind::Normal(Type::Double)
                }
            );

            // Return type should be a function (double -> double)
            if let Type::Function {
                params: ret_params,
                return_type: ret_return,
            } = &**return_type
            {
                assert_eq!(ret_params.len(), 1);
                assert_eq!(
                    ret_params[0],
                    Param {
                        name: String::new(),
                        kind: ParamKind::Normal(Type::Double)
                    }
                );
                assert_eq!(**ret_return, Type::Double);
            } else {
                panic!("Expected return type to be Function");
            }
        } else {
            panic!("Expected Function type");
        }
    } else {
        panic!("Expected VarAssign");
    }
}

#[test]
fn test_vector_function_type() {
    // vector<int> -> int
    let code = "sum_vec: vector<int> -> int <- function(v: vector<int>): int { return(0) }";
    let stmts = parse_program(code);

    assert_eq!(stmts.len(), 1);
    if let Stmt::VarAssign { name, x_type, .. } = &stmts[0] {
        assert_eq!(name, "sum_vec");
        assert!(x_type.is_some());

        if let Some(Type::Function {
            params,
            return_type,
        }) = x_type
        {
            assert_eq!(params.len(), 1);
            assert_eq!(
                params[0],
                Param {
                    name: String::new(),
                    kind: ParamKind::Normal(Type::Vector(Box::new(Type::Int)))
                }
            );
            assert_eq!(**return_type, Type::Int);
        } else {
            panic!("Expected Function type");
        }
    } else {
        panic!("Expected VarAssign");
    }
}
