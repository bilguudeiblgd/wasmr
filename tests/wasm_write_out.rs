use rty_compiler::ast::{Expr, Stmt};
use rty_compiler::codegen::compile_and_write;

#[test]
fn writes_wasm_file_to_data_wasm_out() {
    // program: result <- 1 + 2
    let program = vec![Stmt::VarAssign {
        name: "result".to_string(),
        x_type: None,
        value: Expr::Binary {
            left: Box::new(Expr::Number("1".into())),
            op: rty_compiler::ast::BinaryOp::Plus,
            right: Box::new(Expr::Number("2".into())),
        },
        is_super_assign: false,
    }];

    let path = compile_and_write(program, "test_out").expect("should write wasm file");
    assert!(path.exists(), "output file should exist");

    // Read back and check wasm header
    let bytes = std::fs::read(&path).expect("should read wasm file");
    assert!(bytes.len() >= 4);
    assert_eq!(&bytes[0..4], b"\0asm");

    // Clean up after test to avoid polluting working tree
    // std::fs::remove_file(&path).ok();
}
