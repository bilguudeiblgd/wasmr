use rty_compiler::ast::{Stmt, Expr, Type, BinaryOp};
use rty_compiler::codegen::compile_to_wasm;

#[test]
fn generates_non_empty_wasm() {
    // program: return 40 + 2
    let program = vec![
        Stmt::Return(Some(Expr::Binary{
            left: Box::new(Expr::Number("40".into())),
            op: BinaryOp::Plus,
            right: Box::new(Expr::Number("2".into())),
        }))
    ];

    let bytes = compile_to_wasm(program);
    assert!(bytes.len() > 8, "wasm bytes should be non-trivial");

    // optional: minimal validation of header "\0asm"
    assert_eq!(&bytes[0..4], b"\0asm");
}
