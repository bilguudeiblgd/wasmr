use crate::ast::BinaryOp;
use crate::types::Type;
use crate::ir::{IRExpr, IRExprKind};
use wasm_encoder::{Function, Instruction};

use super::super::{context::LocalContext, WasmGenerator};

impl WasmGenerator {
    pub(crate) fn gen_vector_binary_op(&mut self,
                            func: &mut Function,
                            ctx: &LocalContext,
                            op: &BinaryOp,
                            left: &IRExpr,
                            right: &IRExpr) {
        // allowed
        // Type -> Vector[Type] op Type
        // Or -> Vector[Type] op Type
        // func.instruction(&Instruction::ArrayGet())

        match op {
            BinaryOp::Plus => {
                let callee = IRExpr {
                    kind: IRExprKind::Identifier("vector_add".to_string()),
                    ty: Type::Vector(Box::new(Type::Int)),
                };
                let args = vec![left.clone(), right.clone()];
                self.gen_call(func, ctx, &callee, &args);
            }
            _ => {}
        }
    }

    pub(crate) fn gen_binary_op(
        &mut self,
        func: &mut Function,
        ctx: &LocalContext,
        op: &BinaryOp,
        left: &IRExpr,
        right: &IRExpr,
    ) {
        if(matches!(left.ty, Type::Vector(_)) || matches!(right.ty, Type::Vector(_))) {
            self.gen_vector_binary_op(func, ctx, op, left, right);
            return;
        }
        // func.instruction(&)
        // func.instruction(&Instruction::Call())
        // Verify both operands have the same type (sanity check)
        // IR should guarantee this, but we check defensively
        if left.ty != right.ty {
            eprintln!(
                "Warning: Binary operation type mismatch: left={:?}, right={:?}",
                left.ty, right.ty
            );
        }

        // Use the left operand's type to determine which instruction to emit
        let ty = &left.ty;

        // sign of bad design
        if(matches!(op, BinaryOp::Range)) {
            self.gen_range(func, ctx, left, right);
            return;
        }

        self.gen_expr(func, ctx, left);
        self.gen_expr(func, ctx, right);


        match op {
            BinaryOp::Plus => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Add),
                    Type::Float => func.instruction(&Instruction::F32Add),
                    Type::Double => func.instruction(&Instruction::F64Add),
                    _ => func.instruction(&Instruction::I32Add), // fallback
                };
            }
            BinaryOp::Minus => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Sub),
                    Type::Float => func.instruction(&Instruction::F32Sub),
                    Type::Double => func.instruction(&Instruction::F64Sub),
                    _ => func.instruction(&Instruction::I32Sub),
                };
            }
            BinaryOp::Mul => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Mul),
                    Type::Float => func.instruction(&Instruction::F32Mul),
                    Type::Double => func.instruction(&Instruction::F64Mul),
                    _ => func.instruction(&Instruction::I32Mul),
                };
            }
            BinaryOp::Div => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32DivS),
                    Type::Float => func.instruction(&Instruction::F32Div),
                    Type::Double => func.instruction(&Instruction::F64Div),
                    _ => func.instruction(&Instruction::I32DivS),
                };
            }
            BinaryOp::Less => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32LtS),
                    Type::Float => func.instruction(&Instruction::F32Lt),
                    Type::Double => func.instruction(&Instruction::F64Lt),
                    _ => func.instruction(&Instruction::I32LtS),
                };
            }
            BinaryOp::LessEqual => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32LeS),
                    Type::Float => func.instruction(&Instruction::F32Le),
                    Type::Double => func.instruction(&Instruction::F64Le),
                    _ => func.instruction(&Instruction::I32LeS),
                };
            }
            BinaryOp::Greater => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32GtS),
                    Type::Float => func.instruction(&Instruction::F32Gt),
                    Type::Double => func.instruction(&Instruction::F64Gt),
                    _ => func.instruction(&Instruction::I32GtS),
                };
            },
            BinaryOp::GreaterEqual => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32GeS),
                    Type::Float => func.instruction(&Instruction::F32Ge),
                    Type::Double => func.instruction(&Instruction::F64Ge),
                    _ => func.instruction(&Instruction::I32GeS),
                };
            },
            BinaryOp::Equality => {
                match ty {
                    Type::Int => func.instruction(&Instruction::I32Eq),
                    Type::Float => func.instruction(&Instruction::F32Eq),
                    Type::Double => func.instruction(&Instruction::F64Eq),
                    _ => func.instruction(&Instruction::I32Eq),
                };
            }
            BinaryOp::Or => {
                // booleans are represented as i32 0/1
                func.instruction(&Instruction::I32Or);
            }
            BinaryOp::And => {
                func.instruction(&Instruction::I32And);
            }

            // #TODO: add if statement
            _ => {}
        }
    }

    pub(crate) fn gen_range(&mut self, func: &mut Function, _ctx: &LocalContext, left: &IRExpr, right: &IRExpr) {
        let start = match &left.kind {
            IRExprKind::Number(num) => {
                 num.parse::<i32>().unwrap()
            }
            _ => panic!("Range start must be a number")
        };
        let end = match &right.kind {
            IRExprKind::Number(num) => {
                num.parse::<i32>().unwrap()
            }
            _ => panic!("Range end must be a number")
        };

        // Push all elements
        for i in start..(end + 1) {
            func.instruction(&Instruction::I32Const(i));
        }

        // Create the data array
        let storage = self.storage_type_for(&Type::Int);
        let array_type_index = self.ensure_array_type(&storage);
        let array_size = (end - start + 1) as u32;
        func.instruction(&Instruction::ArrayNewFixed { array_type_index, array_size });

        // Push length
        func.instruction(&Instruction::I32Const(array_size as i32));

        // Create vector struct: (struct (field data) (field length))
        let vector_struct_index = self.ensure_vector_struct_type(&Type::Int);
        func.instruction(&Instruction::StructNew(vector_struct_index));
    }
}
