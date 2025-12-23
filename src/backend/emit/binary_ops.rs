use crate::ast::BinaryOp;
use crate::types::Type;
use crate::ir::{IRExpr, IRExprKind};
use wasm_encoder::{Function, Instruction};

use super::super::{context::LocalContext, WasmGenerator};

impl WasmGenerator {
    fn emit_typed_instruction(
        func: &mut Function,
        ty: &Type,
        i32_instr: Instruction,
        f64_instr: Instruction,
    ) {
        match ty {
            Type::Int => func.instruction(&i32_instr),
            Type::Double => func.instruction(&f64_instr),
            _ => func.instruction(&i32_instr), // fallback
        };
    }

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
        if matches!(left.ty, Type::Vector(_)) || matches!(right.ty, Type::Vector(_)) {
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
        if matches!(op, BinaryOp::Range) {
            self.gen_range(func, ctx, left, right);
            return;
        }

        self.gen_expr(func, ctx, left);
        self.gen_expr(func, ctx, right);


        match op {
            BinaryOp::Plus => {
                Self::emit_typed_instruction(func, ty, Instruction::I32Add, Instruction::F64Add);
            }
            BinaryOp::Minus => {
                Self::emit_typed_instruction(func, ty, Instruction::I32Sub, Instruction::F64Sub);
            }
            BinaryOp::Mul => {
                Self::emit_typed_instruction(func, ty, Instruction::I32Mul, Instruction::F64Mul);
            }
            BinaryOp::Div => {
                Self::emit_typed_instruction(func, ty, Instruction::I32DivS, Instruction::F64Div);
            }
            BinaryOp::Less => {
                Self::emit_typed_instruction(func, ty, Instruction::I32LtS, Instruction::F64Lt);
            }
            BinaryOp::LessEqual => {
                Self::emit_typed_instruction(func, ty, Instruction::I32LeS, Instruction::F64Le);
            }
            BinaryOp::Greater => {
                Self::emit_typed_instruction(func, ty, Instruction::I32GtS, Instruction::F64Gt);
            }
            BinaryOp::GreaterEqual => {
                Self::emit_typed_instruction(func, ty, Instruction::I32GeS, Instruction::F64Ge);
            }
            BinaryOp::Equality => {
                Self::emit_typed_instruction(func, ty, Instruction::I32Eq, Instruction::F64Eq);
            }
            BinaryOp::NotEqual => {
                Self::emit_typed_instruction(func, ty, Instruction::I32Ne, Instruction::F64Ne);
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
