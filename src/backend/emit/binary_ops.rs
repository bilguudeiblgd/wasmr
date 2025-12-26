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
        // Vector operations dispatch to runtime functions based on types
        // Name mangling scheme: system_vector_{op}___{left_type}__{right_type}
        // Examples:
        //   vector<int> + vector<int> -> system_vector_add___vec_int__vec_int
        //   vector<int> + int -> system_vector_add___vec_int__int
        //   int + vector<int> -> system_vector_add___int__vec_int

        let op_name = match op {
            BinaryOp::Plus => "add",
            BinaryOp::Minus => "sub",
            BinaryOp::Mul => "mul",
            BinaryOp::Div => "div",
            BinaryOp::Mod => "mod",
            _ => {
                eprintln!("Unsupported vector operation: {:?}", op);
                return;
            }
        };

        // Generate type suffixes for mangling
        let left_suffix = self.mangle_type_suffix(&left.ty);
        let right_suffix = self.mangle_type_suffix(&right.ty);

        // Build mangled function name (follows scheme: name___arg1__arg2)
        let mangled_name = format!("system_vector_{}___{}__{}",
                                   op_name, left_suffix, right_suffix);

        // Determine result type (promote if needed)
        let result_ty = match (&left.ty, &right.ty) {
            (Type::Vector(inner), _) | (_, Type::Vector(inner)) => Type::Vector(inner.clone()),
            _ => left.ty.clone(), // fallback
        };

        let callee = IRExpr {
            kind: IRExprKind::Identifier(mangled_name),
            ty: result_ty,
        };
        let args = vec![left.clone(), right.clone()];
        self.gen_call(func, ctx, &callee, &args);
    }

    /// Generate type suffix for name mangling
    /// Examples: Type::Int -> "int", Type::Vector(Int) -> "vec_int"
    fn mangle_type_suffix(&self, ty: &Type) -> String {
        match ty {
            Type::Int => "int".to_string(),
            Type::Double => "double".to_string(),
            Type::Logical => "logical".to_string(),
            Type::Vector(inner) => format!("vec_{}", self.mangle_type_suffix(inner)),
            _ => "any".to_string(),
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
        // Vector operations have different type rules (handled by runtime functions)
        if matches!(left.ty, Type::Vector(_)) || matches!(right.ty, Type::Vector(_)) {
            self.gen_vector_binary_op(func, ctx, op, left, right);
            return;
        }

        // For scalar operations, verify both operands have the same type
        // IR should guarantee this through ensure_ty and unify_numeric
        if left.ty != right.ty {
            eprintln!(
                "Warning: Scalar binary operation type mismatch: left={:?}, right={:?}",
                left.ty, right.ty
            );
        }

        // Use the left operand's type to determine which instruction to emit
        let ty = &left.ty;

        // sign of bad design
        if matches!(op, BinaryOp::Seq) {
            self.gen_seq(func, ctx, left, right);
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
            BinaryOp::Mod => {
                // Note: F64 modulo would need a library function; for now we only support i32
                Self::emit_typed_instruction(func, ty, Instruction::I32RemS, Instruction::I32RemS);
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

    pub(crate) fn gen_seq(&mut self, func: &mut Function, _ctx: &LocalContext, left: &IRExpr, right: &IRExpr) {
        // Check if both are constant numbers
        let is_const_start = matches!(&left.kind, IRExprKind::Number(_));
        let is_const_end = matches!(&right.kind, IRExprKind::Number(_));

        if is_const_start && is_const_end {
            // Constant range - generate at compile time
            let start = match &left.kind {
                IRExprKind::Number(num) => num.parse::<i32>().unwrap(),
                _ => unreachable!()
            };
            let end = match &right.kind {
                IRExprKind::Number(num) => num.parse::<i32>().unwrap(),
                _ => unreachable!()
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
        } else {
            // Dynamic range not yet supported
            // TODO: Implement dynamic range generation by either:
            // 1. Desugaring at IR level into a loop that builds the vector
            // 2. Adding a runtime function to create ranges
            // 3. Pre-registering temporary variables in variable collection pass
            panic!("Dynamic range expressions (e.g., 1:n where n is a variable) are not yet supported. Use constant ranges like 1:10 instead.");
        }
    }
}
