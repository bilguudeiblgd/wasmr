use super::super::type_resolver::TypeResolver;
use super::super::types::{IRBlock, IRProgram, IRStmt, TyResult};
use crate::ast::{Block, Stmt as AstStmt};
use crate::types::Type;

/// Internal lowering context that uses a borrowed TypeResolver for environments and type ops.
pub(crate) struct LowerCtx<'a> {
    pub(crate) tr: &'a mut TypeResolver,
}

impl<'a> LowerCtx<'a> {
    /// Create a new lowering context with the given type resolver
    pub(crate) fn new(tr: &'a mut TypeResolver) -> Self {
        Self { tr }
    }

    /// Lower a whole program (list of AST statements) to typed IR statements.
    pub(crate) fn lower_program(&mut self, program: Vec<AstStmt>) -> TyResult<IRProgram> {
        // Functions are now stored in scope_stack during normal statement processing
        // No need for separate function registration pass

        let mut out = Vec::with_capacity(program.len());
        for s in program {
            out.push(self.lower_stmt(s)?);
        }
        Ok(IRProgram::new(out))
    }

    /// Lower a list of statements (used for older code paths)
    #[allow(dead_code)]
    pub(crate) fn lower_block(&mut self, stmts: Vec<AstStmt>) -> TyResult<Vec<IRStmt>> {
        let mut out = Vec::with_capacity(stmts.len());
        for s in stmts {
            out.push(self.lower_stmt(s)?);
        }
        Ok(out)
    }

    /// Lower a Block (statements + optional tail expression) to IRBlock
    pub(crate) fn lower_ast_block(&mut self, block: Block) -> TyResult<IRBlock> {
        // Lower all statements
        let mut stmts = Vec::with_capacity(block.stmts.len());
        for s in block.stmts {
            stmts.push(self.lower_stmt(s)?);
        }

        // Lower tail expression if present
        let (tail_expr, ty) = if let Some(expr) = block.tail_expr {
            let ir_expr = self.lower_expr(*expr)?;
            let ty = ir_expr.ty.clone();
            (Some(Box::new(ir_expr)), ty)
        } else {
            (None, Type::Void)
        };

        Ok(IRBlock {
            stmts,
            tail_expr,
            ty,
        })
    }
}
