use crate::ast::{BinaryOp, Block, Expr};
use crate::types::{Param, ParamKind, Type};
use crate::lexer::Token;

use super::{ParseError, Parser};

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_bool()
    }

    pub(crate) fn parse_bool(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;
        loop {
            if self.match_token(&Token::Or) {
                let right = self.parse_comparison()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::Or,
                    right: Box::new(right),
                };
            } else if self.match_token(&Token::And) {
                let right = self.parse_comparison()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::And,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    pub(crate) fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        // Handle range ':' with lowest precedence among arithmetic, but above comparison may vary.
        // We'll make ':' lower than +,- for typical R-like precedence of sequence operator.
        let mut expr = self.parse_term()?;

        while let Some(op) = self.match_comparison_or_range() {
            let right = self.parse_term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    pub(crate) fn match_comparison_or_range(&mut self) -> Option<BinaryOp> {
        if self.match_token(&Token::LessEqual) {
            return Some(BinaryOp::LessEqual);
        }
        if self.match_token(&Token::Less) {
            return Some(BinaryOp::Less);
        }
        if self.match_token(&Token::Colon) {
            return Some(BinaryOp::Range);
        }
        if self.match_token(&Token::Equality) {
            return Some(BinaryOp::Equality)
        }
        if self.match_token(&Token::NotEqual) {
            return Some(BinaryOp::NotEqual)
        }
        if self.match_token(&Token::Greater) {
            return Some(BinaryOp::Greater);
        }
        if self.match_token(&Token::GreaterEqual) {
            return Some(BinaryOp::Greater);
        }
        None
    }

    pub(crate) fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;
        loop {
            if self.match_token(&Token::Plus) {
                let right = self.parse_factor()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::Plus,
                    right: Box::new(right),
                };
            } else if self.match_token(&Token::Minus) {
                let right = self.parse_factor()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::Minus,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    pub(crate) fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_call()?;
        loop {
            if self.match_token(&Token::Mul) {
                let right = self.parse_call()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::Mul,
                    right: Box::new(right),
                };
            } else if self.match_token(&Token::Div) {
                let right = self.parse_call()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::Div,
                    right: Box::new(right),
                };
            } else if self.match_token(&Token::Mod) {
                let right = self.parse_call()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::Mod,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    pub(crate) fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        loop {
            if self.match_token(&Token::LParen) {
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if self.match_token(&Token::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                self.consume(&Token::RParen)?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                };
            } else if self.match_token(&Token::LBracket) {
                let index = self.parse_expression()?;
                self.consume(&Token::RBracket)?;
                expr = Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    pub(crate) fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        use crate::ast::UnaryOp;

        // Check for unary operators
        if self.match_token(&Token::LogicalNot) {
            let operand = self.parse_unary()?; // Allow chaining: !!x
            return Ok(Expr::Unary {
                op: UnaryOp::LogicalNot,
                operand: Box::new(operand),
            });
        }

        // No unary operator, parse primary
        self.parse_primary()
    }

    pub(crate) fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            // Treat ... as a special identifier token in expressions (varargs forwarding)
            Some(Token::Dot) => {
                if self.peek_nth(1) == Some(&Token::Dot) && self.peek_nth(2) == Some(&Token::Dot) {
                    self.advance();
                    self.advance();
                    self.advance();
                    Ok(Expr::VarArgs)
                } else {
                    Err(ParseError::Expected(Token::Dot))
                }
            }
            Some(Token::Function) => {
                // function literal
                self.advance(); // consume 'function'
                self.consume(&Token::LParen)?;
                let mut params = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        // variable arguments '...'
                        if self.peek() == Some(&Token::Dot) {
                            let second = self.peek_nth(1).ok_or(ParseError::Eof)?;
                            let third = self.peek_nth(2).ok_or(ParseError::Eof)?;
                            if second == &Token::Dot && third == &Token::Dot {
                                // consume '...'
                                self.advance();
                                self.advance();
                                self.advance();
                                params.push(Param {
                                    name: "...".to_string(),
                                    kind: ParamKind::VarArgs,
                                });
                            } else {
                                return Err(ParseError::Expected(Token::Dot));
                            }
                        } else {
                            // parameter name
                            let pname = match self.peek() {
                                Some(Token::Identifier(s)) => s.clone(),
                                Some(tok) => return Err(ParseError::Unexpected(tok.clone())),
                                None => return Err(ParseError::Eof),
                            };
                            self.advance();

                            // require ':' type
                            let x_type = if self.match_token(&Token::Colon) {
                                self.parse_param_type()?
                            } else {
                                return Err(ParseError::Expected(Token::Colon));
                            };
                            params.push(Param {
                                name: pname,
                                kind: ParamKind::Normal(x_type),
                            });
                        }
                        if self.match_token(&Token::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                self.consume(&Token::RParen)?;
                // optional return type
                let mut ret_ty: Option<Type> = None;
                if self.match_token(&Token::Colon) {
                    ret_ty = Some(self.parse_x_type()?);
                }
                self.consume(&Token::LBrace)?;
                let body = self.parse_block_after_lbrace()?;
                Ok(Expr::FunctionDef {
                    params,
                    return_type: ret_ty,
                    body,
                })
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::Logical(true))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::Logical(false))
            }
            Some(Token::Number(n)) => {
                let n = n.clone();
                self.advance();
                Ok(Expr::Number(n))
            }
            Some(Token::Identifier(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::Identifier(s))
            }
            Some(Token::XString(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::XString(s))
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(&Token::RParen)?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            Some(Token::If) => {
                self.advance(); // consume 'if'
                self.parse_if_expr()
            }
            Some(tok) => Err(ParseError::Unexpected(tok.clone())),
            None => Err(ParseError::Eof),
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        self.skip_newlines();
        self.consume(&Token::LParen)?;
        self.skip_newlines();
        let condition = self.parse_expression()?;
        self.skip_newlines();
        self.consume(&Token::RParen)?;
        self.skip_newlines();

        // Parse then branch - can be single expression or block
        let then_branch = if self.check(&Token::LBrace) {
            self.consume(&Token::LBrace)?;
            self.skip_newlines();
            self.parse_block_after_lbrace()?
        } else {
            // Single expression without braces
            let expr = self.parse_expression()?;
            Block {
                stmts: vec![],
                tail_expr: Some(Box::new(expr)),
            }
        };

        self.skip_newlines();

        // Parse else branch (required for expressions)
        let else_branch = if self.peek() == Some(&Token::Else) && self.peek_nth(1) == Some(&Token::If) {
            self.consume(&Token::Else)?;
            self.consume(&Token::If)?;
            self.skip_newlines();

            // Recursive if expression
            let embedded_if = self.parse_if_expr()?;
            Some(Block {
                stmts: vec![],
                tail_expr: Some(Box::new(embedded_if)),
            })
        } else if self.peek() == Some(&Token::Else) {
            self.consume(&Token::Else)?;
            self.skip_newlines();

            if self.check(&Token::LBrace) {
                self.consume(&Token::LBrace)?;
                self.skip_newlines();
                Some(self.parse_block_after_lbrace()?)
            } else {
                // Single expression without braces
                let expr = self.parse_expression()?;
                Some(Block {
                    stmts: vec![],
                    tail_expr: Some(Box::new(expr)),
                })
            }
        } else {
            // If expression without else - will default to void/unit
            None
        };

        Ok(Expr::If {
            condition: Box::new(condition),
            then_branch,
            else_branch,
        })
    }
}
