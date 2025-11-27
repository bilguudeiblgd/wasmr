use crate::ast::{Expr, Stmt, Type};
use crate::lexer::Token;

use super::{ParseError, Parser};

impl Parser {
    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            // allow and skip newlines between top-level statements
            self.skip_newlines();
            if self.check(&Token::EOF) {
                break;
            }
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            // consume any trailing newlines after a statement
            self.skip_newlines();
        }
        Ok(stmts)
    }

    pub fn parse_if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.skip_newlines();
        self.consume(&Token::LParen)?;
        self.skip_newlines();
        let condition = self.parse_expression()?;
        self.skip_newlines();
        self.consume(&Token::RParen)?;
        // #TODO: can have 1 lined expression
        self.skip_newlines();
        self.consume(&Token::LBrace)?;
        self.skip_newlines();
        let then_branch = self.parse_block_after_lbrace()?;
        self.skip_newlines();
        let else_branch;
        if self.peek() == Some(&Token::Else) && self.peek_nth(1) == Some(&Token::If)  {
            self.consume(&Token::Else)?;
            self.consume(&Token::If)?;
            self.skip_newlines();

            if let Ok(embedded_if_statement) = self.parse_if_statement() {
                else_branch = Some(vec![embedded_if_statement]);
            } else {
                panic!("Error parsing embedded if statement");
            }
            return Ok(Stmt::If { condition, then_branch, else_branch });
        }
        if self.peek() == Some(&Token::Else) {
            self.consume(&Token::Else)?;
            self.consume(&Token::LBrace)?;
            else_branch = Some(self.parse_block_after_lbrace()?);
            return Ok(Stmt::If { condition, then_branch, else_branch });
        }
        Ok(Stmt::If { condition, then_branch, else_branch: None })
    }

    pub fn parse_for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&Token::LParen)?;
        let identifier = match self.peek() {
            Some(Token::Identifier(str)) => str.clone(),
            _ => return Err(ParseError::Expected(Token::Identifier("identifier".to_string()))),
        };
        self.advance();
        self.consume(&Token::In)?;

        if let Ok(expr) = self.parse_expression() {
            self.consume(&Token::RParen)?;
            self.skip_newlines();
            self.consume(&Token::LBrace)?;
            self.skip_newlines();
            let body = self.parse_block_after_lbrace()?;
            return Ok(Stmt::For {
                iter_name: identifier,
                iter_vector: expr,
                body,
            })
        }
        Err(ParseError::Expected(Token::Identifier("expression".to_string())))
    }

    pub fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        // assignment: name <- expr OR name[index] <- expr
        if self.lookahead_is_assignment() {
            // Parse the left side as an expression
            let lhs = self.parse_call()?;

            // Determine if it's a regular assignment or index assignment
            match lhs {
                Expr::Identifier(name) => {
                    // Regular variable assignment with optional type annotation
                    let x_type = if self.match_token(&Token::Colon) {
                        Some(self.parse_x_type()?)
                    } else {
                        None
                    };
                    self.consume(&Token::AssignArrow)?;
                    let value = self.parse_expression()?;
                    return Ok(Stmt::VarAssign { name, x_type, value });
                }
                Expr::Index { target, index } => {
                    // Index assignment (no type annotation allowed)
                    self.consume(&Token::AssignArrow)?;
                    let value = self.parse_expression()?;
                    return Ok(Stmt::IndexAssign {
                        target: *target,
                        index: *index,
                        value,
                    });
                }
                _ => {
                    return Err(ParseError::UnexpectedExplained(
                        "Invalid assignment target (expected identifier or index)".to_string(),
                    ));
                }
            }
        }

        if self.match_token(&Token::If) {
            return self.parse_if_statement()
        }

        if self.match_token(&Token::For) {
            return self.parse_for_statement()
        }

        // return statement
        if self.match_token(&Token::Return) {
            // Allow: return(), return(expr), or return expr
            if self.match_token(&Token::LParen) {
                if self.match_token(&Token::RParen) {
                    return Ok(Stmt::Return(None));
                }
                let value = self.parse_expression()?;
                self.consume(&Token::RParen)?;
                return Ok(Stmt::Return(Some(value)));
            } else {
                // return expr (without parens)
                let value = self.parse_expression()?;
                return Ok(Stmt::Return(Some(value)));
            }
        }
        // block statement
        if self.match_token(&Token::LBrace) {
            let body = self.parse_block_after_lbrace()?;
            return Ok(Stmt::Block(body));
        }
        // expression statement
        let expr = self.parse_expression()?;
        Ok(Stmt::ExprStmt(expr))
    }

    pub(crate) fn parse_block_after_lbrace(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        // skip any leading newlines inside the block
        self.skip_newlines();
        while !self.check(&Token::RBrace) && !self.check(&Token::EOF) {
            // also handle possible blank lines between stmts
            if self.check(&Token::RBrace) || self.check(&Token::EOF) {
                break;
            }
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            // allow newlines between statements
            self.skip_newlines();
        }
        // allow trailing newlines before closing brace
        self.skip_newlines();
        self.consume(&Token::RBrace)?;
        Ok(stmts)
    }

    pub(crate) fn parse_assignment(&mut self) -> Result<Stmt, ParseError> {
        let name = match self.peek() {
            Some(Token::Identifier(s)) => s.clone(),
            Some(tok) => return Err(ParseError::Unexpected(tok.clone())),
            None => return Err(ParseError::Eof),
        };
        self.advance(); // consume identifier

        let x_type = match self.peek() {
            Some(Token::Colon) => {
                self.consume(&Token::Colon)?;
                Some(self.parse_x_type()?)
            }
            _ => None,
        };

        self.consume(&Token::AssignArrow)?;
        let value = self.parse_expression()?;
        Ok(Stmt::VarAssign {
            name,
            x_type,
            value,
        })
    }

    // removed parse_function_def: function definitions are now expressions parsed in parse_primary

    pub(crate) fn parse_x_type(&mut self) -> Result<Type, ParseError> {
        let token = self.peek().ok_or(ParseError::Unexpected(Token::EOF))?;

        match token {
            Token::Type(type_name) => {
                let type_name = type_name.clone();
                self.advance();

                // Check for generic type syntax like vector<int>
                if self.match_token(&Token::Less) {
                    // Parse the inner type
                    let inner_type = self.parse_x_type()?;

                    // Expect closing '>'
                    self.consume(&Token::Greater)?;

                    // Map the type name to the appropriate generic type
                    match type_name.as_str() {
                        "vector" => Ok(Type::Vector(Box::new(inner_type))),
                        "list" => Ok(Type::Vector(Box::new(inner_type))), // list can be alias for vector
                        _ => Err(ParseError::UnexpectedExplained(format!(
                            "Type '{}' does not support generic parameters",
                            type_name
                        ))),
                    }
                } else {
                    // Simple type without generics
                    match type_name.as_str() {
                        "int" => Ok(Type::Int),
                        "float" => Ok(Type::Float),
                        "double" => Ok(Type::Double),
                        "string" => Ok(Type::String),
                        "char" => Ok(Type::Char),
                        "void" => Ok(Type::Void),
                        "bool" => Ok(Type::Bool),
                        "any" => Ok(Type::Any),
                        _ => Err(ParseError::UnexpectedExplained(format!(
                            "Unknown type: {}",
                            type_name
                        ))),
                    }
                }
            }
            _ => Err(ParseError::Unexpected(token.clone())),
        }
    }

    pub(crate) fn lookahead_is_assignment(&self) -> bool {
        if matches!(self.peek(), Some(Token::Identifier(_))) {
            // Check: ident <- or ident: type <-
            if matches!(self.peek_nth(1), Some(Token::AssignArrow) | Some(Token::Colon)) {
                return true;
            }
            // Check: ident[...] <-
            if matches!(self.peek_nth(1), Some(Token::LBracket)) {
                return self.lookahead_has_arrow_after_brackets();
            }
        }
        false
    }

    fn lookahead_has_arrow_after_brackets(&self) -> bool {
        let mut pos = 2; // Start after "identifier["
        let mut bracket_depth = 1;

        while bracket_depth > 0 {
            match self.peek_nth(pos) {
                Some(Token::LBracket) => bracket_depth += 1,
                Some(Token::RBracket) => bracket_depth -= 1,
                Some(Token::EOF) | None => return false,
                _ => {}
            }
            pos += 1;
        }

        // After closing ], check for <-
        matches!(self.peek_nth(pos), Some(Token::AssignArrow))
    }
}
