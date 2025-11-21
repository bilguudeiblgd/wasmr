use crate::ast::{BinaryOp, Expr, Param, ParamKind, Stmt, Type};
use crate::lexer::Token;
use crate::map_builtin_type;
use std::cmp::PartialEq;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    Eof,
    Unexpected(Token),
    Expected(Token),
    UnexpectedExplained(String)
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_bool()
    }

    fn parse_bool(&mut self) -> Result<Expr, ParseError> {
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

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
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

    fn match_comparison_or_range(&mut self) -> Option<BinaryOp> {
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
        if self.match_token(&Token::Greater) {
            return Some(BinaryOp::Greater);
        }
        if self.match_token(&Token::GreaterEqual) {
            return Some(BinaryOp::Greater);
        }
        None
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
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

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
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
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;
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
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
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
                                self.parse_x_type()?
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
            Some(tok) => Err(ParseError::Unexpected(tok.clone())),
            None => Err(ParseError::Eof),
        }
    }

    // Utility methods
    fn is_at_end(&self) -> bool {
        matches!(self.peek(), Some(Token::EOF) | None)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.tokens.get(self.pos.saturating_sub(1))
    }

    // #TODO: we don't get full-coverage for branches
    fn check(&self, token: &Token) -> bool {
        match (self.peek(), token) {
            (Some(Token::Plus), Token::Plus)
            | (Some(Token::Minus), Token::Minus)
            | (Some(Token::Mul), Token::Mul)
            | (Some(Token::Div), Token::Div)
            | (Some(Token::Or), Token::Or)
            | (Some(Token::And), Token::And)
            | (Some(Token::LParen), Token::LParen)
            | (Some(Token::RParen), Token::RParen)
            | (Some(Token::LBrace), Token::LBrace)
            | (Some(Token::RBrace), Token::RBrace)
            | (Some(Token::LBracket), Token::LBracket)
            | (Some(Token::RBracket), Token::RBracket)
            | (Some(Token::Colon), Token::Colon)
            | (Some(Token::Dot), Token::Dot)
            | (Some(Token::Greater), Token::Greater)
            | (Some(Token::Comma), Token::Comma)
            | (Some(Token::Less), Token::Less)
            | (Some(Token::LessEqual), Token::LessEqual)
            | (Some(Token::AssignArrow), Token::AssignArrow)
            | (Some(Token::Function), Token::Function)
            | (Some(Token::Return), Token::Return)
            | (Some(Token::Identifier(_)), Token::Identifier(_))
            | (Some(Token::EOF), Token::EOF)
            | (Some(Token::If), Token::If)
            | (Some(Token::Else), Token::Else)
            | (Some(Token::For), Token::For)
            | (Some(Token::Equality), Token::Equality)
            | (Some(Token::In), Token::In)
            | (Some(Token::Newline), Token::Newline) => true,
            | (None, _) => false,
            _ => false,
        }
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume(&mut self, token: &Token) -> Result<(), ParseError> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            match self.peek() {
                // #TODO: make meaning message from this area.
                Some(t) => Err(ParseError::Unexpected(t.clone())),
                None => Err(ParseError::Eof),
            }
        }
    }

    fn skip_newlines(&mut self) {
        while self.match_token(&Token::Newline) {}
    }
}

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
        if(self.peek() == Some(&Token::Else) && self.peek_nth(1) == Some(&Token::If) ) {
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
        if(self.peek() == Some(&Token::Else)) {
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
        // assignment: name <- expr
        if self.lookahead_is_assignment() {
            return self.parse_assignment();
        }

        if(self.match_token(&Token::If)) {
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

    fn parse_block_after_lbrace(&mut self) -> Result<Vec<Stmt>, ParseError> {
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

    fn parse_assignment(&mut self) -> Result<Stmt, ParseError> {
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

    fn parse_x_type(&mut self) -> Result<Type, ParseError> {
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
    
    fn lookahead_is_assignment(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier(_)))
            && (matches!(self.peek_nth(1), Some(Token::AssignArrow))
                || matches!(self.peek_nth(1), Some(Token::Colon)))
    }

    // lookahead_is_function_def no longer needed: function is an expression

    fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n)
    }
}
