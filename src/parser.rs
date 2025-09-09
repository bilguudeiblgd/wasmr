use crate::lexer::Token;
use crate::ast::{BinaryOp, Expr, Stmt, Param, Type};
use crate::map_builtin_type;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    Eof,
    Unexpected(Token),
    Expected,
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
        self.parse_comparison()
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

    fn check(&self, token: &Token) -> bool {
        match (self.peek(), token) {
            (Some(Token::Plus), Token::Plus)
            | (Some(Token::Minus), Token::Minus)
            | (Some(Token::Mul), Token::Mul)
            | (Some(Token::Div), Token::Div)
            | (Some(Token::LParen), Token::LParen)
            | (Some(Token::RParen), Token::RParen)
            | (Some(Token::LBrace), Token::LBrace)
            | (Some(Token::RBrace), Token::RBrace)
            | (Some(Token::Colon), Token::Colon)
            | (Some(Token::Comma), Token::Comma)
            | (Some(Token::Less), Token::Less)
            | (Some(Token::LessEqual), Token::LessEqual)
            | (Some(Token::AssignArrow), Token::AssignArrow)
            | (Some(Token::Function), Token::Function)
            | (Some(Token::Return), Token::Return)
            | (Some(Token::EOF), Token::EOF)
            | (Some(Token::Newline), Token::Newline) => true,
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

    pub fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        // function definition: name <- function(...)
        if self.lookahead_is_function_def() {
            return self.parse_function_def();
        }
        // assignment: name <- expr
        if self.lookahead_is_assignment() {
            return self.parse_assignment();
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
            if self.check(&Token::RBrace) || self.check(&Token::EOF) { break; }
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
            _ => return Err(ParseError::Eof)
        };

        self.consume(&Token::AssignArrow)?;
        let value = self.parse_expression()?;
        Ok(Stmt::VarAssign { name, x_type, value })
    }

    fn parse_function_def(&mut self) -> Result<Stmt, ParseError> {
        // name <- function ( params ) [: return_type]? { body }
        let name = match self.peek() {
            Some(Token::Identifier(s)) => s.clone(),
            Some(tok) => return Err(ParseError::Unexpected(tok.clone())),
            None => return Err(ParseError::Eof),
        };
        self.advance(); // name
        self.consume(&Token::AssignArrow)?;
        self.consume(&Token::Function)?;
        self.consume(&Token::LParen)?;
        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                // parameter name
                let pname = match self.peek() {
                    Some(Token::Identifier(s)) => s.clone(),
                    Some(tok) => return Err(ParseError::Unexpected(tok.clone())),
                    None => return Err(ParseError::Eof),
                };
                self.advance();
                // optional type: ":" Type/Identifier

                let x_type = if self.match_token(&Token::Colon) {
                    self.parse_x_type()?
                } else {
                    return Err(ParseError::Expected);
                };
                params.push(Param { name: pname, ty: x_type });
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
        Ok(Stmt::FunctionDef { name, params, return_type: ret_ty, body })
    }

    fn parse_x_type(&mut self) -> Result<Type, ParseError> {
        match self.peek() {
            Some(Token::Type(s)) => {
                let t = s.clone();
                self.advance();
                let x_type = map_builtin_type(&t);
                match x_type {
                    Some(value) => Ok(value),
                    None => Err(ParseError::Expected)
                }
            }
            Some(tok) => Err(ParseError::Unexpected(tok.clone())),
            None => Err(ParseError::Eof),
        }
    }

    fn lookahead_is_assignment(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier(_)))
            && (matches!(self.peek_nth(1), Some(Token::AssignArrow))
                || matches!(self.peek_nth(1), Some(Token::Colon)))
    }

    fn lookahead_is_function_def(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier(_)))
            && matches!(self.peek_nth(1), Some(Token::AssignArrow))
            && matches!(self.peek_nth(2), Some(Token::Function))
    }

    fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n)
    }
}
