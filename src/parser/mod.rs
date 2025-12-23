mod expressions;
mod statements;

use crate::lexer::Token;
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

    // Token utility methods
    pub(crate) fn is_at_end(&self) -> bool {
        matches!(self.peek(), Some(Token::EOF) | None)
    }

    pub(crate) fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    pub(crate) fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.tokens.get(self.pos.saturating_sub(1))
    }

    // #TODO: we don't get full-coverage for branches
    pub(crate) fn check(&self, token: &Token) -> bool {
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
            | (Some(Token::SuperAssignArrow), Token::SuperAssignArrow)
            | (Some(Token::Arrow), Token::Arrow)
            | (Some(Token::Function), Token::Function)
            | (Some(Token::Return), Token::Return)
            | (Some(Token::Identifier(_)), Token::Identifier(_))
            | (Some(Token::EOF), Token::EOF)
            | (Some(Token::If), Token::If)
            | (Some(Token::Else), Token::Else)
            | (Some(Token::For), Token::For)
            | (Some(Token::While), Token::While)
            | (Some(Token::Equality), Token::Equality)
            | (Some(Token::NotEqual), Token::NotEqual)
            | (Some(Token::LogicalNot), Token::LogicalNot)
            | (Some(Token::In), Token::In)
            | (Some(Token::Newline), Token::Newline) => true,
            | (None, _) => false,
            _ => false,
        }
    }

    pub(crate) fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(crate) fn consume(&mut self, token: &Token) -> Result<(), ParseError> {
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

    pub(crate) fn skip_newlines(&mut self) {
        while self.match_token(&Token::Newline) {}
    }

    pub(crate) fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n)
    }
}
