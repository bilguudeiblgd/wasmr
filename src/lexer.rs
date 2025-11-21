use crate::is_builtin_type_name;

pub struct Lexer {}

// #TODO: handle comments and strings

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {}
    }

    // Look at the current character without consuming it
    fn peek(&self, chars: &Vec<char>, current: usize) -> Option<char> {
        chars.get(current).copied()
    }

    // Look ahead n characters without consuming
    fn peek_n(&self, chars: &Vec<char>, current: usize, n: usize) -> Option<char> {
        chars.get(current + n).copied()
    }

    // Consume the current character and advance the cursor by one
    fn consume(&self, chars: &Vec<char>, current: &mut usize) -> Option<char> {
        let ch = chars.get(*current).copied();
        if ch.is_some() {
            *current += 1;
        }
        ch
    }

    pub fn lex(&self, string: &String) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut current: usize = 0;
        let chars: Vec<char> = string.chars().collect();

        while current < chars.len() {
            let c = match self.peek(&chars, current) {
                Some(ch) => ch,
                None => break,
            };

            // Handle whitespace: newlines become a distinct token, other whitespace is skipped
            if c.is_whitespace() {
                if c == '\n' {
                    tokens.push(Token::Newline);
                }
                self.consume(&chars, &mut current);
                continue;
            }

            // Two-character tokens and '<' handling
            if c == '<' {
                if let Some('-') = self.peek_n(&chars, current, 1) {
                    tokens.push(Token::AssignArrow);
                    // consume both '<' and '-'
                    self.consume(&chars, &mut current);
                    self.consume(&chars, &mut current);
                    continue;
                }
                if let Some('=') = self.peek_n(&chars, current, 1) {
                    tokens.push(Token::LessEqual);
                    // consume both '<' and '='
                    self.consume(&chars, &mut current);
                    self.consume(&chars, &mut current);
                    continue;
                }
                // single '<' - could be less-than operator or type angle bracket
                tokens.push(Token::Less);
                self.consume(&chars, &mut current);
                continue;
            }

            match c {
                '+' => {
                    tokens.push(Token::Plus);
                    self.consume(&chars, &mut current);
                }
                '-' => {
                    tokens.push(Token::Minus);
                    self.consume(&chars, &mut current);
                }
                '*' => {
                    tokens.push(Token::Mul);
                    self.consume(&chars, &mut current);
                }
                '/' => {
                    tokens.push(Token::Div);
                    self.consume(&chars, &mut current);
                }
                '|' => {
                    tokens.push(Token::Or);
                    self.consume(&chars, &mut current);
                }
                '&' => {
                    tokens.push(Token::And);
                    self.consume(&chars, &mut current);
                }
                '(' => {
                    tokens.push(Token::LParen);
                    self.consume(&chars, &mut current);
                }
                ')' => {
                    tokens.push(Token::RParen);
                    self.consume(&chars, &mut current);
                }
                '{' => {
                    tokens.push(Token::LBrace);
                    self.consume(&chars, &mut current);
                }
                '}' => {
                    tokens.push(Token::RBrace);
                    self.consume(&chars, &mut current);
                }
                '>' => {
                    tokens.push(Token::Greater);
                    if(self.peek_n(&chars, current, 1) == Some('=')) {
                        self.consume(&chars, &mut current);
                        tokens.push(Token::GreaterEqual);
                    }
                    self.consume(&chars, &mut current);
                }
                ':' => {
                    tokens.push(Token::Colon);
                    self.consume(&chars, &mut current);
                }
                ',' => {
                    tokens.push(Token::Comma);
                    self.consume(&chars, &mut current);
                }
                '.' => {
                    tokens.push(Token::Dot);
                    self.consume(&chars, &mut current);
                }
                '=' => {
                    match self.peek_n(&chars, current, 1) {
                        None => {
                            panic!("Unexpected end of file while parsing assignment, '='");
                        }
                        Some('=') => {
                            tokens.push(Token::Equality);
                            self.consume(&chars, &mut current);
                            self.consume(&chars, &mut current);
                        }
                        Some(_) => {
                            panic!("Unexpected value after '='");
                        },
                    }
                }
                '[' => {
                    tokens.push(Token::LBracket);
                    self.consume(&chars, &mut current);
                }
                ']' => {
                    tokens.push(Token::RBracket);
                    self.consume(&chars, &mut current);
                }
                '0'..='9' => {
                    let (num, new_current) = self.parse_number(&chars, current);
                    tokens.push(Token::Number(num));
                    current = new_current; // already points to first non-number char
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let (identifier_name, new_current) = self.parse_identifier(&chars, current);
                    // Keywords
                    match identifier_name.as_str() {
                        "if" => tokens.push(Token::If),
                        "else" => tokens.push(Token::Else),
                        "while" => tokens.push(Token::While),
                        "repeat" => tokens.push(Token::Repeat),
                        "for" => tokens.push(Token::For),
                        "next" => tokens.push(Token::Next),
                        "break" => tokens.push(Token::Break),
                        "function" => tokens.push(Token::Function),
                        "return" => tokens.push(Token::Return),
                        "in" => tokens.push(Token::In),
                        // Treat built-in type names specially
                        _ => {
                            if is_builtin_type_name(&identifier_name) {
                                tokens.push(Token::Type(identifier_name))
                            } else {
                                tokens.push(Token::Identifier(identifier_name))
                            }
                        }
                    }
                    current = new_current; // already points to the first non-ident char
                }
                '"' => {
                    self.consume(&chars, &mut current);
                    let (str, new_current) = self.parse_string(&chars, current);
                    tokens.push(Token::XString(str));
                    current = new_current;
                }
                _ => {
                    // Unknown character: skip it for now
                    self.consume(&chars, &mut current);
                }
            }
        }
        tokens.push(Token::EOF); // End of file token
        tokens
    }

    fn parse_number(&self, chars: &Vec<char>, start: usize) -> (String, usize) {
        let mut num_str = String::new();
        let mut current = start;

        while let Some(ch) = self.peek(chars, current) {
            if ch.is_ascii_digit() || ch == '.' {
                num_str.push(ch);
                // safe because peek returned Some
                let _ = self.consume(chars, &mut current);
            } else {
                break;
            }
        }

        (num_str, current)
    }

    fn parse_identifier(&self, chars: &Vec<char>, start: usize) -> (String, usize) {
        let mut ident_str = String::new();
        let mut current = start;
        while let Some(ch) = self.peek(chars, current) {
            if ch.is_alphanumeric() || ch == '_' || ch == '.' {
                ident_str.push(ch);
                let _ = self.consume(chars, &mut current);
            } else {
                break;
            }
        }
        (ident_str, current)
    }

    fn parse_string(&self, chars: &Vec<char>, start: usize) -> (String, usize) {
        let mut str_str = String::new();
        let mut current = start;
        while let Some(ch) = self.peek(chars, current) {
            if ch != '"' {
                str_str.push(ch);
                let _ = self.consume(chars, &mut current);
            } else {
                self.consume(chars, &mut current);
                break;
            }
        }
        (str_str, current)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Single-char punctuation/operators
    Plus,
    Minus,
    Mul,
    Div,
    Or,  // '|'
    And, // '&'
    Equality,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Comma,
    Less,
    Greater,  // '>'
    GreaterEqual,
    LessEqual,
    Dot,

    // Multi-char tokens
    AssignArrow, // <-

    // Literals and identifiers
    Identifier(String),
    Type(String),
    Number(String),
    XString(String),

    // Keywords
    Function,
    Return,
    If,
    Else,
    While,
    Repeat,
    For,
    Next,
    Break,
    True,
    False,
    Inf,
    In,

    // Special
    Newline,
    EOF,
}
