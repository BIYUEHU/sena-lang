use std::fmt::{self, Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Semicolon,    // ;
    Colon,        // :

    Plus,         // +
    Sub,          // -
    Mul,          // *
    Div,          // /
    Mod,          // %
    Assign,       // =
    Equal,        // ==
    Not,          // !
    NotEqual,     // !=
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=
    ReturnValue,  // =>
    ReturnType,   // ->
    Pipe,         // |
    And,          // &&
    Or,           // ||

    Identifier(String),
    Float(f64),
    Int(i64),
    String(String),
    Char(char),

    Let,
    Type,
    Match,
    If,
    Then,
    Else,
    In,

    LineComment(String),
    BlockComment(String),
}

pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "Token: {:?} | lexeme: {} | line: {}, column: {}",
            self.kind, self.lexeme, self.line, self.column
        )
    }
}

pub struct TokenError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "Syntax error: {} | line: {}, column: {}",
            self.message, self.line, self.column
        )
    }
}

type TokenResult = Result<Token, TokenError>;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current: String,
    line: usize,
    column: usize,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            input: source.chars().peekable(),
            current: String::new(),
            line: 1,
            column: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.input.next() {
            self.current.push(c);
            self.column += 1;
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            }
            Some(c)
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
                self.current.clear();
            } else {
                break;
            }
        }
    }

    fn scan_simple_token(&mut self, c: char) -> Option<TokenKind> {
        match c {
            '(' => Some(TokenKind::LeftParen),
            ')' => Some(TokenKind::RightParen),
            '{' => Some(TokenKind::LeftBrace),
            '}' => Some(TokenKind::RightBrace),
            '[' => Some(TokenKind::LeftBracket),
            ']' => Some(TokenKind::RightBracket),
            ',' => Some(TokenKind::Comma),
            '.' => Some(TokenKind::Dot),
            ';' => Some(TokenKind::Semicolon),
            ':' => Some(TokenKind::Colon),
            _ => None,
        }
    }

    fn scan_operator(&mut self, start: char) -> Option<TokenKind> {
        match start {
            '+' => Some(TokenKind::Plus),
            '-' => {
                if let Some(&'>') = self.peek() {
                    self.advance();
                    Some(TokenKind::ReturnType)
                } else {
                    Some(TokenKind::Sub)
                }
            }
            '*' => Some(TokenKind::Mul),
            '/' => match self.peek() {
                Some('/') => {
                    self.advance();
                    let mut comment = String::new();
                    while let Some(&c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        comment.push(c);
                        self.advance();
                    }
                    Some(TokenKind::LineComment(comment))
                }
                Some('*') => {
                    self.advance();
                    let mut comment = String::new();
                    while let Some(&c) = self.peek() {
                        if c == '*' {
                            self.advance();
                            if let Some(&'/') = self.peek() {
                                self.advance();
                                break;
                            }
                        }
                        comment.push(c);
                        self.advance();
                    }
                    Some(TokenKind::BlockComment(comment))
                }
                _ => Some(TokenKind::Div),
            },
            '%' => Some(TokenKind::Mod),
            '=' => match self.peek() {
                Some(&'=') => {
                    self.advance();
                    Some(TokenKind::Equal)
                }
                Some(&'>') => {
                    self.advance();
                    Some(TokenKind::ReturnValue)
                }
                _ => Some(TokenKind::Assign),
            },
            '!' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Some(TokenKind::NotEqual)
                } else {
                    Some(TokenKind::Not)
                }
            }
            '>' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Some(TokenKind::GreaterEqual)
                } else {
                    Some(TokenKind::Greater)
                }
            }
            '<' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Some(TokenKind::LessEqual)
                } else {
                    Some(TokenKind::Less)
                }
            }
            '&' => {
                if let Some(&'&') = self.peek() {
                    self.advance();
                    Some(TokenKind::And)
                } else {
                    None
                }
            }
            '|' => {
                if let Some(&'|') = self.peek() {
                    self.advance();
                    Some(TokenKind::Or)
                } else {
                    Some(TokenKind::Pipe)
                }
            }
            _ => None,
        }
    }

    fn scan_identifier(&mut self, start: char) -> TokenKind {
        let mut identifier = String::from(start);
        while let Some(&c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(c);
                self.advance();
            } else {
                break;
            }
        }

        match identifier.as_str() {
            "let" => TokenKind::Let,
            "type" => TokenKind::Type,
            "match" => TokenKind::Match,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "in" => TokenKind::In,
            _ => TokenKind::Identifier(identifier),
        }
    }

    fn scan_number(&mut self, start: char) -> Result<TokenKind, String> {
        let mut number = String::from(start);
        let mut saw_dot = false;
        let mut saw_e = false;
        let mut is_float = false;

        while let Some(&c) = self.peek() {
            match c {
                '0'..='9' => {
                    number.push(c);
                    self.advance();
                }
                '_' => {
                    self.advance();
                }
                '.' if !saw_dot && !saw_e => {
                    saw_dot = true;
                    number.push(c);
                    self.advance();
                    if !self.peek().map_or(false, |&c| c.is_digit(10)) {
                        return Err(
                            "Invalid number format: decimal point must be followed by digits"
                                .to_string(),
                        );
                    }
                    is_float = true;
                }
                _ => break,
            }
        }

        if is_float {
            return number
                .parse::<f64>()
                .map(TokenKind::Float)
                .map_err(|_| "Invalid number format".to_string());
        } else {
            return number
                .parse::<i64>()
                .map(TokenKind::Int)
                .map_err(|_| "Invalid number format".to_string());
        }
    }

    fn scan_string(&mut self) -> Result<TokenKind, String> {
        let mut string = String::new();

        while let Some(c) = self.advance() {
            match c {
                '"' => return Ok(TokenKind::String(string)),
                '\\' => {
                    if let Some(next) = self.advance() {
                        let escaped = match next {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            '\\' => '\\',
                            '"' => '"',
                            _ => return Err("Invalid escape sequence".to_string()),
                        };
                        string.push(escaped);
                    } else {
                        return Err("Unterminated escape sequence".to_string());
                    }
                }
                '\n' => return Err("Unterminated string: new line in string literal".to_string()),
                _ => string.push(c),
            }
        }

        Err("Unterminated string".to_string())
    }

    fn scan_char(&mut self) -> Result<TokenKind, String> {
        let c = self.advance().unwrap();
        if let Some('\'') = self.advance() {
            Ok(TokenKind::Char(c))
        } else {
            Err("Invalid character literal".to_string())
        }
    }

    fn reset_current(&mut self) {
        self.current.clear();
    }
}

impl Iterator for Lexer<'_> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<TokenResult> {
        self.reset_current();
        self.skip_whitespace();

        let start_column = self.column;

        if let None = self.peek() {
            return None;
        }

        let c = self.advance().unwrap();
        let kind = match c {
            c if self.scan_simple_token(c).is_some() => self.scan_simple_token(c).unwrap(),

            '+' | '-' | '*' | '/' | '%' | '=' | '!' | '>' | '<' | '|' | '&' => {
                match self.scan_operator(c) {
                    Some(token_kind) => token_kind,
                    None => {
                        return Some(Err(TokenError {
                            message: format!("Unexpected operator: {}", c),
                            line: self.line,
                            column: start_column,
                        }))
                    }
                }
            }
            c if c.is_alphabetic() || c == '_' => self.scan_identifier(c),

            c if c.is_digit(10)
                || (c == '-' && self.peek().map_or(false, |&next| next.is_digit(10))) =>
            {
                match self.scan_number(c) {
                    Ok(token_kind) => token_kind,
                    Err(message) => {
                        return Some(Err(TokenError {
                            message,
                            line: self.line,
                            column: start_column,
                        }))
                    }
                }
            }

            '"' => match self.scan_string() {
                Ok(token_kind) => token_kind,
                Err(err) => {
                    return Some(Err(TokenError {
                        message: err,
                        line: self.line,
                        column: start_column,
                    }))
                }
            },

            '\'' => match self.scan_char() {
                Ok(token_kind) => token_kind,
                Err(err) => {
                    return Some(Err(TokenError {
                        message: err,
                        line: self.line,
                        column: start_column,
                    }))
                }
            },

            _ => {
                return Some(Err(TokenError {
                    message: format!("Unexpected character: {}", c),
                    line: self.line,
                    column: start_column,
                }))
            }
        };

        Some(Ok(Token {
            kind,
            lexeme: self.current.clone(),
            line: self.line,
            column: start_column,
        }))
    }
}

#[cfg(test)]
mod tests {
    // pub use super::*;
}
