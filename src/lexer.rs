use std::fmt::{self, Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

use crate::token::{Token, TokenData};

pub enum LexerError {
    UnexpectedCharacter {
        ch: char,
        line: usize,
        column: usize,
    },
    InvalidSyntax {
        message: String,
        line: usize,
        column: usize,
    },
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            LexerError::UnexpectedCharacter { ch, line, column } => {
                write!(
                    f,
                    "Unexpected character: {} at line {}, column {}",
                    ch, line, column
                )
            }
            LexerError::InvalidSyntax {
                message,
                line,
                column,
            } => write!(
                f,
                "Syntax error: {} at line {}, column {}",
                message, line, column
            ),
        }
    }
}

pub type LexerResult = Result<TokenData, LexerError>;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    ch: char,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            input: source.chars().peekable(),
            ch: '\0',
            line: 1,
            column: 0,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                ' ' | '\t' | '\r' => {
                    self.column += 1;
                    self.read();
                }
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                }
                _ => continue,
            }
        }
    }

    fn read(&mut self) {
        self.ch = self.input.next().unwrap_or('\0');
        self.column += 1;
    }

    fn peek(&mut self) -> char {
        self.read();
        self.ch
    }

    fn next(&mut self) -> Option<LexerResult> {
        self.skip_whitespace();

        let start_column = self.column;

        let token = match self.peek() {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            ',' => Token::Comma,
            '.' => Token::Dot,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '+' => Token::Plus,
            '-' => match self.peek() {
                '>' => Token::ThinArrow,
                _ => Token::Sub,
            },
            '*' => Token::Mul,
            '/' => match self.peek() {
                '/' => {
                    let mut comment = String::new();
                    loop {
                        if self.peek() == '\n' {
                            break;
                        }
                        comment.push(self.ch);
                    }
                    Token::LineComment(comment)
                }
                '*' => {
                    let mut comment = String::new();
                    loop {
                        if self.peek() == '*' {
                            if self.peek() == '/' {
                                break;
                            }
                            comment.push('*');
                        }
                        comment.push(self.ch);
                    }
                    Token::BlockComment(comment)
                }
                _ => Token::Div,
            },
            '%' => Token::Mod,
            '=' => match self.peek() {
                '=' => Token::Equal,
                '>' => Token::Arrow,
                _ => Token::Assign,
            },
            '!' => match self.peek() {
                '=' => Token::NotEqual,
                _ => Token::Not,
            },
            '>' => match self.peek() {
                '=' => Token::GreaterEqual,
                _ => Token::Greater,
            },
            '<' => match self.peek() {
                '=' => Token::LessEqual,
                _ => Token::Less,
            },
            '&' => match self.peek() {
                '&' => Token::And,
                _ => {
                    return Some(Err(LexerError::UnexpectedCharacter {
                        ch: '&',
                        line: self.line,
                        column: start_column,
                    }))
                }
            },
            '|' => match self.peek() {
                '|' => Token::Or,
                _ => Token::Arm,
            },

            ch if ch.is_alphabetic() || ch == '_' => self.scan_identifier(ch),

            ch if ch.is_digit(10)
                || (ch == '-' && self.peek().map_or(false, |&next| next.is_digit(10))) =>
            {
                match self.scan_number(ch) {
                    Ok(token_kind) => token_kind,
                    Err(message) => {
                        return Err(LexerError {
                            message,
                            line: self.line,
                            column: start_column,
                        })
                    }
                }
            }

            '"' => match self.scan_string() {
                Ok(token_kind) => token_kind,
                Err(err) => {
                    return Err(TokenError {
                        message: err,
                        line: self.line,
                        column: start_column,
                    })
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

        Some(Ok(TokenData {
            token,
            line: self.line,
            column: start_column,
        }))
    }

    fn scan_identifier(&mut self, start: char) -> Token {
        let mut identifier = String::from(start);
        loop {
            match self.peek() {
                ch if ch.is_alphanumeric() || ch == '_' => {
                    identifier.push(ch);
                }
                _ => break,
            }
        }

        match identifier.as_str() {
            "let" => Token::Let,
            "type" => Token::Type,
            "match" => Token::Match,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "in" => Token::In,
            _ => Token::Ident(identifier),
        }
    }

    fn scan_number(&mut self, start: char) -> Option<Token> {
        let mut number = String::from(start);
        let mut saw_dot = false;
        let mut is_float = false;

        loop {
            match self.peek() {
                '0'..='9' => {
                    number.push(self.ch);
                }
                // '_' => {
                //     self.advance();
                // }
                '.' if !saw_dot => {
                    saw_dot = true;
                    number.push(self.ch);
                    if !self.peek().is_digit(10) {
                        return None;
                    }
                    is_float = true;
                }
                _ => break,
            }
        }

        if is_float {
            number
                .parse::<f64>()
                .map(|x| Some(Token::Float(x)))
                .unwrap_or(None)
        } else {
            number.parse::<i64>().map(|x| Some(Token::Int(x))).unwrap()
        }
    }

    fn scan_string(&mut self) -> Result<Token, String> {
        let mut string = String::new();

        loop {
            match ch {
                '"' => {
                    break;
                }
                '\\' => match self.advance() {
                    Some('n') => string.push('\n'),
                    Some('t') => string.push('\t'),
                    Some('r') => string.push('\r'),
                    Some('\\') => string.push('\\'),
                    Some('"') => string.push('"'),
                    Some(_) => return Err("Invalid escape sequence".to_string()),
                    None => return Err("Unterminated escape sequence".to_string()),
                },
                Some('\n') => {
                    return Err("Unterminated string: new line in string literal".to_string())
                }
                Some(c) => string.push(c),
                None => return Err("Unterminated string".to_string()),
            }
        }

        while let Some(c) = self.advance() {
            match c {
                '"' => return Ok(Token::String(string)),
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

    fn scan_char(&mut self) -> Result<Token, String> {
        let c = self.advance().unwrap();
        if let Some('\'') = self.advance() {
            Ok(Token::Char(c))
        } else {
            Err("Invalid character literal".to_string())
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexerResult;

    fn next(&mut self) -> Option<LexerResult> {
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

        Some(Ok(TokenData {
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
