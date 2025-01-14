use crate::token::{Token, TokenData, TokenError, TokenResult};
use std::iter::Peekable;
use std::str::Chars;

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

    fn scan_simple_token(&mut self, c: char) -> Option<Token> {
        match c {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            '{' => Some(Token::LeftBrace),
            '}' => Some(Token::RightBrace),
            '[' => Some(Token::LeftBracket),
            ']' => Some(Token::RightBracket),
            ',' => Some(Token::Comma),
            '.' => Some(Token::Dot),
            ';' => Some(Token::Semicolon),
            ':' => Some(Token::Colon),
            _ => None,
        }
    }

    fn scan_operator(&mut self, start: char) -> Option<Token> {
        match start {
            '+' => Some(Token::Plus),
            '-' => {
                if let Some(&'>') = self.peek() {
                    self.advance();
                    Some(Token::ThinArrow)
                } else {
                    Some(Token::Sub)
                }
            }
            '*' => Some(Token::Mul),
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
                    Some(Token::LineComment(comment))
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
                    Some(Token::BlockComment(comment))
                }
                _ => Some(Token::Div),
            },
            '%' => Some(Token::Mod),
            '=' => match self.peek() {
                Some(&'=') => {
                    self.advance();
                    Some(Token::Equal)
                }
                Some(&'>') => {
                    self.advance();
                    Some(Token::Arrow)
                }
                _ => Some(Token::Assign),
            },
            '!' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Some(Token::NotEqual)
                } else {
                    Some(Token::Not)
                }
            }
            '>' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Some(Token::GreaterEqual)
                } else {
                    Some(Token::Greater)
                }
            }
            '<' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    Some(Token::LessEqual)
                } else {
                    Some(Token::Less)
                }
            }
            '&' => {
                if let Some(&'&') = self.peek() {
                    self.advance();
                    Some(Token::And)
                } else {
                    None
                }
            }
            '|' => {
                if let Some(&'|') = self.peek() {
                    self.advance();
                    Some(Token::Or)
                } else {
                    Some(Token::Pipe)
                }
            }
            _ => None,
        }
    }

    fn scan_identifier(&mut self, start: char) -> Token {
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

    fn scan_number(&mut self, start: char) -> Result<Token, String> {
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
                .map(Token::Float)
                .map_err(|_| "Invalid number format".to_string());
        } else {
            return number
                .parse::<i64>()
                .map(Token::Int)
                .map_err(|_| "Invalid number format".to_string());
        }
    }

    fn scan_string(&mut self) -> Result<Token, String> {
        let mut string = String::new();

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
