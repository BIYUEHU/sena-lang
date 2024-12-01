use regex::Regex;
use std::iter::Peekable;
use std::str::Chars;

const IDENTIFIER_REGEX: &'static str = r"^[a-zA-Z_][a-zA-Z0-9_]*";
const NUMBER_REGEX: &'static str = r"^-?\d+(\.\d+)?([eE][+-]?\d+)?";

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // 简单标记 - 用字符匹配处理
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

    // 可能需要向前看一位的操作符 - 用字符匹配+peek处理
    Plus,         // +
    PlusEqual,    // +=
    Minus,        // -
    MinusEqual,   // -=
    Star,         // *
    StarEqual,    // *=
    Slash,        // /
    SlashEqual,   // /=
    Equal,        // =
    EqualEqual,   // ==
    Bang,         // !
    BangEqual,    // !=
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=

    // 复杂标记 - 用正则表达式处理
    Identifier(String), // 标识符
    Number(f64),        // 数字（包括整数和浮点数）

    // 特殊处理 - 用专门的函数处理
    String(String), // 字符串（需要处理转义字符）

    // 关键字 - 从标识符中识别
    Let,
    Fn,
    If,
    Else,
    Return,
    True,
    False,

    EOF,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

pub enum TokenResult {
    Token(Token),
    Err(String),
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current: String,
    line: usize,
    column: usize,
    identifier_regex: Regex,
    number_regex: Regex,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            input: source.chars().peekable(),
            current: String::new(),
            line: 1,
            column: 0,
            identifier_regex: Regex::new(IDENTIFIER_REGEX).unwrap(),
            number_regex: Regex::new(NUMBER_REGEX).unwrap(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.input.next() {
            self.current.push(c); // 修复：记录当前词素
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
                self.current.clear(); // 修复：清除空白字符
            } else {
                break;
            }
        }
    }

    // 处理简单标记 - 单字符标记
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

    // 处理操作符 - 需要向前看一位
    fn scan_operator(&mut self, first: char) -> TokenKind {
        match first {
            '+' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    TokenKind::PlusEqual
                } else {
                    TokenKind::Plus
                }
            }
            '-' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    TokenKind::MinusEqual
                } else {
                    TokenKind::Minus
                }
            }
            '*' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    TokenKind::StarEqual
                } else {
                    TokenKind::Star
                }
            }
            '/' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    TokenKind::SlashEqual
                } else {
                    TokenKind::Slash
                }
            }
            '=' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                }
            }
            '!' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                }
            }
            '>' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            }
            '<' => {
                if let Some(&'=') = self.peek() {
                    self.advance();
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            }
            _ => panic!("Invalid operator"),
        }
    }

    // 处理标识符和关键字
    fn scan_identifier(&mut self, first: char) -> TokenKind {
        let mut identifier = String::from(first);
        while let Some(&c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // 检查是否是关键字
        match identifier.as_str() {
            "let" => TokenKind::Let,
            "fn" => TokenKind::Fn,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Identifier(identifier),
        }
    }

    // 处理数字
    fn scan_number(&mut self, first: char) -> Result<TokenKind, String> {
        let mut number = String::from(first);
        let mut saw_dot = false;
        let mut saw_e = false;

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
                    // 修复：确保小数点后有数字
                    if !self.peek().map_or(false, |&c| c.is_digit(10)) {
                        return Err(
                            "Invalid number format: decimal point must be followed by digits"
                                .to_string(),
                        );
                    }
                }
                'e' | 'E' if !saw_e => {
                    saw_e = true;
                    number.push(c);
                    self.advance();
                    // 处理可能的 +/- 符号
                    if let Some(&sign) = self.peek() {
                        if sign == '+' || sign == '-' {
                            number.push(sign);
                            self.advance();
                            // 修复：确保指数部分有数字
                            if !self.peek().map_or(false, |&c| c.is_digit(10)) {
                                return Err("Invalid number format: exponent must contain digits"
                                    .to_string());
                            }
                        }
                    }
                }
                _ => break,
            }
        }

        number
            .parse::<f64>()
            .map(TokenKind::Number)
            .map_err(|_| "Invalid number format".to_string())
    }

    // 处理字符串
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

    // 重置当前词素
    fn reset_current(&mut self) {
        self.current.clear();
    }
}

impl Iterator for Lexer<'_> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<TokenResult> {
        self.reset_current(); // 修复：在处理新标记前重置current
        self.skip_whitespace();

        let start_column = self.column;

        // 处理文件结束
        if let None = self.peek() {
            return None;
        }

        let c = self.advance().unwrap();
        let kind = match c {
            // 1. 简单的单字符标记
            c if self.scan_simple_token(c).is_some() => self.scan_simple_token(c).unwrap(),

            // 2. 操作符（需要向前看）
            '+' | '-' | '*' | '/' | '=' | '!' | '>' | '<' => self.scan_operator(c),

            // 3. 标识符和关键字
            c if c.is_alphabetic() || c == '_' => self.scan_identifier(c),

            // 4. 数字
            c if c.is_digit(10)
                || (c == '-' && self.peek().map_or(false, |&next| next.is_digit(10))) =>
            {
                match self.scan_number(c) {
                    Ok(token_kind) => token_kind,
                    Err(err) => return Some(TokenResult::Err(err)),
                }
            }

            // 5. 字符串
            '"' => match self.scan_string() {
                Ok(token_kind) => token_kind,
                Err(err) => return Some(TokenResult::Err(err)),
            },

            // 6. 错误处理
            _ => return Some(TokenResult::Err(format!("Unexpected character: {}", c))),
        };

        Some(TokenResult::Token(Token {
            kind,
            lexeme: self.current.clone(),
            line: self.line,
            column: start_column,
        }))
    }
}

// 添加测试模块
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let mut lexer = Lexer::new("(){}[],.;:");
        // assert_eq!(
        //     lexer.next().unwrap(),
        //     TokenResult::Token(Token {
        //         kind: TokenKind::LeftParen,
        //         lexeme: "(".to_string(),
        //         line: 1,
        //         column: 0,
        //     })
        // );
        // ... 添加更多测试
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ += - -= * *= / /= = == ! != > >= < <=");
        // 添加运算符测试
    }

    #[test]
    fn test_identifiers_and_keywords() {
        let mut lexer = Lexer::new("let x = fn if else return true false");
        // 添加标识符和关键字测试
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Lexer::new("123 -123 123.456 1e10 -1.23e-4");
        // 添加数字测试
    }

    #[test]
    fn test_strings() {
        let mut lexer = Lexer::new(r#""hello" "hello\nworld" "escaped\"quote""#);
        // 添加字符串测试
    }
}
