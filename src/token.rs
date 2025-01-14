use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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
    Arrow,        // =>
    ThinArrow,    // ->
    Pipe,         // |
    And,          // &&
    Or,           // ||

    Ident(String),
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

pub struct TokenData {
    pub kind: Token,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Display for TokenData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "TokenData: {:?} | lexeme: {} | line: {}, column: {}",
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

pub type TokenResult = Result<TokenData, TokenError>;
