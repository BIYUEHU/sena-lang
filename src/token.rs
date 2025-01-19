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
    Arm,          // |
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
    Import,
    Export,
    From,
    As,
    Abstract,

    LineComment(String),
    BlockComment(String),

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenData {
    pub token: Token,
    pub line: usize,
    pub column: usize,
}

impl Display for TokenData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "TokenData: {:?} at line {}, column {}",
            self.token, self.line, self.column
        )
    }
}
