use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LeftParen,          // (
    RightParen,         // )
    LeftBrace,          // {
    RightBrace,         // }
    LeftBracket,        // [
    RightBracket,       // ]
    Comma,              // ,
    Dot,                // .
    Semicolon,          // ;
    Colon,              // :
    Plus,               // +
    Sub,                // -
    Mul,                // *
    Div,                // /
    Pow,                // ^
    Mod,                // %
    Assign,             // =
    Equal,              // ==
    Not,                // !
    NotEqual,           // !=
    Greater,            // >
    GreaterEqual,       // >=
    Less,               // <
    LessEqual,          // <=
    Arrow,              // =>
    ThinArrow,          // ->
    Dollar,             // $
    Arm,                // |
    And,                // &&
    Or,                 // ||
    InfixIdent(String), // `xxx`
    InfixFixity(Vec<Token>),

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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Plus => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Pow => write!(f, "^"),
            Token::Mod => write!(f, "%"),
            Token::Assign => write!(f, "="),
            Token::Equal => write!(f, "=="),
            Token::Not => write!(f, "!"),
            Token::NotEqual => write!(f, "!="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Arrow => write!(f, "=>"),
            Token::ThinArrow => write!(f, "->"),
            Token::Dollar => write!(f, "$"),
            Token::Arm => write!(f, "|"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::InfixIdent(s) => write!(f, "`{}`", s),
            Token::InfixFixity(fixity) => write!(
                f,
                "{}",
                fixity
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join("")
            ),

            Token::Ident(s) => write!(f, "{}", s),
            Token::Float(n) => write!(f, "{}", n),
            Token::Int(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "{}", s),
            Token::Char(c) => write!(f, "{}", c),

            Token::Let => write!(f, "let"),
            Token::Type => write!(f, "type"),
            Token::Match => write!(f, "match"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::In => write!(f, "in"),
            Token::Import => write!(f, "import"),
            Token::Export => write!(f, "export"),
            Token::From => write!(f, "from"),
            Token::As => write!(f, "as"),
            Token::Abstract => write!(f, "abstract"),

            Token::LineComment(s) => write!(f, "//{}", s),
            Token::BlockComment(s) => write!(f, "/*{}*/", s),

            Token::Eof => write!(f, "EOF"),
        }
    }
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
