use crate::lexer::token::Token;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,   // $ `xxx` and custom operators
    Or,       // ||
    And,      // &&
    Equal,    // == !=
    Compare,  // < <= > >=
    List,     // : ++
    Sum,      // + -
    Product,  // * / %
    Exponent, // ^
    Mapping,  // ->
    Compose,  // .
    Prefix,   // -X !X
    Call,     // myFunction(x)
    Index,    // array[index]
    Highest,  // (x)
}

impl Precedence {
    pub fn from_token(token: &Token) -> Self {
        match token {
            Token::Plus | Token::Sub => Self::Sum,
            Token::Mul | Token::Div | Token::Mod => Self::Product,
            Token::Equal | Token::NotEqual => Self::Equal,
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => Self::Compare,
            Token::Or => Self::Or,
            Token::And => Self::And,
            Token::ThinArrow => Self::Mapping,
            Token::Arrow => Self::Lowest,
            Token::Not => Self::Prefix,
            Token::LeftParen => Self::Call,
            Token::LeftBracket => Self::Index,
            Token::RightParen | Token::RightBracket => Self::Highest,
            Token::Dot => Self::Compose,
            Token::Colon => Self::List,
            Token::InfixFixity(list) if *list == vec![Token::Plus, Token::Plus] => Self::List,
            Token::Pow => Self::Exponent,
            _ => Self::Lowest,
        }
    }
}
