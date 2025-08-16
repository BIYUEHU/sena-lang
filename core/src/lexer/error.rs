use super::token::TokenData;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
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
