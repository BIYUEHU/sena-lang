use crate::lexer::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: Token,
        line: usize,
        column: usize,
    },
    EndOfInput {
        expected: String,
    },
    InvalidSyntax {
        message: String,
        line: usize,
        column: usize,
    },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                line,
                column,
            } => write!(
                f,
                "Unexpected token: expect {}, found '{}' at line {}, column {}",
                expected, found, line, column
            ),
            ParseError::EndOfInput { expected } => {
                write!(f, "Unexpected end of input: expect {}", expected)
            }
            ParseError::InvalidSyntax {
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
