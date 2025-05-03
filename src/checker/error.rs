use crate::env::error::EnvError;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UndefinedVariable(String),
    RedefinedVariable(String),
    TypeMismatch {
        expected: String,
        found: String,
        context: String,
    },
    ArityMismatch {
        expected: usize,
        found: usize,
        context: String,
    },
    NotCallable {
        found: String,
    },
    InvalidOperation {
        operation: String,
        typ: String,
    },
    NotKind {
        found: String,
    },
}

impl From<EnvError> for TypeError {
    fn from(e: EnvError) -> Self {
        match e {
            EnvError::RedefinedBinding(name) => TypeError::RedefinedVariable(name),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TypeError::*;
        match self {
            UndefinedVariable(n) => write!(f, "Undefined variable '{}'", n),
            RedefinedVariable(n) => write!(f, "Variable '{}' is already defined", n),
            TypeMismatch {
                expected,
                found,
                context,
            } => write!(
                f,
                "Type mismatch in '{}': expected '{}', found '{}'",
                context, expected, found
            ),
            ArityMismatch {
                expected,
                found,
                context,
            } => write!(
                f,
                "Arity mismatch in '{}': expected {} arguments, found {}",
                context, expected, found
            ),
            NotCallable { found } => write!(f, "Not callable: '{}'", found),
            InvalidOperation { operation, typ } => {
                write!(f, "Invalid operation '{}' on type '{}'", operation, typ)
            }
            NotKind { found } => write!(f, "Not a kind: '{}'", found),
        }
    }
}
