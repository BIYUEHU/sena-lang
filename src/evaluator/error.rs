use crate::env::error::EnvError;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    UndefinedVariable(String),
    RedefinedVariable(String),
    TypeMismatch,
    UnsupportedOperator,
    ArityMismatch,
    NotCallable,
    PatternMatchFailure,
}

impl From<EnvError> for EvalError {
    fn from(error: EnvError) -> Self {
        match error {
            EnvError::RedefinedBinding(name) => EvalError::RedefinedVariable(name),
        }
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            EvalError::UndefinedVariable(name) => write!(f, "Undefined variable '{}'", name),
            EvalError::TypeMismatch => write!(f, "Type mismatch"),
            EvalError::UnsupportedOperator => write!(f, "Unsupported operator"),
            EvalError::ArityMismatch => write!(f, "Arity mismatch"),
            EvalError::NotCallable => write!(f, "Not callable"),
            EvalError::RedefinedVariable(name) => {
                write!(f, "Identifier '{}' is already defined", name)
            }
            EvalError::PatternMatchFailure => write!(f, "Pattern match failed"),
        }
    }
}
