use crate::{checker::error::TypeError, env::error::EnvError};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    UndefinedVariable(String),
    RedefinedVariable(String),
    // TODO: replace type error with type mismatch
    TypeMismatch,
    UnsupportedOperator,
    ArityMismatch,
    PatternMatchFailure,
    NotCallable,
    NotAType(String),
    TypeError(TypeError),
    ModuleNotFound(String),
    ModuleError(String),
}

impl From<EnvError> for EvalError {
    fn from(error: EnvError) -> Self {
        match error {
            EnvError::RedefinedBinding(name) => EvalError::RedefinedVariable(name),
        }
    }
}

impl From<TypeError> for EvalError {
    fn from(error: TypeError) -> Self {
        EvalError::TypeError(error)
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
            EvalError::NotAType(name) => write!(f, "Not a type: {}", name),
            EvalError::TypeError(error) => write!(f, "Type error: {}", error),
            EvalError::ModuleNotFound(name) => write!(f, "Module not found: {}", name),
            EvalError::ModuleError(message) => write!(f, "Module error: {}", message),
        }
    }
}
