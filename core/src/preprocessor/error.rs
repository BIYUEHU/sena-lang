use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum PreprocessError {
    CircularImport(String),
    FileNotFound(String),
    InvalidDirective(String),
    UnbalancedConditional,
    MacroExpansionError(String),
    IoError(std::io::Error),
}

impl From<std::io::Error> for PreprocessError {
    fn from(err: std::io::Error) -> Self {
        PreprocessError::IoError(err)
    }
}

impl Display for PreprocessError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use PreprocessError::*;
        match self {
            CircularImport(name) => write!(f, "Circular import detected: {}", name),
            FileNotFound(path) => write!(f, "File not found: {}", path),
            InvalidDirective(dir) => write!(f, "Invalid directive: {}", dir),
            UnbalancedConditional => write!(f, "Unbalanced conditional"),
            MacroExpansionError(msg) => write!(f, "Macro expansion error: {}", msg),
            IoError(err) => err.fmt(f),
        }
    }
}
