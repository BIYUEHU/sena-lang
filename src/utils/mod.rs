use mihama_core::{
    checker::{ast::Program, Checker},
    evaluator::{object::Object, Evaluator},
    lexer::Lexer,
    parser::{
        ast::{Stmt, UnsafeProgram},
        error::ParseError,
        Parser,
    },
    transpiler::{javascript::JavaScriptTranspiler, Transpiler},
    utils::to_checked_stmt,
};
use std::fmt::Display;

pub enum RunningMode {
    Lexer,
    Parser,
    Checker,
    Evaluator,
    UnsafeEvaluator,
}

impl From<String> for RunningMode {
    fn from(s: String) -> Self {
        let str: String = s.into();
        match str.to_lowercase().as_str() {
            "lexer" => RunningMode::Lexer,
            "parser" => RunningMode::Parser,
            "checker" => RunningMode::Checker,
            "evaluator" => RunningMode::Evaluator,
            _ => RunningMode::UnsafeEvaluator,
        }
    }
}

impl From<RunningMode> for String {
    fn from(mode: RunningMode) -> Self {
        match mode {
            RunningMode::Lexer => "Lexer",
            RunningMode::Parser => "Parser",
            RunningMode::Checker => "Checker",
            RunningMode::Evaluator => "Evaluator",
            RunningMode::UnsafeEvaluator => "UnsafeEvaluator",
        }
        .into()
    }
}

impl Display for RunningMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

pub fn parse_code(code: &str) -> Result<Vec<Result<Stmt, ParseError>>, String> {
    let mut error = None;
    let token_data = Lexer::new(code)
        .filter_map(|result| match result {
            Ok(token_data) => Some(token_data),
            Err(err) => {
                if error.is_none() {
                    error = Some(format!("Lexer error: {}", err));
                }
                None
            }
        })
        .collect();

    if let Some(err) = error {
        Err(err)
    } else {
        Ok(Parser::new(token_data, true).collect::<Vec<_>>())
    }
}

pub fn get_ast(code: &str) -> Result<UnsafeProgram, String> {
    let mut error = None;
    let program = parse_code(code)?
        .into_iter()
        .filter_map(|result| match result {
            Ok(stmt) => Some(stmt),
            Err(err) => {
                if error.is_none() {
                    error = Some(format!("Parser error: {}", err));
                }
                None
            }
        })
        .collect::<UnsafeProgram>();

    if let Some(err) = error {
        Err(err)
    } else {
        Ok(program)
    }
}

pub fn get_checked_ast(code: &str, checker: &mut Checker) -> Result<Program, String> {
    Ok(checker
        .check(&get_ast(code)?)
        .map_err(|err| format!("Checker error: {}", err))?)
}

pub fn eval_code(
    code: &str,
    checker: &mut Checker,
    evaluator: &mut Evaluator,
) -> Result<Object, String> {
    evaluator
        .eval(&get_checked_ast(code, checker)?)
        .map_err(|err| format!("Evaluator error: {}", err))
}

pub fn unsafe_eval_code(code: &str, evaluator: &mut Evaluator) -> Result<Object, String> {
    Ok(evaluator
        .eval_unsafe(&get_ast(code)?)
        .map_err(|err| format!("Evaluator error: {}", err))?)
}

pub fn transofrm_code(code: &str) -> Result<String, String> {
    Ok(JavaScriptTranspiler::new().transpile(
        &get_ast(code)?
            .into_iter()
            .map(|stmt| to_checked_stmt(stmt.clone()))
            .collect::<Vec<_>>(),
    ))
}
