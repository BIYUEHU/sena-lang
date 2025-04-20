use crate::{
    checker::Checker,
    evaluator::{object::Object, Evaluator},
    lexer::Lexer,
    parser::{
        ast::{Program, Stmt, TypeExpr},
        ParseError, Parser,
    },
};

pub fn is_uppercase_first_letter(str: &str) -> bool {
    str.chars().next().map_or(false, |c| c.is_uppercase())
}

pub fn format_type_name(type_name: String, type_params: Vec<Box<TypeExpr>>) -> String {
    if type_params.is_empty() {
        type_name
    } else {
        format!(
            "{}({})",
            type_name,
            type_params
                .into_iter()
                .map(|t| { t.to_string() })
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

pub enum RunningMode {
    Lexer,
    Parser,
    Checker,
    Evaluator,
    UnsafeEvaluator,
}

impl<T: Into<String>> From<T> for RunningMode {
    fn from(s: T) -> Self {
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

pub fn get_ast(code: &str) -> Result<Program, String> {
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
        .collect::<Program>();

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
        .eval(&get_ast(code)?)
        .map_err(|err| format!("Evaluator error: {}", err))?)
}
