use crate::{
    checker::Checker,
    evaluator::{object::Object, Evaluator},
    lexer::Lexer,
    parser::{
        ast::{Stmt, TypeExpr, UnsafeProgram},
        error::ParseError,
        Parser,
    },
};

pub fn is_uppercase_first_letter(str: &str) -> bool {
    str.chars().next().map_or(false, |c| c.is_uppercase())
}
pub fn get_arrow_type(params: Vec<TypeExpr>, return_type: TypeExpr) -> TypeExpr {
    match params.len() {
        0 => TypeExpr::Arrow(
            Box::new(TypeExpr::Con("Unit".to_string())),
            Box::new(return_type),
        ),
        1 => TypeExpr::Arrow(Box::new(params[0].clone()), Box::new(return_type)),
        _ => TypeExpr::Arrow(
            Box::new(params[0].clone()),
            Box::new(get_arrow_type(params[1..].to_vec(), return_type)),
        ),
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

pub fn get_checked_ast(code: &str, checker: &mut Checker) -> Result<UnsafeProgram, String> {
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

pub fn is_op_char(ch: char) -> bool {
    matches!(
        ch,
        '+' | '-'
            | '*'
            | '/'
            | '%'
            | '='
            | '<'
            | '>'
            | '!'
            | '&'
            | '|'
            | '^'
            | ':'
            | '$'
            | '?'
            | '.'
    )
}
