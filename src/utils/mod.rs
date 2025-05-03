use std::rc::Rc;

use crate::{
    checker::{
        ast::{
            CheckedCase, CheckedExpr, CheckedStmt, CheckedTypeVariant, CheckedTypeVariantFields,
            Program,
        },
        object::TypeObject,
        Checker,
    },
    env::new_checker_env,
    evaluator::{object::Object, Evaluator},
    lexer::Lexer,
    parser::{
        ast::{Expr, Kind, Stmt, TypeExpr, TypeVariantFields, UnsafeProgram},
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

pub fn to_checked_expr(expr: Expr) -> CheckedExpr {
    match expr {
        Expr::Ident(name) => CheckedExpr::Ident {
            value: name,
            type_annotation: TypeObject::Unknown,
        },
        Expr::Internal(name) => CheckedExpr::Internal {
            value: name,
            type_annotation: TypeObject::Unknown,
        },
        Expr::Literal(lit) => CheckedExpr::Literal {
            value: lit,
            type_annotation: TypeObject::Unknown,
        },
        Expr::Prefix(op, sub) => CheckedExpr::Prefix {
            op,
            expr: Box::new(to_checked_expr(*sub)),
            type_annotation: TypeObject::Unknown,
        },
        Expr::Infix(op, left, right) => CheckedExpr::Infix {
            op,
            left: Box::new(to_checked_expr(*left)),
            right: Box::new(to_checked_expr(*right)),
            type_annotation: TypeObject::Unknown,
        },
        Expr::Call { callee, params } => CheckedExpr::Call {
            callee: Box::new(to_checked_expr(*callee)),
            params: params
                .into_iter()
                .map(|param| to_checked_expr(param))
                .collect::<Vec<_>>(),
            type_annotation: TypeObject::Unknown,
        },
        Expr::Function { params, body, .. } => CheckedExpr::Function {
            params,
            body: Box::new(to_checked_expr(*body)),
            type_annotation: TypeObject::Unknown,
        },
        Expr::If {
            condition,
            then_branch,
            else_branch,
        } => CheckedExpr::If {
            condition: Box::new(to_checked_expr(*condition)),
            then_branch: Box::new(to_checked_expr(*then_branch)),
            else_branch: Box::new(to_checked_expr(*else_branch)),
            type_annotation: TypeObject::Unknown,
        },
        Expr::Match { expr, cases } => CheckedExpr::Match {
            expr: Box::new(to_checked_expr(*expr)),
            cases: {
                cases
                    .into_iter()
                    .map(|case| CheckedCase {
                        pattern: case.pattern.clone(),
                        guard: to_checked_expr(case.guard.clone()),
                        body: to_checked_expr(case.body),
                    })
                    .collect::<Vec<_>>()
            },
            type_annotation: TypeObject::Unknown,
        },
        Expr::LetIn {
            name, value, body, ..
        } => CheckedExpr::LetIn {
            name,
            value: Box::new(to_checked_expr(*value)),
            body: Box::new(to_checked_expr(*body)),
            type_annotation: TypeObject::Unknown,
        },
        Expr::Block(stmts) => CheckedExpr::Block {
            stmts: stmts
                .into_iter()
                .map(|stmt| to_checked_stmt(stmt))
                .collect::<Vec<_>>(),
            type_annotation: TypeObject::Unknown,
        },
    }
}

pub fn to_checked_stmt(stmt: Stmt) -> CheckedStmt {
    let env = new_checker_env();
    match stmt.clone() {
        Stmt::Let {
            name,
            type_annotation,
            value,
        } => CheckedStmt::Let {
            name: name.clone(),
            type_annotation: Checker::resolve(&type_annotation, Rc::clone(&env))
                .unwrap_or(TypeObject::Unknown),
            value: to_checked_expr(*value),
        },
        Stmt::Type {
            name,
            params,
            kind_annotation,
            variants,
        } => CheckedStmt::Type {
            name: name.clone(),
            params: params.clone(),
            kind_annotation: Checker::resolve_kind(
                &Checker::resolve(&kind_annotation, Rc::clone(&env)).unwrap_or(TypeObject::Unknown),
            )
            .unwrap_or(Kind::Star),
            variants: {
                variants
                    .iter()
                    .map(|variant| match variant.fields.clone() {
                        TypeVariantFields::Unit => CheckedTypeVariant {
                            name: variant.name.clone(),
                            fields: CheckedTypeVariantFields::Unit,
                        },
                        TypeVariantFields::Tuple(fields) => CheckedTypeVariant {
                            name: variant.name.clone(),
                            fields: CheckedTypeVariantFields::Tuple(
                                fields
                                    .iter()
                                    .map(|f| {
                                        Checker::resolve(f, Rc::clone(&env))
                                            .unwrap_or(TypeObject::Unknown)
                                    })
                                    .collect::<Vec<_>>(),
                            ),
                        },
                        _ => unimplemented!("record type variant"),
                    })
                    .collect::<Vec<_>>()
            },
        },
        Stmt::ImportAll { source, alias } => CheckedStmt::ImportAll { source, alias },
        Stmt::ImportSome { source, items } => CheckedStmt::ImportSome { source, items },
        Stmt::Export {
            body,
            only_abstract,
        } => CheckedStmt::Export {
            body: Box::new(to_checked_stmt(*body)),
            only_abstract: only_abstract,
        },
        Stmt::Expr(expr) => CheckedStmt::Expr(to_checked_expr(expr)),
    }
}
