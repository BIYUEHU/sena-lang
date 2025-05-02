use crate::checker::ast::{CheckedExpr, CheckedStmt};
use crate::checker::object::TypeObject;
use crate::env::{Env, EvaluatorEnv};
use crate::lexer::token::Token;
use crate::parser::ast::{Literal, Pattern, TypeExpr, TypeVariantFields};
use crate::utils::is_uppercase_first_letter;
use error::EvalError;
use object::{Object, PrettyPrint};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::UNIX_EPOCH;

pub mod error;
pub mod object;

pub type CustomFunc = Box<dyn Fn(Vec<Object>) -> Result<Object, EvalError>>;
pub type CustomFuncs = HashMap<String, CustomFunc>;

pub struct Evaluator {
    env: Rc<RefCell<EvaluatorEnv>>,
    // TODO: improved it
    custom_funcs: CustomFuncs,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<EvaluatorEnv>>) -> Self {
        let mut custom_funcs: CustomFuncs = HashMap::new();
        custom_funcs.insert(
            "print".to_string(),
            Box::new(|args| {
                println!(
                    "{}",
                    args.iter()
                        .map(|arg| arg.pretty_print())
                        .collect::<Vec<_>>()
                        .join(" ")
                );
                Ok(Object::Unit)
            }),
        );
        custom_funcs.insert(
            "get_timestrap".to_string(),
            Box::new(|_| Ok(Object::Int(UNIX_EPOCH.elapsed().unwrap().as_millis() as i64))),
        );
        Evaluator { env, custom_funcs }
    }

    pub fn set_custom_func(&mut self, name: String, func: CustomFunc) {
        self.custom_funcs.insert(name, func);
    }

    fn eval_internal_func(&self, identify: String, args: Vec<Object>) -> Result<Object, EvalError> {
        if let Some(func) = self.custom_funcs.get(identify.as_str()) {
            func(args)
        } else {
            Err(EvalError::UndefinedVariable(identify.to_string()))
        }
    }

    fn eval_func(&self, func: &CheckedExpr, args: Vec<Object>) -> Result<Object, EvalError> {
        match self.eval_expr(func)? {
            Object::Function {
                params, body, env, ..
            } => {
                let check_params = || {
                    if params.len() != args.len() {
                        Err(EvalError::ArityMismatch)
                    } else {
                        Ok(())
                    }
                };
                if let CheckedExpr::Internal { value, .. } = *body.into() {
                    self.eval_internal_func(value, args)
                } else {
                    check_params()?;
                    let local_env = Env::extend(env);
                    for (p, a) in params.iter().zip(args.into_iter()) {
                        local_env.borrow_mut().insert_bind(p.0.clone(), a)?;
                    }
                    Evaluator::new(local_env).eval_expr(&body.into())
                }
            }
            _ => Err(EvalError::NotCallable),
        }
    }

    fn eval_stmt(&mut self, stmt: &CheckedStmt) -> Result<Object, EvalError> {
        match stmt {
            CheckedStmt::Let {
                name,
                type_annotation: _,
                value,
            } => {
                if self.env.borrow().get_bind(name).is_some() {
                    return Err(EvalError::RedefinedVariable(name.clone()));
                }
                let result = self.eval_expr(value)?;
                self.env.borrow_mut().insert_bind(name.clone(), result)?;
                Ok(Object::Unit)
            }
            CheckedStmt::Type {
                name,
                type_annotation: _,
                type_params,
                variants,
            } => {
                let constructors: Vec<(String, Vec<TypeExpr>)> = variants
                    .iter()
                    .map(|variant| {
                        (
                            variant.name.clone(),
                            match &variant.fields {
                                TypeVariantFields::Unit => vec![],
                                TypeVariantFields::Tuple(types) => {
                                    types.iter().map(|t| *t.clone()).collect()
                                }
                                TypeVariantFields::Record(fields) => {
                                    fields.iter().map(|(_, t)| *t.clone()).collect()
                                }
                            },
                        )
                    })
                    .collect();

                let mut env = self.env.borrow_mut();

                let type_params_count = match type_params {
                    None => {
                        env.insert_bind(
                            name.clone(),
                            Object::Type(TypeObject::ADT {
                                name: name.clone(),
                                type_params: vec![],
                                constructors: constructors.clone(),
                            }),
                        )?;
                        0
                    }
                    Some(type_params) => {
                        env.insert_bind(
                            name.clone(),
                            Object::TypeConstructor {
                                type_name: name.clone(),
                                type_params: type_params.clone(),
                                constructors: constructors.clone(),
                            },
                        )?;
                        type_params.len()
                    }
                };
                let type_params = {
                    let mut type_params = vec![];
                    for _ in 0..type_params_count {
                        type_params.push(Box::new(TypeObject::Kind.into()));
                    }
                    type_params
                };
                for variant in variants {
                    let variant_name = variant.name.clone();
                    match &variant.fields {
                        TypeVariantFields::Unit => {
                            let adt_value = Object::ADT {
                                type_name: name.clone(),
                                type_params: type_params.clone(),
                                variant: variant_name.clone(),
                                fields: vec![],
                            };
                            env.insert_bind(variant_name.clone(), adt_value)?;
                        }
                        TypeVariantFields::Tuple(fields) => {
                            let constructor = Object::ADTConstructor {
                                type_name: name.clone(),
                                type_params: type_params.clone(),
                                variant: variant_name.clone(),
                                fields: fields.clone(),
                            };
                            env.insert_bind(variant_name.clone(), constructor)?;
                        }
                        TypeVariantFields::Record(fields) => {
                            let constructor = Object::ADTConstructor {
                                type_name: name.clone(),
                                type_params: type_params.clone(),
                                variant: variant_name.clone(),
                                fields: fields
                                    .iter()
                                    .map(|(_, t)| Box::new(*t.clone()))
                                    .collect::<Vec<_>>(),
                            };
                            env.insert_bind(variant_name.clone(), constructor)?;
                        }
                    }
                }
                Ok(Object::Unit)
            }
            CheckedStmt::Expr(expr) => self.eval_expr(expr),
            _ => Ok(Object::Unit),
        }
    }

    fn eval_expr(&self, expr: &CheckedExpr) -> Result<Object, EvalError> {
        match expr {
            CheckedExpr::Ident { value, .. } | CheckedExpr::Internal { value, .. } => {
                if let Some(val) = self.env.borrow().get_bind(&value) {
                    Ok(val)
                } else {
                    Err(EvalError::UndefinedVariable(*value))
                }
            }
            CheckedExpr::Literal { value, .. } => match value {
                Literal::Int(i) => Ok(Object::Int(*i)),
                Literal::Float(f) => Ok(Object::Float(*f)),
                Literal::String(s) => Ok(Object::String(s.clone())),
                Literal::Char(c) => Ok(Object::Char(*c)),
                Literal::Bool(b) => Ok(Object::Bool(*b)),
                Literal::Array(arr) => {
                    let values = arr
                        .iter()
                        .map(|e| self.eval_expr(e.into()))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Object::Array(values))
                }
                Literal::Unit => Ok(Object::Unit),
            },
            CheckedExpr::Prefix { op, expr, .. } => {
                let val = self.eval_expr(expr)?;
                match op {
                    Token::Sub => match val {
                        Object::Int(i) => Ok(Object::Int(-i)),
                        Object::Float(f) => Ok(Object::Float(-f)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Not => match val {
                        Object::Bool(b) => Ok(Object::Bool(!b)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    _ => Err(EvalError::UnsupportedOperator),
                }
            }
            CheckedExpr::Infix {
                op, left, right, ..
            } => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                match op {
                    Token::Plus => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l + r)),
                        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l + r)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Sub => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
                        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Mul => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
                        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Pow => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l.pow(r as u32))),
                        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l.powf(r))),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Div => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) if r != 0 => Ok(Object::Int(l / r)),
                        (Object::Float(l), Object::Float(r)) if r != 0.0 => {
                            Ok(Object::Float(l / r))
                        }
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Mod => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) if r != 0 => Ok(Object::Int(l % r)),
                        (Object::Float(l), Object::Float(r)) if r != 0.0 => {
                            Ok(Object::Float(l % r))
                        }
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Equal => Ok(Object::Bool(left_val == right_val)),
                    Token::NotEqual => Ok(Object::Bool(left_val != right_val)),
                    Token::Greater => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok(Object::Bool(l > r)),
                        (Object::Float(l), Object::Float(r)) => Ok(Object::Bool(l > r)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::GreaterEqual => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok(Object::Bool(l >= r)),
                        (Object::Float(l), Object::Float(r)) => Ok(Object::Bool(l >= r)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Less => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok(Object::Bool(l < r)),
                        (Object::Float(l), Object::Float(r)) => Ok(Object::Bool(l < r)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::LessEqual => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok(Object::Bool(l <= r)),
                        (Object::Float(l), Object::Float(r)) => Ok(Object::Bool(l <= r)),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::ThinArrow => {
                        if let (Object::Type(param), Object::Type(ret)) = (left_val, right_val) {
                            Ok(Object::Type(TypeObject::Function(
                                Box::new(param),
                                Box::new(ret),
                            )))
                        } else {
                            Err(EvalError::TypeMismatch)
                        }
                    }
                    Token::InfixIdent(value) => self.eval_func(
                        &CheckedExpr::Ident {
                            value: value.clone(),
                            type_annotation: TypeObject::Any, // TODO
                        },
                        vec![left_val, right_val],
                    ),
                    token => self.eval_func(
                        &CheckedExpr::Ident {
                            value: token.to_string(),
                            type_annotation: TypeObject::Any,
                        }, // TODO
                        vec![left_val, right_val],
                    ),
                }
            }
            CheckedExpr::Function {
                params,
                body,
                type_annotation,
            } => Ok(Object::Function {
                params: params.clone(),
                body: *body.clone(),
                type_annotation: type_annotation.clone(),
                env: self.env.clone(),
            }),
            CheckedExpr::Call { callee, params, .. } => self.eval_func(
                callee,
                params
                    .iter()
                    .map(|param| self.eval_expr(param))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            CheckedExpr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond = self.eval_expr(condition)?;
                match cond {
                    Object::Bool(true) => self.eval_expr(then_branch),
                    Object::Bool(false) => self.eval_expr(else_branch),
                    _ => Err(EvalError::TypeMismatch),
                }
            }
            CheckedExpr::Match { expr, cases, .. } => {
                let match_value = self.eval_expr(expr)?;
                for case in cases {
                    if let Some(binds) = self.match_pattern(&case.pattern, &match_value) {
                        let case_env = Env::extend(Rc::clone(&self.env));
                        for (name, value) in binds {
                            case_env.borrow_mut().insert_bind(name, value)?;
                        }
                        let case_evaluator = Evaluator::new(case_env);
                        if case_evaluator
                            .eval_expr(&case.guard)?
                            .eq(&Object::Bool(true))
                        {
                            return case_evaluator.eval_expr(&case.body);
                        }
                    }
                }
                Err(EvalError::PatternMatchFailure)
            }
            CheckedExpr::LetIn {
                name, value, body, ..
            } => {
                let val = self.eval_expr(value)?;
                let create_env = Env::extend(Rc::clone(&self.env));
                create_env.borrow_mut().insert_bind(name.clone(), val)?;
                Evaluator::new(create_env).eval_expr(body)
            }
            CheckedExpr::Block { stmts, .. } => {
                let local_env = Env::extend(Rc::clone(&self.env));
                let mut evaluator = Evaluator::new(local_env);
                let mut last = Object::Unit;
                for stmt in stmts {
                    last = evaluator.eval_stmt(stmt)?;
                }
                Ok(last)
            }
        }
    }

    fn match_pattern(&self, pattern: &Pattern, value: &Object) -> Option<Vec<(String, Object)>> {
        match pattern {
            Pattern::Ident(name) => match self.env.borrow().get_bind(name) {
                Some(target) if is_uppercase_first_letter(name.as_str()) => {
                    if target == value.clone() {
                        Some(vec![])
                    } else {
                        None
                    }
                }
                _ => Some(if name == "_" {
                    vec![]
                } else {
                    vec![(name.clone(), value.clone())]
                }),
            },
            Pattern::ADTConstructor { name, args } => {
                if let Object::ADT {
                    variant, fields, ..
                } = value
                {
                    if variant == name && fields.len() == args.len() {
                        let mut binds = vec![];
                        for (pat_arg, field_value) in args.iter().zip(fields.iter()) {
                            if let Some(sub_binds) = self.match_pattern(pat_arg, field_value) {
                                binds.extend(sub_binds);
                            } else {
                                return None;
                            }
                        }
                        Some(binds)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Pattern::Literal(literal) => match (literal, value) {
                (Literal::Int(i), Object::Int(v)) if i == v => Some(vec![]),
                (Literal::Float(f), Object::Float(v)) if f == v => Some(vec![]),
                (Literal::String(s), Object::String(v)) if s == v => Some(vec![]),
                (Literal::Char(c), Object::Char(v)) if c == v => Some(vec![]),
                (Literal::Bool(b), Object::Bool(v)) if b == v => Some(vec![]),
                _ => None,
            },
        }
    }
}
