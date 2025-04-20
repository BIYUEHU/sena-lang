use crate::checker::object::TypeObject;
use crate::common::env::{Env, EnvError, EvaluatorEnv};
use crate::lexer::token::Token;
use crate::parser::ast::{Expr, Literal, Pattern, Program, Stmt, TypeExpr, TypeVariantFields};
use crate::utils::is_uppercase_first_letter;
use object::{Object, PrettyPrint};
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::time::UNIX_EPOCH;

pub mod object;

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

pub type CustomFunc = Box<dyn Fn(Vec<Object>) -> Result<Object, EvalError>>;
pub type CustomFuncs = HashMap<String, CustomFunc>;

pub struct Evaluator<'a> {
    env: &'a mut EvaluatorEnv<'a>,
    // TODO: improved it
    custom_funcs: CustomFuncs,
}

impl<'a> Evaluator<'a> {
    pub fn new(env: &'a mut EvaluatorEnv<'a>) -> Self {
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

    pub fn eval(&mut self, program: &Program) -> Result<Object, EvalError> {
        let mut last_value = Object::Unit;
        for stmt in program {
            last_value = self.eval_stmt(stmt)?;
        }
        Ok(last_value)
    }

    fn eval_internal_func(
        &mut self,
        identify: &str,
        args: Vec<Object>,
    ) -> Result<Object, EvalError> {
        if let Some(func) = self.custom_funcs.get(identify) {
            func(args)
        } else {
            Err(EvalError::UndefinedVariable(identify.to_string()))
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Object, EvalError> {
        match stmt {
            Stmt::Let {
                name,
                type_annotation: _,
                value,
            } => {
                if self.env.get_bind(name).is_some() {
                    return Err(EvalError::RedefinedVariable(name.clone()));
                }
                let result = self.eval_expr(value)?;
                self.env.insert_bind(name.clone(), result)?;
                Ok(Object::Unit)
            }
            Stmt::Type {
                name,
                type_annotation: _,
                type_params,
                variants,
            } => {
                // Collect constructor definitions
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

                // Handle type constructor
                let type_params_count = match type_params {
                    None => {
                        // No type parameters: Add type constructor as a TypeObject
                        self.env.insert_bind(
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
                        // With type parameters: Add type constructor as a function
                        self.env.insert_bind(
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
                            // No value parameters: Direct ADT value
                            let adt_value = Object::ADT {
                                type_name: name.clone(),
                                type_params: type_params.clone(),
                                variant: variant_name.clone(),
                                fields: vec![],
                            };
                            self.env.insert_bind(variant_name.clone(), adt_value)?;
                        }
                        TypeVariantFields::Tuple(fields) => {
                            // With value parameters: ADT constructor function
                            let constructor = Object::ADTConstructor {
                                type_name: name.clone(),
                                type_params: type_params.clone(),
                                variant: variant_name.clone(),
                                fields: fields.clone(),
                            };
                            self.env.insert_bind(variant_name.clone(), constructor)?;
                        }
                        TypeVariantFields::Record(fields) => {
                            // Record fields treated similarly to tuples
                            let constructor = Object::ADTConstructor {
                                type_name: name.clone(),
                                type_params: type_params.clone(),
                                variant: variant_name.clone(),
                                fields: fields
                                    .iter()
                                    .map(|(_, t)| Box::new(*t.clone()))
                                    .collect::<Vec<_>>(),
                            };
                            self.env.insert_bind(variant_name.clone(), constructor)?;
                        }
                    }
                }
                Ok(Object::Unit)
            }
            Stmt::Expr(expr) => self.eval_expr(expr),
            _ => Ok(Object::Unit), // Import/Export ignored
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Object, EvalError> {
        match expr {
            Expr::Ident(name) => {
                if let Some(val) = self.env.get_bind(name) {
                    Ok(val)
                } else {
                    Err(EvalError::UndefinedVariable(name.clone()))
                }
            }
            Expr::Literal(lit) => match lit {
                Literal::Int(i) => Ok(Object::Int(*i)),
                Literal::Float(f) => Ok(Object::Float(*f)),
                Literal::String(s) => Ok(Object::String(s.clone())),
                Literal::Char(c) => Ok(Object::Char(*c)),
                Literal::Bool(b) => Ok(Object::Bool(*b)),
                Literal::Array(arr) => {
                    let values = arr
                        .iter()
                        .map(|e| self.eval_expr(e))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Object::Array(values))
                }
            },
            Expr::Prefix(op, sub_expr) => {
                let val = self.eval_expr(sub_expr)?;
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
            Expr::Infix(op, left, right) => {
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
                    Token::Div => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) if r != 0 => Ok(Object::Int(l / r)),
                        (Object::Float(l), Object::Float(r)) if r != 0.0 => {
                            Ok(Object::Float(l / r))
                        }
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Mod => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) if r != 0 => Ok(Object::Int(l % r)),
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
                    _ => Err(EvalError::UnsupportedOperator),
                }
            }
            Expr::Call { callee, arguments } => {
                let func = self.eval_expr(callee)?;
                let args = arguments
                    .iter()
                    .map(|arg| self.eval_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                match func {
                    Object::Function { params, body, .. } => {
                        let check_params = || {
                            if params.len() != args.len() {
                                Err(EvalError::ArityMismatch)
                            } else {
                                Ok(())
                            }
                        };
                        match *body {
                            Expr::Ident(ref name) => self.eval_internal_func(name.as_str(), args),
                            _ => {
                                check_params()?;
                                let mut local_env = Env::extend(self.env);
                                for (p, a) in params.iter().zip(args.into_iter()) {
                                    local_env.insert_bind(p.0.clone(), a)?;
                                }
                                Evaluator::new(&mut local_env).eval_expr(&body)
                            }
                        }
                    }
                    Object::ADTConstructor {
                        type_name,
                        type_params,
                        variant,
                        fields,
                    } => {
                        if args.len() != fields.len() {
                            return Err(EvalError::ArityMismatch);
                        }
                        Ok(Object::ADT {
                            type_name,
                            type_params,
                            variant,
                            fields: args,
                        })
                    }
                    Object::TypeConstructor {
                        type_name,
                        type_params,
                        constructors,
                    } => {
                        if args.len() != type_params.len() {
                            return Err(EvalError::ArityMismatch);
                        }
                        Ok(Object::Type(TypeObject::ADT {
                            name: type_name.clone(),
                            type_params: vec![],
                            constructors: constructors
                                .into_iter()
                                .map(|(name, args)| {
                                    (
                                        name,
                                        args.into_iter()
                                            .map(|type_expr| {
                                                // TODO: checker should implement type application and type calculation
                                                type_expr
                                            })
                                            .collect(),
                                    )
                                })
                                .collect::<Vec<_>>(),
                        }))
                    }
                    _ => Err(EvalError::NotCallable),
                }
            }
            Expr::Function {
                params,
                body,
                type_params,
                return_type,
            } => Ok(Object::Function {
                type_params: type_params.clone(),
                return_type: return_type.clone(),
                params: params.clone(),
                body: body.clone(),
            }),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_val = self.eval_expr(condition)?;
                match cond_val {
                    Object::Bool(true) => self.eval_expr(then_branch),
                    Object::Bool(false) => self.eval_expr(else_branch),
                    _ => Err(EvalError::TypeMismatch),
                }
            }
            Expr::LetIn {
                name, value, body, ..
            } => {
                let val = self.eval_expr(value)?;
                let mut create_env = Env::extend(self.env);
                create_env.insert_bind(name.clone(), val)?;
                Evaluator::new(&mut create_env).eval_expr(body)
            }
            Expr::Block(stmts) => {
                let mut create_env = Env::extend(self.env);
                let mut last_value = Object::Unit;
                let mut evaluator = Evaluator::new(&mut create_env);
                for stmt in stmts {
                    last_value = evaluator.eval_stmt(stmt)?;
                }
                Ok(last_value)
            }
            Expr::Match { expr, cases } => {
                let match_value = self.eval_expr(expr)?;
                for case in cases {
                    if let Some(binds) = self.match_pattern(&case.pattern, &match_value) {
                        let mut case_env = Env::extend(self.env);
                        for (name, value) in binds {
                            case_env.insert_bind(name, value)?;
                        }
                        let mut case_evaluator = Evaluator::new(&mut case_env);
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
        }
    }

    fn match_pattern(&self, pattern: &Pattern, value: &Object) -> Option<Vec<(String, Object)>> {
        match pattern {
            Pattern::Ident(name) => match self.env.get_bind(name) {
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
