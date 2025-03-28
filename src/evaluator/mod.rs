pub mod env;
pub mod object;

use env::Env;
use object::{Object, TypeObject};

use crate::ast::{Expr, Literal, Program, Stmt};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};
use std::time::UNIX_EPOCH;

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    UndefinedVariable(String),
    TypeMismatch,
    UnsupportedOperator,
    ArityMismatch,
    NotCallable,
    RedefinedIdentifier(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            EvalError::UndefinedVariable(name) => write!(f, "Undefined variable '{}'", name),
            EvalError::TypeMismatch => write!(f, "Type mismatch"),
            EvalError::UnsupportedOperator => write!(f, "Unsupported operator"),
            EvalError::ArityMismatch => write!(f, "Arity mismatch"),
            EvalError::NotCallable => write!(f, "Not callable"),
            EvalError::RedefinedIdentifier(name) => {
                write!(f, "Identifier '{}' is already defined", name)
            }
        }
    }
}

pub struct Evaluator {
    env: Env,
}

impl Evaluator {
    pub fn new(env: Env) -> Self {
        Evaluator { env }
    }

    pub fn eval(&mut self, program: &Program) -> Result<Object, EvalError> {
        let mut last_value = Object::Unit;
        for stmt in program {
            last_value = self.eval_stmt(stmt)?;
        }
        Ok(last_value)
    }

    pub fn get_env(&self) -> &Env {
        &self.env
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Object, EvalError> {
        match stmt {
            Stmt::Let {
                name,
                type_annotation: _,
                value,
            } => {
                if self.env.get_value(name).is_some() || self.env.get_type(name).is_some() {
                    return Err(EvalError::RedefinedIdentifier(name.clone()));
                }
                let val = self.eval_expr(value)?;
                self.env
                    .insert_value(name.clone(), val, false)
                    .map_err(|_| EvalError::RedefinedIdentifier(name.clone()))?;
                Ok(Object::Unit)
            }
            Stmt::Type {
                name,
                type_annotation,
                type_params: _,
                variants,
            } => {
                if self.env.get_type(name).is_some() || self.env.get_value(name).is_some() {
                    return Err(EvalError::RedefinedIdentifier(name.clone()));
                }
                // 简单实现：若有类型注解则期望其返回一个类型，否则生成自定义类型
                let type_obj = if let Some(expr) = type_annotation {
                    match self.eval_expr(expr)? {
                        Object::Type(t) => t,
                        _ => return Err(EvalError::TypeMismatch),
                    }
                } else {
                    // 构造自定义类型对象，并在其中登记各变体对应的构造函数
                    let mut constructors = Vec::new();
                    for variant in variants {
                        // 每个构造子函数参数数目由 variant.fields 决定
                        let arity = match &variant.fields {
                            crate::ast::TypeVariantFields::Tuple(exprs) => exprs.len(),
                            crate::ast::TypeVariantFields::Record(fields) => fields.len(),
                            crate::ast::TypeVariantFields::Unit => 0,
                        };
                        // 构造函数对象，此处使用闭包形式的包装实现（为了简单，此处未实现真正的闭包执行）
                        let constructor_fn = Object::Function {
                            params: (0..arity).map(|i| format!("arg{}", i)).collect(),
                            body: Box::new(Expr::Ident(format!(
                                "construct_{}_{}",
                                name, variant.name
                            ))),
                        };
                        constructors.push((variant.name.clone(), (arity, constructor_fn)));
                    }
                    TypeObject::CustomType {
                        name: name.clone(),
                        constructors,
                    }
                };
                self.env
                    .insert_type(name.clone(), type_obj.clone())
                    .map_err(|_| EvalError::RedefinedIdentifier(name.clone()))?;
                // 同时将类型的每个构造子函数注入到环境中
                if let TypeObject::CustomType {
                    name: type_name,
                    constructors,
                } = &type_obj
                {
                    for (ctor_name, (_arity, ctor_fn)) in constructors {
                        let full_name = format!("{}::{}", type_name, ctor_name);
                        self.env
                            .insert_value(full_name, ctor_fn.clone(), false)
                            .map_err(|_| EvalError::RedefinedIdentifier(ctor_name.clone()))?;
                    }
                }
                Ok(Object::Unit)
            }
            Stmt::Expr(expr) => self.eval_expr(expr),
            // 对于 Import/Export 这里简单忽略
            _ => Ok(Object::Unit),
        }
    }

    fn eval_expr(&self, expr: &Expr) -> Result<Object, EvalError> {
        match expr {
            Expr::Ident(name) => {
                if let Some(val) = self.env.get_value(name) {
                    Ok(val.clone())
                } else if let Some(type_obj) = self.env.get_type(name) {
                    Ok(Object::Type(type_obj.clone()))
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
            Expr::Prefix(op, expr) => {
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
                        // 构造函数类型运算符：左侧与右侧均须为类型
                        if let (Object::Type(param), Object::Type(ret)) = (left_val, right_val) {
                            Ok(Object::Type(TypeObject::FunctionType(
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
                    Object::Function { params, body } => {
                        if params.len() != args.len() {
                            return Err(EvalError::ArityMismatch);
                        }
                        match body.as_ref().clone() {
                            Expr::Ident(name) if name == "print_builtin".to_string() => {
                                println!(
                                    "{}",
                                    args.iter()
                                        .map(|arg| arg.to_string())
                                        .collect::<Vec<_>>()
                                        .join(" ")
                                );
                                Ok(Object::Unit)
                            }
                            Expr::Ident(name) if name == "get_timestrap_builtin".to_string() => {
                                Ok(Object::Int(UNIX_EPOCH.elapsed().unwrap().as_millis() as i64))
                            }
                            _ => {
                                let mut local_env = self.env.clone();
                                for (param, arg) in params.iter().zip(args) {
                                    local_env.insert_value(param.clone(), arg, true).map_err(
                                        |_| EvalError::RedefinedIdentifier(param.clone()),
                                    )?;
                                }
                                Evaluator { env: local_env }.eval_expr(&body)
                            }
                        }
                    }
                    Object::ADT { .. } => Err(EvalError::NotCallable),
                    _ => Err(EvalError::NotCallable),
                }
            }
            Expr::Lambda { params, body, .. } => {
                let param_names = params.iter().map(|(name, _)| name.clone()).collect();
                Ok(Object::Function {
                    params: param_names,
                    body: body.clone(),
                })
            }
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
                let mut local_env = self.env.clone();
                local_env
                    .insert_value(name.clone(), val, false)
                    .map_err(|_| EvalError::RedefinedIdentifier(name.clone()))?;
                Evaluator { env: local_env }.eval_expr(body)
            }
            Expr::Block(stmts) => {
                let mut local_env = self.env.clone();
                let mut last_value = Object::Unit;
                for stmt in stmts {
                    last_value = Evaluator {
                        env: local_env.clone(),
                    }
                    .eval_stmt(stmt)?;
                    local_env = Evaluator { env: local_env }.env;
                }
                Ok(last_value)
            }
            Expr::Match { expr: _, cases: _ } => Ok(Object::Unit),
        }
    }
}
