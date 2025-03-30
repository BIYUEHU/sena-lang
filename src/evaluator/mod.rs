use env::Env;
use object::{Object, TypeObject};

use crate::ast::{Expr, Literal, Program, Stmt};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};
use std::time::UNIX_EPOCH;

pub mod env;
pub mod object;

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

pub struct Evaluator<'a> {
    env: &'a mut Env<'a>,
}

impl<'a> Evaluator<'a> {
    pub fn new(env: &'a mut Env<'a>) -> Self {
        Evaluator { env }
    }

    pub fn eval(&mut self, program: &Program) -> Result<Object, EvalError> {
        let mut last_value = Object::Unit;
        for stmt in program {
            last_value = self.eval_stmt(stmt)?;
        }
        Ok(last_value)
    }

    /// 返回当前环境的副本
    pub fn get_env(&self) -> Env<'_> {
        self.env.clone()
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
                let result = self.eval_expr_tco(value, false)?;
                self.env
                    .insert_value(name.clone(), result)
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
                // 若有类型注解，则期望其返回一个类型，否则生成自定义类型
                let type_obj = if let Some(expr) = type_annotation {
                    match self.eval_expr_tco(expr, false)? {
                        Object::Type(t) => t,
                        _ => return Err(EvalError::TypeMismatch),
                    }
                } else {
                    // 构造自定义类型对象，并在其中登记各变体对应的构造函数
                    let mut constructors = Vec::new();
                    for variant in variants {
                        let arity = match &variant.fields {
                            crate::ast::TypeVariantFields::Tuple(exprs) => exprs.len(),
                            crate::ast::TypeVariantFields::Record(fields) => fields.len(),
                            crate::ast::TypeVariantFields::Unit => 0,
                        };
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
                            .insert_value(full_name, ctor_fn.clone())
                            .map_err(|_| EvalError::RedefinedIdentifier(ctor_name.clone()))?;
                    }
                }
                Ok(Object::Unit)
            }
            Stmt::Expr(expr) => self.eval_expr_tco(expr, false),
            // 对于 Import/Export 这里简单忽略
            _ => Ok(Object::Unit),
        }
    }

    /// eval_expr_tco(expr, tail) 求值表达式 expr，其中 tail 表示是否处于尾调用位置
    fn eval_expr_tco(&mut self, expr: &Expr, tail: bool) -> Result<Object, EvalError> {
        match expr {
            Expr::Ident(name) => {
                if let Some(val) = self.env.get_value(name) {
                    Ok(val)
                } else if let Some(type_obj) = self.env.get_type(name) {
                    Ok(Object::Type(type_obj))
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
                        .map(|e| self.eval_expr_tco(e, false))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Object::Array(values))
                }
            },
            Expr::Prefix(op, sub_expr) => {
                let val = self.eval_expr_tco(sub_expr, false)?;
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
                let left_val = self.eval_expr_tco(left, false)?;
                let right_val = self.eval_expr_tco(right, false)?;
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
                let func = self.eval_expr_tco(callee, false)?;
                let args = arguments
                    .iter()
                    .map(|arg| self.eval_expr_tco(arg, false))
                    .collect::<Result<Vec<_>, _>>()?;
                self.eval_function_call_tco(func, args, tail)
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
                let cond_val = self.eval_expr_tco(condition, false)?;
                match cond_val {
                    Object::Bool(true) => self.eval_expr_tco(then_branch, tail),
                    Object::Bool(false) => self.eval_expr_tco(else_branch, tail),
                    _ => Err(EvalError::TypeMismatch),
                }
            }
            Expr::LetIn {
                name, value, body, ..
            } => {
                let val = self.eval_expr_tco(value, false)?;
                let mut new_env = Env::extend(self.env);
                new_env.insert_value(name.clone(), val)?;
                Evaluator::new(&mut new_env).eval_expr_tco(body, tail)
            }
            Expr::Block(stmts) => {
                let mut new_env = Env::extend(self.env);
                let mut last_value = Object::Unit;
                let mut evaluator = Evaluator::new(&mut new_env);
                for stmt in stmts {
                    last_value = evaluator.eval_stmt(stmt)?;
                }
                Ok(last_value)
            }
            Expr::Match { expr: _, cases: _ } => Ok(Object::Unit),
        }
    }

    fn eval_function_call_tco(
        &mut self,
        mut func: Object,
        mut args: Vec<Object>,
        tail: bool,
    ) -> Result<Object, EvalError> {
        if tail {
            loop {
                match func {
                    Object::Function { params, body } => {
                        if params.len() != args.len() {
                            return Err(EvalError::ArityMismatch);
                        }
                        println!("{:?}", body);
                        let mut local_env = Env::extend(self.env);
                        for (p, a) in params.iter().zip(args.iter()) {
                            local_env.insert_value(p.clone(), a.clone())?;
                        }
                        // 在求值函数体时，我们令其处于尾调用位置
                        let result = Evaluator::new(&mut local_env).eval_expr_tco(&body, true)?;
                        // 如果 result 是一个尾调用调用（此处我们约定，如果返回的函数调用处于尾调用位置，则它会以 Function 类型返回），
                        // 则继续循环展开；否则直接返回结果。
                        match result {
                            // 如果返回的是 Function，则继续循环展开
                            Object::Function { .. } => {
                                func = result;
                                // 此处为简单起见，不重新计算 args；实际可根据需要调整
                                args = Vec::new();
                                continue;
                            }
                            _ => return Ok(result),
                        }
                    }
                    // Object::ADT { .. } => Err(EvalError::NotCallable),
                    _ => return Err(EvalError::NotCallable),
                }
            }
        } else {
            // 非尾调用直接处理
            match func {
                Object::Function { params, body } => {
                    if params.len() != args.len() {
                        return Err(EvalError::ArityMismatch);
                    }
                    match *body {
                        Expr::Ident(ref name) if name == "print_builtin" => {
                            println!(
                                "{}",
                                args.iter()
                                    .map(|arg| arg.to_string())
                                    .collect::<Vec<_>>()
                                    .join(" ")
                            );
                            Ok(Object::Unit)
                        }
                        Expr::Ident(ref name) if name == "get_timestrap_builtin" => {
                            Ok(Object::Int(UNIX_EPOCH.elapsed().unwrap().as_millis() as i64))
                        }
                        _ => {
                            let mut local_env = Env::extend(self.env);
                            for (p, a) in params.iter().zip(args.into_iter()) {
                                local_env.insert_value(p.clone(), a)?;
                            }
                            Evaluator::new(&mut local_env).eval_expr_tco(&body, false)
                        }
                    }
                }
                _ => Err(EvalError::NotCallable),
            }
        }
    }
}
