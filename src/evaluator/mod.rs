use crate::ast::{Expr, Literal, Pattern, Program, Stmt, TypeExpr, TypeVariantFields};
use crate::token::Token;
use crate::utils::is_uppercase_first_letter;
use env::{Env, EvaluatorEnv};
use object::{Object, PrettyPrint, TypeObject};
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
    PatternMatchFailure,
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
            EvalError::PatternMatchFailure => write!(f, "Pattern match failed"),
        }
    }
}

pub struct Evaluator<'a> {
    env: &'a mut EvaluatorEnv<'a>,
}

impl<'a> Evaluator<'a> {
    pub fn new(env: &'a mut EvaluatorEnv<'a>) -> Self {
        Evaluator { env }
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
        match identify {
            "print" => {
                println!(
                    "{}",
                    args.iter()
                        .map(|arg| arg.pretty_print())
                        .collect::<Vec<_>>()
                        .join(" ")
                );
                Ok(Object::Unit)
            }
            "get_timestrap" => Ok(Object::Int(UNIX_EPOCH.elapsed().unwrap().as_millis() as i64)),
            name if name.starts_with("type_constructor_") => {
                let name = &name["type_constructor_".len()..];
                // Type constructor application
                if let Some(Object::Type(TypeObject::ADT {
                    name,
                    type_params,
                    constructors,
                })) = self.env.get_bind(&name)
                {
                    if type_params.len() != args.len() {
                        return Err(EvalError::ArityMismatch);
                    }
                    let applied_types = args
                        .into_iter()
                        .map(|arg| match arg {
                            Object::Type(t) => Ok(t),
                            _ => Err(EvalError::TypeMismatch),
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Object::Type(TypeObject::ADT {
                        name: name.clone(),
                        type_params: vec![], // Fully applied
                        constructors: constructors.clone(),
                    }))
                } else {
                    Err(EvalError::UndefinedVariable(name.to_string()))
                }
            }
            _ => Err(EvalError::UndefinedVariable(identify.to_string())),
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
                    return Err(EvalError::RedefinedIdentifier(name.clone()));
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
                        let field_types = match &variant.fields {
                            TypeVariantFields::Unit => vec![],
                            TypeVariantFields::Tuple(types) => {
                                types.iter().map(|t| *t.clone()).collect()
                            }
                            TypeVariantFields::Record(fields) => {
                                fields.iter().map(|(_, t)| *t.clone()).collect()
                            }
                        };
                        (variant.name.clone(), field_types)
                    })
                    .collect();

                // Handle type constructor
                match type_params {
                    None => {
                        // No type parameters: Add type constructor as a TypeObject
                        let type_obj = TypeObject::ADT {
                            name: name.clone(),
                            type_params: vec![],
                            constructors: constructors.clone(),
                        };
                        self.env.insert_bind(name.clone(), Object::Type(type_obj))?;
                    }
                    Some(params) => {
                        // With type parameters: Add type constructor as a function
                        let type_constructor = Object::Function {
                            type_params: vec![],
                            params: params
                                .clone()
                                .into_iter()
                                .map(|t| (t, Box::new(TypeObject::Kind.into())))
                                .collect::<Vec<_>>(),
                            body: Box::new(Expr::Ident(format!("type_constructor_{}", name))), // Placeholder, handled specially in eval_expr
                            return_type: Box::new({
                                let mut return_type: TypeExpr = TypeObject::Kind.into();
                                for _ in params {
                                    return_type = TypeExpr::Arrow(
                                        Box::new(return_type.clone()),
                                        Box::new(TypeObject::Kind.into()),
                                    );
                                }
                                return_type
                            }),
                        };
                        self.env.insert_bind(name.clone(), type_constructor)?;
                    }
                }

                // Handle value constructors
                for variant in variants {
                    let variant_name = variant.name.clone();
                    match &variant.fields {
                        TypeVariantFields::Unit => {
                            // No value parameters: Direct ADT value
                            let adt_value = Object::ADT {
                                type_name: name.clone(),
                                variant: variant_name.clone(),
                                fields: vec![],
                            };
                            self.env.insert_bind(variant_name.clone(), adt_value)?;
                        }
                        TypeVariantFields::Tuple(field_types) => {
                            // With value parameters: ADT constructor function
                            let param_count = field_types.len();
                            let constructor = Object::ADTConstructor {
                                type_name: name.clone(),
                                variant: variant_name.clone(),
                                param_count,
                            };
                            self.env.insert_bind(variant_name.clone(), constructor)?;
                        }
                        TypeVariantFields::Record(fields) => {
                            // Record fields treated similarly to tuples
                            let param_count = fields.len();
                            let constructor = Object::ADTConstructor {
                                type_name: name.clone(),
                                variant: variant_name.clone(),
                                param_count,
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
                            Expr::Ident(ref name) => {
                                // TODO
                                // if !name.ends_with("!") {
                                //     check_params()?;
                                // }
                                self.eval_internal_func(name.as_str(), args)
                            }
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
                        variant,
                        param_count,
                    } => {
                        if args.len() != param_count {
                            return Err(EvalError::ArityMismatch);
                        }
                        Ok(Object::ADT {
                            type_name,
                            variant,
                            fields: args,
                        })
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
                        return case_evaluator.eval_expr(&case.body);
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
