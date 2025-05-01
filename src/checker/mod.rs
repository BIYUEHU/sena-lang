use crate::common::env::{CheckerEnv, EnvError};
use crate::lexer::token::Token;
use crate::parser::ast::{
    Expr, Literal, MatchCase, Pattern, Program, Stmt, TypeExpr, TypeVariantFields,
};
use object::TypeObject;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub mod object;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UndefinedVariable(String),
    RedefinedVariable(String),
    TypeMismatch {
        expected: String,
        found: String,
        context: String,
    },
    ArityMismatch {
        expected: usize,
        found: usize,
        context: String,
    },
    NotCallable {
        found: String,
    },
    InvalidOperation {
        operation: String,
        typ: String,
    },
}

impl From<EnvError> for TypeError {
    fn from(error: EnvError) -> Self {
        match error {
            EnvError::RedefinedBinding(name) => TypeError::RedefinedVariable(name),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::UndefinedVariable(name) => write!(f, "Undefined variable '{}'", name),
            TypeError::RedefinedVariable(name) => {
                write!(f, "Variable '{}' is already defined", name)
            }
            TypeError::TypeMismatch {
                expected,
                found,
                context,
            } => write!(
                f,
                "Type mismatch in '{}': expected '{}', found '{}'",
                context, expected, found
            ),
            TypeError::ArityMismatch {
                expected,
                found,
                context,
            } => write!(
                f,
                "Arity mismatch in '{}': expected {} arguments, found {}",
                context, expected, found
            ),
            TypeError::NotCallable { found } => write!(f, "Not callable: '{}'", found),
            TypeError::InvalidOperation { operation, typ } => {
                write!(f, "Invalid operation '{}' on type '{}'", operation, typ)
            }
        }
    }
}

pub struct Checker {
    env: Rc<RefCell<CheckerEnv>>,
    // type_vars: HashMap<String, TypeObject>,
}

impl Checker {
    pub fn new(env: Rc<RefCell<CheckerEnv>>) -> Self {
        Checker {
            env,
            // type_vars: HashMap::new(),
        }
    }

    pub fn check(&mut self, program: &Program) -> Result<Program, TypeError> {
        let mut checked_stmts = Vec::new();
        for stmt in program {
            let checked_stmt = self.check_stmt(stmt)?;
            checked_stmts.push(checked_stmt);
        }
        Ok(checked_stmts)
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<Stmt, TypeError> {
        match stmt {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                let inferred_type = self.infer(value)?;
                let annotation_type = self.resolve(type_annotation)?;
                let final_type = self.unify(&annotation_type, &inferred_type, name)?;
                self.env
                    .borrow_mut()
                    .insert_bind(name.clone(), final_type.clone())
                    .map_err(|_| TypeError::RedefinedVariable(name.clone()))?;
                Ok(Stmt::Let {
                    name: name.clone(),
                    type_annotation: Box::new(final_type.clone().into()),
                    value: Box::new(self.check_expr(value, &final_type)?),
                })
            }
            Stmt::Type {
                name,
                type_params,
                variants,
                ..
            } => {
                let adt_type = TypeObject::ADT {
                    name: name.clone(),
                    type_params: type_params.clone().unwrap_or_default(),
                    constructors: variants
                        .iter()
                        .map(|v| (v.name.clone(), self.resolve_variant_fields(&v.fields)))
                        .collect(),
                };
                self.env
                    .borrow_mut()
                    .insert_bind(name.clone(), adt_type.clone())
                    .map_err(|_| TypeError::RedefinedVariable(name.clone()))?;
                Ok(Stmt::Type {
                    name: name.clone(),
                    type_annotation: Box::new(adt_type.into()),
                    type_params: type_params.clone(),
                    variants: variants.clone(),
                })
            }
            Stmt::Expr(expr) => {
                let checked_expr = self.check_expr(expr, &TypeObject::Unknown)?;
                Ok(Stmt::Expr(checked_expr))
            }
            Stmt::ImportAll { .. } | Stmt::ImportSome { .. } => Ok(stmt.clone()),
            Stmt::Export {
                body,
                only_abstract,
            } => {
                let checked_body = Box::new(self.check_stmt(body)?);
                Ok(Stmt::Export {
                    body: checked_body,
                    only_abstract: *only_abstract,
                })
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr, expected: &TypeObject) -> Result<Expr, TypeError> {
        match expr {
            Expr::Ident(name) | Expr::Internal(name) => {
                let var_type = self
                    .env
                    .borrow()
                    .get_bind(name)
                    .ok_or(TypeError::UndefinedVariable(name.clone()))?;
                self.unify(expected, &var_type, name)?;
                Ok(Expr::Ident(name.clone()))
            }
            Expr::Literal(lit) => {
                let lit_type = self.infer_literal(lit)?;
                self.unify(expected, &lit_type, "literal")?;
                Ok(Expr::Literal(lit.clone()))
            }
            Expr::Prefix(op, expr) => {
                let expr_type = self.infer(expr)?;
                let result_type = match op {
                    Token::Sub => match expr_type {
                        TypeObject::Int => TypeObject::Int,
                        TypeObject::Float => TypeObject::Float,
                        _ => {
                            return Err(TypeError::InvalidOperation {
                                operation: "unary minus".to_string(),
                                typ: expr_type.to_string(),
                            })
                        }
                    },
                    Token::Not => {
                        if expr_type != TypeObject::Bool {
                            return Err(TypeError::InvalidOperation {
                                operation: "logical not".to_string(),
                                typ: expr_type.to_string(),
                            });
                        }
                        TypeObject::Bool
                    }
                    _ => {
                        return Err(TypeError::InvalidOperation {
                            operation: format!("prefix {:?}", op),
                            typ: expr_type.to_string(),
                        })
                    }
                };
                self.unify(expected, &result_type, "prefix operation")?;
                Ok(Expr::Prefix(
                    op.clone(),
                    Box::new(self.check_expr(expr, &expr_type)?),
                ))
            }
            Expr::Infix(op, left, right) => {
                let left_type = self.infer(left)?;
                let right_type = self.infer(right)?;
                let result_type = self.infer_infix(op, &left_type, &right_type)?;
                self.unify(expected, &result_type, "infix operation")?;
                Ok(Expr::Infix(
                    op.clone(),
                    Box::new(self.check_expr(left, &left_type)?),
                    Box::new(self.check_expr(right, &right_type)?),
                ))
            }
            Expr::Call { callee, arguments } => {
                let callee_type = self.infer(callee)?;
                let mut current_type = callee_type.clone();
                let mut checked_args = Vec::new();
                for (i, arg) in arguments.iter().enumerate() {
                    if let TypeObject::Function(param_type, return_type) = current_type {
                        let arg_type = self.infer(arg)?;
                        self.unify(&param_type, &arg_type, &format!("argument {}", i))?;
                        checked_args.push(self.check_expr(arg, &param_type)?);
                        current_type = *return_type;
                    } else {
                        return Err(TypeError::NotCallable {
                            found: current_type.to_string(),
                        });
                    }
                }
                self.unify(expected, &current_type, "function call return")?;
                Ok(Expr::Call {
                    callee: Box::new(self.check_expr(callee, &callee_type)?),
                    arguments: checked_args,
                })
            }
            Expr::Function {
                type_params,
                params,
                body,
                return_type,
            } => {
                let t = self.infer(&expr)?;

                let local_env = CheckerEnv::extend(Rc::clone(&self.env));
                for (name, ty) in params.iter().rev() {
                    local_env
                        .borrow_mut()
                        .insert_bind(name.clone(), self.resolve(ty)?)?;
                }
                let mut checker = Checker::new(local_env);
                let body_type = checker.infer(body)?;
                let return_type =
                    self.unify(&self.resolve(return_type)?, &body_type, "function body")?;
                self.unify(expected, &t, "function")?;
                Ok(Expr::Function {
                    type_params: type_params.clone(),
                    params: params.clone(),
                    body: Box::new(checker.check_expr(body, &body_type)?),
                    return_type: Box::new(return_type.clone().into()),
                })
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.infer(condition)?;
                if cond_type != TypeObject::Bool {
                    return Err(TypeError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: cond_type.to_string(),
                        context: "if condition".to_string(),
                    });
                }
                let then_type = self.infer(then_branch)?;
                let else_type = self.infer(else_branch)?;
                self.unify(&then_type, &else_type, "if branches")?;
                self.unify(expected, &then_type, "if expression")?;
                Ok(Expr::If {
                    condition: Box::new(self.check_expr(condition, &TypeObject::Bool)?),
                    then_branch: Box::new(self.check_expr(then_branch, &then_type)?),
                    else_branch: Box::new(self.check_expr(else_branch, &then_type)?),
                })
            }
            Expr::Match { expr, cases } => {
                let expr_type = self.infer(expr)?;
                let mut case_type = None;
                let mut checked_cases = Vec::new();
                for case in cases {
                    let pattern_type = self.infer_pattern(&case.pattern)?;
                    self.unify(&expr_type, &pattern_type, "match pattern")?;
                    let guard_type = self.infer(&case.guard)?;
                    if guard_type != TypeObject::Bool {
                        return Err(TypeError::TypeMismatch {
                            expected: "Bool".to_string(),
                            found: guard_type.to_string(),
                            context: "match guard".to_string(),
                        });
                    }
                    let body_type = self.infer(&case.body)?;
                    if let Some(ref prev_type) = case_type {
                        self.unify(prev_type, &body_type, "match case")?;
                    } else {
                        case_type = Some(body_type.clone());
                    }
                    checked_cases.push(MatchCase {
                        pattern: case.pattern.clone(),
                        body: Box::new(self.check_expr(&case.body, &body_type)?),
                        guard: Box::new(self.check_expr(&case.guard, &TypeObject::Bool)?),
                    });
                }
                let final_type = case_type.unwrap_or(TypeObject::Unit);
                self.unify(expected, &final_type, "match expression")?;
                Ok(Expr::Match {
                    expr: Box::new(self.check_expr(expr, &expr_type)?),
                    cases: checked_cases,
                })
            }
            Expr::LetIn {
                name,
                type_annotation,
                value,
                body,
            } => {
                let inferred_type = self.infer(value)?;
                let decl_type = self.resolve(type_annotation)?;
                let final_type = self.unify(&decl_type, &inferred_type, name)?;
                let local_env = CheckerEnv::extend(Rc::clone(&self.env));
                local_env
                    .borrow_mut()
                    .insert_bind(name.clone(), final_type.clone())
                    .map_err(|_| TypeError::RedefinedVariable(name.clone()))?;
                // std::mem::swap(&mut self.env, &mut local_env);
                let checked_body = self.check_expr(body, expected)?;
                // std::mem::swap(&mut self.env, &mut local_env);
                Ok(Expr::LetIn {
                    name: name.clone(),
                    type_annotation: Box::new(final_type.clone().into()),
                    value: Box::new(self.check_expr(value, &final_type)?),
                    body: Box::new(checked_body),
                })
            }
            Expr::Block(stmts) => {
                let mut checked_stmts = Vec::new();
                let mut last_type = TypeObject::Unit;
                for stmt in stmts {
                    match stmt {
                        Stmt::Expr(e) => {
                            last_type = self.infer(e)?;
                            checked_stmts.push(Stmt::Expr(self.check_expr(e, &last_type)?));
                        }
                        _ => checked_stmts.push(self.check_stmt(stmt)?),
                    }
                }
                self.unify(expected, &last_type, "block")?;
                Ok(Expr::Block(checked_stmts))
            }
        }
    }

    fn infer(&mut self, expr: &Expr) -> Result<TypeObject, TypeError> {
        match expr {
            Expr::Ident(name) | Expr::Internal(name) => self
                .env
                .borrow()
                .get_bind(name)
                .ok_or(TypeError::UndefinedVariable(name.clone())),
            Expr::Literal(lit) => self.infer_literal(lit),
            Expr::Prefix(op, expr) => {
                let expr_type = self.infer(expr)?;
                match op {
                    Token::Sub => match expr_type {
                        TypeObject::Int => Ok(TypeObject::Int),
                        TypeObject::Float => Ok(TypeObject::Float),
                        _ => Err(TypeError::InvalidOperation {
                            operation: "unary minus".to_string(),
                            typ: expr_type.to_string(),
                        }),
                    },
                    Token::Not => {
                        if expr_type == TypeObject::Bool {
                            Ok(TypeObject::Bool)
                        } else {
                            Err(TypeError::InvalidOperation {
                                operation: "logical not".to_string(),
                                typ: expr_type.to_string(),
                            })
                        }
                    }
                    _ => Err(TypeError::InvalidOperation {
                        operation: format!("prefix {:?}", op),
                        typ: expr_type.to_string(),
                    }),
                }
            }
            Expr::Infix(op, left, right) => {
                let left_type = self.infer(left)?;
                let right_type = self.infer(right)?;
                self.infer_infix(op, &left_type, &right_type)
            }
            Expr::Call { callee, arguments } => {
                let mut current_type = self.infer(callee)?;
                for (i, arg) in arguments.iter().enumerate() {
                    if let TypeObject::Function(param_type, return_type) = current_type {
                        let arg_type = self.infer(arg)?;
                        self.unify(&param_type, &arg_type, &format!("argument {}", i))?;
                        current_type = *return_type;
                    } else {
                        return Err(TypeError::NotCallable {
                            found: current_type.to_string(),
                        });
                    }
                }
                Ok(current_type)
            }
            Expr::Function {
                params,
                body,
                return_type,
                ..
            } => {
                let local_env = CheckerEnv::extend(Rc::clone(&self.env));
                for (name, ty) in params.iter().rev() {
                    local_env
                        .borrow_mut()
                        .insert_bind(name.clone(), self.resolve(ty)?)?;
                }
                let mut checker = Checker::new(local_env);
                let return_type = self.unify(
                    &self.resolve(return_type)?,
                    &checker.infer(body)?,
                    "function body",
                )?;
                Ok(self.resolve_function(params, &return_type)?)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.infer(condition)?;
                if cond_type != TypeObject::Bool {
                    return Err(TypeError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: cond_type.to_string(),
                        context: "if condition".to_string(),
                    });
                }
                let then_type = self.infer(then_branch)?;
                let else_type = self.infer(else_branch)?;
                self.unify(&then_type, &else_type, "if branches")?;
                Ok(then_type)
            }
            Expr::Match { expr, cases } => {
                let expr_type = self.infer(expr)?;
                let mut case_type = None;
                for case in cases {
                    let pattern_type = self.infer_pattern(&case.pattern)?;
                    self.unify(&expr_type, &pattern_type, "match pattern")?;
                    let guard_type = self.infer(&case.guard)?;
                    if guard_type != TypeObject::Bool {
                        return Err(TypeError::TypeMismatch {
                            expected: "Bool".to_string(),
                            found: guard_type.to_string(),
                            context: "match guard".to_string(),
                        });
                    }
                    let body_type = self.infer(&case.body)?;
                    if let Some(ref prev_type) = case_type {
                        self.unify(prev_type, &body_type, "match case")?;
                    } else {
                        case_type = Some(body_type);
                    }
                }
                Ok(case_type.unwrap_or(TypeObject::Unit))
            }
            Expr::LetIn {
                name,
                type_annotation,
                value,
                body,
            } => {
                let inferred_type = self.infer(value)?;
                let decl_type = self.resolve(type_annotation)?;
                let final_type = self.unify(&decl_type, &inferred_type, name)?;
                let local_env = CheckerEnv::extend(Rc::clone(&self.env));
                local_env
                    .borrow_mut()
                    .insert_bind(name.clone(), final_type)
                    .map_err(|_| TypeError::RedefinedVariable(name.clone()))?;
                // std::mem::swap(&mut self.env, &mut local_env);
                let body_type = self.infer(body)?;
                // std::mem::swap(&mut self.env, &mut local_env);
                Ok(body_type)
            }
            Expr::Block(stmts) => {
                let mut last_type = TypeObject::Unit;
                for stmt in stmts {
                    if let Stmt::Expr(e) = stmt {
                        last_type = self.infer(e)?;
                    } else {
                        self.check_stmt(stmt)?;
                    }
                }
                Ok(last_type)
            }
        }
    }

    fn infer_literal(&self, lit: &Literal) -> Result<TypeObject, TypeError> {
        Ok(match lit {
            Literal::Int(_) => TypeObject::Int,
            Literal::Float(_) => TypeObject::Float,
            Literal::String(_) => TypeObject::String,
            Literal::Char(_) => TypeObject::Char,
            Literal::Bool(_) => TypeObject::Bool,
            _ => unimplemented!(),
        })
    }

    fn infer_infix(
        &self,
        op: &Token,
        left_type: &TypeObject,
        right_type: &TypeObject,
    ) -> Result<TypeObject, TypeError> {
        match op {
            Token::Plus | Token::Sub | Token::Mul | Token::Div => {
                if left_type == right_type {
                    match left_type {
                        TypeObject::Int => Ok(TypeObject::Int),
                        TypeObject::Float => Ok(TypeObject::Float),
                        _ => Err(TypeError::InvalidOperation {
                            operation: format!("arithmetic {:?}", op),
                            typ: left_type.to_string(),
                        }),
                    }
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: left_type.to_string(),
                        found: right_type.to_string(),
                        context: "arithmetic operation".to_string(),
                    })
                }
            }
            Token::Equal | Token::NotEqual => {
                self.unify(left_type, right_type, "comparison")?;
                Ok(TypeObject::Bool)
            }
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                if left_type == right_type
                    && matches!(left_type, TypeObject::Int | TypeObject::Float)
                {
                    Ok(TypeObject::Bool)
                } else {
                    Err(TypeError::InvalidOperation {
                        operation: format!("comparison {:?}", op),
                        typ: left_type.to_string(),
                    })
                }
            }
            Token::And | Token::Or => {
                if left_type == &TypeObject::Bool && right_type == &TypeObject::Bool {
                    Ok(TypeObject::Bool)
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: if left_type != &TypeObject::Bool {
                            left_type.to_string()
                        } else {
                            right_type.to_string()
                        },
                        context: "logical operation".to_string(),
                    })
                }
            }
            _ => Err(TypeError::InvalidOperation {
                operation: format!("infix {:?}", op),
                typ: left_type.to_string(),
            }),
        }
    }

    fn infer_pattern(&self, pattern: &Pattern) -> Result<TypeObject, TypeError> {
        match pattern {
            Pattern::Ident(name) => Ok(self
                .env
                .borrow()
                .get_bind(name)
                .unwrap_or(TypeObject::Unknown)),
            Pattern::Literal(lit) => self.infer_literal(lit),
            Pattern::ADTConstructor { name, args } => {
                let adt_type = self
                    .env
                    .borrow()
                    .get_bind(name)
                    .ok_or(TypeError::UndefinedVariable(name.clone()))?;
                if let TypeObject::ADT {
                    name: adt_name,
                    constructors,
                    ..
                } = adt_type
                {
                    for (ctor_name, field_types) in constructors.clone() {
                        if ctor_name == name.clone() {
                            if field_types.len() != args.len() {
                                return Err(TypeError::ArityMismatch {
                                    expected: field_types.len(),
                                    found: args.len(),
                                    context: format!("constructor '{}'", name),
                                });
                            }
                            for (arg, field_type) in args.iter().zip(field_types.iter()) {
                                let arg_type = self.infer_pattern(arg)?;
                                let resolved_field_type = self.resolve(field_type)?;
                                self.unify(&resolved_field_type, &arg_type, "constructor arg")?;
                            }
                            return Ok(TypeObject::ADT {
                                name: adt_name,
                                type_params: vec![],
                                constructors,
                            });
                        }
                    }
                    Err(TypeError::UndefinedVariable(format!(
                        "Constructor '{}' not found in ADT",
                        name
                    )))
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: "ADT".to_string(),
                        found: adt_type.to_string(),
                        context: format!("constructor '{}'", name),
                    })
                }
            }
        }
    }

    fn resolve(&self, ty: &TypeExpr) -> Result<TypeObject, TypeError> {
        match ty {
            TypeExpr::Var(name) | TypeExpr::Con(name) => self
                // .type_vars
                .env
                .borrow()
                .get_bind(name)
                // .cloned()
                .ok_or(TypeError::UndefinedVariable(name.clone())),
            TypeExpr::App(ty, args) => {
                let base_type = self.resolve(ty)?;
                if let TypeObject::ADT {
                    name,
                    type_params,
                    constructors,
                } = base_type
                {
                    if type_params.len() == args.len() {
                        Ok(TypeObject::ADT {
                            name,
                            type_params,
                            constructors,
                        })
                    } else {
                        Err(TypeError::ArityMismatch {
                            expected: type_params.len(),
                            found: args.len(),
                            context: format!("type application '{}'", name),
                        })
                    }
                } else {
                    Err(TypeError::InvalidOperation {
                        operation: "type application".to_string(),
                        typ: base_type.to_string(),
                    })
                }
            }
            TypeExpr::Arrow(left, right) => Ok(TypeObject::Function(
                Box::new(self.resolve(left)?),
                Box::new(self.resolve(right)?),
            )),
            TypeExpr::Literal(lit) => self.infer_literal(lit),
        }
    }

    fn resolve_variant_fields(&self, fields: &TypeVariantFields) -> Vec<TypeExpr> {
        match fields {
            TypeVariantFields::Tuple(types) => types.iter().map(|t| *t.clone()).collect(),
            TypeVariantFields::Record(fields) => fields.iter().map(|(_, t)| *t.clone()).collect(),
            TypeVariantFields::Unit => vec![],
        }
    }

    fn resolve_function(
        &self,
        params: &Vec<(String, Box<TypeExpr>)>,
        return_type: &TypeObject,
    ) -> Result<TypeObject, TypeError> {
        if params.is_empty() {
            Ok(TypeObject::Function(
                Box::new(TypeObject::Unit),
                Box::new(return_type.clone()),
            ))
        } else {
            let mut func_type = None;
            for (_, param_type) in params.iter() {
                func_type = Some(if let Some(func_type) = func_type {
                    TypeObject::Function(Box::new(self.resolve(param_type)?), Box::new(func_type))
                } else {
                    TypeObject::Function(
                        Box::new(self.resolve(param_type)?),
                        Box::new(return_type.clone()),
                    )
                })
            }
            func_type.ok_or(TypeError::InvalidOperation {
                operation: "function".to_string(),
                typ: "empty parameter list".to_string(),
            })
        }
    }

    fn unify(
        &self,
        expected: &TypeObject,
        actual: &TypeObject,
        context: &str,
    ) -> Result<TypeObject, TypeError> {
        match (expected, actual) {
            (TypeObject::Unknown, t) => Ok(t.clone()),
            (t, TypeObject::Unknown) => Ok(t.clone()),
            (TypeObject::Function(e_param, e_ret), TypeObject::Function(a_param, a_ret)) => {
                let unified_param = self.unify(e_param, a_param, context)?;
                let unified_ret = self.unify(e_ret, a_ret, context)?;
                Ok(TypeObject::Function(
                    Box::new(unified_param),
                    Box::new(unified_ret),
                ))
            }
            (TypeObject::ADT { name: e_name, .. }, TypeObject::ADT { name: a_name, .. })
                if e_name == a_name =>
            {
                Ok(expected.clone())
            }
            (e, a) if e == a => Ok(e.clone()),
            (e, a) => Err(TypeError::TypeMismatch {
                expected: e.to_string(),
                found: a.to_string(),
                context: context.to_string(),
            }),
        }
    }
}
