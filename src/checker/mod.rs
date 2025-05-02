use crate::checker::object::TypeObject;
use crate::env::CheckerEnv;
use crate::lexer::token::Token;
use crate::parser::ast::{
    Expr, Literal, Pattern, Stmt, TypeExpr, TypeVariantFields, UnsafeProgram,
};
use ast::{CheckedCase, CheckedExpr, CheckedStmt};
use error::TypeError;
use std::cell::RefCell;
use std::rc::Rc;

pub mod ast;
pub mod error;
pub mod object;

pub struct Checker {
    env: Rc<RefCell<CheckerEnv>>,
}

impl Checker {
    pub fn new(env: Rc<RefCell<CheckerEnv>>) -> Self {
        Checker { env }
    }

    /// Top-level: check all statements, returning typed AST
    pub fn check(&self, program: &UnsafeProgram) -> Result<Vec<CheckedStmt>, TypeError> {
        program.iter().map(|s| self.check_stmt(s)).collect()
    }

    fn check_stmt(&self, stmt: &Stmt) -> Result<CheckedStmt, TypeError> {
        match stmt {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                let expected_type = self.resolve(type_annotation)?;
                let checked_value = self.check_expr(value, &expected_type)?;
                self.env
                    .borrow_mut()
                    .insert_bind(name.clone(), expected_type.clone())?;
                Ok(CheckedStmt::Let {
                    name: name.clone(),
                    type_annotation: expected_type,
                    value: checked_value,
                })
            }
            Stmt::Type {
                name,
                type_params,
                variants,
                ..
            } => {
                let type_definition = self.build_adt(name, type_params, variants)?;
                self.env
                    .borrow_mut()
                    .insert_bind(name.clone(), type_definition.clone())?;
                Ok(CheckedStmt::Type {
                    name: name.clone(),
                    type_annotation: type_definition,
                    type_params: type_params.clone(),
                })
            }
            Stmt::Expr(expression) => {
                let checked_expression = self.infer_expr(expression)?;
                Ok(CheckedStmt::Expr {
                    value: checked_expression,
                })
            }
            Stmt::ImportAll { source, alias } => Ok(CheckedStmt::ImportAll {
                source: source.clone(),
                alias: alias.clone(),
            }),
            Stmt::ImportSome { source, items } => Ok(CheckedStmt::ImportSome {
                source: source.clone(),
                items: items.clone(),
            }),
            Stmt::Export {
                body,
                only_abstract,
            } => {
                let checked_body = Box::new(self.check_stmt(body)?);
                Ok(CheckedStmt::Export {
                    body: checked_body,
                    only_abstract: *only_abstract,
                })
            }
        }
    }

    pub fn infer_expr(&self, expression: &Expr) -> Result<CheckedExpr, TypeError> {
        match expression {
            Expr::Ident(name) | Expr::Internal(name) => {
                let found_type = self
                    .env
                    .borrow()
                    .get_bind(name)
                    .ok_or(TypeError::UndefinedVariable(name.clone()))?;
                Ok(CheckedExpr::Ident {
                    value: name.clone(),
                    type_annotation: found_type,
                })
            }
            Expr::Literal(literal) => {
                let literal_type = self.infer_literal(literal)?;
                Ok(CheckedExpr::Literal {
                    value: literal.clone(),
                    type_annotation: literal_type,
                })
            }
            Expr::Prefix(operator, sub_expr) => {
                let checked_sub = self.infer_expr(sub_expr)?;
                let result_type = self.infer_prefix(operator, checked_sub.get_type())?;
                Ok(CheckedExpr::Prefix {
                    op: operator.clone(),
                    expr: Box::new(checked_sub),
                    type_annotation: result_type,
                })
            }
            Expr::Infix(operator, left_expr, right_expr) => {
                let checked_left = self.infer_expr(left_expr)?;
                let checked_right = self.infer_expr(right_expr)?;
                let result_type =
                    self.infer_infix(operator, checked_left.get_type(), checked_right.get_type())?;
                Ok(CheckedExpr::Infix {
                    op: operator.clone(),
                    left: Box::new(checked_left),
                    right: Box::new(checked_right),
                    type_annotation: result_type,
                })
            }
            Expr::Call { callee, arguments } => {
                let checked_callee = self.infer_expr(callee)?;
                let mut checked_args = Vec::new();
                let mut current_type = checked_callee.clone_type();
                for arg in arguments {
                    let checked_arg = self.infer_expr(arg)?;
                    if let TypeObject::Function(param_type, return_type) = current_type {
                        self.unify(
                            &param_type,
                            checked_arg.get_type(),
                            "function call argument",
                        )?;
                        current_type = *return_type;
                        checked_args.push(checked_arg);
                    } else {
                        return Err(TypeError::NotCallable {
                            found: current_type.to_string(),
                        });
                    }
                }
                Ok(CheckedExpr::Call {
                    callee: Box::new(checked_callee),
                    args: checked_args,
                    type_annotation: current_type,
                })
            }
            Expr::Function {
                params,
                body,
                return_type,
                ..
            } => {
                let extended_env = CheckerEnv::extend(Rc::clone(&self.env));
                let nested_checker = Checker::new(extended_env);
                let mut param_list = Vec::new();
                for (param_name, type_annotation) in params {
                    let param_type = self.resolve(type_annotation)?;
                    nested_checker
                        .env
                        .borrow_mut()
                        .insert_bind(param_name.clone(), param_type.clone())?;
                    param_list.push((param_name.clone(), param_type));
                }
                let checked_body = nested_checker.check_expr(body, &self.resolve(return_type)?)?;
                let function_type = TypeObject::Function(
                    Box::new(param_list.first().unwrap().1.clone()),
                    Box::new(checked_body.clone_type()),
                );
                Ok(CheckedExpr::Function {
                    params: param_list,
                    body: Box::new(checked_body),
                    type_annotation: function_type,
                })
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let checked_condition = self.check_expr(condition, &TypeObject::Bool)?;
                let checked_then = self.infer_expr(then_branch)?;
                let checked_else = self.check_expr(else_branch, checked_then.get_type())?;
                Ok(CheckedExpr::If {
                    cond: Box::new(checked_condition),
                    then_branch: Box::new(checked_then),
                    else_branch: Box::new(checked_else.clone()),
                    type_annotation: checked_else.clone_type(),
                })
            }
            Expr::LetIn {
                name,
                type_annotation,
                value,
                body,
            } => {
                let expected_type = self.resolve(type_annotation)?;
                let checked_value = self.check_expr(value, &expected_type)?;
                let extended_env = CheckerEnv::extend(Rc::clone(&self.env));
                extended_env
                    .borrow_mut()
                    .insert_bind(name.clone(), expected_type.clone())?;
                let nested_checker = Checker::new(extended_env);
                let checked_body = nested_checker.infer_expr(body)?;
                Ok(CheckedExpr::LetIn {
                    name: name.clone(),
                    value: Box::new(checked_value),
                    body: Box::new(checked_body.clone()),
                    type_annotation: checked_body.clone_type(),
                })
            }
            Expr::Match { expr, cases } => {
                let checked_matched = self.infer_expr(expr)?;
                let mut result_type = TypeObject::Unknown;
                let mut checked_cases = Vec::new();
                for case in cases {
                    let pattern_type = self.infer_pattern(&case.pattern)?;
                    self.unify(checked_matched.get_type(), &pattern_type, "match pattern")?;
                    let checked_guard = self.check_expr(&case.guard, &TypeObject::Bool)?;
                    let checked_body = self.infer_expr(&case.body)?;
                    if result_type == TypeObject::Unknown {
                        result_type = checked_body.clone_type();
                    } else {
                        self.unify(&result_type, checked_body.get_type(), "match body")?;
                    }
                    checked_cases.push(CheckedCase {
                        pattern: case.pattern.clone(),
                        guard: checked_guard,
                        body: checked_body,
                    });
                }
                Ok(CheckedExpr::Match {
                    expr: Box::new(checked_matched),
                    cases: checked_cases,
                    type_annotation: result_type,
                })
            }
            Expr::Block(statements) => {
                let mut last_expression_type = TypeObject::Unit;
                let mut checked_statements = Vec::new();
                for s in statements {
                    let cs = self.check_stmt(s)?;
                    if let CheckedStmt::Expr { expr } = &cs {
                        last_expression_type = expr.clone_type();
                    }
                    checked_statements.push(cs);
                }
                Ok(CheckedExpr::Block {
                    stmts: checked_statements,
                    type_annotation: last_expression_type,
                })
            }
        }
    }

    pub fn check_expr(
        &self,
        expression: &Expr,
        expected_type: &TypeObject,
    ) -> Result<CheckedExpr, TypeError> {
        let node = self.infer_expr(expression)?;
        if node.get_type() != expected_type {
            return Err(TypeError::TypeMismatch {
                expected: expected_type.to_string(),
                found: node.get_type().to_string(),
                context: format!("{:?}", expression),
            });
        }
        Ok(node)
    }

    fn unify(
        &self,
        expected: &TypeObject,
        actual: &TypeObject,
        context: &str,
    ) -> Result<TypeObject, TypeError> {
        use TypeObject::*;
        match (expected, actual) {
            (TypeObject::Unknown, t) | (t, TypeObject::Unknown) => Ok(t.clone()),
            (Function(ea, eb), Function(aa, ab)) => {
                let p = self.unify(ea, aa, context)?;
                let r = self.unify(eb, ab, context)?;
                Ok(Function(Box::new(p), Box::new(r)))
            }
            (ADT { name: en, .. }, ADT { name: an, .. }) if en == an => Ok(expected.clone()),
            (e, a) if e == a => Ok(e.clone()),
            _ => Err(TypeError::TypeMismatch {
                expected: expected.to_string(),
                found: actual.to_string(),
                context: context.to_string(),
            }),
        }
    }

    fn resolve(&self, type_expr: &TypeExpr) -> Result<TypeObject, TypeError> {
        match type_expr {
            TypeExpr::Var(n) | TypeExpr::Con(n) => self
                .env
                .borrow()
                .get_bind(n)
                .ok_or(TypeError::UndefinedVariable(n.clone())),
            TypeExpr::App(t, args) => {
                let base = self.resolve(t)?;
                if let TypeObject::ADT {
                    name,
                    type_params,
                    constructors,
                } = base
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
                            context: format!("type {}", name),
                        })
                    }
                } else {
                    Err(TypeError::InvalidOperation {
                        operation: "type app".into(),
                        typ: format!("{:?}", base),
                    })
                }
            }
            TypeExpr::Arrow(l, r) => Ok(TypeObject::Function(
                Box::new(self.resolve(l)?),
                Box::new(self.resolve(r)?),
            )),
            TypeExpr::Literal(l) => self.infer_literal(l),
        }
    }

    fn infer_literal(&self, lit: &Literal) -> Result<TypeObject, TypeError> {
        Ok(match lit {
            Literal::Int(_) => TypeObject::Int,
            Literal::Float(_) => TypeObject::Float,
            Literal::String(_) => TypeObject::String,
            Literal::Char(_) => TypeObject::Char,
            Literal::Bool(_) => TypeObject::Bool,
            Literal::Unit => TypeObject::Unit,
            _ => TypeObject::Unknown,
        })
    }

    fn infer_infix(
        &self,
        op: &Token,
        l: &TypeObject,
        r: &TypeObject,
    ) -> Result<TypeObject, TypeError> {
        use TypeObject::*;
        match op {
            Token::Plus | Token::Sub | Token::Mul | Token::Div => {
                if l == r {
                    match l {
                        Int => Ok(Int),
                        Float => Ok(Float),
                        _ => Err(TypeError::InvalidOperation {
                            operation: format!("arith {:?}", op),
                            typ: l.to_string(),
                        }),
                    }
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: l.to_string(),
                        found: r.to_string(),
                        context: "arith".into(),
                    })
                }
            }
            Token::Equal | Token::NotEqual => {
                self.unify(l, r, "cmp");
                Ok(TypeObject::Bool)
            }
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                if l == r && (matches!(l, Int | Float)) {
                    Ok(TypeObject::Bool)
                } else {
                    Err(TypeError::InvalidOperation {
                        operation: format!("cmp {:?}", op),
                        typ: l.to_string(),
                    })
                }
            }
            Token::And | Token::Or => {
                if l == &TypeObject::Bool && r == &TypeObject::Bool {
                    Ok(TypeObject::Bool)
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: "Bool".into(),
                        found: l.to_string(),
                        context: "logic".into(),
                    })
                }
            }
            _ => Err(TypeError::InvalidOperation {
                operation: format!("infix {:?}", op),
                typ: l.to_string(),
            }),
        }
    }

    fn infer_prefix(&self, op: &Token, sub: &TypeObject) -> Result<TypeObject, TypeError> {
        match op {
            Token::Sub => match sub {
                TypeObject::Int => Ok(TypeObject::Int),
                TypeObject::Float => Ok(TypeObject::Float),
                _ => Err(TypeError::InvalidOperation {
                    operation: "unary-".into(),
                    typ: sub.to_string(),
                }),
            },
            Token::Not if sub == &TypeObject::Bool => Ok(TypeObject::Bool),
            _ => Err(TypeError::InvalidOperation {
                operation: format!("prefix {:?}", op),
                typ: sub.to_string(),
            }),
        }
    }

    fn infer_pattern(&self, pat: &Pattern) -> Result<TypeObject, TypeError> {
        match pat {
            Pattern::Ident(n) => Ok(self.env.borrow().get_bind(n).unwrap_or(TypeObject::Unknown)),
            Pattern::Literal(l) => self.infer_literal(l),
            Pattern::ADTConstructor { name, args } => {
                let t = self
                    .env
                    .borrow()
                    .get_bind(name)
                    .ok_or(TypeError::UndefinedVariable(name.clone()))?;
                if let TypeObject::ADT {
                    name: _,
                    constructors,
                    ..
                } = t.clone()
                {
                    for (ctor, fts) in constructors {
                        if &ctor == name {
                            if fts.len() != args.len() {
                                return Err(TypeError::ArityMismatch {
                                    expected: fts.len(),
                                    found: args.len(),
                                    context: format!("ctor {}", name),
                                });
                            }
                            for (p, ft) in args.iter().zip(fts.iter()) {
                                let pt = self.infer_pattern(p)?;
                                let rt = self.resolve(ft)?;
                                self.unify(&rt, &pt, "pat arg")?;
                            }
                            return Ok(t);
                        }
                    }
                }
                Err(TypeError::UndefinedVariable(name.clone()))
            }
        }
    }

    fn build_adt(
        &self,
        name: &String,
        type_params: &Option<Vec<String>>,
        variants: &Vec<crate::parser::ast::TypeVariant>,
    ) -> Result<TypeObject, TypeError> {
        let params = type_params.clone().unwrap_or_default();
        let mut ctors = Vec::new();
        for var in variants {
            let fields = match &var.fields {
                TypeVariantFields::Tuple(ts) => ts.iter().map(|t| *t.clone()).collect(),
                TypeVariantFields::Record(r) => r.iter().map(|(_, t)| *t.clone()).collect(),
                TypeVariantFields::Unit => vec![],
            };
            ctors.push((var.name.clone(), fields));
        }
        Ok(TypeObject::ADT {
            name: name.clone(),
            type_params: params,
            constructors: ctors,
        })
    }
}
