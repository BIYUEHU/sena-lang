use crate::checker::object::TypeObject;
use crate::env::CheckerEnv;
use crate::lexer::token::Token;
use crate::parser::ast::{
    Expr, Kind, Literal, Pattern, Stmt, TypeExpr, TypeVariantFields, UnsafeProgram,
};
use ast::{
    Checked, CheckedCase, CheckedExpr, CheckedStmt, CheckedTypeVariant, CheckedTypeVariantFields,
};
use error::TypeError;
use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

pub mod ast;
pub mod error;
pub mod object;

pub struct Checker {
    env: Rc<RefCell<CheckerEnv>>,
}

impl Checker {
    fn infer_literal(literal: &Literal) -> Result<TypeObject, TypeError> {
        Ok(match literal {
            Literal::Int(_) => TypeObject::Int,
            Literal::Float(_) => TypeObject::Float,
            Literal::String(_) => TypeObject::String,
            Literal::Char(_) => TypeObject::Char,
            Literal::Bool(_) => TypeObject::Bool,
            Literal::Unit => TypeObject::Unit,
            _ => TypeObject::Unknown,
        })
    }

    pub fn resolve_kind(type_object: &TypeObject) -> Result<Kind, TypeError> {
        match type_object {
            TypeObject::Kind(kind) => Ok(kind.clone()),
            _ => Err(TypeError::TypeMismatch {
                expected: "Kind".into(),
                found: format!("{}", type_object),
                context: "".into(),
            }),
        }
    }

    pub fn resolve(
        type_expr: &TypeExpr,
        env: Rc<RefCell<CheckerEnv>>,
    ) -> Result<TypeObject, TypeError> {
        match type_expr {
            TypeExpr::Var(name) | TypeExpr::Con(name) => env
                .borrow()
                .get_bind(name)
                .ok_or(TypeError::UndefinedVariable(name.clone())),
            TypeExpr::App(constructor, args) => {
                let base = Checker::resolve(constructor, Rc::clone(&env))?;
                let mut param_objs = Vec::with_capacity(args.len());
                for arg in args {
                    param_objs.push(Checker::resolve(arg, Rc::clone(&env))?);
                }
                match base {
                    TypeObject::ADTDef { name, .. } => Ok(TypeObject::ADTInst {
                        name,
                        params: param_objs,
                    }),
                    _ => Err(TypeError::InvalidOperation {
                        operation: "type application".into(),
                        typ: format!("{:?}", base),
                    }),
                }
            }
            // a -> b
            TypeExpr::Arrow(left, right) => Ok(TypeObject::Function(
                Box::new(Checker::resolve(left, Rc::clone(&env))?),
                Box::new(Checker::resolve(right, Rc::clone(&env))?),
            )),
            // System F ∀a. T
            TypeExpr::Forall(binders, body) => {
                let inner = *body.clone();
                // 先将所有量化变量插入临时环境，指明它们的 kind（此处默认 Kind）
                let saved = env.borrow().clone();
                {
                    let mut env = env.borrow_mut();
                    for (var, _) in binders {
                        env.insert_bind(var.clone(), TypeObject::Kind(Kind::Star))
                            .map_err(|_| TypeError::RedefinedVariable(var.clone()))?;
                    }
                }
                let ty_body = Checker::resolve(&inner, Rc::clone(&env))?;
                *env.borrow_mut() = saved;
                Ok(TypeObject::Forall(
                    binders
                        .iter()
                        .map(|(n, _)| (n.clone(), TypeObject::Kind(Kind::Star)))
                        .collect(),
                    Box::new(ty_body),
                ))
            }

            // λ(a :: K). T
            TypeExpr::Lambda((param, body_ty), body) => {
                let param_kind = Checker::resolve(body_ty, Rc::clone(&env))?;
                let saved = env.borrow().clone();
                {
                    let mut env = env.borrow_mut();
                    env.insert_bind(param.clone(), param_kind.clone())
                        .map_err(|_| TypeError::RedefinedVariable(param.clone()))?;
                }
                let ty_body = Checker::resolve(body, Rc::clone(&env))?;
                *env.borrow_mut() = saved;
                Ok(TypeObject::Lambda(
                    (param.clone(), Box::new(param_kind)),
                    Box::new(ty_body),
                ))
            }
            TypeExpr::Literal(lit) => Checker::infer_literal(lit),
            TypeExpr::Kind(kind) => Ok(match kind {
                Kind::Star => TypeObject::Kind(Kind::Star),
                Kind::Arrow(param_type, return_type) => TypeObject::Function(
                    Box::new(Checker::resolve(
                        &TypeExpr::Kind(*param_type.clone()),
                        Rc::clone(&env),
                    )?),
                    Box::new(Checker::resolve(
                        &TypeExpr::Kind(*return_type.clone()),
                        Rc::clone(&env),
                    )?),
                ),
            }),
        }
    }

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
                let expected_type = Checker::resolve(type_annotation, Rc::clone(&self.env))?;
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
                params,
                variants,
                kind_annotation,
            } => {
                let kind_annotation = Checker::resolve(kind_annotation, Rc::clone(&self.env))?;
                let checked_kind_annotation = Checker::resolve_kind(&kind_annotation)?;
                self.env
                    .borrow_mut()
                    .insert_bind(name.clone(), kind_annotation.clone())?;
                Ok(CheckedStmt::Type {
                    name: name.clone(),
                    params: params.clone(),
                    variants: {
                        let mut checked_variants = vec![];
                        for variant in variants {
                            checked_variants.push(match variant.clone().fields {
                                TypeVariantFields::Unit => CheckedTypeVariant {
                                    name: variant.clone().name,
                                    fields: CheckedTypeVariantFields::Unit,
                                },
                                TypeVariantFields::Tuple(fields) => CheckedTypeVariant {
                                    name: variant.clone().name,
                                    fields: CheckedTypeVariantFields::Tuple(
                                        fields
                                            .iter()
                                            .map(|f| Checker::resolve(f, Rc::clone(&self.env)))
                                            .collect::<Result<Vec<_>, _>>()?,
                                    ),
                                },
                                _ => unimplemented!("record type variant"),
                            });
                        }
                        checked_variants
                    },
                    kind_annotation: checked_kind_annotation,
                })
            }
            Stmt::Expr(expression) => {
                let checked_expression = self.infer_expr(expression)?;
                Ok(CheckedStmt::Expr(checked_expression))
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
                let literal_type = Checker::infer_literal(literal)?;
                Ok(CheckedExpr::Literal {
                    value: literal.clone(),
                    type_annotation: literal_type,
                })
            }
            Expr::Prefix(operator, sub_expr) => {
                let checked_sub = self.infer_expr(sub_expr)?;
                let result_type = self.infer_prefix(operator, &checked_sub.get_type())?;
                Ok(CheckedExpr::Prefix {
                    op: operator.clone(),
                    expr: Box::new(checked_sub),
                    type_annotation: result_type,
                })
            }
            Expr::Infix(operator, left_expr, right_expr) => {
                let checked_left = self.infer_expr(left_expr)?;
                let checked_right = self.infer_expr(right_expr)?;
                let result_type = self.infer_infix(
                    operator,
                    &checked_left.get_type(),
                    &checked_right.get_type(),
                )?;
                Ok(CheckedExpr::Infix {
                    op: operator.clone(),
                    left: Box::new(checked_left),
                    right: Box::new(checked_right),
                    type_annotation: result_type,
                })
            }
            Expr::Call { callee, params, .. } => {
                let checked_callee = self.infer_expr(callee)?;
                let mut checked_args = Vec::new();
                let mut current_type = checked_callee.get_type();
                for param in params {
                    let checked_arg = self.infer_expr(param)?;
                    if let TypeObject::Function(param_type, return_type) = current_type {
                        self.unify(
                            &param_type,
                            &checked_arg.get_type(),
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
                    params: checked_args,
                    type_annotation: current_type,
                })
            }
            Expr::Function {
                params,
                body,
                type_annotation,
            } => Ok(CheckedExpr::Function {
                params: params.clone(),
                body: Box::new(
                    Checker::new(CheckerEnv::extend(Rc::clone(&self.env))).check_expr(
                        body,
                        self.type_to_vec(&Checker::resolve(
                            &type_annotation,
                            Rc::clone(&self.env),
                        )?)?
                        .last()
                        .unwrap(),
                    )?,
                ),
                type_annotation: Checker::resolve(type_annotation, Rc::clone(&self.env))?,
            }),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let checked_condition = self.check_expr(condition, &TypeObject::Bool)?;
                let checked_then = self.infer_expr(then_branch)?;
                let checked_else = self.check_expr(else_branch, &checked_then.get_type())?;
                Ok(CheckedExpr::If {
                    condition: Box::new(checked_condition),
                    then_branch: Box::new(checked_then),
                    else_branch: Box::new(checked_else.clone()),
                    type_annotation: checked_else.get_type(),
                })
            }
            Expr::LetIn {
                name,
                type_annotation,
                value,
                body,
            } => {
                let expected_type = Checker::resolve(type_annotation, Rc::clone(&self.env))?;
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
                    type_annotation: checked_body.get_type(),
                })
            }
            Expr::Match { expr, cases } => {
                let checked_matched = self.infer_expr(expr)?;
                let mut result_type = TypeObject::Unknown;
                let mut checked_cases = Vec::new();
                for case in cases {
                    let pattern_type = self.infer_pattern(&case.pattern)?;
                    self.unify(&checked_matched.get_type(), &pattern_type, "match pattern")?;
                    let checked_guard = self.check_expr(&case.guard, &TypeObject::Bool)?;
                    let checked_body = self.infer_expr(&case.body)?;
                    if result_type == TypeObject::Unknown {
                        result_type = checked_body.get_type();
                    } else {
                        self.unify(&result_type, &checked_body.get_type(), "match body")?;
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
                    if let CheckedStmt::Expr(expr) = &cs {
                        last_expression_type = expr.get_type();
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
        if &node.get_type() == expected_type {
            Ok(node)
        } else {
            Err(TypeError::TypeMismatch {
                expected: expected_type.to_string(),
                found: node.get_type().to_string(),
                context: format!("{:?}", expression),
            })
        }
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
            (TypeObject::Function(ea, eb), Function(aa, ab)) => {
                let p = self.unify(ea, aa, context)?;
                let r = self.unify(eb, ab, context)?;
                Ok(Function(Box::new(p), Box::new(r)))
            }
            (TypeObject::ADTDef { name: en, .. }, TypeObject::ADTDef { name: an, .. })
                if en == an =>
            {
                Ok(expected.clone())
            }
            (e, a) if e == a => Ok(e.clone()),
            _ => Err(TypeError::TypeMismatch {
                expected: expected.to_string(),
                found: actual.to_string(),
                context: context.to_string(),
            }),
        }
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
                self.unify(l, r, "cmp")?;
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

    fn infer_pattern(&self, pattern: &Pattern) -> Result<TypeObject, TypeError> {
        match pattern {
            Pattern::Ident(n) => Ok(self.env.borrow().get_bind(n).unwrap_or(TypeObject::Unknown)),
            Pattern::Literal(l) => Checker::infer_literal(l),
            Pattern::ADTConstructor { name, .. } => {
                let t = self
                    .env
                    .borrow()
                    .get_bind(name)
                    .ok_or(TypeError::UndefinedVariable(name.clone()))?;
                if let TypeObject::ADTDef {
                    name: _,
                    constructors,
                    ..
                } = t.clone()
                {
                    for (constructor, _definition) in constructors {
                        if &constructor == name {
                            // if fts.len() != args.len() {
                            //     return Err(TypeError::ArityMismatch {
                            //         expected: fts.len(),
                            //         found: args.len(),
                            //         context: format!("ctor {}", name),
                            //     });
                            // // }
                            // for (p, ft) in args.iter().zip(fts.iter()) {
                            //     let pt = self.infer_pattern(p)?;
                            //     let rt = self.resolve(ft)?;
                            //     self.unify(&rt, &pt, "pat arg")?;
                            // }
                            return Ok(t);
                        }
                    }
                }
                Err(TypeError::UndefinedVariable(name.clone()))
            }
        }
    }

    fn type_to_vec(&self, func_type: &TypeObject) -> Result<Vec<TypeObject>, TypeError> {
        match func_type {
            TypeObject::Function(param_type, return_type) => Ok([
                self.type_to_vec(param_type)?,
                self.type_to_vec(return_type)?,
            ]
            .concat()),
            _ => Ok(vec![func_type.clone()]),
        }
    }
}
