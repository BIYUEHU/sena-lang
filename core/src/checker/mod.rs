use crate::checker::object::TypeObject;
use crate::env::CheckerEnv;
use crate::lexer::token::Token;
use crate::parser::ast::{Expr, Kind, Literal, Pattern, Stmt, TypeExpr, TypeVariantFields};
use crate::utils::{num_to_kind, vec_to_kind, vec_to_type};
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
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
struct Constraint(pub TypeObject, pub TypeObject);

#[derive(Debug, Clone)]
struct Substitution(HashMap<String, TypeObject>);

impl Substitution {
    fn new() -> Self {
        Substitution(HashMap::new())
    }

    fn apply(&self, ty: &TypeObject) -> TypeObject {
        match ty {
            TypeObject::Var(var_id) => {
                if let Some(t) = self.0.get(var_id) {
                    self.apply(t)
                } else {
                    TypeObject::Var(var_id.clone())
                }
            }
            TypeObject::Function(a, b) => {
                TypeObject::Function(Box::new(self.apply(a)), Box::new(self.apply(b)))
            }
            TypeObject::Forall(params, body) => {
                TypeObject::Forall(params.clone(), Box::new(self.apply(body)))
            }
            TypeObject::Lambda((param, kind), body) => {
                TypeObject::Lambda((param.clone(), kind.clone()), Box::new(self.apply(body)))
            }
            TypeObject::ADTInst { name, params } => TypeObject::ADTInst {
                name: name.clone(),
                params: params.iter().map(|t| self.apply(t)).collect(),
            },
            _ => ty.clone(),
        }
    }

    fn compose(self, other: Substitution) -> Substitution {
        let mut result = Substitution::new();
        for (v, t) in other.0.iter() {
            result.0.insert(v.clone(), self.apply(t));
        }
        for (v, t) in self.0.into_iter() {
            if !result.0.contains_key(&v) {
                result.0.insert(v, t);
            }
        }
        result
    }
}

fn occurs_check(var_id: String, ty: &TypeObject) -> bool {
    match ty {
        TypeObject::Var(v) => *v == var_id,
        TypeObject::Function(a, b) => occurs_check(var_id.clone(), a) || occurs_check(var_id, b),
        TypeObject::Forall(_, body) => occurs_check(var_id, body),
        TypeObject::Lambda((_, _), body) => occurs_check(var_id, body),
        TypeObject::ADTInst { params, .. } => {
            params.iter().any(|t| occurs_check(var_id.clone(), t))
        }
        _ => false,
    }
}

fn unify(constraints: Vec<Constraint>) -> Result<Substitution, TypeError> {
    let mut subst = Substitution::new();
    let mut queue: VecDeque<Constraint> = constraints.into();

    while let Some(Constraint(t1, t2)) = queue.pop_front() {
        let s1 = subst.apply(&t1);
        let s2 = subst.apply(&t2);

        if s1 == s2 {
            continue;
        }

        match (s1.clone(), s2.clone()) {
            (TypeObject::Var(v), ref other) => {
                // occurs check
                if occurs_check(v.clone(), other) {
                    return Err(TypeError::InfiniteType(v, other.clone()));
                }
                let mut new_sub = Substitution::new();
                new_sub.0.insert(v.clone(), other.clone());
                subst = new_sub.compose(subst);
            }
            (ref other, TypeObject::Var(v)) => {
                if occurs_check(v.clone(), other) {
                    return Err(TypeError::InfiniteType(v.clone(), other.clone()));
                }
                let mut new_sub = Substitution::new();
                new_sub.0.insert(v.clone(), other.clone());
                subst = new_sub.compose(subst);
            }
            (TypeObject::Function(a1, b1), TypeObject::Function(a2, b2)) => {
                queue.push_back(Constraint(*a1, *a2));
                queue.push_back(Constraint(*b1, *b2));
            }
            (
                TypeObject::ADTInst {
                    name: n1,
                    params: p1,
                },
                TypeObject::ADTInst {
                    name: n2,
                    params: p2,
                },
            ) if n1 == n2 && p1.len() == p2.len() => {
                for (x, y) in p1.into_iter().zip(p2.into_iter()) {
                    queue.push_back(Constraint(x, y));
                }
            }
            (ref x, ref y) if x == y => continue,
            (TypeObject::Any, _) | (_, TypeObject::Any) => (),
            (t1, t2) => {
                return Err(TypeError::CannotUnify { t1, t2 });
            }
        }
    }

    Ok(subst)
}

pub struct Checker {
    env: Rc<RefCell<CheckerEnv>>,
    next_var: RefCell<u32>,
    constraints: RefCell<Vec<Constraint>>,
}

impl Checker {
    pub fn new(env: Rc<RefCell<CheckerEnv>>) -> Self {
        Checker {
            env,
            next_var: RefCell::new(0),
            constraints: RefCell::new(Vec::new()),
        }
    }

    fn fresh_var(&self) -> TypeObject {
        let id = *self.next_var.borrow();
        *self.next_var.borrow_mut() = id + 1;
        TypeObject::Var(format!("t{}", id))
    }

    fn add_constraint(&self, t1: TypeObject, t2: TypeObject) -> Result<(), TypeError> {
        let mut con = self.constraints.borrow_mut();
        con.push(Constraint(t1, t2));
        if let Err(err) = unify(con.clone()) {
            con.pop();
            return Err(err);
        }
        Ok(())
    }

    fn resolve_type_expr(&self, te: Option<&TypeExpr>) -> Result<TypeObject, TypeError> {
        match te {
            Some(te_inner) => self.resolve(te_inner, Rc::clone(&self.env)),
            None => Ok(self.fresh_var()),
        }
    }

    fn resolve(
        &self,
        type_expr: &TypeExpr,
        env: Rc<RefCell<CheckerEnv>>,
    ) -> Result<TypeObject, TypeError> {
        match type_expr {
            TypeExpr::Var(id) => Ok(TypeObject::Var(id.clone())),
            TypeExpr::Con(name) => match name.as_str() {
                "Int" => Ok(TypeObject::Int),
                "Float" => Ok(TypeObject::Float),
                "String" => Ok(TypeObject::String),
                "Char" => Ok(TypeObject::Char),
                "Bool" => Ok(TypeObject::Bool),
                "Unit" => Ok(TypeObject::Unit),
                "Any" => Ok(TypeObject::Any),
                _ => env
                    .borrow()
                    .get_bind(name)
                    .ok_or(TypeError::UndefinedVariable(name.clone())),
            },
            TypeExpr::App(constructor, args) => {
                let base = self.resolve(constructor, Rc::clone(&env))?;
                let mut arg_tys = Vec::with_capacity(args.len());
                for arg in args.iter() {
                    arg_tys.push(self.resolve(arg, Rc::clone(&env))?);
                }
                match base {
                    TypeObject::ADTDef { name, .. } => Ok(TypeObject::ADTInst {
                        name,
                        params: arg_tys,
                    }),
                    // TypeObject::Function(a, b) if arg_tys.len() == 1 => {
                    //     self.add_constraint(*a, arg_tys[0])?;
                    //     Ok(*b)
                    // }
                    other => Err(TypeError::InvalidOperation {
                        operation: "type application".into(),
                        typ: format!("{}", other),
                    }),
                }
            }
            TypeExpr::Arrow(left, right) => Ok(TypeObject::Function(
                Box::new(self.resolve(left, Rc::clone(&env))?),
                Box::new(self.resolve(right, Rc::clone(&env))?),
            )),
            TypeExpr::Forall(binders, body) => {
                let saved = env.borrow().clone();
                {
                    let mut e = env.borrow_mut();
                    for (var_name, _) in binders.iter() {
                        e.insert_bind(var_name.clone(), TypeObject::Kind(Kind::Star))?;
                    }
                }
                let ty_body = self.resolve(body, Rc::clone(&env))?;
                *env.borrow_mut() = saved;
                Ok(TypeObject::Forall(
                    binders
                        .iter()
                        .map(|(v, _)| (v.clone(), TypeObject::Kind(Kind::Star)))
                        .collect(),
                    Box::new(ty_body),
                ))
            }
            TypeExpr::Lambda((param, kind), body) => {
                let kind_obj = self.resolve(kind, Rc::clone(&env))?;
                let saved = env.borrow().clone();
                {
                    let mut e = env.borrow_mut();
                    e.insert_bind(param.clone(), kind_obj.clone())?;
                }
                let ty_body = self.resolve(body, Rc::clone(&env))?;
                *env.borrow_mut() = saved;
                Ok(TypeObject::Lambda(
                    (param.clone(), Box::new(kind_obj)),
                    Box::new(ty_body),
                ))
            }
            TypeExpr::Literal(lit) => self.infer_literal(lit),
            TypeExpr::Kind(kind) => Ok(TypeObject::Kind(kind.clone())),
        }
    }

    fn infer_literal(&self, literal: &Literal) -> Result<TypeObject, TypeError> {
        Ok(match literal {
            Literal::Int(_) => TypeObject::Int,
            Literal::Float(_) => TypeObject::Float,
            Literal::String(_) => TypeObject::String,
            Literal::Char(_) => TypeObject::Char,
            Literal::Bool(_) => TypeObject::Bool,
            Literal::Unit => TypeObject::Unit,
            _ => TypeObject::Any,
        })
    }

    pub fn check(&self, program: &Vec<Stmt>) -> Result<Vec<CheckedStmt>, TypeError> {
        let mut checked_list = Vec::with_capacity(program.len());
        for stmt in program.iter() {
            checked_list.push(self.check_stmt(stmt)?);
        }
        let subst = unify(self.constraints.borrow().clone())?;
        let mut final_list = Vec::with_capacity(checked_list.len());
        for cs in checked_list.into_iter() {
            final_list.push(self.apply_subst_stmt(cs, &subst));
        }
        Ok(final_list)
    }

    fn apply_subst_stmt(&self, stmt: CheckedStmt, subst: &Substitution) -> CheckedStmt {
        match stmt {
            CheckedStmt::Let {
                name,
                type_annotation,
                value,
            } => {
                let ta2 = subst.apply(&type_annotation);
                let v2 = self.apply_subst_expr(value, subst);
                CheckedStmt::Let {
                    name,
                    type_annotation: ta2,
                    value: v2,
                }
            }
            CheckedStmt::Expr(expr) => {
                let expr2 = self.apply_subst_expr(expr, subst);
                CheckedStmt::Expr(expr2)
            }
            CheckedStmt::ImportAll { source, alias } => CheckedStmt::ImportAll { source, alias },
            CheckedStmt::ImportSome { source, items } => CheckedStmt::ImportSome { source, items },
            CheckedStmt::Export {
                body,
                only_abstract,
            } => {
                let b2 = Box::new(self.apply_subst_stmt(*body, subst));
                CheckedStmt::Export {
                    body: b2,
                    only_abstract,
                }
            }
            _ => stmt,
        }
    }

    fn apply_subst_expr(&self, expr: CheckedExpr, subst: &Substitution) -> CheckedExpr {
        match expr {
            CheckedExpr::Ident {
                value,
                type_annotation,
            } => {
                let ta = subst.apply(&type_annotation);
                CheckedExpr::Ident {
                    value,
                    type_annotation: ta,
                }
            }
            CheckedExpr::Literal {
                value,
                type_annotation,
            } => {
                let ta = subst.apply(&type_annotation);
                CheckedExpr::Literal {
                    value,
                    type_annotation: ta,
                }
            }
            CheckedExpr::Prefix {
                op,
                expr: sub,
                type_annotation,
            } => {
                let sub2 = Box::new(self.apply_subst_expr(*sub, subst));
                let ta = subst.apply(&type_annotation);
                CheckedExpr::Prefix {
                    op,
                    expr: sub2,
                    type_annotation: ta,
                }
            }
            CheckedExpr::Infix {
                op,
                left,
                right,
                type_annotation,
            } => {
                let l2 = Box::new(self.apply_subst_expr(*left, subst));
                let r2 = Box::new(self.apply_subst_expr(*right, subst));
                let ta = subst.apply(&type_annotation);
                CheckedExpr::Infix {
                    op,
                    left: l2,
                    right: r2,
                    type_annotation: ta,
                }
            }
            CheckedExpr::Call {
                callee,
                params,
                type_annotation,
            } => {
                let c2 = Box::new(self.apply_subst_expr(*callee, subst));
                let ps2 = params
                    .into_iter()
                    .map(|p| self.apply_subst_expr(p, subst))
                    .collect();
                let ta = subst.apply(&type_annotation);
                CheckedExpr::Call {
                    callee: c2,
                    params: ps2,
                    type_annotation: ta,
                }
            }
            CheckedExpr::Function {
                params,
                body,
                type_annotation,
            } => {
                let ps = params.clone();
                let b2 = Box::new(self.apply_subst_expr(*body, subst));
                let ta = subst.apply(&type_annotation);
                CheckedExpr::Function {
                    params: ps,
                    body: b2,
                    type_annotation: ta,
                }
            }
            CheckedExpr::If {
                condition,
                then_branch,
                else_branch,
                type_annotation,
            } => {
                let c2 = Box::new(self.apply_subst_expr(*condition, subst));
                let t2 = Box::new(self.apply_subst_expr(*then_branch, subst));
                let e2 = Box::new(self.apply_subst_expr(*else_branch, subst));
                let ta = subst.apply(&type_annotation);
                CheckedExpr::If {
                    condition: c2,
                    then_branch: t2,
                    else_branch: e2,
                    type_annotation: ta,
                }
            }
            CheckedExpr::LetIn {
                name,
                value,
                body,
                type_annotation,
            } => {
                let v2 = Box::new(self.apply_subst_expr(*value, subst));
                let b2 = Box::new(self.apply_subst_expr(*body, subst));
                let ta = subst.apply(&type_annotation);
                CheckedExpr::LetIn {
                    name,
                    value: v2,
                    body: b2,
                    type_annotation: ta,
                }
            }
            CheckedExpr::Match {
                expr,
                cases,
                type_annotation,
            } => {
                let e2 = Box::new(self.apply_subst_expr(*expr, subst));
                let cs2: Vec<CheckedCase> = cases
                    .into_iter()
                    .map(|case| {
                        let guard2 = Box::new(self.apply_subst_expr(case.guard, subst));
                        let body2 = Box::new(self.apply_subst_expr(case.body, subst));
                        CheckedCase {
                            pattern: case.pattern,
                            guard: *guard2,
                            body: *body2,
                        }
                    })
                    .collect();
                let ta = subst.apply(&type_annotation);
                CheckedExpr::Match {
                    expr: e2,
                    cases: cs2,
                    type_annotation: ta,
                }
            }
            CheckedExpr::Block {
                stmts,
                type_annotation,
            } => {
                let ss2 = stmts
                    .into_iter()
                    .map(|s| self.apply_subst_stmt(s, subst))
                    .collect();
                let ta = subst.apply(&type_annotation);
                CheckedExpr::Block {
                    stmts: ss2,
                    type_annotation: ta,
                }
            }
            _ => expr,
        }
    }

    fn check_stmt(&self, stmt: &Stmt) -> Result<CheckedStmt, TypeError> {
        match stmt {
            Stmt::Let {
                name,
                type_annotation,
                value,
            } => {
                if self.env.borrow().has_bind(name) {
                    return Err(TypeError::RedefinedVariable(name.clone()));
                }

                let ann_ty = self.resolve_type_expr(type_annotation.as_ref())?;

                let checked_value = self.check_expr(value, &ann_ty)?;

                let generalized = self.generalize(ann_ty.clone());
                self.env
                    .borrow_mut()
                    .insert_bind(name.clone(), generalized)?;

                Ok(CheckedStmt::Let {
                    name: name.clone(),
                    type_annotation: ann_ty,
                    value: checked_value,
                })
            }
            Stmt::Type {
                name,
                params,
                kind_annotation: _,
                variants,
            } => {
                if self.env.borrow().has_bind(name) {
                    return Err(TypeError::RedefinedVariable(name.clone()));
                }
                // TODO: 解析 kind 注解
                // println!("{:?}", kind_annotation);
                // let kind_ann = match kind_annotation {
                //     Some(k) => self.resolve_type_expr(Some(k))?,
                //     None => TypeObject::Kind(Kind::Star),
                // };
                let kind = num_to_kind(params.len());
                let kind_ann = TypeObject::Kind(kind.clone());
                self.env.borrow_mut().insert_bind(
                    name.clone(),
                    /*                     TypeObject::Kind(match kind_ann.clone() {
                        TypeObject::Kind(k) => k,
                        _ => {
                            return Err(TypeError::KindMismatch {
                                expected: "Type".into(),
                                found: format!("{}", kind_ann),
                            })
                        }
                    }), */
                    TypeObject::ADTDef {
                        name: name.clone(),
                        params: params.clone(),
                        constructors: vec![],
                    },
                )?;

                let mut checked_variants = Vec::new();
                for variant in variants.iter() {
                    match &variant.fields {
                        TypeVariantFields::Unit => {
                            let cons_type = TypeObject::ADTInst {
                                name: name.clone(),
                                params: vec![],
                            };
                            self.env
                                .borrow_mut()
                                .insert_bind(variant.name.clone(), cons_type.clone())?;
                            checked_variants.push(CheckedTypeVariant {
                                name: variant.name.clone(),
                                fields: CheckedTypeVariantFields::Unit,
                            });
                        }
                        TypeVariantFields::Tuple(fields) => {
                            {
                                let mut e = self.env.borrow_mut();
                                for p in params.iter() {
                                    e.insert_bind(p.clone(), TypeObject::Any)?;
                                }
                            }
                            let mut field_tys = Vec::new();
                            for f in fields.iter() {
                                let ty_f = self.resolve(f, Rc::clone(&self.env))?;
                                field_tys.push(ty_f);
                            }
                            let retty = TypeObject::ADTInst {
                                name: name.clone(),
                                params: params
                                    .iter()
                                    .map(|_| TypeObject::Kind(Kind::Star))
                                    .collect(),
                            };
                            let func_ty =
                                vec_to_type([field_tys.clone(), vec![retty.clone()]].concat());
                            self.env
                                .borrow_mut()
                                .insert_bind(variant.name.clone(), func_ty.clone())?;
                            checked_variants.push(CheckedTypeVariant {
                                name: variant.name.clone(),
                                fields: CheckedTypeVariantFields::Tuple(field_tys),
                            });
                        }
                        _ => unimplemented!("record variant"),
                    }
                }

                Ok(CheckedStmt::Type {
                    name: name.clone(),
                    params: params.clone(),
                    variants: checked_variants,
                    kind_annotation: match kind_ann.clone() {
                        TypeObject::Kind(_) => vec_to_kind(
                            [
                                params.iter().map(|_| Kind::Star).collect::<Vec<_>>(),
                                vec![Kind::Star],
                            ]
                            .concat(),
                        ),
                        _ => {
                            return Err(TypeError::KindMismatch {
                                expected: "Type".into(),
                                found: format!("{}", kind_ann),
                            })
                        }
                    },
                })
            }
            Stmt::Expr(expression) => {
                let checked_expr = self.infer_expr(expression)?;
                Ok(CheckedStmt::Expr(checked_expr))
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
                let cb = Box::new(self.check_stmt(body)?);
                Ok(CheckedStmt::Export {
                    body: cb,
                    only_abstract: *only_abstract,
                })
            }
        }
    }

    pub fn check_expr(&self, expr: &Expr, expected: &TypeObject) -> Result<CheckedExpr, TypeError> {
        let inferred = self.infer_expr(expr)?;
        self.add_constraint(inferred.get_type(), expected.clone())?;
        unify(self.constraints.borrow().clone())?;

        Ok(inferred)
    }

    pub fn infer_expr(&self, expression: &Expr) -> Result<CheckedExpr, TypeError> {
        match expression {
            Expr::Ident(name) | Expr::Internal(name) => {
                let scheme = self
                    .env
                    .borrow()
                    .get_bind(name)
                    .ok_or(TypeError::UndefinedVariable(name.clone()))?;
                let inst = self.instantiate(scheme);
                Ok(CheckedExpr::Ident {
                    value: name.clone(),
                    type_annotation: inst,
                })
            }
            Expr::Literal(lit) => {
                let ty = self.infer_literal(lit)?;
                Ok(CheckedExpr::Literal {
                    value: lit.clone(),
                    type_annotation: ty,
                })
            }
            Expr::Prefix(op, sub_expr) => {
                let checked_sub = self.infer_expr(sub_expr)?;
                let sub_ty = checked_sub.get_type();
                let result_ty = self.infer_prefix(op, &sub_ty)?;
                Ok(CheckedExpr::Prefix {
                    op: op.clone(),
                    expr: Box::new(checked_sub),
                    type_annotation: result_ty,
                })
            }
            Expr::Infix(op, left_expr, right_expr) => {
                let left_c = self.infer_expr(left_expr)?;
                let right_c = self.infer_expr(right_expr)?;
                let lty = left_c.get_type();
                let rty = right_c.get_type();
                let result_ty = self.infer_infix(op, &lty, &rty)?;
                Ok(CheckedExpr::Infix {
                    op: op.clone(),
                    left: Box::new(left_c),
                    right: Box::new(right_c),
                    type_annotation: result_ty,
                })
            }
            Expr::Call { callee, params, .. } => {
                let callee_c = self.infer_expr(callee)?;
                let mut current_ty = callee_c.get_type();
                let mut checked_args = Vec::new();
                for arg in params.iter() {
                    let arg_c = self.infer_expr(arg)?;
                    match current_ty.clone() {
                        TypeObject::Function(param_ty, ret_ty) => {
                            self.add_constraint((*param_ty).clone(), arg_c.get_type())?;
                            current_ty = *ret_ty;
                            checked_args.push(arg_c);
                        }
                        TypeObject::ADTDef {
                            name,
                            params: type_params,
                            constructors,
                        } => {
                            if checked_args.len() >= type_params.len() {
                                return Err(TypeError::ArityMismatch {
                                    expected: type_params.len(),
                                    found: checked_args.len() + 1,
                                    context: format!("calling constructor `{}`", name),
                                });
                            }

                            checked_args.push(arg_c);

                            if checked_args.len() < type_params.len() {
                                current_ty = TypeObject::ADTDef {
                                    name: name.clone(),
                                    params: type_params[checked_args.len()..].to_vec(),
                                    constructors: constructors.clone(),
                                };
                            } else {
                                current_ty = TypeObject::ADTInst {
                                    name: name.clone(),
                                    params: checked_args.iter().map(|c| c.get_type()).collect(),
                                };
                            }
                        }
                        other => {
                            return Err(TypeError::NotCallable {
                                found: format!("{}", other),
                            });
                        }
                    }
                }
                Ok(CheckedExpr::Call {
                    callee: Box::new(callee_c),
                    params: checked_args,
                    type_annotation: current_ty,
                })
            }
            Expr::Function {
                params,
                body,
                return_type,
            } => {
                let mut param_tys = Vec::new();
                for (_name, ann_opt) in params.iter() {
                    let t = self.resolve_type_expr(ann_opt.as_ref())?;
                    param_tys.push(t);
                }
                let ret_ty = self.resolve_type_expr(return_type.as_ref())?;

                let extended_env = CheckerEnv::extend(Rc::clone(&self.env));
                {
                    let mut ev = extended_env.borrow_mut();
                    for (idx, (name, _ann)) in params.iter().enumerate() {
                        let pty = param_tys[idx].clone();
                        let scheme = self.generalize(pty.clone());
                        ev.insert_bind(name.clone(), scheme).unwrap();
                    }
                }
                let nested_checker = Checker::new(extended_env);
                let checked_body = nested_checker.check_expr(body, &ret_ty)?;
                let actual_ret = checked_body.get_type();
                self.add_constraint(ret_ty.clone(), actual_ret.clone())?;

                let func_ty = param_tys.into_iter().rev().fold(actual_ret, |acc, p| {
                    TypeObject::Function(Box::new(p), Box::new(acc))
                });
                Ok(CheckedExpr::Function {
                    params: params.clone().into_iter().map(|(n, _)| n.clone()).collect(),
                    body: Box::new(checked_body),
                    type_annotation: func_ty,
                })
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_c = self.check_expr(condition, &TypeObject::Bool)?;
                let then_c = self.infer_expr(then_branch)?;
                let else_c = self.check_expr(else_branch, &then_c.get_type())?;
                Ok(CheckedExpr::If {
                    condition: Box::new(cond_c),
                    then_branch: Box::new(then_c),
                    else_branch: Box::new(else_c.clone()),
                    type_annotation: else_c.get_type(),
                })
            }
            Expr::LetIn {
                name,
                type_annotation,
                value,
                body,
            } => {
                let ann_ty = self.resolve_type_expr(type_annotation.as_ref())?;
                let checked_val = self.check_expr(value, &ann_ty)?;
                let extended_env = CheckerEnv::extend(Rc::clone(&self.env));
                let gen = self.generalize(ann_ty.clone());
                extended_env.borrow_mut().insert_bind(name.clone(), gen)?;
                let nested_checker = Checker::new(extended_env);
                let checked_body = nested_checker.infer_expr(body)?;

                Ok(CheckedExpr::LetIn {
                    name: name.clone(),
                    value: Box::new(checked_val),
                    body: Box::new(checked_body.clone()),
                    type_annotation: checked_body.get_type(),
                })
            }
            Expr::Match { expr, cases } => {
                let matched_c = self.infer_expr(expr)?;
                let result_ty = self.fresh_var(); // 先假设一个 result 变量
                let mut checked_cases = Vec::new();
                for case in cases.iter() {
                    let pat_ty = self.infer_pattern(&case.pattern)?;
                    self.add_constraint(matched_c.get_type(), pat_ty)?;
                    let guard_c = self.check_expr(&case.guard, &TypeObject::Bool)?;
                    let body_c = self.infer_expr(&case.body)?;
                    self.add_constraint(result_ty.clone(), body_c.get_type())?;
                    checked_cases.push(CheckedCase {
                        pattern: case.pattern.clone(),
                        guard: guard_c,
                        body: body_c,
                    });
                }
                Ok(CheckedExpr::Match {
                    expr: Box::new(matched_c),
                    cases: checked_cases,
                    type_annotation: result_ty,
                })
            }
            Expr::Block(stmts) => {
                let mut last_ty = TypeObject::Unit;
                let mut checked_statements = Vec::new();
                for s in stmts.iter() {
                    let cs = self.check_stmt(s)?;
                    if let CheckedStmt::Expr(e) = &cs {
                        last_ty = e.get_type();
                    }
                    checked_statements.push(cs);
                }
                Ok(CheckedExpr::Block {
                    stmts: checked_statements,
                    type_annotation: last_ty,
                })
            }
        }
    }

    fn infer_pattern(&self, pattern: &Pattern) -> Result<TypeObject, TypeError> {
        match pattern {
            Pattern::Ident(name) => Ok(self.env.borrow().get_bind(name).unwrap_or(TypeObject::Any)),
            Pattern::Literal(lit) => self.infer_literal(lit),
            Pattern::ADTConstructor { name, args } => {
                let ctor_ty = self
                    .env
                    .borrow()
                    .get_bind(name)
                    .ok_or(TypeError::UndefinedVariable(name.clone()))?;
                let inst = self.instantiate(ctor_ty);
                let mut current_ty = inst.clone();
                for arg in args.iter() {
                    if let TypeObject::Function(p_ty, next_ty) = current_ty.clone() {
                        let arg_ty = self.infer_pattern(arg)?;
                        self.add_constraint(*p_ty.clone(), arg_ty)?;
                        current_ty = *next_ty.clone();
                    } else {
                        return Err(TypeError::ArityMismatch {
                            expected: 0,
                            found: args.len(),
                            context: format!("Pattern constructor {}", name),
                        });
                    }
                }
                Ok(current_ty)
            }
        }
    }

    fn instantiate(&self, scheme: TypeObject) -> TypeObject {
        if let TypeObject::Forall(binders, body) = scheme {
            let mut subst = Substitution::new();
            for _ in binders.iter() {
                let new_var = self.fresh_var();
                if let TypeObject::Var(id) = &new_var {
                    subst.0.insert(id.clone(), new_var.clone());
                }
            }
            subst.apply(&body)
        } else {
            scheme
        }
    }

    fn generalize(&self, ty: TypeObject) -> TypeObject {
        let mut tvs = Vec::new();
        fn collect_vars(ty: &TypeObject, acc: &mut Vec<String>) {
            match ty {
                TypeObject::Var(id) => {
                    if !acc.contains(id) {
                        acc.push(id.clone());
                    }
                }
                TypeObject::Function(a, b) => {
                    collect_vars(a, acc);
                    collect_vars(b, acc);
                }
                TypeObject::Forall(_, body) => {
                    collect_vars(body, acc);
                }
                TypeObject::Lambda((_, _), body) => {
                    collect_vars(body, acc);
                }
                TypeObject::ADTInst { params, .. } => {
                    for t in params.iter() {
                        collect_vars(t, acc);
                    }
                }
                _ => {}
            }
        }
        collect_vars(&ty, &mut tvs);
        let vars: Vec<String> = self
            .env
            .borrow()
            .binds
            .iter()
            .filter_map(|(v, t)| {
                if let TypeObject::Var(_) = t {
                    Some(v.clone())
                } else {
                    None
                }
            })
            .collect();
        let quantifiable: Vec<String> = tvs.into_iter().filter(|v| !vars.contains(v)).collect();

        if quantifiable.is_empty() {
            return ty;
        }

        let binders = quantifiable
            .iter()
            .map(|id| (id.clone(), TypeObject::Kind(Kind::Star)))
            .collect::<Vec<_>>();
        TypeObject::Forall(binders, Box::new(ty))
    }

    fn infer_infix(
        &self,
        op: &Token,
        l: &TypeObject,
        r: &TypeObject,
    ) -> Result<TypeObject, TypeError> {
        match op {
            Token::Plus | Token::Sub | Token::Mul | Token::Div => {
                self.add_constraint(l.clone(), r.clone())?;
                let result = self.fresh_var();
                self.add_constraint(l.clone(), result.clone())?;
                Ok(result)
            }
            Token::Equal | Token::NotEqual => {
                self.add_constraint(l.clone(), r.clone())?;
                Ok(TypeObject::Bool)
            }
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                self.add_constraint(l.clone(), r.clone())?;
                let result = TypeObject::Bool;
                Ok(result)
            }
            Token::And | Token::Or => {
                self.add_constraint(l.clone(), TypeObject::Bool)?;
                self.add_constraint(r.clone(), TypeObject::Bool)?;
                Ok(TypeObject::Bool)
            }
            _ => Err(TypeError::InvalidOperation {
                operation: format!("infix {:?}", op),
                typ: format!("{:?}, {:?}", l, r),
            }),
        }
    }

    fn infer_prefix(&self, op: &Token, sub: &TypeObject) -> Result<TypeObject, TypeError> {
        match op {
            Token::Sub => {
                let result = self.fresh_var();
                self.add_constraint(sub.clone(), result.clone())?;
                Ok(result)
            }
            Token::Not => {
                self.add_constraint(sub.clone(), TypeObject::Bool)?;
                Ok(TypeObject::Bool)
            }
            _ => Err(TypeError::InvalidOperation {
                operation: format!("prefix {:?}", op),
                typ: format!("{}", sub),
            }),
        }
    }
}
