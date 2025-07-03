use crate::checker::ast::{CheckedExpr, CheckedStmt, CheckedTypeVariantFields, Program};
use crate::checker::object::TypeObject;
use crate::env::{Env, EvaluatorEnv};
use crate::lexer::token::Token;
use crate::parser::ast::{Kind, Literal, Pattern, UnsafeProgram};
use crate::utils::{is_uppercase_first_letter, to_checked_expr, to_checked_stmt};
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
            "get_timestamp".to_string(),
            Box::new(|_| Ok((UNIX_EPOCH.elapsed().unwrap().as_millis() as i64).into())),
        );
        let env2 = Rc::clone(&env);
        custom_funcs.insert(
            "get_bind".to_string(),
            Box::new(move |args| {
                let name =
                    if let Object::String(str) = args.first().ok_or(EvalError::ArityMismatch)? {
                        str.clone()
                    } else {
                        return Err(EvalError::ArityMismatch);
                    };
                env2.borrow()
                    .get_bind(&name)
                    .ok_or(EvalError::UndefinedVariable(name))
            }),
        );
        custom_funcs.insert(
            "concat".to_string(),
            Box::new(|args| {
                Ok(args
                    .iter()
                    .map(|arg| arg.pretty_print())
                    .collect::<String>()
                    .into())
            }),
        );
        Evaluator { env, custom_funcs }
    }

    pub fn set_custom_func(&mut self, name: String, func: CustomFunc) {
        self.custom_funcs.insert(name, func);
    }

    fn eval_internal_func(
        &self,
        identify: String,
        params: Vec<Object>,
        type_annotation: TypeObject,
    ) -> Result<Object, EvalError> {
        if identify.starts_with("ADTVALUECONSTRUCTOR_") {
            Ok(Object::ADTValue {
                variant: identify
                    .split('_')
                    .collect::<Vec<_>>()
                    .last()
                    .ok_or(EvalError::UndefinedVariable(identify.to_string()))?
                    .to_string(),
                fields: params,
                type_annotation,
            })
        } else {
            if let Some(func) = self.custom_funcs.get(identify.as_str()) {
                func(params)
            } else {
                Err(EvalError::UndefinedVariable(identify.to_string()))
            }
        }
    }

    pub fn eval(&mut self, program: &Program) -> Result<Object, EvalError> {
        Ok(program
            .iter()
            .map(|stmt| self.eval_stmt(stmt))
            .collect::<Result<Vec<_>, _>>()?
            .last()
            .unwrap_or(&Object::Unit)
            .clone())
    }

    pub fn eval_unsafe(&mut self, unsafe_program: &UnsafeProgram) -> Result<Object, EvalError> {
        Ok(unsafe_program
            .iter()
            .map(|stmt| self.eval_stmt(&to_checked_stmt(stmt.clone())))
            .collect::<Result<Vec<_>, _>>()?
            .last()
            .unwrap_or(&Object::Unit)
            .clone())
    }

    fn eval_func(&self, func: &CheckedExpr, args: Vec<Object>) -> Result<Object, EvalError> {
        match self.eval_expr(func)? {
            Object::Function {
                params,
                body,
                env,
                type_annotation,
            } => {
                let check_params = || {
                    if params.len() != args.len() {
                        Err(EvalError::ArityMismatch)
                    } else {
                        Ok(())
                    }
                };
                if let CheckedExpr::Internal { value, .. } = body {
                    self.eval_internal_func(value, args, type_annotation)
                } else {
                    check_params()?;
                    let local_env = Env::extend(env);
                    for (name, type_annotation) in params.iter().zip(args.into_iter()) {
                        local_env
                            .borrow_mut()
                            .insert_bind(name.clone(), type_annotation)?;
                    }
                    Evaluator::new(local_env).eval_expr(&body.into())
                }
            }
            Object::Kind {
                value: TypeObject::ADTDef { name, params, .. },
                ..
            } => {
                if params.len() == args.len() {
                    let mut real_params = vec![];
                    for arg in args {
                        if let Object::Kind { value, .. } = arg {
                            real_params.push(value)
                        } else {
                            return Err(EvalError::TypeMismatch);
                        }
                    }
                    Ok(Object::Kind {
                        value: TypeObject::ADTInst {
                            name: name.clone(),
                            params: real_params,
                        },
                        kind_annotation: Kind::Star,
                    })
                } else {
                    Err(EvalError::ArityMismatch)
                }
            }
            _ => Err(EvalError::NotCallable),
        }
    }

    fn eval_stmt(&mut self, stmt: &CheckedStmt) -> Result<Object, EvalError> {
        match stmt {
            CheckedStmt::Let { name, value, .. } => {
                if self.env.borrow().get_bind(name).is_some() {
                    return Err(EvalError::RedefinedVariable(name.clone()));
                }
                let result = self.eval_expr(value)?;
                self.env.borrow_mut().insert_bind(name.clone(), result)?;
                Ok(Object::Unit)
            }
            CheckedStmt::Type {
                name,
                variants,
                params,
                kind_annotation,
            } => {
                let mut env = self.env.borrow_mut();

                /* Type Constructor */
                env.insert_bind(
                    name.clone(),
                    Object::Kind {
                        value: TypeObject::ADTDef {
                            name: name.clone(),
                            params: params.clone(),
                            constructors: vec![],
                        },
                        kind_annotation: kind_annotation.clone(),
                    },
                )?;

                // TODO: relate value constructors with params of type constructor, instead of `Unknown`
                let adt_type_annotation = {
                    let real_params = params.iter().map(|_| TypeObject::Any).collect::<Vec<_>>();
                    match env
                        .get_bind(name)
                        .ok_or(EvalError::UndefinedVariable(name.to_string()))?
                    {
                        Object::Kind { value: type_, .. } => match type_ {
                            TypeObject::ADTDef { params, name, .. } => {
                                if params.len() == real_params.len() {
                                    Ok(TypeObject::ADTInst {
                                        name: name.clone(),
                                        params: real_params.clone(),
                                    })
                                } else {
                                    Err(EvalError::ArityMismatch)
                                }
                            }
                            _ => Err(EvalError::TypeMismatch),
                        },
                        _ => Err(EvalError::NotAType(name.to_string())),
                    }?
                };

                for variant in variants {
                    let variant_name = variant.name.clone();
                    let constructor = match variant.fields.clone() {
                        CheckedTypeVariantFields::Unit => Object::ADTValue {
                            variant: variant_name.clone(),
                            fields: vec![],
                            type_annotation: adt_type_annotation.clone(),
                        },
                        CheckedTypeVariantFields::Tuple(params) => Object::Function {
                            params: params.iter().map(|_| "".to_string()).collect::<Vec<_>>(),
                            body: CheckedExpr::Internal {
                                value: format!("ADTVALUECONSTRUCTOR_{}_{}", name, variant_name),
                                type_annotation: adt_type_annotation.clone(),
                            },
                            env: Rc::clone(&self.env),
                            type_annotation: adt_type_annotation.clone().as_return_type(params),
                        },
                        CheckedTypeVariantFields::Record(_) => {
                            unimplemented!("Record type variant")
                        }
                    };

                    env.insert_bind(variant_name, constructor)?;
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
                    Err(EvalError::UndefinedVariable(value.clone()))
                }
            }
            CheckedExpr::Literal { value, .. } => match value {
                Literal::Int(i) => Ok((*i).into()),
                Literal::Float(f) => Ok((*f).into()),
                Literal::String(s) => Ok(s.clone().into()),
                Literal::Char(c) => Ok((*c).into()),
                Literal::Bool(b) => Ok((*b).into()),
                Literal::Array(arr) => Ok(arr
                    .iter()
                    .map(|e| self.eval_expr(&to_checked_expr(e.clone())))
                    .collect::<Result<Vec<_>, _>>()?
                    .into()),
                Literal::Unit => Ok(Object::Unit),
            },
            CheckedExpr::Prefix { op, expr, .. } => {
                let val = self.eval_expr(expr)?;
                match op {
                    Token::Sub => match val {
                        Object::Int(i) => Ok((-i).into()),
                        Object::Float(f) => Ok((-f).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Not => match val {
                        Object::Bool(b) => Ok((!b).into()),
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
                    Token::And => match (left_val, right_val) {
                        (Object::Bool(l), Object::Bool(r)) => Ok((l && r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Or => match (left_val, right_val) {
                        (Object::Bool(l), Object::Bool(r)) => Ok((l || r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Plus => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok((l + r).into()),
                        (Object::Float(l), Object::Float(r)) => Ok((l + r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Sub => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok((l - r).into()),
                        (Object::Float(l), Object::Float(r)) => Ok((l - r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Mul => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok((l * r).into()),
                        (Object::Float(l), Object::Float(r)) => Ok((l * r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Pow => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok((l.pow(r as u32)).into()),
                        (Object::Float(l), Object::Float(r)) => Ok((l.powf(r)).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Div => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) if r != 0 => Ok((l / r).into()),
                        (Object::Float(l), Object::Float(r)) if r != 0.0 => {
                            Ok(Object::Float(l / r))
                        }
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Mod => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) if r != 0 => Ok((l % r).into()),
                        (Object::Float(l), Object::Float(r)) if r != 0.0 => {
                            Ok(Object::Float(l % r))
                        }
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Equal => Ok((left_val == right_val).into()),
                    Token::NotEqual => Ok((left_val != right_val).into()),
                    Token::Greater => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok((l > r).into()),
                        (Object::Float(l), Object::Float(r)) => Ok((l > r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::GreaterEqual => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok((l >= r).into()),
                        (Object::Float(l), Object::Float(r)) => Ok((l >= r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::Less => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok((l < r).into()),
                        (Object::Float(l), Object::Float(r)) => Ok((l < r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::LessEqual => match (left_val, right_val) {
                        (Object::Int(l), Object::Int(r)) => Ok((l <= r).into()),
                        (Object::Float(l), Object::Float(r)) => Ok((l <= r).into()),
                        _ => Err(EvalError::TypeMismatch),
                    },
                    Token::ThinArrow => {
                        if let (
                            Object::Kind { value: param, .. },
                            Object::Kind { value: ret, .. },
                        ) = (left_val, right_val)
                        {
                            Ok(Object::Kind {
                                value: TypeObject::Function(Box::new(param), Box::new(ret)),
                                kind_annotation: Kind::Star,
                            })
                        } else {
                            Err(EvalError::TypeMismatch)
                        }
                    }
                    Token::InfixIdent(value) => self.eval_func(
                        &CheckedExpr::Ident {
                            value: value.clone(),
                            type_annotation: TypeObject::Any, // TODO: infer type of custom operator
                        },
                        vec![left_val, right_val],
                    ),
                    token => self.eval_func(
                        &CheckedExpr::Ident {
                            value: token.to_string(),
                            type_annotation: TypeObject::Any,
                        },
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
                env: Rc::clone(&self.env),
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
                        if case_evaluator.eval_expr(&case.guard)?.eq(&(true).into()) {
                            return case_evaluator.eval_expr(&case.body);
                        }
                    }
                }
                println!("match failure: {}", match_value);
                println!("match cases: {:#?}", cases);
                Err(EvalError::PatternMatchFailure)
            }
            CheckedExpr::LetIn {
                name, value, body, ..
            } => {
                let val = self.eval_expr(value)?;
                let create_env = Env::extend(Rc::clone(&self.env));
                create_env.borrow_mut().insert_bind(
                    name.clone(),
                    if let Object::Function {
                        params,
                        body,
                        type_annotation,
                        ..
                    } = val
                    {
                        Object::Function {
                            params: params.clone(),
                            body: body.clone(),
                            type_annotation: type_annotation.clone(),
                            env: Rc::clone(&create_env),
                        }
                    } else {
                        val
                    },
                )?;
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
                if let Object::ADTValue {
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

#[cfg(test)]
mod tests {
    use crate::env::new_evaluator_env;
    use crate::evaluator::{Evaluator, Object};
    use crate::lexer::Lexer;
    use crate::parser::ast::UnsafeProgram;
    use crate::parser::Parser;

    fn u(code: &str) -> Object {
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
            return err.into();
        };

        let program = Parser::new(token_data, true)
            .collect::<Vec<_>>()
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
            return err.into();
        }

        match Evaluator::new(new_evaluator_env())
            .eval_unsafe(&program)
            .map_err(|err| format!("Evaluator error: {}", err))
        {
            Ok(obj) => obj,
            Err(e) => e.into(),
        }
    }

    #[test]
    fn basic_literals_and_arithmetic() {
        assert_eq!(u("1"), (1).into());
        assert_eq!(u("1 + 2 * 3"), (7).into());
        assert_eq!(u("2.5 * 4.0"), (10.0).into());
        assert_eq!(u("true && !false"), (true).into());
        assert_eq!(u("\"foo\""), ("foo").into());
        assert_eq!(u("'c'"), ('c').into());
        assert_eq!(u("[1,2,3]"), vec![1, 2, 3].into());
    }

    #[test]
    fn maybe_and_list_adt_usage() {
        let code = r#"
            type Maybe = <A> Just(A) | Nothing
            type List  = <A> Cons(A, List(A)) | Nil

            let xs = Nil
            let ys = Cons(1, Cons(2, xs))
            ys
        "#;
        let result = u(code);
        // ys = Cons(1, Cons(2, Nil))
        match result {
            Object::ADTValue {
                variant,
                fields,
                type_annotation: _,
            } => {
                assert_eq!(variant, "Cons");
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0], (1).into());
                // second field itself is ADTValue Cons(2, Nil)
                match &fields[1] {
                    Object::ADTValue {
                        variant: v2,
                        fields: f2,
                        ..
                    } => {
                        assert_eq!(v2, "Cons");
                        assert_eq!(f2[0], (2).into());
                    }
                    _ => panic!("expected nested Cons"),
                }
                // verify type_annotation = List(Int)
                // ! assert_eq!(
                //     type_annotation,
                //     TypeObject::ADTInst {
                //         name: "List".into(),
                //         params: vec![TypeObject::Int],
                //     }
                // );
            }
            _ => panic!("expected ADTValue"),
        }
    }

    #[test]
    fn recursive_functions_and_nat() {
        let code = r#"
            type Nat = Z | S(Nat)
            let to_number = (x) =>
              match x then
                | S(n) => 1 + to_number(n)
                | Z    => 0

            to_number(S(S(S(Z))))
        "#;
        assert_eq!(u(code), (3).into());

        // test add
        let code2 = r#"
            type Nat = Z | S(Nat)
            let add = (x, y) =>
              match x then
                | S(n) => S(add(n, y))
                | Z    => y

            add(S(S(Z)), S(S(S(Z))))
        "#;
        // S(S(Z)) + SSS(Z) = five
        match u(code2) {
            Object::ADTValue { variant, .. } => {
                assert_eq!(variant, "S");
                // to_number should give 5
            }
            _ => panic!(),
        }
    }

    #[test]
    fn fibonacci_and_timing() {
        let code = r#"
            let fib = (n: Int): Int =>
              if n < 2 then 1 else fib(n - 1) + fib(n - 2)

            fib(5)
        "#;
        assert_eq!(u(code), (8).into());
    }

    #[test]
    fn custom_infix_and_composition() {
        let code = r#"
            let #-# = (a, b) => a - b
            10 `-` 3
        "#;
        assert_eq!(u(code), (7).into());

        let code2 = r#"
            let #$# = (f, x) => f(x)
            let dbl = (y) => y * 2
            dbl $ 5
        "#;
        assert_eq!(u(code2), (10).into());

        let code3 = r#"
            let #.# = (f, g) => (x) => f(g(x))
            let inc = (x) => x + 1
            let dbl = (x) => x * 2
            let h = inc . dbl
            h(3)
        "#;
        assert_eq!(u(code3), (7).into());
    }

    #[test]
    fn let_in_expression() {
        let code = r#"
            let foo =
              let x = 10 in
              let y = 20 in
              x + y
            foo
        "#;
        assert_eq!(u(code), (30).into());
    }
}
