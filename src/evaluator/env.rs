use crate::ast::{Expr, TypeExpr};

use super::{
    object::{Object, TypeObject, PRIMIVE_TYPES},
    EvalError,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Env<'a, T: Clone> {
    pub parent: Option<&'a Env<'a, T>>,
    pub binds: HashMap<String, T>,
}

impl<T: Clone> PartialEq for Env<'_, T> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl<'a, T: Clone> Env<'a, T> {
    pub fn new() -> Self {
        Env {
            parent: None,
            binds: HashMap::new(),
        }
    }

    pub fn extend(parent: &'a Env<'a, T>) -> Env<'a, T> {
        Env {
            parent: Some(parent),
            binds: HashMap::new(),
        }
    }

    pub fn insert_bind(&mut self, name: String, value: T) -> Result<(), EvalError> {
        if self.binds.contains_key(&name) {
            Err(EvalError::RedefinedIdentifier(format!(
                "Identifier '{}' is already defined",
                name
            )))
        } else {
            self.binds.insert(name, value);
            Ok(())
        }
    }

    pub fn get_bind(&self, name: &str) -> Option<T> {
        if let Some(bind) = self.get_own_bind(name) {
            Some(bind)
        } else if let Some(ref parent) = self.parent {
            parent.get_bind(name)
        } else {
            None
        }
    }

    pub fn get_own_bind(&self, name: &str) -> Option<T> {
        if let Some(bind) = self.binds.get(name) {
            Some(bind.clone())
        } else {
            None
        }
    }
}

pub fn new_typechecker_env<'a>() -> TypecheckerEnv<'a> {
    let mut env = Env::<'_, TypeObject>::new();
    PRIMIVE_TYPES.iter().for_each(|t| {
        env.insert_bind(t.to_string(), t.clone()).unwrap();
    });
    env
}

pub fn new_evaluator_env<'a>() -> EvaluatorEnv<'a> {
    let mut env = Env::<'_, Object>::new();
    PRIMIVE_TYPES.iter().for_each(|t| {
        env.insert_bind(t.to_string(), Object::Type(t.clone()))
            .unwrap();
    });
    env.insert_bind(
        "print".to_string(),
        Object::Function {
            type_params: vec![],
            params: vec![(
                "...args".to_string(),
                Box::new(TypeExpr::Con("*".to_string())),
            )],
            body: Box::new(Expr::Ident("print".to_string())),
            return_type: Box::new(TypeExpr::default()),
        },
    )
    .unwrap();
    env.insert_bind(
        "get_timestrap".to_string(),
        Object::Function {
            type_params: vec![],
            params: vec![],
            body: Box::new(Expr::Ident("get_timestrap".to_string())),
            return_type: Box::new(TypeObject::Float.into()),
        },
    )
    .unwrap();
    env
}

pub type TypecheckerEnv<'a> = Env<'a, TypeObject>;
pub type EvaluatorEnv<'a> = Env<'a, Object>;
