use crate::checker::object::{TypeObject, PRIMIVE_TYPES};
use crate::evaluator::object::Object;
use crate::parser::ast::Expr;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env<T: Clone> {
    pub parent: Option<Rc<RefCell<Env<T>>>>,
    pub binds: HashMap<String, T>,
}

#[derive(Debug, Clone)]
pub enum EnvError {
    RedefinedBinding(String),
}

impl<T: Clone> PartialEq for Env<T> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl<T: Clone> From<HashMap<String, T>> for Env<T> {
    fn from(binds: HashMap<String, T>) -> Self {
        Env {
            parent: None,
            binds,
        }
    }
}

impl<T: Clone> Env<T> {
    pub fn new() -> Self {
        Env {
            parent: None,
            binds: HashMap::new(),
        }
    }

    pub fn extend(parent: Rc<RefCell<Env<T>>>) -> Rc<RefCell<Env<T>>> {
        Rc::new(RefCell::new(Env {
            parent: Some(parent),
            binds: HashMap::new(),
        }))
    }

    pub fn insert_bind(&mut self, name: String, value: T) -> Result<(), EnvError> {
        if self.binds.contains_key(&name) {
            Err(EnvError::RedefinedBinding(name))
        } else {
            self.binds.insert(name, value);
            Ok(())
        }
    }

    pub fn insert_bind_force(&mut self, name: String, value: T) {
        self.binds.insert(name, value);
    }

    pub fn get_bind(&self, name: &str) -> Option<T> {
        if let Some(bind) = self.get_own_bind(name) {
            Some(bind)
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get_bind(name)
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

pub fn new_checker_env() -> Rc<RefCell<CheckerEnv>> {
    let env_origin = Rc::new(RefCell::new(Env::new()));
    let env = Rc::clone(&env_origin);
    let mut env = env.borrow_mut();
    for t in PRIMIVE_TYPES.iter() {
        env.insert_bind(t.to_string(), t.clone()).unwrap();
    }
    env.insert_bind(
        "print".to_string(),
        TypeObject::Function(
            Box::new(TypeObject::Any.into()),
            Box::new(TypeObject::Unit.into()),
        ),
    )
    .unwrap();
    env.insert_bind(
        "get_timestrap".to_string(),
        TypeObject::Function(
            Box::new(TypeObject::Unit.into()),
            Box::new(TypeObject::Float.into()),
        ),
    )
    .unwrap();
    env_origin
}

pub fn new_evaluator_env() -> Rc<RefCell<EvaluatorEnv>> {
    let env_origin = Rc::new(RefCell::new(Env::new()));
    let env = Rc::clone(&env_origin);
    let mut env = env.borrow_mut();
    for t in PRIMIVE_TYPES.iter() {
        env.insert_bind(t.to_string(), Object::Type(t.clone()))
            .unwrap();
    }
    env.insert_bind(
        "print".to_string(),
        Object::Function {
            type_params: vec![],
            params: vec![("...args".to_string(), Box::new(TypeObject::Any.into()))],
            body: Box::new(Expr::Internal("print".to_string())),
            return_type: Box::new(TypeObject::Unit.into()),
            env: Rc::clone(&env_origin),
        },
    )
    .unwrap();
    env.insert_bind(
        "get_timestrap".to_string(),
        Object::Function {
            type_params: vec![],
            params: vec![],
            body: Box::new(Expr::Internal("get_timestrap".to_string())),
            return_type: Box::new(TypeObject::Float.into()),
            env: Rc::clone(&env_origin),
        },
    )
    .unwrap();
    env_origin
}

pub type CheckerEnv = Env<TypeObject>;
pub type EvaluatorEnv = Env<Object>;
