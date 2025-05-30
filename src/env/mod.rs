use crate::checker::ast::CheckedExpr;
use crate::checker::object::{TypeObject, PRIMIVE_TYPES};
use crate::evaluator::object::Object;
use crate::parser::ast::Kind;
use error::EnvError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

pub mod error;

#[derive(Debug, Clone)]
pub struct Env<T: Clone> {
    pub parent: Option<Weak<RefCell<Env<T>>>>,
    pub binds: HashMap<String, T>,
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
            parent: Some(Rc::downgrade(&parent)),
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
            if let Some(parent) = parent.upgrade() {
                parent.borrow().get_bind(name)
            } else {
                None
            }
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

    pub fn has_bind(&self, name: &str) -> bool {
        if self.get_own_bind(name).is_some() {
            true
        } else if let Some(ref parent) = self.parent {
            if let Some(parent) = parent.upgrade() {
                parent.borrow().has_bind(name)
            } else {
                false
            }
        } else {
            false
        }
    }
}

pub fn new_checker_env() -> Rc<RefCell<CheckerEnv>> {
    let env_origin = Rc::new(RefCell::new(Env::new()));
    let env = Rc::clone(&env_origin);
    let mut env = env.borrow_mut();
    for t in PRIMIVE_TYPES.iter() {
        env.insert_bind(t.to_string(), TypeObject::Kind(Kind::Star))
            .unwrap(); /* <name, type> */
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
        let name = t.to_string();
        env.insert_bind(
            name,
            Object::Kind {
                value: t.clone(),
                kind_annotation: Kind::Star,
            },
        )
        .unwrap(); /* <name, value: type> */
    }
    env.insert_bind(
        "print".to_string(),
        Object::Function {
            params: vec!["...args".to_string()],
            body: CheckedExpr::Internal {
                value: "print".to_string(),
                type_annotation: TypeObject::Unit,
            },
            type_annotation: TypeObject::Function(
                Box::new(TypeObject::Any),
                Box::new(TypeObject::Unit),
            ),
            env: Rc::clone(&env_origin),
        },
    )
    .unwrap();
    env.insert_bind(
        "get_timestrap".to_string(),
        Object::Function {
            params: vec![],
            body: CheckedExpr::Internal {
                value: "get_timestrap".to_string(),
                type_annotation: TypeObject::Float,
            },
            type_annotation: TypeObject::Function(
                Box::new(TypeObject::Unit),
                Box::new(TypeObject::Float),
            ),
            env: Rc::clone(&env_origin),
        },
    )
    .unwrap();
    env.insert_bind(
        "get_bind".to_string(),
        Object::Function {
            params: vec!["x".to_string()],
            body: CheckedExpr::Internal {
                value: "get_bind".to_string(),
                type_annotation: TypeObject::String,
            },
            type_annotation: TypeObject::Function(
                Box::new(TypeObject::String),
                Box::new(TypeObject::String),
            ),
            env: Rc::clone(&env_origin),
        },
    )
    .unwrap();
    env.insert_bind(
        "connect".to_string(),
        Object::Function {
            params: vec!["...args".to_string()],
            body: CheckedExpr::Internal {
                value: "connect".to_string(),
                type_annotation: TypeObject::String,
            },
            type_annotation: TypeObject::Function(
                Box::new(TypeObject::Any),
                Box::new(TypeObject::String),
            ),
            env: Rc::clone(&env_origin),
        },
    )
    .unwrap();
    env_origin
}

pub type CheckerEnv = Env<TypeObject>;
pub type EvaluatorEnv = Env<Object>;
