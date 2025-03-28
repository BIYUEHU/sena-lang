use std::collections::HashMap;

use super::object::{Object, TypeObject};

#[derive(Clone, Debug)]
pub struct Env {
    values: HashMap<String, Object>,
    types: HashMap<String, TypeObject>,
}

impl PartialEq for Env {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Env {
    pub fn new() -> Self {
        let mut env = Env {
            values: HashMap::new(),
            types: HashMap::new(),
        };
        env.init();
        env
    }

    fn init(&mut self) {
        self.insert_type("Int".to_string(), TypeObject::IntType)
            .unwrap();
        self.insert_type("Float".to_string(), TypeObject::FloatType)
            .unwrap();
        self.insert_type("String".to_string(), TypeObject::StringType)
            .unwrap();
        self.insert_type("Char".to_string(), TypeObject::CharType)
            .unwrap();
        self.insert_type("Bool".to_string(), TypeObject::BoolType)
            .unwrap();
        self.insert_value(
            "print".to_string(),
            Object::Function {
                params: vec!["x".to_string()],
                body: Box::new(crate::ast::Expr::Ident("print_builtin".to_string())),
                // env: self.clone(),
            },
        )
        .unwrap();
        self.insert_value(
            "get_timestrap".to_string(),
            Object::Function {
                params: vec![],
                body: Box::new(crate::ast::Expr::Ident("get_timestrap_builtin".to_string())),
                // env: self.clone(),
            },
        )
        .unwrap();
    }

    pub fn insert_value(&mut self, name: String, value: Object) -> Result<(), String> {
        // TODO: check if name is valid
        // if self.values.contains_key(&name) || self.types.contains_key(&name) {
        //     Err(format!("Identifier '{}' is already defined", name))
        // } else {
        self.values.insert(name, value);
        Ok(())
        // }
    }

    pub fn insert_type(&mut self, name: String, type_obj: TypeObject) -> Result<(), String> {
        if self.types.contains_key(&name) || self.values.contains_key(&name) {
            Err(format!("Identifier '{}' is already defined", name))
        } else {
            self.types.insert(name, type_obj);
            Ok(())
        }
    }

    pub fn get_value(&self, name: &str) -> Option<&Object> {
        self.values.get(name)
    }

    pub fn get_type(&self, name: &str) -> Option<&TypeObject> {
        self.types.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env() {
        let env = Env::new();
        assert_eq!(env.get_type("Int"), Some(&TypeObject::IntType));
        assert_eq!(env.get_type("Float"), Some(&TypeObject::FloatType));
        assert_eq!(env.get_type("String"), Some(&TypeObject::StringType));
        assert_eq!(env.get_type("Char"), Some(&TypeObject::CharType));
        assert_eq!(env.get_type("Bool"), Some(&TypeObject::BoolType));
    }
}
