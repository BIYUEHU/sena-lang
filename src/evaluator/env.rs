use super::{
    object::{Object, TypeObject},
    EvalError,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Env<'a> {
    pub parent: Option<&'a Env<'a>>,
    pub values: HashMap<String, Object>,
    pub types: HashMap<String, TypeObject>,
}

impl PartialEq for Env<'_> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        let mut env = Env {
            parent: None,
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
        // 示例内置函数
        self.insert_value(
            "print".to_string(),
            Object::Function {
                params: vec!["x".to_string()],
                body: Box::new(crate::ast::Expr::Ident("print_builtin".to_string())),
            },
        )
        .unwrap();
        self.insert_value(
            "get_timestrap".to_string(),
            Object::Function {
                params: vec![],
                body: Box::new(crate::ast::Expr::Ident("get_timestrap_builtin".to_string())),
            },
        )
        .unwrap();
    }

    /// 创建一个扩展环境，其 parent 指向当前环境
    pub fn extend(parent: &'a Env<'a>) -> Env<'a> {
        Env {
            parent: Some(parent),
            values: HashMap::new(),
            types: HashMap::new(),
        }
    }

    /// 插入变量时先检查当前环境，不检查父环境（查找时会递归）
    pub fn insert_value(&mut self, name: String, value: Object) -> Result<(), EvalError> {
        if self.values.contains_key(&name) || self.types.contains_key(&name) {
            Err(EvalError::RedefinedIdentifier(format!(
                "Identifier '{}' is already defined",
                name
            )))
        } else {
            self.values.insert(name, value);
            Ok(())
        }
    }

    /// 插入类型时先检查当前环境
    pub fn insert_type(&mut self, name: String, type_obj: TypeObject) -> Result<(), EvalError> {
        if self.types.contains_key(&name) || self.values.contains_key(&name) {
            Err(EvalError::RedefinedIdentifier(format!(
                "Identifier '{}' is already defined",
                name
            )))
        } else {
            self.types.insert(name, type_obj);
            Ok(())
        }
    }

    /// 从当前环境及其父环境中查找变量
    pub fn get_value(&self, name: &str) -> Option<Object> {
        if let Some(val) = self.values.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get_value(name)
        } else {
            None
        }
    }

    /// 从当前环境及其父环境中查找类型
    pub fn get_type(&self, name: &str) -> Option<TypeObject> {
        if let Some(typ) = self.types.get(name) {
            Some(typ.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get_type(name)
        } else {
            None
        }
    }
}
