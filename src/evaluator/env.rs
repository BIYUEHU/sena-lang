use super::object::{Object, TypeObject};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env {
    pub parent: Option<Rc<RefCell<Env>>>,
    pub values: HashMap<String, Object>,
    pub types: HashMap<String, TypeObject>,
}

impl PartialEq for Env {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

// TODO: 实现 Env 的相关方法

impl Env {
    pub fn new() -> Self /* Rc<RefCell<Self>> */ {
        let mut env = Env {
            parent: None,
            values: HashMap::new(),
            types: HashMap::new(),
        };
        // let rc_env = Rc::new(RefCell::new(env));
        // 初始化全局环境（内置类型、函数等）
        // rc_env.borrow_mut().init();
        // rc_env
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
            true,
        )
        .unwrap();
        self.insert_value(
            "get_timestrap".to_string(),
            Object::Function {
                params: vec![],
                body: Box::new(crate::ast::Expr::Ident("get_timestrap_builtin".to_string())),
            },
            true,
        )
        .unwrap();
    }

    /// 创建一个扩展环境，其 parent 指向当前环境
    pub fn extend(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            parent: Some(parent),
            values: HashMap::new(),
            types: HashMap::new(),
        }))
    }

    /// 插入变量时先检查当前环境，不检查父环境（查找时会递归）
    pub fn insert_value(&mut self, name: String, value: Object, force: bool) -> Result<(), String> {
        if !force && (self.values.contains_key(&name) || self.types.contains_key(&name)) {
            Err(format!("Identifier '{}' is already defined", name))
        } else {
            self.values.insert(name, value);
            Ok(())
        }
    }

    /// 插入类型时先检查当前环境
    pub fn insert_type(&mut self, name: String, type_obj: TypeObject) -> Result<(), String> {
        if self.types.contains_key(&name) || self.values.contains_key(&name) {
            Err(format!("Identifier '{}' is already defined", name))
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
            parent.borrow().get_value(name)
        } else {
            None
        }
    }

    /// 从当前环境及其父环境中查找类型
    pub fn get_type(&self, name: &str) -> Option<TypeObject> {
        if let Some(typ) = self.types.get(name) {
            Some(typ.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get_type(name)
        } else {
            None
        }
    }
}
