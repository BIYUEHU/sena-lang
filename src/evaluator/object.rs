use crate::ast::Expr;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Array(Vec<Object>),
    Function {
        params: Vec<String>,
        body: Box<Expr>,
        // env: Env,
    },
    Type(TypeObject),
    ADT {
        type_name: String,
        variant: String,
        fields: Vec<Object>,
    },
    Unit,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Object::Int(i) => write!(f, "{}", i),
            Object::Float(fl) => write!(f, "{}", fl),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Char(c) => write!(f, "'{}'", c),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Array(arr) => {
                let elements: Vec<String> = arr.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", elements.join(", "))
            }
            Object::Function { .. } => write!(f, "<function>"),
            Object::Type(t) => write!(f, "{}", t),
            Object::ADT {
                type_name,
                variant,
                fields,
            } => {
                if fields.is_empty() {
                    write!(f, "{}::{}", type_name, variant)
                } else {
                    let field_strs: Vec<String> = fields.iter().map(|v| v.to_string()).collect();
                    write!(f, "{}::{}({})", type_name, variant, field_strs.join(", "))
                }
            }
            Object::Unit => write!(f, "()"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeObject {
    IntType,
    FloatType,
    StringType,
    CharType,
    BoolType,
    ArrayType(Box<TypeObject>),
    FunctionType(Box<TypeObject>, Box<TypeObject>),
    CustomType {
        name: String,
        constructors: Vec<(String, (usize, Object))>,
    },
}

impl Display for TypeObject {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TypeObject::IntType => write!(f, "Int"),
            TypeObject::FloatType => write!(f, "Float"),
            TypeObject::StringType => write!(f, "String"),
            TypeObject::CharType => write!(f, "Char"),
            TypeObject::BoolType => write!(f, "Bool"),
            TypeObject::ArrayType(t) => write!(f, "[{}]", t),
            TypeObject::FunctionType(param, ret) => write!(f, "{} -> {}", param, ret),
            TypeObject::CustomType { name, .. } => write!(f, "{}", name),
        }
    }
}
