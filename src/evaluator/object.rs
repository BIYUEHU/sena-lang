use crate::ast::{Expr, TypeExpr};
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
        type_params: Vec<(String, Box<TypeExpr>)>,
        params: Vec<(String, Box<TypeExpr>)>,
        body: Box<Expr>,
        return_type: Box<TypeExpr>,
    },
    Type(TypeObject),
    ADT {
        type_name: String,
        variant: String,
        fields: Vec<Object>,
    },
    ADTConstructor {
        type_name: String,
        variant: String,
        param_count: usize,
    },
    Unit,
}

/// Trait for detailed internal inspection (e.g. for REPL debug output)
pub trait Inspect {
    fn inspect(&self) -> String;
}

/// Trait for user-friendly output (for target language I/O)
pub trait PrettyPrint {
    fn pretty_print(&self) -> String;
}

/// Trait for detailed type information output (类似于 Haskell 的 :t)
pub trait TypeInfo {
    fn type_info(&self) -> String;
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TypeExpr::Var(v) => write!(f, "{}", v),
            TypeExpr::Con(c) => write!(f, "{}", c),
            TypeExpr::App(t1, ts) => {
                // TODO
                write!(f, "({}) {:?}", t1, ts)
            }
            TypeExpr::Arrow(t1, t2) => {
                write!(f, "{} -> {}", t1, t2)
            }
            TypeExpr::Literal(l) => write!(f, "{:?}", l),
        }
    }
}

// TODO: merge to display trait
impl Inspect for Object {
    fn inspect(&self) -> String {
        match self {
            Object::Int(i) => format!("Int({})", i),
            Object::Float(f) => format!("Float({})", f),
            Object::String(s) => format!("String(\"{}\")", s),
            Object::Char(c) => format!("Char('{}')", c),
            Object::Bool(b) => format!("Bool({})", b),
            Object::Array(arr) => {
                let elems: Vec<String> = arr.iter().map(|o| o.inspect()).collect();
                format!("Array([{}])", elems.join(", "))
            }
            Object::Function {
                type_params,
                params,
                return_type,
                ..
            } => {
                let tps: Vec<String> = type_params
                    .iter()
                    .map(|(n, t)| format!("{}:{}", n, t))
                    .collect();
                let ps: Vec<String> = params.iter().map(|(n, t)| format!("{}:{}", n, t)).collect();
                format!(
                    "<Function [{}] ({}) -> {}>",
                    tps.join(", "),
                    ps.join(", "),
                    return_type,
                )
            }
            Object::Type(t) => format!("Kind({})", t),
            Object::ADT {
                type_name,
                variant,
                fields,
            } => {
                // TODO
                if fields.is_empty() {
                    format!("ADT({}:{})", type_name, variant)
                } else {
                    let fs: Vec<String> = fields.iter().map(|o| o.inspect()).collect();
                    format!("ADT({}:{}({}))", type_name, variant, fs.join(", "))
                }
            }
            Object::ADTConstructor {
                type_name,
                variant,
                param_count,
            } => {
                if *param_count == 0 {
                    format!("ADTConstructor({}:{})", type_name, variant)
                } else {
                    format!(
                        "ADTConstructor({}:{} ({} params))",
                        type_name, variant, param_count
                    )
                }
            }
            Object::Unit => "Unit".to_string(),
        }
    }
}

impl PrettyPrint for Object {
    fn pretty_print(&self) -> String {
        match self {
            Object::Int(i) => i.to_string(),
            Object::Float(f) => f.to_string(),
            Object::String(s) => s.clone(),
            Object::Char(c) => c.to_string(),
            Object::Bool(b) => b.to_string(),
            Object::Array(arr) => {
                let elems: Vec<String> = arr.iter().map(|o| o.pretty_print()).collect();
                format!("[{}]", elems.join(", "))
            }
            Object::Function { .. } => "<function>".to_string(),
            Object::Type(t) => t.to_string(),
            Object::ADT {
                type_name,
                variant,
                fields,
            } => {
                if fields.is_empty() {
                    format!("{}:{}()", type_name, variant)
                } else {
                    let fs: Vec<String> = fields.iter().map(|o| o.pretty_print()).collect();
                    format!("{}:{}({})", type_name, variant, fs.join(", "))
                }
            }
            Object::ADTConstructor {
                type_name,
                variant,
                param_count,
            } => {
                if *param_count == 0 {
                    format!("{}:{}", type_name, variant)
                } else {
                    format!("{}:{}(...)", type_name, variant)
                }
            }
            Object::Unit => "()".to_string(),
        }
    }
}

// --- 实现 TypeInfo：输出目标值的类型信息 --- //
impl TypeInfo for Object {
    fn type_info(&self) -> String {
        match self {
            Object::Int(_) => "Int".to_string(),
            Object::Float(_) => "Float".to_string(),
            Object::String(_) => "String".to_string(),
            Object::Char(_) => "Char".to_string(),
            Object::Bool(_) => "Bool".to_string(),
            Object::Array(arr) => {
                if let Some(first) = arr.first() {
                    format!("[{}]", first.type_info())
                } else {
                    "[?]".to_string()
                }
            }
            Object::Function {
                type_params,
                params,
                return_type,
                ..
            } => {
                let tps: Vec<String> = type_params
                    .iter()
                    .map(|(n, t)| format!("{}:{}", n, t))
                    .collect();
                let ps: Vec<String> = params.iter().map(|(n, t)| format!("{}:{}", n, t)).collect();
                format!(
                    "<Function [{}] ({}) -> {}>",
                    tps.join(", "),
                    ps.join(", "),
                    return_type
                )
            }
            Object::Type(t) => format!("{}", t),
            Object::ADT {
                type_name,
                variant,
                fields,
            } => {
                let fs: Vec<String> = fields.iter().map(|o| o.type_info()).collect();
                if fs.is_empty() {
                    format!("{}:{}()", type_name, variant)
                } else {
                    format!("{}:{}({})", type_name, variant, fs.join(", "))
                }
            }
            Object::ADTConstructor {
                type_name,
                variant,
                param_count,
            } => {
                if *param_count == 0 {
                    format!("{}:{}", type_name, variant)
                } else {
                    format!("{}:{} ({} params)", type_name, variant, param_count)
                }
            }
            Object::Unit => "Unit".to_string(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeObject {
    Kind,
    Int,
    Float,
    String,
    Char,
    Bool,
    Array(Box<TypeObject>),
    Function(Box<TypeObject>, Box<TypeObject>),
    ADT {
        name: String,
        type_params: Vec<String>,                   // e.g., ["a"] for List a
        constructors: Vec<(String, Vec<TypeExpr>)>, // (variant_name, field_types)
    },
}

impl Into<TypeExpr> for TypeObject {
    fn into(self) -> TypeExpr {
        match self {
            TypeObject::Int
            | TypeObject::Float
            | TypeObject::String
            | TypeObject::Char
            | TypeObject::Bool => TypeExpr::Con(self.to_string()),
            _ => unimplemented!(),
        }
    }
}

impl Display for TypeObject {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TypeObject::Kind => write!(f, "Kind"),
            TypeObject::Int => write!(f, "Int"),
            TypeObject::Float => write!(f, "Float"),
            TypeObject::String => write!(f, "String"),
            TypeObject::Char => write!(f, "Char"),
            TypeObject::Bool => write!(f, "Bool"),
            TypeObject::Array(t) => write!(f, "[{}]", t),
            TypeObject::Function(param, ret) => {
                if matches!(**param, TypeObject::Function(_, _)) {
                    write!(f, "({}) -> {}", param, ret)
                } else {
                    write!(f, "{} -> {}", param, ret)
                }
            }
            TypeObject::ADT { name, .. } => write!(f, "{}", name),
        }
    }
}

pub static PRIMIVE_TYPES: [TypeObject; 5] = [
    TypeObject::Int,
    TypeObject::Float,
    TypeObject::String,
    TypeObject::Char,
    TypeObject::Bool,
];
