use crate::{
    checker::object::TypeObject,
    common::env::EvaluatorEnv,
    parser::ast::{Expr, TypeExpr},
    utils::format_type_name,
};
use std::{
    cell::RefCell,
    fmt::{self, Display, Formatter},
    rc::Rc,
};

pub trait PrettyPrint {
    fn pretty_print(&self) -> String;
}

pub trait TypeInfo {
    fn type_info(&self) -> String;
}

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
        env: Rc<RefCell<EvaluatorEnv>>,
    },
    Type(TypeObject),
    ADT {
        type_name: String,
        type_params: Vec<Box<TypeExpr>>,
        variant: String,
        fields: Vec<Object>,
    },
    ADTConstructor {
        type_name: String,
        type_params: Vec<Box<TypeExpr>>,
        variant: String,
        fields: Vec<Box<TypeExpr>>,
    },
    TypeConstructor {
        type_name: String,
        type_params: Vec<String>,
        constructors: Vec<(String, Vec<TypeExpr>)>,
    },
    Unit,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Object::Int(i) => write!(f, "Int({})", i),
            Object::Float(fl) => write!(f, "Float({})", fl),
            Object::String(s) => write!(f, "String(\"{}\")", s),
            Object::Char(c) => write!(f, "Char('{}')", c),
            Object::Bool(b) => write!(f, "Bool({})", b),
            Object::Array(arr) => {
                let elems: Vec<String> = arr.iter().map(|o| o.to_string()).collect();
                write!(f, "Array([{}])", elems.join(", "))
            }
            Object::Function { .. } => {
                write!(f, "Function({})", self.type_info())
            }
            Object::Type(t) => write!(f, "Kind({})", t),
            Object::ADT {
                type_name,
                type_params,
                variant,
                fields,
            } => {
                if fields.is_empty() {
                    write!(
                        f,
                        "ADT({}::{})",
                        format_type_name(type_name.clone(), type_params.clone()),
                        variant
                    )
                } else {
                    let fs: Vec<String> = fields.iter().map(|o| o.to_string()).collect();
                    write!(f, "ADT({}::{}({}))", type_name, variant, fs.join(", "))
                }
            }
            Object::ADTConstructor {
                type_name,
                type_params,
                variant,
                fields,
            } => {
                write!(
                    f,
                    "ADTConstructor({}::{}({}))",
                    format_type_name(type_name.clone(), type_params.clone()),
                    variant,
                    fields
                        .iter()
                        .map(|t| (**t).to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Object::TypeConstructor { .. } => {
                write!(f, "TypeConstructor({})", self.type_info())
            }
            Object::Unit => write!(f, "()"),
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
                type_params,
                variant,
                fields,
            } => {
                let type_name = format_type_name(type_name.clone(), type_params.clone());
                if fields.is_empty() {
                    format!("{}::{}", type_name, variant)
                } else {
                    format!(
                        "{}::{}({})",
                        type_name,
                        variant,
                        fields
                            .iter()
                            .map(|o| o.pretty_print())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Object::ADTConstructor { .. } => "<adt_constructor>".to_string(),
            Object::TypeConstructor { .. } => "<type_constructor>".to_string(),
            Object::Unit => "()".to_string(),
        }
    }
}

impl TypeInfo for Object {
    fn type_info(&self) -> String {
        match self {
            Object::Int(_) => "Int".to_string(),
            Object::Float(_) => "Float".to_string(),
            Object::String(_) => "String".to_string(),
            Object::Char(_) => "Char".to_string(),
            Object::Bool(_) => "Bool".to_string(),
            Object::Type(_) => "Kind".to_string(),
            Object::Array(arr) => {
                if let Some(first) = arr.first() {
                    format!("[{}]", first.type_info())
                } else {
                    "[Unknown]".to_string()
                }
            }
            Object::Function {
                type_params,
                params,
                return_type,
                ..
            } => {
                let type_params = type_params
                    .iter()
                    .map(|(n, t)| format!("{}: {}", n, t))
                    .collect::<Vec<String>>()
                    .join(", ");
                let params = if params.is_empty() {
                    "Unit".to_string()
                } else {
                    params
                        .iter()
                        .map(|(_, t)| {
                            if matches!(**t, TypeExpr::Arrow(..)) {
                                format!("({})", t)
                            } else {
                                t.to_string()
                            }
                        })
                        .collect::<Vec<String>>()
                        .join(" -> ")
                };
                if type_params.is_empty() {
                    format!("{} -> {}", params, return_type)
                } else {
                    format!("<{}> {} -> {}", type_params, params, return_type)
                }
            }
            Object::ADT {
                type_name,
                type_params,
                ..
            } => format_type_name(type_name.clone(), type_params.clone()),
            Object::ADTConstructor {
                type_name,
                type_params,
                fields,
                ..
            } => format!(
                "{} -> {}",
                fields
                    .iter()
                    .map(|_| "Kind")
                    .collect::<Vec<_>>()
                    .join(" -> "),
                format_type_name(type_name.clone(), type_params.clone()),
            ),
            Object::TypeConstructor { type_params, .. } => format!(
                "{} -> Kind",
                type_params
                    .iter()
                    .map(|_| "Kind")
                    .collect::<Vec<_>>()
                    .join(" -> "),
            ),
            Object::Unit => "Unit".to_string(),
        }
    }
}
