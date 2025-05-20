use crate::{
    checker::{
        ast::{Checked, CheckedExpr},
        object::TypeObject,
    },
    env::EvaluatorEnv,
    parser::ast::Kind,
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
    Array(Vec<Object>), // TODO: add type info for array elements
    Function {
        params: Vec<String>,
        body: CheckedExpr,
        env: Rc<RefCell<EvaluatorEnv>>,
        type_annotation: TypeObject,
    },
    ADTValue {
        variant: String,
        fields: Vec<Object>,
        type_annotation: TypeObject,
    },
    Type {
        value: TypeObject,
        kind_annotation: Kind,
    },
    /*? TypeFn {
        param: String,
        kind: TypeObject,
        body: Box<TypeExpr>,
    }, */ //?
    // ADTConstructor {
    //     type_name: String,
    //     type_params: Vec<TypeExpr>,
    //     variant: String,
    //     fields: Vec<TypeExpr>,
    // },
    // TypeConstructor {
    //     type_name: String,
    //     type_params: Vec<String>,
    //     constructors: Vec<(String, Vec<TypeExpr>)>,
    // },
    Unit,
}

impl From<i64> for Object {
    fn from(i: i64) -> Self {
        Object::Int(i)
    }
}

impl From<f64> for Object {
    fn from(f: f64) -> Self {
        Object::Float(f)
    }
}

impl From<String> for Object {
    fn from(s: String) -> Self {
        Object::String(s)
    }
}

impl From<&str> for Object {
    fn from(s: &str) -> Self {
        Object::String(s.to_string())
    }
}

impl From<char> for Object {
    fn from(c: char) -> Self {
        Object::Char(c)
    }
}

impl From<bool> for Object {
    fn from(b: bool) -> Self {
        Object::Bool(b)
    }
}

impl<T: Into<Object>> From<Vec<T>> for Object {
    fn from(t: Vec<T>) -> Self {
        Object::Array(
            {
                let t: Vec<_> = t.into();
                t
            }
            .into_iter()
            .map(|o| o.into())
            .collect(),
        )
    }
}

impl Checked for Object {
    fn get_type(&self) -> TypeObject {
        match self {
            Object::Int(_) => TypeObject::Int,
            Object::Float(_) => TypeObject::Float,
            Object::String(_) => TypeObject::String,
            Object::Char(_) => TypeObject::Char,
            Object::Bool(_) => TypeObject::Bool,
            Object::Array(elements) => TypeObject::ADTInst {
                name: "Array".to_string(),
                params: vec![elements
                    .first()
                    .map(|e| e.get_type().clone())
                    .unwrap_or(TypeObject::Unknown)],
            },
            Object::Function {
                type_annotation, ..
            } => type_annotation.clone(),
            Object::ADTValue {
                type_annotation, ..
            } => type_annotation.clone(),
            Object::Type {
                kind_annotation, ..
            } => TypeObject::Kind(kind_annotation.clone()),
            Object::Unit => TypeObject::Unit,
        }
    }
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
            Object::Type { value, .. } => write!(f, "Kind({})", value),
            Object::ADTValue {
                variant,
                fields,
                type_annotation,
            } => {
                if fields.is_empty() {
                    write!(f, "ADT({}::{})", type_annotation, variant)
                } else {
                    let fs: Vec<String> = fields.iter().map(|o| o.to_string()).collect();
                    write!(
                        f,
                        "ADT({}::{}({}))",
                        type_annotation,
                        variant,
                        fs.join(", ")
                    )
                }
            }
            // Object::ADTConstructor {
            //     type_name,
            //     type_params,
            //     variant,
            //     fields,
            // } => {
            //     write!(
            //         f,
            //         "ADTConstructor({}::{}({}))",
            //         format_type_name(type_name.clone(), type_params.clone()),
            //         variant,
            //         fields
            //             .iter()
            //             .map(|t| (**t).to_string())
            //             .collect::<Vec<String>>()
            //             .join(", ")
            //     )
            // }
            // Object::TypeConstructor { .. } => {
            //     write!(f, "TypeConstructor({})", self.type_info())
            // }
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
            Object::Type { value, .. } => value.to_string(),
            Object::ADTValue {
                variant, fields, ..
            } => {
                if fields.is_empty() {
                    variant.clone()
                } else {
                    format!(
                        "{}({})",
                        variant,
                        fields
                            .iter()
                            .map(|o| o.pretty_print())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            // Object::ADTConstructor { .. } => "<adt_constructor>".to_string(),
            // Object::TypeConstructor { .. } => "<type_constructor>".to_string(),
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
            Object::Type {
                kind_annotation, ..
            } => kind_annotation.to_string(),
            Object::Array(arr) => {
                if let Some(first) = arr.first() {
                    format!("[{}]", first.type_info())
                } else {
                    "[Unknown]".to_string()
                }
            }
            Object::Function {
                type_annotation, ..
            } => type_annotation.to_string(),
            Object::ADTValue {
                type_annotation, ..
            } => type_annotation.to_string(),
            // Object::ADTConstructor {
            //     type_name,
            //     type_params,
            //     fields,
            //     ..
            // } => format!(
            //     "{} -> {}",
            //     fields
            //         .iter()
            //         .map(|_| "Kind")
            //         .collect::<Vec<_>>()
            //         .join(" -> "),
            //     format_type_name(type_name.clone(), type_params.clone()),
            // ),
            // Object::TypeConstructor { type_params, .. } => format!(
            //     "{} -> Kind",
            //     type_params
            //         .iter()
            //         .map(|_| "Kind")
            //         .collect::<Vec<_>>()
            //         .join(" -> "),
            // ),
            Object::Unit => "Unit".to_string(),
        }
    }
}
