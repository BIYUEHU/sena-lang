use core::fmt;
use std::fmt::{Display, Formatter};

use crate::parser::ast::TypeExpr;

#[derive(Clone, PartialEq, Debug)]
pub enum TypeObject {
    Kind,
    Int,
    Float,
    String,
    Char,
    Bool,
    Unit,
    Unknown,
    Any,
    // Array(Box<TypeObject>),
    Function(Box<TypeObject>, Box<TypeObject>),
    ADT {
        name: String,
        type_params: Vec<String>,
        constructors: Vec<(String, Vec<TypeExpr>)>,
    },
}

pub static PRIMIVE_TYPES: [TypeObject; 7] = [
    TypeObject::Kind,
    TypeObject::Int,
    TypeObject::Float,
    TypeObject::String,
    TypeObject::Char,
    TypeObject::Bool,
    TypeObject::Unit,
];

impl Into<TypeExpr> for TypeObject {
    fn into(self) -> TypeExpr {
        match self {
            TypeObject::Int
            | TypeObject::Float
            | TypeObject::String
            | TypeObject::Char
            | TypeObject::Bool
            | TypeObject::Kind
            | TypeObject::Unit
            | TypeObject::Unknown
            | TypeObject::Any => TypeExpr::Con(self.to_string()),
            // TypeObject::Array(t) => {
            //     TypeExpr::App(Box::new(TypeExpr::Con("Array".to_string())), vec![t.into()])
            // }
            TypeObject::Function(param, ret) => TypeExpr::Arrow(
                Box::new(param.as_ref().clone().into()),
                Box::new(ret.as_ref().clone().into()),
            ),
            TypeObject::ADT {
                name,
                type_params,
                constructors,
            } => {
                let mut args = vec![];
                for (_, ts) in constructors {
                    args.extend(ts.iter().cloned());
                }
                TypeExpr::App(
                    Box::new(TypeExpr::Con(name)),
                    type_params
                        .iter()
                        .map(|s| TypeExpr::Var(s.clone()))
                        .collect(),
                )
            }
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
            TypeObject::Unit => write!(f, "Unit"),
            TypeObject::Unknown => write!(f, "Unknown"),
            TypeObject::Any => write!(f, "*"),
            // TypeObject::Array(t) => write!(f, "[{}]", t),
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
