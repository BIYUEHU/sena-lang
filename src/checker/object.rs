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

pub static PRIMIVE_TYPES: [TypeObject; 9] = [
    TypeObject::Kind,
    TypeObject::Int,
    TypeObject::Float,
    TypeObject::String,
    TypeObject::Char,
    TypeObject::Bool,
    TypeObject::Unit,
    TypeObject::Unknown,
    TypeObject::Any,
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

// impl Into<TypeObject> for TypeExpr {
//     fn into(self) -> TypeObject {
//         match self {
//             TypeExpr::Con(name) => match name.as_str() {
//                 "Int" => TypeObject::Int,
//                 "Float" => TypeObject::Float,
//                 "String" => TypeObject::String,
//                 "Char" => TypeObject::Char,
//                 "Bool" => TypeObject::Bool,
//                 "Kind" => TypeObject::Kind,
//                 "Unit" => TypeObject::Unit,
//                 "Unknown" => TypeObject::Unknown,
//                 "Any" => TypeObject::Any,
//                 _ => TypeObject::ADT {
//                     name,
//                     type_params: vec![],
//                     constructors: vec![],
//                 },
//             },
//             TypeExpr::App(f, args) => {
//                 let f = f.as_ref().clone().into();
//                 let args = args
//                     .iter()
//                     .map(|t| t.clone().into())
//                     .collect::<Vec<TypeObject>>();
//                 match f {
//                     TypeExpr::Con(name) => match name.as_str() {
//                         "Array" => {
//                             // TypeObject::Array(Box::new(args[0].into()))
//                             unimplemented!()
//                         }
//                         _ => TypeObject::ADT {
//                             name,
//                             type_params: vec![],
//                             constructors: vec![],
//                         },
//                     },
//                     TypeExpr::Var(_) => TypeObject::Unknown,
//                     TypeExpr::Arrow(param, ret) => {
//                         TypeObject::Function(Box::new((*param).into()), Box::new((*ret).into()))
//                     }
//                     TypeExpr::App(_, _) => TypeObject::Unknown,
//                     TypeExpr::Literal(_) => TypeObject::Unknown,
//                 }
//             }
//             TypeExpr::Arrow(param, ret) => {
//                 TypeObject::Function(Box::new((*param).into()), Box::new((*ret).into()))
//             }
//             TypeExpr::Var(_) => TypeObject::Unknown,
//             TypeExpr::Literal(_) => TypeObject::Unknown,
//         }
//     }
// }

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
