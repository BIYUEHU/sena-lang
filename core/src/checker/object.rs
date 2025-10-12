use core::fmt;
use std::fmt::{Display, Formatter};

use crate::parser::ast::Kind;

#[derive(Clone, PartialEq, Debug)]
pub enum TypeObject {
    Int,
    Float,
    String,
    Char,
    Bool,
    Unit,
    Any,
    Kind(Kind),
    Var(String),
    Function(Box<TypeObject>, Box<TypeObject>),
    ADTDef {
        name: String,
        params: Vec<String>,
        constructors: Vec<(String, TypeObject)>,
    },
    ADTInst {
        name: String,
        params: Vec<TypeObject>,
    },
    Forall(Vec<(String, TypeObject)>, Box<TypeObject>), // System F, Rank-n
    Lambda((String, Box<TypeObject>), Box<TypeObject>), // ?
}

impl TypeObject {
    pub fn as_return_type(&self, params: Vec<TypeObject>) -> TypeObject {
        match params.len() {
            0 => TypeObject::Function(Box::new(TypeObject::Unit), Box::new(self.clone())),
            1 => TypeObject::Function(Box::new(params[0].clone()), Box::new(self.clone())),
            _ => TypeObject::Function(
                Box::new(params[0].clone()),
                Box::new(self.as_return_type(params[1..].to_vec())),
            ),
        }
    }

    pub fn get_last_type(&self) -> TypeObject {
        if let TypeObject::Function(_, ret) = self {
            ret.get_last_type()
        } else {
            self.clone()
        }
    }
}

pub static PRIMIVE_TYPES: [TypeObject; 8] = [
    TypeObject::Kind(Kind::Star),
    TypeObject::Int,
    TypeObject::Float,
    TypeObject::String,
    TypeObject::Char,
    TypeObject::Bool,
    TypeObject::Unit,
    TypeObject::Any,
];

// impl Into<TypeExpr> for TypeObject {
//     fn into(self) -> TypeExpr {
//         match self {
//             TypeObject::Int
//             | TypeObject::Float
//             | TypeObject::String
//             | TypeObject::Char
//             | TypeObject::Bool
//             | TypeObject::Kind
//             | TypeObject::Unit
//             | TypeObject::Unknown
//             | TypeObject::Any => TypeExpr::Con(self.to_string()),
//             // TypeObject::Array(t) => {
//             //     TypeExpr::App(Box::new(TypeExpr::Con("Array".to_string())), vec![t.into()])
//             // }
//             TypeObject::Function(param, ret) => TypeExpr::Arrow(
//                 Box::new(param.as_ref().clone().into()),
//                 Box::new(ret.as_ref().clone().into()),
//             ),
//             TypeObject::ADT {
//                 name,
//                 type_params,
//                 constructors,
//             } => {
//                 let mut args = vec![];
//                 for (_, ts) in constructors {
//                     args.extend(ts.iter().cloned());
//                 }
//                 TypeExpr::App(
//                     Box::new(TypeExpr::Con(name)),
//                     type_params
//                         .iter()
//                         .map(|s| TypeExpr::Var(s.clone()))
//                         .collect(),
//                 )
//             }
//         }
//     }
// }

// impl Into<TypeObject> for TypeExpr {
//     fn into(self) -> TypeObject {
//         match self {
//             TypeExpr::Con(name) => match name.as_str() {
//                 "Int" => TypeObject::Int,
//                 "Float" => TypeObject::Float,
//                 "String" => TypeObject::String,
//                 "Char" => TypeObject::Char,
//                 "Bool" => TypeObject::Bool,
//                 "Type" => TypeObject::Kind,
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
            TypeObject::Kind(kind) => write!(f, "{}", kind),
            TypeObject::Int => write!(f, "Int"),
            TypeObject::Float => write!(f, "Float"),
            TypeObject::String => write!(f, "String"),
            TypeObject::Char => write!(f, "Char"),
            TypeObject::Bool => write!(f, "Bool"),
            TypeObject::Unit => write!(f, "Unit"),
            TypeObject::Var(name) => write!(f, "Var({})", name),
            TypeObject::Any => write!(f, "Any"),
            // TypeObject::Array(t) => write!(f, "[{}]", t),
            TypeObject::Function(param, ret) => {
                if matches!(**param, TypeObject::Function(_, _)) {
                    write!(f, "({}) -> {}", param, ret)
                } else {
                    write!(f, "{} -> {}", param, ret)
                }
            }
            TypeObject::ADTDef { name, .. } => write!(f, "{}", name),
            TypeObject::ADTInst { name, params } => {
                if params.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(
                        f,
                        "{}<{}>",
                        name,
                        params
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            TypeObject::Forall(vars, body) => {
                write!(
                    f,
                    "forall {}. {}", // TODO: here copyed from haskell
                    vars.iter()
                        .map(|(v, t)| format!("{}: {}", v, t))
                        .collect::<Vec<String>>()
                        .join(", "),
                    body
                )
            }
            TypeObject::Lambda((_var, param), ret) => {
                if matches!(**param, TypeObject::Function(_, _)) {
                    write!(f, "({}) -> {}", param, ret)
                } else {
                    write!(f, "{} -> {}", param, ret)
                }
            }
        }
    }
}
