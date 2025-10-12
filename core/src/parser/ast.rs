use crate::lexer::token::Token;
use core::fmt;
use std::fmt::{Display, Formatter};

#[derive(PartialEq, Clone, Debug)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

impl Display for Kind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Kind::Star => write!(f, "Type"),
            Kind::Arrow(t1, t2) => {
                if matches!(**t2, Kind::Arrow(..)) {
                    write!(f, "({}) -> {}", t1, t2)
                } else {
                    write!(f, "{} -> {}", t1, t2)
                }
            }
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum TypeExpr {
    Var(String),
    Con(String),
    App(Box<TypeExpr>, Vec<TypeExpr>), // Vec -> Box
    Arrow(Box<TypeExpr>, Box<TypeExpr>),
    Forall(Vec<(String, Kind)>, Box<TypeExpr>),
    // DepFn(String, Box<TypeExpr>, Box<TypeExpr>),
    Lambda((String, Box<TypeExpr>), Box<TypeExpr>),
    // Constraint(String, Vec<TypeExpr>),
    Literal(Literal),
    Kind(Kind),
}

impl TryFrom<Expr> for TypeExpr {
    type Error = ();

    // TODO: here is used to convert to type expressions from value expressions, that's need to be improved
    fn try_from(expr: Expr) -> Result<Self, ()> {
        match expr {
            Expr::Ident(name) => Ok(TypeExpr::Con(name)),
            Expr::Literal(literal) => Ok(TypeExpr::Literal(literal)),
            Expr::Call { callee, params } => Ok(TypeExpr::App(
                Box::new(TypeExpr::try_from(*callee)?),
                params
                    .into_iter()
                    .map(|arg| TypeExpr::try_from(arg))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Expr::Infix(op, left, right) if op == Token::ThinArrow => Ok(TypeExpr::Arrow(
                Box::new(TypeExpr::try_from(*left)?),
                Box::new(TypeExpr::try_from(*right)?),
            )),
            _ => Err(()),
        }
    }
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TypeExpr::Var(v) => write!(f, "{}", v),
            TypeExpr::Con(c) => write!(f, "{}", c),
            TypeExpr::App(t1, ts) => {
                write!(
                    f,
                    "({}) {}",
                    t1,
                    ts.iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            TypeExpr::Arrow(t1, t2) => {
                if matches!(**t1, TypeExpr::Arrow(..)) {
                    write!(f, "({}) -> {}", t1, t2)
                } else {
                    write!(f, "{} -> {}", t1, t2)
                }
            }
            TypeExpr::Literal(l) => write!(f, "Literal({})", l),
            TypeExpr::Forall(vs, t) => {
                write!(
                    f,
                    "forall {}. {}",
                    vs.iter()
                        .map(|(v, t)| format!("{}: {}", v, t))
                        .collect::<Vec<_>>()
                        .join(", "),
                    t
                )
            }
            TypeExpr::Lambda((v, t1), t2) => {
                write!(f, "({} -> {}) -> {}", t1, t2, v)
            }
            TypeExpr::Kind(k) => write!(f, "{}", k),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Ident(String),
    Prefix(Token, Box<Expr>),
    Infix(Token, Box<Expr>, Box<Expr>),
    Call {
        callee: Box<Expr>,
        params: Vec<Expr>,
    },
    Function {
        params: Vec<(String, Option<TypeExpr>)>,
        body: Box<Expr>,
        return_type: Option<TypeExpr>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Match {
        expr: Box<Expr>,
        cases: Vec<MatchCase>,
    },
    LetIn {
        name: String,
        type_annotation: Option<TypeExpr>,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Literal(Literal),
    Block(Vec<Stmt>),
    Internal(String),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    String(String),
    Char(char),
    Int(i64),
    Float(f64),
    Bool(bool),
    Array(Vec<Expr>),
    Unit,
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Char(c) => write!(f, "'{}'", c),
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Array(_) => {
                write!(f, "[...]")
            }
            Literal::Unit => write!(f, "()"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Let {
        name: String,
        type_annotation: Option<TypeExpr>,
        value: Box<Expr>,
    },
    Type {
        name: String,
        params: Vec<String>,
        kind_annotation: Option<TypeExpr>,
        variants: Vec<TypeVariant>,
    },
    ImportAll {
        source: String,
        alias: String,
    },
    ImportSome {
        source: String,
        items: Vec<String>,
    },
    Export {
        body: Box<Stmt>,
        only_abstract: bool,
    },
    Expr(Expr),
}

pub type UnsafeProgram = Vec<Stmt>;

#[derive(PartialEq, Clone, Debug)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Expr,
    pub guard: Expr,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Pattern {
    Ident(String),
    ADTConstructor { name: String, args: Vec<Pattern> },
    Literal(Literal),
}

#[derive(PartialEq, Clone, Debug)]
pub struct TypeVariant {
    pub name: String,
    pub fields: TypeVariantFields,
}

#[derive(PartialEq, Clone, Debug)]
pub enum TypeVariantFields {
    Tuple(Vec<TypeExpr>),
    Record(Vec<(String, TypeExpr)>),
    Unit,
}
