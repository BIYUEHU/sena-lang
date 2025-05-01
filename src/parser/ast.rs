use core::fmt;
use std::fmt::{Display, Formatter};

use crate::lexer::token::Token;

#[derive(PartialEq, Clone, Debug)]
pub enum TypeExpr {
    Var(String),
    Con(String),
    App(Box<TypeExpr>, Vec<TypeExpr>),
    Arrow(Box<TypeExpr>, Box<TypeExpr>),
    Literal(Literal),
}

impl Default for TypeExpr {
    fn default() -> Self {
        TypeExpr::Con("Unknown".into())
    }
}

impl TryFrom<Expr> for TypeExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, ()> {
        match expr {
            Expr::Ident(name) => Ok(TypeExpr::Con(name)),
            Expr::Literal(literal) => Ok(TypeExpr::Literal(literal)),
            Expr::Call { callee, arguments } => Ok(TypeExpr::App(
                Box::new(TypeExpr::try_from(*callee)?),
                arguments
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
            TypeExpr::Literal(l) => write!(f, "Literal({:?})", l),
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
        arguments: Vec<Expr>,
    },
    Function {
        type_params: Vec<(String, Box<TypeExpr>)>,
        params: Vec<(String, Box<TypeExpr>)>,
        body: Box<Expr>,
        return_type: Box<TypeExpr>,
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
        type_annotation: Box<TypeExpr>,
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

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Let {
        name: String,
        type_annotation: Box<TypeExpr>,
        value: Box<Expr>,
    },
    Type {
        name: String,
        type_annotation: Box<TypeExpr>,
        type_params: Option<Vec<String>>,
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

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,   // $ `xxx` and custom operators
    Or,       // ||
    And,      // &&
    Equal,    // == !=
    Compare,  // < <= > >=
    List,     // : ++
    Sum,      // + -
    Product,  // * / %
    Exponent, // ^
    Mapping,  // ->
    Compose,  // .
    Prefix,   // -X !X
    Call,     // myFunction(x)
    Index,    // array[index]
    Highest,  // (x)
}

impl Precedence {
    pub fn from_token(token: &Token) -> Self {
        match token {
            Token::Plus | Token::Sub => Self::Sum,
            Token::Mul | Token::Div | Token::Mod => Self::Product,
            Token::Equal | Token::NotEqual => Self::Equal,
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => Self::Compare,
            Token::Or => Self::Or,
            Token::And => Self::And,
            Token::ThinArrow => Self::Mapping,
            Token::Arrow => Self::Lowest,
            Token::Not => Self::Prefix,
            Token::LeftParen => Self::Call,
            Token::LeftBracket => Self::Index,
            Token::RightParen | Token::RightBracket => Self::Highest,
            Token::Dot => Self::Compose,
            Token::Colon => Self::List,
            Token::InfixFixity(list) if *list == vec![Token::Plus, Token::Plus] => Self::List,
            Token::Pow => Self::Exponent,
            _ => Self::Lowest,
        }
    }
}

pub type Program = Vec<Stmt>;

#[derive(PartialEq, Clone, Debug)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Box<Expr>,
    pub guard: Box<Expr>,
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
    Tuple(Vec<Box<TypeExpr>>),
    Record(Vec<(String, Box<TypeExpr>)>),
    Unit,
}
