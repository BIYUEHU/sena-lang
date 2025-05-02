use super::object::TypeObject;
use crate::{
    lexer::token::Token,
    parser::ast::{Literal, Pattern, TypeVariant},
};

impl CheckedExpr {
    pub fn get_type(&self) -> &TypeObject {
        match self {
            CheckedExpr::Ident {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::Prefix {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::Infix {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::Call {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::Function {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::If {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::Match {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::LetIn {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::Block {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::Literal {
                type_annotation, ..
            } => type_annotation,
            CheckedExpr::Internal {
                type_annotation, ..
            } => type_annotation,
        }
    }

    pub fn clone_type(&self) -> TypeObject {
        self.get_type().clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CheckedExpr {
    Ident {
        value: String,
        type_annotation: TypeObject,
    },
    Prefix {
        op: Token,
        expr: Box<CheckedExpr>,
        type_annotation: TypeObject,
    },
    Infix {
        op: Token,
        left: Box<CheckedExpr>,
        right: Box<CheckedExpr>,
        type_annotation: TypeObject,
    },
    Call {
        callee: Box<CheckedExpr>,
        params: Vec<CheckedExpr>,
        type_annotation: TypeObject,
    },
    Function {
        params: Vec<String>,
        body: Box<CheckedExpr>,
        type_annotation: TypeObject,
    },
    If {
        condition: Box<CheckedExpr>,
        then_branch: Box<CheckedExpr>,
        else_branch: Box<CheckedExpr>,
        type_annotation: TypeObject,
    },
    Match {
        expr: Box<CheckedExpr>,
        cases: Vec<CheckedCase>,
        type_annotation: TypeObject,
    },
    LetIn {
        name: String,
        value: Box<CheckedExpr>,
        body: Box<CheckedExpr>,
        type_annotation: TypeObject,
    },
    Block {
        stmts: Vec<CheckedStmt>,
        type_annotation: TypeObject,
    },
    Literal {
        value: Literal,
        type_annotation: TypeObject,
    },
    Internal {
        value: String,
        type_annotation: TypeObject,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct CheckedCase {
    pub pattern: Pattern,
    pub guard: CheckedExpr,
    pub body: CheckedExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CheckedStmt {
    Let {
        name: String,
        type_annotation: TypeObject,
        value: CheckedExpr,
    },
    Type {
        name: String,
        type_annotation: TypeObject,
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
        body: Box<CheckedStmt>,
        only_abstract: bool,
    },
    Expr(CheckedExpr),
}
