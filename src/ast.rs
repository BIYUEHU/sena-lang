use crate::token::Token;

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    StringLiteral(String),
    CharLiteral(char),
    IntLiteral(i64),
    FloatLiteral(f64),
    Ident(String),
    Call {
        callee: Box<Expr>,
        type_args: Option<Vec<String>>,
        arguments: Vec<Expr>,
    },
    Lambda {
        params: Vec<(String, Option<Box<Expr>>)>, // 参数名和可选类型注解
        body: Box<Expr>,
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
    Let {
        name: String,
        type_annotation: Option<Box<Expr>>,
        value: Box<Expr>,
        body: Option<Box<Expr>>, // 用于 let-in 表达式
    },
    Type {
        name: String,
        type_params: Option<Vec<String>>,
        variants: Vec<TypeVariant>,
    },
    Block(Vec<Expr>),
    Import {
        items: Vec<String>,
        source: String,
        is_all: bool,
    },
    Export(Box<Expr>),
}

#[derive(Debug)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub enum Pattern {
    Ident(String),
    Constructor { name: String, args: Vec<Pattern> },
    Literal(Token),
}

#[derive(Debug)]
pub struct TypeVariant {
    pub name: String,
    pub fields: TypeVariantFields,
}

#[derive(Debug)]
pub enum TypeVariantFields {
    Tuple(Vec<Box<Expr>>),
    Record(Vec<(String, Box<Expr>)>),
    Unit,
}
