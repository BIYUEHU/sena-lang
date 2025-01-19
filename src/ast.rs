use crate::token::Token;

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Ident(String),
    Prefix(Token, Box<Expr>),
    Infix(Token, Box<Expr>, Box<Expr>),
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Lambda {
        type_params: Vec<(String, Box<Expr>)>,
        params: Vec<(String, Option<Box<Expr>>)>,
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
    LetIn {
        name: String,
        type_decl: Option<Box<Expr>>,
        value: Box<Expr>,
        body: Box<Expr>,
    },

    Block(Vec<Expr>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    String(String),
    Char(char),
    Int(i64),
    Float(f64),
    Bool(bool),
    Array(Vec<Expr>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Let {
        name: String,
        type_annotation: Option<Box<Expr>>,
        value: Box<Expr>,
    },
    Type {
        name: String,
        type_annotation: Option<Box<Expr>>,
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

pub type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,
    Custom,  // . & |>...
    Or,      // ||
    And,     // &
    Equal,   // == !=
    Compare, // < <= > >=
    Sum,     // + -
    Product, // * / %
    Mapping, // ->
    Prefix,  // -X !X
    Call,    // myFunction(x)
    Index,   // array[index]
    Highest, // (x)
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
            Token::Dot => Self::Custom,
            Token::Arm => Self::Custom,
            _ => Self::Lowest,
        }
    }
}

pub type Program = Vec<Stmt>;

#[derive(PartialEq, Clone, Debug)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Box<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Pattern {
    Ident(String),
    Constructor { name: String, args: Vec<Pattern> },
    Literal(Token),
}

#[derive(PartialEq, Clone, Debug)]
pub struct TypeVariant {
    pub name: String,
    pub fields: TypeVariantFields,
}

#[derive(PartialEq, Clone, Debug)]
pub enum TypeVariantFields {
    Tuple(Vec<Box<Expr>>),
    Record(Vec<(String, Box<Expr>)>),
    Unit,
}
