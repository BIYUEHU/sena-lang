use std::fmt::{self, Display, Formatter};

use crate::ast::{
    Expr, Literal, MatchCase, Pattern, Precedence, Stmt, TypeVariant, TypeVariantFields,
};
use crate::token::{Token, TokenData};
use crate::utils::vec_boxify;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: Token,
        line: usize,
        column: usize,
    },
    EndOfInput {
        expected: String,
    },
    InvalidSyntax {
        message: String,
        line: usize,
        column: usize,
    },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                line,
                column,
            } => write!(
                f,
                "Unexpected token: expect {}, found '{}' at line {}, column {}",
                expected, found, line, column
            ),
            ParseError::EndOfInput { expected } => {
                write!(f, "Unexpected end of input: expect {}", expected)
            }
            ParseError::InvalidSyntax {
                message,
                line,
                column,
            } => write!(
                f,
                "Syntax error: {} at line {}, column {}",
                message, line, column
            ),
        }
    }
}

#[derive(Clone)]
pub struct Parser {
    token_data: Vec<TokenData>,
    pos: usize,
    current_token: Token,
    next_token: Token,
    line: usize,
    column: usize,
    top_expression: bool,
}

impl Parser {
    pub fn new(token_data: Vec<TokenData>, top_expression: bool) -> Self {
        let mut parser = Parser {
            token_data,
            pos: 0,
            current_token: Token::Eof,
            next_token: Token::Eof,
            line: 0,
            column: 0,
            top_expression,
        };
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = match self.token_data.get(self.pos) {
            Some(TokenData {
                token,
                line,
                column,
            }) => {
                self.line = *line;
                self.column = *column;
                token.clone()
            }
            None => Token::Eof,
        };
        self.pos += 1;
    }

    fn next_token_is(&mut self, token: &Token) -> bool {
        self.next_token == *token
    }

    fn next_token_consume<T: Into<String>>(
        &mut self,
        token: &Token,
        message: T,
    ) -> Result<(), ParseError> {
        if self.next_token_is(token) {
            self.next_token();
            Ok(())
        } else {
            Err(self.error_unexpected_token(message))
        }
    }

    fn save_position(&self) -> (usize, Token, Token) {
        (
            self.pos,
            self.current_token.clone(),
            self.next_token.clone(),
        )
    }

    fn restore_position(&mut self, pos: (usize, Token, Token)) {
        let (pos, current, next) = pos;

        self.pos = pos;
        self.current_token = current;
        self.next_token = next;
    }

    fn error_unexpected_token<T: Into<String>>(&mut self, message: T) -> ParseError {
        ParseError::UnexpectedToken {
            expected: message.into(),
            found: self.current_token.clone(),
            line: self.line,
            column: self.column,
        }
    }

    fn error_invalid_syntax<T: Into<String>>(&mut self, message: T) -> ParseError {
        ParseError::InvalidSyntax {
            message: message.into(),
            line: self.line,
            column: self.column,
        }
    }

    fn parse(&mut self) -> Option<Result<Stmt, ParseError>> {
        self.next_token();
        Some(match self.current_token {
            Token::Import => self.import_declaration(),
            Token::Export => self.export_declaration(),
            Token::Let => self.let_declaration(),
            Token::Type => self.type_declaration(),
            Token::Eof => return None,
            Token::LineComment(_) | Token::BlockComment(_) => {
                return self.parse();
            }
            _ => {
                if self.top_expression {
                    self.expression(Precedence::Lowest)
                        .map(|expr| Stmt::Expr(expr))
                } else {
                    Err(self.error_invalid_syntax("expect a declaration"))
                }
            }
        })
    }

    fn import_declaration(&mut self) -> Result<Stmt, ParseError> {
        let mut alias = None;
        let mut items = vec![];

        self.next_token();
        match self.current_token {
            Token::As => {
                self.next_token();
                alias = Some(
                    match &self.current_token {
                        Token::Ident(name) => name,
                        _ => return Err(self.error_unexpected_token("identifier after 'as'")),
                    }
                    .clone(),
                )
            }
            Token::LeftBrace => {
                items = self.identifier_list(Token::RightBrace)?;
                if items.is_empty() {
                    return Err(self.error_invalid_syntax("import items couldn't be empty"));
                }
            }
            _ => return Err(self.error_invalid_syntax("expect all or part importing")),
        }

        self.next_token_consume(&Token::From, "'from' after import declaration")?;
        self.next_token();
        let source = match &self.current_token {
            Token::String(name) => name,
            _ => return Err(self.error_unexpected_token("string literal after 'from'")),
        }
        .clone();

        if source.is_empty() {
            return Err(self.error_invalid_syntax("import source couldn't be empty"));
        }

        Ok(match alias {
            Some(alias) => Stmt::ImportAll { source, alias },
            None => Stmt::ImportSome { source, items },
        })
    }

    fn export_declaration(&mut self) -> Result<Stmt, ParseError> {
        let only_abstract = self.next_token_is(&Token::Abstract);
        if only_abstract {
            self.next_token();
        }

        let declaration = match self.parse() {
            Some(Ok(stmt)) if matches!(stmt, Stmt::Let { .. }) => {
                if only_abstract {
                    return Err(self
                        .error_invalid_syntax("'abstract' only appears before type declaration"));
                } else {
                    stmt
                }
            }
            Some(Ok(stmt)) if matches!(stmt, Stmt::Type { .. }) => stmt,
            Some(Err(err)) => return Err(err),
            _ => return Err(self.error_invalid_syntax("expect a let or type declaration")),
        };

        Ok(Stmt::Export {
            only_abstract,
            body: Box::new(declaration),
        })
    }

    fn let_declaration(&mut self) -> Result<Stmt, ParseError> {
        self.next_token();
        let name = match &self.current_token {
            Token::Ident(name) => name,
            _ => return Err(self.error_unexpected_token("identifier")),
        }
        .clone();

        let type_annotation = if self.next_token_is(&Token::Colon) {
            self.next_token();
            self.next_token();
            Some(Box::new(self.expression(Precedence::Lowest)?))
        } else {
            None
        };

        self.next_token_consume(&Token::Assign, "'=' after variable name")?;
        self.next_token();
        let value = Box::new(self.expression(Precedence::Lowest)?);

        // TODO: Let-In expression

        Ok(Stmt::Let {
            name,
            type_annotation,
            value,
        })
    }

    fn type_declaration(&mut self) -> Result<Stmt, ParseError> {
        self.next_token();
        let name = match &self.current_token {
            Token::Ident(name) => name.clone(),
            _ => return Err(self.error_unexpected_token("identifier")),
        };

        let type_annotation = if self.next_token_is(&Token::Colon) {
            self.next_token();
            self.next_token();
            Some(Box::new(self.expression(Precedence::Lowest)?))
        } else {
            None
        };

        self.next_token_consume(&Token::Assign, "'=' after type name")?;
        let type_params = if self.next_token_is(&Token::Less) {
            self.next_token();
            let params = self.identifier_list(Token::Greater)?;
            if params.is_empty() {
                return Err(self.error_invalid_syntax("type parameters couldn't be empty"));
            }
            Some(params)
        } else {
            None
        };

        let variants = self.type_variants()?;

        Ok(Stmt::Type {
            name,
            type_annotation,
            type_params,
            variants,
        })
    }

    fn type_variants(&mut self) -> Result<Vec<TypeVariant>, ParseError> {
        let mut variants = vec![];

        loop {
            let name = match &self.next_token {
                Token::Ident(name) => name.clone(),
                _ => return Err(self.error_unexpected_token("identifier")),
            };
            self.next_token();

            let fields = if self.next_token_is(&Token::LeftParen) {
                self.next_token();
                let exprs = vec_boxify(self.expression_list(Token::RightParen)?);
                if exprs.is_empty() {
                    return Err(self.error_invalid_syntax("tuple type requires at least one field"));
                }
                TypeVariantFields::Tuple(exprs)
            }
            // TODO: record
            /*  else if self.next_token_is(&Token::LeftBrace) {
                self.next_token();
                let mut fields = vec![];
                if !self.next_token_is(&Token::RightBrace) {
                    loop {
                        let field_name = match &self.next_token {
                            Token::Ident(name) => name.clone(),
                            _ => return Err(self.error_unexpected_token("identifier")),
                        };
                        self.next_token();
                        self.next_token_consume(&Token::Colon, "':' after field name")?;
                        let field_type = Box::new(self.expression(Precedence::Lowest)?);
                        fields.push((field_name, field_type));
                        if self.next_token_is(&Token::Comma) {
                            self.next_token();
                        } else {
                            break;
                        }
                    }
                }
                self.fun_name()?;
                if fields.is_empty() {
                    return Err(
                        self.error_invalid_syntax("record type requires at least one field")
                    );
                }
                TypeVariantFields::Record(fields)
            } */
            else {
                TypeVariantFields::Unit
            };

            variants.push(TypeVariant { name, fields });

            if !self.next_token_is(&Token::Arm) {
                break;
            }
            self.next_token();
        }

        Ok(variants)
    }

    fn expression(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        let mut left = match &self.current_token {
            Token::Ident(name) => match name.as_str() {
                "true" => Expr::Literal(Literal::Bool(true)),
                "false" => Expr::Literal(Literal::Bool(false)),
                _ => Expr::Ident(name.clone()),
            },
            Token::Int(value) => Expr::Literal(Literal::Int(*value)),
            Token::Float(value) => Expr::Literal(Literal::Float(*value)),
            Token::String(value) => Expr::Literal(Literal::String(value.clone())),
            Token::Char(value) => Expr::Literal(Literal::Char(*value)),
            Token::Sub | Token::Not => self.prefix()?,
            Token::LeftParen => {
                let save_pos = self.save_position();
                match self.try_lambda() {
                    Ok(lambda) => lambda,
                    Err(err) => {
                        println!("lambda error: {}", err);
                        self.restore_position(save_pos);
                        self.grouped_expression()?
                    }
                }
            }
            Token::If => self.if_expression()?,
            Token::Match => self.match_expression()?,
            Token::LeftBrace => self.block_expression()?,
            _ => return Err(self.error_unexpected_token("expression")),
        };

        while !self.next_token_is(&Token::Eof)
            && precedence < Precedence::from_token(&self.next_token)
        {
            match self.next_token {
                Token::Plus
                | Token::Sub
                | Token::Mul
                | Token::Div
                | Token::Mod
                | Token::Equal
                | Token::NotEqual
                | Token::Greater
                | Token::GreaterEqual
                | Token::Less
                | Token::LessEqual
                | Token::And
                | Token::Or
                | Token::ThinArrow => {
                    self.next_token();
                    left = self.infix(left)?;
                }
                Token::LeftParen => {
                    self.next_token();
                    left = self.call(left)?;
                }
                _ => return Ok(left),
            }
        }

        Ok(left)
    }

    fn prefix(&mut self) -> Result<Expr, ParseError> {
        let prefix = match self.current_token {
            Token::Sub => Token::Sub,
            Token::Not => Token::Not,
            _ => return Err(self.error_unexpected_token("prefix operator")),
        };

        self.next_token();
        Ok(Expr::Prefix(
            prefix,
            Box::new(self.expression(Precedence::Prefix)?),
        ))
    }

    fn infix(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let infix = match self.current_token {
            Token::Plus => Token::Plus,
            Token::Sub => Token::Sub,
            Token::Mul => Token::Mul,
            Token::Div => Token::Div,
            Token::Mod => Token::Mod,
            Token::Equal => Token::Equal,
            Token::NotEqual => Token::NotEqual,
            Token::Greater => Token::Greater,
            Token::GreaterEqual => Token::GreaterEqual,
            Token::Less => Token::Less,
            Token::LessEqual => Token::LessEqual,
            Token::And => Token::And,
            Token::Or => Token::Or,
            Token::ThinArrow => Token::ThinArrow,
            _ => return Err(self.error_unexpected_token("infix operator")),
        };

        let precedence = Precedence::from_token(&self.current_token);
        self.next_token();
        Ok(Expr::Infix(
            infix,
            Box::new(left),
            Box::new(self.expression(precedence)?),
        ))
    }

    fn grouped_expression(&mut self) -> Result<Expr, ParseError> {
        self.next_token();
        let expr = self.expression(Precedence::Lowest)?;
        self.next_token_consume(&Token::RightParen, "')' after expression")?;
        Ok(expr)
    }

    // fn lambda(&mut self) {}

    fn call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        Ok(Expr::Call {
            callee: Box::new(callee),
            arguments: self.expression_list(Token::RightParen)?,
        })
    }

    fn items_list<T, F: FnMut(&mut Self) -> Result<T, ParseError>>(
        &mut self,
        end: Token,
        mut handler: F,
    ) -> Result<Vec<T>, ParseError> {
        let mut list = vec![];
        if self.next_token_is(&end) {
            self.next_token();
            return Ok(list);
        }

        self.next_token();
        let item = handler(self)?;
        list.push(item);

        while self.next_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            list.push(handler(self)?);
        }

        self.next_token_consume(&end, &end.to_string())?;
        Ok(list)
    }

    fn expression_list(&mut self, end: Token) -> Result<Vec<Expr>, ParseError> {
        self.items_list(end, |self2| self2.expression(Precedence::Lowest))
    }

    fn identifier_list(&mut self, end: Token) -> Result<Vec<String>, ParseError> {
        self.items_list(end, |self2| match &self2.current_token {
            Token::Ident(name) => Ok(name.clone()),
            _ => Err(self2.error_unexpected_token("identifier")),
        })
    }

    fn lambda_param_list(&mut self) -> Result<Vec<(String, Option<Box<Expr>>)>, ParseError> {
        let mut params = vec![];
        self.next_token(); // Consume '('

        if self.current_token == Token::RightParen {
            self.next_token(); // Consume ')'
            return Ok(params); // Allow empty parameter list for flexibility
        }

        loop {
            // Expect an identifier for the parameter name
            let name = match &self.current_token {
                Token::Ident(name) => name.clone(),
                _ => return Err(self.error_unexpected_token("identifier")),
            };
            self.next_token();

            // Check for optional type annotation
            let type_annotation = if self.current_token == Token::Colon {
                self.next_token();
                let type_expr = self.expression(Precedence::Lowest)?;
                self.next_token();
                Some(Box::new(type_expr))
            } else {
                None
            };

            params.push((name, type_annotation));

            // Check for comma or end of parameter list
            if self.current_token == Token::Comma {
                self.next_token();
            } else if self.current_token == Token::RightParen {
                self.next_token(); // Consume ')'
                break;
            } else {
                return Err(self.error_unexpected_token("',' or ')'"));
            }
        }

        Ok(params)
    }

    fn try_lambda(&mut self) -> Result<Expr, ParseError> {
        let params = match self.lambda_param_list() {
            Ok(params) => params,
            Err(e) => {
                return Err(e);
            }
        };

        // Check for optional return type annotation (e.g., `: Int`)
        let return_type = if self.current_token == Token::Colon {
            self.next_token();
            let type_expr = self.expression(Precedence::Lowest)?;
            self.next_token();
            Some(Box::new(type_expr))
        } else {
            None
        };

        // Expect '=>'
        if self.current_token != Token::Arrow {
            return Err(self.error_unexpected_token("'=>'"));
        }
        self.next_token(); // Consume '=>'

        // Parse the Lambda body
        let body = Box::new(self.expression(Precedence::Lowest)?);

        Ok(Expr::Lambda {
            type_params: vec![], // Assuming no generic type parameters for now
            params,
            body,
            // Assuming Expr::Lambda has a field for return type; adjust based on actual AST
            // If not, this may need to be handled differently
            return_type,
        })
    }

    fn if_expression(&mut self) -> Result<Expr, ParseError> {
        self.next_token();
        let condition = Box::new(self.expression(Precedence::Lowest)?);
        self.next_token_consume(&Token::Then, "'then' after condition")?;
        self.next_token();
        let then_branch = Box::new(self.expression(Precedence::Lowest)?);
        self.next_token_consume(&Token::Else, "'else' after then branch")?;
        self.next_token();
        let else_branch = Box::new(self.expression(Precedence::Lowest)?);
        Ok(Expr::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn match_expression(&mut self) -> Result<Expr, ParseError> {
        self.next_token();
        let expr = Box::new(self.expression(Precedence::Lowest)?);
        self.next_token_consume(&Token::Then, "'then' after match expression")?;
        let mut cases = vec![];
        while self.next_token_is(&Token::Arm) {
            self.next_token();
            let pattern = self.pattern()?;
            self.next_token_consume(&Token::Arrow, "'=>' after pattern")?;
            let body = Box::new(self.expression(Precedence::Lowest)?);
            cases.push(MatchCase { pattern, body });
        }
        if cases.is_empty() {
            return Err(self.error_invalid_syntax("match expression requires at least one case"));
        }
        Ok(Expr::Match { expr, cases })
    }

    fn pattern(&mut self) -> Result<Pattern, ParseError> {
        match &self.next_token {
            Token::Ident(name) => {
                let name = name.clone();
                self.next_token();
                if self.next_token_is(&Token::LeftParen) {
                    self.next_token();
                    let mut args = vec![];
                    if !self.next_token_is(&Token::RightParen) {
                        loop {
                            args.push(self.pattern()?);
                            if self.next_token_is(&Token::Comma) {
                                self.next_token();
                            } else {
                                break;
                            }
                        }
                    }
                    self.next_token_consume(&Token::RightParen, "')' after patterns")?;
                    Ok(Pattern::Constructor { name, args })
                } else {
                    Ok(Pattern::Ident(name))
                }
            }
            Token::Int(value) => {
                let value = Token::Int(*value);
                self.next_token();
                Ok(Pattern::Literal(value))
            }
            _ => Err(self.error_unexpected_token("pattern")),
        }
    }

    fn block_expression(&mut self) -> Result<Expr, ParseError> {
        let statements =
            self.items_list(Token::RightBrace, |self2| match &self2.current_token {
                Token::Let => self2.let_declaration(),
                Token::Type => self2.type_declaration(),
                _ => self2
                    .expression(Precedence::Lowest)
                    .map(|expr| Stmt::Expr(expr)),
            })?;
        if statements.is_empty() || !matches!(statements.last(), Some(Stmt::Expr(_))) {
            return Err(self.error_invalid_syntax("block must end with an expression"));
        }
        Ok(Expr::Block(statements))
    }
}

impl Iterator for Parser {
    type Item = Result<Stmt, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    fn parse(input: &str) -> Vec<Stmt> {
        Parser::new(
            Lexer::new(input).filter_map(|result| result.ok()).collect(),
            false,
        )
        .filter_map(|result| result.ok())
        .collect()
    }

    fn expr(input: &str, expected: Expr) {
        assert_eq!(
            parse(format!("let _ = {}", input).as_str()),
            vec![Stmt::Let {
                name: "_".to_string(),
                type_annotation: None,
                value: Box::new(expected),
            }]
        );
    }

    #[test]
    fn test_let_declaration() {
        assert_eq!(
            parse("let x = 1"),
            vec![Stmt::Let {
                name: "x".to_string(),
                type_annotation: None,
                value: Box::new(Expr::Literal(Literal::Int(1))),
            }]
        );
        assert_eq!(
            parse("let x: int = 1"),
            vec![Stmt::Let {
                name: "x".to_string(),
                type_annotation: Some(Box::new(Expr::Ident("int".to_string()))),
                value: Box::new(Expr::Literal(Literal::Int(1))),
            }]
        );
    }

    #[test]
    fn test_type_declaration() {
        assert_eq!(
            parse("type List = Nil | Cons(int, List)"),
            vec![Stmt::Type {
                name: "List".to_string(),
                type_annotation: None,
                type_params: None,
                variants: vec![
                    TypeVariant {
                        name: "Nil".to_string(),
                        fields: TypeVariantFields::Unit,
                    },
                    TypeVariant {
                        name: "Cons".to_string(),
                        fields: TypeVariantFields::Tuple(vec![
                            Box::new(Expr::Ident("int".to_string())),
                            Box::new(Expr::Ident("List".to_string())),
                        ]),
                    },
                ],
            }]
        );

        assert_eq!(
            parse("type List = <T> Nil | Cons(T, List(T))"),
            vec![Stmt::Type {
                name: "List".to_string(),
                type_annotation: None,
                type_params: Some(vec!["T".to_string()]),
                variants: vec![
                    TypeVariant {
                        name: "Nil".to_string(),
                        fields: TypeVariantFields::Unit,
                    },
                    TypeVariant {
                        name: "Cons".to_string(),
                        fields: TypeVariantFields::Tuple(vec![
                            Box::new(Expr::Ident("T".to_string())),
                            Box::new(Expr::Call {
                                callee: Box::new(Expr::Ident("List".to_string())),
                                arguments: vec![Expr::Ident("T".to_string())],
                            }),
                        ]),
                    },
                ],
            }]
        );

        assert_eq!(
            parse("type Color: Kind = Red | Green | Blue"),
            vec![Stmt::Type {
                name: "Color".to_string(),
                type_annotation: Some(Box::new(Expr::Ident("Kind".to_string()))),
                type_params: None,
                variants: vec![
                    TypeVariant {
                        name: "Red".to_string(),
                        fields: TypeVariantFields::Unit,
                    },
                    TypeVariant {
                        name: "Green".to_string(),
                        fields: TypeVariantFields::Unit,
                    },
                    TypeVariant {
                        name: "Blue".to_string(),
                        fields: TypeVariantFields::Unit,
                    },
                ],
            }]
        );
    }

    #[test]
    fn test_import_declaration() {
        assert_eq!(
            parse("import as F from \"foo\""),
            vec![Stmt::ImportAll {
                source: "foo".to_string(),
                alias: "F".to_string()
            }]
        );

        assert_eq!(
            parse("import {x, y} from \"foo\""),
            vec![Stmt::ImportSome {
                source: "foo".to_string(),
                items: vec!["x".to_string(), "y".to_string()]
            }]
        );
    }

    #[test]
    fn test_export_declaration() {
        assert_eq!(
            parse("export let x = 1"),
            vec![Stmt::Export {
                only_abstract: false,
                body: Box::new(Stmt::Let {
                    name: "x".to_string(),
                    type_annotation: None,
                    value: Box::new(Expr::Literal(Literal::Int(1))),
                })
            }]
        );

        assert_eq!(
            parse("export abstract type Nat = Zero | Succ(Nat)"),
            vec![Stmt::Export {
                only_abstract: true,
                body: Box::new(Stmt::Type {
                    name: "Nat".to_string(),
                    type_annotation: None,
                    type_params: None,
                    variants: vec![
                        TypeVariant {
                            name: "Zero".to_string(),
                            fields: TypeVariantFields::Unit,
                        },
                        TypeVariant {
                            name: "Succ".to_string(),
                            fields: TypeVariantFields::Tuple(vec![Box::new(Expr::Ident(
                                "Nat".to_string()
                            ))]),
                        }
                    ],
                })
            }]
        );
    }

    #[test]
    fn test_lambda_expressions() {
        // Simple Lambda
        expr(
            "(x, y) => x + y",
            Expr::Lambda {
                type_params: vec![],
                params: vec![("x".to_string(), None), ("y".to_string(), None)],
                body: Box::new(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Ident("y".to_string())),
                )),
                return_type: None,
            },
        );

        // Lambda with parameter types
        expr(
            "(x: Int, y: Int) => x + y",
            Expr::Lambda {
                type_params: vec![],
                params: vec![
                    (
                        "x".to_string(),
                        Some(Box::new(Expr::Ident("Int".to_string()))),
                    ),
                    (
                        "y".to_string(),
                        Some(Box::new(Expr::Ident("Int".to_string()))),
                    ),
                ],
                body: Box::new(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Ident("y".to_string())),
                )),
                return_type: None,
            },
        );

        // Lambda with return type
        expr(
            "(x, y): Int => x + y",
            Expr::Lambda {
                type_params: vec![],
                params: vec![("x".to_string(), None), ("y".to_string(), None)],
                body: Box::new(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Ident("y".to_string())),
                )),
                return_type: Some(Box::new(Expr::Ident("Int".to_string()))),
            },
        );

        // Single parameter Lambda
        expr(
            "(x) => x",
            Expr::Lambda {
                type_params: vec![],
                params: vec![("x".to_string(), None)],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            },
        );
    }

    #[test]
    fn test_expression() {
        expr("1", Expr::Literal(Literal::Int(1)));
        expr(
            "1 + 2",
            Expr::Infix(
                Token::Plus,
                Box::new(Expr::Literal(Literal::Int(1))),
                Box::new(Expr::Literal(Literal::Int(2))),
            ),
        );
        expr(
            "1 + 2 * 3",
            Expr::Infix(
                Token::Plus,
                Box::new(Expr::Literal(Literal::Int(1))),
                Box::new(Expr::Infix(
                    Token::Mul,
                    Box::new(Expr::Literal(Literal::Int(2))),
                    Box::new(Expr::Literal(Literal::Int(3))),
                )),
            ),
        );
        expr(
            "1 + 2 * 3 - 4 / 5",
            Expr::Infix(
                Token::Sub,
                Box::new(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Literal(Literal::Int(1))),
                    Box::new(Expr::Infix(
                        Token::Mul,
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Box::new(Expr::Literal(Literal::Int(3))),
                    )),
                )),
                Box::new(Expr::Infix(
                    Token::Div,
                    Box::new(Expr::Literal(Literal::Int(4))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
        );
        expr(
            "a -> (a -> b) -> b",
            Expr::Infix(
                Token::ThinArrow,
                Box::new(Expr::Infix(
                    Token::ThinArrow,
                    Box::new(Expr::Ident("a".to_string())),
                    Box::new(Expr::Infix(
                        Token::ThinArrow,
                        Box::new(Expr::Ident("a".to_string())),
                        Box::new(Expr::Ident("b".to_string())),
                    )),
                )),
                Box::new(Expr::Ident("b".to_string())),
            ),
        );
        expr(
            "!a",
            Expr::Prefix(Token::Not, Box::new(Expr::Ident("a".to_string()))),
        );
        expr(
            "a()",
            Expr::Call {
                callee: Box::new(Expr::Ident("a".to_string())),
                arguments: vec![],
            },
        );
        expr(
            "a(1, 2, 3)",
            Expr::Call {
                callee: Box::new(Expr::Ident("a".to_string())),
                arguments: vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                    Expr::Literal(Literal::Int(3)),
                ],
            },
        );
        // expr("a.b", Expr::Field {
        //     object: Box::new(Expr::Ident("a".to_string())),
        //     field: "b".to_string()
        // });
        // expr("a[1]", Expr::Index {
        //     array: Box::new(Expr::Ident("a".to_string())),
        //     index: Box::new(Expr::Literal(Literal::Int(1)))
        // });
        expr(
            "if true then 1 else 2",
            Expr::If {
                condition: Box::new(Expr::Literal(Literal::Bool(true))),
                then_branch: Box::new(Expr::Literal(Literal::Int(1))),
                else_branch: Box::new(Expr::Literal(Literal::Int(2))),
            },
        );
        // expr(
        //     "match x with | 0 => 1 | _ => 2",
        //     Expr::Match {
        //         expr: Box::new(Expr::Ident("x".to_string())),
        //         cases: vec![
        //             MatchCase {
        //                 pattern: Pattern::Literal(Token::Int(0)),
        //                 body: Box::new(Expr::Literal(Literal::Int(1))),
        //             },
        //             MatchCase {
        //                 pattern: Pattern::Ident("_".to_string()),
        //                 body: Box::new(Expr::Literal(Literal::Int(2))),
        //             },
        //         ],
        //     },
        // );
    }
}
