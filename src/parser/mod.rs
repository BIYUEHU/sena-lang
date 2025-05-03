use crate::{
    lexer::token::{Token, TokenData},
    utils::get_arrow_type,
};
use ast::{
    Expr, Kind, Literal, MatchCase, Pattern, Stmt, TypeExpr, TypeVariant, TypeVariantFields,
};
use error::ParseError;
use precedence::Precedence;

pub mod ast;
pub mod error;
pub mod precedence;

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
            Token::Let => {
                let save_pos = self.save_position();
                match self.let_declaration() {
                    Ok(stmt) => Ok(stmt),
                    Err(ParseError::InvalidSyntax { .. }) => {
                        self.restore_position(save_pos);
                        self.try_top_expression()
                    }
                    Err(err) => Err(err),
                }
            }
            Token::Type => self.type_declaration(),
            Token::Eof => return None,
            Token::LineComment(_) | Token::BlockComment(_) => {
                return self.parse();
            }
            _ => self.try_top_expression(),
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

    fn collect_bindings(&mut self) -> Result<(String, TypeExpr), ParseError> {
        self.next_token();
        let name = match &self.current_token {
            Token::Ident(name) => name,
            _ => return Err(self.error_unexpected_token("identifier")),
        }
        .clone();

        let type_annotation = if self.next_token_is(&Token::Colon) {
            self.next_token();
            self.next_token();
            self.type_expression()?
        } else {
            TypeExpr::default()
        };

        self.next_token_consume(&Token::Assign, "'=' after identifier name")?;
        Ok((name, type_annotation))
    }

    fn let_declaration(&mut self) -> Result<Stmt, ParseError> {
        let (name, type_annotation) = self.collect_bindings()?;
        self.next_token();
        let value = Box::new(self.expression(Precedence::Lowest)?);
        if self.next_token_is(&Token::In) {
            return Err(self.error_invalid_syntax("let declaration couldn't have 'in' clause"));
        }

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

        let mut kind_annotation = if self.next_token_is(&Token::Colon) {
            self.next_token();
            self.next_token();
            self.type_expression()?
        } else {
            TypeExpr::Con("Unknown".to_string())
        };

        self.next_token_consume(&Token::Assign, "'=' after type name")?;
        let params = if self.next_token_is(&Token::Less) {
            self.next_token();
            let params = self.identifier_list(Token::Greater)?;
            if params.is_empty() {
                return Err(self.error_invalid_syntax("type parameters couldn't be empty"));
            }
            params
        } else {
            vec![]
        };

        if kind_annotation == TypeExpr::Con("Unknown".to_string()) {
            kind_annotation = if params.is_empty() {
                TypeExpr::Kind(Kind::Star)
            } else {
                get_arrow_type(
                    params.iter().map(|_| TypeExpr::Kind(Kind::Star)).collect(),
                    TypeExpr::Kind(Kind::Star),
                )
            };
        }

        let variants = self.type_variants()?;

        // TODO use lambda typeexpr to explain type annotations
        Ok(Stmt::Type {
            name: name.clone(),
            params,
            kind_annotation,
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
                let exprs = self.items_list(Token::RightParen, |self2| self2.type_expression())?;
                if exprs.is_empty() {
                    return Err(self.error_invalid_syntax("tuple type requires at least one field"));
                }
                TypeVariantFields::Tuple(exprs)
            }
            // TODO: record type
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

    fn try_top_expression(&mut self) -> Result<Stmt, ParseError> {
        if self.top_expression {
            self.expression(Precedence::Lowest)
                .map(|expr| Stmt::Expr(expr))
        } else {
            Err(self.error_invalid_syntax("expect a declaration"))
        }
    }

    fn expression(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        let mut left = match &self.current_token {
            Token::Let => self.let_in_expression()?,
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
            Token::If => self.if_expression()?,
            Token::Match => self.match_expression()?,
            // Token::With => self.with_expression()?,
            Token::LeftParen => {
                let save_pos = self.save_position();
                match self.try_function() {
                    Ok(lambda) => lambda,
                    _ => {
                        self.restore_position(save_pos);
                        self.grouped_expression()?
                    }
                }
            }
            Token::LeftBracket => self.array_expression()?,
            Token::LeftBrace => self.block_expression()?,
            _ => return Err(self.error_unexpected_token("expression")),
        };

        while !self.next_token_is(&Token::Eof)
            && precedence <= Precedence::from_token(&self.next_token)
        {
            match self.next_token.clone() {
                Token::Plus
                | Token::Sub
                | Token::Mul
                | Token::Div
                | Token::Pow
                | Token::Mod
                | Token::Equal
                | Token::NotEqual
                | Token::Greater
                | Token::GreaterEqual
                | Token::Less
                | Token::LessEqual
                | Token::Dollar
                | Token::And
                | Token::Or
                | Token::ThinArrow
                | Token::Dot
                | Token::Colon
                | Token::InfixFixity(_)
                | Token::InfixIdent(_) => {
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

    fn type_expression(&mut self) -> Result<TypeExpr, ParseError> {
        TypeExpr::try_from(self.expression(Precedence::Lowest)?)
            .map_err(|_| self.error_invalid_syntax("expect a type expression"))
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
        let infix = self.current_token.clone();

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
        if self.current_token == Token::RightParen {
            Ok(Expr::Literal(Literal::Unit))
        } else {
            let expr = self.expression(Precedence::Lowest)?;
            self.next_token_consume(&Token::RightParen, "')' after expression")?;
            Ok(expr)
        }
    }

    fn call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        Ok(Expr::Call {
            callee: Box::new(callee),
            params: self.items_list(Token::RightParen, |self2| {
                self2.expression(Precedence::Lowest)
            })?,
        })
    }

    fn array_expression(&mut self) -> Result<Expr, ParseError> {
        Ok(Expr::Literal(Literal::Array(
            self.items_list(Token::RightBracket, |self2| {
                self2.expression(Precedence::Lowest)
            })?,
        )))
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

    fn identifier_list(&mut self, end: Token) -> Result<Vec<String>, ParseError> {
        self.items_list(end, |self2| match &self2.current_token {
            Token::Ident(name) => Ok(name.clone()),
            _ => Err(self2.error_unexpected_token("identifier")),
        })
    }

    fn try_function(&mut self) -> Result<Expr, ParseError> {
        let params = {
            let mut params = vec![];
            self.next_token(); // Consume '('

            if self.current_token == Token::RightParen {
                self.next_token(); // Consume ')'
                params // Allow empty parameter list for flexibility
            } else {
                loop {
                    // Expect an identifier for the parameter name
                    let name = match &self.current_token {
                        Token::Ident(name) => name.clone(),
                        _ => return Err(self.error_unexpected_token("identifier")),
                    };
                    self.next_token();

                    // Check for optional type annotation
                    let type_annotation = Box::new(if self.current_token == Token::Colon {
                        self.next_token();
                        let type_expr = self.type_expression()?;
                        self.next_token();
                        type_expr
                    } else {
                        TypeExpr::default()
                    });

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
                params
            }
        };

        // TODO: move to typechecker phase
        if params.is_empty() {
            return Err(self.error_invalid_syntax("function requires at least one parameter"));
        }

        // Check for optional return type annotation (e.g., `: Int`)
        let return_type = if self.current_token == Token::Colon {
            self.next_token();
            let type_expr = self.type_expression()?;
            self.next_token();
            type_expr
        } else {
            TypeExpr::default()
        };

        if self.current_token != Token::Arrow {
            return Err(self.error_unexpected_token("'=>'"));
        }
        self.next_token();

        let body = Box::new(self.expression(Precedence::Lowest)?);
        /*
        [Int, String, Int]

        Int -> String -> Int
        -> (Int, -> (String -> Int))
        */
        Ok(Expr::Function {
            params: params
                .clone()
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            body,
            type_annotation: get_arrow_type(
                params
                    .into_iter()
                    .map(|(_, type_annotation)| *type_annotation)
                    .collect(),
                return_type,
            ),
        })
    }

    fn let_in_expression(&mut self) -> Result<Expr, ParseError> {
        let (name, type_annotation) = self.collect_bindings()?;
        self.next_token();
        let value = Box::new(self.expression(Precedence::Lowest)?);
        self.next_token_consume(&Token::In, "'in' after let binding")?;
        self.next_token();
        let body = Box::new(self.expression(Precedence::Lowest)?);
        Ok(Expr::LetIn {
            name,
            type_annotation,
            value,
            body,
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
            let guard = if self.next_token_is(&Token::If) {
                self.next_token();
                self.next_token();
                self.expression(Precedence::Lowest)?
            } else {
                Expr::Literal(Literal::Bool(true))
            };
            self.next_token_consume(&Token::Arrow, "'=>' after pattern")?;
            self.next_token();
            cases.push(MatchCase {
                pattern,
                body: self.expression(Precedence::Lowest)?,
                guard,
            });
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
                    Ok(Pattern::ADTConstructor { name, args })
                } else {
                    match name.as_str() {
                        "True" => Ok(Pattern::Literal(Literal::Bool(true))),
                        "False" => Ok(Pattern::Literal(Literal::Bool(false))),
                        _ => Ok(Pattern::Ident(name)),
                    }
                }
            }
            Token::Int(value) => {
                let value = Literal::Int(*value);
                self.next_token();
                Ok(Pattern::Literal(value))
            }
            Token::Float(value) => {
                let value = Literal::Float(*value);
                self.next_token();
                Ok(Pattern::Literal(value))
            }
            Token::String(value) => {
                let value = Literal::String(value.clone());
                self.next_token();
                Ok(Pattern::Literal(value))
            }
            Token::Char(value) => {
                let value = Literal::Char(*value);
                self.next_token();
                Ok(Pattern::Literal(value))
            }
            _ => Err(self.error_unexpected_token("pattern")),
        }
    }

    fn block_expression(&mut self) -> Result<Expr, ParseError> {
        let mut statements = vec![];

        loop {
            if self.next_token_is(&Token::RightBrace) {
                self.next_token();
                break;
            }

            self.next_token();
            statements.push(match &self.current_token {
                Token::Let => {
                    let save_pos = self.save_position();
                    match self.let_declaration() {
                        Ok(stmt) => stmt,
                        Err(ParseError::InvalidSyntax { .. }) => {
                            self.restore_position(save_pos);
                            Stmt::Expr(self.expression(Precedence::Lowest)?)
                        }
                        Err(err) => return Err(err),
                    }
                }
                Token::Type => self.type_declaration()?,
                _ => Stmt::Expr(self.expression(Precedence::Lowest)?),
            });
        }

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
            true,
        )
        .filter_map(|result| {
            result
                .map_err(|error| {
                    eprintln!("Error: {}", error);
                })
                .ok()
        })
        .collect()
    }

    #[test]
    fn test_let_declaration() {
        assert_eq!(
            parse("let x = 1"),
            vec![Stmt::Let {
                name: "x".to_string(),
                type_annotation: TypeExpr::default(),
                value: Box::new(Expr::Literal(Literal::Int(1))),
            }]
        );
        assert_eq!(
            parse("let x: int = 1"),
            vec![Stmt::Let {
                name: "x".to_string(),
                type_annotation: TypeExpr::Con("int".to_string()),
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
                params: vec![],
                kind_annotation: TypeExpr::Kind(Kind::Star),
                variants: vec![
                    TypeVariant {
                        name: "Nil".to_string(),
                        fields: TypeVariantFields::Unit,
                    },
                    TypeVariant {
                        name: "Cons".to_string(),
                        fields: TypeVariantFields::Tuple(vec![
                            TypeExpr::Con("int".to_string()),
                            TypeExpr::Con("List".to_string()),
                        ]),
                    },
                ],
            }]
        );

        assert_eq!(
            parse("type List = <T> Nil | Cons(T, List(T))"),
            vec![Stmt::Type {
                name: "List".to_string(),
                params: vec!["T".to_string()],
                kind_annotation: TypeExpr::Arrow(
                    Box::new(TypeExpr::Kind(Kind::Star)),
                    Box::new(TypeExpr::Kind(Kind::Star))
                ),
                variants: vec![
                    TypeVariant {
                        name: "Nil".to_string(),
                        fields: TypeVariantFields::Unit,
                    },
                    TypeVariant {
                        name: "Cons".to_string(),
                        fields: TypeVariantFields::Tuple(vec![
                            TypeExpr::Con("T".to_string()),
                            TypeExpr::App(
                                Box::new(TypeExpr::Con("List".to_string())),
                                vec![TypeExpr::Con("T".to_string())],
                            ),
                        ]),
                    },
                ],
            }]
        );

        assert_eq!(
            parse("type Color: Kind = Red | Green | Blue"),
            vec![Stmt::Type {
                name: "Color".to_string(),
                params: vec![],
                kind_annotation: TypeExpr::Con("Kind".to_string()),
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
                    type_annotation: TypeExpr::default(),
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
                    params: vec![],
                    kind_annotation: TypeExpr::Kind(Kind::Star),
                    variants: vec![
                        TypeVariant {
                            name: "Zero".to_string(),
                            fields: TypeVariantFields::Unit,
                        },
                        TypeVariant {
                            name: "Succ".to_string(),
                            fields: TypeVariantFields::Tuple(vec![TypeExpr::Con(
                                "Nat".to_string()
                            )]),
                        }
                    ],
                })
            }]
        );
    }

    fn expr(expr: Expr) -> Vec<Stmt> {
        vec![Stmt::Expr(expr)]
    }

    #[test]
    fn test_function_expressions() {
        assert_eq!(
            parse("(x, y) => x + y"),
            expr(Expr::Function {
                params: vec!["x".to_string(), "y".to_string(),],
                body: Box::new(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Ident("y".to_string())),
                )),
                type_annotation: TypeExpr::Arrow(
                    Box::new(TypeExpr::default()),
                    Box::new(TypeExpr::Arrow(
                        Box::new(TypeExpr::default()),
                        Box::new(TypeExpr::default())
                    ))
                ),
            }),
        );

        assert_eq!(
            parse("(x: Int, y: Int) => x + y"),
            expr(Expr::Function {
                params: vec!["x".to_string(), "y".to_string(),],
                body: Box::new(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Ident("y".to_string())),
                )),
                type_annotation: TypeExpr::Arrow(
                    Box::new(TypeExpr::Con("Int".to_string())),
                    Box::new(TypeExpr::Arrow(
                        Box::new(TypeExpr::Con("Int".to_string())),
                        Box::new(TypeExpr::default())
                    ))
                )
            }),
        );

        // Lambda with return type
        assert_eq!(
            parse("(x, y): Int => x + y"),
            expr(Expr::Function {
                params: vec!["x".to_string(), "y".to_string(),],
                body: Box::new(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Ident("y".to_string())),
                )),
                type_annotation: TypeExpr::Arrow(
                    Box::new(TypeExpr::default()),
                    Box::new(TypeExpr::Arrow(
                        Box::new(TypeExpr::default()),
                        Box::new(TypeExpr::Con("Int".to_string()))
                    ))
                )
            }),
        );

        assert_eq!(
            parse("(x) => x"),
            expr(Expr::Function {
                params: vec!["x".to_string()],
                body: Box::new(Expr::Ident("x".to_string())),
                type_annotation: TypeExpr::Arrow(
                    Box::new(TypeExpr::default()),
                    Box::new(TypeExpr::default())
                ),
            }),
        );
    }

    #[test]
    fn test_expression() {
        assert_eq!(parse("1"), expr(Expr::Literal(Literal::Int(1))));
        assert_eq!(
            parse("1 + 2"),
            expr(Expr::Infix(
                Token::Plus,
                Box::new(Expr::Literal(Literal::Int(1))),
                Box::new(Expr::Literal(Literal::Int(2))),
            )),
        );
        assert_eq!(
            parse("1 + 2 * 3"),
            expr(Expr::Infix(
                Token::Plus,
                Box::new(Expr::Literal(Literal::Int(1))),
                Box::new(Expr::Infix(
                    Token::Mul,
                    Box::new(Expr::Literal(Literal::Int(2))),
                    Box::new(Expr::Literal(Literal::Int(3))),
                )),
            )),
        );
        assert_eq!(
            parse("1 + 2 * 3 - 4 / 5"),
            expr(Expr::Infix(
                Token::Plus,
                Box::new(Expr::Literal(Literal::Int(1))),
                Box::new(Expr::Infix(
                    Token::Sub,
                    Box::new(Expr::Infix(
                        Token::Mul,
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Box::new(Expr::Literal(Literal::Int(3))),
                    )),
                    Box::new(Expr::Infix(
                        Token::Div,
                        Box::new(Expr::Literal(Literal::Int(4))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    ))
                )),
            )),
        );
        assert_eq!(
            parse("a -> (a -> b) -> b"),
            expr(Expr::Infix(
                Token::ThinArrow,
                Box::new(Expr::Ident("a".to_string())),
                Box::new(Expr::Infix(
                    Token::ThinArrow,
                    Box::new(Expr::Infix(
                        Token::ThinArrow,
                        Box::new(Expr::Ident("a".to_string())),
                        Box::new(Expr::Ident("b".to_string())),
                    )),
                    Box::new(Expr::Ident("b".to_string())),
                )),
            )),
        );
        assert_eq!(
            parse("!a"),
            expr(Expr::Prefix(
                Token::Not,
                Box::new(Expr::Ident("a".to_string()))
            )),
        );
        assert_eq!(
            parse("a()"),
            expr(Expr::Call {
                callee: Box::new(Expr::Ident("a".to_string())),
                params: vec![],
            }),
        );
        assert_eq!(
            parse("1 `xxx` 2 . 3 $ 4 <*> 5 ==> 6"),
            expr(Expr::Infix(
                Token::InfixIdent("xxx".to_string()),
                Box::new(Expr::Literal(Literal::Int(1))),
                Box::new(Expr::Infix(
                    Token::Dollar,
                    Box::new(Expr::Infix(
                        Token::Dot,
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Box::new(Expr::Literal(Literal::Int(3))),
                    )),
                    Box::new(Expr::Infix(
                        Token::InfixFixity(vec![Token::Less, Token::Mul, Token::Greater]),
                        Box::new(Expr::Literal(Literal::Int(4))),
                        Box::new(Expr::Infix(
                            Token::InfixFixity(vec![Token::Equal, Token::Greater]),
                            Box::new(Expr::Literal(Literal::Int(5))),
                            Box::new(Expr::Literal(Literal::Int(6))),
                        ))
                    ))
                ),)
            ),)
        );
        assert_eq!(
            parse("a(1, 2, 3)"),
            expr(Expr::Call {
                callee: Box::new(Expr::Ident("a".to_string())),
                params: vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                    Expr::Literal(Literal::Int(3)),
                ],
            }),
        );
        // assert_eq!("a.b", Expr::Field {
        //     object: Box::new(Expr::Ident("a".to_string())),
        //     field: "b".to_string()
        // });
        assert_eq!(
            parse("[1]",),
            expr(Expr::Literal(Literal::Array(vec![Expr::Literal(
                Literal::Int(1)
            )])))
        );
        assert_eq!(
            parse("[1,2,3,[4]]"),
            expr(Expr::Literal(Literal::Array(vec![
                Expr::Literal(Literal::Int(1)),
                Expr::Literal(Literal::Int(2)),
                Expr::Literal(Literal::Int(3)),
                Expr::Literal(Literal::Array(vec![Expr::Literal(Literal::Int(4))])),
            ])))
        );
        assert_eq!(
            parse("let x = 1 in x + 2"),
            expr(Expr::LetIn {
                name: "x".to_string(),
                type_annotation: TypeExpr::default(),
                value: Box::new(Expr::Literal(Literal::Int(1))),
                body: Box::new(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Literal(Literal::Int(2)))
                ))
            })
        );
        assert_eq!(
            parse("if true then 1 else 2"),
            expr(Expr::If {
                condition: Box::new(Expr::Literal(Literal::Bool(true))),
                then_branch: Box::new(Expr::Literal(Literal::Int(1))),
                else_branch: Box::new(Expr::Literal(Literal::Int(2))),
            }),
        );
        assert_eq!(
            parse("match x then | 0 => 1 | x if x > 0 => 1 | _ => 2"),
            expr(Expr::Match {
                expr: Box::new(Expr::Ident("x".to_string())),
                cases: vec![
                    MatchCase {
                        pattern: Pattern::Literal(Literal::Int(0)),
                        body: Expr::Literal(Literal::Int(1)),
                        guard: Expr::Literal(Literal::Bool(true)),
                    },
                    MatchCase {
                        pattern: Pattern::Ident("x".to_string()),
                        body: Expr::Literal(Literal::Int(1)),
                        guard: Expr::Infix(
                            Token::Greater,
                            Box::new(Expr::Ident("x".to_string())),
                            Box::new(Expr::Literal(Literal::Int(0))),
                        ),
                    },
                    MatchCase {
                        pattern: Pattern::Ident("_".to_string()),
                        body: Expr::Literal(Literal::Int(2)),
                        guard: Expr::Literal(Literal::Bool(true)),
                    },
                ],
            }),
        );
        assert_eq!(
            parse(
                r#"{
                let x = 1
                print(x)
                x + 2
            }"#
            ),
            expr(Expr::Block(vec![
                Stmt::Let {
                    name: "x".to_string(),
                    type_annotation: TypeExpr::default(),
                    value: Box::new(Expr::Literal(Literal::Int(1))),
                },
                Stmt::Expr(Expr::Call {
                    callee: Box::new(Expr::Ident("print".to_string())),
                    params: vec![Expr::Ident("x".to_string())],
                }),
                Stmt::Expr(Expr::Infix(
                    Token::Plus,
                    Box::new(Expr::Ident("x".to_string())),
                    Box::new(Expr::Literal(Literal::Int(2))),
                )),
            ])),
        );
    }
}
