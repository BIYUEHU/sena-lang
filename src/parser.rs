use std::fmt::{self, Display, Formatter};
use std::slice::Iter;

use crate::ast::{
    Expr, Literal, MatchCase, Pattern, Precedence, Stmt, TypeVariant, TypeVariantFields,
};
use crate::token::{Token, TokenData};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: &'static str,
        found: Token,
        line: usize,
        column: usize,
    },
    EndOfInput {
        expected: &'static str,
    },
    InvalidSyntax {
        message: &'static str,
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
                "Unexpected token: expect {}, found {:?} at line {}, column {}",
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
pub struct Parser<'a> {
    iter: Iter<'a, TokenData>,
    current_token: Token,
    next_token: Token,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    pub fn new(iter: Iter<'a, TokenData>) -> Self {
        let mut parser = Parser {
            iter,
            current_token: Token::Eof,
            next_token: Token::Eof,
            line: 0,
            column: 0,
        };
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = match self.iter.next() {
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
    }

    fn next_token_is(&mut self, token: &Token) -> bool {
        self.next_token == *token
    }

    fn next_token_consume(
        &mut self,
        token: &Token,
        message: &'static str,
    ) -> Result<(), ParseError> {
        if self.next_token_is(token) {
            self.next_token();
            Ok(())
        } else {
            Err(self.error_unexpected_token(message))
        }
    }

    fn error_unexpected_token<'b>(&mut self, message: &'static str) -> ParseError {
        ParseError::UnexpectedToken {
            expected: message,
            found: self.current_token.clone(),
            line: self.line,
            column: self.column,
        }
    }

    fn error_invalid_syntax<'b>(&mut self, message: &'static str) -> ParseError {
        ParseError::InvalidSyntax {
            message,
            line: self.line,
            column: self.column,
        }
    }

    fn select_comma_items<T, H: FnMut(Token, bool, bool, &mut Self) -> Result<T, ParseError>>(
        &mut self,
        finished: Token,
        mut handler: H,
    ) -> Result<Vec<T>, ParseError> {
        let mut items = vec![];
        let mut scan_dot = false;

        loop {
            self.next_token();
            match &self.next_token {
                Token::Comma => {
                    scan_dot = true;
                }
                token => {
                    if token == &finished {
                        return if scan_dot {
                            Err(self.error_unexpected_token("',' after identifier"))
                        } else {
                            Ok(items)
                        };
                    }

                    match handler(token.clone(), scan_dot, items.is_empty(), self) {
                        Ok(item) => {
                            items.push(item);
                            scan_dot = false;
                        }
                        Err(err) => return Err(err),
                    }
                }
            }
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
            _ => return Some(Err(self.error_unexpected_token("a declaration"))),
        })
    }

    fn import_declaration(&mut self) -> Result<Stmt, ParseError> {
        let mut alias = None;
        let mut items = vec![];

        match self.next_token {
            Token::As => {
                self.next_token();
                alias = Some(
                    match &self.next_token {
                        Token::Ident(name) => name,
                        _ => return Err(self.error_unexpected_token("identifier after 'as'")),
                    }
                    .clone(),
                )
            }
            Token::LeftBrace => {
                items = self.select_comma_items(
                    Token::RightBrace,
                    |token, scan_dot, is_empty, self2| match token {
                        Token::Ident(name) => {
                            if scan_dot == is_empty {
                                Err(self2.error_unexpected_token(if scan_dot {
                                    "identifier after '{'"
                                } else {
                                    "',' after identifier"
                                }))
                            } else {
                                Ok(name.clone())
                            }
                        }
                        _ => Err(self2.error_invalid_syntax("expect identifier")),
                    },
                )?;
                if items.is_empty() {
                    return Err(self.error_invalid_syntax("import items couldn't be empty"));
                }
            }
            _ => return Err(self.error_invalid_syntax("expect all or part importing")),
        }

        self.next_token();
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
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        self.next_token_consume(&Token::Assign, "'=' after variable name")?;
        let value = Box::new(self.expression()?);

        // TODO: Let-In expression

        Ok(Stmt::Let {
            name,
            type_annotation,
            value,
        })
    }

    fn type_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = match &self.next_token {
            Token::Ident(name) => name.clone(),
            _ => return Err(self.error_unexpected_token("identifier")),
        };
        self.next_token();

        let type_annotation = if self.next_token_is(&Token::Colon) {
            self.next_token();
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        self.next_token_consume(&Token::Assign, "'=' after type name")?;
        let type_params = if self.next_token_is(&Token::Less) {
            let params =
                self.select_comma_items(Token::Greater, |token, scan_dot, is_empty, self2| {
                    match token {
                        Token::Ident(name) => {
                            if scan_dot == is_empty {
                                Err(self2.error_unexpected_token(if scan_dot {
                                    "identifier after '<'"
                                } else {
                                    "',' after identifier"
                                }))
                            } else {
                                Ok(name.clone())
                            }
                        }
                        _ => Err(self2.error_unexpected_token("expect identifier")),
                    }
                })?;
            if params.is_empty() {
                return Err(self.error_invalid_syntax("type parameters couldn't be empty"));
            }
            self.next_token();
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
                let exprs =
                    self.select_comma_items(Token::RightParen, |_, scan_dot, is_empty, self2| {
                        match self2.expression() {
                            Ok(expr) => {
                                if scan_dot == is_empty {
                                    Err(self2.error_unexpected_token(if scan_dot {
                                        "expression after '('"
                                    } else {
                                        "',' after expression"
                                    }))
                                } else {
                                    Ok(Box::new(expr))
                                }
                            }
                            Err(err) => Err(err),
                        }
                    })?;
                self.next_token_consume(&Token::RightParen, "')' after tuple")?;
                if exprs.is_empty() {
                    return Err(self.error_invalid_syntax("tuple type requires at least one field"));
                }
                self.next_token();
                TypeVariantFields::Tuple(exprs)
            } else if self.next_token_is(&Token::LeftBrace) {
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
                        let field_type = Box::new(self.expression()?);
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
            } else {
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

    // fn fun_name(&mut self) -> Result<(), ParseError> {
    //     self.fun_name()?;
    //     Ok(())
    // }

    fn fun_name(&mut self) -> Result<(), ParseError> {
        self.next_token_consume(&Token::RightBrace, "'}' after record")?;
        Ok(())
    }

    // 表达式解析
    fn expression(&mut self) -> Result<Expr, ParseError> {
        // 按照优先级从高到低依次尝试解析
        // 优先级顺序：
        // 1. lambda 表达式
        // 2. if 表达式
        // 3. match 表达式
        // 4. let-in 表达式
        // 5. 二元运算表达式（包含各种运算符优先级）
        // 6. 一元运算表达式
        // 7. 函数调用
        // 8. 基础表达式（字面量、标识符、括号表达式）
        self.parse_expression(Precedence::Lowest)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        self.next_token();
        let mut left = match &self.current_token {
            Token::Ident(name) => Expr::Ident(name.clone()),
            Token::Int(value) => Expr::Literal(Literal::Int(*value)),
            Token::Float(value) => Expr::Literal(Literal::Float(*value)),
            Token::String(value) => Expr::Literal(Literal::String(value.clone())),
            Token::Char(value) => Expr::Literal(Literal::Char(*value)),
            Token::LeftParen => {
                let expr = self.expression()?;
                self.next_token_consume(&Token::RightParen, "')' after expression")?;
                expr
            }
            Token::Sub | Token::Not => {
                let operator = self.next_token.clone();
                let expr = self.parse_expression(Precedence::Prefix)?;
                Expr::Prefix(operator, Box::new(expr))
            }
            Token::If => self.if_expression()?,
            Token::Match => self.match_expression()?,
            // Token::Let => self.par(),
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
                    let right = self.parse_expression(Precedence::from_token(&self.next_token))?;
                    Ok(Expr::Infix(
                        self.next_token.clone(),
                        Box::new(left),
                        Box::new(right),
                    ))
                }
                Token::LeftParen => {
                    // let arguments = self.parse_argument_list()?;
                    // self.next_token_consume(&Token::RightParen, "')' after arguments")?;
                    Ok(Expr::Call {
                        callee: Box::new(left),
                        arguments: vec![],
                    })
                }
                Token::Arrow => {
                    if let Some(params) = self.extract_param_list(&left) {
                        let body = Box::new(self.expression()?);
                        Ok(Expr::Lambda {
                            type_params: vec![], // 语法中未明确支持 lambda 的类型参数，留空
                            params,
                            body,
                        })
                    } else {
                        Err(self.error_invalid_syntax("invalid lambda parameter list"))
                    }
                }
                _ => Ok(left),
            };
            self.next_token();
            result
        }

        Ok(left)
    }

    fn extract_param_list(&self, expr: &Expr) -> Option<Vec<(String, Option<Box<Expr>>)>> {
        let list = self.flatten_comma_list(expr);
        let mut params = vec![];

        for e in list {
            match e {
                Expr::Ident(name) => params.push((name.clone(), None)),
                Expr::Infix(Token::Colon, name, type_expr) => match *name.clone() {
                    Expr::Ident(name) => params.push((name, Some(type_expr.clone()))),
                    _ => return None,
                },
                _ => return None,
            }
        }
        Some(params)
    }

    fn flatten_comma_list(&self, expr: &Expr) -> Vec<Expr> {
        match expr {
            Expr::Infix(Token::Comma, left, right) => {
                let mut list = self.flatten_comma_list(left.as_ref());
                list.extend(self.flatten_comma_list(right.as_ref()));
                list
            }
            _ => vec![expr.clone()],
        }
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = vec![];
        if !self.next_token_is(&Token::RightParen) {
            loop {
                args.push(self.expression()?);
                if self.next_token_is(&Token::Comma) {
                    self.next_token();
                } else {
                    break;
                }
            }
        }
        Ok(args)
    }

    fn if_expression(&mut self) -> Result<Expr, ParseError> {
        self.next_token(); // 消费 'if'
        let condition = Box::new(self.expression()?);
        self.next_token_consume(&Token::Then, "'then' after condition")?;
        let then_branch = Box::new(self.expression()?);
        self.next_token_consume(&Token::Else, "'else' after then branch")?;
        let else_branch = Box::new(self.expression()?);
        Ok(Expr::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn match_expression(&mut self) -> Result<Expr, ParseError> {
        self.next_token(); // 消费 'match'
        let expr = Box::new(self.expression()?);
        self.next_token_consume(&Token::Then, "'then' after match expression")?;
        let mut cases = vec![];
        while self.next_token_is(&Token::Arm) {
            self.next_token();
            let pattern = self.pattern()?;
            self.next_token_consume(&Token::Arrow, "'=>' after pattern")?;
            let body = Box::new(self.expression()?);
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
            // 其他字面量类似
            _ => Err(self.error_unexpected_token("pattern")),
        }
    }

    fn block_expression(&mut self) -> Result<Expr, ParseError> {
        self.next_token(); // 消费 '{'
        let mut statements = vec![];
        while !self.next_token_is(&Token::RightBrace) {
            let stmt = match &self.next_token {
                Token::Let => self.let_declaration()?,
                Token::Type => self.type_declaration()?,
                _ => Stmt::Expr(self.expression()?),
            };
            statements.push(stmt);
            // if self.next_token_is(&Token::Newline) {
            // self.next_token();
            // }
        }
        self.next_token_consume(&Token::RightBrace, "'}' after block")?;
        if statements.is_empty() || !matches!(statements.last(), Some(Stmt::Expr(_))) {
            return Err(self.error_invalid_syntax("block must end with an expression"));
        }
        Ok(Expr::Block(vec![])) // 注意：AST 中应为 Vec<Stmt>，参考内容可能有误
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}
