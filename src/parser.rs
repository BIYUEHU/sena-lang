use std::fmt::{self, Display, Formatter};
use std::slice::Iter;

use crate::ast::{Expr, MatchCase, Pattern, Stmt, TypeVariant, TypeVariantFields};
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
                *token
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

    fn parse(&mut self) -> Option<Result<Stmt, ParseError>> {
        self.next_token();
        Some(match self.current_token {
            Token::Import => self.import_declaration(),
            Token::Export => self.export_declaration(),
            Token::Let => self.let_declaration(),
            Token::Type => self.type_declaration(),
            Token::Eof => return None,
            _ => return Some(Err(self.error_unexpected_token("expect a declaration"))),
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
            Token::LeftBrace => loop {
                self.next_token();
                if items.len() != 0 {
                    self.next_token_consume(&Token::Dot, "','")?
                }
                match &self.next_token {
                    Token::Ident(name) => items.push(name.clone()),
                    Token::RightBrace => break,
                    _ => return Err(self.error_unexpected_token("identifier after 'as'")),
                }
            },
            _ => return Err(self.error_invalid_syntax("expect all or part importing")),
        }

        self.next_token();
        self.next_token_consume(&Token::From, "'from' after import declaration")?;
        self.next_token();
        let source = match &self.next_token {
            Token::String(name) => name,
            _ => return Err(self.error_unexpected_token("string literal after 'from'")),
        }
        .clone();

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
        let name = match &self.next_token {
            Token::Ident(name) => name,
            _ => return Err(self.error_unexpected_token("identifyier")),
        }
        .clone();
        self.next_token();

        let type_annotation = if self.next_token_is(&Token::Colon) {
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
        let name = self.consume_identifier()?;

        // 可选的类型参数
        let type_params = if self.match_token(&Token::Less) {
            let mut params = Vec::new();
            loop {
                params.push(self.consume_identifier()?);
                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
            self.consume(&Token::Greater, "Expect '>' after type parameters")?;
            Some(params)
        } else {
            None
        };

        self.consume(&Token::Assign, "Expect '=' after type name")?;

        let variants = self.type_variants()?;

        Ok(Expr::Type {
            name,
            type_params,
            variants,
        })
    }

    // 类型变体
    fn type_variants(&mut self) -> Result<Vec<TypeVariant>, ParseError> {
        let mut variants = Vec::new();

        loop {
            let name = self.consume_identifier()?;

            let fields = if self.match_token(&Token::LeftParen) {
                // 元组风格的变体
                let mut args = Vec::new();
                if !self.check(&Token::RightParen) {
                    loop {
                        args.push(Box::new(self.expression()?));
                        if !self.match_token(&Token::Comma) {
                            break;
                        }
                    }
                }
                self.consume(&Token::RightParen, "Expect ')' after variant arguments")?;
                TypeVariantFields::Tuple(args)
            } else if self.match_token(&Token::LeftBrace) {
                // 记录风格的变体
                let mut fields = Vec::new();
                if !self.check(&Token::RightBrace) {
                    loop {
                        let field_name = self.consume_identifier()?;
                        self.consume(&Token::Colon, "Expect ':' after field name")?;
                        let field_type = Box::new(self.expression()?);
                        fields.push((field_name, field_type));

                        if !self.match_token(&Token::Comma) {
                            break;
                        }
                    }
                }
                self.consume(&Token::RightBrace, "Expect '}' after record fields")?;
                TypeVariantFields::Record(fields)
            } else {
                // 无参数变体
                TypeVariantFields::Unit
            };

            variants.push(TypeVariant { name, fields });

            if !self.match_token(&Token::Pipe) {
                break;
            }
        }

        Ok(variants)
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

        if self.match_token(&Token::Arrow) {
            self.lambda()
        } else if self.match_token(&Token::If) {
            self.if_expression()
        } else if self.match_token(&Token::Match) {
            self.match_expression()
        } else if self.match_token(&Token::Let) {
            self.let_in_expression()
        } else {
            let mut expr = self.prefix()?;
            expr = self.infix(expr, 0)?;
            Ok(expr)
        }
    }

    fn infix(&mut self, mut left: Expr, precedence: u8) -> Result<Expr, ParseError> {
        loop {
            let token = self.peek().map(|t| t.kind.clone());
            let next_precedence = self.get_precedence(&token);

            if next_precedence.is_some() && next_precedence.unwrap() > precedence {
                left = self.parse_infix(left)?;
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn get_precedence(&self, token: &Option<Token>) -> Option<u8> {
        match token {
            Some(Token::Plus) | Some(Token::Sub) => Some(1),
            Some(Token::Mul) | Some(Token::Div) => Some(2),
            Some(Token::Equal) | Some(Token::NotEqual) => Some(3),
            _ => None,
        }
    }

    fn parse_infix(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let token = self.advance().unwrap();
        let right = self.prefix()?;
        let next_precedence = self.get_precedence(&Some(token.kind.clone())).unwrap();
        let right = self.infix(right, next_precedence)?;

        Ok(Expr::Binary {
            left: Box::new(left),
            operator: token.kind,
            right: Box::new(right),
        })
    }

    fn prefix(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance().unwrap();
        let expr = match token.kind {
            Token::Int(i) => Expr::IntLiteral(i),
            Token::Float(f) => Expr::FloatLiteral(f),
            Token::String(s) => Expr::StringLiteral(s),
            Token::Char(c) => Expr::CharLiteral(c),
            Token::Ident(name) => Expr::Ident(name),
            Token::LeftParen => {
                let expr = self.expression()?;
                self.consume(&Token::RightParen, "Expect ')' after expression")?;
                expr
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "literal, identifier, or '('",
                    found: token.kind,
                    line: token.line,
                    column: token.column,
                })
            }
        };

        Ok(expr)
    }

    // Lambda 表达式
    fn lambda(&mut self) -> Result<Expr, ParseError> {
        let params = if self.check(&Token::LeftParen) {
            self.param_list()?
        } else {
            // 单个参数无括号的情况
            vec![(self.consume_identifier()?, None)]
        };

        if self.match_token(&Token::Arrow) {
            let body = Box::new(self.expression()?);
            Ok(Expr::Lambda { params, body })
        } else {
            self.if_expression()
        }
    }

    // 参数列表
    fn param_list(&mut self) -> Result<Vec<(String, Option<Box<Expr>>)>, ParseError> {
        self.consume(&Token::LeftParen, "Expect '(' after lambda")?;

        let mut params = Vec::new();
        if !self.check(&Token::RightParen) {
            loop {
                let name = self.consume_identifier()?;
                let type_annotation = if self.match_token(&Token::Colon) {
                    Some(Box::new(self.expression()?))
                } else {
                    None
                };
                params.push((name, type_annotation));

                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        self.consume(&Token::RightParen, "Expect ')' after parameters")?;
        Ok(params)
    }

    // If 表达式
    fn if_expression(&mut self) -> Result<Expr, ParseError> {
        // 已经消费了 "if" token
        let condition = Box::new(self.expression()?);

        self.consume(&Token::Then, "期望 'then' 关键字")?;
        let then_branch = Box::new(self.expression()?);

        self.consume(&Token::Else, "期望 'else' 关键字")?;
        let else_branch = Box::new(self.expression()?);

        Ok(Expr::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    // Match 表达式
    fn match_expression(&mut self) -> Result<Expr, ParseError> {
        // 已经消费了 "match" token
        let expr = Box::new(self.expression()?);

        self.consume(&Token::Then, "期望 'then' 关键字")?;

        let mut cases = Vec::new();
        // 至少需要一个 match case
        cases.push(self.match_case()?);

        // 继续解析其他 match cases
        while self.match_token(&Token::Pipe) {
            cases.push(self.match_case()?);
        }

        Ok(Expr::Match { expr, cases })
    }

    // Match case 解析
    fn match_case(&mut self) -> Result<MatchCase, ParseError> {
        let pattern = self.pattern()?;
        self.consume(&Token::Arrow, "期望 '=>' 箭头")?;
        let body = Box::new(self.expression()?);

        Ok(MatchCase { pattern, body })
    }

    // Pattern 解析
    fn pattern(&mut self) -> Result<Pattern, ParseError> {
        let token = self
            .peek()
            .ok_or(ParseError::EndOfInput { expected: "模式" })?
            .clone(); // 克隆 token 以避免借用冲突

        match token.kind {
            Token::Int(_) | Token::Float(_) | Token::String(_) | Token::Char(_) => {
                self.advance(); // 前进到下一个 token
                Ok(Pattern::Literal(token.kind))
            }
            Token::Ident(name) => {
                self.advance(); // 消费标识符
                if self.match_token(&Token::LeftParen) {
                    // 构造器模式
                    let mut args = Vec::new();
                    if !self.check(&Token::RightParen) {
                        loop {
                            args.push(self.pattern()?);
                            if !self.match_token(&Token::Comma) {
                                break;
                            }
                        }
                    }
                    self.consume(&Token::RightParen, "期望 ')'")?;
                    Ok(Pattern::Constructor { name, args })
                } else {
                    Ok(Pattern::Ident(name))
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "标识符或字面量",
                found: token.kind,
                line: token.line,
                column: token.column,
            }),
        }
    }

    // Let-in 表达式
    fn let_in_expression(&mut self) -> Result<Expr, ParseError> {
        // 已经消费了 "let" token
        let name = match self.advance() {
            Some(TokenData {
                kind: Token::Ident(name),
                ..
            }) => name,
            Some(token) => {
                return Err(ParseError::UnexpectedToken {
                    expected: "标识符",
                    found: token.kind,
                    line: token.line,
                    column: token.column,
                })
            }
            None => {
                return Err(ParseError::EndOfInput {
                    expected: "标识符"
                })
            }
        };

        // 可选的类型注解
        let type_annotation = if self.match_token(&Token::Colon) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        self.consume(&Token::Assign, "期望 '='")?;
        let value = Box::new(self.expression()?);

        self.consume(&Token::In, "期望 'in' 关键字")?;
        let body = Box::new(self.expression()?);

        Ok(Expr::Let {
            name,
            type_annotation,
            value,
            body: Some(body),
        })
    }

    // 函数调用表达式
    fn call_expression(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut type_args = None;

        // 处理可选的类型参数
        if self.match_token(&Token::Less) {
            let mut args = Vec::new();
            if !self.check(&Token::Greater) {
                loop {
                    match self.advance() {
                        Some(TokenData {
                            kind: Token::Ident(name),
                            ..
                        }) => {
                            args.push(name);
                        }
                        Some(token) => {
                            return Err(ParseError::UnexpectedToken {
                                expected: "类型参数",
                                found: token.kind,
                                line: token.line,
                                column: token.column,
                            })
                        }
                        None => {
                            return Err(ParseError::EndOfInput {
                                expected: "类型参数",
                            })
                        }
                    }
                    if !self.match_token(&Token::Comma) {
                        break;
                    }
                }
            }
            self.consume(&Token::Greater, "期望 '>'")?;
            type_args = Some(args);
        }

        // 解析函数参数
        self.consume(&Token::LeftParen, "期望 '('")?;
        let mut arguments = Vec::new();
        if !self.check(&Token::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }
        self.consume(&Token::RightParen, "期望 ')'")?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            arguments,
            type_args,
        })
    }

    // 块表达式
    fn block_expression(&mut self) -> Result<Expr, ParseError> {
        // 已经消费了 "{" token
        let mut expressions = Vec::new();

        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            expressions.push(self.declaration()?);

            // 可选的换行
            while self.match_token(&Token::Semicolon) {}
        }

        self.consume(&Token::RightBrace, "期望 '}'")?;
        Ok(Expr::Block(expressions))
    }
}

impl Iterator for Parser {
    type Item = Result<Expr, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_at_end() {
            None
        } else {
            Some(self.declaration())
        }
    }
}
