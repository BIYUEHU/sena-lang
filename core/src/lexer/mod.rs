use crate::utils::is_op_char;
use error::{LexerError, LexerResult};
use token::{Token, TokenData};

pub mod error;
pub mod token;

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    next_pos: usize,
    ch: char,
    next_ch: char,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            pos: 0,
            next_pos: 0,
            ch: '\0',
            next_ch: '\0',
            line: 1,
            column: 0,
        };
        lexer.read_char();
        lexer
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                ' ' | '\t' | '\r' | '\n' => self.read_char(),
                _ => break,
            }
        }
    }

    fn read_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.as_bytes()[self.next_pos] as char;
            if self.ch == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }
        self.pos = self.next_pos;
        self.next_pos += 1;
        self.next_ch = if self.next_pos >= self.input.len() {
            '\0'
        } else {
            self.input.as_bytes()[self.next_pos] as char
        };
    }

    fn nextch_is(&mut self, ch: char) -> bool {
        (if self.next_pos >= self.input.len() {
            '\0'
        } else {
            self.input.as_bytes()[self.next_pos] as char
        }) == ch
    }

    fn next_token(&mut self) -> Option<LexerResult> {
        self.skip_whitespace();
        let start_column = self.column;
        let start_line = self.line;

        let token = match self.ch {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            ',' => Token::Comma,
            ';' => Token::Semicolon,

            '/' => {
                if self.nextch_is('/') {
                    self.scan_line_comment()
                } else if self.nextch_is('*') {
                    match self.scan_block_comment() {
                        Some(token) => token,
                        None => {
                            return Some(Err(LexerError::InvalidSyntax {
                                message: "unterminated block comment".to_string(),
                                line: self.line,
                                column: start_column,
                            }))
                        }
                    }
                } else {
                    self.scan_fixity()
                }
            }
            ch if is_op_char(ch) => self.scan_fixity(),
            ch if ch.is_alphabetic() || ch == '_' => self.scan_identifier(),
            '0'..='9' => match self.scan_number() {
                Some(token_kind) => token_kind,
                None => {
                    return Some(Err(LexerError::InvalidSyntax {
                        message: "invalid number".to_string(),
                        line: self.line,
                        column: start_column,
                    }))
                }
            },
            '`' => {
                self.read_char();
                if self.ch == '`' {
                    self.read_char();
                    Token::InfixIdent("`".to_string())
                } else {
                    let mut op = String::new();
                    loop {
                        op.push(self.ch);
                        self.read_char();
                        if self.ch == '`' {
                            self.read_char();
                            break;
                        }
                    }
                    Token::InfixIdent(op)
                }
            }
            '"' => match self.scan_string() {
                Some(token) => token,
                None => {
                    return Some(Err(LexerError::InvalidSyntax {
                        message: "invalid string".to_string(),
                        line: self.line,
                        column: start_column,
                    }))
                }
            },
            '#' => match self.scan_origin_identifier() {
                Some(token) => token,
                None => {
                    return Some(Err(LexerError::InvalidSyntax {
                        message: "invalid identifier".to_string(),
                        line: self.line,
                        column: start_column,
                    }))
                }
            },
            '\'' => match self.scan_char() {
                Some(token) => token,
                None => {
                    return Some(Err(LexerError::InvalidSyntax {
                        message: "invalid char".to_string(),
                        line: self.line,
                        column: start_column,
                    }))
                }
            },
            '\0' => return None,
            ch => {
                self.read_char();
                return Some(Err(LexerError::UnexpectedCharacter {
                    ch,
                    line: self.line,
                    column: self.column,
                }));
            }
        };

        self.read_char();
        Some(Ok(TokenData {
            token,
            line: start_line,
            column: start_column,
        }))
    }

    fn scan_fixity(&mut self) -> Token {
        let mut chars = vec![self.ch];

        while is_op_char(self.next_ch) {
            chars.push(self.next_ch);
            self.read_char();
        }

        let mut tokens = Vec::new();
        let mut i = 0;
        while i < chars.len() {
            if i + 1 < chars.len() {
                let pair = format!("{}{}", chars[i], chars[i + 1]);
                let two_tok = match pair.as_str() {
                    "==" => Some(Token::Equal),
                    "!=" => Some(Token::NotEqual),
                    ">=" => Some(Token::GreaterEqual),
                    "<=" => Some(Token::LessEqual),
                    "&&" => Some(Token::And),
                    "||" => Some(Token::Or),
                    "->" => Some(Token::ThinArrow),
                    "=>" => Some(Token::Arrow),
                    _ => None,
                };
                if let Some(tok) = two_tok {
                    tokens.push(tok);
                    i += 2;
                    continue;
                }
            }

            let one = chars[i];
            let tok = match one {
                '+' => Token::Plus,
                '-' => Token::Sub,
                '*' => Token::Mul,
                '/' => Token::Div,
                '%' => Token::Mod,
                '^' => Token::Pow,
                '=' => Token::Assign,
                '>' => Token::Greater,
                '<' => Token::Less,
                '!' => Token::Not,
                '|' => Token::Arm,
                '$' => Token::Dollar,
                ':' => Token::Colon,
                '.' => Token::Dot,
                _ => {
                    i += 1;
                    continue;
                }
            };
            tokens.push(tok);
            i += 1;
        }

        if tokens.len() == 1 {
            tokens.into_iter().next().unwrap()
        } else {
            Token::InfixFixity(tokens)
        }
    }

    fn scan_identifier(&mut self) -> Token {
        let pos = self.pos;

        loop {
            if self.input.len().eq(&self.next_pos) {
                break;
            }
            match self.input.as_bytes()[self.next_pos] as char {
                ch if ch.is_alphanumeric() || ch == '_' => self.read_char(),
                _ => break,
            }
        }

        match &self.input[pos..self.pos + 1] {
            "let" => Token::Let,
            "type" => Token::Type,
            "match" => Token::Match,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "in" => Token::In,
            "import" => Token::Import,
            "export" => Token::Export,
            "from" => Token::From,
            "as" => Token::As,
            "abstract" => Token::Abstract,
            "with" => Token::With,
            str => Token::Ident(str.to_string()),
        }
    }

    fn scan_number(&mut self) -> Option<Token> {
        let mut is_float = false;
        let pos = self.pos;

        loop {
            match self.ch {
                '0'..='9' => self.read_char(),
                '.' => {
                    self.read_char();
                    if is_float {
                        return None;
                    }

                    let ch = self.ch;
                    self.read_char();
                    match ch {
                        '0'..='9' => {}
                        _ => return None,
                    }
                    is_float = true;
                }
                _ => {
                    self.next_pos = self.pos;
                    self.pos -= 1;
                    break;
                }
            }
        }

        if is_float {
            self.input[pos..self.pos + 1]
                .parse::<f64>()
                .map(|x| Some(Token::Float(x)))
                .unwrap_or(None)
        } else {
            self.input[pos..self.pos + 1]
                .parse::<i64>()
                .map(|x| Some(Token::Int(x)))
                .unwrap()
        }
    }

    // fn handle_escape_token(&mut self, ch: char) -> Option<char> {
    //     Some(match ch {
    //         'n' => '\n',
    //         't' => '\t',
    //         'r' => '\r',
    //         '0' => '\0',
    //         '"' => '\"',
    //         '\\' => '\\',
    //         '\'' => '\'',
    //         _ => return None,
    //     })
    // }

    fn scan_origin_identifier(&mut self) -> Option<Token> {
        let mut str = String::new();

        loop {
            self.read_char();
            match self.ch {
                '#' => return Some(Token::Ident(str)),
                '\0' => return None,
                ch => str.push(ch),
            }
        }
    }

    fn scan_string(&mut self) -> Option<Token> {
        let mut str = String::new();

        loop {
            self.read_char();
            match self.ch {
                '"' => return Some(Token::String(str)),
                // '\\' => {
                //     self.read_char();
                //     match self.handle_escape_token(self.ch) {
                //         Some(ch) => str.push(ch),
                //         None => return None,
                //     }
                // }
                '\0' => return None,
                ch => str.push(ch),
            }
        }
    }

    fn scan_char(&mut self) -> Option<Token> {
        self.read_char();
        let ch = self.ch;
        self.read_char();
        if self.ch == '\'' {
            Some(Token::Char(ch))
        } else {
            None
        }
    }

    fn scan_line_comment(&mut self) -> Token {
        self.read_char();
        let pos = self.next_pos;
        loop {
            self.read_char();
            if self.ch == '\0' || self.ch == '\n' {
                break;
            }
        }
        Token::LineComment(self.input[pos..self.pos].to_string())
    }

    fn scan_block_comment(&mut self) -> Option<Token> {
        self.read_char();
        let pos = self.next_pos;
        loop {
            self.read_char();
            match self.ch {
                '\0' => return None,
                '*' => {
                    self.read_char();
                    if self.ch == '/' {
                        break;
                    }
                }
                _ => continue,
            }
        }
        Some(Token::BlockComment(
            self.input[pos..self.pos - 1].to_string(),
        ))
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
        Lexer::new(input)
            .filter_map(|result| result.ok())
            .map(|token_data| token_data.token)
            .collect()
    }

    fn expect_error(input: &str) -> LexerError {
        Lexer::new(input)
            .find_map(|result| result.err())
            .expect("Expected lexer error")
    }

    #[test]
    fn test_single_char_tokens() {
        let tokens = lex("() {} [] ; , . : + - * / ^ % ==> <*>");
        assert_eq!(
            tokens,
            vec![
                LeftParen,
                RightParen,
                LeftBrace,
                RightBrace,
                LeftBracket,
                RightBracket,
                Semicolon,
                Comma,
                Dot,
                Colon,
                Plus,
                Sub,
                Mul,
                Div,
                Pow,
                Mod,
                InfixFixity(vec![Equal, Greater]),
                InfixFixity(vec![Less, Mul, Greater]),
            ]
        );
    }

    #[test]
    fn test_operators() {
        let tokens = lex("= == ! != > >= < <= => -> && || `compose` `apply`");
        assert_eq!(
            tokens,
            vec![
                Assign,
                Equal,
                Not,
                NotEqual,
                Greater,
                GreaterEqual,
                Less,
                LessEqual,
                Arrow,
                ThinArrow,
                And,
                Or,
                InfixIdent("compose".to_string()),
                InfixIdent("apply".to_string())
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let tokens = lex("let type match if then else in import export from as");
        assert_eq!(
            tokens,
            vec![Let, Type, Match, If, Then, Else, In, Import, Export, From, As]
        );
    }

    #[test]
    fn test_identifiers() {
        let tokens = lex("foo bar_123 _hidden");
        assert_eq!(
            tokens,
            vec![
                Ident("foo".to_string()),
                Ident("bar_123".to_string()),
                Ident("_hidden".to_string())
            ]
        );
    }

    #[test]
    fn test_numbers() {
        let tokens = lex("123 45.67");
        assert_eq!(tokens, vec![Int(123), Float(45.67)]);
    }

    #[test]
    fn test_strings() {
        let tokens = lex(r#""hello" "world\n" "escape\"quote""#);
        assert_eq!(
            tokens,
            vec![
                String("hello".to_string()),
                String("world\n".to_string()),
                String("escape\"quote".to_string())
            ]
        );
    }

    #[test]
    fn test_chars() {
        let tokens = lex("'a' '\\n' '\\''");
        assert_eq!(tokens, vec![Char('a'), Char('\n'), Char('\'')]);
    }

    #[test]
    fn test_comments() {
        let tokens = lex("// line comment\n/* block comment */");
        assert_eq!(
            tokens,
            vec![
                LineComment(" line comment".to_string()),
                BlockComment(" block comment ".to_string())
            ]
        );
    }

    #[test]
    fn test_invalid_number() {
        let error = expect_error("12.34.56");
        assert!(matches!(
            error,
            LexerError::InvalidSyntax {
                message,
                line: 1,
                ..
            } if message.contains("invalid number")
        ));
    }

    #[test]
    fn test_unterminated_string() {
        let error = expect_error("\"unterminated");
        assert!(matches!(
            error,
            LexerError::InvalidSyntax {
                message,
                line: 1,
                ..
            } if message.contains("invalid string")
        ));
    }

    #[test]
    fn test_unterminated_char() {
        let error = expect_error("'");
        assert!(matches!(
            error,
            LexerError::InvalidSyntax {
                message,
                line: 1,
                ..
            } if message.contains("invalid char")
        ));
    }

    #[test]
    fn test_unterminated_block_comment() {
        let error = expect_error("/* unterminated block comment");
        assert!(matches!(
            error,
            LexerError::InvalidSyntax {
                message,
                line: 1,
                ..
            } if message.contains("unterminated block comment")
        ));
    }

    #[test]
    fn test_unexpected_character() {
        let error = expect_error("@");
        assert!(matches!(
            error,
            LexerError::UnexpectedCharacter {
                ch: '@',
                line: 1,
                ..
            }
        ));
    }

    #[test]
    fn test_line_and_column_tracking() {
        let lexer = Lexer::new("let\nx = 1");
        let tokens: Vec<TokenData> = lexer.filter_map(Result::ok).collect();

        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].column, 1);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[1].column, 1);
    }

    #[test]
    fn test_import_declaration() {
        let tokens = lex(r#"import { Show, Eq } from "prelude""#);
        assert_eq!(
            tokens,
            vec![
                Import,
                LeftBrace,
                Ident("Show".to_string()),
                Comma,
                Ident("Eq".to_string()),
                RightBrace,
                From,
                String("prelude".to_string())
            ]
        );

        let tokens = lex(r#"import as std from "stdlib""#);
        assert_eq!(
            tokens,
            vec![
                Import,
                As,
                Ident("std".to_string()),
                From,
                String("stdlib".to_string())
            ]
        );
    }

    #[test]
    fn test_let_declaration() {
        let tokens = lex(r#"let #!x#: int = 1 + 2 * 3"#);
        assert_eq!(
            tokens,
            vec![
                Let,
                Ident("!x".to_string()),
                Colon,
                Ident("int".to_string()),
                Assign,
                Int(1),
                Plus,
                Int(2),
                Mul,
                Int(3)
            ]
        );
    }
}
