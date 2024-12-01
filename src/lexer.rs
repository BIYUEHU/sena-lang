type TokenType = &'static str;

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

// 标识符 + 字面量
const IDENT: TokenType = "IDENT";
const INT: TokenType = "INT";

// 运算符
const ASSIGN: TokenType = "=";
const PLUS: TokenType = "+";

// 分隔符
const COMMA: TokenType = ",";
const SEMICOLON: TokenType = ";";

const LPAREN: TokenType = "(";
const RPAREN: TokenType = ")";
const LBRACE: TokenType = "{";
const RBRACE: TokenType = "}";

// 关键字
const FUNCTION: TokenType = "FUNCTION";
const LET: TokenType = "LET";

const ILLEGAL: TokenType = "ILLEGAL";
const EOF: TokenType = "EOF";

fn is_letter(ch: char) -> bool {
    ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch) || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ('0'..='9').contains(&ch)
}

use std::collections::HashMap;

fn look_up_ident(ident: &str) -> TokenType {
    let mut key_words = HashMap::new();
    key_words.insert("fn", FUNCTION);
    key_words.insert("let", LET);

    if key_words.contains_key(ident) {
        return key_words[ident];
    }

    IDENT
}

pub struct Lexer {
    input: String,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer { input }
    }

    pub fn iter(&self) -> LexerIter {
        LexerIter::new(self.input.as_str())
    }
}

pub struct LexerIter<'a> {
    lex_input: &'a str,
    pos: usize,
    read_pos: usize,
    ch: u8,
}

impl LexerIter<'_> {
    fn new(lex: &str) -> LexerIter {
        let mut t = LexerIter {
            lex_input: lex,
            pos: 0,
            read_pos: 0,
            ch: 0,
        };
        t.read_char();
        t
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.lex_input.len() {
            self.ch = 0;
        } else {
            let temp = self.lex_input.as_bytes();
            self.ch = temp[self.read_pos];
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn read_number(&mut self) -> String {
        let p = self.pos;
        while is_digit(self.ch as char) {
            self.read_char()
        }

        self.lex_input[p..self.pos].to_string()
    }

    fn skip_white_space(&mut self) {
        let mut ch = self.ch as char;

        while ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
            self.read_char();
            ch = self.ch as char;
        }
    }

    fn read_identifier(&mut self) -> String {
        let p = self.pos;
        while is_letter(self.ch as char) {
            self.read_char()
        }

        self.lex_input[p..self.pos].to_string()
    }
}

impl Iterator for LexerIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let length = self.lex_input.len();

        self.skip_white_space();

        let res: Token = if self.pos > length {
            return None;
        } else {
            match self.ch as char {
                '=' => Token {
                    token_type: ASSIGN,
                    literal: ASSIGN.to_string(),
                },
                ';' => Token {
                    token_type: SEMICOLON,
                    literal: SEMICOLON.to_string(),
                },
                '(' => Token {
                    token_type: LPAREN,
                    literal: LPAREN.to_string(),
                },
                ')' => Token {
                    token_type: RPAREN,
                    literal: RPAREN.to_string(),
                },
                ',' => Token {
                    token_type: COMMA,
                    literal: COMMA.to_string(),
                },
                '+' => Token {
                    token_type: PLUS,
                    literal: PLUS.to_string(),
                },
                '{' => Token {
                    token_type: LBRACE,
                    literal: LBRACE.to_string(),
                },
                '}' => Token {
                    token_type: RBRACE,
                    literal: RBRACE.to_string(),
                },
                '\0' => Token {
                    token_type: EOF,
                    literal: "".to_string(),
                },
                _ => {
                    if is_letter(self.ch as char) {
                        let literal = self.read_identifier();
                        let token_type: TokenType = look_up_ident(literal.as_str());
                        return Some(Token {
                            token_type,
                            literal,
                        });
                    } else if is_digit(self.ch as char) {
                        return Some(Token {
                            token_type: INT,
                            literal: self.read_number(),
                        });
                    } else {
                        Token {
                            token_type: ILLEGAL,
                            literal: "".to_string(),
                        }
                    }
                }
            }
        };

        self.read_char();

        Some(res)
    }
}
