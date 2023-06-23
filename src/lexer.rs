use std::{fmt::Write, error::Error};
use unic::emoji::{char::is_emoji};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Decimal(f64),
    String(String),
    Boolean(bool)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    SemiColon,
    Colon,
    Comma,
    Dot,

    Plus,
    Minus,
    Asterisk,
    Carat,
    Slash,
    Modulo,
    Bang,
    Equal,
    NotEqual,
    Assign,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Literal(Literal),

    Let,
    Fn,
    True,
    False,
    If,
    Else,
    Return,

    Illegal,
    EOF,
}

impl From<String> for Token {
    fn from(other: String) -> Token {
        Token::Ident(other)
    }
}

impl<'a> From<&'a str> for Token {
    fn from(other: &'a str) -> Token {
        Token::Ident(other.to_string())
    }
}

impl From<i64> for Token {
    fn from(other: i64) -> Token {
        Token::Literal(Literal::Integer(other))
    }
}

impl From<f64> for Token {
    fn from(other: f64) -> Token {
        Token::Literal(Literal::Decimal(other))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize
}

pub struct Lexer {
    input: String,
    position: usize,
    len: usize,
    ch: char
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            position: 0,
            len: input.chars().count(),
            input: input,
            ch: '\0'
        };

        lexer.read_char();

        lexer
    }

    pub fn next_token(&mut self) -> Result<(Token, Span), String> {
        self.skip_whitespace();

        while self.ch == '/' && self.peek() == '/' {
            while self.ch != '\n' && self.ch != '\0' {
                self.read_char();
            }
            self.skip_whitespace();
        }

        let start = self.position;

        let token = match self.ch {
            '('  => Token::LeftParen,
            ')'  => Token::RightParen,
            '{'  => Token::LeftBrace,
            '}'  => Token::RightBrace,
            '['  => Token::LeftBracket,
            ']'  => Token::RightBracket,
            ';'  => Token::SemiColon,
            ':'  => Token::Colon,
            ','  => Token::Comma,
            '.'  => Token::Dot,
            '+'  => Token::Plus,
            '-'  => Token::Minus,
            '*'  => Token::Asterisk,
            '^'  => Token::Carat,
            '/'  => Token::Slash,
            '%'  => Token::Modulo,
            '!' => {
                if self.peek() == '=' { self.read_char(); Token::NotEqual } else { Token::Bang }
            },
            '=' => {
                if self.peek() == '=' { self.read_char(); Token::Equal } else { Token::Assign }
            },
            '>' => {
                if self.peek() == '=' { self.read_char(); Token::GreaterEqual } else { Token::Greater }
            },
            '<' => {
                if self.peek() == '=' { self.read_char(); Token::LessEqual } else { Token::Less }
            },
            '0'..='9' => {
                let a = self.read_number();

                if self.ch != '\0' {
                    self.position -= 1;
                }

                a
            },
            '"' => {
                let a = self.read_string();

                if self.ch != '\0' {
                    // self.position -= 1;
                }

                a
            },
            'a'..='z' | 'A'..='Z' | '_' | '\u{4E00}'..='\u{9FFF}' => {
                let ident = self.read_ident();
                
                if self.ch != '\0' {
                    self.position -= 1;
                }

                match ident.as_str() {
                    "let"    => Token::Let,
                    "fn"     => Token::Fn,
                    "if"     => Token::If,
                    "else"   => Token::Else,
                    "true"   => Token::Literal(Literal::Boolean(true)),
                    "false"  => Token::Literal(Literal::Boolean(false)),
                    "return" => Token::Return,
                    _        => Token::Ident(ident)
                }
            },
            n if is_emoji(n) => {
                let ident = self.read_ident();

                if self.ch != '\0' {
                    self.position -= 1;
                }

                Token::Ident(ident)
            },
            '\0' => Token::EOF,
            _ => {
                println!("ILLEGAL {}", self.ch);
                Token::Illegal
            }
        };

        self.read_char();
        let end = self.position;

        Ok((token, Span { start, end }))
    }

    pub fn read_number(&mut self) -> Token {
        let mut ident = String::new();
        let mut decimal = false;
        while self.ch.is_digit(10) || self.ch == '.' {
            if self.ch == '.' {
                if decimal {
                    break;
                }

                decimal = true;
            }
            
            ident.write_char(self.ch).unwrap();
            self.read_char();
        }

        if decimal {
            Token::from(ident.parse::<f64>().unwrap())
        } else {
            Token::from(ident.parse::<i64>().unwrap())
        }
    }

    pub fn read_string(&mut self) -> Token {
        let mut out = String::new();
        self.read_char();
        
        while self.ch != '\0' && self.ch != '"' {
            out.write_char(self.ch).unwrap();
            self.read_char();
        }
        
        Token::Literal(Literal::String(out))
    }

    pub fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        while self.ch.is_ascii_alphanumeric() || self.ch == '_' || (self.ch >= '\u{4E00}' && self.ch <= '\u{9FFF}') || is_emoji(self.ch) {
            ident.write_char(self.ch).unwrap();
            self.read_char();
        }
        
        ident
    }

    pub fn peek(&self) -> char {
        if self.position >= self.len - 1 {
            '\0'
        } else {
            self.input.chars().nth(self.position).unwrap()
        }
    }

    pub fn read_char(&mut self) {
        if self.position >= self.len {
            self.ch = '\0' ;
        } else {
            self.ch = self.input.chars().nth(self.position).unwrap();
            self.position += 1;
        }
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}
