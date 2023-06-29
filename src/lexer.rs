use std::{fmt::Write};
use unic::emoji::{char::is_emoji};
use serde::{Deserialize, Serialize};
use crate::err::CompileError;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Decimal(f64),
    String(String),
    Boolean(bool)
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Boolean(n) => n.to_string(),
            Literal::Integer(n) => n.to_string(),
            Literal::Decimal(n) => n.to_string(),
            Literal::String(n)  => n.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum TokenKind {
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
    DotDot,

    Plus,
    Minus,
    Star,
    Caret,
    Slash,
    Percent,

    PlusEq,
    MinusEq,
    StarEq,
    CaretEq,
    SlashEq,
    PercentEq,

    And,
    AndAnd,
    Or,
    OrOr,

    Not,
    EqEq,
    NotEq,
    Eq,
    Gt,
    Ge,
    Lt,
    Le,

    Literal(Literal),

    Let,
    Fn,
    True,
    False,
    If,
    Else,
    While,
    Return,

    Struct,

    Extern,

    Illegal,
    EOF,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self {
            kind,
            span
        }
    }
}

impl From<String> for TokenKind {
    fn from(other: String) -> TokenKind {
        TokenKind::Ident(other)
    }
}

impl<'a> From<&'a str> for TokenKind {
    fn from(other: &'a str) -> TokenKind {
        TokenKind::Ident(other.to_string())
    }
}

impl From<i64> for TokenKind {
    fn from(other: i64) -> TokenKind {
        TokenKind::Literal(Literal::Integer(other))
    }
}

impl From<f64> for TokenKind {
    fn from(other: f64) -> TokenKind {
        TokenKind::Literal(Literal::Decimal(other))
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn
}

impl Span {
    pub fn merge(start: Span, end: Span) -> Span {
        Span {
            start: start.start,
            end: end.end
        }
    }
}

pub struct Lexer {
    input: String,
    position: usize,
    len: usize,
    ch: char,
    line_column: LineColumn
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            position: 0,
            line_column: LineColumn { line: 1, column: 1 },
            len: input.chars().count(),
            input: input,
            ch: '\0'
        };

        if lexer.len > 1 {
            lexer.ch = lexer.input.chars().nth(lexer.position).unwrap();
            lexer.position += 1;
        }

        lexer
    }

    pub fn next_token(&mut self) -> Result<Token, CompileError> {
        self.skip_whitespace();

        while self.ch == '/' && self.peek() == '/' {
            while self.ch != '\n' && self.ch != '\0' {
                self.read_char();
            }
            self.skip_whitespace();
        }

        let start = self.line_column;

        let token = match self.ch {
            '('  => TokenKind::LeftParen,
            ')'  => TokenKind::RightParen,
            '{'  => TokenKind::LeftBrace,
            '}'  => TokenKind::RightBrace,
            '['  => TokenKind::LeftBracket,
            ']'  => TokenKind::RightBracket,
            ';'  => TokenKind::SemiColon,
            ':'  => TokenKind::Colon,
            ','  => TokenKind::Comma,
            '.'  => {
                if self.peek() == '.' { self.read_char(); TokenKind::DotDot } else { TokenKind::Dot }
            },
            '+'  => TokenKind::Plus,
            '-'  => TokenKind::Minus,
            '*'  => TokenKind::Star,
            '^'  => TokenKind::Caret,
            '/'  => TokenKind::Slash,
            '%'  => TokenKind::Percent,
            '&' => {
                if self.peek() == '&' { self.read_char(); TokenKind::AndAnd } else { TokenKind::And }
            },
            '|' => {
                if self.peek() == '|' { self.read_char(); TokenKind::OrOr } else { TokenKind::Or }
            },
            '!' => {
                if self.peek() == '=' { self.read_char(); TokenKind::NotEq } else { TokenKind::Not }
            },
            '=' => {
                if self.peek() == '=' { self.read_char(); TokenKind::EqEq } else { TokenKind::Eq }
            },
            '>' => {
                if self.peek() == '=' { self.read_char(); TokenKind::Ge } else { TokenKind::Gt }
            },
            '<' => {
                if self.peek() == '=' { self.read_char(); TokenKind::Le } else { TokenKind::Lt }
            },
            '0'..='9' => {
                let a = self.read_number();

                a
            },
            '"' => {
                let a = self.read_string();

                a
            },
            'a'..='z' | 'A'..='Z' | '_' | '\u{4E00}'..='\u{9FFF}' => {
                let ident = self.read_ident();

                match ident.as_str() {
                    "let"    => TokenKind::Let,
                    "fn"     => TokenKind::Fn,
                    "if"     => TokenKind::If,
                    "else"   => TokenKind::Else,
                    "true"   => TokenKind::Literal(Literal::Boolean(true)),
                    "false"  => TokenKind::Literal(Literal::Boolean(false)),
                    "while"  => TokenKind::While,
                    "return" => TokenKind::Return,
                    "extern" => TokenKind::Extern,
                    "struct" => TokenKind::Struct,
                    _        => TokenKind::Ident(ident)
                }
            },
            n if is_emoji(n) => {
                let ident = self.read_ident();

                TokenKind::Ident(ident)
            },
            '\0' => TokenKind::EOF,
            n => {
                return Err(CompileError::IllegalToken(n, start));
            }
        };

        let end = self.line_column;

        self.read_char();

        Ok(Token::new(token, Span { start, end }))
    }

    pub fn read_number(&mut self) -> TokenKind {
        let mut ident = String::new();
        let mut decimal = false;
        loop {
            if self.ch == '.' {
                if decimal {
                    break;
                }

                decimal = true;
            }
            
            ident.write_char(self.ch).unwrap();

            if self.peek().is_digit(10) || self.peek() == '.' {
                self.read_char();
            } else {
                break;
            }
        }

        if decimal {
            TokenKind::from(ident.parse::<f64>().unwrap())
        } else {
            TokenKind::from(ident.parse::<i64>().unwrap())
        }
    }

    pub fn read_string(&mut self) -> TokenKind {
        let mut out = String::new();
        self.read_char();
        
        while self.ch != '\0' && self.ch != '"'  {
            out.write_char(self.ch).unwrap();
            self.read_char();
        }
        
        TokenKind::Literal(Literal::String(out))
    }

    pub fn read_ident(&mut self) -> String {
        let mut ident = String::new();

        loop {
            ident.write_char(self.ch).unwrap();

            if self.peek().is_ascii_alphanumeric() || self.peek() == '_' || (self.peek() >= '\u{4E00}' && self.peek() <= '\u{9FFF}') || is_emoji(self.peek()) {
                self.read_char();
            } else {
                break;
            }
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
            if self.ch == '\n' {
                self.line_column.line += 1;
                self.line_column.column = 1;
            } else {
               self.line_column.column += 1; 
            }

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