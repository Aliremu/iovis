use core::fmt;
use std::error::Error;

use crate::lexer::{TokenKind, LineColumn};

#[derive(Debug)]
pub enum CompileError {
    IllegalToken(char, LineColumn),
    UnexpectedToken(TokenKind, TokenKind, LineColumn),
    UnbalancedToken(TokenKind, LineColumn),

    Other(String),
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CompileError::IllegalToken(ch, span)               => format!("Illegal token {} at {:?}", ch, span),
                CompileError::UnexpectedToken(got, expected, span) => format!("Unexpected token {:?} at {:?}. Expected {:?}", got, span, expected),
                CompileError::UnbalancedToken(token, span)         => format!("Unbalanced {:?} at {:?}", token, span),
                CompileError::Other(msg)                           => msg.to_string(),
            }
        )
    }
}
