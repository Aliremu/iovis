pub mod err;
pub mod lexer;
pub mod parser;
pub mod ast;
pub mod context;

use context::Context;
use lexer::{Lexer, Token};
use parser::Parser;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn get_tokens(input: &str) -> JsValue {
    let mut lexer = Lexer::new(input.to_string());

    let mut tokens = Vec::new();

    while let Ok(token) = lexer.next_token() {
        if token.0 == Token::EOF {
            break;
        }

        tokens.push(token);
    }

    JsValue::from(serde_json::to_string(&tokens).unwrap())
}

#[wasm_bindgen]
pub fn parse(input: &str) -> JsValue {
    let mut parser = Parser::new(input.to_string());

    match parser.parse() {
        Ok(parsed) => {
            JsValue::from(serde_json::to_string(&parsed).unwrap())
        }

        Err(err) => {
            JsValue::from(format!("{{ \"error\": \"{}\" }}", err.to_string().replace("\"", "\\\"")))
        }
    }
}

#[wasm_bindgen]
pub fn run(input: &str) -> JsValue {
    let mut parser = Parser::new(input.to_string());

    match parser.parse() {
        Ok(parsed) => {
            let mut ctx = Context::new();

            for stmt in parsed {
                ctx.evaluate_stmt(stmt);
            }
        
            JsValue::from(serde_json::to_string(&ctx.flush()).unwrap())
        }

        Err(err) => {
            JsValue::from(format!("{{ error: \"{}\" }}", err))
        }
    }
}