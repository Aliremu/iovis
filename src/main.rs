use std::{collections::HashMap, fs};
use std::env;

use iovis::context::Context;
use iovis::parser::Parser;

fn main() {
    // let input = r#"
    //     fn 雪花飘飘() {
    //         let mat = [
    //             [7.5,  2.3],
    //             [-4.6, 2.1]
    //         ];

    //         let det = mat.det();

    //         print(det == 26.33);
    //     }

    //     雪花飘飘();
    // "#.to_string();
    let args: Vec<String> = env::args().collect();
    let contents = fs::read_to_string(args.get(1).expect("Usage: iovis.exe <file>"))
        .expect("Should have been able to read the file");

    let mut parser = Parser::new(contents.clone());
    
    match parser.parse() {
        Ok(parsed) => {
            let mut ctx = Context::new();

            for stmt in parsed {
                ctx.evaluate_stmt(stmt);
            }
        
            print!("{}", ctx.output);
        }

        Err(err) => {
            print!("{}", err);
        }
    }
}
