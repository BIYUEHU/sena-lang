use parser::{
    lexer::{Lexer, LexerResult},
    token::{Token, TokenData},
};
// use parser::parser::Parser;
use std::io::Write;

const PROMPT: &str = ">> ";

enum ReplMode {
    Lexer,
    Parser,
    Evaluator,
}

fn main() {
    println!("Welcome to the Tamaki programming language REPL!");
    println!("Input '.read <file>' to parse from a file");
    println!("Input '.exit' to exit the REPL");
    println!("Input '.switch <\"lexer\"|\"parser\"|\"evaluator\">' to switch the REPL mode, default is parser");

    let mut input = String::new();
    let mut mode = ReplMode::Lexer;
    loop {
        print!("{}", PROMPT);
        if std::io::stdout().flush().is_err() {
            println!("error: flush err")
        }

        input.clear();
        std::io::stdin().read_line(&mut input).unwrap();

        if input.starts_with(".exit") {
            println!("Goodbye!");
            break;
        }

        if input.starts_with(".switch") {
            mode = match input.split_whitespace().last().unwrap() {
                "lexer" => {
                    println!("Switched to lexer mode");
                    ReplMode::Lexer
                }
                "parser" => {
                    println!("Switched to parser mode");
                    ReplMode::Parser
                }
                "evaluator" => {
                    println!("Switched to evaluator mode");
                    ReplMode::Evaluator
                }
                _ => {
                    println!("Invalid mode");
                    continue;
                }
            };
            continue;
        }

        let code = if input.starts_with(".read") {
            match std::fs::read_to_string(input.split_whitespace().last().unwrap()) {
                Ok(code) => code,
                Err(err) => {
                    println!("REPL error: {}", err);
                    continue;
                }
            }
        } else {
            input.clone()
        };

        // fn pl(token: LexerResult) {
        //     match token {
        //         Ok(token) => println!(" -> {}", token),
        //         Err(err) => println!(" {}", err),
        //     }
        // }

        match mode {
            ReplMode::Lexer => {
                // let l = Lexer::new(&code);
                for token in Lexer::new(&code) {
                    match token {
                        Ok(token) => println!(" -> {}", token),
                        Err(err) => println!(" {}", err),
                    }
                }
            }
            // ReplMode::Parser => {
            //     for expr in Parser::new(Lexer::new(&code).map(|t| t.unwrap()).collect()) {
            //         match expr {
            //             Ok(expr) => println!("{:?}", expr),
            //             Err(err) => println!("Parser error: {}", err),
            //         }
            //     }
            // }
            _ => {
                println!("Not implemented yet");
            }
        }
    }
}
