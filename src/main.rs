use mihama::checker::Checker;
use mihama::common::env::{new_checker_env, new_evaluator_env};
use mihama::evaluator::object::{Object, PrettyPrint, TypeInfo};
use mihama::evaluator::Evaluator;
use mihama::lexer::Lexer;
use mihama::parser::ast::{Program, Stmt};
use mihama::parser::Parser;
use std::io::Write;

const PROMPT: &str = ">> ";

enum ReplMode {
    Lexer,
    Parser,
    Checker,
    Evaluator,
}

fn get_parser(code: &str) -> Result<Parser, String> {
    let mut error = None;
    let token_data = Lexer::new(code)
        .filter_map(|result| match result {
            Ok(token_data) => Some(token_data),
            Err(err) => {
                if error.is_none() {
                    error = Some(format!("Lexer error: {}", err));
                }
                None
            }
        })
        .collect();

    if let Some(err) = error {
        Err(err)
    } else {
        Ok(Parser::new(token_data, true))
    }
}

fn get_checker(code: &str, checker: &mut Checker) -> Result<Program, String> {
    let parser = get_parser(code)?;
    let mut error = None;
    let program = parser
        .filter_map(|result| match result {
            Ok(stmt) => Some(stmt),
            Err(err) => {
                if error.is_none() {
                    error = Some(format!("Parser error: {}", err));
                }
                None
            }
        })
        .collect::<Vec<Stmt>>();

    if let Some(err) = error {
        Err(err)
    } else {
        Ok(checker
            .check(&program)
            .map_err(|err| format!("Checker error: {}", err))?)
    }
}

fn get_evaluator(
    code: &str,
    checker: &mut Checker,
    evaluator: &mut Evaluator,
) -> Result<Object, String> {
    evaluator
        .eval(&get_checker(code, checker)?)
        .map_err(|err| format!("Evaluator error: {}", err))
}

fn main() {
    println!("Welcome to the Mihama programming language REPL!");
    println!("Input '.read <file>' to parse from a file");
    println!("Input '.exit' to exit the REPL");
    println!("Input '.switch <\"lexer\"|\"parser\"|\"evaluator\">' to switch the REPL mode, default is parser");

    let mut input = String::new();
    let mut mode = ReplMode::Evaluator;
    let mut env = new_evaluator_env();
    let mut env2 = new_checker_env();
    let mut evaluator = Evaluator::new(&mut env);
    let mut checker = Checker::new(&mut env2);

    loop {
        print!("{}", PROMPT);
        if std::io::stdout().flush().is_err() {
            println!("error: flush err")
        }

        input.clear();
        let mut view_type_info = false;
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
                "checker" => {
                    println!("Switched to checker mode");
                    ReplMode::Checker
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

        if input.starts_with(".clear") {
            env = new_evaluator_env();
            evaluator = Evaluator::new(&mut env);
            continue;
        }

        let code = if input.starts_with(".read") {
            if input.starts_with(".readc") {
                env = new_evaluator_env();
                evaluator = Evaluator::new(&mut env);
            }
            match std::fs::read_to_string(input.split_whitespace().last().unwrap()) {
                Ok(code) => code,
                Err(err) => {
                    println!("REPL error: {}", err);
                    continue;
                }
            }
        } else if input.starts_with(".t") {
            view_type_info = true;
            input.clone().as_str()[2..].to_string()
        } else {
            input.clone()
        };

        match mode {
            ReplMode::Lexer => {
                for token in Lexer::new(&code) {
                    match token {
                        Ok(token) => println!(" -> {}", token),
                        Err(err) => println!("{}", err),
                    }
                }
            }
            ReplMode::Parser => match get_parser(&code) {
                Ok(parser) => {
                    for stmt in parser {
                        match stmt {
                            Ok(stmt) => println!(" : {:?}", stmt),
                            Err(err) => println!("{}", err),
                        }
                    }
                }
                Err(err) => println!("Parser error: {}", err),
            },
            ReplMode::Checker => match get_checker(&code, &mut checker) {
                Ok(program) => {
                    for stmt in program {
                        println!(" : {:?}", stmt)
                    }
                }
                Err(err) => println!("Checker error: {}", err),
            },
            ReplMode::Evaluator => match get_evaluator(&code, &mut checker, &mut evaluator) {
                Ok(value) => {
                    if view_type_info {
                        println!("{} : {}", value.pretty_print(), value.type_info())
                    } else {
                        println!("{}", value.to_string())
                    }
                }
                Err(err) => println!("{}", err),
            },
        }
    }
}
