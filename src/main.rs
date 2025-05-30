use mihama::checker::Checker;
use mihama::env::{new_checker_env, new_evaluator_env};
use mihama::evaluator::object::{PrettyPrint, TypeInfo};
use mihama::evaluator::Evaluator;
use mihama::lexer::Lexer;
use mihama::utils::{eval_code, get_checked_ast, parse_code, unsafe_eval_code, RunningMode};
use std::io::Write;

const PROMPT: &str = ">> ";

// use std::thread;

// fn main() {
//     thread::Builder::new()
//         .name("main_with_big_stack".into())
//         .stack_size(32 * 1024 * 120024) // 32MB
//         .spawn(main_with_stack)
//         .unwrap()
//         .join()
//         .unwrap();
// }

fn main() {
    println!("Welcome to the Mihama programming language REPL!");
    println!("Input '.read <file>' to parse from a file");
    println!("Input '.exit' to exit the REPL");
    println!(
        "Input '.switch <\"l\"|\"p\"|\"c\"|\"e\"|\"u\">' to switch the REPL mode, default is parser"
    );

    let mut input = String::new();
    let mut mode = RunningMode::Evaluator;
    let mut env = new_evaluator_env();
    let mut evaluator = Evaluator::new(env);
    let mut checker = Checker::new(new_checker_env());

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
                "l" => {
                    println!("Switched to lexer mode");
                    RunningMode::Lexer
                }
                "p" => {
                    println!("Switched to parser mode");
                    RunningMode::Parser
                }
                "c" => {
                    println!("Switched to checker mode");
                    RunningMode::Checker
                }
                "e" => {
                    println!("Switched to evaluator mode");
                    RunningMode::Evaluator
                }
                "u" => {
                    println!("Switched to unsafe evaluator mode");
                    RunningMode::UnsafeEvaluator
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
            evaluator = Evaluator::new(env);
            continue;
        }

        let code = if input.starts_with(".read") {
            if input.starts_with(".readc") {
                env = new_evaluator_env();
                evaluator = Evaluator::new(env);
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
            RunningMode::Lexer => {
                for token in Lexer::new(&code) {
                    match token {
                        Ok(token) => println!(" -> {}", token),
                        Err(err) => println!("{}", err),
                    }
                }
            }
            RunningMode::Parser => match parse_code(&code) {
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
            RunningMode::Checker => match get_checked_ast(&code, &mut checker) {
                Ok(program) => {
                    for stmt in program {
                        println!(" : {:?}", stmt)
                    }
                }
                Err(err) => println!("Checker error: {}", err),
            },
            RunningMode::Evaluator => match eval_code(&code, &mut checker, &mut evaluator) {
                Ok(value) => {
                    if view_type_info {
                        println!("{} : {}", value.pretty_print(), value.type_info())
                    } else {
                        println!("{}", value.to_string())
                    }
                }
                Err(err) => println!("{}", err),
            },
            RunningMode::UnsafeEvaluator => match unsafe_eval_code(&code, &mut evaluator) {
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
