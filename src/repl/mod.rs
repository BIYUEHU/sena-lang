use crate::utils::{
    eval_code, get_checked_ast, parse_code, transofrm_code, unsafe_eval_code, RunningMode,
};
use mihama_core::checker::Checker;
use mihama_core::env::{new_checker_env, new_evaluator_env};
use mihama_core::evaluator::object::{PrettyPrint, TypeInfo};
use mihama_core::evaluator::Evaluator;
use mihama_core::lexer::Lexer;
use std::fs;
use std::io::{self, Write};

const PROMPT: &str = "> ";

pub fn run_repl(initial_mode: RunningMode, verbose: bool) {
    println!(
        "Welcome to Mihama Programming Language v{}",
        env!("CARGO_PKG_VERSION")
    );
    println!("Type :help for available commands");

    if verbose {
        println!("Starting REPL in {} mode", initial_mode);
    }

    let mut input = String::new();
    let mut mode = initial_mode;
    let env = new_evaluator_env();
    let mut evaluator = Evaluator::new(env);
    let mut checker = Checker::new(new_checker_env());

    loop {
        print!("{}", PROMPT);
        if io::stdout().flush().is_err() {
            eprintln!("Error: Failed to flush stdout");
            continue;
        }

        input.clear();
        if io::stdin().read_line(&mut input).is_err() {
            eprintln!("Error: Failed to read input");
            continue;
        }

        let command = input.trim();

        if command.is_empty() {
            continue;
        }

        // 处理REPL命令
        if command.starts_with(':') {
            match handle_repl_command(command, &mut mode, &mut evaluator, &mut checker, verbose) {
                ReplAction::Continue => continue,
                ReplAction::Exit => break,
                ReplAction::Error(msg) => {
                    eprintln!("Error: {}", msg);
                    continue;
                }
            }
        }

        // 处理特殊前缀
        let (code, show_types) = if command.starts_with(".t ") {
            (command[3..].to_string(), true)
        } else {
            (command.to_string(), false)
        };

        // 执行代码
        execute_code(&code, &mode, &mut checker, &mut evaluator, show_types);
    }

    println!("Goodbye!");
}

enum ReplAction {
    Continue,
    Exit,
    Error(String),
}

fn handle_repl_command(
    command: &str,
    mode: &mut RunningMode,
    evaluator: &mut Evaluator,
    checker: &mut Checker,
    verbose: bool,
) -> ReplAction {
    let parts: Vec<&str> = command.split_whitespace().collect();

    match parts[0] {
        ":help" | ":h" => {
            print_help();
            ReplAction::Continue
        }
        ":exit" | ":quit" | ":q" => ReplAction::Exit,
        ":mode" | ":m" => {
            if parts.len() < 2 {
                return ReplAction::Error("Usage: :mode <l|p|c|e|u>".into());
            }
            match parts[1] {
                "l" => {
                    *mode = RunningMode::Lexer;
                    println!("Switched to lexer mode");
                }
                "p" => {
                    *mode = RunningMode::Parser;
                    println!("Switched to parser mode");
                }
                "c" => {
                    *mode = RunningMode::Checker;
                    println!("Switched to checker mode");
                }
                "e" => {
                    *mode = RunningMode::Evaluator;
                    println!("Switched to evaluator mode");
                }
                "u" => {
                    *mode = RunningMode::UnsafeEvaluator;
                    println!("Switched to unsafe evaluator mode");
                }
                _ => return ReplAction::Error("Invalid mode, choose one of l, p, c, e, u".into()),
            }
            ReplAction::Continue
        }
        ":read" | ":r" => {
            if parts.len() < 2 {
                return ReplAction::Error("Usage: :read <file>".into());
            }
            let fname = parts[1];
            match fs::read_to_string(fname) {
                Ok(code) => {
                    execute_code(&code, mode, checker, evaluator, false);
                }
                Err(err) => eprintln!("REPL error: {}", err),
            }
            ReplAction::Continue
        }
        ":clear" | ":c" => {
            let new_env = new_evaluator_env();
            *evaluator = Evaluator::new(new_env);
            *checker = Checker::new(new_checker_env());
            if verbose {
                println!("Environments cleared");
            }
            ReplAction::Continue
        }
        ":trans" | ":t" => {
            if parts.len() != 3 {
                return ReplAction::Error("Usage: :trans <from> <to>".into());
            }
            let (src, dst) = (parts[1], parts[2]);
            match fs::read_to_string(src) {
                Ok(text) => match transofrm_code(&text) {
                    Ok(out) => match fs::write(dst, out) {
                        Ok(_) => println!("Transpiled code written to {}", dst),
                        Err(e) => eprintln!("Error writing file: {}", e),
                    },
                    Err(e) => eprintln!("Transform error: {}", e),
                },
                Err(e) => eprintln!("Read error: {}", e),
            }
            ReplAction::Continue
        }
        _ => ReplAction::Error(format!("Unknown command: {}", parts[0])),
    }
}

fn print_help() {
    println!("Available commands:");
    println!(":help, :h          - Show this help");
    println!(":exit, :quit, :q   - Exit the REPL");
    println!(":mode, :m <mode>   - Switch REPL mode (l|p|c|e|u)");
    println!(":read, :r <file>   - Read and execute file");
    println!(":clear, :c         - Reset environments");
    println!(":trans, :t <src> <dst> - Transpile source to destination");
    println!(".t <code>           - Evaluate and show types");
}

fn execute_code(
    code: &str,
    mode: &RunningMode,
    checker: &mut Checker,
    evaluator: &mut Evaluator,
    show_types: bool,
) {
    match *mode {
        RunningMode::Lexer => {
            for token in Lexer::new(code) {
                match token {
                    Ok(tok) => println!(" -> {}", tok),
                    Err(err) => eprintln!("{}", err),
                }
            }
        }
        RunningMode::Parser => match parse_code(code) {
            Ok(parser) => {
                for stmt in parser {
                    match stmt {
                        Ok(s) => println!(" : {:?}", s),
                        Err(err) => eprintln!("{}", err),
                    }
                }
            }
            Err(err) => eprintln!("Parser error: {}", err),
        },
        RunningMode::Checker => match get_checked_ast(code, checker) {
            Ok(program) => {
                for stmt in program {
                    println!(" : {:?}", stmt);
                }
            }
            Err(err) => eprintln!("Checker error: {}", err),
        },
        RunningMode::Evaluator => match eval_code(code, checker, evaluator) {
            Ok(val) => {
                if show_types {
                    println!("{} : {}", val.pretty_print(), val.type_info());
                } else {
                    println!("{}", val.to_string());
                }
            }
            Err(err) => eprintln!("{}", err),
        },
        RunningMode::UnsafeEvaluator => match unsafe_eval_code(code, evaluator) {
            Ok(val) => {
                if show_types {
                    println!("{} : {}", val.pretty_print(), val.type_info());
                } else {
                    println!("{}", val.to_string());
                }
            }
            Err(err) => eprintln!("{}", err),
        },
    }
}
