use crate::utils::{
    eval_code, get_checked_ast, parse_code, transofrm_code, unsafe_eval_code, RunningMode,
};
use mihama_core::checker::Checker;
use mihama_core::env::{new_checker_env, new_evaluator_env};
use mihama_core::evaluator::object::{PrettyPrint, TypeInfo};
use mihama_core::evaluator::Evaluator;
use mihama_core::lexer::Lexer;
use std::fs;
use std::path::PathBuf;

pub fn parse_mode(mode: &str) -> RunningMode {
    match mode {
        "lexer" | "l" => RunningMode::Lexer,
        "parser" | "p" => RunningMode::Parser,
        "checker" | "c" => RunningMode::Checker,
        "evaluator" | "e" => RunningMode::Evaluator,
        "unsafe" | "u" => RunningMode::UnsafeEvaluator,
        _ => {
            eprintln!("Warning: Unknown mode '{}', defaulting to evaluator", mode);
            RunningMode::Evaluator
        }
    }
}

pub fn run_file(file: PathBuf, mode: RunningMode, show_types: bool, verbose: bool) {
    let code = match fs::read_to_string(&file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file.display(), err);
            std::process::exit(1);
        }
    };

    if verbose {
        println!("Running file: {}", file.display());
        println!("Mode: {}", mode);
    }

    let mut checker = Checker::new(new_checker_env());
    let mut evaluator = Evaluator::new(new_evaluator_env());

    match mode {
        RunningMode::Lexer => {
            for token in Lexer::new(&code) {
                match token {
                    Ok(token) => println!("{}", token),
                    Err(err) => {
                        eprintln!("Lexer error: {}", err);
                        std::process::exit(1);
                    }
                }
            }
        }
        RunningMode::Parser => match parse_code(&code) {
            Ok(parser) => {
                for stmt in parser {
                    match stmt {
                        Ok(stmt) => println!("{:?}", stmt),
                        Err(err) => {
                            eprintln!("Parser error: {}", err);
                            std::process::exit(1);
                        }
                    }
                }
            }
            Err(err) => {
                eprintln!("Parser error: {}", err);
                std::process::exit(1);
            }
        },
        RunningMode::Checker => match get_checked_ast(&code, &mut checker) {
            Ok(program) => {
                for stmt in program {
                    println!("{:?}", stmt);
                }
            }
            Err(err) => {
                eprintln!("Type checker error: {}", err);
                std::process::exit(1);
            }
        },
        RunningMode::Evaluator => match eval_code(&code, &mut checker, &mut evaluator) {
            Ok(value) => {
                if show_types {
                    println!("{} : {}", value.pretty_print(), value.type_info());
                } else {
                    println!("{}", value.to_string());
                }
            }
            Err(err) => {
                eprintln!("Evaluation error: {}", err);
                std::process::exit(1);
            }
        },
        RunningMode::UnsafeEvaluator => match unsafe_eval_code(&code, &mut evaluator) {
            Ok(value) => {
                if show_types {
                    println!("{} : {}", value.pretty_print(), value.type_info());
                } else {
                    println!("{}", value.to_string());
                }
            }
            Err(err) => {
                eprintln!("Unsafe evaluation error: {}", err);
                std::process::exit(1);
            }
        },
    }
}

pub fn transpile_file(input: PathBuf, output: PathBuf, target: &str, verbose: bool) {
    let code = match fs::read_to_string(&input) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading input file {}: {}", input.display(), err);
            std::process::exit(1);
        }
    };

    if verbose {
        println!("Transpiling {} to {}", input.display(), output.display());
        println!("Target: {}", target);
    }

    let transpiled = match target {
        "javascript" | "js" => match transofrm_code(&code) {
            Ok(result) => result,
            Err(err) => {
                eprintln!("Transpilation error: {}", err);
                std::process::exit(1);
            }
        },
        _ => {
            eprintln!("Error: Unsupported target language '{}'", target);
            eprintln!("Supported targets: javascript, js");
            std::process::exit(1);
        }
    };

    match fs::write(&output, transpiled) {
        Ok(_) => {
            if verbose {
                println!("Successfully transpiled to {}", output.display());
            }
        }
        Err(err) => {
            eprintln!("Error writing output file {}: {}", output.display(), err);
            std::process::exit(1);
        }
    }
}

pub fn check_file(file: PathBuf, verbose: bool) {
    let code = match fs::read_to_string(&file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file.display(), err);
            std::process::exit(1);
        }
    };

    if verbose {
        println!("Type checking file: {}", file.display());
    }

    let mut checker = Checker::new(new_checker_env());

    match get_checked_ast(&code, &mut checker) {
        Ok(_) => {
            println!("✓ Type checking passed");
        }
        Err(err) => {
            eprintln!("✗ Type checking failed: {}", err);
            std::process::exit(1);
        }
    }
}

pub fn parse_file(file: PathBuf, format: &str, verbose: bool) {
    let code = match fs::read_to_string(&file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file.display(), err);
            std::process::exit(1);
        }
    };

    if verbose {
        println!("Parsing file: {}", file.display());
        println!("Format: {}", format);
    }

    match parse_code(&code) {
        Ok(parser) => {
            for stmt in parser {
                match stmt {
                    Ok(stmt) => {
                        match format {
                            "debug" => println!("{:?}", stmt),
                            // "json" => {
                            //     // 需要实现Serialize trait
                            //     println!(
                            //         "{}",
                            //         serde_json::to_string_pretty(&stmt)
                            //             .unwrap_or_else(|_| format!("{:?}", stmt))
                            //     );
                            // }
                            "pretty" => {
                                // 需要实现pretty print
                                println!("{:#?}", stmt);
                            }
                            _ => println!("{:?}", stmt),
                        }
                    }
                    Err(err) => {
                        eprintln!("Parser error: {}", err);
                        std::process::exit(1);
                    }
                }
            }
        }
        Err(err) => {
            eprintln!("Parser error: {}", err);
            std::process::exit(1);
        }
    }
}

pub fn lex_file(file: PathBuf, verbose: bool) {
    let code = match fs::read_to_string(&file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file.display(), err);
            std::process::exit(1);
        }
    };

    if verbose {
        println!("Tokenizing file: {}", file.display());
    }

    for token in Lexer::new(&code) {
        match token {
            Ok(token) => println!("{}", token),
            Err(err) => {
                eprintln!("Lexer error: {}", err);
                std::process::exit(1);
            }
        }
    }
}

pub fn format_file(file: PathBuf, write_back: bool, verbose: bool) {
    let code = match fs::read_to_string(&file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {}: {}", file.display(), err);
            std::process::exit(1);
        }
    };

    if verbose {
        println!("Formatting file: {}", file.display());
    }

    // TODO: 实现格式化功能
    // let formatted = format_code(&code)?;

    if write_back {
        println!("Code formatting not implemented yet");
        // fs::write(&file, formatted)?;
    } else {
        println!("{}", code); // 暂时输出原始代码
    }
}
