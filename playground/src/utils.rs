use std::{cell::RefCell, rc::Rc};

use mihama::{
    checker::Checker,
    env::env::{new_checker_env, new_evaluator_env},
    evaluator::{
        object::{Object, PrettyPrint, TypeInfo},
        Evaluator,
    },
    lexer::Lexer,
    utils::{eval_code, get_checked_ast, parse_code, unsafe_eval_code, RunningMode},
};

use crate::example::{DEMO_CODE, FIBONACII_CODE, MATCH_CODE};

pub fn run_code(code: String, mode: RunningMode, view_type_info: bool) -> Result<String, String> {
    let mut evaluator = Evaluator::new(new_evaluator_env());
    let mut checker = Checker::new(new_checker_env());
    let mut result = String::new();
    let stdout = Rc::new(RefCell::new(String::new()));
    let stdout2 = Rc::clone(&stdout);

    evaluator.set_custom_func(
        "print".to_string(),
        Box::new(move |args| {
            stdout2.borrow_mut().push_str(&format!(
                "{}\n",
                args.iter()
                    .map(|arg| arg.pretty_print())
                    .collect::<Vec<_>>()
                    .join(" ")
            ));
            Ok(Object::Unit)
        }),
    );
    evaluator.set_custom_func(
        "get_timestrap".to_string(),
        Box::new(|_| Ok(Object::Int(1))),
    );

    match mode {
        RunningMode::Lexer => {
            for token in Lexer::new(&code) {
                match token {
                    Ok(token) => result.push_str(&format!(" -> {}\n", token)),
                    Err(err) => result.push_str(&format!("{}\n", err)),
                }
            }
        }
        RunningMode::Parser => match parse_code(&code) {
            Ok(parser) => {
                for stmt in parser {
                    match stmt {
                        Ok(stmt) => result.push_str(&format!(" : {:?}\n", stmt)),
                        Err(err) => result.push_str(&format!("{}\n", err)),
                    }
                }
            }
            Err(err) => return Err(format!("Parser error: {}\n", err)),
        },
        RunningMode::Checker => match get_checked_ast(&code, &mut checker) {
            Ok(program) => {
                for stmt in program {
                    result.push_str(&format!(" : {:?}\n", stmt))
                }
            }
            Err(err) => return Err(format!("Checker error: {}\n", err)),
        },
        RunningMode::Evaluator => match eval_code(&code, &mut checker, &mut evaluator) {
            Ok(value) => result.push_str(&if view_type_info {
                format!("> {} : {}\n", value.pretty_print(), value.type_info())
            } else {
                format!("> {}\n", value.to_string())
            }),
            Err(err) => return Err(err),
        },
        RunningMode::UnsafeEvaluator => match unsafe_eval_code(&code, &mut evaluator) {
            Ok(value) => {
                result.push_str(&if view_type_info {
                    format!("> {} : {}\n", value.pretty_print(), value.type_info())
                } else {
                    format!("> {}\n", value.to_string())
                });
            }
            Err(err) => return Err(err),
        },
    };
    Ok(format!("{}{}", stdout.borrow(), result))
}

pub fn get_example_code(name: String) -> &'static str {
    match name.as_str() {
        "fibonacii" => FIBONACII_CODE,
        "match" => MATCH_CODE,
        "demo" => DEMO_CODE,
        "hello_world" => {
            r#"let main:Unit = print("Hello, world!")
"#
        }
        _ => "",
    }
}
