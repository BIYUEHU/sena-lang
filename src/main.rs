use parser::lexer::Lexer;
use std::io::Write;

const PROMPT: &str = ">> ";

fn main() {
    println!("Welcome to the Tamaki programming language REPL!");
    println!("Input '.read <file>' to parse from a file");
    println!("Input '.exit' to exit the REPL");

    let mut input = String::new();
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

        for token in Lexer::new(code.as_str()) {
            match token {
                Ok(token) => println!(" -> {}", token),
                Err(err) => println!(" {}", err),
            }
        }
    }
}
