use std::io::Write;

use lexer2::TokenResult;

mod lexer2;

const PROMPT: &str = ">> ";

fn main() {
    let mut input = String::new();
    loop {
        print!("{}", PROMPT);
        if std::io::stdout().flush().is_err() {
            println!("error: flush err")
        }
        input.clear();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                let lexer = lexer2::Lexer::new(input.as_str());
                for token in lexer {
                    match token {
                        TokenResult::Token(token) => println!(
                            " -> type: {} | literal: {:?}",
                            token.lexeme, token.kind
                        ),
                        TokenResult::Err(err) => println!(" syntax error: {}", err)
                    }
                }
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
