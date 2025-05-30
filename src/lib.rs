pub mod checker;
pub mod env;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod utils;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        println!("{}", fib(22220))
    }

    fn fib(x: i32) -> i32 {
        if x <= 2 {
            x
        } else {
            fib(x - 1) + fib(x - 2)
        }
    }
}
