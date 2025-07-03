use clap::{Parser, Subcommand};
use mihama::utils::RunningMode;
use std::path::PathBuf;

use mihama::cli::*;
use mihama::repl::*;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(name = "mihama")]
#[command(about = "A modern functional programming language with dependent types")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Start interactive REPL
    Repl {
        /// Initial mode (lexer/parser/checker/evaluator/unsafe)
        #[arg(short, long, default_value = "evaluator")]
        mode: String,
    },
    /// Run a Mihama source file
    Run {
        /// Source file path
        file: PathBuf,
        /// Running mode
        #[arg(short, long, default_value = "evaluator")]
        mode: String,
        /// Show type information
        #[arg(short, long)]
        types: bool,
    },
    /// Transpile source file to target language
    Trans {
        /// Source file path
        input: PathBuf,
        /// Output file path
        output: PathBuf,
        /// Target language (javascript/python/c)
        #[arg(short, long, default_value = "javascript")]
        target: String,
    },
    /// Check source file for type errors
    Check {
        /// Source file path
        file: PathBuf,
        /// Show detailed type information
        #[arg(short, long)]
        verbose: bool,
    },
    /// Parse and display AST
    Parse {
        /// Source file path
        file: PathBuf,
        /// Output format (debug/json/pretty)
        #[arg(short, long, default_value = "debug")]
        format: String,
    },
    /// Tokenize and display tokens
    Lex {
        /// Source file path
        file: PathBuf,
    },
    /// Format source code
    Format {
        /// Source file path
        file: PathBuf,
        /// Write result back to file
        #[arg(short, long)]
        write: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    if let Some(command) = cli.command {
        match command {
            Commands::Repl { mode } => {
                run_repl(parse_mode(&mode), cli.verbose);
            }
            Commands::Run { file, mode, types } => {
                run_file(file, parse_mode(&mode), types, cli.verbose);
            }
            Commands::Trans {
                input,
                output,
                target,
            } => {
                transpile_file(input, output, &target, cli.verbose);
            }
            Commands::Check { file, verbose } => {
                check_file(file, verbose || cli.verbose);
            }
            Commands::Parse { file, format } => {
                parse_file(file, &format, cli.verbose);
            }
            Commands::Lex { file } => {
                lex_file(file, cli.verbose);
            }
            Commands::Format { file, write } => {
                format_file(file, write, cli.verbose);
            }
        }
    } else {
        // 默认进入REPL
        run_repl(RunningMode::Evaluator, cli.verbose);
    }
}
