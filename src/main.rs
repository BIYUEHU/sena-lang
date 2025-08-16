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

    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    Repl {
        #[arg(short, long, default_value = "evaluator")]
        mode: String,
    },
    Run {
        file: PathBuf,
        #[arg(short, long, default_value = "evaluator")]
        mode: String,
        #[arg(short, long)]
        types: bool,
    },
    Trans {
        input: PathBuf,
        output: PathBuf,
        #[arg(short, long, default_value = "javascript")]
        target: String,
    },
    Check {
        file: PathBuf,
        #[arg(short, long)]
        verbose: bool,
    },
    Parse {
        file: PathBuf,
        #[arg(short, long, default_value = "debug")]
        format: String,
    },
    Lex {
        file: PathBuf,
    },
    Format {
        file: PathBuf,
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
        run_repl(RunningMode::Evaluator, cli.verbose);
    }
}
