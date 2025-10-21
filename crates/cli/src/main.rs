mod cli;
mod pipeline;

use clap::Parser;
use cli::{Cli, ResolvedCommand};
use pipeline::{check_file, format_file, run_file, start_repl};
use std::process;

fn main() {
    let cli = Cli::parse();

    let result = match cli.resolve_command() {
        ResolvedCommand::Run { file } => run_file(&file).map_err(|e| e.to_string()),
        ResolvedCommand::Check { file, strict } => {
            check_file(&file, strict).map_err(|e| e.to_string())
        }
        ResolvedCommand::Format { file } => format_file(&file).map_err(|e| e.to_string()),
        ResolvedCommand::Repl => start_repl().map_err(|e| e.to_string()),
    };

    if let Err(error) = result {
        if !error.is_empty() {
            eprintln!("{}", error);
        }
        process::exit(1);
    }
}
