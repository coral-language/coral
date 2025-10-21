//! Command-line interface for Coral.

use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// Coral - A fast interpreted/compiled language with elegant syntax
#[derive(Parser)]
#[command(name = "coral")]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,

    /// File to run (when no subcommand is specified)
    pub file: Option<PathBuf>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Check a Coral file for errors (parse, semantic, type check)
    Check {
        /// Path to the Coral file
        file: PathBuf,

        /// Strict mode: treat all errors (including recoverable ones) as fatal
        #[arg(short, long)]
        strict: bool,
    },

    /// Format a Coral file (coming soon)
    #[command(hide = true)]
    Format {
        /// Path to the Coral file
        file: PathBuf,
    },
}

impl Cli {
    /// Resolve the actual command to run
    pub fn resolve_command(&self) -> ResolvedCommand {
        match &self.command {
            Some(Commands::Check { file, strict }) => ResolvedCommand::Check {
                file: file.clone(),
                strict: *strict,
            },
            Some(Commands::Format { file }) => ResolvedCommand::Format { file: file.clone() },
            None => {
                if let Some(file) = &self.file {
                    // Direct file execution: coral file.coral
                    ResolvedCommand::Run { file: file.clone() }
                } else {
                    // No file and no subcommand: start REPL
                    ResolvedCommand::Repl
                }
            }
        }
    }
}

/// Resolved command after processing CLI arguments
pub enum ResolvedCommand {
    Run { file: PathBuf },
    Check { file: PathBuf, strict: bool },
    Format { file: PathBuf },
    Repl,
}
