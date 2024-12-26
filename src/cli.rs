use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Debug, Parser)]
#[command(version, about = None, long_about = None)]
#[command(propagate_version = true)]
pub struct Args {
    /// Source code input file
    pub input_file: PathBuf,

    /// Execution mode
    #[arg(value_enum)]
    #[arg(short, long)]
    #[arg(default_value_t = Mode::Compile)]
    pub mode: Mode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum Mode {
    /// Compile the source code to machine code
    Compile,

    /// Inspect the AST of the parsed source code
    Parse,
}
