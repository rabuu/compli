use std::fs;
use std::path::PathBuf;

use compli::parsing::ParsingError;
use miette::{bail, miette, Diagnostic, IntoDiagnostic, Result};

use clap::{Parser, ValueEnum};

use thiserror::Error;
use tracing::level_filters::LevelFilter;
use tracing::{info, warn};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;

use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;

use compli::{codegen, lowering, parsing, type_check};

#[derive(Debug, Parser)]
#[command(version, about = None, long_about = None)]
#[command(propagate_version = true)]
struct CliArgs {
    /// Path to the source code file
    input_file: PathBuf,

    /// Path to the output file
    #[arg(short, long)]
    output_file: Option<PathBuf>,

    /// Execution mode
    #[arg(value_enum)]
    #[arg(short, long)]
    #[arg(default_value_t = ExecutionMode::Compile)]
    mode: ExecutionMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum ExecutionMode {
    /// Compile the source code to machine code
    Compile,

    /// Inspect the AST of the parsed source code
    Parse,

    /// Inspect the IR of the lowered AST
    Ir,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Oops, something went wrong")]
struct AppError {
    #[related]
    errs: Vec<ParsingError>,

    #[source_code]
    code: String,
}

fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().without_time())
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::WARN.into())
                .from_env_lossy(),
        )
        .init();

    let args = CliArgs::parse();

    if !args.input_file.is_file() {
        bail!("No proper input file: {:?}", args.input_file);
    }

    let source = fs::read_to_string(&args.input_file).into_diagnostic()?;
    let program = match parsing::parse(&source) {
        Ok(program) => {
            info!("Parsing successful");
            program
        }
        Err(reports) => {
            return Err(AppError {
                errs: reports,
                code: source,
            }.into())
        }
    };

    if args.mode == ExecutionMode::Parse {
        println!("{program:#?}");
        return Ok(());
    }

    type_check(&program).into_diagnostic()?;

    let program = lowering::lower(program).unwrap();
    if args.mode == ExecutionMode::Ir {
        println!("{program:#?}");
        return Ok(());
    }

    let context = Context::create();
    let module = codegen::compile(&context, program);

    Target::initialize_x86(&InitializationConfig::default());

    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-linux-gnu"),
            "x86-64",
            "+avx2",
            opt,
            reloc,
            model,
        )
        .unwrap();

    let out = args.output_file.unwrap_or(PathBuf::from("myModule.o"));

    if out.exists() {
        warn!("{:?} already exists and will be overridden", &out);
    }

    target_machine
        .write_to_file(&module, FileType::Object, &out)
        .or_else(|_| bail!("Failed to write to file"))?;

    Ok(())
}
