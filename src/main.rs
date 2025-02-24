use std::fs;
use std::path::PathBuf;

use miette::{Diagnostic, Report, Result};
use thiserror::Error;

use clap::{Parser, ValueEnum};

use tracing::level_filters::LevelFilter;
use tracing::{info, warn};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;

use inkwell::context::Context;
use inkwell::targets::{
    FileType, InitializationConfig, Target, TargetMachine, TargetMachineOptions,
};

use compli::{
    compile, lower, parse, type_check, CodegenError, LoweringError, ParsingError, TypeCheckError,
};

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

    /// Don't log progress
    #[arg(short, long)]
    quiet: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum ExecutionMode {
    /// Compile the source code to machine code
    Compile,

    /// Inspect the AST of the parsed source code
    Ast,

    /// Inspect the AST after type checking
    TypedAst,

    /// Inspect the IR of the lowered AST
    Ir,
}

#[derive(Debug, Error, Diagnostic)]
enum AppError {
    #[error("Failed to read input file: {file_path:?}")]
    BadInput {
        file_path: PathBuf,

        #[help]
        context: String,
    },

    #[error("Couldn't write to output file: {file_path:?}")]
    BadOutput {
        file_path: PathBuf,

        #[help]
        context: Option<String>,
    },

    #[error("Couldn't initialize target: {triple}")]
    #[diagnostic(help("Only `x86_64-pc-linux-gnu` is tested"))]
    BadTarget { triple: String },

    #[error("Failed to parse the source code file")]
    ParsingError(#[related] Vec<ParsingError>),

    #[error("Type checking of the source code failed")]
    #[diagnostic(transparent)]
    TypeCheckError(#[from] TypeCheckError),

    #[error("Lowering of the AST to IR failed")]
    #[diagnostic(transparent)]
    LoweringError(#[from] LoweringError),

    #[error("Code generation failed")]
    #[diagnostic(transparent)]
    CodegenError(#[from] CodegenError),

    #[error("An I/O operation failed")]
    GenericIoError(#[from] std::io::Error),
}

fn main() -> Result<()> {
    // parse CLI arguments
    let args = CliArgs::parse();

    // initialize logger
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().without_time())
        .with(
            EnvFilter::builder()
                .with_default_directive(
                    if args.quiet {
                        LevelFilter::OFF
                    } else {
                        LevelFilter::INFO
                    }
                    .into(),
                )
                .from_env_lossy(),
        )
        .init();

    /* HANDLE INPUT */

    // read input file
    let source = fs::read_to_string(&args.input_file).map_err(|e| AppError::BadInput {
        file_path: args.input_file.clone(),
        context: e.to_string(),
    })?;
    info!("Reading of input file {:?} was successful", args.input_file);

    // determine output file
    let output_file = match args.output_file {
        Some(file) => file,
        None => {
            let basename = args.input_file.file_stem().ok_or(AppError::BadInput {
                file_path: args.input_file.clone(),
                context: String::from("Cannot read file name properly"),
            })?;
            PathBuf::from(basename).with_extension("o")
        }
    };

    // validate output file
    if output_file.exists() && !output_file.is_file() {
        return Err(AppError::BadOutput {
            file_path: output_file.clone(),
            context: Some(String::from("This is no file")),
        }
        .into());
    }

    /* INITIALIZE LLVM TARGET MACHINE */

    Target::initialize_x86(&InitializationConfig::default());
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).map_err(|_| AppError::BadTarget {
        triple: triple.to_string(),
    })?;
    let options = TargetMachineOptions::default();
    let target_machine = target
        .create_target_machine_from_options(&triple, options)
        .ok_or(AppError::BadTarget {
            triple: triple.to_string(),
        })?;

    /* RUN COMPILER PIPELINE */

    // helper function to build error messages
    let wrap_with_source = |err: AppError| {
        let report: Report = err.into();
        report.with_source_code(source.clone())
    };

    // run parser
    let program = parse(&source)
        .map_err(AppError::ParsingError)
        .map_err(wrap_with_source)?;
    info!("Parsing of source code file was successful");

    if args.mode == ExecutionMode::Ast {
        program.pretty_print().map_err(AppError::GenericIoError)?;
        return Ok(());
    }

    // run type checker
    let program = type_check(program)
        .map_err(AppError::TypeCheckError)
        .map_err(wrap_with_source)?;
    info!("Type checking was successful");

    if args.mode == ExecutionMode::TypedAst {
        program.pretty_print().map_err(AppError::GenericIoError)?;
        return Ok(());
    }

    // run lowering
    let program = lower(program)
        .map_err(AppError::LoweringError)
        .map_err(wrap_with_source)?;
    info!("Lowering to intermediate representation was successful");

    if args.mode == ExecutionMode::Ir {
        program.pretty_print().map_err(AppError::GenericIoError)?;
        return Ok(());
    }

    // run codegen
    let context = Context::create();
    let module = compile(&context, program).map_err(AppError::CodegenError)?;
    info!("Code generation was successful");

    /* WRITE OUTPUT FILE */

    // warn if existing output file gets overridden
    if output_file.exists() {
        warn!("{:?} already exists and will be overridden", &output_file);
    }

    // actually write output file
    target_machine
        .write_to_file(&module, FileType::Object, &output_file)
        .map_err(|_| AppError::BadOutput {
            file_path: output_file.clone(),
            context: None,
        })?;
    info!("Wrote object file to {output_file:?} successfully");

    Ok(())
}
