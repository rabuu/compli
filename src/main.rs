use std::fs;
use std::path::PathBuf;

use miette::{Diagnostic, Report, Result};

use clap::{Parser, ValueEnum};

use thiserror::Error;
use tracing::level_filters::LevelFilter;
use tracing::{info, warn};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;

use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::OptimizationLevel;

use compli::{codegen, lowering, parsing, type_checking};

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

    #[error("Couldn't initialize target machine")]
    NoTargetMachine,

    #[error("Failed to parse the source code file")]
    ParsingError(#[related] Vec<parsing::ParsingError>),

    #[error("Type checking of the source code failed")]
    #[diagnostic(transparent)]
    TypeCheckError(#[from] type_checking::TypeCheckError),
}

fn main() -> Result<()> {
    // initialize logger
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().without_time())
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::WARN.into())
                .from_env_lossy(),
        )
        .init();

    /* HANDLE INPUT */

    // gather CLI arguments
    let args = CliArgs::parse();

    // read input file
    let source = fs::read_to_string(&args.input_file).map_err(|e| AppError::BadInput {
        file_path: args.input_file.clone(),
        context: e.to_string(),
    })?;
    info!("Reading of input file {:?} successful", args.input_file);

    // validate output file
    let output_file = args.output_file.unwrap_or(PathBuf::from("myModule.o"));
    if !output_file.is_file() {
        return Err(AppError::BadOutput {
            file_path: output_file.clone(),
            context: Some(String::from("This is no file")),
        }
        .into());
    }

    /* INITIALIZE LLVM TARGET MACHINE */

    Target::initialize_x86(&InitializationConfig::default());
    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            "x86-64",
            "+avx2",
            opt,
            reloc,
            model,
        )
        .ok_or(AppError::NoTargetMachine)?;

    /* RUN COMPILER PIPELINE */

    // helper function to build error messages
    let wrap_with_source = |err: AppError| {
        let report: Report = err.into();
        report.with_source_code(source.clone())
    };

    // run parser
    let program = parsing::parse(&source)
        .map_err(AppError::ParsingError)
        .map_err(wrap_with_source)?;
    info!("Parsing of source code file was successful");

    if args.mode == ExecutionMode::Parse {
        println!("{program:#?}");
        return Ok(());
    }

    // run type checker
    type_checking::type_check(&program)
        .map_err(AppError::TypeCheckError)
        .map_err(wrap_with_source)?;
    info!("Type checking was successful");

    // run lowering
    let program = lowering::lower(program).unwrap(); // TODO: error handling
    info!("Lowering to intermediate representation was successful");

    if args.mode == ExecutionMode::Ir {
        println!("{program:#?}");
        return Ok(());
    }

    // run codegen
    let context = Context::create();
    let module = codegen::compile(&context, program); // TODO: error handling
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
