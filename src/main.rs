use std::io::Write;
use std::path::{Path, PathBuf};
use std::{fs, iter, process};

use miette::{Diagnostic, Report, Result};
use thiserror::Error;

use clap::{Parser, Subcommand, ValueEnum};

use tracing::level_filters::LevelFilter;
use tracing::{info, warn};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    FileType, InitializationConfig, Target, TargetMachine, TargetMachineOptions,
};

const RUNTIME_FILE: &str = include_str!("../runtime.c");

#[derive(Debug, Parser)]
#[command(version, about = None, long_about = None)]
struct CliArgs {
    #[command(subcommand)]
    mode: Mode,

    /// Don't log progress
    #[arg(short, long)]
    quiet: bool,
}

#[derive(Debug, Subcommand)]
enum Mode {
    /// Build an executable from the source code (compile & link)
    ///
    /// This will call a system C compiler to link with the runtime.
    #[command(alias = "b")]
    Build {
        /// Path to the source code file
        input_file: PathBuf,

        /// Path to the output file
        #[arg(short, long)]
        output_file: Option<PathBuf>,

        /// Path to a custom C compiler
        #[arg(long)]
        c_compiler: Option<PathBuf>,
    },

    /// Generate an object/assembly file from the source code (compile-only)
    #[command(alias = "gen")]
    Generate {
        /// Path to the source code file
        input_file: PathBuf,

        /// Path to the output file
        #[arg(short, long)]
        output_file: Option<PathBuf>,

        /// Output file type
        #[arg(value_enum, short, long, default_value_t = OutputFileType::Object)]
        filetype: OutputFileType,
    },

    /// Inspect the AST of a parsed source code file
    #[command(alias = "ast")]
    InspectAst {
        /// Path to the source code file
        input_file: PathBuf,

        /// Include type information (needs type checking)
        #[arg(short, long)]
        typed: bool,
    },

    /// Inspect the intermediate representation of a source code file
    #[command(alias = "ir")]
    InspectIr {
        /// Path to the source code file
        input_file: PathBuf,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum OutputFileType {
    Object,
    Assembly,
}

impl OutputFileType {
    fn extension(&self) -> &'static str {
        match self {
            OutputFileType::Object => "o",
            OutputFileType::Assembly => "asm",
        }
    }
}

impl From<OutputFileType> for FileType {
    fn from(value: OutputFileType) -> Self {
        match value {
            OutputFileType::Object => Self::Object,
            OutputFileType::Assembly => Self::Assembly,
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
enum AppError {
    #[error("Failed to read input file {file_path:?}")]
    BadInput {
        file_path: PathBuf,

        #[help]
        context: String,
    },

    #[error("Cannot determine output file name (source file has no .compli extension)")]
    #[diagnostic(help("Provide an output file name manually with the -o flag"))]
    CantDetermineOutputFile,

    #[error("Cannot write to output file {file_path:?}")]
    BadOutput {
        file_path: PathBuf,

        #[help]
        context: Option<String>,
    },

    #[error("Couldn't initialize target: {triple}")]
    #[diagnostic(help("Only `x86_64-pc-linux-gnu` is tested"))]
    BadTarget { triple: String },

    #[error("Failed to parse the source code file")]
    ParsingError(#[related] Vec<compli::ParsingError>),

    #[error("Type checking of the source code failed")]
    #[diagnostic(transparent)]
    TypeCheckError(#[from] compli::TypeCheckError),

    #[error("Lowering of the AST to IR failed")]
    #[diagnostic(transparent)]
    LoweringError(#[from] compli::LoweringError),

    #[error("Code generation failed")]
    #[diagnostic(transparent)]
    CodegenError(#[from] compli::CodegenError),

    #[error("Compiling/linking failed")]
    #[diagnostic(help("Try to specify a custom C compiler"))]
    CompileOrLinkError,

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

    match args.mode {
        Mode::Build {
            input_file,
            output_file,
            c_compiler,
        } => {
            let output_file = determine_output_file(&input_file, output_file, None)?;
            let target_machine = initialize_llvm_target_machine()?;

            let source = read_input_file(&input_file)?;
            let tokens = lex(&source)?;

            let context = Context::create();
            let module = codegen(
                &context,
                lower(type_check(parse(&tokens, &source)?, &source)?, &source)?,
            )?;

            let tempdir = tempfile::tempdir().map_err(AppError::GenericIoError)?;

            let module_path = tempdir.path().join("module.o");
            write_module_to_file(
                &module_path,
                &target_machine,
                &module,
                OutputFileType::Object,
            )?;

            let runtime_path = tempdir.path().join("runtime.c");
            let mut runtime_file =
                fs::File::create(&runtime_path).map_err(AppError::GenericIoError)?;
            write!(runtime_file, "{RUNTIME_FILE}").map_err(AppError::GenericIoError)?;

            compile_runtime_and_link(
                &module_path,
                &runtime_path,
                &output_file,
                c_compiler.as_ref(),
            )?;
        }
        Mode::Generate {
            input_file,
            output_file,
            filetype,
        } => {
            let output_file = determine_output_file(&input_file, output_file, Some(filetype))?;
            let target_machine = initialize_llvm_target_machine()?;

            let source = read_input_file(&input_file)?;
            let tokens = lex(&source)?;

            let context = Context::create();
            let module = codegen(
                &context,
                lower(type_check(parse(&tokens, &source)?, &source)?, &source)?,
            )?;

            write_module_to_file(&output_file, &target_machine, &module, filetype)?;
        }
        Mode::InspectAst { input_file, typed } => {
            let source = read_input_file(&input_file)?;
            let tokens = lex(&source)?;
            let ast = parse(&tokens, &source)?;

            if typed {
                let typed_ast = type_check(ast, &source)?;
                typed_ast.pretty_print().map_err(AppError::GenericIoError)?;
            } else {
                ast.pretty_print().map_err(AppError::GenericIoError)?;
            }
        }
        Mode::InspectIr { input_file } => {
            let source = read_input_file(&input_file)?;
            let tokens = lex(&source)?;
            let ir = lower(type_check(parse(&tokens, &source)?, &source)?, &source)?;
            ir.pretty_print().map_err(AppError::GenericIoError)?;
        }
    }

    Ok(())
}

fn read_input_file(input_file: &PathBuf) -> Result<String, AppError> {
    let source = fs::read_to_string(input_file).map_err(|e| AppError::BadInput {
        file_path: input_file.clone(),
        context: e.to_string(),
    })?;
    info!("Reading of input file {:?} was successful", input_file);

    Ok(source)
}

fn determine_output_file(
    input_file: &Path,
    output_file: Option<PathBuf>,
    filetype: Option<OutputFileType>,
) -> Result<PathBuf, AppError> {
    if let Some(file) = output_file {
        return Ok(file);
    }

    match input_file.extension() {
        Some(ext) if ext == "compli" => (),
        _ => return Err(AppError::CantDetermineOutputFile),
    }

    let basename = input_file.file_stem().ok_or(AppError::BadInput {
        file_path: input_file.to_path_buf(),
        context: String::from("Cannot read input file name properly"),
    })?;

    let output_file = match filetype {
        Some(ft) => PathBuf::from(basename).with_extension(ft.extension()),
        None => PathBuf::from(basename),
    };

    if output_file.exists() && !output_file.is_file() {
        return Err(AppError::BadOutput {
            file_path: output_file.to_path_buf(),
            context: Some(String::from("The specified path is no valid file")),
        });
    }

    Ok(output_file)
}

fn initialize_llvm_target_machine() -> Result<TargetMachine, AppError> {
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

    Ok(target_machine)
}

fn with_source(err: AppError, source: &str) -> Report {
    let report: Report = err.into();
    report.with_source_code(source.to_string())
}

fn lex(source: &str) -> Result<Vec<(compli::Token, compli::Span)>> {
    let tokens = compli::lex(source)
        .map_err(AppError::ParsingError)
        .map_err(|err| with_source(err, source))?;
    info!("Lexing of source code was successful");

    Ok(tokens)
}

fn parse<'src, 'tok: 'src>(
    tokens: &'tok [(compli::Token<'src>, compli::Span)],
    source: &'src str,
) -> Result<compli::UntypedAst<'src>> {
    let ast = compli::parse(tokens, source.len())
        .map_err(AppError::ParsingError)
        .map_err(|err| with_source(err, source))?;
    info!("Parsing of source code was successful");

    Ok(ast)
}

fn type_check<'src>(
    ast: compli::UntypedAst<'src>,
    source: &'src str,
) -> Result<compli::TypedAst<'src>> {
    let typed_ast = compli::type_check(ast)
        .map_err(AppError::TypeCheckError)
        .map_err(|err| with_source(err, source))?;
    info!("Type checking was successful");

    Ok(typed_ast)
}

fn lower<'src>(
    typed_ast: compli::TypedAst<'src>,
    source: &'src str,
) -> Result<compli::IntermediateRepresentation<'src>> {
    let ir = compli::lower(typed_ast)
        .map_err(AppError::LoweringError)
        .map_err(|err| with_source(err, source))?;
    info!("Lowering to intermediate representation was successful");

    Ok(ir)
}

fn codegen<'ctx>(
    context: &'ctx Context,
    ir: compli::IntermediateRepresentation,
) -> Result<Module<'ctx>> {
    let module = compli::compile(context, ir).map_err(AppError::CodegenError)?;
    info!("Code generation was successful");

    Ok(module)
}

fn write_module_to_file(
    output_file: &Path,
    target_machine: &TargetMachine,
    module: &Module,
    filetype: OutputFileType,
) -> Result<()> {
    if output_file.exists() {
        warn!("{output_file:?} already exists and will be overwritten");
    }

    target_machine
        .write_to_file(module, filetype.into(), output_file)
        .map_err(|_| AppError::BadOutput {
            file_path: output_file.to_path_buf(),
            context: None,
        })?;
    info!("Generation of module file {output_file:?} was successful");

    Ok(())
}

fn compile_runtime_and_link(
    module_path: &Path,
    runtime_path: &Path,
    output_file: &Path,
    c_compiler: Option<&PathBuf>,
) -> Result<(), AppError> {
    if output_file.exists() {
        warn!("{output_file:?} already exists and will be overwritten");
    }

    let compilers: &[PathBuf] = &["cc", "gcc", "clang"].map(PathBuf::from);
    for compiler in iter::once(c_compiler).flatten().chain(compilers) {
        let status = process::Command::new(compiler)
            .arg("-o")
            .arg(output_file)
            .arg(runtime_path)
            .arg(module_path)
            .status();

        if let Ok(status) = status {
            if status.success() {
                info!("Compiling and linking with runtime to {output_file:?} was successful");
                return Ok(());
            }
        }
    }

    Err(AppError::CompileOrLinkError)
}
