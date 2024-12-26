use std::fs;

use anyhow::{anyhow, bail, Result};
use ariadne::sources;
use clap::Parser;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;
use tracing::level_filters::LevelFilter;
use tracing::info;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;

use compli::codegen::Codegen;
use compli::lowering::Lowerer;
use compli::parsing;

mod cli;

fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::WARN.into())
                .from_env_lossy(),
        )
        .init();

    let args = cli::Args::parse();

    if !args.input_file.is_file() {
        bail!("No proper input file: {:?}", args.input_file);
    }

    let source = fs::read_to_string(&args.input_file)?;
    let input_file = args
        .input_file
        .to_str()
        .ok_or(anyhow!("Bad input path"))?
        .to_string();

    let program = match parsing::parse(&source, input_file.clone()) {
        Ok(program) => {
            info!("Parsing successful");
            program
        }
        Err(reports) => {
            for report in reports {
                report
                    .eprint(sources([(input_file.clone(), &source)]))
                    .unwrap();
            }
            bail!("Parsing failed");
        }
    };

    if args.mode == cli::Mode::Parse {
        println!("{program:#?}");
        return Ok(());
    }

    // let mut lowerer = Lowerer::default();
    // let program = lowerer.lower_program(program);
    //
    // let context = Context::create();
    // let codegen = Codegen::new(&context);
    // let module = codegen.compile(program);
    //
    // Target::initialize_x86(&InitializationConfig::default());
    //
    // let opt = OptimizationLevel::Default;
    // let reloc = RelocMode::Default;
    // let model = CodeModel::Default;
    // let target = Target::from_name("x86-64").unwrap();
    // let target_machine = target
    //     .create_target_machine(
    //         &TargetTriple::create("x86_64-pc-linux-gnu"),
    //         "x86-64",
    //         "+avx2",
    //         opt,
    //         reloc,
    //         model,
    //     )
    //     .unwrap();
    //
    // assert!(target_machine
    //     .write_to_file(&module, FileType::Object, Path::new("myModule.o"))
    //     .is_ok());

    Ok(())
}
