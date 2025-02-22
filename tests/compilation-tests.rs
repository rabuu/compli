use std::io::Write;

use goldenfile::Mint;
use inkwell::context::Context;
use inkwell::targets::{
    FileType, InitializationConfig, Target, TargetMachine, TargetMachineOptions,
};

fn compile_test(source: &str, goldenfile: &str) {
    let mut mint = Mint::new("tests/goldenfiles/compilation");
    let mut goldenfile = mint.new_goldenfile(goldenfile).unwrap();

    Target::initialize_x86(&InitializationConfig::default());
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let options = TargetMachineOptions::default();
    let target_machine = target
        .create_target_machine_from_options(&triple, options)
        .unwrap();

    let program = compli::parse(source).unwrap();
    let program = compli::type_check(program).unwrap();
    let program = compli::lower(program).unwrap();

    let context = Context::create();
    let module = compli::compile(&context, program).unwrap();

    let output = target_machine
        .write_to_memory_buffer(&module, FileType::Assembly)
        .unwrap();
    goldenfile.write_all(output.as_slice()).unwrap();
}

#[test]
fn minimal() {
    compile_test(include_str!("testfiles/minimal.compli"), "minimal.golden");
}
