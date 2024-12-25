use std::collections::HashMap;
use std::path::Path;

use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;

use codegen::Codegen;

use self::ast::Lowerer;

mod ast;
mod codegen;
mod ir;
mod variable;

fn main() {
    let expr = ast::Expression::Call {
        name: "if".to_string(),
        args: vec![
            ast::Expression::Bool(false),
            ast::Expression::Int(42),
            ast::Expression::Call {
                name: "+".to_string(),
                args: vec![ast::Expression::Var("x".to_string()), ast::Expression::Int(2)],
            },
        ],
    };

    let expr = ast::Expression::LetIn {
        var: "x".to_string(),
        bind: Box::new(ast::Expression::Int(3)),
        body: Box::new(expr),
    };

    let mut lowerer = Lowerer::default();
    let expr = lowerer.lower_expression(expr, &HashMap::new());

    eprintln!("{expr:#?}");

    let context = Context::create();
    let codegen = Codegen::new(&context);
    let module = codegen.compile(expr);

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

    assert!(target_machine
        .write_to_file(&module, FileType::Object, Path::new("myModule.o"))
        .is_ok());
}
