use std::path::Path;

use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple};
use inkwell::OptimizationLevel;
use inkwell::context::Context;

use codegen::Codegen;

mod codegen;
mod ir;

fn main() {
    let expr1 = ir::Expression::Direct(ir::Value::Boolean(true));
    let expr2 = ir::Expression::Direct(ir::Value::Number(-42));
    let expr3 = ir::Expression::Operation {
        kind: ir::OpKind::Add,
        lhs: Box::new(ir::Expression::Direct(ir::Value::Number(1))),
        rhs: Box::new(ir::Expression::Direct(ir::Value::Number(2))),
    };
    let expr4 = ir::Expression::IfThenElse { cond: Box::new(expr1), then_clause: Box::new(expr2), else_clause: Box::new(expr3) };

    let context = Context::create();
    let codegen = Codegen::new(&context);
    let module = codegen.compile(expr4);

    Target::initialize_x86(&InitializationConfig::default());

    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target.create_target_machine(
        &TargetTriple::create("x86_64-pc-linux-gnu"),
        "x86-64",
        "+avx2",
        opt,
        reloc,
        model
    ).unwrap();

    assert!(target_machine.write_to_file(&module, FileType::Assembly, Path::new("test.asm")).is_ok());
}
