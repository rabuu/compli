fn run_pipeline(src: &str) {
    let ast = compli::parse(src).unwrap();
    let typed_ast = compli::type_check(ast).unwrap();
    let ir = compli::lower(typed_ast).unwrap();
    let context = inkwell::context::Context::create();
    let _module = compli::compile(&context, ir).unwrap();
}

#[test]
fn language_tour() {
    run_pipeline(include_str!("../examples/language-tour.compli"));
}

#[test]
fn fib() {
    run_pipeline(include_str!("../examples/fib.compli"));
}

#[test]
fn approx_pi() {
    run_pipeline(include_str!("../examples/approx-pi.compli"));
}

#[test]
fn records() {
    run_pipeline(include_str!("../examples/records.compli"));
}

#[test]
fn casting() {
    run_pipeline(include_str!("../examples/casting.compli"));
}

#[test]
fn quadratic_formula() {
    run_pipeline(include_str!("../examples/quadratic-formula.compli"));
}
