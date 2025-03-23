pub fn run_pipeline(src: &str) {
    let tokens = compli::lex(src).unwrap();
    let ast = compli::parse(&tokens, src.len()).unwrap();
    let typed_ast = compli::type_check(ast).unwrap();
    let ir = compli::lower(typed_ast).unwrap();
    let context = inkwell::context::Context::create();
    let _module = compli::compile(&context, ir).unwrap();
}
