mod common;
use common::run_pipeline;

#[test]
#[should_panic]
fn fail_lexing() {
    run_pipeline(include_str!("fails/fail_lexing.compli"));
}

#[test]
#[should_panic]
fn fail_parsing() {
    run_pipeline(include_str!("fails/fail_parsing.compli"));
}

#[test]
#[should_panic]
fn fail_typechecking() {
    run_pipeline(include_str!("fails/fail_typechecking.compli"));
}

#[test]
#[should_panic]
fn fail_lowering() {
    run_pipeline(include_str!("fails/fail_lowering.compli"));
}
