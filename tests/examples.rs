mod common;
use common::run_pipeline;

#[test]
fn minimal() {
    run_pipeline(include_str!("../examples/minimal.compli"));
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

#[test]
fn input() {
    run_pipeline(include_str!("../examples/input.compli"));
}
