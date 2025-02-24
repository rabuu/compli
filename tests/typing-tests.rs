use std::io::Write;

use goldenfile::Mint;

fn typing_test(source: &str, goldenfile: &str) {
    let mut mint = Mint::new("tests/goldenfiles/typing");
    let mut goldenfile = mint.new_goldenfile(goldenfile).unwrap();

    let program = compli::parse(source).unwrap();
    let program = compli::type_check(program).unwrap();

    write!(goldenfile, "{program:?}").unwrap()
}

#[test]
fn minimal() {
    typing_test(include_str!("testfiles/minimal.compli"), "minimal.golden");
}

#[test]
fn complex_let() {
    typing_test(
        include_str!("testfiles/complex-let.compli"),
        "complex-let.golden",
    );
}
