use std::io::Write;

use goldenfile::Mint;

fn parsing_test(source: &str, goldenfile: &str) {
    let mut mint = Mint::new("tests/goldenfiles/parsing");
    let mut goldenfile = mint.new_goldenfile(goldenfile).unwrap();

    let program = compli::parse(source).unwrap();

    write!(goldenfile, "{program:?}").unwrap()
}

#[test]
fn minimal() {
    parsing_test(include_str!("testfiles/minimal.compli"), "minimal.golden");
}

#[test]
fn complex_let() {
    parsing_test(include_str!("testfiles/complex-let.compli"), "complex-let.golden");
}
