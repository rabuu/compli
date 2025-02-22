use std::io::Write;

use goldenfile::Mint;

fn lowering_test(source: &str, goldenfile: &str) {
    let mut mint = Mint::new("tests/goldenfiles/lowering");
    let mut goldenfile = mint.new_goldenfile(goldenfile).unwrap();

    let program = compli::parse(source).unwrap();
    let program = compli::type_check(program).unwrap();
    let program = compli::lower(program).unwrap();

    write!(goldenfile, "{program:?}").unwrap()
}

#[test]
fn minimal() {
    lowering_test(include_str!("testfiles/minimal.compli"), "minimal.golden");
}
