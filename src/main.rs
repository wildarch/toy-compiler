extern crate toy_compiler;
use toy_compiler::parse;

fn main() {
    let program = r#"
fun main():
    a = 1
    print_int(a)
"#;
    let parsed = parse::parse(program).expect("failed to parse");
    println!("Parsed: {:#?}", parsed);
}
