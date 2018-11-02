extern crate toy_compiler;
use toy_compiler::compile_mips;
use toy_compiler::parse;

fn main() {
    let program = r#"
fun main():
    say_hello()

fun say_hello():
    print("Hello, world!")
"#;
    let parsed = parse::parse(program).expect("failed to parse");
    let compiled = compile_mips::compile(parsed).expect("failed to compile for mips");
    println!("{}", compiled);
}
