extern crate toy_compiler;
use toy_compiler::compile_mips;
use toy_compiler::parse;

fn main() {
    let program = r#"
fun main():
    a = 3 + 4 + 5
    string = "Hello, world!"
    print_int(a)
    print(string)
    if a > 10:
        print("a hella big!")
    else:
        print("lol a tiny")
"#;
    let parsed = parse::parse(program).expect("failed to parse");
    let compiled = compile_mips::compile(parsed).expect("failed to compile for mips");
    println!("{}", compiled);
}
