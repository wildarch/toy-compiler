extern crate toy_compiler;
use toy_compiler::compile_mips;
use toy_compiler::parse;

fn main() {
    let program = r#"
fun main():
    a = 0
    loop:
        a = a + 1
        if a > 10:
            break
        elif a < 5:
            print("Very small ")
            continue
        print("Getting there..")

"#;
    let parsed = parse::parse(program).expect("failed to parse");
    let compiled = compile_mips::compile(parsed).expect("failed to compile for mips");
    println!("{}", compiled);
}
