extern crate toy_compiler;
use toy_compiler::compile_mips;
use toy_compiler::parse;

fn main() {
    let program = r#"
fun fib(n):
    a = 0
    a2 = 1
    c = 0
    while c < n:
        c = c + 1
        t = a + a2
        a = a2
        a2 = t
    return a
        

fun main():
    for i in [10, 20, 30, 35]:
        print_int(fib(i))
"#;
    let parsed = parse::parse(program).expect("failed to parse");
    let compiled = compile_mips::compile(parsed).expect("failed to compile for mips");
    println!("{}", compiled);
}
