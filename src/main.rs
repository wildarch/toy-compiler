extern crate toy_compiler;
use toy_compiler::compile_mips;
use toy_compiler::parse;

fn main() {
    let program = r#"
fun fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-2) + fib(n-1)

fun main():
    for i in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
        print_int(fib(i))
"#;
    let parsed = parse::parse(program).expect("failed to parse");
    let compiled = compile_mips::compile(parsed).expect("failed to compile for mips");
    println!("{}", compiled);
}
