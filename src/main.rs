extern crate toy_compiler;

fn main() {
    /*
        let program = r#"
    fun main():
        print_int(fib(2))

    fun fib(n):
        if n == 0:
            return 1
        if n == 1:
            return 1
        return fib(n-2) + fib(n-1)
    "#;
        let parsed = parse::parse(program).expect("failed to parse");
        let compiled = compile_mips::compile(parsed).expect("failed to compile for mips");
        println!("{}", compiled);
        */
    unimplemented!()
}
