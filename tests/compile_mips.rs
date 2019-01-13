extern crate tempfile;
extern crate toy_compiler;

use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;
use toy_compiler::compile_mips;
use toy_compiler::parse;

fn verify_program_output(program: &str, expected_output: &str) {
    let parsed = parse::parse(program).expect("failed to parse");
    let compiled = compile_mips::compile(parsed).expect("failed to compile for mips");
    let mut file = NamedTempFile::new().expect("failed to create temporary file");
    file.write(compiled.to_string().as_bytes())
        .expect("failed to write program to file");

    let path = file
        .path()
        .to_str()
        .expect("failed to get path to temporary file");

    let output = Command::new("spim")
        .arg("-file")
        .arg(path)
        .output()
        .expect("failed to execute spim")
        .stdout;
    let mut output = String::from_utf8(output).expect("output was not valid utf-8");
    let target = "exceptions.s\n";
    let newline = output
        .find(target)
        .expect("load exception header not found");
    let output = output.split_off(newline + target.len());
    println!("{}", output);
    assert_eq!(output, expected_output);
}

#[test]
fn recursion_fibonacci() {
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
    let output = r#"0
1
1
2
3
5
8
13
21
34
55
"#;
    verify_program_output(program, output);
}

#[test]
fn while_fibonacci() {
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
    let output = r#"55
6765
832040
9227465
"#;
    verify_program_output(program, output);
}

#[test]
fn print_string() {
    let program = r#"
fun main():
    str = "Hello, world!"
    print(str)
"#;
    let output = "Hello, world!\n";
    verify_program_output(program, output);
}

#[test]
fn variable_aliasing() {
    let program = r#"
fun sub_fn(a):
    print_int(a)
    a = 2
    print_int(a)

fun main():
    sub_fn(1)
"#;
    let output = "1\n2\n";
    verify_program_output(program, output);
}
