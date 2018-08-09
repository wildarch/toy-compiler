extern crate toy_compiler;

use toy_compiler::parse::parse;
use toy_compiler::transpile_js::transpile;

use std::io::Write;
use std::process::{Command, Stdio};

#[test]
fn test_program() {
    let toy_source = r#"
fun main():
    print("Hello from main!")
    show_expr(5)
    show_if()
    show_loops()

fun show_expr(x):
    print("this is 3: " + (x - 2))

fun show_if():
    if 1 == 1:
        print("the universe is intact")

    if 1 == 0:
        print("the universe is broken")
    elif 1 == 1:
        print("the universe is intact (again)")

    if 1 == 0:
        print("the universe is really broken")
    elif 1 == 0:
        print("the universe is really really broken")
    else:
        print("the universe is still intact!")

fun show_loops():
    a = 3
    loop:
        print("loop: (3 > 0)" + a)
        if a == 0:
            break
        else:
            a = a - 1

    while a == 0:
        print("while: 0 == " + a)
        a = a + 1
    print("after while: 1 == " + a)

    for x in [0, 1, 3]:
        print("for: " + x)

"#;

    let expected = 
r#"Hello from main!
this is 3: 3
the universe is intact
the universe is intact (again)
the universe is still intact!
loop: (3 > 0)3
loop: (3 > 0)2
loop: (3 > 0)1
loop: (3 > 0)0
while: 0 == 0
after while: 1 == 1
for: 0
for: 1
for: 3
"#;

    let parsed = parse(&toy_source).expect("Parsing failed!");
    let transpiled = transpile(parsed);

    let mut node_process = Command::new("node")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to spawn nodejs process");
    {
        let stdin = node_process.stdin.as_mut().expect("Failed to open stdin");
        stdin.write_all(transpiled.as_bytes()).expect("Failed to write to stdin");
    }

    let output = node_process.wait_with_output().expect("Failed to read stdout");
    let std_output = String::from_utf8_lossy(&output.stdout);
    assert_eq!(std_output, expected);
}
