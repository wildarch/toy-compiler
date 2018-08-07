extern crate learn_nom;

use learn_nom::parse;

fn main() {
    let program = 
    r#"
fun main(args, smth):
    print("Hello, world!")
    res = do_smth("input: " + args)
    print(res)
    return 0

fun do_smth(x):
    a = x + 5
    process(a)
    if a == 1:
        print("a is 1")
    elif a == 2:
        print("a is 2")
    else:
        print("a is greater than 2")
        while a:
            print("loop the loop!")
            if a == b:
                break
            else:
                continue
    loop:
        print("loop the real loop")
    for char in "Hello, world":
        print("character: " + char)

    for x in [0, 1, 2, 3, 4, 5]:
        print(x)
    return a
"#;

    println!("{:?}", parse(&program));
}
