extern crate toy_compiler;

use toy_compiler::parse;

use toy_compiler::Lit::*;
use toy_compiler::Lit;

fn transpile_lit(l: Lit) -> String {
    match l {
        Int(i) => i.to_string(),
        Bool(b) => b.to_string(),
        Float(f) => f.to_string(),
        Str(s) => format!("\"{}\"", s),
        List(vals) => transpile_list(vals)
    }
}

fn transpile_list(vals: Vec<Expr>) -> String {
    let vals_trans: Vec<String> = vals.into_iter()
        .map(transpile_expr)
        .collect();
    format!("[{}]", vals_trans.join(", "))
}

use toy_compiler::Ident;

const JS_KEYWORDS: &'static [&'static str] = &[
	"do",
	"if",
	"in",
	"for",
	"let",
	"new",
	"try",
	"var",
	"case",
	"else",
	"enum",
	"eval",
	"null",
	"this",
	"true",
	"void",
	"with",
	"await",
	"break",
	"catch",
	"class",
	"const",
	"false",
	"super",
	"throw",
	"while",
	"yield",
	"delete",
	"export",
	"import",
	"public",
	"return",
	"static",
	"switch",
	"typeof",
	"default",
	"extends",
	"finally",
	"package",
	"private",
	"continue",
	"debugger",
	"function",
	"arguments",
	"interface",
	"protected",
	"implements",
	"instanceof"
];

fn transpile_ident(i: Ident) -> String {
    if JS_KEYWORDS.contains(&i.0.as_str()) {
        format!("${}", i.0)
    } else {
        i.0
    }
}

use toy_compiler::Op::*;
use toy_compiler::Op;

fn transpile_binop(op: Op) -> String {
    match op {
        Add => "+".into(),
        Min => "-".into(),
        Eq => "===".into()
    }
}

fn transpile_call(fun_name: Ident, args: Vec<Expr>) -> String {
    let args_trans: Vec<String> = args.into_iter()
        .map(|e| transpile_expr(e))
        .collect();
    format!("{}({})", fun_name.0, args_trans.join(", "))
}

use toy_compiler::Expr::*;
use toy_compiler::Expr;

fn transpile_expr(e: Expr) -> String {
    match e {
        Lit(l) => transpile_lit(l),
        Expr::Ident(i) => transpile_ident(i),
        BinOp(left, op, right) => format!("({} {} {})", 
            transpile_expr(*left),
            transpile_binop(op),
            transpile_expr(*right)
        ),
        Call(fun_name, args) => transpile_call(fun_name, args)
    }
}

// --- Statements
// TODO use let instead of var (and keep track of redeclarations)
fn transpile_assign(var: Ident, val: Expr) -> String {
    format!("var {} = {}", transpile_ident(var), transpile_expr(val))
}

fn is_block(stmt: &Stmt) -> bool {
    match stmt {
        Expr(_) => false,
        Assign(_, _) => false,
        Fun(_, _, _) => true,
        Return(_) => false,
        If(_) => true,
        While(_, _) => true,
        Break => false,
        Continue => false,
        Loop(_) => true,
        For(_, _, _) => true
    }
}

fn transpile_stmts(stmts: Vec<Stmt>, indent_level: usize) -> Vec<String> {
    stmts.into_iter()
        .map(|stmt| {
            let block = is_block(&stmt);
            let trans = transpile_stmt(stmt, indent_level);
            if block {
                trans
            }
            else {
                format!("{};", trans)
            }
        })
        .collect()
}

fn transpile_fun(fun_name: Ident, args: Vec<Ident>, 
                 body: Vec<Stmt>, indent_level: usize) -> String {
    let args_trans: Vec<String> = args.into_iter().map(|s| s.0).collect();
    let head = indent(
        format!(
            "function {}({}) {{", 
            fun_name.0, 
            args_trans.join(", ")
        ), 
        indent_level
    );

    let foot = indent("}".into(), indent_level);

    let mut body_trans = transpile_stmts(body, indent_level + 1);

    body_trans.insert(0, head);
    body_trans.push(foot);

    body_trans.join("\n")

}

use toy_compiler::If;
fn transpile_if(if_cond: If, indent_level: usize) -> String {
    // TODO check this works correctly when used with multiple statements
    if if_cond.cases.len() == 0 {
        return "".into();
    }
    let mut cases = if_cond.cases.into_iter();
    let (base_cond, base_body) = cases.next().unwrap();

    let if_head = indent(format!("if({}) {{", transpile_expr(base_cond)), indent_level);
    let mut if_trans = transpile_stmts(base_body, indent_level + 1);
    let if_foot = indent("}".into(), indent_level);

    if_trans.insert(0, if_head);
    if_trans.push(if_foot.clone());

    for (case_cond, case_body) in cases {
        if_trans.append(&mut transpile_elif(case_cond, case_body, indent_level));
    }

    if let Some(else_body) = if_cond.else_case {
        let else_head = indent("else {".into(), indent_level);
        let mut else_trans = transpile_stmts(else_body, indent_level + 1);
        let else_foot = if_foot;

        if_trans.push(else_head);
        if_trans.append(&mut else_trans);
        if_trans.push(else_foot);

    }

    if_trans.join("\n")
}

fn transpile_elif(cond: Expr, body: Vec<Stmt>, indent_level: usize) -> Vec<String> {
    let head = indent(format!("else if({}) {{", transpile_expr(cond)), indent_level);
    let mut body = transpile_stmts(body, indent_level + 1);
    let foot = indent("}".into(), indent_level);

    body.insert(0, head);
    body.push(foot);
    body
}

fn transpile_while(cond: Expr, body: Vec<Stmt>, indent_level: usize) -> String {
    let head = indent(format!("while({}) {{", transpile_expr(cond)), indent_level);
    let mut body_trans = transpile_stmts(body, indent_level + 1);
    let foot = indent("}".into(), indent_level);

    body_trans.insert(0, head);
    body_trans.push(foot);
    body_trans.join("\n")
}

fn transpile_loop(body: Vec<Stmt>, indent_level: usize) -> String {
    let head = indent("while(true) {".into(), indent_level);
    let mut body_trans = transpile_stmts(body, indent_level + 1);
    let foot = indent("}".into(), indent_level);

    body_trans.insert(0, head);
    body_trans.push(foot);
    body_trans.join("\n")
}

fn transpile_for(var: Ident, list: Expr, body: Vec<Stmt>, indent_level: usize) -> String {
    let head = indent(
        format!(
            "for(var {} of {}) {{", 
            transpile_ident(var),
            transpile_expr(list)
        ),
        indent_level
    );
    let mut body_trans = transpile_stmts(body, indent_level + 1);
    let foot = indent("}".into(), indent_level);

    body_trans.insert(0, head);
    body_trans.push(foot);
    body_trans.join("\n")
}

fn indent(mut s: String, level: usize) -> String {
    for _ in 0..level {
        s.insert_str(0, "    ");
    }
    s
}

use toy_compiler::Stmt::*;
use toy_compiler::Stmt;

fn transpile_stmt(s: Stmt, indent_level: usize) -> String {
    match s {
        Expr(e) => indent(transpile_expr(e), indent_level),
        Assign(var, val) => indent(transpile_assign(var, val), indent_level),
        Fun(fun_name, args, body) => transpile_fun(fun_name, args, body, indent_level),
        Return(e) => indent(format!("return {}", transpile_expr(e)), indent_level),
        Break => indent("break".into(), indent_level),
        Continue => indent("continue".into(), indent_level),
        If(i) => transpile_if(i, indent_level),
        While(cond, body) => transpile_while(cond, body, indent_level),
        Loop(body) => transpile_loop(body, indent_level),
        For(var, list, body) => transpile_for(var, list, body, indent_level),
    }
}

#[cfg(test)]
mod tests {
    use toy_compiler::Lit::*;
    #[test]
    fn transpile_lit() {
        assert_eq!(super::transpile_lit(Int(1)), "1");
    }

    use toy_compiler::Ident;
    #[test]
    fn transpile_ident() {
        assert_eq!(super::transpile_ident(Ident("instanceof".into())), "$instanceof");
    }

    use toy_compiler::Op::*;
    use toy_compiler::Expr::*;
    #[test]
    fn transpile_binop() {
        assert_eq!(
            super::transpile_expr(
                BinOp(
                    Box::new(Lit(Bool(false))), 
                    Eq, 
                    Box::new(Lit(Float(3.14)))
                )
            ), 
            "(false === 3.14)"
        );
    }

    use toy_compiler::Expr;
    #[test]
    fn transpile_call() {
        assert_eq!(
            super::transpile_call(
                Ident("print".into()), 
                vec![
                    BinOp(
                        Box::new(Expr::Ident(Ident("x".into()))),
                        Add,
                        Box::new(Lit(Int(2)))
                    ),
                    Lit(Str("test".into()))
                ]
            ),
            "print((x + 2), \"test\")"
        );
    }

    use toy_compiler::Stmt::*;
    #[test]
    fn transpile_assign() {
        assert_eq!(
            super::transpile_assign(
                Ident("yield".into()),
                BinOp(
                    Box::new(
                        Call(
                            Ident("calc".into()),
                            vec![Expr::Ident(Ident("x".into()))]
                        )
                    ),
                    Min,
                    Box::new(
                        Lit(Int(42))
                    )
                )
            ),
            "var $yield = (calc(x) - 42)"
        );
    }

    #[test]
    fn transpile_fun() {
        assert_eq!(
            super::transpile_fun(
                Ident("test".into()),
                vec![
                    Ident("a".into()), 
                    Ident("b".into())
                ],
                vec![
                    Return(Lit(Int(1)))
                ],
                1
            ),
r#"    function test(a, b) {
        return 1;
    }"#
        );
    }

    use toy_compiler::If;

    #[test]
    fn transpile_if() {
        assert_eq!(
            super::transpile_if(If {
                cases: vec![
                    (
                        BinOp(
                            Box::new(Expr::Ident(Ident("a".into()))),
                            Eq,
                            Box::new(Lit(Bool(true)))
                        ),
                        vec![Break]
                    ),
                    (
                        Expr::Ident(Ident("b".into())),
                        vec![Continue]
                    )
                ],
                else_case: Some(vec![
                    Expr(Call(Ident("test".into()), vec![]))
                ])
            }, 0),
r#"if((a === true)) {
    break;
}
else if(b) {
    continue;
}
else {
    test();
}"#
        );
    }

    #[test]
    fn transpile_while() {
        assert_eq!(
            super::transpile_while(
                Expr::Ident(Ident("a".into())),
                vec![While(
                    Expr::Ident(Ident("b".into())),
                    vec![Break]
                )],
                2
            ),
r#"        while(a) {
            while(b) {
                break;
            }
        }"#
        );
    }

    #[test]
    fn transpile_loop() {
        assert_eq!(
            super::transpile_loop(
                vec![
                    Expr(
                        BinOp(
                            Box::new(Lit(Int(5))),
                            Min,
                            Box::new(Lit(Int(1)))
                        )
                    )
                ],
                0
            ),
r#"while(true) {
    (5 - 1);
}"#
        );
    }

    #[test]
    fn transpile_for() {
        assert_eq!(
            super::transpile_for(
                Ident("x".into()),
                Lit(List(vec![
                    Lit(Int(0)),
                    Lit(Int(1))
                ])),
                vec![
                    Expr(Call(
                        Ident("print".into()),
                        vec![
                            Expr::Ident(Ident("x".into()))
                        ]
                    ))
                ],
                0
            ),
r#"for(var x of [0, 1]) {
    print(x);
}"#
        );
    }

}

fn main() {
    let program = 
    r#"
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
    let parsed = parse(&program);

    eprintln!("{:?}", parsed);

    if let Ok(stmts) = parsed {
        let transpiled = transpile_stmts(stmts, 0).join("\n\n");

        let std = r#"
function print(x) {
    console.log(x);
}

main();
            "#;

        println!("{}\n{}", std, transpiled);

    }
}
