use parse::{Ident, Stmt};

use super::common::indent;
use super::statement::transpile_stmts;

pub fn transpile_fun(
    fun_name: Ident,
    args: Vec<Ident>,
    body: Vec<Stmt>,
    indent_level: usize,
) -> String {
    let args_trans: Vec<String> = args.into_iter().map(|s| s.0).collect();
    let head = indent(
        format!("function {}({}) {{", fun_name.0, args_trans.join(", ")),
        indent_level,
    );

    let foot = indent("}".into(), indent_level);

    let mut body_trans = transpile_stmts(body, indent_level + 1);

    body_trans.insert(0, head);
    body_trans.push(foot);

    body_trans.join("\n")
}

#[cfg(test)]
mod tests {
    use parse::Expr::*;
    use parse::Ident;
    use parse::Lit::*;
    use parse::Stmt::*;

    #[test]
    fn transpile_fun() {
        assert_eq!(
            super::transpile_fun(
                Ident("test".into()),
                vec![Ident("a".into()), Ident("b".into())],
                vec![Return(Lit(Int(1)))],
                1
            ),
            r#"    function test(a, b) {
        return 1;
    }"#
        );
    }

}
