use crate::parse::{Expr, Ident, Stmt};

use super::common::indent;
use super::expression::transpile_expr;
use super::identifier::transpile_ident;
use super::statement::transpile_stmts;

pub fn transpile_while(cond: Expr, body: Vec<Stmt>, indent_level: usize) -> String {
    let head = indent(format!("while({}) {{", transpile_expr(cond)), indent_level);
    let mut body_trans = transpile_stmts(body, indent_level + 1);
    let foot = indent("}".into(), indent_level);

    body_trans.insert(0, head);
    body_trans.push(foot);
    body_trans.join("\n")
}

pub fn transpile_loop(body: Vec<Stmt>, indent_level: usize) -> String {
    let head = indent("while(true) {".into(), indent_level);
    let mut body_trans = transpile_stmts(body, indent_level + 1);
    let foot = indent("}".into(), indent_level);

    body_trans.insert(0, head);
    body_trans.push(foot);
    body_trans.join("\n")
}

pub fn transpile_for(var: Ident, list: Expr, body: Vec<Stmt>, indent_level: usize) -> String {
    let head = indent(
        format!(
            "for(var {} of {}) {{",
            transpile_ident(var),
            transpile_expr(list)
        ),
        indent_level,
    );
    let mut body_trans = transpile_stmts(body, indent_level + 1);
    let foot = indent("}".into(), indent_level);

    body_trans.insert(0, head);
    body_trans.push(foot);
    body_trans.join("\n")
}

#[cfg(test)]
mod tests {
     use crate::parse::Expr;
     use crate::parse::Expr::*;
     use crate::parse::Ident;
     use crate::parse::Lit::*;
     use crate::parse::Op::*;
     use crate::parse::Stmt::*;

    #[test]
    fn transpile_while() {
        assert_eq!(
            super::transpile_while(
                Expr::Ident(Ident("a".into())),
                vec![While(Expr::Ident(Ident("b".into())), vec![Break])],
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
                vec![Expr(BinOp(
                    Box::new(Lit(Int(5))),
                    Min,
                    Box::new(Lit(Int(1)))
                ))],
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
                Lit(List(vec![Lit(Int(0)), Lit(Int(1))])),
                vec![Expr(Call(
                    Ident("print".into()),
                    vec![Expr::Ident(Ident("x".into()))]
                ))],
                0
            ),
            r#"for(var x of [0, 1]) {
    print(x);
}"#
        );
    }

}
