use crate::parse::{Expr, If, Stmt};

use super::common::indent;
use super::expression::transpile_expr;
use super::statement::transpile_stmts;

pub fn transpile_if(if_cond: If, indent_level: usize) -> String {
    // TODO check this works correctly when used with multiple statements
    if if_cond.cases.len() == 0 {
        return "".into();
    }
    let mut cases = if_cond.cases.into_iter();
    let (base_cond, base_body) = cases.next().unwrap();

    let if_head = indent(
        format!("if({}) {{", transpile_expr(base_cond)),
        indent_level,
    );
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
    let head = indent(
        format!("else if({}) {{", transpile_expr(cond)),
        indent_level,
    );
    let mut body = transpile_stmts(body, indent_level + 1);
    let foot = indent("}".into(), indent_level);

    body.insert(0, head);
    body.push(foot);
    body
}

#[cfg(test)]
mod tests {
    use crate::parse::Expr;
    use crate::parse::Expr::*;
    use crate::parse::Ident;
    use crate::parse::If;
    use crate::parse::Lit::*;
    use crate::parse::Op::*;
    use crate::parse::Stmt::*;

    #[test]
    fn transpile_if() {
        assert_eq!(
            super::transpile_if(
                If {
                    cases: vec![
                        (
                            BinOp(
                                Box::new(Expr::Ident(Ident("a".into()))),
                                Eq,
                                Box::new(Lit(Bool(true)))
                            ),
                            vec![Break]
                        ),
                        (Expr::Ident(Ident("b".into())), vec![Continue])
                    ],
                    else_case: Some(vec![Expr(Call(Ident("test".into()), vec![]))])
                },
                0
            ),
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
}
