use crate::parse::Stmt::*;
use crate::parse::{Expr, Ident, Stmt};

use super::common::indent;
use super::expression::transpile_expr;
use super::function::transpile_fun;
use super::identifier::transpile_ident;
use super::if_cond::transpile_if;
use super::loops::*;

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
        For(_, _, _) => true,
    }
}

pub fn transpile_stmts(stmts: Vec<Stmt>, indent_level: usize) -> Vec<String> {
    stmts
        .into_iter()
        .map(|stmt| {
            let block = is_block(&stmt);
            let trans = transpile_stmt(stmt, indent_level);
            if block {
                trans
            } else {
                format!("{};", trans)
            }
        }).collect()
}

pub fn transpile_stmt(s: Stmt, indent_level: usize) -> String {
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
    use crate::parse::Expr;
    use crate::parse::Expr::*;
    use crate::parse::Ident;
    use crate::parse::Lit::*;
    use crate::parse::Op::*;

    #[test]
    fn transpile_assign() {
        assert_eq!(
            super::transpile_assign(
                Ident("yield".into()),
                BinOp(
                    Box::new(Call(
                        Ident("calc".into()),
                        vec![Expr::Ident(Ident("x".into()))]
                    )),
                    Min,
                    Box::new(Lit(Int(42)))
                )
            ),
            "var $yield = (calc(x) - 42)"
        );
    }

}
