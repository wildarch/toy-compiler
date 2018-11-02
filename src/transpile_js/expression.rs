use parse::Expr::*;
use parse::Op::*;
use parse::{Expr, Ident, Op};

use super::identifier::transpile_ident;
use super::literal::transpile_lit;

fn transpile_call(fun_name: Ident, args: Vec<Expr>) -> String {
    let args_trans: Vec<String> = args.into_iter().map(|e| transpile_expr(e)).collect();
    format!("{}({})", fun_name.0, args_trans.join(", "))
}

pub fn transpile_expr(e: Expr) -> String {
    match e {
        Lit(l) => transpile_lit(l),
        Expr::Ident(i) => transpile_ident(i),
        BinOp(left, op, right) => format!(
            "({} {} {})",
            transpile_expr(*left),
            transpile_binop(op),
            transpile_expr(*right)
        ),
        Call(fun_name, args) => transpile_call(fun_name, args),
    }
}

fn transpile_binop(op: Op) -> String {
    match op {
        Add => "+".into(),
        Min => "-".into(),
        Eq => "===".into(),
        Gt => ">".into(),
    }
}

#[cfg(test)]
mod tests {
    use parse::Expr::*;
    use parse::Lit::*;
    use parse::Op::*;
    use parse::{Expr, Ident};

    #[test]
    fn transpile_binop() {
        assert_eq!(
            super::transpile_expr(BinOp(
                Box::new(Lit(Bool(false))),
                Eq,
                Box::new(Lit(Float(3.14)))
            )),
            "(false === 3.14)"
        );
    }

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

}
