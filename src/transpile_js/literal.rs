use parse::Lit::*;
use parse::{Lit, Expr};

use super::expression::transpile_expr;

pub fn transpile_lit(l: Lit) -> String {
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

#[cfg(test)]
mod tests {
    use parse::Lit::*;
    #[test]
    fn transpile_lit() {
        assert_eq!(super::transpile_lit(Int(1)), "1");
    }

}
