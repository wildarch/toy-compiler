use nom::types::CompleteStr;

use super::common::comma_sep;
use super::identifier::{ident, Ident};
use super::literal::{lit, Lit};

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Ident(Ident),
    BinOp(Box<Expr>, Op, Box<Expr>),
    Call(Ident, Vec<Expr>),
}
use self::Expr::*;

#[derive(Debug)]
pub enum Op {
    Add,
    Min,
    Eq,
}
use self::Op::*;

named!(op<CompleteStr, Op>,
    alt!(
        tag!("+") => { |_| Add } |
        tag!("-") => { |_| Min } |
        tag!("==") => { |_| Eq }
    )
);

named!(bin_op<CompleteStr, Expr>,
    do_parse!(
        left: atom >>
        o: ws!(op) >>
        right: expr >>
        (BinOp(Box::new(left), o, Box::new(right)))
    )
);

named!(call<CompleteStr, Expr>,
    do_parse!(
        name: ident >>
        tag!("(") >>
        args: separated_list!(comma_sep, expr) >>
        tag!(")") >>
        (Call(name, args))
    )
);

named!(expr_ident<CompleteStr, Expr>,
    map!(
        ident, 
        |s| Expr::Ident(s)
    )
);

named!(pub expr<CompleteStr, Expr>,
    alt!(
        bin_op 
        | call 
        | expr_ident
        | lit => { |s| Lit(s) }
        | delimited!(tag!("("), expr, tag!(")"))
    )
);

// Prevents infinite recursion in bin_op parsing.
// bin_op starts by parsing an atom instead of an expr to prevent infinite recursion
// on bin_op
named!(atom<CompleteStr, Expr>,
    alt!(
        call
        | expr_ident
        | lit => { |s| Lit(s) }
        | delimited!(tag!("("), ws!(bin_op), tag!(")"))
    )
);

#[cfg(test)]
mod tests {
    #[test]
    fn call() {
        let parsed = super::call(r#"print("this is 3: " + (x - 2))"#.into());
        println!("{:?}", parsed);
        /* TODO make work
        assert_eq!(
            Ok(
                (
                    CompleteStr(""), 
                    Call(
                        Ident("print".into()),
                        vec![
                            BinOp(
                                Box::new(Str("this is 3: ")),
                                Add,
                                BinOp(
                                    Box::new(Ident("x".into())),
                                    Min,
                                    Lit(Int(2))
                                )
                            )
                        ]
                    )
                )
            ),
            parsed
        );
        */
    }

    #[test]
    fn bin_op() {
        let parsed = super::bin_op(r#""this is 3: " + (x - 2)"#.into());
        println!("{:?}", parsed);
    }
}
