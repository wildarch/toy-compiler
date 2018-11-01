use nom::space;
use nom::types::CompleteStr;

use super::common::{indent, newline};
use super::expression::{expr, Expr};
use super::function::*;
use super::identifier::{ident, Ident};
use super::if_cond::If;
use super::if_cond::*;
use super::loops::*;

#[derive(Debug)]
pub enum Stmt {
    Assign(Ident, Expr),
    Expr(Expr),
    Fun(Ident, Vec<Ident>, Vec<Stmt>),
    Return(Expr),
    If(If),
    While(Expr, Vec<Stmt>),
    Break,
    Continue,
    Loop(Vec<Stmt>),
    For(Ident, Expr, Vec<Stmt>),
}
use self::Stmt::*;

named_args!(assign(indent_size: usize)<CompleteStr, Stmt>,
    do_parse!(
        apply!(indent, indent_size) >>
        var: ident >>
        ws!(tag!("=")) >>
        val: expr >>
        (Assign(var, val))
    )
);

named_args!(stmt(indent_level: usize)<CompleteStr, Stmt>,
   alt!(
       apply!(ret, indent_level)
       | apply!(assign, indent_level)
       | preceded!(apply!(indent, indent_level), expr) => { |e| Stmt::Expr(e) }
       | apply!(fun, indent_level)
       | apply!(ifelse, indent_level) => { |i| Stmt::If(i) }
       | apply!(while_loop, indent_level)
       | apply!(break_or_continue, indent_level)
       | apply!(loop_loop, indent_level)
       | apply!(for_loop, indent_level)
   )
);

named_args!(pub stmts(indent_level: usize)<CompleteStr, Vec<Stmt>>,
    separated_list!(
        preceded!(
            newline,
            blanks
        ),
        apply!(stmt, indent_level)
    )
);

named!(blanks<CompleteStr, Vec<Vec<CompleteStr>>>,
    many0!(
        terminated!(
           many0!(space),
           newline
        )
    )
);
