use nom::types::CompleteStr;
use nom::space;

use loops::*;
use if_cond::*;
use if_cond::If;
use function::*;
use expression::{expr, Expr};
use identifier::{ident, Ident};
use common::{newline, indent};

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
