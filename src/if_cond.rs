use nom::types::CompleteStr;

use expression::{Expr, expr};
use statement::{Stmt, stmts};
use common::{newline, indent};

#[derive(Debug)]
pub struct If {
    pub cases: Vec<(Expr, Vec<Stmt>)>,
    pub else_case: Option<Vec<Stmt>>
}

named_args!(pub ifelse(indent_level: usize)<CompleteStr, If>,
    do_parse!(
        apply!(indent, indent_level) >>
        tag!("if") >>
        base_cond: ws!(expr) >>
        tag!(":") >>
        newline >>
        base_body: apply!(stmts, indent_level + 1) >>
        other_cases: many0!(
            preceded!(
                newline,
                apply!(elif, indent_level)
            )
        ) >>
        else_case: opt!(
            preceded!(
                newline,
                apply!(parse_else, indent_level)
            )
        ) >>
        (If{
            cases: prepend((base_cond, base_body), other_cases),
            else_case: else_case
        })
    )
);

fn prepend<T>(first: T, mut vec: Vec<T>) -> Vec<T> {
    vec.insert(0, first);
    vec
}

named_args!(pub elif(indent_level: usize)<CompleteStr, (Expr, Vec<Stmt>)>,
    do_parse!(
        apply!(indent, indent_level) >> 
        tag!("elif") >>
        cond: ws!(expr) >>
        tag!(":") >>
        newline >>
        body: apply!(stmts, indent_level + 1) >>
        (cond, body)
    )
);

named_args!(pub parse_else(indent_level: usize)<CompleteStr, Vec<Stmt>>,
    do_parse!(
        apply!(indent, indent_level) >>
        tag!("else:") >>
        newline >>
        body: apply!(stmts, indent_level + 1) >>
        (body)
    )
);
