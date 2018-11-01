use nom::space;
use nom::types::CompleteStr;

use super::common::{indent, newline};
use super::expression::expr;
use super::identifier::ident;
use super::statement::Stmt::*;
use super::statement::{stmts, Stmt};

named_args!(pub while_loop(indent_level: usize)<CompleteStr, Stmt>,
    do_parse!(
        apply!(indent, indent_level) >>
        tag!("while") >>
        cond: ws!(expr) >>
        tag!(":") >>
        newline >>
        body: apply!(stmts, indent_level + 1) >>
        (While(cond, body))
    )
);

named_args!(pub loop_loop(indent_level: usize)<CompleteStr, Stmt>,
    do_parse!(
        apply!(indent, indent_level) >>
        tag!("loop:") >>
        newline >>
        body: apply!(stmts, indent_level + 1) >>
        (Loop(body))
    )
);

named_args!(pub for_loop(indent_level: usize)<CompleteStr, Stmt>,
    do_parse!(
        apply!(indent, indent_level) >>
        tag!("for") >>
        space >>
        var: ident >>
        ws!(tag!("in")) >>
        list: expr >>
        tag!(":") >>
        newline >>
        body: apply!(stmts, indent_level + 1) >>
        (For(var, list, body))
    )
);

named_args!(pub break_or_continue(indent_level: usize)<CompleteStr, Stmt>,
    preceded!(
        apply!(indent, indent_level),
        alt!(
            tag!("break") => { |_| Break }
            | tag!("continue") => { |_| Continue }
        )
    )
);
