use nom::types::CompleteStr;
use nom::space;

use super::common::{indent, newline, comma_sep};
use super::statement::Stmt::{Fun, Return};
use super::statement::{Stmt, stmts};
use super::expression::expr;
use super::identifier::ident;

named_args!(pub fun(indent_level: usize)<CompleteStr, Stmt>,
    do_parse!(
        apply!(indent, indent_level) >>
        tag!("fun ") >>
        name: ident >>
        tag!("(") >>
        args: separated_list!(comma_sep, ident) >>
        tag!("):") >>
        newline >>
        // TODO properly handle indents
        body: apply!(stmts, indent_level + 1) >>
        (Fun(name, args, body))
    )
);

named_args!(pub ret(indent_level: usize)<CompleteStr, Stmt>,
    do_parse!(
        apply!(indent, indent_level) >>
        tag!("return") >>
        space >>
        val: expr >>
        (Return(val))
    )
);