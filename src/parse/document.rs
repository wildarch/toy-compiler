use nom::types::CompleteStr;

use super::common::newline;
use super::statement::{stmts, Stmt};

pub type Document = Vec<Stmt>;

named!(pub document<CompleteStr, Document>,
    terminated!(
        delimited!(
            many0!(newline),
            apply!(stmts, 0),
            many0!(newline)
        ),
        eof!()
    )
);
