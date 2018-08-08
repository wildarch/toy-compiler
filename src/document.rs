use nom::types::CompleteStr;

use statement::{stmts, Stmt};
use common::newline;

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

