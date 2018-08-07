#[macro_use]
extern crate nom;
use nom::types::CompleteStr;
use nom::Err;

mod literal;
mod expression;
mod identifier;
mod statement;
mod if_cond;
mod loops;
mod function;
mod common;

use statement::{stmts, Stmt};
use common::newline;

// TODO
// * struct
// * enum

pub type Document = Vec<Stmt>;

named!(document<CompleteStr, Document>,
    terminated!(
        delimited!(
            many0!(newline),
            apply!(stmts, 0),
            many0!(newline)
        ),
        eof!()
    )
);

pub fn parse<'a>(input: &'a str) -> Result<Document, Err<CompleteStr<'a>>> {
//    document(input.into()).map(|(_i, r)| r)
//    Let's be extra sure for now
    document(input.into()).map(|(i, r)| {
        // All input must be consumed!
        assert_eq!(i, CompleteStr(""));
        r
    })
}
