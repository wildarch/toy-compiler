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
mod document;
mod common;

pub use literal::Lit;
pub use expression::{Expr, Op};
pub use identifier::Ident;
pub use statement::Stmt;
pub use if_cond::If;
pub use document::Document;

use document::document;

// TODO
// * unit tests
// * struct
// * enum

pub fn parse<'a>(input: &'a str) -> Result<Document, Err<CompleteStr<'a>>> {
//    document(input.into()).map(|(_i, r)| r)
//    Let's be extra sure for now
    document(input.into()).map(|(i, r)| {
        // All input must be consumed!
        assert_eq!(i, CompleteStr(""));
        r
    })
}
