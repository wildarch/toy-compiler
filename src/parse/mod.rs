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

pub use self::literal::Lit;
pub use self::expression::{Expr, Op};
pub use self::identifier::Ident;
pub use self::statement::Stmt;
pub use self::if_cond::If;
pub use self::document::Document;

use self::document::document;


// TODO
// * unit tests
// * struct
// * enum

pub fn parse<'a>(input: &'a str) -> Result<Document, Err<CompleteStr<'a>>> {
    document(input.into()).map(|(_i, r)| r)
}
