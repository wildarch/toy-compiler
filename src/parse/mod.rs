use nom::types::CompleteStr;
use nom::Err;

mod common;
mod document;
mod expression;
mod function;
mod identifier;
mod if_cond;
mod literal;
mod loops;
mod statement;

pub use self::document::Document;
pub use self::expression::{Expr, Op};
pub use self::identifier::Ident;
pub use self::if_cond::If;
pub use self::literal::Lit;
pub use self::statement::Stmt;

use self::document::document;

// TODO
// * struct
// * enum

pub fn parse<'a>(input: &'a str) -> Result<Document, Err<CompleteStr<'a>>> {
    document(input.into()).map(|(_i, r)| r)
}
