use nom::types::CompleteStr;
use nom::{alpha, alphanumeric};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

const KEYWORDS: &'static [&'static str] = &[
    "fun", "if", "elif", "else", "return", "while", "break", "continue", "loop", "for",
];

named!(pub ident<CompleteStr, Ident>, 
    map!(
        verify!(
            recognize!(
                preceded!(
                    many1!(alt!(tag!("_") | alpha)),
                    many0!(alt!(tag!("_") | alphanumeric))
                )
            ),
            |s: CompleteStr| !KEYWORDS.contains(&s.0)
        ),
        |s: CompleteStr| Ident(s.0.to_string())
    )
);
