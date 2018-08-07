use nom::types::CompleteStr;
use nom::digit;

use expression::{Expr, expr};
use common::comma_sep;

#[derive(Debug)]
pub enum Lit {
    Int(i32),
    Bool(bool),
    Float(f32),
    Str(String),
    List(Vec<Expr>),
}

use self::Lit::*;

named!(int<CompleteStr, Lit>, 
	map!(
		recognize!(
			do_parse!(
				opt!(tag!("-")) >>
				digit >>
				()
			)
		),
		|s: CompleteStr| Int(s.parse().unwrap())
	)
);

named!(boolean<CompleteStr, Lit>,
	alt!(
		tag!("true") => { |_| Bool(true) } |
		tag!("false") => { |_| Bool(false) }
	)
);

named!(float<CompleteStr, Lit>,
	map!(
		recognize!(
			do_parse!(
				opt!(tag!("-")) >>
				digit >>
				()
			)
		),
		|s: CompleteStr| Float(s.parse().unwrap())
	)
);

//TODO use escaped! here
named!(string<CompleteStr, Lit>,
	map!(
		delimited!(
			tag!("\""),
			take_until!("\""),
			tag!("\"")
		),
		|s: CompleteStr| Str(s.0.into())
	)
);

named!(list<CompleteStr, Lit>,
    map!(
        delimited!(
            tag!("["),
            separated_list!(
                comma_sep,
                expr
            ),
            tag!("]")
        ),
        |s: Vec<Expr>| List(s)
    )
);

named!(pub lit<CompleteStr, Lit>, 
    alt!(
        int
        | boolean
        | float
        | string
        | list
    )
);
