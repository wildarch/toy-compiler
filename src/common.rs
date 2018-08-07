use nom::space;
use nom::types::CompleteStr;

named!(pub comma_sep<CompleteStr, Option<CompleteStr>>,
    preceded!(
        tag!(","),
        opt!(space)
    )
);

named!(pub newline<CompleteStr, CompleteStr>, 
    alt!(
        tag!("\n\r")
        | tag!("\n")
    )
);

const INDENT_SIZE: usize = 4;

named_args!(pub indent(level: usize)<CompleteStr, Vec<CompleteStr>>, 
    count!(tag!(" "), INDENT_SIZE * level)
);

