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

#[cfg(test)] use nom::IResult;
#[cfg(test)] 
pub fn assert_parsed<T>(r: IResult<CompleteStr, T>) {
    let (remaining_input, _) = r.expect("Parsing failed");
    assert_eq!(remaining_input, CompleteStr(""));
}

#[cfg(test)]
mod tests {
    #[test]
    fn comma_sep() {
        let res = super::comma_sep(",    ".into());
        super::assert_parsed(res);
    }

    #[test]
    fn indent() {
        let res = super::indent("        ".into(), 2);
        super::assert_parsed(res);
    }
}
