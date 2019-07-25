use crate::parse::Ident;

const JS_KEYWORDS: &'static [&'static str] = &[
    "do",
    "if",
    "in",
    "for",
    "let",
    "new",
    "try",
    "var",
    "case",
    "else",
    "enum",
    "eval",
    "null",
    "this",
    "true",
    "void",
    "with",
    "await",
    "break",
    "catch",
    "class",
    "const",
    "false",
    "super",
    "throw",
    "while",
    "yield",
    "delete",
    "export",
    "import",
    "public",
    "return",
    "static",
    "switch",
    "typeof",
    "default",
    "extends",
    "finally",
    "package",
    "private",
    "continue",
    "debugger",
    "function",
    "arguments",
    "interface",
    "protected",
    "implements",
    "instanceof",
];

pub fn transpile_ident(i: Ident) -> String {
    if JS_KEYWORDS.contains(&i.0.as_str()) {
        format!("${}", i.0)
    } else {
        i.0
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::Ident;

    #[test]
    fn transpile_ident() {
        assert_eq!(
            super::transpile_ident(Ident("instanceof".into())),
            "$instanceof"
        );
    }
}
