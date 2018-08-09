mod common;
mod expression;
mod function;
mod identifier;
mod if_cond;
mod literal;
mod loops;
mod statement;

use parse::Document;
use self::statement::transpile_stmts;

const STANDARD_LIB: &'static str = r#"
function print(x) {
    console.log(x);
}

main();
"#;

pub fn transpile(doc: Document) -> String {
        let mut program = transpile_stmts(doc, 0).join("\n\n");
        program.push_str(STANDARD_LIB);
        return program
}
