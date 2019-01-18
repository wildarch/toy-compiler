pub mod instruction;
pub mod register;
pub mod label;
pub mod compiler;
pub mod program;

use parse::Document;
use self::program::Program;
use self::compiler::Compiler;

/// TODO do some actual error handling
#[derive(Debug)]
pub struct CompileError;

pub fn compile(document: Document) -> Result<Program, CompileError> {
    let mut compiler = Compiler::new();
    for statement in document {
        compiler.stmt(&statement);
    }
    Ok(compiler.program())
}

