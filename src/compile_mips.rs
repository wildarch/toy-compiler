/// Legacy mips compiler
use names::Generator;
use parse::{Document, Expr, Lit, Op, Stmt};

use std::collections::HashMap;
use std::fmt::{Display, Error as FormatError, Formatter};

#[derive(Default)]
struct Compiler<'a> {
    data: HashMap<Label, DataValue>,
    text: HashMap<Label, Vec<MipsInst>>,
    free_temp_regs: Vec<Register>,
    name_gen: Generator<'a>,
    continue_label: Option<Label>,
    break_label: Option<Label>,
}

impl<'a> Compiler<'a> {
    pub fn new() -> Compiler<'a> {
        let mut c = Compiler::default();
        c.reset_temp_regs();
        c
    }

    fn reset_temp_regs(&mut self) {
        self.free_temp_regs = vec![T0, T1, T2, T3, T4, T5, T6, T7];
    }

    pub fn program(self) -> MipsProgram {
        MipsProgram(self.data, self.text)
    }
}

#[derive(Debug)]
pub struct CompileError;

pub fn compile(document: Document) -> Result<MipsProgram, CompileError> {
    let mut compiler = Compiler::new();
    for statement in document {
        compiler.stmt(&statement);
        compiler.reset_temp_regs();
    }
    Ok(compiler.program())
}

#[cfg(test)]
mod tests {
    use super::Compiler;
    use parse::{Expr::*, Ident, Lit::*, Op::*};

    #[test]
    fn print_arith() {
        let expr = Call(
            Ident("print".to_string()),
            vec![BinOp(Box::new(Lit(Int(1))), Add, Box::new(Lit(Int(2))))],
        );
        let mut compiler = Compiler::new();
        println!("{:?}", compiler.expr(&expr));
    }

    #[test]
    fn print_string() {
        let expr = Call(
            Ident("print".to_string()),
            vec![Lit(Str("Hallo, chinees?".to_string()))],
        );
        let mut compiler = Compiler::new();
        println!("{:?}", compiler.expr(&expr));
    }
}
