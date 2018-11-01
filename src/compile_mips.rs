use names::{Generator, Name};
use parse::{Document, Expr, Ident, Lit, Op};

use std::collections::HashMap;

pub type MipsDoc = Vec<MipsInst>;

#[derive(Debug)]
pub enum MipsInst {
    La(Register, Label),
    Add(Register, Register, Register),
    Sub(Register, Register, Register),
    Slt(Register, Register, Register),
    Move(Register, Register),
    Jal(Label),
}
use self::MipsInst::*;

#[derive(Debug)]
enum DataValue {
    Asciiz(String),
    Byte(Vec<u8>),
    Word(Vec<i32>),
    Space(usize),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Label(String);

#[derive(Default)]
struct Compiler<'a> {
    data: HashMap<Label, DataValue>,
    text: HashMap<Label, Vec<MipsInst>>,
    free_temp_regs: Vec<Register>,
    name_gen: Generator<'a>,
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

    fn temp_reg(&mut self) -> Register {
        self.free_temp_regs
            .pop()
            .expect("No more registers available")
    }

    fn label(&mut self) -> Label {
        Label(self.name_gen.next().unwrap())
    }

    fn put_lit(&mut self, label: Label, lit: &Lit) -> (Register, Vec<MipsInst>) {
        match lit {
            Lit::Int(n) => {
                self.data.insert(label.clone(), DataValue::Word(vec![*n]));
            }
            Lit::Bool(b) => {
                self.data
                    .insert(label.clone(), DataValue::Byte(vec![*b as u8]));
            }
            Lit::Float(_) => unimplemented!(),
            Lit::Str(s) => {
                self.data
                    .insert(label.clone(), DataValue::Asciiz(s.to_string()));
            }
            Lit::List(l) => unimplemented!(),
        }
        let reg = self.temp_reg();
        (reg, vec![La(reg, label)])
    }

    pub fn expr(&mut self, e: &Expr) -> (Register, Vec<MipsInst>) {
        match e {
            Expr::Lit(l) => {
                let label = self.label();
                self.put_lit(label, l)
            }
            Expr::Ident(i) => unimplemented!(),
            Expr::BinOp(a, op, b) => {
                let mut inst = Vec::new();
                let (reg_a, inst_a) = self.expr(&a);
                inst.extend(inst_a);
                let (reg_b, inst_b) = self.expr(&b);
                inst.extend(inst_b);

                let reg = self.temp_reg();
                match op {
                    Op::Add => inst.push(Add(reg, reg_a, reg_b)),
                    Op::Min => inst.push(Sub(reg, reg_a, reg_b)),
                    Op::Eq => inst.push(Slt(reg, reg_a, reg_b)),
                };
                (reg, inst)
            }
            Expr::Call(fname, params) => {
                let mut arg_regs = vec![A3, A2, A1, A0];
                let mut inst = Vec::new();
                for param in params {
                    let (reg, sub_inst) = self.expr(param);
                    inst.extend(sub_inst);
                    let arg_reg = arg_regs.pop().expect("Too many arguments");
                    inst.push(Move(arg_reg, reg));
                }
                inst.push(Jal(Label(fname.0.to_string())));
                (V0, inst)
            }
        }
    }
}

#[derive(Debug)]
pub struct CompileError;

pub fn compile(document: Document) -> Result<MipsDoc, CompileError> {
    let compiled = Vec::new();

    unimplemented!();
    Ok(compiled)
}

#[derive(Debug, Copy, Clone)]
pub enum Register {
    Zero,
    At,
    V0,
    V1,
    A0,
    A1,
    A2,
    A3,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    T8,
    T9,
    K0,
    K1,
    Gp,
    Sp,
    Fp,
    Ra,
}
use self::Register::*;

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
