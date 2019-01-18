use std::fmt::{Formatter, Display, Error as FormatError};
use super::register::Register;
use super::label::Label;

#[derive(Debug)]
pub enum Instruction {
    La(Register, Label),
    Add(Register, Register, Register),
    Addi(Register, Register, i32),
    Sub(Register, Register, Register),
    Seq(Register, Register, Register),
    Sgt(Register, Register, Register),
    Slt(Register, Register, Register),
    Move(Register, Register),
    Jal(Label),
    Li(Register, i32),
    Jr(Register),
    SysCall,
    // Actually just 15 bits
    Sw(Register, i16, Register),
    Lw(Register, i16, Register),
    B(Label),
    Ble(Register, Register, Label),
    // Not really an instruction
    Label(Label),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        use self::Instruction::*;
        match self {
            La(r, l) => write!(f, "la   {}, {}", r, l),
            Add(a, b, c) => write!(f, "add  {}, {}, {}", a, b, c),
            Addi(a, b, c) => write!(f, "addi {}, {}, {}", a, b, c),
            Sub(a, b, c) => write!(f, "sub  {}, {}, {}", a, b, c),
            Seq(a, b, c) => write!(f, "seq  {}, {}, {}", a, b, c),
            Sgt(a, b, c) => write!(f, "sgt  {}, {}, {}", a, b, c),
            Slt(a, b, c) => write!(f, "slt  {}, {}, {}", a, b, c),
            Move(a, b) => write!(f, "move {}, {}", a, b),
            Jal(l) => write!(f, "jal  {}", l),
            Li(r, i) => write!(f, "li   {}, {}", r, i),
            Jr(r) => write!(f, "jr   {}", r),
            SysCall => write!(f, "syscall"),
            Sw(v, i, d) => write!(f, "sw   {}, {}({})", v, i, d),
            Lw(d, i, v) => write!(f, "lw   {}, {}({})", d, i, v),
            B(l) => write!(f, "b    {}", l),
            Ble(a, b, l) => write!(f, "ble  {}, {}, {}", a, b, l),
            Label(l) => write!(f, "{}:", l),
        }
    }
}
