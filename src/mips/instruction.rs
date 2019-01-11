use super::label::Label;
use super::register::Register;

#[derive(Debug)]
pub enum Inst {
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
