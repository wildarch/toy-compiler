use std::fmt::{Display, Error as FormatError, Formatter};

use super::data::DataValue;
use super::instruction::Inst;
use super::label::Label;
use super::program::MipsProgram;
use super::register::Register;

impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        write!(f, "{}", self.as_str())
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        use self::Inst::*;
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
            Inst::Label(l) => write!(f, "{}:", l),
        }
    }
}

impl Display for DataValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        use self::DataValue::*;
        match self {
            Asciiz(s) => write!(f, ".asciiz \"{}\"", s),
            Word(n) => write!(f, ".word {}", n),
            List(n) => write!(f, ".word {} \n.space {}", n, n * 4),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        use self::Register::*;
        let s = match self {
            Zero => "$zero",
            At => "$at",
            V0 => "$v0",
            V1 => "$v1",
            A0 => "$a0",
            A1 => "$a1",
            A2 => "$a2",
            A3 => "$a3",
            T0 => "$t0",
            T1 => "$t1",
            T2 => "$t2",
            T3 => "$t3",
            T4 => "$t4",
            T5 => "$t5",
            T6 => "$t6",
            T7 => "$t7",
            S0 => "$s0",
            S1 => "$s1",
            S2 => "$s2",
            S3 => "$s3",
            S4 => "$s4",
            S5 => "$s5",
            S6 => "$s6",
            S7 => "$s7",
            T8 => "$t8",
            T9 => "$t9",
            K0 => "$k0",
            K1 => "$k1",
            Gp => "$gp",
            Sp => "$sp",
            Fp => "$fp",
            Ra => "$ra",
        };
        write!(f, "{}", s)
    }
}

impl Display for MipsProgram {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        writeln!(f, ".data")?;
        for (label, value) in self.data.iter() {
            writeln!(f, "    {}:", label)?;
            writeln!(f, "        {}", value)?;
        }

        writeln!(f, ".text")?;

        writeln!(
            f,
            r#"
# BEGIN Standard library
    main:
        jal  program_main
        li   $v0, 10
        syscall
    print:
        li   $v0, 4
        syscall

        # Add newline
        li   $a0, 10
        sb   $a0, -2($sp)
        sb   $zero, -1($sp)
        addi $a0, $sp, -2
        syscall

    print_end:
        jr   $ra
    print_int:
        li   $v0, 1
        syscall

        # Add newline
        li   $v0, 4
        li   $a0, 10
        sb   $a0, -2($sp)
        sb   $zero, -1($sp)
        addi $a0, $sp, -2
        syscall

        jr   $ra

# END Standard library
                 "#
        )?;

        for (label, body) in self.text.iter() {
            writeln!(f, "    {}:", label)?;
            for inst in body {
                writeln!(f, "        {}", inst)?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}
