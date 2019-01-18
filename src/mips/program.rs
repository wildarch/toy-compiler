use std::fmt::{Display, Formatter, Error as FormatError};
use super::instruction::Instruction;
use super::label::Label;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub data: HashMap<Label, DataValue>, 
    pub text: HashMap<Label, Vec<Instruction>>,
}

#[derive(Debug)]
pub enum DataValue {
    Asciiz(String),
    Word(i32),
    // Custom type
    List(usize),
}

impl Display for Program {
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

impl Display for DataValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        match self {
            DataValue::Asciiz(s) => write!(f, ".asciiz \"{}\"", s),
            DataValue::Word(n) => write!(f, ".word {}", n),
            DataValue::List(n) => write!(f, ".word {} \n.space {}", n, n * 4),
        }
    }
}


