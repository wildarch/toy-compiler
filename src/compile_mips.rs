use names::Generator;
use parse::{Document, Expr, Lit, Op, Stmt};

use std::collections::HashMap;
use std::fmt::{Display, Error as FormatError, Formatter};

#[derive(Debug)]
pub enum MipsInst {
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
use self::MipsInst::*;

impl Display for MipsInst {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
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
            MipsInst::Label(l) => write!(f, "{}:", l),
        }
    }
}

#[derive(Debug)]
pub enum DataValue {
    Asciiz(String),
    Word(i32),
}

impl Display for DataValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        match self {
            DataValue::Asciiz(s) => write!(f, ".asciiz \"{}\"", s),
            DataValue::Word(n) => write!(f, ".word {}", n),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Label(String);

impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        write!(f, "{}", self.0)
    }
}

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

    fn temp_reg(&mut self) -> Register {
        let r = self
            .free_temp_regs
            .pop()
            .expect("No more registers available");
        r
    }

    fn label(&mut self) -> Label {
        Label(self.name_gen.next().unwrap().replace("-", "_"))
    }

    fn put_lit(&mut self, label: Label, lit: &Lit) -> (Register, Vec<MipsInst>) {
        let reg = self.temp_reg();
        match lit {
            Lit::Int(n) => (reg, vec![Li(reg, *n)]),
            Lit::Bool(b) => (reg, vec![Li(reg, *b as i32)]),
            Lit::Float(_) => unimplemented!(),
            Lit::Str(s) => {
                self.data
                    .insert(label.clone(), DataValue::Asciiz(s.to_string()));
                (reg, vec![La(reg, label)])
            }
            Lit::List(_l) => unimplemented!(),
        }
    }

    pub fn return_register(&mut self, r: Register) {
        self.free_temp_regs.push(r);
    }

    pub fn expr(&mut self, e: &Expr) -> (Register, Vec<MipsInst>) {
        match e {
            Expr::Lit(l) => {
                let label = self.label();
                self.put_lit(label, l)
            }
            Expr::Ident(i) => {
                let reg = self.temp_reg();
                let label = Label(i.0.clone());
                (reg, vec![La(reg, label), Lw(reg, 0, reg)])
            }
            Expr::BinOp(a, op, b) => {
                let mut inst = Vec::new();
                let (reg_a, inst_a) = self.expr(&a);
                inst.extend(inst_a);
                let (reg_b, inst_b) = self.expr(&b);
                inst.extend(inst_b);

                match op {
                    Op::Add => inst.push(Add(reg_a, reg_a, reg_b)),
                    Op::Min => inst.push(Sub(reg_a, reg_a, reg_b)),
                    Op::Eq => inst.push(Seq(reg_a, reg_a, reg_b)),
                    Op::Gt => inst.push(Sgt(reg_a, reg_a, reg_b)),
                    Op::Lt => inst.push(Slt(reg_a, reg_a, reg_b)),
                };
                self.return_register(reg_b);
                (reg_a, inst)
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
                inst.extend(vec![
                    // Push return address on stack
                    Addi(Sp, Sp, -4),
                    Sw(Ra, 0, Sp),
                    Jal(Label(fname.0.to_string())),
                    // Pop return address from stack
                    Lw(Ra, 0, Sp),
                    Addi(Sp, Sp, 4),
                ]);
                (V0, inst)
            }
        }
    }

    pub fn stmt(&mut self, stmt: &Stmt) -> Vec<MipsInst> {
        match stmt {
            Stmt::Expr(e) => self.expr(e).1,
            Stmt::Fun(fname, _params, body) => {
                let mut body_inst = Vec::new();
                for stmt in body {
                    body_inst.extend(self.stmt(stmt));
                }
                body_inst.push(Jr(Ra));
                let fname = if fname.0 == "main" {
                    "program_main".to_string()
                } else {
                    fname.0.clone()
                };
                self.text.insert(Label(fname), body_inst);
                // Put into the text section instead
                vec![]
            }
            Stmt::Assign(i, e) => {
                let (reg, mut inst) = self.expr(e);
                let label = Label(i.0.clone());
                if !self.data.contains_key(&label) {
                    self.data.insert(label.clone(), DataValue::Word(0));
                }
                inst.extend(vec![La(A0, label), Sw(reg, 0, A0)]);
                self.return_register(reg);
                inst
            }
            Stmt::If(ifs) => {
                let mut inst = Vec::new();
                let end_label = self.label();
                for (cond, body) in ifs.cases.iter() {
                    let (cond_reg, cond_inst) = self.expr(cond);
                    inst.extend(cond_inst);
                    let skip_label = self.label();
                    inst.push(Ble(cond_reg, Zero, skip_label.clone()));
                    for stmt in body.iter() {
                        inst.extend(self.stmt(stmt));
                    }
                    inst.push(B(end_label.clone()));
                    inst.push(MipsInst::Label(skip_label));
                }
                if let Some(ref body) = ifs.else_case {
                    for stmt in body.iter() {
                        inst.extend(self.stmt(stmt));
                    }
                }
                inst.push(MipsInst::Label(end_label));
                inst
            }
            Stmt::Return(e) => {
                let (reg, mut inst) = self.expr(e);
                inst.push(Move(V0, reg));
                inst
            }
            Stmt::While(cond, body) => {
                let mut inst = Vec::new();
                let start_label = self.label();
                let end_label = self.label();
                self.continue_label = Some(start_label.clone());
                self.break_label = Some(end_label.clone());
                inst.push(MipsInst::Label(start_label.clone()));

                let (cond_reg, cond_inst) = self.expr(cond);
                inst.extend(cond_inst);
                inst.push(Ble(cond_reg, Zero, end_label.clone()));

                for stmt in body {
                    inst.extend(self.stmt(stmt));
                }
                inst.push(B(start_label.clone()));

                inst.push(MipsInst::Label(end_label));
                self.continue_label = None;
                self.break_label = None;
                inst
            }
            Stmt::Break => vec![B(self.break_label.clone().expect("No label to break to!"))],
            Stmt::Continue => vec![B(self
                .continue_label
                .clone()
                .expect("No label to continue to!"))],
            Stmt::Loop(body) => {
                let mut inst = Vec::new();
                let start_label = self.label();
                let end_label = self.label();
                self.continue_label = Some(start_label.clone());
                self.break_label = Some(end_label.clone());
                inst.push(MipsInst::Label(start_label.clone()));

                for stmt in body {
                    inst.extend(self.stmt(stmt));
                }
                inst.push(B(start_label.clone()));

                inst.push(MipsInst::Label(end_label));
                self.continue_label = None;
                self.break_label = None;
                inst
            }
            Stmt::For(var, expr, body) => unimplemented!(),
        }
    }

    pub fn program(self) -> MipsProgram {
        MipsProgram(self.data, self.text)
    }
}

#[derive(Debug)]
pub struct MipsProgram(HashMap<Label, DataValue>, HashMap<Label, Vec<MipsInst>>);

impl Display for MipsProgram {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        writeln!(f, ".data")?;
        for (label, value) in self.0.iter() {
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

        for (label, body) in self.1.iter() {
            writeln!(f, "    {}:", label)?;
            for inst in body {
                writeln!(f, "        {}", inst)?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct CompileError;

pub fn compile(document: Document) -> Result<MipsProgram, CompileError> {
    let mut compiler = Compiler::new();
    for statement in document {
        compiler.stmt(&statement);
    }
    Ok(compiler.program())
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

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
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
