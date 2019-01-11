use self::Inst::*;
use super::context::Context;
use super::data::DataValue;
use super::instruction::Inst;
use super::label::Label;
use super::register::Register;
use name_gen::RandomIdent;
use parse::{Expr, Lit, Op};
use rand::prelude::*;
use std::collections::HashMap;

#[derive(Default)]
struct Compiler {
    data: HashMap<Label, DataValue>,
    text: HashMap<Label, Vec<Inst>>,
    free_regs: Vec<Register>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            free_regs: Register::temporaries().to_vec(),
            ..Default::default()
        }
    }

    fn temp_reg(&mut self) -> Register {
        self.free_regs.pop().expect("No more registers available")
    }

    fn return_reg(&mut self, reg: Register) {
        self.free_regs.push(reg)
    }

    fn label(&mut self) -> Label {
        Label::new(random::<RandomIdent>())
    }

    fn put_lit(&mut self, label: Label, lit: &Lit) -> (Register, Vec<Inst>) {
        let reg = self.temp_reg();
        use self::Lit::*;
        match lit {
            Int(n) => (reg, vec![Li(reg, *n)]),
            Bool(b) => (reg, vec![Li(reg, *b as i32)]),
            Float(_) => unimplemented!(),
            Str(s) => {
                self.data
                    .insert(label.clone(), DataValue::Asciiz(s.to_string()));
                (reg, vec![La(reg, label)])
            }
            List(l) => {
                self.data.insert(label.clone(), DataValue::List(l.len()));
                let mut inst = vec![La(reg, label)];
                for (i, e) in l.iter().enumerate() {
                    let (sub_reg, sub_inst) = self.expr(e);
                    inst.extend(sub_inst);
                    inst.push(Sw(sub_reg, ((i + 1) * 4) as i16, reg));
                    self.return_reg(sub_reg);
                }
                (reg, inst)
            }
        }
    }

    pub fn expr(&mut self, e: &Expr) -> (Register, Vec<Inst>) {
        use self::Expr::*;
        use self::Register::*;
        match e {
            Lit(l) => {
                let label = self.label();
                self.put_lit(label, l)
            }
            Ident(i) => {
                let reg = self.temp_reg();
                let label = Label::new(i.0.clone());
                (reg, vec![La(reg, label), Lw(reg, 0, reg)])
            }
            BinOp(a, op, b) => {
                let mut inst = Vec::new();
                let (reg_a, inst_a) = self.expr(&a);
                inst.extend(inst_a);
                let (reg_b, inst_b) = self.expr(&b);
                inst.extend(inst_b);

                let op_inst = match op {
                    Op::Add => Add,
                    Op::Min => Sub,
                    Op::Eq => Seq,
                    Op::Gt => Sgt,
                    Op::Lt => Slt,
                };

                inst.push(op_inst(reg_a, reg_a, reg_b));
                self.return_reg(reg_b);
                (reg_a, inst)
            }
            Call(fname, params) => {
                let mut arg_regs = vec![A3, A2, A1, A0];
                let mut inst = Vec::new();
                for param in params {
                    let (reg, sub_inst) = self.expr(param);
                    inst.extend(sub_inst);
                    let arg_reg = arg_regs.pop().expect("Too many arguments");
                    inst.push(Move(arg_reg, reg));
                }
                // Push temporaries to stack
                for temp_reg in Register::temporaries().iter() {
                    inst.extend(vec![Addi(Sp, Sp, -4), Sw(*temp_reg, 0, Sp)]);
                }
                inst.extend(vec![
                    // Push return address on stack
                    Addi(Sp, Sp, -4),
                    Sw(Ra, 0, Sp),
                    Jal(Label::new(fname.0.to_string())),
                    // Pop return address from stack
                    Lw(Ra, 0, Sp),
                    Addi(Sp, Sp, 4),
                ]);
                // Pop temporaries from stack
                for temp_reg in Register::temporaries().iter().rev() {
                    inst.extend(vec![Lw(*temp_reg, 0, Sp), Addi(Sp, Sp, 4)]);
                }
                (V0, inst)
            }
        }
    }

    pub fn stmt<C: Context>(&mut self, cx: &mut C, stmt: &Stmt) -> Vec<Inst> {
        use self::Register::*;
        match stmt {
            Stmt::Expr(e) => self.expr(e).1,
            Stmt::Fun(fname, params, body) => {
                let mut body_inst = Vec::new();
                for (i, param) in params.iter().enumerate() {
                    let label = Label(param.0.clone());
                    self.data.insert(label.clone(), DataValue::Word(0));
                    let reg = self.temp_reg();
                    body_inst.push(La(reg, label));
                    body_inst.push(Sw(Register::arg(i as u8), 0, reg));
                    self.return_register(reg);
                }
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
                inst.push(Jr(Ra));
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
            Stmt::For(var, expr, body) => {
                let var = Label(var.0.clone());
                self.data.insert(var.clone(), DataValue::Word(0));
                let (iter_reg, mut inst) = self.expr(expr);
                let counter = self.temp_reg();
                inst.push(Lw(counter, 0, iter_reg));

                // Loop setup
                let start_label = self.label();
                let end_label = self.label();
                self.continue_label = Some(start_label.clone());
                self.break_label = Some(end_label.clone());
                inst.push(MipsInst::Label(start_label.clone()));

                // Move to the next item
                inst.push(Addi(iter_reg, iter_reg, 4));
                let addr_reg = self.temp_reg();
                inst.push(La(addr_reg, var));
                let val_reg = self.temp_reg();
                inst.push(Lw(val_reg, 0, iter_reg));
                inst.push(Sw(val_reg, 0, addr_reg));
                self.return_register(val_reg);
                self.return_register(addr_reg);

                // Check and then decrement counter
                inst.push(Ble(counter, Zero, end_label.clone()));
                inst.push(Addi(counter, counter, -1));

                for stmt in body {
                    inst.extend(self.stmt(stmt));
                }
                inst.push(B(start_label.clone()));

                // Loop teardown
                inst.push(MipsInst::Label(end_label));
                self.continue_label = None;
                self.break_label = None;
                inst
            }
        }
    }
}
