use crate::parse::{Ident, Expr, Stmt, Lit, Op};
use super::program::{DataValue, Program};
use super::instruction::Instruction::{self, *};
use super::register::Register::{self, *};
use super::label::Label;
use names::Generator;
use std::collections::HashMap;

#[derive(Default)]
pub struct Compiler<'a> {
    data: HashMap<Label, DataValue>,
    text: HashMap<Label, Vec<Instruction>>,
    free_temp_regs: Vec<Register>,
    name_gen: Generator<'a>,
    continue_label: Option<Label>,
    break_label: Option<Label>,
    fn_vars: Vec<Ident>,
}

fn variables(body: &[Stmt], vars: &mut Vec<Ident>) {
    for stmt in body {
        match stmt {
            Stmt::Assign(ident, _) => vars.push(ident.clone()),
            Stmt::Break | Stmt::Continue | Stmt::Expr(_) | Stmt::Fun(_, _, _) | Stmt::Return(_) => {
            }
            Stmt::For(ident, _, body) => {
                vars.push(ident.clone());
                variables(body, vars);
            }
            Stmt::If(ifs) => {
                for (_, ref body) in &ifs.cases {
                    variables(body, vars);
                }
                if let Some(ref body) = ifs.else_case {
                    variables(body, vars);
                }
            }
            Stmt::Loop(body) => variables(body, vars),
            Stmt::While(_, body) => variables(body, vars),
        }
    }
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

    fn hoist_variables(&mut self, body: &[Stmt], params: &[Ident]) -> Vec<Instruction> {
        self.fn_vars.clear();

        let mut inst = Vec::new();

        for (param, arg_reg) in params.iter().zip(Register::args().iter()) {
            self.fn_vars.push(param.clone());
            inst.push(Sw(*arg_reg, self.variable_offset(param).unwrap(), Sp));
        }

        variables(body, &mut self.fn_vars);
        inst.insert(0, Addi(Sp, Sp, -4 * self.fn_vars.len() as i32));
        inst
    }

    fn unhoist_variables(&self) -> Vec<Instruction> {
        vec![Addi(Sp, Sp, 4 * self.fn_vars.len() as i32)]
    }

    fn variable_offset(&self, ident: &Ident) -> Option<i16> {
        let count = self.fn_vars.iter().take_while(|var| var != &ident).count() as i16;
        if count as usize == self.fn_vars.len() {
            None
        } else {
            Some(count * 4)
        }
    }

    fn label(&mut self) -> Label {
        Label::new(self.name_gen.next().unwrap().replace("-", "_"))
    }

    fn put_lit(&mut self, label: Label, lit: &Lit) -> (Register, Vec<Instruction>) {
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
            Lit::List(l) => {
                self.data.insert(label.clone(), DataValue::List(l.len()));
                let mut inst = vec![La(reg, label)];
                for (i, e) in l.iter().enumerate() {
                    let (sub_reg, sub_inst) = self.expr(e);
                    inst.extend(sub_inst);
                    inst.push(Sw(sub_reg, ((i + 1) * 4) as i16, reg));
                    self.return_register(sub_reg);
                }
                (reg, inst)
            }
        }
    }

    pub fn return_register(&mut self, r: Register) {
        self.free_temp_regs.push(r);
    }

    pub fn expr(&mut self, e: &Expr) -> (Register, Vec<Instruction>) {
        match e {
            Expr::Lit(l) => {
                let label = self.label();
                self.put_lit(label, l)
            }
            Expr::Ident(ident) => {
                let reg = self.temp_reg();
                if let Some(var_off) = self.variable_offset(ident) {
                    return (reg, vec![Lw(reg, var_off, Sp)]);
                }
                panic!("Could not find variable: {}", ident.0)
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
                let mut arg_regs = Register::args().to_vec();
                arg_regs.reverse();
                let mut inst = Vec::new();

                // Set arguments
                for param in params {
                    let (reg, sub_inst) = self.expr(param);
                    inst.extend(sub_inst);
                    let arg_reg = arg_regs.pop().expect("Too many arguments");
                    inst.push(Move(arg_reg, reg));
                    self.return_register(reg);
                }

                // Backup return addr
                inst.push(Addi(Sp, Sp, -4));
                inst.push(Sw(Ra, 0, Sp));

                // Make the call
                inst.push(Jal(Label::new(fname.0.to_string())));

                // Restore return addr
                inst.push(Lw(Ra, 0, Sp));
                inst.push(Addi(Sp, Sp, 4));

                let res_reg = self.temp_reg();
                inst.push(Move(res_reg, V0));
                (res_reg, inst)
            }
        }
    }

    pub fn stmt(&mut self, stmt: &Stmt) -> Vec<Instruction> {
        match stmt {
            Stmt::Expr(e) => self.expr(e).1,
            Stmt::Fun(fname, params, body) => {
                let mut body_inst = Vec::new();

                // Backup registers
                const BACKED_UP_REGS: &'static [Register] =
                    &[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9];
                body_inst.push(Addi(Sp, Sp, -4 * BACKED_UP_REGS.len() as i32));
                for (i, reg) in BACKED_UP_REGS.iter().enumerate() {
                    body_inst.push(Sw(*reg, i as i16 * 4, Sp));
                }

                // Hoist variables
                body_inst.extend(self.hoist_variables(body, params));

                /*
                // Load function parameters
                self.fn_params.clear();
                for (param, arg_reg) in params.iter().zip(Register::args().iter()) {
                    let reg = self.temp_reg();
                    self.fn_params.insert(param.clone(), reg);
                    body_inst.push(Move(reg, *arg_reg));
                }
                */

                // Process function body
                for stmt in body {
                    body_inst.extend(self.stmt(stmt));
                }

                // Unhoist variables
                body_inst.extend(self.unhoist_variables());

                // Restore registers
                for (i, reg) in BACKED_UP_REGS.iter().enumerate() {
                    body_inst.push(Lw(*reg, i as i16 * 4, Sp));
                }
                body_inst.push(Addi(Sp, Sp, 4 * BACKED_UP_REGS.len() as i32));

                // Return call
                body_inst.push(Jr(Ra));

                // Override the name of the main function
                let fname = if fname.0 == "main" {
                    "program_main".to_string()
                } else {
                    fname.0.clone()
                };
                self.text.insert(Label::new(fname), body_inst);
                // Put into the text section instead
                vec![]
            }
            Stmt::Assign(ident, expr) => {
                let (reg, mut inst) = self.expr(expr);
                let var_off = self
                    .variable_offset(ident)
                    .expect(&format!("Could not find variable: {}", ident.0));
                inst.push(Sw(reg, var_off, Sp));
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
                    inst.push(Instruction::Label(skip_label));
                }
                if let Some(ref body) = ifs.else_case {
                    for stmt in body.iter() {
                        inst.extend(self.stmt(stmt));
                    }
                }
                inst.push(Instruction::Label(end_label));
                inst
            }
            Stmt::Return(e) => {
                let (reg, mut inst) = self.expr(e);
                inst.push(Move(V0, reg));
                self.return_register(reg);
                inst
            }
            Stmt::While(cond, body) => {
                let mut inst = Vec::new();
                let start_label = self.label();
                let end_label = self.label();
                self.continue_label = Some(start_label.clone());
                self.break_label = Some(end_label.clone());
                inst.push(Instruction::Label(start_label.clone()));

                let (cond_reg, cond_inst) = self.expr(cond);
                inst.extend(cond_inst);
                inst.push(Ble(cond_reg, Zero, end_label.clone()));

                for stmt in body {
                    inst.extend(self.stmt(stmt));
                }
                inst.push(B(start_label.clone()));

                inst.push(Instruction::Label(end_label));
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
                inst.push(Instruction::Label(start_label.clone()));

                for stmt in body {
                    inst.extend(self.stmt(stmt));
                }
                inst.push(B(start_label.clone()));

                inst.push(Instruction::Label(end_label));
                self.continue_label = None;
                self.break_label = None;
                inst
            }
            Stmt::For(var, expr, body) => {
                // TODO Need to be re-evaluated after every iteration
                let (iter_reg, mut inst) = self.expr(expr);
                let counter = self.temp_reg();
                // Load list length
                inst.push(Lw(counter, 0, iter_reg));

                // Loop setup
                let start_label = self.label();
                let end_label = self.label();
                self.continue_label = Some(start_label.clone());
                self.break_label = Some(end_label.clone());
                inst.push(Instruction::Label(start_label.clone()));

                // Move to the next item
                inst.push(Addi(iter_reg, iter_reg, 4));

                let val_reg = self.temp_reg();
                inst.push(Lw(val_reg, 0, iter_reg));
                let var_off = self
                    .variable_offset(&var)
                    .expect(&format!("failed to find variable: {}", var.0));
                inst.push(Sw(val_reg, var_off, Sp));
                self.return_register(val_reg);

                // Check and then decrement counter
                inst.push(Ble(counter, Zero, end_label.clone()));
                inst.push(Addi(counter, counter, -1));

                for stmt in body {
                    inst.extend(self.stmt(stmt));
                }
                inst.push(B(start_label.clone()));

                // Loop teardown
                inst.push(Instruction::Label(end_label));
                self.continue_label = None;
                self.break_label = None;
                inst
            }
        }
    }

    pub fn program(self) -> Program {
        Program {
            data: self.data, 
            text: self.text,
        }
    }
}

