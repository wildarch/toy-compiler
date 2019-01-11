use super::data::DataValue;
use super::instruction::Inst;
use super::label::Label;
use std::collections::HashMap;

#[derive(Debug)]
pub struct MipsProgram {
    pub data: HashMap<Label, DataValue>,
    pub text: HashMap<Label, Vec<Inst>>,
}
