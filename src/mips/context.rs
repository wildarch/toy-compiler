use super::label::Label;

#[derive(Debug)]
pub enum ContextType {
    Func(Box<FunctionContext>),
    Loop(Box<LoopContext>),
}

#[derive(Debug)]
pub struct FunctionContext {
    arguments: Vec<Label>,
}

impl FunctionContext {
    pub fn new(arguments: Vec<Label>) -> FunctionContext {
        FunctionContext { arguments }
    }
}

impl Into<ContextType> for FunctionContext {
    fn into(self) -> ContextType {
        ContextType::Func(Box::new(self))
    }
}

impl Context for FunctionContext {}

#[derive(Debug)]
pub struct LoopContext {
    parent: ContextType,
    break_label: Label,
    continue_label: Label,
}

impl LoopContext {
    fn new<C: Into<ContextType>>(
        parent: C,
        break_label: Label,
        continue_label: Label,
    ) -> LoopContext {
        LoopContext {
            parent: parent.into(),
            break_label,
            continue_label,
        }
    }
}

impl Into<ContextType> for LoopContext {
    fn into(self) -> ContextType {
        ContextType::Loop(Box::new(self))
    }
}

impl Context for LoopContext {}

pub trait Context: Sized + Into<ContextType> {
    /* Might become useful
    fn find_cached_var(&mut self, _label: Label) -> Option<Register> {
        None
    }
    */

    fn enter_loop(self, break_label: Label, continue_label: Label) -> LoopContext {
        LoopContext::new(self.into(), break_label, continue_label)
    }
}
