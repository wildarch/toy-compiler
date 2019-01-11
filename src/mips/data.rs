#[derive(Debug)]
pub enum DataValue {
    Asciiz(String),
    Word(i32),
    // Custom type
    List(usize),
}
