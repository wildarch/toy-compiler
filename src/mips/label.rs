use std::fmt::{Display, Formatter, Error as FormatError};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Label(String);

impl Label {
    pub fn new<S: ToString>(s: S) -> Label {
        Label(s.to_string())
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        write!(f, "{}", self.0)
    }
}
