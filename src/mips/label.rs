#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Label(String);

impl Label {
    pub fn new<S: ToString>(s: S) -> Label {
        Label(s.to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}
