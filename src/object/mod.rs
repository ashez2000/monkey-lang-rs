use std::fmt::Display;

#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Null,
}

impl Object {
    pub fn object_type(&self) -> String {
        match self {
            Self::Integer(_) => String::from("INTEGER"),
            Self::Boolean(_) => String::from("BOOLEAN"),
            Self::Return(_) => String::from("RETURN"),
            Self::Error(_) => String::from("ERROR"),
            Self::Null => String::from("NULL"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(int) => write!(f, "{}", int),
            Self::Boolean(bool) => write!(f, "{}", bool),
            Self::Return(value) => write!(f, "{}", value),
            Self::Error(msg) => write!(f, "{}", msg),
            Self::Null => write!(f, "null"),
        }
    }
}
