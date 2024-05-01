use std::collections::HashMap;
use std::fmt::Display;

use crate::ast::*;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Fn(Function),
    String(String),
    Null,
}

impl Object {
    pub fn object_type(&self) -> String {
        match self {
            Self::Integer(_) => String::from("INTEGER"),
            Self::Boolean(_) => String::from("BOOLEAN"),
            Self::String(_) => String::from("STRING"),
            Self::Return(_) => String::from("RETURN"),
            Self::Error(_) => String::from("ERROR"),
            Self::Fn(_) => String::from("FUNCTION"),
            Self::Null => String::from("NULL"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(int) => write!(f, "{}", int),
            Self::Boolean(bool) => write!(f, "{}", bool),
            Self::String(s) => write!(f, "{}", s),
            Self::Return(value) => write!(f, "{}", value),
            Self::Error(msg) => write!(f, "{}", msg),
            Self::Fn(func) => write!(f, "{}", func.to_string()),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(outer: Option<Box<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer,
        }
    }

    pub fn get(&self, name: String) -> Option<Object> {
        match self.store.get(name.as_str()) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(env) => env.get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, value: Object) -> Option<Object> {
        self.store.insert(name.clone(), value);
        return self.get(name);
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Function {
    fn to_string(&self) -> String {
        let mut out = String::from("");
        let mut params = vec![];

        for p in &self.parameters {
            params.push(p.to_string());
        }

        out.push_str("fn");
        out.push_str("(");
        out.push_str(params.join(", ").as_str());
        out.push_str(") {\n");
        out.push_str(self.body.to_string().as_str());
        out.push_str("\n}");

        out
    }
}
