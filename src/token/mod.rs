#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,

    Int,
    Assign,

    Plus,
    Minus,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
}

pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(ttype: TokenType, literal: String) -> Self {
        Self { ttype, literal }
    }
}
