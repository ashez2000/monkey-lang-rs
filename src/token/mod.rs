#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum TokenType {
    #[default]
    Eof,
    Illegal,
    Null,

    Ident,
    Int,
    String,

    // operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

    Comma,
    Semicolon,
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Print,
}

#[derive(Clone, Debug, Default)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
    pub line: u32,
}

impl Token {
    pub fn new<T: Into<String>>(ttype: TokenType, literal: T, line: u32) -> Self {
        Self {
            ttype,
            literal: literal.into(),
            line,
        }
    }
}

pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        "print" => TokenType::Print,
        "null" => TokenType::Null,

        _ => TokenType::Ident,
    }
}
