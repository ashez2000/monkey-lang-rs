#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum TokenType {
    #[default]
    Eof,
    Illegal,

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

    LParen,
    RParen,
    LBrace,
    RBrace,

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

#[derive(Clone, Debug, Default)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(ttype: TokenType, literal: String) -> Self {
        Self { ttype, literal }
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

        _ => TokenType::Ident,
    }
}
