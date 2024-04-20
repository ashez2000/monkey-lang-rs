mod tests;

use crate::token::*;

#[derive(Default)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self::default();
        lexer.input = input.chars().collect();
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        let tok = match self.ch {
            '=' => new_token(TokenType::Assign, self.ch),
            ';' => new_token(TokenType::Semicolon, self.ch),
            '(' => new_token(TokenType::LParen, self.ch),
            ')' => new_token(TokenType::RParen, self.ch),
            ',' => new_token(TokenType::Comma, self.ch),
            '+' => new_token(TokenType::Plus, self.ch),
            '{' => new_token(TokenType::LBrace, self.ch),
            '}' => new_token(TokenType::RBrace, self.ch),

            '\0' => new_token(TokenType::Eof, self.ch),
            _ => new_token(TokenType::Illegal, self.ch),
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }
}

fn new_token(tt: TokenType, c: char) -> Token {
    Token::new(tt, c.to_string())
}
