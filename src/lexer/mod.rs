mod tests;

use crate::token::*;

#[derive(Debug, Default)]
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
        self.skip_whitespace();

        let tok = match self.ch {
            // Assign, Eq
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==".to_string())
                } else {
                    new_token(TokenType::Assign, self.ch)
                }
            }

            // Bang, NotEq
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".to_string())
                } else {
                    new_token(TokenType::Bang, self.ch)
                }
            }

            // String
            '"' => Token::new(TokenType::String, self.read_string()),

            // Operators
            '+' => new_token(TokenType::Plus, self.ch),
            '-' => new_token(TokenType::Minus, self.ch),
            '*' => new_token(TokenType::Asterisk, self.ch),
            '/' => new_token(TokenType::Slash, self.ch),
            '<' => new_token(TokenType::Lt, self.ch),
            '>' => new_token(TokenType::Gt, self.ch),

            // Delimiters
            ';' => new_token(TokenType::Semicolon, self.ch),
            '(' => new_token(TokenType::LParen, self.ch),
            ')' => new_token(TokenType::RParen, self.ch),
            ',' => new_token(TokenType::Comma, self.ch),
            '{' => new_token(TokenType::LBrace, self.ch),
            '}' => new_token(TokenType::RBrace, self.ch),
            '[' => new_token(TokenType::LBracket, self.ch),
            ']' => new_token(TokenType::RBracket, self.ch),

            '\0' => new_token(TokenType::Eof, self.ch),

            // Int, Ident
            // TODO: pattern match
            _ => {
                return if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let tt = lookup_ident(&literal);
                    Token::new(tt, literal)
                } else if self.ch.is_numeric() {
                    let number = self.read_number();
                    Token::new(TokenType::Int, number)
                } else {
                    let ch = self.ch;
                    self.read_char();
                    new_token(TokenType::Illegal, ch)
                }
            }
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

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_numeric() {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn read_string(&mut self) -> String {
        self.read_char(); // skip (")
        let position = self.position;
        while self.ch != '"' && self.ch != '\0' {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }
}

fn new_token(tt: TokenType, c: char) -> Token {
    Token::new(tt, c.to_string())
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}
