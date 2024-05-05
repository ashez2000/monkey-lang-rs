use crate::token::{lookup_ident, Token, TokenType};

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
                    Token::new(TokenType::Eq, "==")
                } else {
                    Token::new(TokenType::Assign, self.ch)
                }
            }

            // Bang, NotEq
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=")
                } else {
                    Token::new(TokenType::Bang, self.ch)
                }
            }

            // String
            '"' => Token::new(TokenType::String, self.read_string()),

            // Operators
            '+' => Token::new(TokenType::Plus, self.ch),
            '-' => Token::new(TokenType::Minus, self.ch),
            '*' => Token::new(TokenType::Asterisk, self.ch),
            '/' => Token::new(TokenType::Slash, self.ch),
            '<' => Token::new(TokenType::Lt, self.ch),
            '>' => Token::new(TokenType::Gt, self.ch),

            // Delimiters
            ';' => Token::new(TokenType::Semicolon, self.ch),
            '(' => Token::new(TokenType::LParen, self.ch),
            ')' => Token::new(TokenType::RParen, self.ch),
            ',' => Token::new(TokenType::Comma, self.ch),
            '{' => Token::new(TokenType::LBrace, self.ch),
            '}' => Token::new(TokenType::RBrace, self.ch),
            '[' => Token::new(TokenType::LBracket, self.ch),
            ']' => Token::new(TokenType::RBracket, self.ch),

            '\0' => Token::new(TokenType::Eof, self.ch),

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
                    Token::new(TokenType::Illegal, ch)
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

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}
