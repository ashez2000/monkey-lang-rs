use crate::token::{lookup_ident, Token, TokenType};

#[derive(Debug, Default)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
    line: u32,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self::default();
        lexer.input = input.chars().collect();
        lexer.line = 1;
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        while self.ch.is_ascii_whitespace() || self.ch == '#' {
            if self.ch == '#' {
                self.skip_comment();
            }
            self.skip_whitespace();
        }

        let tok = match self.ch {
            // Assign, Eq
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==", self.line)
                } else {
                    Token::new(TokenType::Assign, self.ch, self.line)
                }
            }

            // Bang, NotEq
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=", self.line)
                } else {
                    Token::new(TokenType::Bang, self.ch, self.line)
                }
            }

            // String
            '"' => Token::new(TokenType::String, self.read_string(), self.line),

            // Operators
            '+' => Token::new(TokenType::Plus, self.ch, self.line),
            '-' => Token::new(TokenType::Minus, self.ch, self.line),
            '*' => Token::new(TokenType::Asterisk, self.ch, self.line),
            '/' => Token::new(TokenType::Slash, self.ch, self.line),
            '<' => Token::new(TokenType::Lt, self.ch, self.line),
            '>' => Token::new(TokenType::Gt, self.ch, self.line),

            // Delimiters
            ';' => Token::new(TokenType::Semicolon, self.ch, self.line),
            ':' => Token::new(TokenType::Colon, self.ch, self.line),
            '(' => Token::new(TokenType::LParen, self.ch, self.line),
            ')' => Token::new(TokenType::RParen, self.ch, self.line),
            ',' => Token::new(TokenType::Comma, self.ch, self.line),
            '{' => Token::new(TokenType::LBrace, self.ch, self.line),
            '}' => Token::new(TokenType::RBrace, self.ch, self.line),
            '[' => Token::new(TokenType::LBracket, self.ch, self.line),
            ']' => Token::new(TokenType::RBracket, self.ch, self.line),

            '\0' => Token::new(TokenType::Eof, self.ch, self.line),

            // Int, Ident
            // TODO: pattern match
            _ => {
                return if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let tt = lookup_ident(&literal);
                    Token::new(tt, literal, self.line)
                } else if self.ch.is_numeric() {
                    let number = self.read_number();
                    Token::new(TokenType::Int, number, self.line)
                } else {
                    let ch = self.ch;
                    self.read_char();
                    Token::new(TokenType::Illegal, ch, self.line)
                };
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
            if self.ch == '\n' {
                self.line += 1;
            }
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        while self.ch != '\n' {
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

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::token::TokenType;

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = fs::read_to_string("./src/lexer/input.txt").unwrap();

        let mut lexer = Lexer::new(&input);

        let tests = vec![
            // let five = 5;
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            // let ten = 10;
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            // let add = fn (x, y) { x + y; }
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            // let result = add(five, ten);
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            // !-/*5;
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            // 5 < 10 > 5;
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Gt, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            // if (5 < 10) { return true; }
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            // else { return false; }
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            // 10 == 10;
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            // 10 != 10;
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            // "foobar";
            (TokenType::String, "foobar"),
            // "foo bar";
            (TokenType::String, "foo bar"),
            // [1, 2];
            (TokenType::LBracket, "["),
            (TokenType::Int, "1"),
            (TokenType::Comma, ","),
            (TokenType::Int, "2"),
            (TokenType::RBracket, "]"),
            (TokenType::Semicolon, ";"),
            // {"foo": "bar"};
            (TokenType::LBrace, "{"),
            (TokenType::String, "foo"),
            (TokenType::Colon, ":"),
            (TokenType::String, "bar"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            // null
            (TokenType::Null, "null"),
            //
            (TokenType::Eof, "\0"),
        ];

        for (i, tt) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(tok.ttype, tt.0, "tests[{}] {:?}", i, tok);
            assert_eq!(tok.literal, tt.1, "tests[{}] {:?}", i, tok);
            println!("[line {}] {:?}({:?})", tok.line, tok.ttype, tok.literal);
        }
    }
}
