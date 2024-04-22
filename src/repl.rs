use std::io::{stdin, stdout, Write};

use crate::lexer::*;
use crate::token::*;

pub fn start() {
    loop {
        print!(">> ");
        stdout().flush().unwrap();

        let mut buf = String::new();
        stdin().read_line(&mut buf).unwrap();

        let mut lexer = Lexer::new(&buf);
        loop {
            let tok = lexer.next_token();
            let mut out = format!("{:?}", tok.ttype);
            if tok.ttype == TokenType::Ident || tok.ttype == TokenType::Int {
                out.push_str(&format!("({})", tok.literal));
            }

            println!("{}", out);

            if tok.ttype == TokenType::Eof {
                break;
            }
        }
    }
}
