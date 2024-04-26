use std::io::{stdin, stdout, Write};

use crate::ast::AstNode;
use crate::lexer::*;
use crate::parser::*;

pub fn start() {
    loop {
        print!(">> ");
        stdout().flush().unwrap();

        let mut buf = String::new();
        stdin().read_line(&mut buf).unwrap();

        let lexer = Lexer::new(&buf);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().expect("error parsing program");
        if parser.get_errors().len() != 0 {
            print_parse_errors(parser.get_errors());
            continue;
        }

        println!("{}", program.to_string());
    }
}

fn print_parse_errors(errors: &Vec<String>) {
    eprintln!("Oops! We ran into parser errors");
    for e in errors {
        eprintln!("\t-> {}", e);
    }
}
