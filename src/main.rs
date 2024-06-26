use std::env;
use std::fs;

use monkey::*;

fn print_parse_errors(errors: &Vec<String>) {
    println!("ERROR:");
    for e in errors {
        println!("\t-> {}", e);
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();

    match args.len() {
        1 => {
            println!("Welcome to Monkey Lang v0.1.0.");
            repl::start();
        }

        2 => {
            let input = fs::read_to_string(&args[1]).unwrap();
            let lexer = lexer::Lexer::new(&input);
            let mut parser = parser::Parser::new(lexer);
            let mut eval = evaluator::Evaluator::new();
            let program = parser.parse_program();
            let _res = eval.eval_program(program);
            if parser.get_errors().len() != 0 {
                print_parse_errors(parser.get_errors());
            }

            // println!("{}", _res);
        }

        _ => panic!("Invalid args len"),
    }
}
