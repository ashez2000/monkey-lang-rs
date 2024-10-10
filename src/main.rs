use std::env;
use std::fs;
use std::io::{stdin, stdout, Write};

use monkey::evaluator::Evaluator;
use monkey::lexer::Lexer;
use monkey::object::Object;
use monkey::parser::Parser;

fn main() {
    let args: Vec<_> = env::args().collect();

    match args.len() {
        1 => {
            println!("Welcome to Monkey Lang v0.1.0.");
            repl();
        }

        2 => {
            let source = fs::read_to_string(&args[1]).expect("Failed to read script, Invalid path");
            let mut evaluator = Evaluator::new();

            if let Some((r, out)) = run(&source, &mut evaluator) {
                if r.object_type() == "ERROR" {
                    println!("RUNTIME_ERROR: {}", r)
                } else {
                    for o in out {
                        println!("{}", o);
                    }
                }
            }
        }

        _ => panic!("Usage: monkey [script]"),
    }
}

fn repl() {
    let mut evaluator = Evaluator::new();

    loop {
        // Write prompt to the screen
        print!(">> ");
        stdout().flush().unwrap();

        // Read line from stdio
        let mut buf = String::new();
        stdin().read_line(&mut buf).unwrap();

        if let Some((r, _)) = run(&buf, &mut evaluator) {
            if r.object_type() == "ERROR" {
                println!("RUNTIME_ERROR: {}", r)
            } else {
                println!("{}", r);
            }
        }
    }
}

fn run(source: &str, eval: &mut Evaluator) -> Option<(Object, Vec<String>)> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.get_errors().len() != 0 {
        print_parser_error(parser.get_errors());
        return None;
    }

    Some((eval.eval_program(program), eval.out.clone()))
}

fn print_parser_error(errors: &Vec<String>) {
    for e in errors {
        println!("ERROR: {}", e);
    }
}
