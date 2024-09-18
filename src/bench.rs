use std::{fs, time::Instant, vec};

use monkey::*;

static FIB_CODE: &str =
    "let fib = fn (n) { if (n < 2) { return n; }; return fib(n - 2) + fib(n - 1); }";

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let name = args.last().unwrap();

    let mut res: Vec<String> = vec![];

    for i in 0..25 {
        let mut input = String::from(FIB_CODE);
        input.push_str(&format!("fib({})", i));
        let start = Instant::now();
        let lexer = lexer::Lexer::new(&input);
        let mut parser = parser::Parser::new(lexer);
        let mut eval = evaluator::Evaluator::new();
        let program = parser.parse_program();
        let _res = eval.eval_program(program);
        let duration = start.elapsed();

        let r = format!("fib({}): {:?}", i, duration);
        println!("{}", r);
        res.push(r);
    }

    let mut s = String::new();
    for r in res {
        s.push_str(&r);
        s.push_str("\n");
    }

    fs::write(&format!("./fib-benches/{}.txt", name), s).unwrap();
}
