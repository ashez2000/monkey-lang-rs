mod lexer;
mod repl;
mod token;

fn main() {
    println!("Welcome to Monkey Lang v0.1.0");
    repl::start();
}
