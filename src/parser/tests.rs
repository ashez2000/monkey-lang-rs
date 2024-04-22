use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

use super::*;

// #### let statements ####
#[test]
fn test_let_statements() {
    let input = r"
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    match program {
        None => panic!("expected Some(program), got None"),
        Some(program) => {
            assert_eq!(program.statements.len(), 3);

            let tests = vec!["x", "y", "foobar"];
            for (i, t) in tests.into_iter().enumerate() {
                let stmt = &program.statements[i];
                test_let_statement(stmt, t);
            }
        }
    }
}

fn test_let_statement(stmt: &Statement, expected: &str) {
    match stmt {
        Statement::Let(let_stmt) => {
            assert_eq!(let_stmt.ident.name, expected);
        }
        _ => panic!("not Statement::Let"),
    }
}

fn check_parser_errors(parser: &Parser) {
    let errors = parser.get_errors();

    if errors.len() == 0 {
        return;
    }

    for e in errors {
        eprintln!("{}", e)
    }

    panic!("parser errors");
}
