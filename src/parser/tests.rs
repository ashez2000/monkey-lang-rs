use std::any::Any;

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
        _ => panic!("expected Statement::Let"),
    }
}

// #### return statements ####

#[test]
fn test_return_statements() {
    let input = r"
        return 5;
        return 10;
        return 993322;
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
                match stmt {
                    Statement::Return(return_stmt) => {
                        assert_eq!(return_stmt.token.literal, "return")
                    }
                    _ => panic!("expected Statement::Return"),
                }
            }
        }
    }
}

// #### identifiers ####

#[test]
fn test_identifier_expression() {
    let input = "foobar;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    // TODO: too much Option stuff, reduce it
    match program {
        None => panic!("expected Some(program), got None"),
        Some(program) => {
            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expr_stmt) => match &expr_stmt.expr {
                    Some(expr) => match expr {
                        Expression::Ident(i) => {
                            assert_eq!(i.name, "foobar")
                        }
                        _ => panic!("expected Expression::Ident"),
                    },
                    None => panic!("expected Some expression stmt"),
                },
                _ => panic!("expected Statement::Expression"),
            }
        }
    }
}

// #### integer literal ####

#[test]
fn testt_integer_literal() {
    let input = "5;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    // TODO: too much Option stuff, reduce it
    match program {
        None => panic!("expected Some(program), got None"),
        Some(program) => {
            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expr_stmt) => match &expr_stmt.expr {
                    Some(expr) => match expr {
                        Expression::Integer(i) => {
                            assert_eq!(i.value, 5)
                        }
                        _ => panic!("expected Expression::Integer"),
                    },
                    None => panic!("expected Some expression stmt"),
                },
                _ => panic!("expected Statement::Expression"),
            }
        }
    }
}

// #### boolean literal ####

#[test]
fn test_boolean_literal() {
    let input = "true;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    match program {
        None => panic!("expected Some(program), got None"),
        Some(program) => {
            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expr_stmt) => match &expr_stmt.expr {
                    Some(expr) => match expr {
                        Expression::Boolean(i) => {
                            assert_eq!(i.value, true)
                        }
                        _ => panic!("expected Expression::Boolean"),
                    },
                    None => panic!("expected Some expression stmt"),
                },
                _ => panic!("expected Statement::Expression"),
            }
        }
    }
}

// #### prefix expressions ####

#[test]
fn test_parsing_prefix_expressions() {
    let prefix_tests = vec![("!5", "!", 5), ("-15", "-", 15)];

    for (input, op, val) in prefix_tests {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("expected Some(Program)");
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(expr_stmt) => {
                let expr = expr_stmt.expr.as_ref().expect("expected Some(Expression)");

                match expr {
                    Expression::Prefix(prefix_exp) => {
                        assert_eq!(prefix_exp.operator, op);
                        test_integer_literal(&prefix_exp.expr, val);
                    }
                    other => panic!("expected Expression::Prefix. got={:?}", other),
                }
            }

            other => panic!("expected Statement::Expression, got = {:?}", other),
        }
    }
}

// #### test infix expressions ####

#[test]
fn test_infix_expressions() {
    let infix_tests: Vec<(&str, i64, &str, i64)> = vec![
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
    ];

    for (input, left, op, right) in infix_tests {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("expected Some(Program)");
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(expr_stmt) => {
                let expr = expr_stmt.expr.as_ref().expect("expected Some(Expression)");
                test_infix_expression(expr, Box::new(left), op.into(), Box::new(right));
            }

            other => panic!("expected Statement::Expression, got = {:?}", other),
        }
    }
}

#[test]
fn test_precedence_parsing() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
    ];

    for t in tests {
        let lexer = Lexer::new(t.0);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("expected Some(Program)");
        check_parser_errors(&parser);

        assert_eq!(program.to_string(), t.1)
    }
}

// #### test helpers ####

fn test_integer_literal(expr: &Expression, value: i64) {
    match expr {
        Expression::Integer(i) => {
            assert_eq!(i.value, value);
        }
        other => panic!(
            "expected Expression::Integer, got = {}",
            other.token_literal()
        ),
    }
}

fn test_identifier(expr: &Expression, value: &str) {
    match expr {
        Expression::Ident(i) => {
            assert_eq!(i.name, value);
        }
        _ => panic!("expected Expression::Ident"),
    }
}

fn test_literal_expression(expr: &Expression, expected: Box<dyn Any>) {
    if let Some(s) = expected.downcast_ref::<String>() {
        test_identifier(expr, s);
        return;
    }

    if let Some(i) = expected.downcast_ref::<i64>() {
        test_integer_literal(expr, i.to_owned());
        return;
    }

    panic!("invalid type")
}

fn test_infix_expression(
    expr: &Expression,
    left: Box<dyn Any>,
    operator: String,
    right: Box<dyn Any>,
) {
    match expr {
        Expression::Infix(infix_exp) => {
            test_literal_expression(&infix_exp.left, left);
            assert_eq!(
                infix_exp.operator, operator,
                "operator is not {}. got={}",
                operator, infix_exp.operator
            );
            test_literal_expression(&infix_exp.right, right);
        }
        _ => panic!("expected Expression::Infix"),
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
