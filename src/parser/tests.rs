use std::any::Any;

use crate::ast::*;
use crate::lexer::*;

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

    let tests = vec!["let x = 5;", "let y = 10;", "let foobar = 838383;"];

    match program {
        None => panic!("expected Some(program), got None"),
        Some(program) => {
            assert_eq!(program.statements.len(), 3);
            let res: Vec<_> = program.statements.iter().map(|s| s.to_string()).collect();
            assert_eq!(res, tests);
        }
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
            for (i, _) in tests.into_iter().enumerate() {
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

// #### test precedence parsing ####

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

// #### test if expression ####

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    check_parser_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expression(expr_stmt) => match expr_stmt.expr.as_ref().unwrap() {
            Expression::If(if_expr) => {
                // TODO: deal with Any/Box<String>
                test_infix_expression(
                    &if_expr.condition,
                    Box::new(String::from("x")),
                    String::from("<"),
                    Box::new(String::from("y")),
                );

                assert_eq!(if_expr.consequence.statements.len(), 1);

                match &if_expr.consequence.statements[0] {
                    Statement::Expression(consequence) => test_identifier(
                        consequence
                            .expr
                            .as_ref()
                            .expect("error parsing consequence"),
                        "x",
                    ),
                    _ => panic!("expected Statement::Expression"),
                }

                assert!(&if_expr.alternative.is_none());
            }
            _ => panic!("expected Expression::IF"),
        },
        _ => panic!("expected Statement::Expression"),
    }
}

// #### test if else expression ####

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    check_parser_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expression(expr_stmt) => match expr_stmt.expr.as_ref().unwrap() {
            Expression::If(if_expr) => {
                // TODO: deal with Any/Box<String>
                test_infix_expression(
                    &if_expr.condition,
                    Box::new(String::from("x")),
                    String::from("<"),
                    Box::new(String::from("y")),
                );

                assert_eq!(if_expr.consequence.statements.len(), 1);

                match &if_expr.consequence.statements[0] {
                    Statement::Expression(consequence) => test_identifier(
                        consequence
                            .expr
                            .as_ref()
                            .expect("error parsing consequence"),
                        "x",
                    ),
                    _ => panic!("expected Statement::Expression"),
                }
            }
            _ => panic!("expected Expression::IF"),
        },
        _ => panic!("expected Statement::Expression"),
    }
}

// #### test function literals parsing ####
#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    check_parser_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expression(expr_stmt) => match expr_stmt.expr.as_ref().unwrap() {
            Expression::Fn(fn_lit) => {
                assert_eq!(fn_lit.parameters.len(), 2);

                match &fn_lit.parameters[0] {
                    Identifier { token, name } => {
                        assert_eq!(name, "x");
                        assert_eq!(token.literal, "x");
                    }
                }

                match &fn_lit.parameters[1] {
                    Identifier { token, name } => {
                        assert_eq!(name, "y");
                        assert_eq!(token.literal, "y");
                    }
                }

                assert_eq!(fn_lit.body.statements.len(), 1);

                match &fn_lit.body.statements[0] {
                    Statement::Expression(expr_stmt) => test_infix_expression(
                        expr_stmt.expr.as_ref().unwrap(),
                        Box::new(String::from("x")),
                        String::from("+"),
                        Box::new(String::from("y")),
                    ),
                    _ => panic!("expected Statement::Expression"),
                }
            }
            _ => panic!("expected Expression::Fn"),
        },
        _ => panic!("expected Statement::Expression"),
    }
}

#[test]
fn test_function_parameter_parsing() {
    let tests = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x"]),
        ("fn(x, y, z) {};", vec!["x", "y", "z"]),
    ];

    for test in tests {
        let lexer = Lexer::new(test.0);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(&parser);

        match &program.statements[0] {
            Statement::Expression(expr_stmt) => match expr_stmt.expr.as_ref().unwrap() {
                Expression::Fn(fn_lit) => {
                    assert_eq!(fn_lit.parameters.len(), test.1.len());

                    for (idx, ident) in test.1.into_iter().enumerate() {
                        assert_eq!(fn_lit.parameters[idx].name, ident);
                    }
                }
                _ => panic!("expected Expression::Fn"),
            },
            _ => panic!("expected Statement::Expression"),
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    check_parser_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        Statement::Expression(expr_stmt) => match expr_stmt.expr.as_ref().unwrap() {
            Expression::Call(call_expr) => {
                test_identifier(&call_expr.function, "add");
                assert_eq!(call_expr.arguments.len(), 3);
                test_literal_expression(&call_expr.arguments[0], Box::new(1_i64));
                test_infix_expression(
                    &call_expr.arguments[1],
                    Box::new(2_i64),
                    String::from("*"),
                    Box::new(3_i64),
                );
                test_infix_expression(
                    &call_expr.arguments[2],
                    Box::new(4_i64),
                    String::from("+"),
                    Box::new(5_i64),
                );
            }
            _ => panic!("expected Expression::Call"),
        },
        _ => panic!("exptected Statement::Expression"),
    }
}

#[test]
fn test_string_literal_expression() {
    let input = r#""hello world""#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    match &program.unwrap().statements[0] {
        Statement::Expression(expr_stmt) => {
            match &expr_stmt.expr.as_ref().expect("error parsing expression") {
                Expression::String(str_literal) => {
                    assert_eq!(
                        str_literal.value, "hello world",
                        "str_literal value not `hello world` got={}",
                        str_literal.value
                    );
                }
                other => panic!("not a string literal. got={:?}", other),
            }
        }
        other => panic!("not an expression statement. got={:?}", other),
    }
}

#[test]
fn test_parsing_array_literal() {
    let input = "[1, 2 * 2, 3 + 3]";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    match &program.unwrap().statements[0] {
        Statement::Expression(exp_stmt) => {
            match &exp_stmt.expr.as_ref().expect("error parsing expression") {
                Expression::Array(array_literal) => {
                    assert_eq!(
                        array_literal.elements.len(),
                        3,
                        "Length of array literal elements not 3. got={}",
                        array_literal.elements.len()
                    );
                    test_integer_literal(&array_literal.elements[0], 1);
                    test_infix_expression(
                        &array_literal.elements[1],
                        Box::new(2_i64),
                        String::from("*"),
                        Box::new(2_i64),
                    );
                    test_infix_expression(
                        &array_literal.elements[2],
                        Box::new(3_i64),
                        String::from("+"),
                        Box::new(3_i64),
                    );
                }
                other => panic!("not a array literal. got={:?}", other),
            }
        }
        other => panic!("not an expression statement. got={:?}", other),
    }
}

#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[1 + 1]";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parser_errors(&parser);

    match &program.unwrap().statements[0] {
        Statement::Expression(exp_stmt) => {
            match &exp_stmt.expr.as_ref().expect("error parsing expression") {
                Expression::Index(index_exp) => {
                    test_identifier(&index_exp.left, "myArray");
                    test_infix_expression(
                        &index_exp.index,
                        Box::new(1_i64),
                        String::from("+"),
                        Box::new(1_i64),
                    );
                }
                other => panic!("not an index expression. got={:?}", other),
            }
        }
        other => panic!("not an expression statement. got={:?}", other),
    }
}

// #### test helpers ####

fn test_integer_literal(expr: &Expression, value: i64) {
    match expr {
        Expression::Integer(i) => {
            assert_eq!(i.value, value);
        }
        _ => panic!("expected Expression::Integer"),
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
