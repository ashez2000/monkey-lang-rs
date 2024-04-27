use crate::evaluator::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        test_integer_object(evaluated, test.1);
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![("true", true), ("false", false)];

    for test in tests {
        let evaluated = test_eval(test.0);
        test_boolean_object(evaluated, test.1);
    }
}

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 > 2", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        test_boolean_object(evaluated, test.1);
    }
}

#[test]
fn test_if_else_expression() {
    let tests: Vec<(&str, Option<i64>)> = vec![
        ("if (true) { 10 }", Some(10)),
        ("if (false) { 10 }", None),
        ("if (1) { 10 }", Some(10)),
        ("if (1 < 2) { 10 }", Some(10)),
        ("if (1 > 2) { 10 }", None),
        ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ("if (1 < 2) { 10 } else { 20 }", Some(10)),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        match test.1 {
            Some(i) => test_integer_object(evaluated, i),
            None => test_null_object(evaluated),
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        ("return 10", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
            "if (10 > 1) {
            if (10 > 1) {
                return 10;
            }
            return 1;
        }",
            10,
        ),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        test_integer_object(evaluated, test.1);
    }
}

fn test_eval(input: &str) -> Object {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    let evaluator = Evaluator::new();
    evaluator.eval_program(program.unwrap())
}

fn test_integer_object(obj: Object, expected: i64) {
    match obj {
        Object::Integer(int) => assert_eq!(
            int, expected,
            "object has wrong value. got={}, want={}",
            int, expected
        ),
        other => panic!("object is not integer. got={:?}", other),
    }
}

fn test_boolean_object(obj: Object, expected: bool) {
    match obj {
        Object::Boolean(bool) => assert_eq!(
            bool, expected,
            "object has wrong value. got={}, want={}",
            bool, expected
        ),
        other => panic!("object is not bool. got={}", other),
    }
}

fn test_null_object(obj: Object) {
    match obj {
        Object::Null => assert!(true),
        _ => assert!(false),
    }
}
