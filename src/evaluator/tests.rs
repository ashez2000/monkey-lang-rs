use crate::evaluator::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;

#[test]
fn test_eval_integer_expression() {
    let tests = vec![("5", 5), ("10", 10)];

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
