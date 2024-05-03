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

#[test]
fn test_error_handling() {
    let tests = vec![
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        (
            "if (10 > 1) {
            if (10 > 1) {
            return true + false;
            }
            return 1;
            }
            ",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        match evaluated {
            Object::Error(err) => assert_eq!(err, test.1),
            other => panic!("no error object returned. got={:?}", other),
        }
    }
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for test in tests {
        test_integer_object(test_eval(test.0), test.1);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; }";
    let evaluated = test_eval(input);

    match evaluated {
        Object::Fn(function) => {
            assert_eq!(
                function.parameters.len(),
                1,
                "function has wrong parameters length. got={}",
                function.parameters.len()
            );
            assert_eq!(
                function.parameters[0].to_string(),
                "x",
                "parameter is not `x`, got={}",
                function.parameters[0].to_string()
            );
            assert_eq!(
                function.body.to_string(),
                "(x + 2)",
                "body is not `(x + 2)`. got={}",
                function.body.to_string()
            );
        }
        other => panic!("object is not Function. got={:?}", other),
    }
}

#[test]
fn test_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x+ y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];

    for test in tests {
        test_integer_object(test_eval(test.0), test.1);
    }
}

#[test]
fn test_string_literal() {
    let input = r#""Hello World!""#;
    let evaluated = test_eval(input);

    match evaluated {
        Object::String(str) => {
            assert_eq!(str, "Hello World!", "String has wrong value. got={}", str);
        }
        other => panic!("object is not string. got={:?}", other),
    }
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;
    let evaluated = test_eval(input);

    match evaluated {
        Object::String(string) => assert_eq!(
            string, "Hello World!",
            "string has wrong value. got={}",
            string
        ),
        other => panic!("object is not string literal. got={:?}", other),
    }
}

#[test]
fn test_builtin_functions() {
    use std::any::Any;

    let tests: Vec<(&str, Box<dyn Any>)> = vec![
        (r#"len("")"#, Box::new(0_i64)),
        (r#"len("four")"#, Box::new(4_i64)),
        (r#"len("hello world")"#, Box::new(11_i64)),
        (
            r#"len(1)"#,
            Box::new(String::from("argument to 'len' not supported, got INTEGER")),
        ),
        (
            r#"len("one", "two")"#,
            Box::new(String::from("wrong number of arguments. got=2, want=1")),
        ),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        match test.1.downcast_ref::<i64>() {
            Some(expected) => test_integer_object(evaluated, *expected),
            None => match test.1.downcast_ref::<String>() {
                Some(expected) => match evaluated {
                    Object::Error(err) => assert_eq!(
                        err, *expected,
                        "wrong error message. expected={}, got={}",
                        *expected, err
                    ),
                    other => panic!("object is not error. got={}", other),
                },
                None => panic!("should not happen"),
            },
        }
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input);

    match evaluated {
        Object::Array(elements) => {
            assert_eq!(
                elements.len(),
                3,
                "array has wrong num of elements. got={}",
                elements.len()
            );
            test_integer_object(elements[0].clone(), 1);
            test_integer_object(elements[1].clone(), 4);
            test_integer_object(elements[2].clone(), 6);
        }
        other => panic!("object is not array, got={:?}", other),
    }
}

#[test]
fn test_array_index_expressions() {
    use std::any::Any;

    let tests: Vec<(&str, Box<dyn Any>)> = vec![
        ("[1, 2, 3][0]", Box::new(1_i64)),
        ("[1, 2, 3][1]", Box::new(2_i64)),
        ("[1, 2, 3][2]", Box::new(3_i64)),
        ("let i = 0; [1][i];", Box::new(1_i64)),
        ("[1, 2, 3][1 + 1];", Box::new(3_i64)),
        ("let myArray = [1, 2, 3]; myArray[2];", Box::new(3_i64)),
        (
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            Box::new(6_i64),
        ),
        (
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            Box::new(2_i64),
        ),
        ("[1, 2, 3][3]", Box::new(NULL)),
        ("[1, 2, 3][-1]", Box::new(NULL)),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        match test.1.downcast_ref::<i64>() {
            Some(expected) => test_integer_object(evaluated, *expected),
            None => test_null_object(evaluated),
        }
    }
}

fn test_eval(input: &str) -> Object {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    let mut evaluator = Evaluator::new();
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
