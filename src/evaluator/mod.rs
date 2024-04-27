mod tests;

use crate::ast::*;
use crate::object::*;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn eval_program(&self, program: Program) -> Object {
        let mut result = Object::Null;

        for stmt in program.statements {
            result = self.eval_statement(stmt);
        }

        result
    }

    fn eval_statement(&self, stmt: Statement) -> Object {
        match stmt {
            Statement::Expression(expr_stmt) => self.eval_expression(expr_stmt.expr),
            _ => Object::Null,
        }
    }

    fn eval_expression(&self, expression: Option<Expression>) -> Object {
        if let Some(expr) = expression {
            return match expr {
                Expression::Integer(i) => Object::Integer(i.value),
                Expression::Boolean(b) => Object::Boolean(b.value),
                Expression::Prefix(prefix_expr) => {
                    let e = self.eval_expression(Some(*prefix_expr.expr));
                    return Self::eval_prefix_expression(prefix_expr.operator, e);
                }
                _ => Object::Null,
            };
        }
        Object::Null
    }

    fn eval_prefix_expression(operator: String, right: Object) -> Object {
        match operator.as_str() {
            "!" => Self::eval_bang_operator_expression(right),
            "-" => Self::eval_minus_prefix_operator_expression(right),
            _ => NULL,
        }
    }

    fn eval_bang_operator_expression(right: Object) -> Object {
        match right {
            Object::Boolean(true) => FALSE,
            Object::Boolean(false) => TRUE,
            Object::Null => TRUE,
            _ => FALSE,
        }
    }

    fn eval_minus_prefix_operator_expression(right: Object) -> Object {
        match right {
            Object::Integer(int) => Object::Integer(-int),
            _ => NULL,
        }
    }

    fn native_bool_to_boolean_object(bool: bool) -> Object {
        if bool {
            TRUE
        } else {
            FALSE
        }
    }
}
