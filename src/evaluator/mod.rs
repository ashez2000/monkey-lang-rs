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
                _ => Object::Null,
            };
        }
        Object::Null
    }

    fn native_bool_to_boolean_object(bool: bool) -> Object {
        if bool {
            TRUE
        } else {
            FALSE
        }
    }
}
