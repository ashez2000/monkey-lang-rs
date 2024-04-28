mod tests;

use crate::ast::*;
use crate::object::*;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        let mut result = Object::Null;

        for stmt in program.statements {
            result = self.eval_statement(stmt);
            if let Object::Return(value) = result {
                return *value;
            }

            if let Object::Error(_) = result {
                return result;
            }
        }

        result
    }

    fn eval_statement(&mut self, stmt: Statement) -> Object {
        match stmt {
            Statement::Expression(expr_stmt) => self.eval_expression(expr_stmt.expr),
            Statement::Return(ret_stmt) => {
                let value = self.eval_expression(ret_stmt.expr);
                if Self::is_error(&value) {
                    return value;
                }
                return Object::Return(Box::new(value));
            }
            Statement::Let(let_stmt) => {
                let value = self.eval_expression(let_stmt.expr);
                if Self::is_error(&value) {
                    return value;
                }

                self.env.set(let_stmt.ident.name, value).unwrap()
            }
            _ => Object::Null,
        }
    }

    fn eval_expression(&mut self, expression: Option<Expression>) -> Object {
        if let Some(expr) = expression {
            return match expr {
                Expression::Integer(i) => Object::Integer(i.value),
                Expression::Boolean(b) => Object::Boolean(b.value),
                Expression::Prefix(prefix_expr) => {
                    let e = self.eval_expression(Some(*prefix_expr.expr));
                    return Self::eval_prefix_expression(prefix_expr.operator, e);
                }
                Expression::Infix(infix_exp) => {
                    let left = self.eval_expression(Some(*infix_exp.left));
                    let right = self.eval_expression(Some(*infix_exp.right));
                    return Self::eval_infix_expression(infix_exp.operator, &left, &right);
                }
                Expression::If(if_expr) => self.eval_if_expression(if_expr),
                Expression::Ident(i) => self.eval_identifier(i),
                _ => Object::Null,
            };
        }
        Object::Null
    }

    fn eval_prefix_expression(operator: String, right: Object) -> Object {
        match operator.as_str() {
            "!" => Self::eval_bang_operator_expression(right),
            "-" => Self::eval_minus_prefix_operator_expression(right),
            _ => Object::Error(format!(
                "unknown operator: {}{}",
                operator,
                right.object_type()
            )),
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
            _ => Object::Error(format!("unknown operator: -{}", right.object_type())),
        }
    }

    fn eval_infix_expression(operator: String, left: &Object, right: &Object) -> Object {
        if left.object_type() != right.object_type() {
            return Object::Error(format!(
                "type mismatch: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ));
        }

        match (left, right, operator) {
            (Object::Integer(left), Object::Integer(right), op) => {
                Self::eval_integer_infix_expression(op, *left, *right)
            }
            (Object::Boolean(l), Object::Boolean(r), operator) => {
                return match operator.as_str() {
                    "==" => Self::native_bool_to_boolean_object(l == r),
                    "!=" => Self::native_bool_to_boolean_object(l != r),
                    _ => Object::Error(format!(
                        "unknown operator: {} {} {}",
                        left.object_type(),
                        operator,
                        right.object_type()
                    )),
                };
            }
            (left, right, operator) => Object::Error(format!(
                "unknown operator: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            )),
        }
    }

    fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Object {
        match operator.as_str() {
            "+" => Object::Integer(left + right),
            "-" => Object::Integer(left - right),
            "*" => Object::Integer(left * right),
            "/" => Object::Integer(left / right),
            "<" => Self::native_bool_to_boolean_object(left < right),
            ">" => Self::native_bool_to_boolean_object(left > right),
            "==" => Self::native_bool_to_boolean_object(left == right),
            "!=" => Self::native_bool_to_boolean_object(left != right),
            _ => NULL,
        }
    }

    fn eval_block_statement(&mut self, block: BlockStatement) -> Object {
        let mut result = NULL;

        for stmt in block.statements {
            result = self.eval_statement(stmt);
            if let Object::Return(_) = result {
                return result;
            }

            if let Object::Error(_) = result {
                return result;
            }
        }

        result
    }

    fn eval_if_expression(&mut self, exp: IfExpression) -> Object {
        let condition = self.eval_expression(Some(*exp.condition));

        return if Self::is_truthy(condition) {
            self.eval_block_statement(exp.consequence)
        } else if let Some(alt) = exp.alternative {
            self.eval_block_statement(alt)
        } else {
            NULL
        };
    }

    fn eval_identifier(&self, identifier: Identifier) -> Object {
        let value = self.env.get(identifier.name.clone());
        match value {
            Some(val) => val,
            None => Object::Error(format!("identifier not found: {}", identifier.name)),
        }
    }

    fn native_bool_to_boolean_object(bool: bool) -> Object {
        if bool {
            TRUE
        } else {
            FALSE
        }
    }

    fn is_truthy(obj: Object) -> bool {
        match obj {
            Object::Null => false,
            Object::Boolean(true) => true,
            Object::Boolean(false) => false,
            _ => true,
        }
    }

    fn is_error(obj: &Object) -> bool {
        obj.object_type() == "ERROR"
    }
}
