use super::*;

pub fn print_ast(ast: Ast) -> String {
    match ast {
        Ast::Expression(expr) => match expr {
            Expression::Integer(i) => print_integer(i),
            Expression::String(i) => print_string(i),
            Expression::Boolean(i) => print_boolean(i),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn print_integer(i: IntegerLiteral) -> String {
    i.value.to_string()
}

fn print_string(i: StringLiteral) -> String {
    i.value.to_string()
}

fn print_boolean(i: BooleanLiteral) -> String {
    i.value.to_string()
}
