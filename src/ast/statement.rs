use super::*;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        token: Token,
        ident: Ident,
        expr: Expression,
    },

    Return {
        token: Token,
        expr: Expression,
    },

    Print {
        token: Token,
        expr: Expression,
    },

    Expression {
        token: Token,
        expr: Expression,
    },

    Block {
        token: Token,
        statements: Vec<Statement>,
    },
}
