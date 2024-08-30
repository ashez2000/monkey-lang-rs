use super::*;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        token: Token,
        ident: Identifier,
        expr: Expression,
    },

    Return {
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

impl AstNode for Statement {
    fn to_string(&self) -> String {
        match self {
            Self::Let { ident, expr, .. } => {
                format!("let {} = {};", ident.to_string(), expr.to_string())
            }

            Self::Return { expr, .. } => expr.to_string(),
            Self::Expression { expr, .. } => expr.to_string(),
            Self::Block { .. } => "block".to_string(),
        }
    }
}
