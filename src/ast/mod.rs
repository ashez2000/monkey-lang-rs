use crate::token::*;

// AstNode:
// base node interface of AST
// every node of AST implements AstNode
pub trait AstNode {
    // literal value of token
    // used for debugging and testing
    fn token_literal(&self) -> String;

    // string representation of AST struct
    // for testing and debugging
    fn to_string(&self) -> String;
}

// Statement:
// statement types in Monkey Lang
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

// Expression:
// expressions in Monkey Lang
pub enum Expression {
    Ident(Identifier),
}

// Program:
// root AST / represents Monkey Lang program as a list of statements
#[derive(Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

// LetStatement:
// structure of a let statement
// let <name> = <expr>;
pub struct LetStatement {
    pub token: Token, // Let token
    pub ident: Identifier,
    pub expr: Option<Expression>,
}

impl AstNode for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    // TODO: to_string for expr
    fn to_string(&self) -> String {
        format!("let {} = {};", self.ident.to_string(), "None")
    }
}

// ReturnStatement
// return <expr>;
pub struct ReturnStatement {
    pub token: Token,
    pub expr: Option<Expression>,
}

impl AstNode for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    // TODO: to_string for expr
    fn to_string(&self) -> String {
        format!("return {}", "None")
    }
}

// Identifier:
// stucture for identifier as expression
pub struct Identifier {
    pub token: Token, // Ident token
    pub name: String,
}

impl AstNode for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.name.clone()
    }
}
