use crate::token::*;

// AstNode:
// base node interface of AST
// every node of AST implements AstNode
pub trait AstNode {
    // literal value of token
    // used for debugging and testing
    fn token_literal(&self) -> String;
}

// Statement:
// statement types in Monkey Lang
pub enum Statement {
    Let(LetStatement),
}

// Expression:
// expressions in Monkey Lang
pub enum Expression {
    Ident(Identifier),
}

// Program:
// root AST / represents Monkey Lang program as a list of statements
pub struct Program {
    pub statements: Vec<Statement>,
}

// LetStatement:
// structure of a let statement
// let <name> = <expr>;
pub struct LetStatement {
    pub token: Token, // Let token
    pub name: Identifier,
    pub expr: Option<Expression>,
}

impl AstNode for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
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
}
