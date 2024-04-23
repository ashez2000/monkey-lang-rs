use crate::token::*;

// AstNode:
// base node interface of AST
// every node of AST implements AstNode
// TODO: refactor/remove the entire trait
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
#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl AstNode for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(let_stmt) => let_stmt.token.literal.clone(),
            Self::Return(return_stmt) => return_stmt.token.literal.clone(),
            Self::Expression(expr_stmt) => expr_stmt.token.literal.clone(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Let(let_stmt) => let_stmt.to_string(),
            Self::Return(return_stmt) => return_stmt.to_string(),
            Self::Expression(expr_stmt) => expr_stmt.to_string(),
        }
    }
}

// Expression:
// expressions in Monkey Lang
#[derive(Debug)]
pub enum Expression {
    Ident(Identifier),
}

impl AstNode for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Ident(i) => i.token.literal.clone(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Ident(i) => i.to_string(),
        }
    }
}

// Program:
// root AST / represents Monkey Lang program as a list of statements
#[derive(Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl AstNode for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".into()
        }
    }

    fn to_string(&self) -> String {
        if self.statements.len() > 0 {
            let mut out = String::new();
            for s in &self.statements {
                out.push_str(&s.to_string());
            }
            out
        } else {
            "".into()
        }
    }
}

// LetStatement:
// structure of a let statement
// let <name> = <expr>;
#[derive(Debug)]
pub struct LetStatement {
    pub token: Token, // Let token
    pub ident: Identifier,
    pub expr: Option<Expression>,
}

impl AstNode for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str(&self.token_literal());
        out.push_str(" ");
        out.push_str(&self.ident.to_string());
        out.push_str(" = ");

        if let Some(expr) = &self.expr {
            out.push_str(&expr.to_string())
        }

        out.push_str(";");

        out
    }
}

// ReturnStatement
// return <expr>;
#[derive(Debug)]
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
        let mut out = String::new();

        out.push_str(&self.token_literal());
        out.push_str(" ");

        if let Some(expr) = &self.expr {
            out.push_str(&expr.to_string())
        }

        out.push_str(";");

        out
    }
}

// ExpressionStatement:
// expression as a statement
// exx: 1 + 2 + foo;
#[derive(Debug)]
pub struct ExpressionStatement {
    // first token of expr
    // what is the need for this ???
    pub token: Token,
    pub expr: Option<Expression>,
}

impl AstNode for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        if let Some(expression) = &self.expr {
            expression.to_string()
        } else {
            "".into()
        }
    }
}

// Identifier:
#[derive(Debug)]
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
