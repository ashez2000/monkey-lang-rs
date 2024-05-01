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
#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl AstNode for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(let_stmt) => let_stmt.token.literal.clone(),
            Self::Return(return_stmt) => return_stmt.token.literal.clone(),
            Self::Expression(expr_stmt) => expr_stmt.token.literal.clone(),
            Self::Block(block_stmt) => block_stmt.token.literal.clone(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Let(let_stmt) => let_stmt.to_string(),
            Self::Return(return_stmt) => return_stmt.to_string(),
            Self::Expression(expr_stmt) => expr_stmt.to_string(),
            Self::Block(block_stmt) => block_stmt.to_string(),
        }
    }
}

// Expression:
// expressions in Monkey Lang
#[derive(Debug, Default, Clone)]
pub enum Expression {
    #[default]
    None,
    Ident(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanLiteral),
    If(IfExpression),
    Fn(FunctionLiteral),
    Call(CallExpression),
    String(StringLiteral),
}

impl AstNode for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Ident(i) => i.token.literal.clone(),
            Self::Integer(i) => i.token.literal.clone(),
            Self::Prefix(i) => i.token.literal.clone(),
            Self::Infix(i) => i.token.literal.clone(),
            Self::Boolean(i) => i.token.literal.clone(),
            Self::If(i) => i.token.literal.clone(),
            Self::Fn(i) => i.token.literal.clone(),
            Self::Call(i) => i.token.literal.clone(),
            Self::String(i) => i.token.literal.clone(),
            Self::None => "".into(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Ident(i) => i.to_string(),
            Self::Integer(i) => i.to_string(),
            Self::Prefix(i) => i.to_string(),
            Self::Infix(i) => i.to_string(),
            Self::Boolean(i) => i.to_string(),
            Self::If(i) => i.to_string(),
            Self::Fn(i) => i.to_string(),
            Self::Call(i) => i.to_string(),
            Self::String(i) => i.to_string(),
            Self::None => "".into(),
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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

// BlockStatement:
#[derive(Debug, Default, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl AstNode for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::from("");

        for stmt in &self.statements {
            out.push_str(stmt.to_string().as_str());
        }

        out
    }
}

// Identifier:
#[derive(Debug, Clone)]
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

// IntegerLiteral:
#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token, // Int token
    pub value: i64,
}

impl AstNode for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

// PrefixExpression:
// <op><expr>;
// ex: !foo, -100
#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String, // TODO: narrow down type
    pub expr: Box<Expression>,
}

impl AstNode for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str("(");
        out.push_str(&self.operator);
        out.push_str(&self.expr.to_string());
        out.push_str(")");

        out
    }
}

// InfixExpression:
// <left><op><right>;
// ex: !foo, -100
#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub operator: String, // TODO: narrow down type
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl AstNode for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str("(");
        out.push_str(&self.left.to_string());
        out.push_str(" ");
        out.push_str(&self.operator);
        out.push_str(" ");
        out.push_str(&self.right.to_string());
        out.push_str(")");

        out
    }
}

// BooleanLiteral:
#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub token: Token, // True / False
    pub value: bool,
}

impl AstNode for BooleanLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

// IfExpression:
// if (<expr>) <consequence_block>
// if (<expr>) <consequence_block> else <alternative_block>
#[derive(Debug, Default, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl AstNode for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::from("");

        out.push_str("if");
        out.push_str(self.condition.to_string().as_str());
        out.push_str(" ");
        out.push_str(self.consequence.to_string().as_str());

        if let Some(alt) = &self.alternative {
            out.push_str("else ");
            out.push_str(alt.to_string().as_str());
        }

        out
    }
}

// FunctionLiteral:
// fn <params> <body>
#[derive(Debug, Default, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl AstNode for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::from("");
        let mut params = vec![];

        for param in &self.parameters {
            params.push(param.to_string());
        }

        out.push_str(self.token_literal().as_str());
        out.push_str("(");
        out.push_str(params.join(", ").as_str());
        out.push_str(")");
        out.push_str(self.body.to_string().as_str());

        out
    }
}

// CallExpression:
// <expr>(<args>);
// ex: foo(1+1), fn (n) { return n + 1; } (1);
#[derive(Debug, Default, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl AstNode for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::from("");
        let mut args = vec![];

        for arg in &self.arguments {
            args.push(arg.to_string());
        }

        out.push_str(self.function.to_string().as_str());
        out.push_str("(");
        out.push_str(args.join(", ").as_str());
        out.push_str(")");

        out
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl AstNode for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.token_literal()
    }
}
