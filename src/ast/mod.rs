use crate::token::*;
// AstNode:
// base node interface of AST
// every node of AST implements AstNode
// TODO: refactor/remove the entire trait
pub trait AstNode {
    // literal value of token
    // used for debugging and testing
    // fn token_literal(&self) -> String;

    // string representation of AST struct
    // for testing and debugging
    fn to_string(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Ast {
    Statement(Statement),
    Expression(Expression),
}

impl AstNode for Ast {
    fn to_string(&self) -> String {
        match self {
            Self::Statement(stmt) => stmt.to_string(),
            Self::Expression(expr) => expr.to_string(),
        }
    }
}

// Statement:
// statement types in Monkey Lang
#[derive(Debug, Clone)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl AstNode for Statement {
    fn to_string(&self) -> String {
        match self {
            Self::Let(ident, expr) => format!("let {} = {};", ident.to_string(), expr.to_string()),
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
    Array(ArrayLiteral),
    Index(IndexExpression),
}

impl AstNode for Expression {
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
            Self::Array(i) => i.to_string(),
            Self::Index(i) => i.to_string(),
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

// ReturnStatement
// return <expr>;
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub expr: Option<Expression>,
}

impl AstNode for ReturnStatement {
    // TODO: to_string for expr
    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str("return");
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
    fn to_string(&self) -> String {
        let mut out = String::from("");

        for stmt in &self.statements {
            out.push_str(stmt.to_string().as_str());
        }

        out
    }
}

// Identifier Ast
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(pub String);

impl AstNode for Identifier {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

// IntegerLiteral:
#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token, // Int token
    pub value: i64,
}

impl AstNode for IntegerLiteral {
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
    fn to_string(&self) -> String {
        let mut out = String::from("");
        let mut params = vec![];

        for param in &self.parameters {
            params.push(param.to_string());
        }

        out.push_str("fn");
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

// StringLiteral:
#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl AstNode for StringLiteral {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

// ArrayLiteral:
#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token, // LBracket
    pub elements: Vec<Expression>,
}

impl AstNode for ArrayLiteral {
    fn to_string(&self) -> String {
        let mut out = String::from("");
        let mut elements = vec![];

        for el in &self.elements {
            elements.push(el.to_string());
        }

        out.push_str("[");
        out.push_str(elements.join(", ").as_str());
        out.push_str("]");

        out
    }
}

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: Vec<(Expression, Expression)>,
}

impl AstNode for HashLiteral {
    fn to_string(&self) -> String {
        let mut out = String::from("");
        let mut pairs = vec![];

        for (key, value) in &self.pairs {
            pairs.push(format!("{}: {}", key.to_string(), value.to_string()))
        }

        out.push_str("{");
        out.push_str(pairs.join(", ").as_str());
        out.push_str("}");

        out
    }
}

// IndexExpression
// <expr>[<expr>];
// ex: foo[1], [1, 2][1 - 1]
#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub token: Token, // LBracket
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl AstNode for IndexExpression {
    fn to_string(&self) -> String {
        let mut out = String::from("");

        out.push_str("(");
        out.push_str(self.left.to_string().as_str());
        out.push_str("[");
        out.push_str(self.index.to_string().as_str());
        out.push_str("])");

        out
    }
}
