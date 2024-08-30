use crate::token::*;

mod statement;
pub use statement::Statement;

#[derive(Debug, Clone)]
pub enum Ast {
    Statement(Statement),
    Expression(Expression),
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
    Hash(HashLiteral),
    Index(IndexExpression),
}

// Program:
// root AST / represents Monkey Lang program as a list of statements
#[derive(Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

// ReturnStatement
// return <expr>;
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub expr: Option<Expression>,
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

// BlockStatement:
#[derive(Debug, Default, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

// Identifier Ast
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(pub String);

// IntegerLiteral:
#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token, // Int token
    pub value: i64,
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
// BooleanLiteral:
#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub token: Token, // True / False
    pub value: bool,
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

// FunctionLiteral:
// fn <params> <body>
#[derive(Debug, Default, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
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

// StringLiteral:
#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

// ArrayLiteral:
#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token, // LBracket
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: Vec<(Expression, Expression)>,
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
