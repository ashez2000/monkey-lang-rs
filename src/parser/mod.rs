mod tests;

use std::collections::HashMap;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

type PrefixParseFn = fn(parser: &mut Parser) -> Option<Expression>;
type InfixParseFn = fn(parser: &mut Parser, exp: Expression) -> Option<Expression>;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum PrecedenceLevel {
    Lowest,
    Equals,
    LtGt,
    Sum,
    Product,
    Prefix,
}

fn precedence_map(kind: &TokenType) -> PrecedenceLevel {
    return match kind {
        TokenType::Eq => PrecedenceLevel::Equals,
        TokenType::NotEq => PrecedenceLevel::Equals,
        TokenType::Lt => PrecedenceLevel::LtGt,
        TokenType::Gt => PrecedenceLevel::LtGt,
        TokenType::Plus => PrecedenceLevel::Sum,
        TokenType::Minus => PrecedenceLevel::Sum,
        TokenType::Slash => PrecedenceLevel::Product,
        TokenType::Asterisk => PrecedenceLevel::Product,
        _ => PrecedenceLevel::Lowest,
    };
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::Ident, Self::parse_identifier);
        parser.register_prefix(TokenType::Int, Self::parse_integer_literal);
        parser.register_prefix(TokenType::Bang, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Self::parse_prefix_expression);

        parser.register_infix(TokenType::Plus, Self::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Self::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Self::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Self::parse_infix_expression);
        parser.register_infix(TokenType::Eq, Self::parse_infix_expression);
        parser.register_infix(TokenType::NotEq, Self::parse_infix_expression);
        parser.register_infix(TokenType::Lt, Self::parse_infix_expression);
        parser.register_infix(TokenType::Gt, Self::parse_infix_expression);

        // set cur and peek tokens
        parser.next_token();
        parser.next_token();
        parser
    }

    // returns parsed AST of the program
    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::default();

        while !self.cur_token_is(&TokenType::Eof) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt)
            }
            self.next_token();
        }

        Some(program)
    }

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    // #### parse statements ####
    //
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.ttype {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    // parse_let_statement
    //
    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(&TokenType::Ident) {
            return None;
        }

        let ident = Identifier {
            token: self.cur_token.clone(),
            name: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(&TokenType::Assign) {
            return None;
        }

        // TODO: parse <expr>
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        let let_stmt = LetStatement {
            token: Token::new(TokenType::Let, "let".to_string()),
            ident,
            expr: None,
        };

        Some(Statement::Let(let_stmt))
    }

    // parse_return_statement
    //
    fn parse_return_statement(&mut self) -> Option<Statement> {
        let return_stmt = ReturnStatement {
            token: self.cur_token.clone(),
            expr: None,
        };

        self.next_token();

        // TODO: parse <expr>
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(return_stmt))
    }

    // parse_expression_statement
    //
    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let mut expr_stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expr: None,
        };

        expr_stmt.expr = self.parse_expression(PrecedenceLevel::Lowest);

        // optional semicolon check (usecase for repl)
        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token()
        }

        Some(Statement::Expression(expr_stmt))
    }

    //
    // #### expressions ####
    //

    // main expression parser
    fn parse_expression(&mut self, precedence: PrecedenceLevel) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.ttype);

        if let Some(prefix_fn) = prefix {
            let mut left_expr = prefix_fn(self);

            // ????
            while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence()
            {
                let infix = self.infix_parse_fns.get(&self.peek_token.ttype);
                if let Some(infix_fn) = infix {
                    left_expr = infix_fn(self, left_expr.unwrap());
                }
            }
            return left_expr;
        }

        // TODO: figure out clone issue
        self.no_prefix_parse_fn_error(&self.cur_token.ttype.clone());
        None
    }

    // parse_identifier
    //
    fn parse_identifier(&mut self) -> Option<Expression> {
        let ident = Identifier {
            token: self.cur_token.clone(),
            name: self.cur_token.literal.clone(),
        };

        Some(Expression::Ident(ident))
    }

    // parse_integer_literal
    //
    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let mut literal = IntegerLiteral {
            token: self.cur_token.clone(),
            value: Default::default(),
        };

        match self.cur_token.literal.parse::<i64>() {
            Ok(value) => {
                literal.value = value;
                Some(Expression::Integer(literal))
            }
            Err(_) => {
                let msg = format!("could not parse {} as integer", self.cur_token.literal);
                self.errors.push(msg);
                None
            }
        }
    }

    // parse prefix expression
    //
    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let mut prefix_expr = PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            expr: Default::default(),
        };

        self.next_token();

        prefix_expr.expr = match self.parse_expression(PrecedenceLevel::Prefix) {
            Some(expr) => Box::new(expr),
            None => return None,
        };

        Some(Expression::Prefix(prefix_expr))
    }

    // parse infix expression
    //
    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token(); // ???

        let mut infix_expr = InfixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            left: Box::new(left),
            right: Default::default(),
        };

        let p = self.cur_precedence();
        self.next_token();

        match self.parse_expression(p) {
            Some(expr) => infix_expr.right = Box::new(expr),
            None => return None,
        }

        Some(Expression::Infix(infix_expr))
    }

    //
    // #### helpers ####
    //

    fn cur_token_is(&self, t: &TokenType) -> bool {
        self.cur_token.ttype == *t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token.ttype == *t
    }

    fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn peek_error(&mut self, t: &TokenType) {
        let msg = format!(
            "expected next token: {:?}, got {:?}",
            t, self.cur_token.ttype
        );
        self.errors.push(msg);
    }

    fn register_prefix(&mut self, token_kind: TokenType, prefix_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_kind, prefix_fn);
    }

    fn register_infix(&mut self, token_kind: TokenType, infix_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token_kind, infix_fn);
    }

    fn no_prefix_parse_fn_error(&mut self, t: &TokenType) {
        let msg = format!("no prefix parse fn for {:?}", t);
        self.errors.push(msg);
    }

    fn peek_precedence(&self) -> PrecedenceLevel {
        precedence_map(&self.peek_token.ttype)
    }

    fn cur_precedence(&self) -> PrecedenceLevel {
        precedence_map(&self.cur_token.ttype)
    }
}
