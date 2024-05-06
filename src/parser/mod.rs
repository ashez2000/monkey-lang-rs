mod precedence;
mod tests;

use std::collections::HashMap;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

use self::precedence::{token_to_precedence, Precedence};

type PrefixParseFn = fn(parser: &mut Parser) -> Option<Expression>;
type InfixParseFn = fn(parser: &mut Parser, exp: Expression) -> Option<Expression>;

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
        parser.register_prefix(TokenType::True, Self::parse_boolean);
        parser.register_prefix(TokenType::False, Self::parse_boolean);
        parser.register_prefix(TokenType::LParen, Self::parse_grouped_expression);
        parser.register_prefix(TokenType::If, Self::parse_if_expression);
        parser.register_prefix(TokenType::Function, Self::parse_function_literal);
        parser.register_prefix(TokenType::String, Self::parse_string_literal);
        parser.register_prefix(TokenType::LBracket, Self::parse_array_literal);

        parser.register_infix(TokenType::Plus, Self::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Self::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Self::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Self::parse_infix_expression);
        parser.register_infix(TokenType::Eq, Self::parse_infix_expression);
        parser.register_infix(TokenType::NotEq, Self::parse_infix_expression);
        parser.register_infix(TokenType::Lt, Self::parse_infix_expression);
        parser.register_infix(TokenType::Gt, Self::parse_infix_expression);
        parser.register_infix(TokenType::LParen, Self::parse_call_expression);
        parser.register_infix(TokenType::LBracket, Self::parse_index_expression);

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

        self.next_token();

        let let_stmt = LetStatement {
            token: Token::new(TokenType::Let, "let".to_string()),
            ident,
            expr: self.parse_expression(Precedence::Lowest),
        };

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(let_stmt))
    }

    // parse_return_statement
    //
    fn parse_return_statement(&mut self) -> Option<Statement> {
        let mut return_stmt = ReturnStatement {
            token: self.cur_token.clone(),
            expr: None,
        };

        self.next_token();

        return_stmt.expr = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&TokenType::Semicolon) {
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

        expr_stmt.expr = self.parse_expression(Precedence::Lowest);

        // optional semicolon check (usecase for repl)
        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token()
        }

        Some(Statement::Expression(expr_stmt))
    }

    // parse_block_statement
    //
    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            statements: vec![],
        };

        self.next_token();

        while !self.cur_token_is(&TokenType::RBrace) && !self.cur_token_is(&TokenType::Eof) {
            let stmt = self.parse_statement();

            if let Some(stmt) = stmt {
                block.statements.push(stmt);
            }

            self.next_token();
        }

        block
    }

    //
    // #### expressions ####
    //

    // main expression parser
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
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

    // parse_boolean
    //
    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Boolean(BooleanLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token_is(&TokenType::True),
        }))
    }

    // parse string literal
    //
    fn parse_string_literal(&mut self) -> Option<Expression> {
        Some(Expression::String(StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
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

        prefix_expr.expr = match self.parse_expression(Precedence::Prefix) {
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

    // parse_grouped_expression
    //
    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        expr
    }

    // parse_if_expression
    //
    fn parse_if_expression(&mut self) -> Option<Expression> {
        let mut if_expr = IfExpression {
            token: self.cur_token.clone(),
            alternative: None,
            condition: Default::default(),
            consequence: Default::default(),
        };

        if !self.expect_peek(&TokenType::LParen) {
            return None;
        }

        self.next_token();

        // TODO: refactor to Result
        if_expr.condition = Box::new(
            self.parse_expression(Precedence::Lowest)
                .expect("error parsing condition"),
        );

        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(&TokenType::LBrace) {
            return None;
        }

        if_expr.consequence = self.parse_block_statement();

        if self.peek_token_is(&TokenType::Else) {
            self.next_token();

            if !self.expect_peek(&TokenType::LBrace) {
                return None;
            }

            if_expr.alternative = Some(self.parse_block_statement());
        }

        Some(Expression::If(if_expr))
    }

    // parse_function_literal
    //
    fn parse_function_literal(&mut self) -> Option<Expression> {
        let mut fn_lit = FunctionLiteral {
            token: self.cur_token.clone(),
            body: Default::default(),
            parameters: vec![],
        };

        if !self.expect_peek(&TokenType::LParen) {
            return None;
        }

        fn_lit.parameters = self
            .parse_function_parameters()
            .expect("error parsing fn parameters");

        if !self.expect_peek(&TokenType::LBrace) {
            return None;
        }

        fn_lit.body = self.parse_block_statement();

        Some(Expression::Fn(fn_lit))
    }

    // parse_function_paramenters
    //
    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = vec![];

        // zero fn params
        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = Identifier {
            token: self.cur_token.clone(),
            name: self.cur_token.literal.clone(),
        };

        identifiers.push(ident);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();

            let ident = Identifier {
                token: self.cur_token.clone(),
                name: self.cur_token.literal.clone(),
            };

            identifiers.push(ident);
        }

        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }

        Some(identifiers)
    }

    // parse_call_expression
    //
    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        self.next_token();

        let mut call_expr = CallExpression {
            token: self.cur_token.clone(),
            function: Box::new(function),
            arguments: vec![],
        };

        call_expr.arguments = self
            .parse_expression_list(&TokenType::RParen)
            .expect("error parsing arguments");

        Some(Expression::Call(call_expr))
    }

    fn parse_expression_list(&mut self, end: &TokenType) -> Option<Vec<Expression>> {
        let mut args = vec![];

        if self.peek_token_is(end) {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        args.push(
            self.parse_expression(Precedence::Lowest)
                .expect("error parsing arguments"),
        );

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(
                self.parse_expression(Precedence::Lowest)
                    .expect("error parsing arguments"),
            )
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(args)
    }

    // parse_array_literal
    fn parse_array_literal(&mut self) -> Option<Expression> {
        let array = ArrayLiteral {
            token: self.cur_token.clone(),
            elements: self.parse_expression_list(&TokenType::RBracket).unwrap(),
        };
        Some(Expression::Array(array))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let mut expr = IndexExpression {
            token: self.cur_token.clone(),
            left: Box::new(left),
            index: Default::default(),
        };

        self.next_token();

        expr.index = Box::new(
            self.parse_expression(Precedence::Lowest)
                .expect("error parsing index expression"),
        );

        if !self.expect_peek(&TokenType::RBracket) {
            return None;
        }

        Some(Expression::Index(expr))
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

    fn peek_precedence(&self) -> Precedence {
        token_to_precedence(&self.peek_token.ttype)
    }

    fn cur_precedence(&self) -> Precedence {
        token_to_precedence(&self.cur_token.ttype)
    }
}
