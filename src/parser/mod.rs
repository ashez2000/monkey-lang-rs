mod tests;

use std::collections::HashMap;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

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
            _ => None,
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
}
