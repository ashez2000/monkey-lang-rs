mod tests;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::default(),
            peek_token: Token::default(),
        };

        // set cur and peek tokens
        parser.next_token();
        parser.next_token();
        parser
    }

    // returns parsed AST of the program
    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::default();

        while !self.cur_token_is(TokenType::Eof) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt)
            }
            self.next_token();
        }

        Some(program)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    //
    // #### statements ####
    //

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.ttype {
            TokenType::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let ident = Identifier {
            token: self.cur_token.clone(),
            name: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        // TODO: parse <expr>
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        let let_stmt = LetStatement {
            token: Token::new(TokenType::Let, "let".to_string()),
            ident,
            expr: None,
        };

        Some(Statement::Let(let_stmt))
    }

    //
    // #### helpers ####
    //

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.ttype == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.ttype == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }
}
