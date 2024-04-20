use super::*;
use crate::token::*;

#[test]
fn test_next_token() {
    let input = "=+(){},;";

    let tests = vec![
        Token::new(TokenType::Assign, "=".into()),
        Token::new(TokenType::Plus, "+".into()),
        Token::new(TokenType::LParen, "(".into()),
        Token::new(TokenType::RParen, ")".into()),
        Token::new(TokenType::LBrace, "{".into()),
        Token::new(TokenType::RBrace, "}".into()),
        Token::new(TokenType::Comma, ",".into()),
        Token::new(TokenType::Semicolon, ";".into()),
        Token::new(TokenType::Eof, "\0".into()),
    ];

    let mut lexer = Lexer::new(input);

    for tt in tests {
        let tok = lexer.next_token();
        assert_eq!(tok.ttype, tt.ttype);
        assert_eq!(tok.literal, tt.literal);
    }
}
