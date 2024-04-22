use super::*;
use crate::token::*;

#[test]
fn test_single_char_tokens() {
    let input = r"
        =+-!*/<>
        ,;(){}
        &
    ";

    let tests = vec![
        // operators
        Token::new(TokenType::Assign, "=".into()),
        Token::new(TokenType::Plus, "+".into()),
        Token::new(TokenType::Minus, "-".into()),
        Token::new(TokenType::Bang, "!".into()),
        Token::new(TokenType::Asterisk, "*".into()),
        Token::new(TokenType::Slash, "/".into()),
        Token::new(TokenType::Lt, "<".into()),
        Token::new(TokenType::Gt, ">".into()),
        // delimiters
        Token::new(TokenType::Comma, ",".into()),
        Token::new(TokenType::Semicolon, ";".into()),
        Token::new(TokenType::LParen, "(".into()),
        Token::new(TokenType::RParen, ")".into()),
        Token::new(TokenType::LBrace, "{".into()),
        Token::new(TokenType::RBrace, "}".into()),
        // eof
        Token::new(TokenType::Illegal, "&".into()),
        Token::new(TokenType::Eof, "\0".into()),
    ];

    let mut lexer = Lexer::new(input);

    for tt in tests {
        let tok = lexer.next_token();
        assert_eq!(tok.ttype, tt.ttype);
        assert_eq!(tok.literal, tt.literal);
    }
}

#[test]
fn test_keyword_tokens() {
    let input = r"
        fn
        let
        true
        false
        if
        else
        return
    ";

    let tests = vec![
        Token::new(TokenType::Function, "fn".into()),
        Token::new(TokenType::Let, "let".into()),
        Token::new(TokenType::True, "true".into()),
        Token::new(TokenType::False, "false".into()),
        Token::new(TokenType::If, "if".into()),
        Token::new(TokenType::Else, "else".into()),
        Token::new(TokenType::Return, "return".into()),
        Token::new(TokenType::Eof, "\0".into()),
    ];

    let mut lexer = Lexer::new(input);

    for tt in tests {
        let tok = lexer.next_token();
        assert_eq!(tok.ttype, tt.ttype);
        assert_eq!(tok.literal, tt.literal);
    }
}

#[test]
fn test_number_tokens() {
    let input = r"
        1
        100
        1000
    ";

    let tests = vec![
        Token::new(TokenType::Int, "1".into()),
        Token::new(TokenType::Int, "100".into()),
        Token::new(TokenType::Int, "1000".into()),
        Token::new(TokenType::Eof, "\0".into()),
    ];

    let mut lexer = Lexer::new(input);

    for tt in tests {
        let tok = lexer.next_token();
        assert_eq!(tok.ttype, tt.ttype);
        assert_eq!(tok.literal, tt.literal);
    }
}

#[test]
fn test_identifier_tokens() {
    let input = r"
       foo
       foo_bar
       _foo
       f _
    ";

    let tests = vec![
        Token::new(TokenType::Ident, "foo".into()),
        Token::new(TokenType::Ident, "foo_bar".into()),
        Token::new(TokenType::Ident, "_foo".into()),
        Token::new(TokenType::Ident, "f".into()),
        Token::new(TokenType::Ident, "_".into()),
        Token::new(TokenType::Eof, "\0".into()),
    ];

    let mut lexer = Lexer::new(input);

    for tt in tests {
        let tok = lexer.next_token();
        assert_eq!(tok.ttype, tt.ttype);
        assert_eq!(tok.literal, tt.literal);
    }
}

#[test]
fn test_double_char_tokens() {
    let input = r"
       ==
       != 
    ";

    let tests = vec![
        Token::new(TokenType::Eq, "==".into()),
        Token::new(TokenType::NotEq, "!=".into()),
        Token::new(TokenType::Eof, "\0".into()),
    ];

    let mut lexer = Lexer::new(input);

    for tt in tests {
        let tok = lexer.next_token();
        assert_eq!(tok.ttype, tt.ttype);
        assert_eq!(tok.literal, tt.literal);
    }
}
