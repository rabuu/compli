use std::fmt;

use chumsky::prelude::*;

use crate::Spanned;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    KwBool,
    Bool(bool),
    KwInt,
    Int(u16),

    Ident(String),

    Decl,
    Func,

    Assign,
    Equals,
    Less,
    Plus,
    Minus,
    And,

    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,

    Comma,
    Colon,
    Semicolon,
    Arrow,

    If,
    Then,
    Else,
    Let,
    In,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::KwBool => write!(f, "bool"),
            Token::Bool(b) => write!(f, "{}", b),
            Token::KwInt => write!(f, "int"),
            Token::Int(i) => write!(f, "{}", i),
            Token::Ident(id) => write!(f, "{}", id),
            Token::Decl => write!(f, "decl"),
            Token::Func => write!(f, "func"),
            Token::Assign => write!(f, "="),
            Token::Equals => write!(f, "=="),
            Token::Less => write!(f, "<"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::And => write!(f, "&&"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::CurlyOpen => write!(f, "{{"),
            Token::CurlyClose => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Arrow => write!(f, "->"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
        }
    }
}

pub fn lex() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let integer = text::int(10).from_str().unwrapped().map(Token::Int);

    let symbol = choice((
        just("==").to(Token::Equals),
        just("&&").to(Token::And),
        just("->").to(Token::Arrow),
        one_of("(){}=<+-,:;").map(|symb: char| match symb {
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '{' => Token::CurlyOpen,
            '}' => Token::CurlyClose,
            '=' => Token::Assign,
            '<' => Token::Less,
            '+' => Token::Plus,
            '-' => Token::Minus,
            ',' => Token::Comma,
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            _ => unreachable!(),
        }),
    ));

    let kw_or_ident = text::ident().map(|ident: String| match ident.as_str() {
        "bool" => Token::KwBool,
        "int" => Token::KwInt,
        "decl" => Token::Decl,
        "func" => Token::Func,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "let" => Token::Let,
        "in" => Token::In,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        _ => Token::Ident(ident),
    });

    let token = integer.or(symbol).or(kw_or_ident);

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}
