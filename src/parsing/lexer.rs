use chumsky::prelude::*;

use super::Spanned;

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
}
