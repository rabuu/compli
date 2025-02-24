//! Lexer
//!
//! This submodule prepares source code for parsing by splitting the text into easy-to-work-with
//! tokens. The [lex] function is its main interface.

use std::fmt;

use chumsky::prelude::*;

use crate::Span;

use super::ParseErr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Int(u16),
    Bool(bool),

    Ident(String),

    KwBool,
    KwInt,

    KwFunc,
    KwIf,
    KwThen,
    KwElse,
    KwLet,
    KwIn,

    Assign,
    Equals,
    Less,
    Plus,
    Minus,
    And,

    ParenOpen,
    ParenClose,

    Comma,
    Colon,
}

/// Tokenize source code
pub fn lex() -> impl Parser<char, Vec<(Token, Span)>, Error = ParseErr<char>> {
    let integer = text::int(10).from_str().unwrapped().map(Token::Int);

    let symbol = choice((
        just("==").to(Token::Equals),
        just("&&").to(Token::And),
        one_of("()=<+-,:").map(|symb: char| match symb {
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '=' => Token::Assign,
            '<' => Token::Less,
            '+' => Token::Plus,
            '-' => Token::Minus,
            ',' => Token::Comma,
            ':' => Token::Colon,
            _ => unreachable!(),
        }),
    ));

    let kw_or_ident = text::ident().map(|ident: String| match ident.as_str() {
        "bool" => Token::KwBool,
        "int" => Token::KwInt,
        "func" => Token::KwFunc,
        "if" => Token::KwIf,
        "then" => Token::KwThen,
        "else" => Token::KwElse,
        "let" => Token::KwLet,
        "in" => Token::KwIn,
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::KwBool => write!(f, "bool"),
            Token::Bool(b) => write!(f, "{}", b),
            Token::KwInt => write!(f, "int"),
            Token::Int(i) => write!(f, "{}", i),
            Token::Ident(id) => write!(f, "{}", id),
            Token::KwFunc => write!(f, "func"),
            Token::Assign => write!(f, "="),
            Token::Equals => write!(f, "=="),
            Token::Less => write!(f, "<"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::And => write!(f, "&&"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::KwIf => write!(f, "if"),
            Token::KwThen => write!(f, "then"),
            Token::KwElse => write!(f, "else"),
            Token::KwLet => write!(f, "let"),
            Token::KwIn => write!(f, "in"),
        }
    }
}
