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
    Int(String),
    Bool(bool),
    Float(String),

    Ident(String),

    KwBool,
    KwInt,
    KwFloat,

    KwFunc,
    KwIf,
    KwThen,
    KwElse,
    KwLet,
    KwIn,

    Assign,
    Equals,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Plus,
    Minus,
    Asterisk,
    Slash,
    And,
    Or,
    Bang,

    ParenOpen,
    ParenClose,

    Comma,
    Colon,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::KwBool => write!(f, "bool"),
            Token::Bool(b) => write!(f, "{}", b),
            Token::KwInt => write!(f, "int"),
            Token::Int(i) => write!(f, "{}", i),
            Token::KwFloat => write!(f, "float"),
            Token::Float(x) => write!(f, "{}", x),
            Token::Ident(id) => write!(f, "{}", id),
            Token::KwFunc => write!(f, "func"),
            Token::Assign => write!(f, "="),
            Token::Equals => write!(f, "=="),
            Token::Less => write!(f, "<"),
            Token::LessEq => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEq => write!(f, ">="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Bang => write!(f, "!"),
            Token::KwIf => write!(f, "if"),
            Token::KwThen => write!(f, "then"),
            Token::KwElse => write!(f, "else"),
            Token::KwLet => write!(f, "let"),
            Token::KwIn => write!(f, "in"),
        }
    }
}

/// Tokenize source code
pub fn lex() -> impl Parser<char, Vec<(Token, Span)>, Error = ParseErr<char>> {
    let integer = text::int(10).map(Token::Int);

    let float = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .collect::<String>()
        .map(Token::Float);

    let symbol = choice((
        just("==").to(Token::Equals),
        just("<=").to(Token::LessEq),
        just(">=").to(Token::GreaterEq),
        just("&&").to(Token::And),
        just("||").to(Token::Or),
        one_of("()=<>+-*/,:!").map(|symb: char| match symb {
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '=' => Token::Assign,
            '<' => Token::Less,
            '>' => Token::Greater,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '!' => Token::Bang,
            _ => unreachable!(),
        }),
    ));

    let kw_or_ident = text::ident().map(|ident: String| match ident.as_str() {
        "bool" => Token::KwBool,
        "int" => Token::KwInt,
        "float" => Token::KwFloat,
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

    let token = float.or(integer).or(symbol).or(kw_or_ident);

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}
