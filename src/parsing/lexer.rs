//! Lexer
//!
//! This submodule prepares source code for parsing by splitting the text into easy-to-work-with
//! tokens. The [lex] parser is its main interface.

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

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(src: &str) -> Vec<(Token, Span)> {
        let eoi = Span::marker(src.chars().count());

        let chars = src.chars().enumerate().map(|(i, c)| (c, Span::single(i)));

        lex().parse(chumsky::Stream::from_iter(eoi, chars)).unwrap()
    }

    fn tokenize_without_spans(src: &str) -> Vec<Token> {
        tokenize(src).into_iter().map(|(tok, _)| tok).collect()
    }

    #[test]
    fn numbers() {
        let src = r#"0 0.0 123.4"#;

        assert_eq!(
            tokenize_without_spans(src),
            vec![
                Token::Int(String::from("0")),
                Token::Float(String::from("0.0")),
                Token::Float(String::from("123.4"))
            ]
        );
    }

    #[test]
    fn spans() {
        let src = r#"func
let
    in
        "#;

        assert_eq!(
            tokenize(src),
            vec![
                (Token::KwFunc, Span::new(0, 4)),
                (Token::KwLet, Span::new(5, 8)),
                (Token::KwIn, Span::new(13, 15))
            ]
        );
    }

    #[test]
    fn function() {
        let src = r#"
func foo(a: int): float = (
    let b = 1, c = true in
    if 3 > 4 then 123.4 else 0.0
)
        "#;

        assert_eq!(
            tokenize_without_spans(src),
            vec![
                Token::KwFunc,
                Token::Ident(String::from("foo")),
                Token::ParenOpen,
                Token::Ident(String::from("a")),
                Token::Colon,
                Token::KwInt,
                Token::ParenClose,
                Token::Colon,
                Token::KwFloat,
                Token::Assign,
                Token::ParenOpen,
                Token::KwLet,
                Token::Ident(String::from("b")),
                Token::Assign,
                Token::Int(String::from("1")),
                Token::Comma,
                Token::Ident(String::from("c")),
                Token::Assign,
                Token::Bool(true),
                Token::KwIn,
                Token::KwIf,
                Token::Int(String::from("3")),
                Token::Greater,
                Token::Int(String::from("4")),
                Token::KwThen,
                Token::Float(String::from("123.4")),
                Token::KwElse,
                Token::Float(String::from("0.0")),
                Token::ParenClose,
            ]
        );
    }

    #[test]
    #[should_panic]
    fn unknown_symbol() {
        let src = "#";
        let _ = tokenize(src);
    }
}
