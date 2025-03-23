//! Lexer
//!
//! This submodule prepares source code for parsing by splitting the text into easy-to-work-with
//! tokens. The [lexer] parser is its main interface.

use std::fmt;

use chumsky::input::MappedSpan;
use chumsky::prelude::*;

use crate::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<'src> {
    Int(&'src str),
    Bool(bool),
    Float(&'src str),

    Ident(&'src str),

    KwBool,
    KwInt,
    KwFloat,

    KwDef,
    KwRec,
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
    Dot,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::KwBool => write!(f, "bool"),
            Token::Bool(b) => write!(f, "{}", b),
            Token::KwInt => write!(f, "int"),
            Token::Int(i) => write!(f, "{}", i),
            Token::KwFloat => write!(f, "float"),
            Token::Float(x) => write!(f, "{}", x),
            Token::Ident(id) => write!(f, "{}", id),
            Token::KwDef => write!(f, "def"),
            Token::KwRec => write!(f, "rec"),
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
            Token::Dot => write!(f, "."),
            Token::Bang => write!(f, "!"),
            Token::KwIf => write!(f, "if"),
            Token::KwThen => write!(f, "then"),
            Token::KwElse => write!(f, "else"),
            Token::KwLet => write!(f, "let"),
            Token::KwIn => write!(f, "in"),
        }
    }
}

type LexInput<'src, F> = MappedSpan<Span, &'src str, F>;
type LexOutput<'src> = Vec<(Token<'src>, Span)>;
type LexErr<'src> = chumsky::extra::Err<Rich<'src, char, Span>>;

/// Tokenize source code
pub fn lexer<'src, F>() -> impl Parser<'src, LexInput<'src, F>, LexOutput<'src>, LexErr<'src>>
where
    F: Fn(SimpleSpan) -> Span + 'src,
{
    let integer = text::int(10).map(Token::Int);

    let float = text::int(10)
        .then(just('.').then(text::digits(10)))
        .to_slice()
        .map(Token::Float);

    let symbol = choice((
        just("==").to(Token::Equals),
        just("<=").to(Token::LessEq),
        just(">=").to(Token::GreaterEq),
        just("&&").to(Token::And),
        just("||").to(Token::Or),
        one_of("()=<>+-*/,:.!").map(|symb: char| match symb {
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
            '.' => Token::Dot,
            '!' => Token::Bang,
            _ => unreachable!(),
        }),
    ));

    let kw_or_ident = text::ident().map(|ident| match ident {
        "bool" => Token::KwBool,
        "int" => Token::KwInt,
        "float" => Token::KwFloat,
        "def" => Token::KwDef,
        "rec" => Token::KwRec,
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

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(src: &str) -> Vec<(Token, Span)> {
        lexer().parse(src.map_span(Into::into)).into_output().unwrap()
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
                Token::Int("0"),
                Token::Float("0.0"),
                Token::Float("123.4")
            ]
        );
    }

    #[test]
    fn spans() {
        let src = r#"def
let
    in
        "#;

        assert_eq!(
            tokenize(src),
            vec![
                (Token::KwDef, Span::new(0, 3)),
                (Token::KwLet, Span::new(4, 7)),
                (Token::KwIn, Span::new(12, 14))
            ]
        );
    }

    #[test]
    fn function() {
        let src = r#"
def foo(a: int): float = (
    let b = 1, c = true in
    if 3 > 4 then 123.4 else 0.0
)
        "#;

        assert_eq!(
            tokenize_without_spans(src),
            vec![
                Token::KwDef,
                Token::Ident("foo"),
                Token::ParenOpen,
                Token::Ident("a"),
                Token::Colon,
                Token::KwInt,
                Token::ParenClose,
                Token::Colon,
                Token::KwFloat,
                Token::Assign,
                Token::ParenOpen,
                Token::KwLet,
                Token::Ident("b"),
                Token::Assign,
                Token::Int("1"),
                Token::Comma,
                Token::Ident("c"),
                Token::Assign,
                Token::Bool(true),
                Token::KwIn,
                Token::KwIf,
                Token::Int("3"),
                Token::Greater,
                Token::Int("4"),
                Token::KwThen,
                Token::Float("123.4"),
                Token::KwElse,
                Token::Float("0.0"),
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
