use std::{
    fmt::{self, Display},
    num::ParseFloatError,
};

use crate::{
    diag::{Diag, DiagEmitter},
    Sp, Span,
};

pub type Token = Sp<TokenKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Number(f32),
    Identifier(String),

    LetKeyword,

    Equals,
    Comma,

    ParenOpen,
    ParenClose,

    Plus,
    Minus,
    Star,
    Slash,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{num}"),
            Self::Identifier(_) => write!(f, "<identifier>"),
            Self::Equals => write!(f, "="),
            Self::Comma => write!(f, ","),
            Self::ParenOpen => write!(f, "("),
            Self::ParenClose => write!(f, ")"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::LetKeyword => write!(f, "let"),
        }
    }
}

pub struct Lexer<'inp, 'em> {
    input: &'inp [u8],
    position: usize,
    emitter: &'em DiagEmitter<'em>,
    span_offset: usize,
}

impl<'inp, 'em> Lexer<'inp, 'em> {
    pub fn new(input: &'inp str, emitter: &'em DiagEmitter<'em>, span_offset: usize) -> Self {
        assert!(input.is_ascii());
        Self {
            input: input.as_bytes(),
            position: 0,
            emitter,
            span_offset,
        }
    }

    fn new_span(&self, start: usize, end: usize) -> Span {
        Span::new(self.span_offset + start, self.span_offset + end)
    }

    fn next_char(&mut self) -> Option<u8> {
        let ch = self.peek_char()?;
        self.position += 1;
        Some(ch)
    }

    fn peek_char(&self) -> Option<u8> {
        (self.position < self.input.len()).then(|| self.input[self.position])
    }

    fn next_token(&mut self) -> Option<Result<Token, Diag>> {
        let ch = loop {
            let ch = self.next_char()?;
            if !ch.is_ascii_whitespace() {
                break ch;
            }
        };

        let span_start = self.position - 1;

        let kind = match ch {
            b'=' => TokenKind::Equals,
            b',' => TokenKind::Comma,
            b'+' => TokenKind::Plus,
            b'-' => TokenKind::Minus,
            b'*' => TokenKind::Star,
            b'/' => TokenKind::Slash,
            b'(' => TokenKind::ParenOpen,
            b')' => TokenKind::ParenClose,

            ch if ch.is_ascii_alphabetic() => {
                let id = self.next_identifier(ch);
                match id.as_str() {
                    "let" => TokenKind::LetKeyword,
                    _ => TokenKind::Identifier(id),
                }
            }

            ch if ch.is_ascii_digit() || ch == b'.' => {
                if let Ok(num) = self.next_number(ch) {
                    TokenKind::Number(num)
                } else {
                    let err = self.emitter.create_err(
                        "invalid float literal",
                        self.new_span(span_start, self.position),
                    );

                    return Some(Err(err));
                }
            }

            ch => {
                let err = self.emitter.create_err(
                    format!("invalid start of token `{}`", ch as char),
                    self.new_span(span_start, self.position),
                );

                return Some(Err(err));
            }
        };

        let span = self.new_span(span_start, self.position);

        Some(Ok(Token::new(kind, span)))
    }

    fn next_identifier(&mut self, initial: u8) -> String {
        let mut ident = (initial as char).to_string();
        while let Some(ch) = self.peek_char() {
            if !ch.is_ascii_alphabetic() {
                break;
            }

            self.position += 1;
            ident.push(ch as char);
        }

        ident
    }

    fn next_number(&mut self, initial: u8) -> Result<f32, ParseFloatError> {
        let mut num = (initial as char).to_string();
        while let Some(ch) = self.peek_char() {
            if !(ch.is_ascii_digit() || ch == b'.') {
                break;
            }

            self.position += 1;
            num.push(ch as char);
        }

        num.parse()
    }
}

impl Iterator for Lexer<'_, '_> {
    type Item = Result<Token, Diag>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
