use std::{fmt, str::FromStr};

use crate::Sp;

/// A spanned token/lexme.
///
/// Is a spanned wrapper for [`TokenKind`], the "meat" of each token.
pub type Token = Sp<TokenKind>;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.inner(), self.span())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // "complex" tokens
    Literal(Literal),
    Identifier(String),

    // keywords
    Keyword(Keyword),

    // misc punctuation
    Equals,
    Comma,
    Semi,

    // delimiters
    ParenOpen,
    ParenClose,

    // operators
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
}

impl TokenKind {
    /// Returns a string literal which identifies this token *as it should look in a diagnostic*.
    ///
    /// The caller should wrap the returned value in backticks (`` ` ` ``)
    /// before displaying in a diagnostic.
    pub fn diag_str(&self) -> &'static str {
        match self {
            Self::Literal(_) => "<float literal>",
            Self::Identifier(_) => "<identifier>",
            Self::Equals => "=",
            Self::Comma => ",",
            Self::Semi => ";",
            Self::ParenOpen => "(",
            Self::ParenClose => ")",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Bang => "!",
            Self::Keyword(kw) => kw.diag_str(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    Float(f32),
    Bool(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Float(x) => write!(f, "{x}"),
            Self::Bool(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    Let,
    Fn,
}

impl FromStr for Keyword {
    type Err = ();

    /// Attempts to get a keyword from a given string.
    ///
    /// This function is used by the lexer.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "let" => Self::Let,
            "fn" => Self::Fn,
            _ => return Err(()),
        })
    }
}

impl Keyword {
    /// Derived function for getting the "diagnostic-friendly" representation
    /// of a token. See the docs on [`TokenKind::diag_str`].
    pub fn diag_str(&self) -> &'static str {
        match self {
            Self::Let => "let",
            Self::Fn => "fn",
        }
    }
}
