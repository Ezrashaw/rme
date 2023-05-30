use std::{fmt, str::FromStr};

/// A spanned token/lexeme.
///
/// Is a spanned wrapper for [`TokenKind`], the "meat" of each token.
pub type Token = crate::Sp<TokenKind>;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.inner(), self.span())
    }
}

/// Core lexer token type.
///
/// You should probably not be using this type; [`Token`] wraps this type with
/// a [`Span`](`crate::Span`).
///
/// Note that currently this type isn't `Copy`, solely because identifiers
/// aren't interned.
// FIXME: make `TokenKind` Copy` by interning identifiers.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum TokenKind {
    // "complex" tokens
    /// "Literal" values (including `true` and `false`)
    Literal(Literal),
    /// Identifiers (names)
    Identifier(String),

    // keywords
    /// Language keywords (special identifiers)
    ///
    /// Note that `true` and `false` are [`Literal`]s instead.
    Keyword(Keyword),

    // misc punctuation
    /// Equals sign (`=`)
    Equals,
    /// Comma (`,`)
    Comma,
    /// Semicolon (`;`)
    Semi,

    // delimiters
    /// Opening parenthesis (`(`)
    ParenOpen,
    /// Closing parenthesis (`)`)
    ParenClose,

    // operators
    /// Plus sign (`+`)
    Plus,
    /// Minus sign (`-`)
    Minus,
    /// Asterisk (`*`)
    Star,
    /// Forward slash (`/`)
    Slash,
    /// Exclamation mark (`!`)
    Bang,
    /// Less than sign (`<`)
    LeftArrow,
    /// Greater than sign (`>`)
    RightArrow,
    /// 2x Equals (`==`)
    DoubleEquals,
}

impl TokenKind {
    /// Returns a string literal which identifies this token *as it should look in a diagnostic*.
    ///
    /// The caller should wrap the returned value in backticks (`` `...` ``)
    /// before displaying in a diagnostic.
    #[must_use]
    pub const fn diag_str(&self) -> &'static str {
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
            Self::DoubleEquals => "==",
            Self::LeftArrow => "<",
            Self::RightArrow => ">",
            Self::Keyword(kw) => kw.diag_str(),
        }
    }
}

/// A "literal" value.
///
/// Either a floating-point number or "true"/"false", representing a value in
/// the program.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    /// Floating-point literal.
    ///
    /// Includes `.5`, `5.`, `1.1`, etc.
    Float(f32),

    /// Boolean literal.
    ///
    /// Either `true` or `false`
    Bool(bool),
}

impl fmt::Display for Literal {
    /// Displays the underlying value without any punctuation or surrounding
    /// text.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Float(x) => x.fmt(f),
            Self::Bool(x) => x.fmt(f),
        }
    }
}

/// RME keyword.
///
/// Implemented in the lexer as special-cased (or reserved) identifiers.
/// Note that [`Keyword`] does not own a heap-allocation, and is just a small
/// 1-byte enum.
///
/// Used as part of [`TokenKind`].
#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    /// `let` keyword.
    ///
    /// Used for defining variables.
    Let,

    /// `fn` keyword.
    ///
    /// Not currently in use.
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
    #[must_use]
    pub const fn diag_str(&self) -> &'static str {
        match self {
            Self::Let => "let",
            Self::Fn => "fn",
        }
    }
}
