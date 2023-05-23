use crate::{DErr, Sp, Span};

#[cfg(test)]
mod tests;

/// A spanned token/lexme.
///
/// Is a spanned wrapper for [`TokenKind`], the "meat" of each token.
pub type Token = Sp<TokenKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // "complex" tokens
    Literal(f32),
    Identifier(String),

    // keywords
    // FIXME: at some point, this should be split off into a `Keyword` struct
    LetKeyword,

    // misc punctuation
    Equals,
    Comma,

    // delimiters
    ParenOpen,
    ParenClose,

    // operators
    Plus,
    Minus,
    Star,
    Slash,
}

impl TokenKind {
    /// Returns a string literal which identifies this token *as it should look in a diagnostic*.
    ///
    /// The caller should wrap the returned value in backticks (`` ` ` ``)
    /// before displaying in a diagnostic.
    pub fn as_diag_str(&self) -> &'static str {
        match self {
            Self::Literal(_) => "<float literal>",
            Self::Identifier(_) => "<identifier>",
            Self::Equals => "=",
            Self::Comma => ",",
            Self::ParenOpen => "(",
            Self::ParenClose => ")",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::LetKeyword => "let",
        }
    }
}

/// Simple whitespace-ignorant lexer.
///
/// Iterates over RME tokens produced from an ASCII `&str`(represented
/// internally as `&[u8]`). Optionally allows a "span offset" to be set which
/// is useful for REPLs where each line has its own [`Lexer`] but [`Span`]s
/// must be unique.
pub struct Lexer<'inp> {
    input: &'inp [u8],
    position: usize,
    span_offset: usize,
}

impl<'inp> Lexer<'inp> {
    /// Creates a new [`Lexer`] from the given input.
    ///
    /// Note that the input **must be ASCII**.
    ///
    /// `span_offset` can be used to ensure correct (unique) [`Span`]s, even
    /// when previously lexed (by a different instance) input exists.
    pub fn new(input: &'inp str, span_offset: usize) -> Self {
        // FIXME: use diagnostics instead of `assert!`
        assert!(input.is_ascii());
        Self {
            input: input.as_bytes(),
            position: 0,
            span_offset,
        }
    }

    /// Creates a new span, ending at the current position, respecting the
    /// `span_offset`.
    fn new_span(&self, start: usize) -> Span {
        Span::new(start + self.span_offset, self.position + self.span_offset)
    }

    /// Returns `Some(char)`, or `None` if we have exhausted the input.
    fn next_char(&mut self) -> Option<u8> {
        let ch = self.peek_char()?;
        self.position += 1;
        Some(ch)
    }

    /// Peeks a character from the input, or returns `None` if no more exist.
    fn peek_char(&self) -> Option<u8> {
        (self.position < self.input.len()).then(|| self.input[self.position])
    }

    /// Returns the next token from the input, advancing the input.
    ///
    /// - If the return value is `None`, then we have reached the end of the
    ///   input.
    /// - If the return value is `Some(Err)`, then some error was encountered
    ///   *while lexing a token* (e.g. float literal had 2 decimal points).
    fn next_token(&mut self) -> Option<Result<Token, DErr>> {
        // skip all whitespace
        let mut ch = self.next_char()?;
        while ch.is_ascii_whitespace() {
            ch = self.next_char()?;
        }

        // We need to save this for later. Note that `self.position` records
        // where we *will* read, not where we *just* read, hence the decrement.
        let span_start = self.position - 1;

        let kind = match ch {
            // single character "easy" tokens
            b'=' => TokenKind::Equals,
            b',' => TokenKind::Comma,
            b'(' => TokenKind::ParenOpen,
            b')' => TokenKind::ParenClose,
            b'+' => TokenKind::Plus,
            b'-' => TokenKind::Minus,
            b'*' => TokenKind::Star,
            b'/' => TokenKind::Slash,

            // identifiers (and keywords)
            ch if ch.is_ascii_alphabetic() => {
                let id = self.lex_complex(u8::is_ascii_alphabetic);
                match id {
                    "let" => TokenKind::LetKeyword,
                    _ => TokenKind::Identifier(id.to_owned()),
                }
            }

            // float literals (note that `.1` is allowed)
            ch if ch.is_ascii_digit() || ch == b'.' => {
                let literal = self.lex_complex(|&ch| ch.is_ascii_digit() || ch == b'.');

                if let Ok(num) = literal.parse::<f32>() {
                    TokenKind::Literal(num)
                } else {
                    return Some(Err(DErr::new_err(
                        "invalid float literal",
                        self.new_span(span_start),
                    )));
                }
            }

            // return an error if the character doesn't match any tokens
            ch => {
                return Some(Err(DErr::new_err(
                    format!("invalid start of token `{}`", ch as char),
                    self.new_span(span_start),
                )));
            }
        };

        // create a new span based on where we started lexing this token and
        // where we are now
        let span = self.new_span(span_start);
        Some(Ok(Token::new(kind, span)))
    }

    /// Implements a simple look-ahead loop for lexing multi-character tokens.
    ///
    /// Returns a `&str` taken from the input. The input is advanced to the
    /// end of this slice.
    fn lex_complex(&mut self, cont: for<'a> fn(&'a u8) -> bool) -> &str {
        let start_pos = self.position - 1;
        while let Some(ch) = self.peek_char() {
            // FIXME: use let chains instead
            if !cont(&ch) {
                break;
            }

            self.position += 1;
        }

        // SAFETY: It is a safety invariant of `Lexer` that its input is ASCII.
        unsafe { std::str::from_utf8_unchecked(&self.input[start_pos..self.position]) }
    }
}

/// Repeatedly calls `Lexer::next_token`
///
/// As per `next_token`:
/// - If the return value is `None`, then we have reached the end of the
///   input.
/// - If the return value is `Some(Err)`, then some error was encountered
///   *while lexing a token* (e.g. float literal had 2 decimal points).
impl Iterator for Lexer<'_> {
    type Item = Result<Token, DErr>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
