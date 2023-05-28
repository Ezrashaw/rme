use std::str::FromStr;

use crate::{DErr, Diag, Span, SubDiag, SubDiagLevel};

mod token;
pub use token::*;

/// Fully lexes the provided input (with a span offset), using the [`Lexer`]
/// structure.
/// 
/// Note that you *must* iterate the returned [`Vec`], and handle every
/// [`Diag`] emitted.
///
/// # Examples
/// 
/// ## In a `for` loop:
/// ```
/// # use rme::{TokenKind, Span};
/// let tokens = rme::lex("+++", 0);
/// for token in tokens {
///     assert_eq!(token?.into_parts().0, TokenKind::Plus);
/// }
/// # Ok::<(), rme::DErr>(())
/// ```
/// 
/// ## Directly with [`Vec`]
/// ```
/// # use rme::{Token, Span};
/// let mut tokens = rme::lex(";", 10);
/// let span = tokens.pop().unwrap()?.span();
/// assert_eq!(span, Span::new(10, 11));
/// # Ok::<(), rme::DErr>(())
/// ```
/// 
/// ## Incorrectly handling [`Diag`]s
/// ```should_panic
/// // we *must* handle each error, dropping isn't OK
/// let tokens = rme::lex("~", 0);
/// drop(tokens); // PANIC: we dropped a `Diag` without emitting it
/// # Ok::<(), rme::DErr>(())
/// ```
/// 
/// ## Incorrectly handling *all* [`Diag`]s
/// ```should_panic
/// # use rme::{DErr, Token, SourceMap};
/// let input = "~~";
/// 
/// // PANIC: collecting `Result`s like this drops any other errors, invoking
/// //        `Diag`'s drop guard.
/// let tokens = rme::lex(input, 0).into_iter().collect::<Result<Vec<Token>, DErr>>();
/// match tokens {
///     Ok(tokens) => { /* ... */ },
///
///     // this is all good, right?
///     Err(err) => err.emit(&SourceMap::from_input(input.to_owned())),
/// }
/// # Ok::<(), rme::DErr>(())
/// ```
#[must_use]
pub fn lex(input: &str, span_offset: usize) -> Vec<Result<Token, DErr>> {
    let lexer = Lexer::new(input, span_offset);
    lexer.collect()
}

/// Simple whitespace-ignorant lexer.
///
/// Iterates over [`Token`]s produced from an ASCII `&str`. Optionally allows a
/// "span offset" to be set which is useful for the REPL where each line has
/// its own [`Lexer`] but [`Span`]s must be unique.
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
    #[must_use]
    pub fn new(input: &'inp str, span_offset: usize) -> Self {
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
            b';' => TokenKind::Semi,
            b'(' => TokenKind::ParenOpen,
            b')' => TokenKind::ParenClose,
            b'+' => TokenKind::Plus,
            b'-' => TokenKind::Minus,
            b'*' => TokenKind::Star,
            b'/' => TokenKind::Slash,
            b'!' => TokenKind::Bang,

            // identifiers (and keywords/booleans)
            ch if ch.is_ascii_alphabetic() || ch == b'_' => {
                let id = self.lex_complex(|ch| {
                    ch.is_ascii_alphabetic() || ch.is_ascii_digit() || *ch == b'_'
                });

                match id {
                    // boolean literals aren't officially keywords, but you
                    // can't get an identifier with them 
                    "true" => TokenKind::Literal(Literal::Bool(true)),
                    "false" => TokenKind::Literal(Literal::Bool(false)),
                    
                    // check if the identifier is a keyword, and return a
                    // lightweight `Keyword`
                    _ if let Ok(kw) = Keyword::from_str(id) => {
                        TokenKind::Keyword(kw)
                    }

                    // otherwise, return a normal identifier
                    _ => TokenKind::Identifier(id.to_owned())
                }
            }

            // float literals (note that `.1` is allowed)
            ch if ch.is_ascii_digit() || ch == b'.' => {
                let literal = self.lex_complex(|&ch| ch.is_ascii_digit() || ch == b'.');

                if let Ok(num) = literal.parse::<f32>() {
                    TokenKind::Literal(Literal::Float(num))
                } else {
                    return Some(Err(DErr::new_err(
                        "invalid float literal",
                        self.new_span(span_start),
                    )));
                }
            }

            // if the character isn't ASCII, then emit a distinct error
            ch if !ch.is_ascii() => {
                let mut err = Diag::new_err("input is not ASCII", self.new_span(span_start));
                err.span_tag("this character is invalid in RME");
                // This is a bit of an understatement, *all* lexed spans after
                // here might break. I'm not too concerned about this though.
                err.add_subdiag(SubDiag::without_span(
                    SubDiagLevel::Note,
                    "the provided span may not be correct",
                ));

                // Every (non-ASCII) UTF-8 character begins with a number of
                // true-bits corresponding to the number of bytes in the
                // multi-byte character. We've already read one character, so
                // skip the rest to avoid emitting duplicate errors. See also
                // the [Wikipedia page](https://en.wikipedia.org/wiki/UTF-8).
                self.position += (ch.leading_ones() - 1) as usize;

                return Some(Err(err));
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
        while let Some(ch) = self.peek_char() && cont(&ch) {
            self.position += 1;
        }

        // SAFETY: It is a safety invariant of `Lexer` that its input is ASCII.
        unsafe { std::str::from_utf8_unchecked(&self.input[start_pos..self.position]) }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, DErr>;

    /// Repeatedly calls `Lexer::next_token`
    ///
    /// As per `next_token`:
    /// - If the return value is `None`, then we have reached the end of the
    ///   input.
    /// - If the return value is `Some(Err)`, then some error was encountered
    ///   *while lexing a token* (e.g. float literal had 2 decimal points).
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
