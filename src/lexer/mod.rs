#![deny(clippy::all)]
#![deny(clippy::pedantic)]
#![deny(clippy::nursery)]
#![deny(missing_docs)]

//! Utilities for lexing RME code.
//!
//! The public members of this module are the [`lex`] function and the
//! [`Lexer`] structure. In general, the [`lex`] function should be used for
//! the general case, whereas the [`Lexer`] structure should be used where
//! precise control is needed. See the documentation on both members for more
//! information.

use std::str::FromStr;

use crate::{
    diag::{DErr, Diag, SubDiagLevel},
    token::{Keyword, Literal, Token, TokenKind},
    Span,
};

pub(crate) mod token;

/// Fully lexes the provided input, using the [`Lexer`]
/// structure.
///
/// Note that you *must* iterate the returned [`Vec`], and handle every
/// [`Diag`] emitted.
///
/// # Examples
///
/// ## In a `for` loop:
/// ```
/// # use rme::{token::TokenKind, Span};
/// let tokens = rme::lexer::lex("+++");
/// for token in tokens {
///     assert_eq!(token?.into_parts().0, TokenKind::Plus);
/// }
/// # Ok::<(), rme::diag::DErr>(())
/// ```
///
/// ## Directly with [`Vec`]
/// ```
/// # use rme::{token::Token, Span};
/// let mut tokens = rme::lexer::lex(";");
/// let span = tokens.pop().unwrap()?.span();
/// assert_eq!(span, Span::new(0, 1));
/// # Ok::<(), rme::diag::DErr>(())
/// ```
///
/// ## Incorrectly handling [`Diag`]s
/// ```should_panic
/// // we *must* handle each error, dropping isn't OK
/// let tokens = rme::lexer::lex("~");
/// drop(tokens); // PANIC: we dropped a `Diag` without emitting it
/// # Ok::<(), rme::diag::DErr>(())
/// ```
///
/// ## Incorrectly handling *all* [`Diag`]s
/// ```should_panic
/// # use rme::{diag::DErr, token::Token, SourceMap};
/// let input = "~~";
///
/// // PANIC: collecting `Result`s like this drops any other errors, invoking
/// //        `Diag`'s drop guard.
/// let tokens = rme::lexer::lex(input)
///     .into_iter()
///     .collect::<Result<Vec<Token>, DErr>>();
/// match tokens {
///     Ok(tokens) => { /* ... */ },
///
///     // this is all good, right?
///     Err(err) => err.emit(&SourceMap::from_input(input.to_owned())),
/// }
/// # Ok::<(), rme::diag::DErr>(())
/// ```
// FIXME: maybe this should return an iterator instead?
#[must_use]
pub fn lex(input: &str) -> Vec<Result<Token, DErr>> {
    Lexer::new(input).collect()
}

// allowing 16-bit platforms would break `Lexer` because it measures positions
// with `u32`; a 16-bit pointer would allow positions that are longer than any
// possible input
#[cfg(target_pointer_width = "16")]
compile_error!("RME does not support 16-bit platforms");

/// Simple whitespace-ignorant lexer.
///
/// Iterates over [`Token`]s produced from an ASCII `&str`.
pub struct Lexer<'inp> {
    input: &'inp [u8],
    position: u32,
}

impl<'inp> Lexer<'inp> {
    /// Creates a new [`Lexer`] from the given input.
    ///
    /// # Panics
    /// This function panics if `input` is longer than `u32::MAX` bytes.
    #[must_use]
    pub const fn new(input: &'inp str) -> Self {
        assert!(input.len() < u32::MAX as usize);
        Self {
            input: input.as_bytes(),
            position: 0,
        }
    }

    /// Creates a new span, ending at the current position.
    const fn new_span(&self, from: u32) -> Span {
        Span::new(from, self.position)
    }

    /// Returns `Some(char)`, or `None` if we have exhausted the input.
    fn next(&mut self) -> Option<u8> {
        let ch = self.peek()?;
        self.position += 1;
        Some(ch)
    }

    /// Peeks a character from the input, or returns `None` if no more exist.
    fn peek(&self) -> Option<u8> {
        // SAFETY: It is a safety variant of this type that `input` is less
        //         than `u32::MAX` bytes.
        let len = unsafe { u32::try_from(self.input.len()).unwrap_unchecked() };
        (self.position < len).then(|| self.input[self.position as usize])
    }

    /// Returns the next token from the input, advancing the input.
    ///
    /// - If the return value is `None`, then we have reached the end of the
    ///   input.
    /// - If the return value is `Some(Err)`, then some error was encountered
    ///   *while lexing a token* (e.g. float literal had 2 decimal points).
    fn next_token(&mut self) -> Option<Result<Token, DErr>> {
        let mut ch = self.next()?;

        // skip all whitespace
        while ch.is_ascii_whitespace() {
            ch = self.next()?;
        }

        // We need to save this for later. Note that `self.position` records
        // where we *will* read, not where we *just* read, hence the decrement.
        let span_start = self.position - 1;

        let kind = match ch {
            // multi-character tokens
            // Note how these are before their single-character partners, so
            // that single characters aren't lexed when a multi exists.
            _ if self.lex_multi_char(b"==") => TokenKind::DoubleEquals,

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
            b'<' => TokenKind::LeftArrow,
            b'>' => TokenKind::RightArrow,

            // identifiers (and keywords/booleans)
            ch if ch.is_ascii_alphabetic() || ch == b'_' => {
                // SAFETY: the provided closure only triggers for ASCII values
                let id = unsafe {
                    self.lex_complex(|&ch| {
                        ch.is_ascii_alphabetic() || ch.is_ascii_digit() || ch == b'_'
                    })
                };

                match id {
                    // boolean literals aren't officially keywords, but you
                    // still can't get an identifier with them
                    "true" => TokenKind::Literal(Literal::Bool(true)),
                    "false" => TokenKind::Literal(Literal::Bool(false)),

                    // check if the identifier is a keyword, and return a
                    // lightweight `Keyword`
                    _ if let Ok(kw) = Keyword::from_str(id) => TokenKind::Keyword(kw),

                    // otherwise, return a normal identifier
                    _ => TokenKind::Identifier(id.to_owned()),
                }
            }

            // float literals (note that `.1` is allowed)
            ch if ch.is_ascii_digit() || ch == b'.' => {
                // SAFETY: the provided closure only triggers for ASCII values
                let literal = unsafe { self.lex_complex(|&ch| ch.is_ascii_digit() || ch == b'.') };

                if let Ok(num) = literal.parse::<f32>() {
                    TokenKind::Literal(Literal::Float(num))
                } else {
                    return Some(Err(Diag::error(
                        "invalid float literal",
                        self.new_span(span_start),
                    )));
                }
            }

            // if the character isn't ASCII, then emit a distinct error
            ch if !ch.is_ascii() => {
                let mut err = Diag::error("input is not ASCII", self.new_span(span_start));
                err.span_tag("this character is invalid in RME");
                // This is a bit of an understatement, *all* lexed spans after
                // here might break. I'm not too concerned about this though.
                err.add_subdiag(Diag::sub_diag(
                    SubDiagLevel::Note,
                    "the provided span may not be correct",
                    None,
                ));

                // Every (non-ASCII) UTF-8 character begins with a number of
                // true-bits corresponding to the number of bytes in the
                // multi-byte character. We've already read one character, so
                // skip the rest to avoid emitting duplicate errors. See also
                // the [Wikipedia page](https://en.wikipedia.org/wiki/UTF-8).
                self.position += ch.leading_ones() - 1;

                return Some(Err(err));
            }

            // return an error if the character doesn't match the start of any
            // tokens
            ch => {
                //
                return Some(Err(Diag::error(
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
    ///
    /// # Safety
    ///
    /// The `cont` parameter may not return true for non-ASCII values.
    unsafe fn lex_complex(&mut self, cont: for<'a> fn(&'a u8) -> bool) -> &str {
        let start_pos = (self.position - 1) as usize;
        while let Some(ch) = self.peek()
            && cont(&ch)
        {
            // it is a safety requirement of this function that this is true,
            // no harm in checking though
            debug_assert!(self.peek().unwrap().is_ascii());

            self.position += 1;
        }

        // SAFETY: This only corresponds to a range which has been accepted by
        //         `cont`. The `cont` function is required to uphold this
        //         functions invariant (must be ASCII).
        unsafe { std::str::from_utf8_unchecked(&self.input[start_pos..self.position as usize]) }
    }

    /// Checks if the given multi-character token exists at the current
    /// position.
    ///
    /// If so, the input is advanced passed the token and `true` is returned,
    /// otherwise, `false` is returned and the position is not advanced.
    fn lex_multi_char(&mut self, multi_char: &[u8]) -> bool {
        // we go backwards to offset the character that we have already read in
        // `next_token`.
        let pos = (self.position - 1) as usize;
        let range = pos..(pos + multi_char.len());

        if let Some(tok) = &self.input.get(range)
            && multi_char == *tok
        {
            // we decrement `multi_char`'s length because we are already one
            // character past, see the comment on `pos` above
            // SAFETY: This is allowed because all `multi_char`s are less than
            // `u32::MAX`. Technically this function should be `unsafe`, but
            // I'm not too worried about that.
            self.position += unsafe { u32::try_from(multi_char.len() - 1).unwrap_unchecked() };
            true
        } else {
            false
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, DErr>;

    /// Repeatedly calls `Lexer::next_token` (note this is private).
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
