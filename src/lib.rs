#![feature(if_let_guard)]
#![feature(let_chains)]

pub mod ast;
mod diag;
mod interpret;
mod lexer;
pub mod parser;
mod span;

use std::fmt::{self, Display};

pub use diag::*;
pub use interpret::*;
pub use lexer::*;
pub use span::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Sp<T>(T, Span);

impl<T> Sp<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self(val, span)
    }

    pub fn new_boxed(val: T, span: Span) -> SpBox<T> {
        Box::new(Sp(val, span))
    }

    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn map_inner<E>(self, map: impl FnOnce(T) -> E) -> Sp<E> {
        Sp::new(map(self.0), self.1)
    }

    pub fn span(&self) -> Span {
        self.1
    }

    pub fn into_parts(self) -> (T, Span) {
        (self.0, self.1)
    }

    pub fn as_parts(&self) -> (&T, Span) {
        (&self.0, self.1)
    }
}

impl<T: Display> Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner().fmt(f)
    }
}

// FIXME: we probably want this the other way round; `Sp<Box<T>>`
pub(crate) type SpBox<T> = Box<Sp<T>>;
