pub mod ast;
mod diag;
mod interpret;
mod lexer;
pub mod parser;
mod span;

use std::{
    fmt::{self, Display},
    ops::Deref,
};

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
        Sp(Box::new(val), span)
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
}

impl<T> Deref for Sp<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Display> Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

pub(crate) type SpBox<T> = Sp<Box<T>>;

impl<T> SpBox<T> {
    pub fn unbox(self) -> Sp<T> {
        Sp::new(*self.0, self.1)
    }
}
