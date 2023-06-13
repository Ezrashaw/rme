#![feature(if_let_guard)]
#![feature(let_chains)]
// #![warn(missing_docs)]
// #![allow(clippy::must_use_candidate)]
// #![allow(clippy::missing_panics_doc)]

mod ansi;
pub mod ast;
mod diag;
mod interpret;
pub mod lexer;
mod parser;
mod span;
pub mod typeck;

pub use diag::*;
pub use interpret::*;
pub use parser::*;
pub use span::*;

pub mod token {
    // Re-export the `token` module from `rme::lexer`. This moves
    // `rme::lexer::token` to `rme::token`.
    pub use crate::lexer::token::*;
}

pub mod ty {
    // Re-export the `ty` module from `rme::typeck`. This moves
    // `rme::typeck::ty` to `rme::ty`.
    pub use crate::typeck::ty::*;
}
