#![feature(if_let_guard)]
#![feature(let_chains)]
// #![warn(missing_docs)]
// #![allow(clippy::must_use_candidate)]
// #![allow(clippy::missing_panics_doc)]

pub mod ast;
mod diag;
mod interpret;
mod lexer;
mod parser;
mod span;

pub use diag::*;
pub use interpret::*;
pub use lexer::*;
pub use parser::*;
pub use span::*;
