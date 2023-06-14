#![allow(unused)]

mod infer;
mod polytype;
pub(crate) mod ty;
mod unify;
pub mod utils;

pub use infer::{infer, TypeEnv};
pub use unify::Subst;
