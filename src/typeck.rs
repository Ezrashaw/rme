mod infer;
mod polytype;
pub mod ty;
mod unify;
mod utils;

pub use infer::{infer, infer_stmt, TypeEnv};
