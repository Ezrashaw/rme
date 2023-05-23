use std::fmt::Display;

use crate::{SourceMap, Span};

#[derive(Debug, Clone, Copy)]
pub enum DiagLevel {
    Error,
    Warn,
    Info,
}

impl Display for DiagLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1B[3;1;")?;
        match self {
            Self::Error => write!(f, "31merror")?,
            Self::Warn => write!(f, "33mwarning")?,
            Self::Info => write!(f, "36minfo")?,
        }

        write!(f, "\x1B[0m")
    }
}

pub struct DiagEmitter<'a> {
    source: &'a SourceMap,
}

impl<'a> DiagEmitter<'a> {
    pub fn new(source: &'a SourceMap) -> Self {
        Self { source }
    }

    pub fn create_diag(&self, level: DiagLevel, msg: impl Display, span: Span) -> Diag {
        Diag {
            level,
            msg: msg.to_string(),
            span,
            emitted: false,
        }
    }

    pub fn create_err(&self, msg: impl Display, span: Span) -> Diag {
        self.create_diag(DiagLevel::Error, msg, span)
    }
}

#[derive(Debug)]
pub struct Diag {
    level: DiagLevel,
    msg: String,
    span: Span,

    emitted: bool,
}

impl Drop for Diag {
    fn drop(&mut self) {
        if !self.emitted {
            panic!("dropped `Diag` {self:?}")
        }
    }
}

impl Diag {
    pub fn emit(self, source_map: &SourceMap) {
        let (line, mut span) = source_map.get_span_lined(self.span);
        span = span.ensure_clamped(line.len() - 1);

        if span.len() == 0 {
            span = Span::new_single(span.start());
        }

        println!("{}: {}", self.level, self.msg);
        print!("\n    {line}");
        print!("    {:width$}", "", width = span.start());
        println!("\x1B[1;96m{:^^width$}\x1B[0m", "", width = span.len());

        self.emitted();
    }

    fn emitted(mut self) {
        self.emitted = true;
        drop(self)
    }
}

// pub trait ToDiag<T> {
//     fn to_diag<'em, 'msg>(
//         self,
//         emitter: &'em DiagEmitter,
//         level: DiagLevel,
//         msg: &'msg str,
//         span: Span,
//     ) -> Result<T, Diag<'em, 'msg>>;
// }

// impl<T> ToDiag<T> for Option<T> {
//     fn to_diag<'em, 'msg>(
//         self,
//         emitter: &'em DiagEmitter,
//         level: DiagLevel,
//         msg: &'msg str,
//         span: Span,
//     ) -> Result<T, Diag<'em, 'msg>> {
//         self.ok_or(emitter.create_diag(level, msg, span))
//     }
// }
