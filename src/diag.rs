use std::{
    fmt::{Debug, Display},
    iter,
};

use crate::{SourceMap, Span};

use self::level::*;

pub use level::{ErrorLevel, SubDiagLevel};
mod level;

pub type DErr = DiagInner<ErrorLevel>;
pub type SubDiag = DiagInner<SubDiagLevel>;

#[derive(Debug)]
pub struct DiagInner<L: DiagnosticLevel> {
    level: L,
    msg: String,
    span: Option<Span>,
    sub_diags: Vec<DiagInner<SubDiagLevel>>,

    emitted: bool,
}

impl<L: DiagnosticLevel> Drop for DiagInner<L> {
    fn drop(&mut self) {
        if !self.emitted {
            panic!(
                "dropped `Diag` {:?}",
                (self.level.name(), &self.msg, self.span)
            );
        }
    }
}

impl DiagInner<ErrorLevel> {
    pub fn new_err(msg: impl Display, span: Span) -> Self {
        Self::new(ErrorLevel, msg, span)
    }
}

impl<L: DiagnosticLevel> DiagInner<L> {
    pub fn new(level: L, msg: impl Display, span: Span) -> Self {
        Self {
            level,
            msg: msg.to_string(),
            span: Some(span),
            emitted: false,
            sub_diags: Vec::new(),
        }
    }

    pub fn without_span(level: L, msg: impl Display) -> Self {
        Self {
            level,
            msg: msg.to_string(),
            span: None,
            emitted: false,
            sub_diags: Vec::new(),
        }
    }

    pub fn add_subdiag(&mut self, mut sub_diag: SubDiag) {
        sub_diag.emitted = true;
        self.sub_diags.push(sub_diag);
    }

    pub fn emit(mut self, source_map: &SourceMap) {
        self.emit_(source_map, 0);

        self.emitted = true;
        drop(self)
    }

    fn emit_(&self, source_map: &SourceMap, indent: usize) {
        let mut arrows = String::from_iter(iter::once('>').take(indent * 2));
        if !arrows.is_empty() {
            arrows.push(' ');
        }

        println!(
            "{arrows}\x1B[1;3;{}m{}\x1B[0m: {}",
            self.level.ansi_color_code(),
            self.level.name(),
            self.msg
        );

        if let Some(span) = self.span {
            let (line, mut span) = source_map.get_span_lined(span);
            span = span.ensure_clamped(line.len() - 1);

            if span.len() == 0 {
                span = Span::new_single(span.start());
            }

            print!("{arrows}\n{arrows}    {line}");
            print!("{arrows}    {:width$}", "", width = span.start());
            println!("\x1B[1;96m{:^^width$}\x1B[0m", "", width = span.len());
        }

        for sub_diag in &self.sub_diags {
            sub_diag.emit_(source_map, indent + 1);
        }
    }
}
