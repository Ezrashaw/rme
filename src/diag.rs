use std::{
    fmt::{Debug, Display},
    io::{self, stdout, Write},
    iter,
};

use crate::{SourceMap, Span};

pub use level::{ErrorLevel, SubDiagLevel};

use self::level::DiagnosticLevel;
mod level;

pub type DErr = Diag<ErrorLevel>;
pub type SubDiag = Diag<SubDiagLevel>;

#[derive(Debug)]
pub struct Diag<L: DiagnosticLevel> {
    level: L,
    msg: String,
    span: Option<Span>,
    sub_diags: Vec<Diag<SubDiagLevel>>,

    emitted: bool,
}

impl<L: DiagnosticLevel> Drop for Diag<L> {
    fn drop(&mut self) {
        assert!(
            self.emitted,
            "dropped `Diag` {:?}",
            (self.level.name(), &self.msg, self.span)
        );
    }
}

impl Diag<ErrorLevel> {
    pub fn new_err(msg: impl Display, span: Span) -> Self {
        Self::new(ErrorLevel, msg, span)
    }
}

impl<L: DiagnosticLevel> Diag<L> {
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

    pub fn emit(self, source_map: &SourceMap) {
        self.emit_to_write(&mut stdout(), source_map);
    }

    pub fn emit_to_write(mut self, write: &mut impl Write, source_map: &SourceMap) {
        self.emit_(write, source_map, 0).unwrap();

        self.emitted = true;
        drop(self);
    }

    fn emit_(&self, w: &mut impl Write, source_map: &SourceMap, indent: usize) -> io::Result<()> {
        let mut arrows = iter::once('>').take(indent * 2).collect::<String>();
        if !arrows.is_empty() {
            arrows.push(' ');
        }

        writeln!(
            w,
            "{arrows}\x1B[1;3;{}m{}\x1B[0m: {}",
            self.level.ansi_color_code(),
            self.level.name(),
            self.msg
        )?;

        if let Some(span) = self.span {
            let (line, mut span) = source_map.get_span_lined(span);
            span = span.ensure_clamped(line.len() - 1);

            if span.is_empty() {
                span = Span::new_single(span.start());
            }

            writeln!(w, "{arrows}")?;
            writeln!(w, "{arrows}    {}", line.trim_end_matches('\n'))?;
            write!(w, "{arrows}    {:width$}", "", width = span.start())?;
            writeln!(w, "\x1B[1;96m{:^^width$}\x1B[0m", "", width = span.len())?;
        }

        for sub_diag in &self.sub_diags {
            sub_diag.emit_(w, source_map, indent + 1)?;
        }

        Ok(())
    }
}
