use std::{
    fmt::{Debug, Display},
    io::{self, stdout, Write},
};

use crate::{
    ansi::{Colour, Style, WriteStyle},
    SourceMap, Span,
};

pub use level::{DiagnosticLevel, ErrorLevel, SubDiagLevel};

mod level;

pub type DErr = Diag<ErrorLevel>;
pub type SubDiag = Diag<SubDiagLevel>;

#[derive(Debug)]
pub struct Diag<L: DiagnosticLevel> {
    level: L,
    msg: String,
    span: Option<Span>,
    span_tag: Option<String>,
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
    pub fn error(msg: impl Display, span: Span) -> Self {
        Self::new(ErrorLevel, msg, Some(span))
    }
}

impl Diag<SubDiagLevel> {
    pub fn sub_diag(level: SubDiagLevel, msg: impl Display, span: Option<Span>) -> Self {
        Self::new(level, msg, span)
    }
}

impl<L: DiagnosticLevel> Diag<L> {
    fn new(level: L, msg: impl Display, span: Option<Span>) -> Self {
        Self {
            level,
            msg: msg.to_string(),
            span,
            span_tag: None,
            emitted: false,
            sub_diags: Vec::new(),
        }
    }

    pub fn add_subdiag(&mut self, mut sub_diag: SubDiag) {
        sub_diag.emitted = true;
        self.sub_diags.push(sub_diag);
    }

    pub fn span_tag(&mut self, tag: impl Display) {
        assert!(self.span.is_some() && self.span_tag.is_none());
        self.span_tag = Some(tag.to_string());
    }

    pub fn emit(self, source_map: &SourceMap) {
        self.emit_to_write(&mut stdout(), source_map).unwrap();
    }

    pub fn emit_to_write(
        mut self,
        write: &mut impl Write,
        source_map: &SourceMap,
    ) -> io::Result<()> {
        self.emit_(write, source_map, None)?;

        self.emitted = true;
        drop(self);

        Ok(())
    }

    fn emit_(
        &self,
        w: &mut impl Write,
        source_map: &SourceMap,
        subd_indent: Option<usize>,
    ) -> io::Result<()> {
        if let Some(indent) = subd_indent {
            write!(w, "{:indent$}= ", "")?;
        }

        writeln!(
            w,
            "{}{}{}: {}",
            self.level.style().bold().italic(),
            self.level.name(),
            Style::reset(),
            self.msg
        )?;

        let mut subd_indent = subd_indent;

        if let Some(span) = self.span {
            let (line_num, line, span) = source_map.get_span_lined(span);
            let line_num = line_num.to_string();
            let margin = Some(line_num.len());

            let mut span = Span::new(
                span.start().clamp(0, line.len().try_into().unwrap()),
                span.end().clamp(0, line.len().try_into().unwrap()),
            );

            if span.is_empty() {
                span = Span::new_single(span.start());
            }

            write_line_start(w, margin, None)?;
            writeln!(w)?;

            write_line_start(w, margin, Some(&line_num))?;
            writeln!(w, "{}", line.trim_end_matches('\n'))?;

            write_line_start(w, margin, None)?;
            write!(w, "{0:width$}", "", width = span.start() as usize)?;

            write!(
                w,
                "{SPAN_TAG}{:^^width$}{RESET}",
                "",
                width = span.len() as usize
            )?;
            if let Some(span_tag) = &self.span_tag {
                writeln!(w, " {SPAN_TAG}{span_tag}{RESET}")?;
                write_line_start(w, margin, None)?;
            }
            writeln!(w)?;

            subd_indent = Some(line_num.len() + 1);
        }

        for sub_diag in &self.sub_diags {
            sub_diag.emit_(w, source_map, subd_indent)?;
        }

        Ok(())
    }
}

const RESET: Style = Style::reset();
const SPAN_TAG: WriteStyle = Style::fg(Colour::BrightCyan).bold();

fn write_line_start(
    w: &mut impl Write,
    margin: Option<usize>,
    line_num: Option<&str>,
) -> io::Result<()> {
    let margin = margin.unwrap_or_default();
    let line_num = line_num.unwrap_or_default();
    write!(
        w,
        "{}{line_num:margin$} |{RESET} ",
        Style::fg(Colour::Colour256(244)).bold()
    )
}
