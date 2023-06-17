use std::fmt::Debug;

use crate::ansi::{Colour, Style, WriteStyle};

pub trait DiagnosticLevel: Debug {
    fn name(&self) -> &'static str;
    fn style(&self) -> WriteStyle;
}

#[derive(Debug, Clone, Copy)]
pub struct ErrorLevel;

impl DiagnosticLevel for ErrorLevel {
    fn name(&self) -> &'static str {
        "error"
    }

    fn style(&self) -> WriteStyle {
        Style::fg(Colour::Red)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SubDiagLevel {
    Info,
    Help,
    Note,
}

impl DiagnosticLevel for SubDiagLevel {
    fn name(&self) -> &'static str {
        match self {
            Self::Info => "info",
            Self::Help => "help",
            Self::Note => "note",
        }
    }

    fn style(&self) -> WriteStyle {
        match self {
            Self::Info => Style::fg(Colour::Cyan),
            Self::Help => Style::fg(Colour::BrightMagenta),
            Self::Note => Style::fg(Colour::BrightGreen),
        }
    }
}
