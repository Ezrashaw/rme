use std::fmt::Debug;

pub trait DiagnosticLevel: Debug {
    fn name(&self) -> &'static str;
    fn ansi_color_code(&self) -> &'static str;
}

#[derive(Debug, Clone, Copy)]
pub struct ErrorLevel;

impl DiagnosticLevel for ErrorLevel {
    fn name(&self) -> &'static str {
        "error"
    }

    fn ansi_color_code(&self) -> &'static str {
        "31"
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SubDiagLevel {
    Info,
    Help,
}

impl DiagnosticLevel for SubDiagLevel {
    fn name(&self) -> &'static str {
        match self {
            Self::Info => "info",
            Self::Help => "help",
        }
    }

    fn ansi_color_code(&self) -> &'static str {
        match self {
            Self::Info => "36",
            Self::Help => "92",
        }
    }
}
