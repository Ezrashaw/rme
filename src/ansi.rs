#![allow(unused)]

use std::fmt;

pub struct Style;

impl Style {
    pub const fn reset() -> Self {
        Self
    }

    pub const fn fg(colour: Colour) -> WriteStyle {
        WriteStyle::empty().fg(colour)
    }

    pub const fn bg(colour: Colour) -> WriteStyle {
        WriteStyle::empty().bg(colour)
    }

    pub const fn bold() -> WriteStyle {
        WriteStyle::empty().bold()
    }

    pub const fn italic() -> WriteStyle {
        WriteStyle::empty().italic()
    }
}

impl fmt::Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1B[0m")
    }
}

pub struct WriteStyle {
    fg: Option<Colour>,
    bg: Option<Colour>,
    bold: bool,
    italic: bool,
}

impl WriteStyle {
    const fn empty() -> Self {
        Self {
            fg: None,
            bg: None,
            bold: false,
            italic: false,
        }
    }

    pub const fn fg(mut self, colour: Colour) -> Self {
        assert!(self.fg.is_none());
        self.fg = Some(colour);

        self
    }

    pub const fn bg(mut self, colour: Colour) -> Self {
        assert!(self.bg.is_none());
        self.bg = Some(colour);

        self
    }

    pub const fn bold(mut self) -> Self {
        assert!(!self.bold);
        self.bold = true;

        self
    }

    pub const fn italic(mut self) -> Self {
        assert!(!self.italic);
        self.italic = true;

        self
    }
}

impl fmt::Display for WriteStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1B[")?;

        let mut needs_semi = false;
        let mut maybe_semi = |f: &mut fmt::Formatter<'_>| {
            if needs_semi {
                write!(f, ";")?;
            }

            needs_semi = true;
            Ok(())
        };

        if let Some(fg) = self.fg {
            maybe_semi(f)?;
            write!(f, "{}", fg.fg_code())?;
        }

        if let Some(bg) = self.bg {
            maybe_semi(f)?;
            write!(f, "{}", bg.bg_code())?;
        }

        if self.bold {
            maybe_semi(f)?;
            write!(f, "1")?;
        }

        if self.italic {
            maybe_semi(f)?;
            write!(f, "3")?;
        }

        write!(f, "m")
    }
}

#[derive(Clone, Copy)]
pub enum Colour {
    Cyan,
    Magenta,
    Yellow,
    BrightBlack,
    BrightMagenta,
    BrightCyan,
}

impl Colour {
    const fn fg_code(self) -> u8 {
        match self {
            Colour::Yellow => 33,
            Colour::Magenta => 35,
            Colour::Cyan => 36,
            Colour::BrightBlack => 90,
            Colour::BrightMagenta => 95,
            Colour::BrightCyan => 96,
        }
    }

    const fn bg_code(self) -> u8 {
        self.fg_code() + 10
    }
}
