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
            fg.write_fg(f)?;
        }

        if let Some(bg) = self.bg {
            maybe_semi(f)?;
            bg.write_bg(f)?;
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
#[repr(u8)]
pub enum Colour {
    Colour256(u8),

    Red = 31,
    Yellow = 33,
    Magenta = 35,
    Cyan = 36,
    BrightBlack = 90,
    BrightGreen = 92,
    BrightMagenta = 95,
    BrightCyan = 96,
}

impl Colour {
    fn write_fg(self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code = match self {
            Self::Colour256(code) => return write!(f, "38;5;{code}"),
            _ => self.discriminant(),
        };

        write!(f, "{code}")
    }

    fn write_bg(self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code = match self {
            Self::Colour256(code) => return write!(f, "48;5;{code}"),
            _ => self.discriminant() + 10,
        };

        write!(f, "{code}")
    }

    // taken from `https://doc.rust-lang.org/std/mem/fn.discriminant.html`
    fn discriminant(&self) -> u8 {
        // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a
        //         `repr(C)` `union` between `repr(C)` structs, each of which
        //         has the `u8` discriminant as its first field, so we can read
        //         the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }
}
