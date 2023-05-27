use std::{
    cmp,
    fmt::{self, Debug},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub const END_POS: usize = usize::MAX;
    pub const ALL: Self = Self::new(0, Self::END_POS);
    pub const EOF: Self = Self::new(Self::END_POS, Self::END_POS);

    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub const fn new_single(pos: usize) -> Self {
        Self {
            start: pos,
            end: pos + 1,
        }
    }

    pub const fn start(&self) -> usize {
        self.start
    }

    pub const fn end(&self) -> usize {
        self.end
    }

    pub const fn len(&self) -> usize {
        self.end - self.start
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn merge(s1: Self, s2: Self) -> Self {
        let start = cmp::min(s1.start, s2.start);
        let end = cmp::max(s1.end, s2.end);

        Self::new(start, end)
    }

    // FIXME: remove this; it's gross
    pub fn ensure_clamped(mut self, max_len: usize) -> Self {
        if self.start == Self::END_POS {
            self.start = max_len;
        }
        if self.end == Self::END_POS {
            self.end = max_len;
        }

        self
    }

    pub fn offset(self, distance: isize) -> Self {
        let start = if self.start == usize::MAX {
            usize::MAX
        } else {
            self.start.checked_add_signed(distance).unwrap()
        };

        let end = if self.end == usize::MAX {
            usize::MAX
        } else {
            self.end.checked_add_signed(distance).unwrap()
        };

        Self::new(start, end)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "Span({}..{})", self.start, self.end)
        } else {
            write!(f, "{}..{}", self.start, self.end)
        }
    }
}

#[derive(Debug, Default)]
pub struct SourceMap {
    input: String,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::from_input(String::new())
    }

    pub fn from_input(input: String) -> Self {
        Self { input }
    }

    pub fn push_line(&mut self, line: String) {
        self.input.push_str(&line);
    }

    pub fn get_span_lined(&self, span: Span) -> (usize, &str, Span) {
        let target = span.start();
        let mut pos = 0;

        for (line_num, line) in self.input.lines().enumerate() {
            let len = line.trim_end_matches('\n').len();
            if pos + len > target {
                return (line_num + 1, line, span.offset(-(pos as isize)));
            }
            pos += len + 1;
        }

        let (line_num, line) = self.input.lines().enumerate().last().unwrap();
        let offset = pos - line.len();
        (line_num + 1, line, span.offset(-(offset as isize)))
    }

    pub fn len(&self) -> usize {
        self.input.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
