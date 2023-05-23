use std::cmp;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

    pub fn merge(s1: Self, s2: Self) -> Self {
        let start = cmp::min(s1.start, s2.start);
        let end = cmp::max(s1.end, s2.end);

        Self::new(start, end)
    }

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

pub struct SourceMap {
    lines: Vec<String>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self { lines: Vec::new() }
    }

    pub fn push_line(&mut self, line: String) {
        self.lines.push(line);
    }

    pub fn get_span_lined(&self, span: Span) -> (&str, Span) {
        let target = span.start();
        let mut pos = 0;

        for line in &self.lines {
            if pos + line.len() > target {
                return (line.as_str(), span.offset(-(pos as isize)));
            }
            pos += line.len();
        }

        let line = self.lines.last().unwrap();
        let offset = pos - line.len();
        (line, span.offset(-(offset as isize)))
    }

    pub fn len(&self) -> usize {
        self.lines.iter().fold(0usize, |len, line| len + line.len())
    }
}
