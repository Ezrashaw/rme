use std::{cmp, fmt};

// FIXME: this should be shrunk to two 32-bit integers (we get register opt
//        then)
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

    #[deprecated]
    pub fn merge(s1: Self, s2: Self) -> Self {
        let start = cmp::min(s1.start, s2.start);
        let end = cmp::max(s1.end, s2.end);

        Self::new(start, end)
    }

    pub const fn to(self, other: Self) -> Self {
        Self::new(self.start, other.end)
    }

    pub const fn offset(self, distance: isize) -> Self {
        let start = self.start.saturating_add_signed(distance);
        let end = self.end.saturating_add_signed(distance);

        Self::new(start, end)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, Default)]
pub struct SourceMap {
    input: String,
}

impl SourceMap {
    pub const fn new() -> Self {
        Self::from_input(String::new())
    }

    pub const fn from_input(input: String) -> Self {
        Self { input }
    }

    pub fn push_line(&mut self, line: &str) {
        self.input.push_str(line);
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

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Sp<T>(T, Span);

impl<T> Sp<T> {
    pub const fn new(val: T, span: Span) -> Self {
        Self(val, span)
    }

    pub const fn as_ref(&self) -> Sp<&T> {
        Sp::new(self.inner(), self.span())
    }

    pub fn map_inner<E>(self, map: impl FnOnce(T) -> E) -> Sp<E> {
        Sp::new(map(self.0), self.1)
    }

    pub const fn inner(&self) -> &T {
        &self.0
    }

    pub const fn span(&self) -> Span {
        self.1
    }

    pub fn into_parts(self) -> (T, Span) {
        (self.0, self.1)
    }

    pub const fn as_parts(&self) -> (&T, Span) {
        (&self.0, self.1)
    }
}

impl<T: fmt::Display> fmt::Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner().fmt(f)
    }
}

impl<T: fmt::Debug> fmt::Debug for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.inner(), self.span())
    }
}

pub type SpBox<T> = Sp<Box<T>>;

impl<T> SpBox<T> {
    pub const fn unbox(&self) -> Sp<&T> {
        Sp::new(self.inner(), self.span())
    }
}
