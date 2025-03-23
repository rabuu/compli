use std::fmt;

/// An identifier string
///
/// It is really just a string slice.
/// The 'src lifetime refers to the source code string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident<'src>(&'src str);

impl Ident<'_> {
    pub fn as_str(&self) -> &str {
        self.0
    }
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<&'static str> for Ident<'_> {
    fn from(value: &'static str) -> Self {
        Self(value)
    }
}
