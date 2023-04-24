use miette::{SourceOffset, SourceSpan};

/// [Span] represents a range in the soruce code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
  pub start: u32,
  pub end: u32,
}

impl Span {
  pub fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }
}

impl From<Span> for SourceSpan {
  fn from(value: Span) -> Self {
    SourceSpan::new(
      SourceOffset::from(value.start as usize),
      SourceOffset::from((value.end - value.start + 1) as usize),
    )
  }
}

/// The [Spanned] trait describes any data structure that represents a span
/// in the source code
pub trait Spanned {
  fn span(&self) -> Span;
}
