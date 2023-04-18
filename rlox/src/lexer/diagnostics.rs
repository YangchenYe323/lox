use miette::{Diagnostic, SourceOffset, SourceSpan};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
  start: u32,
  end: u32,
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
      SourceOffset::from((value.end - value.start) as usize),
    )
  }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Unexpected character {0} on line {1}")]
pub struct UnexpectedCharacter(pub char, pub u32, #[label] pub Span);

#[derive(Debug, Error, Diagnostic)]
#[error("Unterminated string literal")]
pub struct UnterminatedString(#[label] pub Span);
