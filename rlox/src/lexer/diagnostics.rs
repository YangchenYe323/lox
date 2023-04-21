use miette::{Diagnostic, SourceOffset, SourceSpan};
use thiserror::Error;

use crate::common::span::Span;

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

#[derive(Debug, Error, Diagnostic)]
#[error("Unterminated block comments")]
pub struct UnterminatedComments(#[label] pub Span);
