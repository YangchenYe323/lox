use miette::Diagnostic;
use thiserror::Error;

use crate::common::span::Span;

#[derive(Debug, Error, Diagnostic)]
#[error("Unexpected Token {1}")]
pub struct UnexpectedToken(
  #[label] pub Span,
  /*actual */ pub &'static str,
);
