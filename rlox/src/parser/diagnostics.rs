use miette::Diagnostic;
use thiserror::Error;

use crate::common::span::Span;


#[derive(Debug, Error, Diagnostic)]
pub enum ParserError {
  #[error("Unexpected Token {1}")]
  UnexpectedToken(#[label] Span, /*token name */&'static str),
}