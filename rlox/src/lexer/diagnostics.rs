use miette::Diagnostic;
use thiserror::Error;

use rlox_span::Span;

#[derive(Debug, Error, Diagnostic)]
#[error("Unexpected character {0} on line {1}")]
pub struct UnexpectedCharacter(pub char, pub u32, #[label] pub Span);

#[derive(Debug, Error, Diagnostic)]
#[error("Unterminated string literal")]
pub struct UnterminatedString(#[label] pub Span);

#[derive(Debug, Error, Diagnostic)]
#[error("Unterminated block comments")]
pub struct UnterminatedComments(#[label] pub Span);

#[derive(Debug, Error, Diagnostic)]
pub enum LexerError {
  #[error("Unexpected character {0} on line {1}")]
  UnexpectedCharacter(char, u32, #[label] Span),
  #[error("Unterminated string literal")]
  UnterminatedString(#[label] Span),
  #[error("Unterminated block comments")]
  UnterminatedComments(#[label] Span),
}
