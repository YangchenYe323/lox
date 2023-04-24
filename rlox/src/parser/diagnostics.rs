use miette::Diagnostic;
use thiserror::Error;

use crate::lexer::Token;

use rlox_span::Span;

#[derive(Debug, Error, Diagnostic)]
pub enum ParserError {
  #[error("Unexpected Token {1}")]
  UnexpectedToken(#[label] Span, /*token name */ &'static str),
}

pub fn unexpected_token(token: &Token) -> ParserError {
  ParserError::UnexpectedToken(token.span, token.kind.to_str())
}
