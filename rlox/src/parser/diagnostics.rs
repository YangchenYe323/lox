use miette::Diagnostic;
use thiserror::Error;

use crate::lexer::Token;

use rlox_span::Span;

#[derive(Debug, Error, Diagnostic)]
pub enum ParserError {
  #[error("Unexpected Token {1}")]
  UnexpectedToken(#[label] Span, /*token name */ &'static str),
  #[error("Invalid assignment target")]
  InvalidAssignment(#[label] Span),
  #[error("Break statement cannot be used outside of loop")]
  BreakOutsideLoop(#[label] Span),
  #[error("Return statement cannot be used outside of function")]
  ReturnOutsideFunction(#[label] Span),
  #[error("Function call can accept at most 255 arguments")]
  TooManyArguments(#[label] Span),
}

pub fn unexpected_token(token: &Token) -> ParserError {
  ParserError::UnexpectedToken(token.span, token.kind.to_str())
}
