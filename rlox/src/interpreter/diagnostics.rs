use miette::Diagnostic;
use thiserror::Error;

use rlox_span::{Span, Spanned};

#[derive(Debug, Error, Diagnostic)]
pub enum LoxRuntimeError {
  #[error("RuntimeError: Divide by zero")]
  DivideByZero,
  #[error("RuntimeError: Operation {0} cannot be performed on values of type {1} and {2}")]
  BinaryOpTypeError(
    /* operator */ &'static str,
    /* type1 */ &'static str,
    /* type2 */ &'static str,
  ),
  #[error("RuntimeError: Operation {0} cannot be performed on values of type {1}")]
  UnaryOpTypeError(
    /* operator */ &'static str,
    /* type */ &'static str,
  ),
  #[error("RuntimeError: Variable is accessed before declaration")]
  UnresolvedReference,
  #[error("Object of type {0} is not callable")]
  InalidCall(&'static str),
  /// A catch-all case for all non-lox related error produced by rust code itself
  #[error("{0}")]
  SystemError(Box<dyn std::error::Error + Send + Sync + 'static>),
}

#[derive(Debug, Error, Diagnostic)]
#[error("{error}")]
pub struct SpannedLoxRuntimeError {
  error: LoxRuntimeError,
  #[label]
  span: Span,
}

pub trait SpannedLoxRuntimeErrorWrapper {
  fn wrap(&self, error: LoxRuntimeError) -> SpannedLoxRuntimeError;
}

impl<T> SpannedLoxRuntimeErrorWrapper for T
where
  T: Spanned,
{
  fn wrap(&self, error: LoxRuntimeError) -> SpannedLoxRuntimeError {
    SpannedLoxRuntimeError {
      error,
      span: self.span(),
    }
  }
}
