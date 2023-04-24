use miette::Diagnostic;
use thiserror::Error;

use crate::common::span::{Span, Spanned};

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
  #[error("RuntimeError: Variable {0} is referred before declaration")]
  UnresolvedReference(/* variable name*/ &'static str),
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
