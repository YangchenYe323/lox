use miette::Diagnostic;
use rlox_ast::INTERNER;
use thiserror::Error;

use rlox_span::{Span, Spanned, SymbolId};

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
  #[error("RuntimeError: {0} is not an object")]
  InvalidMemberAccess(/* type */ &'static str),
  #[error("Runtime Error: Object has no property {0}")]
  NoSuchProperty(/* property */ &'static str),
  #[error("Object of type {0} is not callable")]
  InalidCall(&'static str),
  /// A catch-all case for all non-lox related error produced by rust code itself
  #[error("{0}")]
  SystemError(Box<dyn std::error::Error + Send + Sync + 'static>),
}

pub fn system_error<E: std::error::Error + Send + Sync + 'static>(error: E) -> LoxRuntimeError {
  LoxRuntimeError::SystemError(Box::new(error))
}

pub fn no_such_property(property: SymbolId) -> LoxRuntimeError {
  LoxRuntimeError::NoSuchProperty(INTERNER.with_borrow(|i| i.get(property)))
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

impl From<SpannedLoxRuntimeError> for LoxRuntimeError {
  fn from(value: SpannedLoxRuntimeError) -> Self {
    value.error
  }
}
