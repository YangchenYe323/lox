use std::rc::Rc;

use crate::interpreter::{
  diagnostics::LoxRuntimeError,
  types::{LoxCallable, LoxValueKind},
  Evaluator,
};

/// Built in callable to get the current time.
pub struct Time;

impl LoxCallable for Time {
  fn call(
    &self,
    _evaluator: &mut Evaluator,
    _arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    let now = std::time::SystemTime::now()
      .duration_since(std::time::UNIX_EPOCH)
      .map_err(|e| LoxRuntimeError::SystemError(Box::new(e)))?
      .as_millis() as f64;
    Ok(LoxValueKind::Number(now))
  }

  fn arity(&self) -> u32 {
    0
  }
}

pub fn builtin_time() -> LoxValueKind {
  let f = Time;
  LoxValueKind::Callable(Rc::new(f))
}
