use std::rc::Rc;

use crate::interpreter::{
  diagnostics::{system_error, LoxRuntimeError},
  types::{LoxCallable, LoxValueKind},
  Interpreter,
};

/// Built in callable to print arbitrary number of objects
pub struct Print;

impl LoxCallable for Print {
  fn call(
    &self,
    evaluator: &mut Interpreter,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    use core::fmt::Write;
    for (idx, arg) in arguments.into_iter().enumerate() {
      if idx == 0 {
        write!(&mut evaluator.output, "{}", arg).map_err(system_error)?;
      } else {
        write!(&mut evaluator.output, " {}", arg).map_err(system_error)?;
      }
    }
    writeln!(&mut evaluator.output).map_err(system_error)?;
    Ok(LoxValueKind::nil())
  }

  fn arity(&self) -> u32 {
    255
  }
}

pub fn builtin_print() -> LoxValueKind {
  let f = Print;
  LoxValueKind::Callable(Rc::new(f))
}
