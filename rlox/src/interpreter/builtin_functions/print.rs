use std::rc::Rc;

use rlox_span::SymbolId;

use crate::interpreter::{
  diagnostics::{system_error, LoxRuntimeError},
  objprint::Printable,
  types::{LoxCallable, LoxValueKind},
  Interpreter,
};

/// Built in callable to print arbitrary number of objects
pub struct Print(SymbolId);

impl LoxCallable for Print {
  fn call(
    &self,
    evaluator: &mut Interpreter,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    use core::fmt::Write;
    for (idx, arg) in arguments.into_iter().enumerate() {
      if idx > 0 {
        write!(&mut evaluator.output, " ").map_err(system_error)?;
      }
      arg
        .print(&evaluator.environment, &mut evaluator.output)
        .map_err(system_error)?;
    }
    writeln!(&mut evaluator.output).map_err(system_error)?;
    Ok(LoxValueKind::nil())
  }

  fn arity(&self) -> u32 {
    255
  }

  fn name(&self) -> SymbolId {
    self.0
  }
}

pub fn builtin_print(id: SymbolId) -> LoxValueKind {
  let f = Print(id);
  LoxValueKind::Callable(Rc::new(f))
}
