use std::rc::Rc;

use rlox_span::SymbolId;

use crate::interpreter::{
  diagnostics::{system_error, LoxRuntimeError},
  types::{LoxCallable, LoxValueKind},
  Interpreter,
};

/// Built in utility to inspect all the object stored in interpretor memory
pub struct Heap(SymbolId);

impl LoxCallable for Heap {
  fn call(
    &self,
    evaluator: &mut Interpreter,
    _arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    use std::fmt::Write;
    for (idx, value) in evaluator.environment.memory.iter().enumerate() {
      writeln!(&mut evaluator.output, "{}: {:?}", idx + 1, value).map_err(system_error)?;
    }
    Ok(LoxValueKind::nil())
  }

  fn arity(&self) -> u32 {
    0
  }

  fn name(&self) -> rlox_span::SymbolId {
    self.0
  }
}

pub fn builtin_heap(id: SymbolId) -> LoxValueKind {
  LoxValueKind::Callable(Rc::new(Heap(id)))
}
