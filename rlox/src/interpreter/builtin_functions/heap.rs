use std::rc::Rc;

use rlox_span::SymbolId;

use crate::interpreter::{
  diagnostics::LoxRuntimeError,
  types::{LoxCallable, LoxValueKind},
  Interpreter,
};

/// Built in utility to inspect all the object stored in interpretor memory
pub struct Heap(SymbolId);

impl LoxCallable for Heap {
  fn call(
    &self,
    _evaluator: &mut Interpreter,
    _arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
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
