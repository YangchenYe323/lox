use std::rc::Rc;

use crate::interpreter::{
  diagnostics::LoxRuntimeError,
  types::{LoxCallable, LoxValueKind},
  Evaluator,
};

/// Built in callable to print arbitrary number of objects
pub struct Print;

impl LoxCallable for Print {
  fn call(
    &self,
    _evaluator: &mut Evaluator,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    for (idx, arg) in arguments.into_iter().enumerate() {
      if idx == 0 {
        print!("{}", arg);
      } else {
        print!(" {}", arg);
      }
    }
    println!();
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
