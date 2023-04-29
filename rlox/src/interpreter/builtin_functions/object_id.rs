use std::rc::Rc;

use rlox_span::SymbolId;

use crate::interpreter::{
  diagnostics::{system_error, LoxRuntimeError},
  types::{LoxCallable, LoxValueKind},
  Interpreter,
};

pub struct GetObjectId(SymbolId);

impl LoxCallable for GetObjectId {
  fn call(
    &self,
    evaluator: &mut Interpreter,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    use std::fmt::Write;
    let Some(arg) = arguments.first() else {
        return Ok(LoxValueKind::nil());
      };
    match arg {
      LoxValueKind::Number(n) => {
        writeln!(&mut evaluator.output, "{}", n).map_err(system_error)?;
      }
      LoxValueKind::String(s) => {
        writeln!(&mut evaluator.output, "{}", s).map_err(system_error)?;
      }
      LoxValueKind::Boolean(b) => {
        writeln!(&mut evaluator.output, "{}", b).map_err(system_error)?;
      }
      LoxValueKind::Callable(_) => {
        writeln!(&mut evaluator.output, "Callable").map_err(system_error)?;
      }
      LoxValueKind::ObjectId(object) => {
        writeln!(&mut evaluator.output, "{:?}", object).map_err(system_error)?;
      }
      LoxValueKind::Nil => {
        writeln!(&mut evaluator.output, "nil").map_err(system_error)?;
      }
      LoxValueKind::Class(c) => {
        writeln!(&mut evaluator.output, "{:?}", c).map_err(system_error)?;
      }
      LoxValueKind::Object(o) => {
        writeln!(&mut evaluator.output, "{:?}", o).map_err(system_error)?;
      }
    }
    Ok(LoxValueKind::nil())
  }

  fn arity(&self) -> u32 {
    1
  }

  fn name(&self) -> SymbolId {
    self.0
  }
}

pub fn builtin_get_object_id(symbol: SymbolId) -> LoxValueKind {
  LoxValueKind::Callable(Rc::new(GetObjectId(symbol)))
}
