use rlox_ast::INTERNER;

use super::{
  runtime::Environment,
  types::{LoxClass, LoxInstance, LoxValueKind, ObjectId},
};

pub trait Printable {
  fn print(&self, environment: &Environment, output: &mut impl std::fmt::Write)
    -> std::fmt::Result;

  fn to_string(&self, environment: &Environment) -> String {
    let mut s = String::new();
    self.print(environment, &mut s).unwrap();
    s
  }
}

impl Printable for LoxValueKind {
  fn print(
    &self,
    environment: &Environment,
    output: &mut impl std::fmt::Write,
  ) -> std::fmt::Result {
    match self {
      LoxValueKind::Number(n) => write!(output, "{}", n),
      LoxValueKind::String(s) => write!(output, "{}", s),
      LoxValueKind::Boolean(b) => write!(output, "{}", b),
      LoxValueKind::Callable(callable) => write!(
        output,
        "Callable({})",
        INTERNER.with_borrow(|interner| interner.get(callable.name()))
      ),
      LoxValueKind::ObjectId(id) => match id {
        ObjectId::Nil => write!(output, "nil"),
        ObjectId::Id(_) => {
          // Follow the pointer
          let val = environment.get_rvalue(*id);
          val.print(environment, output)
        }
      },
      LoxValueKind::Class(c) => c.print(environment, output),
      LoxValueKind::Object(o) => o.print(environment, output),
    }
  }
}

impl Printable for LoxClass {
  fn print(
    &self,
    _environment: &Environment,
    output: &mut impl std::fmt::Write,
  ) -> std::fmt::Result {
    let name = self.name;
    write!(output, "Class({})", INTERNER.with_borrow(|i| i.get(name)))
  }
}

impl Printable for LoxInstance {
  fn print(
    &self,
    environment: &Environment,
    output: &mut impl std::fmt::Write,
  ) -> std::fmt::Result {
    write!(output, "Instance({})", self.class.to_string(environment))
  }
}
