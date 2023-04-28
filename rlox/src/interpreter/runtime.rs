use std::num::NonZeroUsize;

use rustc_hash::FxHashMap;

use super::types::LoxValueKind;
use super::types::ObjectId;

/// [Environment] simulates the runtime environment of the running program,
/// which contains:
/// 1. A simulation of the memory model as a mapping from [ObjectId] -> [LoxValueKind]
/// 2. Dummy memory allocator with just a bump of [ObjectId]
pub struct Environment {
  memory: FxHashMap<ObjectId, LoxValueKind>,
  next_object: usize,
}

impl Default for Environment {
  fn default() -> Self {
    Self::new()
  }
}

impl Environment {
  pub fn new() -> Self {
    Self {
      memory: FxHashMap::default(),
      next_object: 1,
    }
  }

  pub fn assign(&mut self, object: ObjectId, value: LoxValueKind) {
    self.memory.insert(object, value);
  }

  pub fn get_rvalue(&mut self, object: ObjectId) -> Option<LoxValueKind> {
    self.memory.get(&object).cloned()
  }

  pub fn new_object(&mut self) -> ObjectId {
    let id = ObjectId::Id(unsafe { NonZeroUsize::new_unchecked(self.next_object) });
    self.next_object += 1;
    id
  }
}

// pub fn populate_builtin_globals(environment: &mut Environment) {
//   populate_global(environment, "time", builtin_time());
//   populate_global(environment, "print", builtin_print());
// }

// fn populate_global(environment: &mut Environment, name: &'static str, value: LoxValueKind) {
//   let symbol = INTERNER.with_borrow_mut(|interner| interner.intern(name));
//   environment.define(symbol, value);
// }
