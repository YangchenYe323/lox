use std::num::NonZeroUsize;

use super::types::LoxValueKind;
use super::types::ObjectId;

/// [Environment] simulates the runtime environment of the running program,
/// which contains:
/// 1. A simulation of the memory model as a mapping from [ObjectId] -> [LoxValueKind]
/// 2. Dummy memory allocator with just a bump of [ObjectId]
pub struct Environment {
  pub memory: Vec<LoxValueKind>,
}

impl Default for Environment {
  fn default() -> Self {
    Self::new()
  }
}

impl Environment {
  pub fn new() -> Self {
    Self { memory: vec![] }
  }

  pub fn assign(&mut self, object: ObjectId, value: LoxValueKind) {
    let addr = Self::to_addr(object);
    assert!(addr < self.memory.len());
    self.memory[addr] = value;
  }

  pub fn get_rvalue(&self, object: ObjectId) -> LoxValueKind {
    let addr = Self::to_addr(object);
    assert!(addr < self.memory.len());
    self.memory[addr].clone()
  }

  pub fn new_object(&mut self) -> ObjectId {
    self.memory.push(LoxValueKind::nil());
    let next_addr = self.memory.len();

    ObjectId(unsafe { NonZeroUsize::new_unchecked(next_addr) })
  }

  /// Maps an [ObjectId] to internal address used.
  // TODO: Propagate nil pointer dereference as would be used in class
  fn to_addr(object: ObjectId) -> usize {
    usize::from(object.0) - 1
  }
}
