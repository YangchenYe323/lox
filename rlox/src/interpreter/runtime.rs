use std::num::NonZeroUsize;

use rustc_hash::FxHashMap;

use rlox_span::SymbolId;

use super::types::LoxValueKind;
use super::types::ObjectId;

/// [Environment] simulates the memory layout of the running program,
/// which is essentially a mapping from [ObjectId]s to correspoding [LoxValueKind]s.
#[derive(Debug)]
pub struct Environment {
  memory: FxHashMap<ObjectId, LoxValueKind>,
  // TODO(yangchen): Implement lexical scoping
  symbols: FxHashMap<SymbolId, ObjectId>,
  next_addr: usize,
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
      // global scope
      symbols: FxHashMap::default(),
      next_addr: 1,
    }
  }

  pub fn define(&mut self, symbol: SymbolId, value: LoxValueKind) {
    let id = ObjectId::Id(unsafe { NonZeroUsize::new_unchecked(self.next_addr) });
    self.next_addr += 1;
    self.symbols.insert(symbol, id);
    self.memory.insert(id, value);
  }

  pub fn get_rvalue(&mut self, symbol: SymbolId) -> Option<LoxValueKind> {
    self
      .symbols
      .get(&symbol)
      .and_then(|id| self.memory.get(id))
      .cloned()
  }
}
