use std::num::NonZeroUsize;

use rustc_hash::FxHashMap;

use rlox_span::SymbolId;

use crate::ast::facades::AssignTarget;

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
    let id = self.new_object();
    self.symbols.insert(symbol, id);
    self.memory.insert(id, value);
  }

  pub fn assign(&mut self, object: ObjectId, value: LoxValueKind) {
    self.memory.insert(object, value);
  }

  pub fn get_rvalue(&mut self, symbol: SymbolId) -> Option<LoxValueKind> {
    self
      .symbols
      .get(&symbol)
      .and_then(|id| self.memory.get(id))
      .cloned()
  }

  pub fn get_lvalue(&mut self, target: AssignTarget<'_>) -> Option<ObjectId> {
    match target {
      AssignTarget::Ident(var) => {
        // The l-value asscoiated with identifier is always a NEW objectid
        let symbol = var.var_symbol();
        self.symbols.contains_key(&symbol).then(|| {
          let id = self.new_object();
          self.symbols.insert(symbol, id);
          id
        })
      }
    }
  }

  fn new_object(&mut self) -> ObjectId {
    let id = ObjectId::Id(unsafe { NonZeroUsize::new_unchecked(self.next_addr) });
    self.next_addr += 1;
    id
  }
}
