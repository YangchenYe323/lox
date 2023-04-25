use std::num::NonZeroUsize;

use rustc_hash::FxHashMap;

use rlox_span::SymbolId;

use crate::ast::facades::AssignTarget;

use super::types::LoxValueKind;
use super::types::ObjectId;

/// [Environment] simulates the runtime environment of the running program,
/// which contains:
/// 1. A simulation of the memory model as a mapping from [ObjectId] -> [LoxValueKind]
/// 2. Scope chain as a chained mapping from [SymbolId] -> [ObjectId]
/// 3. Dummy memory allocator with just a bump of [ObjectId]
#[derive(Debug)]
pub struct Environment {
  memory: FxHashMap<ObjectId, LoxValueKind>,
  scopes: Vec<FxHashMap<SymbolId, ObjectId>>,
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
      scopes: vec![],
      next_addr: 1,
    }
  }

  pub fn define(&mut self, symbol: SymbolId, value: LoxValueKind) {
    let id = self.new_object();
    self.current_scope().insert(symbol, id);
    self.memory.insert(id, value);
  }

  pub fn assign(&mut self, object: ObjectId, value: LoxValueKind) {
    self.memory.insert(object, value);
  }

  pub fn get_rvalue(&mut self, symbol: SymbolId) -> Option<LoxValueKind> {
    for idx in (0..self.scopes.len()).rev() {
      let scope = &mut self.scopes[idx];
      if let Some(object) = scope.get(&symbol) {
        return self.memory.get(object).cloned();
      }
    }
    None
  }

  /// l-value corresponds to the object_id associated with the assignment target, which is
  /// roughly the memory address that can be written to for assigning to this variable.
  pub fn get_lvalue(&mut self, target: AssignTarget<'_>) -> Option<ObjectId> {
    match target {
      AssignTarget::Ident(var) => {
        let symbol = var.var_symbol();
        for idx in (0..self.scopes.len()).rev() {
          let scope = &mut self.scopes[idx];
          if let Some(object) = scope.get(&symbol) {
            return Some(*object);
          }
        }
        None
      }
    }
  }

  pub fn enter_scope(&mut self) {
    self.scopes.push(FxHashMap::default());
  }

  pub fn exit_scope(&mut self) {
    self.scopes.pop();
  }

  pub fn current_scope(&mut self) -> &mut FxHashMap<SymbolId, ObjectId> {
    self.scopes.last_mut().unwrap()
  }

  fn new_object(&mut self) -> ObjectId {
    let id = ObjectId::Id(unsafe { NonZeroUsize::new_unchecked(self.next_addr) });
    self.next_addr += 1;
    id
  }
}
