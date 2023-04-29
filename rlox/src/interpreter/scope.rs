//! This module implements the runtime scope chain of lox programming language.

use std::rc::Rc;

use rlox_span::SymbolId;
use rustc_hash::FxHashMap;

use super::types::ObjectId;

/// [Scope] keeps track of a chain of mapping from [SymbolId] to [ObjectId] which is active
/// at a specific point in the execution of the program.
/// [Scope] are "immutable", and every declaration
#[derive(Debug, Default)]
pub struct Scope {
  symbols: FxHashMap<SymbolId, ObjectId>,
  enclosing_scope: Option<Rc<Scope>>,
}

impl Scope {
  pub fn new(symbols: FxHashMap<SymbolId, ObjectId>, enclosing_scope: Option<Rc<Scope>>) -> Self {
    Self {
      symbols,
      enclosing_scope,
    }
  }

  pub fn spawn_empty_child(self: &Rc<Self>) -> Rc<Self> {
    Rc::new(Scope::new(FxHashMap::default(), Some(Rc::clone(self))))
  }

  pub fn define(self: &Rc<Self>, symbol: SymbolId, object: ObjectId) -> Self {
    let mut symbols = self.symbols.clone();
    symbols.insert(symbol, object);
    let enclosing_scope = Rc::clone(self);
    Self::new(symbols, Some(enclosing_scope))
  }

  pub fn get_lvalue_symbol(&self, symbol: SymbolId) -> Option<ObjectId> {
    self.symbols.get(&symbol).copied().or_else(|| {
      self
        .enclosing_scope
        .as_ref()
        .and_then(|s| s.get_lvalue_symbol(symbol))
    })
  }
}
