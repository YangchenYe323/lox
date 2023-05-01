//! This module implements the runtime scope chain of lox programming language.

use std::rc::Rc;

use rlox_span::SymbolId;

use super::types::ObjectId;

/// [Scope] keeps track of a chain of mapping from [SymbolId] to [ObjectId] which is active
/// at a specific point in the execution of the program.
/// [Scope] are "immutable", and every declaration
#[derive(Debug, Default)]
pub struct Scope {
  symbols: Option<(SymbolId, ObjectId)>,
  enclosing_scope: Option<Rc<Scope>>,
}

impl Scope {
  pub fn new(symbols: Option<(SymbolId, ObjectId)>, enclosing_scope: Option<Rc<Scope>>) -> Self {
    Self {
      symbols,
      enclosing_scope,
    }
  }

  pub fn spawn_empty_child(self: &Rc<Self>) -> Rc<Self> {
    Rc::new(Scope::new(None, Some(Rc::clone(self))))
  }

  pub fn define(self: &Rc<Self>, symbol: SymbolId, object: ObjectId) -> Self {
    let enclosing_scope = Rc::clone(self);
    Self::new(Some((symbol, object)), Some(enclosing_scope))
  }

  pub fn get_lvalue_symbol(&self, symbol: SymbolId) -> Option<ObjectId> {
    if let Some((cur_symbol, object)) = self.symbols && cur_symbol == symbol {
      Some(object)
    } else {
      self.enclosing_scope.as_ref().and_then(|s| s.get_lvalue_symbol(symbol))
    }
  }
}
