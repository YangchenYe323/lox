//! This module implements the runtime scope chain of lox programming language.

use std::{cell::RefCell, rc::Rc};

use rlox_span::SymbolId;
use rustc_hash::FxHashMap;

use super::{types::LoxValueKind, LoxRuntimeError};

/// [Scope] keeps track of a chain of mapping from [SymbolId] to [ObjectId] which is active
/// at a specific point in the execution of the program.
/// [Scope] are "immutable", and every declaration
#[derive(Debug, Default)]
pub struct Scope {
  symbols: RefCell<FxHashMap<SymbolId, LoxValueKind>>,
  enclosing_scope: Option<Rc<Scope>>,
}

impl Scope {
  pub fn new(
    symbols: FxHashMap<SymbolId, LoxValueKind>,
    enclosing_scope: Option<Rc<Scope>>,
  ) -> Self {
    Self {
      symbols: RefCell::new(symbols),
      enclosing_scope,
    }
  }

  pub fn spawn_empty_child(self: &Rc<Self>) -> Rc<Self> {
    Rc::new(Scope::new(FxHashMap::default(), Some(Rc::clone(self))))
  }

  pub fn define(self: &Rc<Self>, symbol: SymbolId, object: LoxValueKind) -> Self {
    let enclosing_scope = Rc::clone(self);
    let mut symbols = FxHashMap::default();
    symbols.insert(symbol, object);
    Self::new(symbols, Some(enclosing_scope))
  }

  pub fn get_lvalue_symbol(&self, symbol: SymbolId) -> Option<LoxValueKind> {
    if let Some(object) = self.symbols.borrow().get(&symbol) {
      Some(object.clone())
    } else {
      self
        .enclosing_scope
        .as_ref()
        .and_then(|s| s.get_lvalue_symbol(symbol))
    }
  }

  pub fn assign(&self, symbol: SymbolId, value: LoxValueKind) -> Result<(), LoxRuntimeError> {
    if let Some(object) = self.symbols.borrow_mut().get_mut(&symbol) {
      *object = value;
      Ok(())
    } else if let Some(scope) = &self.enclosing_scope {
      scope.assign(symbol, value)
    } else {
      Err(LoxRuntimeError::UnresolvedReference)
    }
  }

  pub fn assign_current_level(&self, symbol: SymbolId, value: LoxValueKind) {
    self.symbols.borrow_mut().insert(symbol, value);
  }
}
