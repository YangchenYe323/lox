use std::{num::NonZeroUsize, rc::Rc};

use rlox_ast::{
  facades::{Block, FnDecl, VarDecl},
  visit::AstVisitor,
};
use rlox_span::SymbolId;
use rustc_hash::FxHashMap;

use super::{diagnostics::LoxRuntimeError, Evaluator};

/// [LoxValueKind] describes the available values of a lox object stored in a variable.
/// Since lox is dynamically typed, every variable is mapped to an [ObjectId], which stores
/// [LoxValueKind]
#[derive(Clone)]
pub enum LoxValueKind {
  Number(f64),
  String(String),
  Boolean(bool),
  Object(ObjectId),
  Callable(Rc<dyn LoxCallable>),
}

impl std::fmt::Display for LoxValueKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LoxValueKind::Number(number) => number.fmt(f),
      LoxValueKind::String(s) => s.fmt(f),
      LoxValueKind::Boolean(b) => b.fmt(f),
      LoxValueKind::Object(object) => object.fmt(f),
      LoxValueKind::Callable(_) => "lox callable".fmt(f),
    }
  }
}

impl LoxValueKind {
  #[inline(always)]
  pub fn nil() -> Self {
    Self::Object(ObjectId::Nil)
  }

  pub fn is_truthful(&self) -> bool {
    match self {
      LoxValueKind::Number(n) => *n != 0.0,
      LoxValueKind::String(s) => !s.is_empty(),
      LoxValueKind::Boolean(b) => *b,
      LoxValueKind::Object(o) => !matches!(o, ObjectId::Nil),
      LoxValueKind::Callable(_) => true,
    }
  }

  pub fn type_name(&self) -> &'static str {
    match self {
      LoxValueKind::Number(_) => "Number",
      LoxValueKind::String(_) => "String",
      LoxValueKind::Boolean(_) => "Boolean",
      LoxValueKind::Object(o) => match o {
        ObjectId::Nil => "Nil",
        ObjectId::Id(_) => "Object",
      },
      LoxValueKind::Callable(_) => "Callable",
    }
  }
}

/// Lox's abstraction of the concept of "Memory Reference". Each variable is mapped to an [ObjectId],
/// and a lox object might store references to other objects, i.e., they might store [ObjectID]s internally.
/// Note that Nil is a special case of [ObjectId]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ObjectId {
  Nil,
  Id(NonZeroUsize),
}

impl std::fmt::Display for ObjectId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ObjectId::Nil => "nil".fmt(f),
      ObjectId::Id(id) => format!("object located at: {}", id).fmt(f),
    }
  }
}

/// [LoxCallable] trait describes a lox value that can be called, e.g., a
/// user defined function or an interpreter built-in function.
pub trait LoxCallable {
  fn call(
    &self,
    evaluator: &mut Evaluator,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError>;

  fn arity(&self) -> u32;
}

/// [Function] is a user-defined function in lox language
pub struct Function {
  // The captured environment of the enclosing scope.
  closure: FxHashMap<SymbolId, ObjectId>,
  formal_parameters: Vec<VarDecl>,
  body: Block,
}

impl Function {
  pub fn new(handle: FnDecl, closure: FxHashMap<SymbolId, ObjectId>) -> Self {
    let formal_parameters = handle.parameter_list().parameters().collect();
    let body = handle.body();
    Self {
      closure,
      formal_parameters,
      body,
    }
  }
}

impl LoxCallable for Function {
  fn call(
    &self,
    evaluator: &mut Evaluator,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    // resolve actual arguments
    let mut arguments = arguments;
    // try fill in missing arguments with default initializers
    for idx in arguments.len()..self.formal_parameters.len() {
      arguments.push(evaluator.visit_expression(self.formal_parameters[idx].init_expr())?);
    }

    assert_eq!(arguments.len(), self.formal_parameters.len());

    // Firstly apply the closure scope
    evaluator.environment.apply_scope(&self.closure);
    // Secondly apply the formal parameter scope
    evaluator.environment.enter_scope();
    // bind formal parameters to argyments
    for (var, value) in self.formal_parameters.iter().zip(arguments.into_iter()) {
      let symbol = var.var_symbol();
      evaluator.environment.define(symbol, value);
    }

    let result = evaluator
      .visit_block(self.body)
      .map_err(LoxRuntimeError::from);

    // Exit formal parameter scope
    evaluator.environment.exit_scope();
    // Exit closure
    evaluator.environment.exit_scope();

    result
  }

  fn arity(&self) -> u32 {
    self.formal_parameters.len() as u32
  }
}
