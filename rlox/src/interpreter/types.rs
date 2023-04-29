use std::fmt::Debug;
use std::{num::NonZeroUsize, rc::Rc};

use super::scope::Scope;
use super::{diagnostics::LoxRuntimeError, Interpreter};
use rlox_ast::facades::ClassDecl;
use rlox_ast::INTERNER;
use rlox_ast::{
  facades::{Block, FnDecl, VarDecl},
  visit::AstVisitor,
};
use rlox_span::SymbolId;
use rustc_hash::FxHashMap;

/// [LoxValueKind] describes the available values of a lox object stored in a variable.
/// Since lox is dynamically typed, every variable is mapped to an [ObjectId], which stores
/// [LoxValueKind]
#[derive(Clone)]
pub enum LoxValueKind {
  // User facing values, i.e., can be stored directly in variables
  Number(f64),
  String(String),
  Boolean(bool),
  Callable(Rc<dyn LoxCallable>),
  ObjectId(ObjectId),
  Nil,

  // Internal values, no user variable would directly store these.
  Class(Rc<LoxClass>),
  Object(LoxInstance),
}

impl std::fmt::Debug for LoxValueKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
      Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
      Self::Boolean(arg0) => f.debug_tuple("Boolean").field(arg0).finish(),
      Self::Callable(_arg0) => f.debug_tuple("Callable").finish(),
      Self::ObjectId(arg0) => f.debug_tuple("ObjectId").field(arg0).finish(),
      Self::Class(arg0) => f.debug_tuple("Class").field(arg0).finish(),
      Self::Object(arg0) => f.debug_tuple("Object").field(arg0).finish(),
      Self::Nil => f.debug_tuple("Nil").finish(),
    }
  }
}

impl LoxValueKind {
  #[inline(always)]
  pub fn nil() -> Self {
    Self::Nil
  }

  pub fn is_truthful(&self) -> bool {
    match self {
      LoxValueKind::Number(n) => *n != 0.0,
      LoxValueKind::String(s) => !s.is_empty(),
      LoxValueKind::Boolean(b) => *b,
      LoxValueKind::ObjectId(_) => true,
      LoxValueKind::Nil => false,
      LoxValueKind::Callable(_) => true,
      _ => unreachable!(),
    }
  }

  pub fn type_name(&self) -> &'static str {
    match self {
      LoxValueKind::Number(_) => "Number",
      LoxValueKind::String(_) => "String",
      LoxValueKind::Boolean(_) => "Boolean",
      LoxValueKind::ObjectId(_) => "Object",
      LoxValueKind::Callable(_) => "Callable",
      LoxValueKind::Nil => "Nil",
      LoxValueKind::Class(_) => "Class",
      LoxValueKind::Object(_) => "ObjectValue",
      // _ => unreachable!(),
    }
  }
}

/// Lox's abstraction of the concept of "Memory Reference". Each variable is mapped to an [ObjectId],
/// and a lox object might store references to other objects, i.e., they might store [ObjectID]s internally.
/// Note that Nil is a special case of [ObjectId]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ObjectId(pub NonZeroUsize);

/// [LoxCallable] trait describes a lox value that can be called, e.g., a
/// user defined function or an interpreter built-in function.
pub trait LoxCallable {
  fn call(
    &self,
    evaluator: &mut Interpreter,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError>;

  fn arity(&self) -> u32;

  fn name(&self) -> SymbolId;
}

/// [Function] is a user-defined function in lox language
#[derive(Debug)]
pub struct Function {
  pub is_init: bool,
  pub name: SymbolId,
  // The captured environment of the enclosing scopeproperty.
  pub closure: Rc<Scope>,
  pub formal_parameters: Vec<VarDecl>,
  pub body: Block,
}

impl Function {
  pub fn new(is_init: bool, handle: FnDecl, closure: Rc<Scope>) -> Self {
    let name = handle.name();
    let formal_parameters = handle.parameter_list().parameters().collect();
    let body = handle.body();
    Self {
      is_init,
      name,
      closure,
      formal_parameters,
      body,
    }
  }
}

impl LoxCallable for Function {
  fn call(
    &self,
    evaluator: &mut Interpreter,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    // resolve actual arguments
    let mut arguments = arguments;
    // try fill in missing arguments with default initializers
    for idx in arguments.len()..self.formal_parameters.len() {
      arguments.push(evaluator.visit_expression(self.formal_parameters[idx].init_expr())?);
    }

    assert_eq!(arguments.len(), self.formal_parameters.len());

    evaluator.with_scope(Rc::clone(&self.closure), |evaluator| {
      for (var, value) in self.formal_parameters.iter().zip(arguments.into_iter()) {
        let symbol = var.var_symbol();
        evaluator.declare_variable(symbol, value);
      }
      let ret = evaluator
        .visit_block(self.body)
        .map_err(LoxRuntimeError::from)?;

      if self.is_init {
        if !matches!(ret, LoxValueKind::Nil) {
          println!("{:?}", ret);
          return Err(LoxRuntimeError::ReturnFromInit);
        }
        let this_var = INTERNER.with_borrow_mut(|i| i.intern("this"));
        let this_ref = evaluator.active_scope.get_lvalue_symbol(this_var).unwrap();
        let LoxValueKind::ObjectId(this_value) = evaluator.environment.get_rvalue(this_ref) else { unreachable!() };
        Ok(LoxValueKind::ObjectId(
          this_value
        ))
      } else {
        Ok(ret)
      }
    })
  }

  fn arity(&self) -> u32 {
    self.formal_parameters.len() as u32
  }

  fn name(&self) -> SymbolId {
    self.name
  }
}

#[derive(Clone, Debug)]
pub struct LoxClass {
  pub name: SymbolId,
  pub methods: FxHashMap<SymbolId, Rc<Function>>,
  pub static_methods: FxHashMap<SymbolId, Rc<Function>>,
}

impl LoxClass {
  pub fn new(class: ClassDecl, closure: Rc<Scope>) -> Self {
    let name = class.name();
    let methods = class
      .method_list()
      .methods()
      .map(|method| {
        let method_name = method.name();
        let is_init = INTERNER.with_borrow(|i| i.get(method_name) == "init");
        let method = Rc::new(Function::new(is_init, method, Rc::clone(&closure)));
        (method_name, method)
      })
      .collect();
    let static_methods = class
      .static_method_list()
      .methods()
      .map(|method| {
        let method_name = method.name();
        let method = Rc::new(Function::new(false, method, Rc::clone(&closure)));
        (method_name, method)
      })
      .collect();

    Self {
      name,
      methods,
      static_methods,
    }
  }

  pub fn new_instance(
    class: Rc<LoxClass>,
    interpreter: &mut Interpreter,
    arguments: Vec<LoxValueKind>,
  ) -> Result<ObjectId, LoxRuntimeError> {
    let this = INTERNER.with_borrow_mut(|i| i.intern("this"));
    let init = INTERNER.with_borrow_mut(|i| i.intern("init"));

    interpreter.with_scope(
      interpreter.active_scope.spawn_empty_child(),
      |interpreter| {
        // Step 1: Allocate memory and bind "this" variable.
        let object_value = interpreter.environment.new_object();
        let object_ref = interpreter.declare_variable(this, LoxValueKind::ObjectId(object_value));

        let instance_raw = LoxInstance::new(object_ref, Rc::clone(&class));

        let constructor = class
          .methods
          .get(&init)
          .map(|constructor| instance_raw.bind_this(constructor));

        interpreter
          .environment
          .assign(object_value, LoxValueKind::Object(instance_raw));

        // Constructor function
        if let Some(constructor) = constructor {
          constructor.call(interpreter, arguments)?;
        }

        Ok(object_value)
      },
    )
  }
}

#[derive(Clone, Debug)]
pub struct LoxInstance {
  pub this: ObjectId,
  pub class: Rc<LoxClass>,
  pub fields: FxHashMap<SymbolId, ObjectId>,
}

impl LoxInstance {
  /// Create a new [LoxInstance]
  /// Note that the 'this' stores another [ObjectId] which stores this instance.
  /// This is to make sure the 'this' variable is accessed in the same way as other object references.
  pub fn new(this: ObjectId, class: Rc<LoxClass>) -> Self {
    Self {
      this,
      class,
      fields: FxHashMap::default(),
    }
  }

  /// Bind a [Function]'s 'this' variable to the current instance.
  pub fn bind_this(&self, function: &Function) -> Rc<dyn LoxCallable> {
    let this_var = INTERNER.with_borrow_mut(|i| i.intern("this"));

    let Function {
      is_init,
      name,
      closure,
      formal_parameters,
      body,
    } = function;
    let closure_with_this = closure.define(this_var, self.this);

    let method_instance = Function {
      is_init: *is_init,
      name: *name,
      closure: Rc::new(closure_with_this),
      formal_parameters: formal_parameters.clone(),
      body: *body,
    };

    Rc::new(method_instance)
  }
}
