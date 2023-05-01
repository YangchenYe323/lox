use std::cell::RefCell;
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
#[derive(Clone)]
pub enum LoxValueKind {
  // User facing values, i.e., can be stored directly in variables
  Number(f64),
  String(String),
  Boolean(bool),
  Callable(Rc<dyn LoxCallable>),
  Nil,
  Class(Rc<LoxClass>),
  Object(Rc<RefCell<LoxInstance>>),
}

impl std::fmt::Display for LoxValueKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LoxValueKind::Number(n) => write!(f, "{}", n),
      LoxValueKind::String(s) => write!(f, "{}", s),
      LoxValueKind::Boolean(b) => write!(f, "{}", b),
      LoxValueKind::Callable(c) => {
        let name = INTERNER.with_borrow(|i| i.get(c.name()));
        write!(f, "Callable({})", name)
      }
      LoxValueKind::Nil => write!(f, "nil"),
      LoxValueKind::Class(c) => {
        let name = INTERNER.with_borrow(|i| i.get(c.name));
        write!(f, "Class({})", name)
      }
      LoxValueKind::Object(o) => {
        write!(f, "Object({})", &o.borrow().class)
      }
    }
  }
}

impl std::fmt::Debug for LoxValueKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
      Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
      Self::Boolean(arg0) => f.debug_tuple("Boolean").field(arg0).finish(),
      Self::Callable(_arg0) => f.debug_tuple("Callable").finish(),
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
      LoxValueKind::Nil => false,
      LoxValueKind::Callable(_) => true,
      LoxValueKind::Object(_) => true,
      _ => unreachable!(),
    }
  }

  pub fn type_name(&self) -> &'static str {
    match self {
      LoxValueKind::Number(_) => "Number",
      LoxValueKind::String(_) => "String",
      LoxValueKind::Boolean(_) => "Boolean",
      LoxValueKind::Callable(_) => "Callable",
      LoxValueKind::Nil => "Nil",
      LoxValueKind::Class(_) => "Class",
      LoxValueKind::Object(_) => "Object",
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
        Ok(this_ref)
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
  pub super_class: Option<Rc<LoxClass>>,
  pub methods: FxHashMap<SymbolId, Rc<Function>>,
  pub static_methods: FxHashMap<SymbolId, Rc<Function>>,
}

impl LoxClass {
  pub fn new(
    class: ClassDecl,
    super_class: Option<Rc<LoxClass>>,
    interpreter: &mut Interpreter,
  ) -> Self {
    let name = class.name();
    let super_var = INTERNER.with_borrow_mut(|i| i.intern("super"));

    let closure = if let Some(super_class) = &super_class {
      // bind "super" at class definition time.
      let _super_class_value =
        interpreter.declare_variable(super_var, LoxValueKind::Class(Rc::clone(super_class)));
      Rc::clone(&interpreter.active_scope)
    } else {
      Rc::clone(&interpreter.active_scope)
    };

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
      super_class,
      methods,
      static_methods,
    }
  }

  pub fn new_instance(
    class: Rc<LoxClass>,
    interpreter: &mut Interpreter,
    arguments: Vec<LoxValueKind>,
  ) -> Result<LoxValueKind, LoxRuntimeError> {
    let init = INTERNER.with_borrow_mut(|i| i.intern("init"));

    interpreter.with_scope(
      interpreter.active_scope.spawn_empty_child(),
      |interpreter| {
        let instance_raw = LoxInstance::new(Rc::clone(&class));

        let instance_value = LoxValueKind::Object(Rc::new(RefCell::new(instance_raw)));
        let LoxValueKind::Object(instance) = &instance_value else { unreachable!() };
        instance.borrow_mut().set_this(instance_value.clone());

        let constructor = class
          .methods
          .get(&init)
          .map(|constructor| instance.borrow().bind_this(constructor));

        // Constructor function
        if let Some(constructor) = constructor {
          constructor.call(interpreter, arguments)?;
        }

        Ok(instance_value)
      },
    )
  }

  pub fn resolve_method(&self, method_nmae: SymbolId) -> Option<Rc<Function>> {
    let mut cur = self;
    loop {
      if let Some(f) = cur.methods.get(&method_nmae) {
        return Some(Rc::clone(f));
      }
      if let Some(super_class) = &cur.super_class {
        cur = super_class;
      } else {
        return None;
      }
    }
  }
}

impl std::fmt::Display for LoxClass {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let name = INTERNER.with_borrow(|i| i.get(self.name));
    write!(f, "Class({})", name)
  }
}

#[derive(Clone, Debug)]
pub struct LoxInstance {
  pub this: LoxValueKind,
  pub class: Rc<LoxClass>,
  pub fields: FxHashMap<SymbolId, LoxValueKind>,
}

impl LoxInstance {
  /// Create a new [LoxInstance]
  pub fn new(class: Rc<LoxClass>) -> Self {
    Self {
      this: LoxValueKind::nil(),
      class,
      fields: FxHashMap::default(),
    }
  }

  pub fn set_this(&mut self, this: LoxValueKind) {
    self.this = this;
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
    let closure_with_this = closure.define(this_var, self.this.clone());

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

impl std::fmt::Display for LoxInstance {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Object({})", &self.class)
  }
}
