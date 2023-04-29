use std::rc::Rc;

use bitflags::bitflags;

use rlox_ast::{
  facades::{
    AssignExpr, AssignTarget, BinaryExpr, Block, BoolLit, BreakStmt, CallExpr, ClassDecl, Expr,
    ExprStmt, FnDecl, IfStmt, LogicExpr, MemberExpr, NilLit, NumericLit, Program, ReturnStmt, Stmt,
    StringLit, SuperExpr, TernaryExpr, UnaryExpr, Var, VarDecl, WhileStmt,
  },
  visit::AstVisitor,
  LogicalOp, INTERNER,
};
use rlox_span::SymbolId;
use rustc_hash::FxHashMap;

use self::{
  builtin_functions::{builtin_get_object_id, builtin_heap, builtin_print, builtin_time},
  diagnostics::{LoxRuntimeError, SpannedLoxRuntimeError, SpannedLoxRuntimeErrorWrapper, no_such_property},
  eval::{logical_and, logical_or, BinaryEval, UnaryEval},
  objprint::Printable,
  runtime::Environment,
  scope::Scope,
  types::{Function, LoxCallable, LoxClass, LoxValueKind, ObjectId},
};

mod builtin_functions;
mod diagnostics;
mod eval;
mod objprint;
mod runtime;
mod scope;
mod types;

/// [Interpreter] manages all the runtime states of a intepreting session, including memory environment,
/// symbol resolution and an output buffer. It is responsible for interpreting parsed AST constructs.
pub struct Interpreter {
  environment: Environment,
  context: ExecutionContext,
  active_scope: Rc<Scope>,
  output: String,
}

impl Default for Interpreter {
  fn default() -> Self {
    Self::new()
  }
}

impl Interpreter {
  pub fn new() -> Self {
    let context = ExecutionContext::default();
    let (environment, active_scope) = setup_globals();
    Self {
      environment,
      context,
      active_scope,
      output: String::new(),
    }
  }

  pub fn value_to_string(&self, value: &LoxValueKind) -> String {
    value.to_string(&self.environment)
  }

  pub fn drain_output(&mut self) -> String {
    std::mem::take(&mut self.output)
  }
}

impl AstVisitor for Interpreter {
  type Ret = Result<LoxValueKind, SpannedLoxRuntimeError>;

  fn visit_program(&mut self, program: Program) -> Self::Ret {
    let mut value = LoxValueKind::nil();
    for stmt in program.stmts() {
      value = self.visit_statement(stmt)?;
    }

    Ok(value)
  }

  fn visit_statement(&mut self, stmt: Stmt) -> Self::Ret {
    match stmt {
      Stmt::Expr(stmt) => self.visit_expression_statement(stmt),
      Stmt::VarDecl(stmt) => self.visit_variable_declaration(stmt),
      Stmt::Block(stmt) => self.visit_block(stmt),
      Stmt::If(stmt) => self.visit_if_statement(stmt),
      Stmt::While(stmt) => self.visit_while_statement(stmt),
      Stmt::Break(stmt) => self.visit_break_statement(stmt),
      Stmt::FnDecl(stmt) => self.visit_function_declaration(stmt),
      Stmt::Return(stmt) => self.visit_return_statement(stmt),
      Stmt::ClassDecl(stmt) => self.visit_class_declaration(stmt),
    }
  }

  fn visit_break_statement(&mut self, _break_stmt: BreakStmt) -> Self::Ret {
    self.context |= ExecutionContext::BREAK;
    Ok(LoxValueKind::nil())
  }

  fn visit_return_statement(&mut self, return_stmt: ReturnStmt) -> Self::Ret {
    let value = self.visit_expression(return_stmt.returned_expr())?;
    self.context |= ExecutionContext::RETURN;
    Ok(value)
  }

  fn visit_while_statement(&mut self, while_stmt: WhileStmt) -> Self::Ret {
    self.with_context(ExecutionContext::IN_LOOP, |evaluator| {
      let pred = while_stmt.pred();
      let body = while_stmt.body();
      while evaluator.visit_expression(pred)?.is_truthful() {
        evaluator.visit_statement(body)?;
        if evaluator.to_break_from_loop() {
          break;
        }
      }
      Ok(LoxValueKind::nil())
    })
  }

  fn visit_if_statement(&mut self, if_stmt: IfStmt) -> Self::Ret {
    let test = self.visit_expression(if_stmt.pred())?;
    if test.is_truthful() {
      self.visit_statement(if_stmt.then_block())
    } else {
      self.visit_statement(if_stmt.else_block())
    }
  }

  fn visit_variable_declaration(&mut self, var_decl: VarDecl) -> Self::Ret {
    let symbol = var_decl.var_symbol();
    let init = self.visit_expression(var_decl.init_expr())?;
    self.declare_variable(symbol, init);
    Ok(LoxValueKind::nil())
  }

  fn visit_class_declaration(&mut self, class_decl: ClassDecl) -> Self::Ret {
    let name = class_decl.name();
    let object = self.declare_variable(name, LoxValueKind::nil());

    // Resolve inheritance
    let super_class = if let Some(parent) = class_decl.superclass() {
      let object = self
        .active_scope
        .get_lvalue_symbol(parent)
        .ok_or_else(|| class_decl.wrap(LoxRuntimeError::UnresolvedReference))?;
      match self.environment.get_rvalue(object) {
        LoxValueKind::Class(c) => Some(c),
        value => return Err(class_decl.wrap(LoxRuntimeError::InvalidInherit(value.type_name()))),
      }
    } else {
      None
    };

    let class = LoxClass::new(class_decl, super_class, self);
    self
      .environment
      .assign(object, LoxValueKind::Class(Rc::new(class)));
    Ok(LoxValueKind::nil())
  }

  fn visit_function_declaration(&mut self, fn_decl: FnDecl) -> Self::Ret {
    // This ensures that the function can find itself in the closure to enable recursion.
    let name = fn_decl.name();
    let object = self.declare_variable(name, LoxValueKind::nil());
    let closure = Rc::clone(&self.active_scope);

    let function = Function::new(false, fn_decl, closure);
    self
      .environment
      .assign(object, LoxValueKind::Callable(Rc::new(function)));
    Ok(LoxValueKind::nil())
  }

  fn visit_expression_statement(&mut self, expr_stmt: ExprStmt) -> Self::Ret {
    let expr = self.visit_expression(expr_stmt.expr())?;
    Ok(expr)
  }

  fn visit_block(&mut self, block: Block) -> Self::Ret {
    self.with_scope(self.active_scope.spawn_empty_child(), |evaluator| {
      let mut value = LoxValueKind::nil();
      for stmt in block.statements() {
        value = evaluator.visit_statement(stmt)?;
        if evaluator.to_break_from_loop() | evaluator.to_return() {
          break;
        }
      }

      // When block is invoked in function, don't implicitly return the last expression
      if !evaluator.to_return() && evaluator.in_function() {
        return Ok(LoxValueKind::nil());
      }

      Ok(value)
    })
  }

  fn visit_expression(&mut self, expr: Expr) -> Self::Ret {
    match expr {
      Expr::Call(e) => self.visit_call_expression(e),
      Expr::Logic(e) => self.visit_logic_expression(e),
      Expr::Assign(e) => self.visit_assignment_expression(e),
      Expr::Ternary(e) => self.visit_ternary_expression(e),
      Expr::Binary(e) => self.visit_binary_expression(e),
      Expr::Unary(e) => self.visit_unary_expression(e),
      Expr::String(e) => self.visit_string_literal(e),
      Expr::Number(e) => self.visit_numeric_literal(e),
      Expr::Bool(e) => self.visit_bool_literal(e),
      Expr::Var(e) => self.visit_var_reference(e),
      Expr::Nil(e) => self.visit_nil(e),
      Expr::Member(e) => self.visit_member_expression(e),
      Expr::Super(e) => self.visit_super_expression(e),
    }
  }

  fn visit_super_expression(&mut self, _super_expr: SuperExpr) -> Self::Ret {
    // Taking a fast route here, super refers to the same object as this, but using the parent class
    // instead of the direct class
    let object = self
      .active_scope
      .get_lvalue_symbol(INTERNER.with_borrow_mut(|i| i.intern("this")))
      .unwrap();
    // Has to be an object id
    let object = self.environment.get_rvalue(object);
    Ok(object)
  }

  fn visit_member_expression(&mut self, member_expr: MemberExpr) -> Self::Ret {
    let object = self.visit_expression(member_expr.object())?;

    // static method access: A.foo()
    if let LoxValueKind::Class(c) = object {
      return c
        .static_methods
        .get(&member_expr.property())
        .map(|f| Ok(LoxValueKind::Callable(Rc::clone(f) as Rc<dyn LoxCallable>)))
        .unwrap_or_else(|| {
          let name = INTERNER.with_borrow(|i| i.get(member_expr.property()));
          Err(member_expr.wrap(LoxRuntimeError::NoSuchProperty(name)))
        });
    }

    let is_super = matches!(member_expr.object(), Expr::Super(_));
    let LoxValueKind::ObjectId(id) = object else {
      return Err(member_expr.wrap(LoxRuntimeError::InvalidMemberAccess(object.type_name())));
    };
    let LoxValueKind::Object(object) = self.environment.get_rvalue(id) else { unreachable!() };

    // Search in property
    if let Some(property) = object.fields.get(&member_expr.property()) {
      Ok(self.environment.get_rvalue(*property))
    } else {
      let method = if is_super {
        let super_var = INTERNER.with_borrow_mut(|i| i.intern("super"));
        let super_class = self.active_scope.get_lvalue_symbol(super_var).ok_or_else(|| member_expr.wrap(LoxRuntimeError::NoSuper))?;
        let LoxValueKind::Class(c) = self.environment.get_rvalue(super_class) else { unreachable!() };
        c.resolve_method(member_expr.property())
      } else {
        object.class.resolve_method(member_expr.property())
      }.ok_or_else(|| member_expr.wrap(no_such_property(member_expr.property())))?;

      Ok(LoxValueKind::Callable(object.bind_this(&method)))
    }
  }

  fn visit_call_expression(&mut self, call_expr: CallExpr) -> Self::Ret {
    self.with_context(ExecutionContext::IN_FUNCTION, |evaluator| {
      let callee = evaluator.visit_expression(call_expr.callee())?;
      match callee {
        LoxValueKind::Callable(c) => {
          let mut arguments = vec![];
          for arg in call_expr.argument_list().arguments() {
            arguments.push(evaluator.visit_expression(arg)?);
          }
          c.call(evaluator, arguments).map_err(|e| call_expr.wrap(e))
        }
        LoxValueKind::Class(c) => {
          let mut arguments = vec![];
          for arg in call_expr.argument_list().arguments() {
            arguments.push(evaluator.visit_expression(arg)?);
          }
          LoxClass::new_instance(c, evaluator, arguments)
            .map(LoxValueKind::ObjectId)
            .map_err(|e| call_expr.wrap(e))
        }
        c => Err(call_expr.wrap(LoxRuntimeError::InalidCall(c.type_name()))),
      }
    })
  }

  fn visit_assignment_expression(&mut self, expr: AssignExpr) -> Self::Ret {
    let target = expr.target();
    let value = expr.value();
    let target = self.resolve_lvalue(target).map_err(|e| expr.wrap(e))?;
    let value = self.visit_expression(value)?;
    self.environment.assign(target, value.clone());
    Ok(value)
  }

  fn visit_binary_expression(&mut self, binary_expr: BinaryExpr) -> Self::Ret {
    let op = binary_expr.operator();

    let left_operand = self.visit_expression(binary_expr.left_operand())?;
    let right_operand = self.visit_expression(binary_expr.right_operand())?;
    op.evaluate(&left_operand, &right_operand)
      .map_err(|e| binary_expr.wrap(e))
  }

  fn visit_logic_expression(&mut self, logic_expr: LogicExpr) -> Self::Ret {
    let op = logic_expr.operator();
    let left = logic_expr.left_operand();
    let right = logic_expr.right_operand();
    match op {
      LogicalOp::And => logical_and(self, left, right),
      LogicalOp::Or => logical_or(self, left, right),
    }
  }

  fn visit_ternary_expression(&mut self, ternary_expr: TernaryExpr) -> Self::Ret {
    let test = self.visit_expression(ternary_expr.predicate())?;
    match test.is_truthful() {
      true => self.visit_expression(ternary_expr.consequence()),
      false => self.visit_expression(ternary_expr.alternative()),
    }
  }

  fn visit_string_literal(&mut self, string_literal: StringLit) -> Self::Ret {
    Ok(LoxValueKind::String(string_literal.value().to_string()))
  }

  fn visit_numeric_literal(&mut self, numeric_literal: NumericLit) -> Self::Ret {
    Ok(LoxValueKind::Number(numeric_literal.value()))
  }

  fn visit_unary_expression(&mut self, unary_expr: UnaryExpr) -> Self::Ret {
    let op = unary_expr.operator();
    let operand = self.visit_expression(unary_expr.arg())?;
    op.evaluate(&operand).map_err(|e| unary_expr.wrap(e))
  }

  fn visit_bool_literal(&mut self, bool_literal: BoolLit) -> Self::Ret {
    Ok(LoxValueKind::Boolean(bool_literal.value()))
  }

  fn visit_var_reference(&mut self, var_reference: Var) -> Self::Ret {
    let reference = var_reference.var_symbol();
    let lvalue = self
      .active_scope
      .get_lvalue_symbol(reference)
      .ok_or_else(|| var_reference.wrap(LoxRuntimeError::UnresolvedReference))?;
    Ok(self.environment.get_rvalue(lvalue))
  }

  fn visit_nil(&mut self, _nil: NilLit) -> Self::Ret {
    Ok(LoxValueKind::nil())
  }
}

bitflags! {
  #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
  struct ExecutionContext: u32 {
    const IN_LOOP = 1 << 0;
    const IN_FUNCTION = 1 << 1;
    const BREAK = 1 << 2;
    const RETURN = 1 << 3;
  }
}

impl Interpreter {
  fn with_context<T>(
    &mut self,
    context: ExecutionContext,
    f: impl FnOnce(&mut Interpreter) -> T,
  ) -> T {
    let old_context = self.context;
    self.context = old_context | context;
    let result = f(self);
    self.context = old_context;
    result
  }

  fn declare_variable(&mut self, var: SymbolId, value: LoxValueKind) -> ObjectId {
    let object = self.environment.new_object();
    self.environment.assign(object, value);
    self.active_scope = Rc::new(self.active_scope.define(var, object));
    object
  }

  fn with_scope<T>(&mut self, new_scope: Rc<Scope>, f: impl FnOnce(&mut Interpreter) -> T) -> T {
    let old_scope = Rc::clone(&self.active_scope);
    self.active_scope = new_scope;
    let result = f(self);
    self.active_scope = old_scope;
    result
  }

  fn to_break_from_loop(&self) -> bool {
    self
      .context
      .contains(ExecutionContext::IN_LOOP | ExecutionContext::BREAK)
  }

  fn resolve_lvalue(&mut self, target: AssignTarget) -> Result<ObjectId, LoxRuntimeError> {
    match target {
      AssignTarget::Ident(var) => self
        .active_scope
        .get_lvalue_symbol(var.var_symbol())
        .ok_or_else(|| LoxRuntimeError::UnresolvedReference),
      AssignTarget::Member(member) => {
        let object = self
          .visit_expression(member.object())
          .map_err(LoxRuntimeError::from)?;

        // We can't do "string".c or 1.c
        let LoxValueKind::ObjectId(id) = object else {
          return Err(LoxRuntimeError::InvalidMemberAccess(object.type_name()));
        };
        let LoxValueKind::Object(mut object) = self.environment.get_rvalue(id) else {
          unreachable!()
        };

        let property = object
          .fields
          .entry(member.property())
          .or_insert_with(|| self.environment.new_object());
        let property = *property;
        self.environment.assign(id, LoxValueKind::Object(object));

        Ok(property)
      }
    }
  }

  fn to_return(&self) -> bool {
    self
      .context
      .contains(ExecutionContext::IN_FUNCTION | ExecutionContext::RETURN)
  }

  fn in_function(&self) -> bool {
    self.context.contains(ExecutionContext::IN_FUNCTION)
  }
}

fn setup_globals() -> (Environment, Rc<Scope>) {
  let mut env = Environment::default();
  let mut symbols = FxHashMap::default();
  populate_global(&mut env, &mut symbols, "time", builtin_time);
  populate_global(&mut env, &mut symbols, "print", builtin_print);
  populate_global(&mut env, &mut symbols, "heap", builtin_heap);
  populate_global(&mut env, &mut symbols, "object_id", builtin_get_object_id);
  (env, Rc::new(Scope::new(symbols, None)))
}

fn populate_global(
  environment: &mut Environment,
  symbols: &mut FxHashMap<SymbolId, ObjectId>,
  name: &'static str,
  value: impl FnOnce(SymbolId) -> LoxValueKind,
) {
  let symbol = INTERNER.with_borrow_mut(|interner| interner.intern(name));
  let object = environment.new_object();
  symbols.insert(symbol, object);
  environment.assign(object, value(symbol));
}
