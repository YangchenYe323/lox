use std::rc::Rc;

use bitflags::bitflags;

use rlox_ast::{
  facades::{
    AssignExpr, BinaryExpr, Block, BoolLit, BreakStmt, CallExpr, Expr, ExprStmt, FnDecl, IfStmt,
    LogicExpr, NilLit, NumericLit, Program, ReturnStmt, Stmt, StringLit, TernaryExpr, UnaryExpr,
    Var, VarDecl, WhileStmt,
  },
  visit::AstVisitor,
  LogicalOp,
};

use self::{
  diagnostics::{LoxRuntimeError, SpannedLoxRuntimeError, SpannedLoxRuntimeErrorWrapper},
  eval::{logical_and, logical_or, BinaryEval, UnaryEval},
  runtime::{populate_builtin_globals, Environment},
  types::{Function, LoxValueKind},
};

mod builtin_functions;
mod diagnostics;
mod eval;
mod runtime;
mod types;

pub struct Evaluator {
  environment: Environment,
  context: ExecutionContext,
}

impl Default for Evaluator {
  fn default() -> Self {
    Self::new()
  }
}

impl Evaluator {
  pub fn new() -> Self {
    let mut environment = Environment::default();
    environment.enter_scope();
    populate_builtin_globals(&mut environment);
    let context = ExecutionContext::default();
    Self {
      environment,
      context,
    }
  }
}

impl AstVisitor for Evaluator {
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
    self.environment.define(symbol, init);
    Ok(LoxValueKind::nil())
  }

  fn visit_function_declaration(&mut self, fn_decl: FnDecl) -> Self::Ret {
    let closure = self.environment.current_scope().clone();
    let name = fn_decl.name();
    let function = Function::new(fn_decl, closure);
    self
      .environment
      .define(name, LoxValueKind::Callable(Rc::new(function)));
    Ok(LoxValueKind::nil())
  }

  fn visit_expression_statement(&mut self, expr_stmt: ExprStmt) -> Self::Ret {
    let expr = self.visit_expression(expr_stmt.expr())?;
    Ok(expr)
  }

  fn visit_block(&mut self, block: Block) -> Self::Ret {
    self.environment.enter_scope();
    let mut value = LoxValueKind::nil();
    for stmt in block.statements() {
      value = self.visit_statement(stmt)?;
      if self.to_break_from_loop() | self.to_return() {
        break;
      }
    }
    self.environment.exit_scope();
    Ok(value)
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
        c => Err(call_expr.wrap(LoxRuntimeError::InalidCall(c.type_name()))),
      }
    })
  }

  fn visit_assignment_expression(&mut self, expr: AssignExpr) -> Self::Ret {
    let target = expr.target();
    let value = expr.value();
    let target = self
      .environment
      .get_lvalue(target)
      .ok_or_else(|| expr.wrap(LoxRuntimeError::UnresolvedReference))?;
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
    self
      .environment
      .get_rvalue(reference)
      .ok_or_else(|| var_reference.wrap(LoxRuntimeError::UnresolvedReference))
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

impl Evaluator {
  fn with_context<T>(
    &mut self,
    context: ExecutionContext,
    f: impl FnOnce(&mut Evaluator) -> T,
  ) -> T {
    let old_context = self.context;
    self.context = old_context | context;
    let result = f(self);
    self.context = old_context;
    result
  }

  fn to_break_from_loop(&self) -> bool {
    self
      .context
      .contains(ExecutionContext::IN_LOOP | ExecutionContext::BREAK)
  }

  fn to_return(&self) -> bool {
    self
      .context
      .contains(ExecutionContext::IN_FUNCTION | ExecutionContext::RETURN)
  }
}
