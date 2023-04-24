use crate::ast::{
  facades::{
    AssignExpr, BinaryExpr, BoolLit, Expr, ExprStmt, NilLit, NumericLit, PrintStmt, Program, Stmt,
    StringLit, TernaryExpr, UnaryExpr, Var, VarDecl,
  },
  visit::AstVisitor,
};

use self::{
  diagnostics::{LoxRuntimeError, SpannedLoxRuntimeError, SpannedLoxRuntimeErrorWrapper},
  eval::{BinaryEval, UnaryEval},
  runtime::Environment,
  types::LoxValueKind,
};

mod diagnostics;
mod eval;
mod runtime;
mod types;

#[derive(Debug, Default)]
pub struct Evaluator {
  environment: Environment,
}

impl<'a> AstVisitor<'a> for Evaluator {
  type Ret = Result<LoxValueKind, SpannedLoxRuntimeError>;

  fn visit_program(&mut self, program: Program<'a>) -> Self::Ret {
    let mut value = LoxValueKind::nil();
    for stmt in program.stmts() {
      value = self.visit_statement(stmt)?;
    }
    Ok(value)
  }

  fn visit_statement(&mut self, stmt: Stmt<'a>) -> Self::Ret {
    match stmt {
      Stmt::Expr(stmt) => self.visit_expression_statement(stmt),
      Stmt::Print(stmt) => self.visit_print_statement(stmt),
      Stmt::VarDecl(stmt) => self.visit_variable_declaration(stmt),
    }
  }

  fn visit_variable_declaration(&mut self, var_decl: VarDecl<'a>) -> Self::Ret {
    let symbol = var_decl.var_symbol();
    let init = if let Some(expr) = var_decl.init_expr() {
      self.visit_expression(expr)?
    } else {
      LoxValueKind::nil()
    };
    self.environment.define(symbol, init);
    Ok(LoxValueKind::nil())
  }

  fn visit_expression_statement(&mut self, expr_stmt: ExprStmt<'a>) -> Self::Ret {
    let expr = self.visit_expression(expr_stmt.expr())?;
    Ok(expr)
  }

  fn visit_print_statement(&mut self, print_stmt: PrintStmt<'a>) -> Self::Ret {
    let expr = self.visit_expression(print_stmt.expr())?;
    println!("{}", expr);
    Ok(LoxValueKind::nil())
  }

  fn visit_expression(&mut self, expr: Expr<'a>) -> Self::Ret {
    match expr {
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

  fn visit_assignment_expression(&mut self, expr: AssignExpr<'a>) -> Self::Ret {
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

  fn visit_binary_expression(&mut self, binary_expr: BinaryExpr<'a>) -> Self::Ret {
    let op = binary_expr.operator();
    let left_operand = self.visit_expression(binary_expr.left_operand())?;
    let right_operand = self.visit_expression(binary_expr.right_operand())?;
    op.evaluate(&left_operand, &right_operand)
      .map_err(|e| binary_expr.wrap(e))
  }

  fn visit_ternary_expression(&mut self, ternary_expr: TernaryExpr<'a>) -> Self::Ret {
    let test = self.visit_expression(ternary_expr.predicate())?;
    match test.is_truthful() {
      true => self.visit_expression(ternary_expr.consequence()),
      false => self.visit_expression(ternary_expr.alternative()),
    }
  }

  fn visit_string_literal(&mut self, string_literal: StringLit<'a>) -> Self::Ret {
    Ok(LoxValueKind::String(string_literal.value().to_string()))
  }

  fn visit_numeric_literal(&mut self, numeric_literal: NumericLit<'a>) -> Self::Ret {
    Ok(LoxValueKind::Number(numeric_literal.value()))
  }

  fn visit_unary_expression(&mut self, unary_expr: UnaryExpr<'a>) -> Self::Ret {
    let op = unary_expr.operator();
    let operand = self.visit_expression(unary_expr.arg())?;
    op.evaluate(&operand).map_err(|e| unary_expr.wrap(e))
  }

  fn visit_bool_literal(&mut self, bool_literal: BoolLit<'a>) -> Self::Ret {
    Ok(LoxValueKind::Boolean(bool_literal.value()))
  }

  fn visit_var_reference(&mut self, var_reference: Var<'a>) -> Self::Ret {
    let reference = var_reference.var_symbol();
    self
      .environment
      .get_rvalue(reference)
      .ok_or_else(|| var_reference.wrap(LoxRuntimeError::UnresolvedReference))
  }

  fn visit_nil(&mut self, _nil: NilLit<'a>) -> Self::Ret {
    Ok(LoxValueKind::nil())
  }
}
