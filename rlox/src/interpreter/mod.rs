use crate::ast::{
  facades::{
    BinaryExpr, BoolLit, Expr, ExprStmt, NilLit, NumericLit, PrintStmt, Program, Stmt, StringLit,
    TernaryExpr, UnaryExpr,
  },
  visit::AstVisitor,
};

use self::{
  diagnostics::{SpannedLoxRuntimeError, SpannedLoxRuntimeErrorWrapper},
  eval::{BinaryEval, UnaryEval},
  types::LoxValueKind,
};

mod diagnostics;
mod eval;
mod types;

pub struct Evaluator {}

impl<'a> AstVisitor<'a> for Evaluator {
  type Ret = Result<LoxValueKind, SpannedLoxRuntimeError>;

  fn visit_program(&mut self, program: Program<'a>) -> Self::Ret {
    let mut value = LoxValueKind::Nil;
    for stmt in program.stmts() {
      value = self.visit_statement(stmt)?;
    }
    Ok(value)
  }

  fn visit_statement(&mut self, stmt: Stmt<'a>) -> Self::Ret {
    match stmt {
      Stmt::ExprStmt(stmt) => self.visit_expression_statement(stmt),
      Stmt::PrintStmt(stmt) => self.visit_print_statement(stmt),
    }
  }

  fn visit_expression_statement(&mut self, expr_stmt: ExprStmt<'a>) -> Self::Ret {
    let expr = self.visit_expression(expr_stmt.expr())?;
    Ok(expr)
  }

  fn visit_print_statement(&mut self, print_stmt: PrintStmt<'a>) -> Self::Ret {
    let expr = self.visit_expression(print_stmt.expr())?;
    println!("{}", expr);
    Ok(LoxValueKind::Nil)
  }

  fn visit_expression(&mut self, expr: Expr<'a>) -> Self::Ret {
    match expr {
      Expr::Ternary(e) => self.visit_ternary_expression(e),
      Expr::Binary(e) => self.visit_binary_expression(e),
      Expr::Unary(e) => self.visit_unary_expression(e),
      Expr::String(e) => self.visit_string_literal(e),
      Expr::Number(e) => self.visit_numeric_literal(e),
      Expr::Bool(e) => self.visit_bool_literal(e),
      Expr::Nil(e) => self.visit_nil(e),
    }
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

  fn visit_nil(&mut self, _nil: NilLit<'a>) -> Self::Ret {
    Ok(LoxValueKind::Nil)
  }
}
