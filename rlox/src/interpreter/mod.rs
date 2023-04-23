use crate::ast::{
  facades::{BinaryExpr, BoolLit, Expr, NilLit, NumericLit, StringLit, TernaryExpr, UnaryExpr},
  visit::AstVisitor,
};

use self::{
  eval::{BinaryEval, UnaryEval},
  types::{LoxValue, LoxValueKind},
};

mod diagnostics;
mod eval;
mod types;

pub struct Evaluator {}

impl<'a> AstVisitor<'a> for Evaluator {
  type Ret = LoxValue;

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
    let left_operand = self.visit_expression(binary_expr.left_operand());
    let right_operand = self.visit_expression(binary_expr.right_operand());
    let value = op.evaluate(&left_operand.value, &right_operand.value);
    LoxValue {
      node: binary_expr.ast_node_id(),
      value,
    }
  }

  fn visit_ternary_expression(&mut self, ternary_expr: TernaryExpr<'a>) -> Self::Ret {
    let test = self.visit_expression(ternary_expr.predicate());
    let node = ternary_expr.ast_node_id();
    let value = match test.value.is_truthful() {
      true => self.visit_expression(ternary_expr.consequence()).value,
      false => self.visit_expression(ternary_expr.alternative()).value,
    };
    LoxValue { node, value }
  }

  fn visit_string_literal(&mut self, string_literal: StringLit<'a>) -> Self::Ret {
    let node = string_literal.ast_node_id();
    let value = LoxValueKind::String(string_literal.value().to_string());
    LoxValue { node, value }
  }

  fn visit_numeric_literal(&mut self, numeric_literal: NumericLit<'a>) -> Self::Ret {
    let node = numeric_literal.ast_node_id();
    let value = LoxValueKind::Number(numeric_literal.value());
    LoxValue { node, value }
  }

  fn visit_unary_expression(&mut self, unary_expr: UnaryExpr<'a>) -> Self::Ret {
    let node = unary_expr.ast_node_id();
    let op = unary_expr.operator();
    let operand = self.visit_expression(unary_expr.arg());
    let value = op.evaluate(&operand.value);
    LoxValue { node, value }
  }

  fn visit_bool_literal(&mut self, bool_literal: BoolLit<'a>) -> Self::Ret {
    let node = bool_literal.ast_node_id();
    let value = LoxValueKind::Boolean(bool_literal.value());
    LoxValue { node, value }
  }

  fn visit_nil(&mut self, nil: NilLit<'a>) -> Self::Ret {
    let node = nil.ast_node_id();
    let value = LoxValueKind::Nil;
    LoxValue { node, value }
  }
}
