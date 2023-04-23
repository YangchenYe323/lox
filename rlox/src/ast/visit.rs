use super::facades::{
  BinaryExpr, BoolLit, Expr, NilLit, NumericLit, StringLit, TernaryExpr, UnaryExpr,
};

pub trait AstVisitor<'a> {
  type Ret;

  fn visit_expression(&mut self, expr: Expr<'a>) -> Self::Ret;

  fn visit_binary_expression(&mut self, binary_expr: BinaryExpr<'a>) -> Self::Ret;

  fn visit_ternary_expression(&mut self, ternary_expr: TernaryExpr<'a>) -> Self::Ret;

  fn visit_unary_expression(&mut self, unary_expr: UnaryExpr<'a>) -> Self::Ret;

  fn visit_string_literal(&mut self, string_literal: StringLit<'a>) -> Self::Ret;

  fn visit_numeric_literal(&mut self, numeric_literal: NumericLit<'a>) -> Self::Ret;

  fn visit_bool_literal(&mut self, bool_literal: BoolLit<'a>) -> Self::Ret;

  fn visit_nil(&mut self, nil: NilLit<'a>) -> Self::Ret;
}
