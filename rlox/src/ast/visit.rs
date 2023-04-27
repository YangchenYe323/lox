use super::facades::{
  AssignExpr, BinaryExpr, Block, BoolLit, BreakStmt, Expr, ExprStmt, IfStmt, LogicExpr, NilLit,
  NumericLit, PrintStmt, Program, Stmt, StringLit, TernaryExpr, UnaryExpr, Var, VarDecl, WhileStmt,
};

pub trait AstVisitor<'a> {
  type Ret;

  fn visit_program(&mut self, program: Program<'a>) -> Self::Ret;

  fn visit_variable_declaration(&mut self, var_decl: VarDecl<'a>) -> Self::Ret;

  fn visit_statement(&mut self, stmt: Stmt<'a>) -> Self::Ret;

  fn visit_break_statement(&mut self, break_stmt: BreakStmt<'a>) -> Self::Ret;

  fn visit_expression_statement(&mut self, expr_stmt: ExprStmt<'a>) -> Self::Ret;

  fn visit_print_statement(&mut self, print_stmt: PrintStmt<'a>) -> Self::Ret;

  fn visit_block(&mut self, block: Block<'a>) -> Self::Ret;

  fn visit_while_statement(&mut self, while_stmt: WhileStmt<'a>) -> Self::Ret;

  fn visit_if_statement(&mut self, if_stmt: IfStmt<'a>) -> Self::Ret;

  fn visit_expression(&mut self, expr: Expr<'a>) -> Self::Ret;

  fn visit_assignment_expression(&mut self, expr: AssignExpr<'a>) -> Self::Ret;

  fn visit_binary_expression(&mut self, binary_expr: BinaryExpr<'a>) -> Self::Ret;

  fn visit_logic_expression(&mut self, logic_expr: LogicExpr<'a>) -> Self::Ret;

  fn visit_ternary_expression(&mut self, ternary_expr: TernaryExpr<'a>) -> Self::Ret;

  fn visit_unary_expression(&mut self, unary_expr: UnaryExpr<'a>) -> Self::Ret;

  fn visit_string_literal(&mut self, string_literal: StringLit<'a>) -> Self::Ret;

  fn visit_numeric_literal(&mut self, numeric_literal: NumericLit<'a>) -> Self::Ret;

  fn visit_bool_literal(&mut self, bool_literal: BoolLit<'a>) -> Self::Ret;

  fn visit_var_reference(&mut self, var_reference: Var<'a>) -> Self::Ret;

  fn visit_nil(&mut self, nil: NilLit<'a>) -> Self::Ret;
}
