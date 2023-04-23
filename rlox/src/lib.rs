#![warn(rust_2018_idioms)]
#![allow(clippy::uninlined_format_args, clippy::mut_from_ref)]
#![feature(
  dropck_eyepatch,
  new_uninit,
  maybe_uninit_slice,
  strict_provenance,
  ptr_alignment_type,
  local_key_cell_methods
)]

//! This crate implements a tree-walking interpreter in rust from [crafting-interpreters](https://craftinginterpreters.com/)

mod ast;
mod common;
mod interpreter;
mod lexer;
mod parser;

use std::cell::RefCell;

use common::symbol::Interner;
use miette::{Diagnostic, GraphicalReportHandler, Report, Result};

use crate::{
  ast::{facades::Expr, visit::AstVisitor},
  interpreter::Evaluator,
  parser::{parse_source, Parse},
};

// Global variables in the parsing context.
// We don't support multi-threaded parser so a thread local is enough.
std::thread_local! {
  pub static INTERNER: RefCell<Interner>  = RefCell::new(Interner::default());
}

/// The interpreter that handles interpreting and executing source code.
#[derive(Debug)]
pub struct Interpreter {
  reporter: GraphicalReportHandler,
}

impl Default for Interpreter {
  fn default() -> Self {
    Self::new()
  }
}

impl Interpreter {
  pub fn new() -> Self {
    Self {
      reporter: GraphicalReportHandler::new(),
    }
  }

  /// Interprete and run a lox source string
  pub fn run(&mut self, source: &'_ str) -> Result<()> {
    let parse_result = parse_source(source);

    match parse_result {
      Parse::Success(syntax_tree) => {
        let ptr = syntax_tree.root_ptr();
        let expr = Expr::new(ptr);
        let mut evaluator = Evaluator {};
        let result = evaluator.visit_expression(expr);
        match result {
          Ok(value) => println!("{}", value),
          Err(error) => self.report_error(error, source),
        }
      }
      Parse::ParseError {
        recovered,
        unrecoverrable,
      } => {
        for error in recovered {
          self.report_error(error, source)
        }
        if let Some(err) = unrecoverrable {
          self.report_error(err, source)
        }
      }
      Parse::LexError(errors) => {
        for error in errors {
          self.report_error(error, source)
        }
      }
    }

    Ok(())
  }

  fn report_error<T: Diagnostic + Send + Sync + 'static>(&self, error: T, source: &str) {
    let report = Report::from(error).with_source_code(source.to_string());
    let mut buf = String::new();
    self
      .reporter
      .render_report(&mut buf, report.as_ref())
      .unwrap();
    println!("{}", buf);
  }
}
