#![warn(rust_2018_idioms)]
#![allow(clippy::uninlined_format_args, clippy::mut_from_ref)]
#![feature(local_key_cell_methods)]

//! This crate implements a tree-walking interpreter in rust from [crafting-interpreters](https://craftinginterpreters.com/)

mod ast;
mod interpreter;
mod lexer;
mod parser;

use std::{borrow::Cow, cell::RefCell};

use ast::{facades::Program, AstNode, SyntaxTree};
use miette::{Diagnostic, GraphicalReportHandler, Report};
use rlox_span::Interner;

use crate::{
  ast::visit::AstVisitor,
  interpreter::Evaluator,
  parser::{parse_source_program, Parse},
};

// Global variables in the parsing context.
// We don't support multi-threaded parser so a thread local is enough.
std::thread_local! {
  // Interner stores mapping from [SymbolId] -> Str of the symbol
  pub static INTERNER: RefCell<Interner>  = RefCell::new(Interner::default());
  /// Node Arena stores mapping from [AstNodeId] -> [AstNode]
  pub static NODE_ARENA: RefCell<indextree::Arena<AstNode>> = RefCell::new(indextree::Arena::new());
}

/// The interpreter that handles interpreting and executing source code.
#[derive(Default)]
pub struct Interpreter {
  reporter: GraphicalReportHandler,
  evaluator: Evaluator,
}

impl Interpreter {
  /// Interprete and run a lox source string
  pub fn run(&mut self, source: &'_ str) -> Cow<'static, str> {
    let parse_result = parse_source_program(source);

    if let Some(tree) = self.handle_parse_result(parse_result, source) {
      let ptr = tree.root_ptr();
      let program = Program::new(ptr);
      let evaluation = self.evaluator.visit_program(program);
      match evaluation {
        Ok(value) => Cow::Owned(value.to_string()),
        Err(runtime_error) => {
          self.report_error(runtime_error, source);
          Cow::Borrowed("")
        }
      }
    } else {
      Cow::Borrowed("")
    }
  }

  fn handle_parse_result(&self, parse: Parse, source: &str) -> Option<SyntaxTree> {
    match parse {
      Parse::Success(syntax_tree) => Some(syntax_tree),
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
        None
      }
      Parse::LexError(errors) => {
        for error in errors {
          self.report_error(error, source)
        }
        None
      }
    }
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
