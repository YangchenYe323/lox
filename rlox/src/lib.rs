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
mod lexer;
mod parser;

use std::cell::RefCell;

use common::symbol::Interner;
use miette::{GraphicalReportHandler, Result};

use crate::{
  ast::facades::{AstNodePtr, Expr},
  lexer::Lex,
  parser::Parser,
};

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
    println!("{:?}", source);

    let lex = lexer::lex_source(source);

    match lex {
      Lex::Success(tokens) => {
        for token in &tokens {
          print!("{} ", token);
        }
        println!();
        INTERNER.with_borrow(|it| println!("{:?}", it));

        let mut parser = Parser::new(tokens);
        let expr = parser.expression();
        {
          let arena = parser.arena();
          let ptr = AstNodePtr::new(arena, expr);
          let expression = Expr::new(ptr);
          let s = serde_json::to_string_pretty(&expression).unwrap();
          println!("{}", s);
        }
      }
      Lex::Failure(reports) => {
        for report in reports {
          let mut out = String::new();
          let report = report.with_source_code(source.to_string());
          self
            .reporter
            .render_report(&mut out, report.as_ref())
            .unwrap();
          println!("{}", out);
        }
      }
    }

    Ok(())
  }
}
