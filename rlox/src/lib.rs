#![warn(rust_2018_idioms, missing_debug_implementations)]

//! This crate implements a tree-walking interpreter in rust from [crafting-interpreters](https://craftinginterpreters.com/)

mod lexer;

use lexer::Lexer;
use miette::Result;

use crate::lexer::Lex;

/// The interpreter that handles interpreting and executing source code.
#[derive(Debug)]
pub struct Interpreter {
  line: u32,
}

impl Default for Interpreter {
  fn default() -> Self {
    Self::new()
  }
}

impl Interpreter {
  pub fn new() -> Self {
    Self { line: 1 }
  }

  /// Interprete and run a lox source string
  pub fn run(&mut self, source: &'_ str) -> Result<()> {
    let mut lexer = Lexer::new(source, self.line);
    println!("{:?}", source);
    lexer.scan_tokens();

    let lex = lexer.into_result();

    match lex {
      Lex::Success(tokens, line) => {
        println!("{:?}", tokens);
        self.line = line + 1;
      }
      Lex::Failure(reports) => {
        for report in reports {
          println!("{}", report);
        }
      }
    }

    Ok(())
  }
}
