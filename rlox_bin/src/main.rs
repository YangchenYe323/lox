#![allow(clippy::uninlined_format_args)]

use std::path::PathBuf;

use clap::Parser;
use miette::Result;
use rlox::InterpreterDriver;
use rustyline::error::ReadlineError;
use rustyline::history::{FileHistory, History};
use rustyline::{DefaultEditor, Editor, Helper};

use colored::*;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
  pub file: Option<PathBuf>,
}

fn main() -> Result<()> {
  let Args { file } = Args::parse();
  let mut interpreter = InterpreterDriver::default();
  match file {
    Some(file) => run_file(file, &mut interpreter),
    None => run_interactive(&mut interpreter),
  }
}

fn run_file(file: PathBuf, interpreter: &mut InterpreterDriver) -> Result<()> {
  let source = std::fs::read_to_string(file.as_path()).unwrap();
  let _output = interpreter.run(&source);
  Ok(())
}

fn run_interactive(interpreter: &mut InterpreterDriver) -> Result<()> {
  let mut rl = PromptReader::default();

  loop {
    let readline = rl.readline();
    match readline {
      Ok(line) => {
        println!("{}", interpreter.run(&line).cyan().dimmed());
      }
      Err(ReadlineError::Interrupted) => {
        println!("{}", "CTRL-C".cyan().dimmed());
      }
      Err(ReadlineError::Eof) => {
        break;
      }
      Err(err) => {
        println!("Error: {:?}", err);
        break;
      }
    }
  }
  Ok(())
}

#[derive(Debug)]
pub struct PromptReader<H: Helper, I: History> {
  editor: Editor<H, I>,
  buffer: String,
  delimiters: i32,
}

impl Default for PromptReader<(), FileHistory> {
  fn default() -> Self {
    Self {
      editor: DefaultEditor::new().unwrap(),
      buffer: Default::default(),
      delimiters: Default::default(),
    }
  }
}

impl<H: Helper, I: History> PromptReader<H, I> {
  pub fn readline(&mut self) -> rustyline::Result<String> {
    let s = self.editor.readline("> ")?;
    self.buffer_and_update_delimiters(&s);
    while self.delimiters > 0 {
      let s = self.editor.readline("... ")?;
      self.buffer.push('\n');
      self.buffer_and_update_delimiters(&s);
    }
    self.delimiters = 0;
    let res = std::mem::take(&mut self.buffer);
    self.editor.add_history_entry(&res)?;
    Ok(res)
  }

  fn buffer_and_update_delimiters(&mut self, input: &str) {
    self.buffer.push_str(input);
    for c in input.chars() {
      match c {
        '(' | '{' => self.delimiters += 1,
        '}' | ')' => self.delimiters -= 1,
        _ => (),
      }
    }
  }
}
