use assert_cmd::cargo::CommandCargoExt;
use std::{
  path::{Path, PathBuf},
  process::Command,
};

const TEST_ROOT: &str = "lox_files";

fn interpret_lox_file(path: impl AsRef<Path>) -> String {
  let path = path.as_ref();
  let mut interpreter = Command::cargo_bin("rlox").unwrap();
  let output = interpreter.arg(path).output().unwrap();
  String::from_utf8(output.stdout).unwrap()
}

fn test_source(name: &'static str) {
  let path = PathBuf::from(TEST_ROOT).join(format!("{name}.lox"));
  let output = interpret_lox_file(&path);
  insta::assert_snapshot!(name, output);
}

// Calls test_soruce on each respective file under `TEST_ROOT` directory
rlox_macros::declare_output_tests! {
  conditionals::if1,
  loops::nested_while,
  loops::simple_while,
}
