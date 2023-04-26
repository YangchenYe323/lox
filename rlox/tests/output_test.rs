use assert_cmd::cargo::CommandCargoExt;
use std::{
  ffi::OsStr,
  fs::{read_dir, read_to_string},
  path::{Path, PathBuf},
  process::Command,
};

const TEST_ROOT: &str = "lox_files";

#[test]
fn test_output_match() {
  let paths = read_dir(TEST_ROOT).unwrap();
  for path in paths {
    let path = path.unwrap().path();
    if is_lox_source(&path) {
      let test_name = path.as_path().as_os_str().to_str().unwrap();
      println!("Testing Lox Source: {}...", test_name);
      let expected_output = {
        let output_path = get_output_path(&path);
        read_to_string(output_path).unwrap()
      };
      let actual_output = {
        let output = Command::cargo_bin("rlox")
          .unwrap()
          .arg(&path)
          .output()
          .unwrap();
        String::from_utf8(output.stdout).unwrap()
      };
      assert_eq!(&expected_output, &actual_output);
      println!("Testing Lox Source: {}...[OK]", test_name)
    }
  }
}

fn is_lox_source(path: impl AsRef<Path>) -> bool {
  path.as_ref().extension() == Some(OsStr::new("lox"))
}

fn get_output_path(code_path: impl AsRef<Path>) -> PathBuf {
  let name = code_path
    .as_ref()
    .strip_prefix(TEST_ROOT)
    .unwrap()
    .file_stem()
    .unwrap()
    .to_str()
    .unwrap();
  let output = PathBuf::from(TEST_ROOT).join(format!("{}.out", name));
  output
}
