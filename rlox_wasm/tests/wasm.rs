wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

use wasm_bindgen_test::*;

#[wasm_bindgen_test]
fn error_case() {
  let mut handle = rlox_wasm::InterpreterHandle::new();
  let _result = handle.interprete("a");
}

#[wasm_bindgen_test]
fn success_case() {
  let mut handle = rlox_wasm::InterpreterHandle::new();
  let _result = handle.interprete("fun foo() {return 1;} foo()");
}
