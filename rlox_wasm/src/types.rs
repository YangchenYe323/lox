use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct InterpreterResult {
  success: bool,
  output: String,
}

#[wasm_bindgen]
impl InterpreterResult {
  pub fn new(success: bool, output: String) -> Self {
    Self { success, output }
  }

  #[wasm_bindgen]
  pub fn success(&self) -> bool {
    self.success
  }

  #[wasm_bindgen]
  pub fn output(&self) -> String {
    self.output.clone()
  }
}
