#[derive(Debug, Clone)]
pub enum Value {
  Number(f64),
}

impl std::fmt::Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Value::Number(n) => write!(f, "{}", n),
    }
  }
}

#[derive(Debug, Default)]
pub struct ValueArray(Vec<Value>);

impl ValueArray {
  pub fn value_at<T>(&self, offset: T) -> Value
  where
    usize: From<T>,
  {
    self.0[usize::from(offset)].clone()
  }

  pub fn push(&mut self, value: Value) {
    self.0.push(value);
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }
}
