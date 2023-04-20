
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
  start: u32,
  end: u32,
}

impl Span {
  pub fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }
}