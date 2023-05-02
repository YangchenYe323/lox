/// [LineBuffer] implements a run-length compressed buffer to store the line number of each
/// instructions.
/// Internally it maintains a vector, where the even index stores line numbers and the odd index stores the last index
/// of instructions which have the same line number.
///
/// For example,
/// [0, 2, 1, 4, 2, 8] means:
/// instruction [0..2] has line number 0
/// instruction [2..4] has line number 1
/// instruction [4..8] has line number 2
#[derive(Debug, Default)]
pub struct LineBuffer(Vec<u32>);

impl LineBuffer {
  pub fn push(&mut self, line: u32) {
    if self.len() >= 2 && line == self.0[self.len() - 2] {
      *self.0.last_mut().unwrap() += 1;
    } else {
      let last_idx = if self.len() > 0 {
        *self.0.last().unwrap()
      } else {
        0
      };
      self.0.push(line);
      self.0.push(last_idx + 1);
    }
  }

  pub fn get_lineno(&self, idx: u32) -> u32 {
    for i in (1..self.len()).step_by(2) {
      if self.0[i] > idx {
        return self.0[i - 1];
      }
    }

    panic!(
      "Index out of bound: {}, length is {}",
      idx,
      self.0.last().unwrap()
    )
  }

  #[inline]
  fn len(&self) -> usize {
    self.0.len()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn getline() {
    let mut buffer = LineBuffer::default();
    let lines = [
      0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 6, 7, 9, 9, 9, 9, 20, 20,
    ];
    for l in &lines {
      buffer.push(*l);
    }
    for (idx, l) in lines.into_iter().enumerate() {
      assert_eq!(l, buffer.get_lineno(idx as u32));
    }
  }

  #[test]
  fn memory_usage() {
    let mut buffer = LineBuffer::default();
    let lines = [
      0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 6, 7, 9, 9, 9, 9, 20, 20,
    ];
    for l in &lines {
      buffer.push(*l);
    }
    assert_eq!(20, buffer.len());
  }
}
