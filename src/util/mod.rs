#[derive(Debug, Clone)]
pub struct MultiLevelIndex {
    pub curr: Box<[usize]>,
    pub end: Box<[usize]>,
}

impl MultiLevelIndex {
    pub fn new(dims: Box<[usize]>) -> Self {
        Self {
            curr: vec![0; dims.len()].into_boxed_slice(),
            end: dims,
        }
    }
    pub fn from_slice(slice: &[usize]) -> Self {
        Self {
            curr: vec![0; slice.len()].into_boxed_slice(),
            end: slice.into(),
        }
    }
    pub fn from_dim_iter(iter: impl Iterator<Item = usize>) -> Self {
        let dims: Vec<usize> = iter.collect();
        Self {
            curr: vec![0; dims.len()].into_boxed_slice(),
            end: dims.into(),
        }
    }

    pub fn n_dimensions(&self) -> usize {
        self.end.len()
    }

    pub fn inc_dim(&mut self, lo: usize) -> bool {
        self.inc_dim_range(self.end.len() - 1, lo)
    }
    pub fn ends(&self) -> bool {
        self.curr[0] >= self.end[0]
    }
    pub fn skip_dim_range(&mut self, hi: usize, lo: usize) -> bool {
        if hi >= self.end.len() {
            panic!(
                "Dimension level overflow: requires (0..{}) but got {}",
                self.end.len(),
                hi
            );
        }
        if self.ends() {
            return false;
        }
        let len = self.end.len();
        for i in (hi + 1)..len {
            self.curr[i] = 0;
        }
        self.inc_dim_range(hi, lo)
    }

    fn inc_dim_range(&mut self, hi: usize, lo: usize) -> bool {
        if hi <= lo {
            panic!("Invalid range: from {} to {}", hi, lo);
        }
        if self.ends() {
            return false;
        }
        if hi >= self.end.len() {
            panic!(
                "Dimension level overflow: requires (0..{}) but got {}",
                self.end.len(),
                hi
            );
        }
        let mut level = hi;
        loop {
            let new_dim = self.curr[level] + 1;
            if new_dim < self.end[level] {
                self.curr[level] = new_dim;
                return true;
            }
            if level <= lo {
                // We are at the end of the last dimension
                return false;
            }
            self.curr[level] = 0;
            level -= 1;
        }
    }
}

/// Convert literal `r#""([^"\\]|\\.)*""#` back to string.
/// 
/// e.g. ['"', '\', 'n', '"'] => [0x10]
pub fn unparse_string_literal(literal: &str) -> String {
    let mut res = String::with_capacity(literal.len());
    let mut literal = literal.chars().skip(1);
    while let Some(c) = literal.next() {
        if c == '\\' {
            if let Some(next) = literal.next() {
                match next {
                    'n' => res.push('\n'),
                    't' => res.push('\t'),
                    'r' => res.push('\r'),
                    '0' => res.push('\0'),
                    '\\' => res.push('\\'),
                    '"' => res.push('"'),
                    _ => panic!("Invalid escape sequence: \\{}", next),
                }
            } else {
                panic!("Invalid escape sequence: \\");
            }
        } else if c == '\"' {
            // End of string literal
            break;
        } else {
            res.push(c);
        }
    }
    res
}

#[cfg(test)]
mod testing {
    use super::*;

    #[test]
    fn test_string_literal() {
        let literal = r#""Hello, world!\n""#;
        let expected = "Hello, world!\n";
        let result = unparse_string_literal(literal);
        assert_eq!(result, expected);
    }
}