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
