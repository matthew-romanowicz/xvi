use std::default::Default;
use std::fmt::{Debug, Display};
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy)]
pub struct BoundedIndex<const BOUND: usize> {
    index: usize
}

impl<const BOUND: usize> BoundedIndex<BOUND> {
    pub const BOUND: usize = BOUND;

    pub fn new(index: usize) -> Result<BoundedIndex<BOUND>, usize> {
        if index < BOUND {
            Ok(BoundedIndex{index})
        } else {
            Err(BOUND)
        }
    }

    // TODO: Limit BOUND such that BOUND >= 1 so this is actually always valid
    pub fn zero() -> BoundedIndex<BOUND> {
        Self::new(0).unwrap()
    }
}

impl<const BOUND: usize> Display for BoundedIndex<BOUND> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.index)
    }
}

pub struct BoundedVec<const BOUND: usize, T> {
    inner: [T; BOUND]
}

impl<const BOUND: usize, T: Default> Default for BoundedVec<BOUND, T> {
    fn default() -> BoundedVec<BOUND, T> {
        // let mut inner = Vec::new();
        // inner.resize_with(BOUND, Default::default);

        // BoundedVec{inner: inner.try_into().unwrap()}

        BoundedVec {
            inner: std::array::from_fn(|_| Default::default())
        }
    }
}

impl<const BOUND: usize, T> BoundedVec<BOUND, T> {
    pub fn new<F>(mut f: F) -> BoundedVec<BOUND, T>
    where 
        F: FnMut() -> T 
    {

        // Vec::new();
        // inner.resize_with(BOUND, f);

        // BoundedVec{inner: inner.try_into().unwrap()}

        BoundedVec {
            inner: std::array::from_fn(|_| f())
        }
    }
}

impl<const BOUND: usize, T> Index<BoundedIndex<BOUND>> for BoundedVec<BOUND, T> {
    type Output = T;

    fn index(&self, index: BoundedIndex<BOUND>) -> &T {
        &self.inner[index.index]
    }
}

impl<const BOUND: usize, T> IndexMut<BoundedIndex<BOUND>> for BoundedVec<BOUND, T> {

    fn index_mut(&mut self, index: BoundedIndex<BOUND>) -> &mut T {
        &mut self.inner[index.index]
    }
}