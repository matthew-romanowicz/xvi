use std::default::Default;
use std::fmt::{Debug, Display};
use std::ops::{Index, IndexMut};

#[derive(Clone, Copy)]
pub struct BoundedIndex<const MAX: usize> {
    index: usize
}

impl<const MAX: usize> BoundedIndex<MAX> {
    pub fn new(index: usize) -> Result<BoundedIndex<MAX>, usize> {
        if index <= MAX {
            Ok(BoundedIndex{index})
        } else {
            Err(MAX)
        }
    }
}

impl<const MAX: usize> Display for BoundedIndex<MAX> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.index)
    }
}

pub struct BoundedVec<const MAX: usize, T> {
    inner: [T; MAX]
}

impl<const MAX: usize, T: Default> Default for BoundedVec<MAX, T> {
    fn default() -> BoundedVec<MAX, T> {
        // let mut inner = Vec::new();
        // inner.resize_with(MAX, Default::default);

        // BoundedVec{inner: inner.try_into().unwrap()}

        BoundedVec {
            inner: std::array::from_fn(|_| Default::default())
        }
    }
}

impl<const MAX: usize, T> BoundedVec<MAX, T> {
    pub fn new<F>(mut f: F) -> BoundedVec<MAX, T>
    where 
        F: FnMut() -> T 
    {

        // Vec::new();
        // inner.resize_with(MAX, f);

        // BoundedVec{inner: inner.try_into().unwrap()}

        BoundedVec {
            inner: std::array::from_fn(|_| f())
        }
    }
}

impl<const MAX: usize, T> Index<BoundedIndex<MAX>> for BoundedVec<MAX, T> {
    type Output = T;

    fn index(&self, index: BoundedIndex<MAX>) -> &T {
        &self.inner[index.index]
    }
}

impl<const MAX: usize, T> IndexMut<BoundedIndex<MAX>> for BoundedVec<MAX, T> {

    fn index_mut(&mut self, index: BoundedIndex<MAX>) -> &mut T {
        &mut self.inner[index.index]
    }
}