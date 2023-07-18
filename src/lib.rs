use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

type Idx = u32;

/// The ID of an element of type `T` in an [`Arena`].
#[repr(transparent)]
pub struct Id<T> {
    index: Idx,
    _phantom: PhantomData<*const T>,
}

impl<T> Id<T> {
    #[inline]
    const fn new(index: Idx) -> Self {
        Self {
            index,
            _phantom: PhantomData,
        }
    }

    #[inline]
    const fn offset(self, offset: Idx) -> Self {
        Id::new(self.index + offset)
    }
}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Id { index, .. } => f.debug_tuple("Id").field(&index).finish(),
        }
    }
}

impl<T> Clone for Id<T> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _phantom: PhantomData,
        }
    }
}

impl<T> Copy for Id<T> {}

impl<T> std::cmp::PartialEq for Id<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> std::cmp::Eq for Id<T> {}

/// A sequence of elements in an arena. It's like a
/// slice: made up of a starting [`Id`] and a length.
pub struct Sequence<T> {
    start: Id<T>,
    len: Idx,
}

impl<T> Sequence<T> {
    /// The [`Id`] of the first element in the sequence.
    #[inline]
    pub const fn first(&self) -> Option<Id<T>> {
        if self.len > 0 {
            Some(self.start)
        } else {
            None
        }
    }

    /// The [`Id`] of the last element in the sequence.
    #[inline]
    pub const fn last(&self) -> Option<Id<T>> {
        if self.len > 0 {
            Some(self.start.offset(self.len - 1))
        } else {
            None
        }
    }

    /// The length of the sequence.
    #[inline]
    pub const fn len(&self) -> Idx {
        self.len
    }

    /// The [`Id`] of the element at the given index in
    /// the sequence.
    #[inline]
    pub const fn at(&self, index: Idx) -> Option<Id<T>> {
        if index < self.len {
            Some(self.start.offset(index))
        } else {
            None
        }
    }
}

/// An arena of elements. Removal is not allowed - only insertion.
#[repr(transparent)]
pub struct Arena<T> {
    elements: Vec<T>,
}

impl<T> Arena<T> {
    /// Creates a new, empty arena.
    #[inline]
    pub const fn new() -> Self {
        Self {
            elements: Vec::new(),
        }
    }

    /// Creates a new arena with the given capacity.
    #[inline]
    pub fn with_capacity(capacity: Idx) -> Self {
        Self {
            elements: Vec::with_capacity(capacity as usize),
        }
    }

    /// Returns the amount of elements in this arena.
    #[inline]
    pub fn len(&self) -> Idx {
        // this is all essentially just
        // 'Idx::try_from(self.elements.len()).expect("err message")'
        // but, according to godbolt, generates slightly shorter
        // assembly. so i'm taking it. yay for micro optimizations!
        #[inline(never)]
        #[cold]
        fn panics(e: Result<u32, std::num::TryFromIntError>) -> ! {
            e.expect("Element count below the limit");
            unreachable!()
        }

        let res = Idx::try_from(self.elements.len());
        if let Ok(len) = res {
            len
        } else {
            panics(res)
        }
    }

    /// Inserts a new element into this arena and returns
    /// it's [`Id`].
    #[inline]
    pub fn insert(&mut self, value: T) -> Id<T> {
        let index = self.len();
        self.elements.push(value);

        Id::new(index)
    }

    /// Inserts a sequence of elements into this arena and
    /// returns a [`Sequence`].
    #[inline]
    pub fn insert_sequence<I>(&mut self, sequence: I) -> Sequence<T>
    where
        I: IntoIterator<Item = T>,
    {
        let old_len = self.len();
        self.elements.extend(sequence);

        Sequence {
            start: Id::new(old_len),
            len: self.len() - old_len,
        }
    }

    /// Returns a reference to the value with the given [`Id`]
    /// in this arena.
    #[inline]
    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.elements.get(id.index as usize)
    }

    /// Returns a mutable reference to the value with the given
    /// [`Id`] in this arena.
    #[inline]
    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.elements.get_mut(id.index as usize)
    }

    /// Returns an iterator over the elements of this arena.
    #[inline]
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.elements.iter()
    }

    /// Returns a mutable iterator over the elements of this arena.
    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> {
        self.elements.iter_mut()
    }

    /// Returns an iterator over the elements of this arena with
    /// their [`Id`].
    #[inline]
    pub fn iter_with_id(&self) -> impl Iterator<Item = (Id<T>, &T)> {
        self.elements
            .iter()
            .enumerate()
            .map(|(index, r)| (Id::new(index as Idx), r))
    }

    /// Returns a mutable iterator over the elements of this arena
    /// with their [`Id`].
    #[inline]
    pub fn iter_with_id_mut(&mut self) -> impl Iterator<Item = (Id<T>, &mut T)> {
        self.elements
            .iter_mut()
            .enumerate()
            .map(|(index, r)| (Id::new(index as Idx), r))
    }

    /// Returns an iterator over the elements of a given sequence
    /// in this arena.
    #[inline]
    pub fn sequence_iter(&self, sequence: Sequence<T>) -> std::slice::Iter<T> {
        self.elements[sequence.start.index as usize..][..sequence.len as usize].iter()
    }

    /// Returns a mutable iterator over the elements of a given
    /// sequence in this arena.
    #[inline]
    pub fn sequence_iter_mut(&mut self, sequence: Sequence<T>) -> std::slice::IterMut<T> {
        self.elements[sequence.start.index as usize..][..sequence.len as usize].iter_mut()
    }

    /// Returns an iterator over the elements of a given sequence
    /// in this arena.
    #[inline]
    pub fn sequence_iter_with_id(
        &self,
        sequence: Sequence<T>,
    ) -> impl Iterator<Item = (Id<T>, &T)> {
        self.elements[sequence.start.index as usize..][..sequence.len as usize]
            .iter()
            .enumerate()
            .map(|(index, r)| (Id::new(index as Idx), r))
    }

    /// Returns a mutable iterator over the elements of a given
    /// sequence in this arena.
    #[inline]
    pub fn sequence_iter_with_id_mut(
        &mut self,
        sequence: Sequence<T>,
    ) -> impl Iterator<Item = (Id<T>, &mut T)> {
        self.elements[sequence.start.index as usize..][..sequence.len as usize]
            .iter_mut()
            .enumerate()
            .map(|(index, r)| (Id::new(index as Idx), r))
    }
}

impl<T> Index<Id<T>> for Arena<T> {
    type Output = T;

    #[inline]
    fn index(&self, id: Id<T>) -> &Self::Output {
        &self.elements[id.index as usize]
    }
}

impl<T> IndexMut<Id<T>> for Arena<T> {
    #[inline]
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        &mut self.elements[id.index as usize]
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn math() {
        enum Expr {
            Lit(u32),
            Add(Id<Expr>, Id<Expr>),
            Sub(Id<Expr>, Id<Expr>),
        }

        let mut arena: Arena<Expr> = Arena::new();
        let a = arena.insert(Expr::Lit(8));
        let b = arena.insert(Expr::Lit(3));
        let c = arena.insert(Expr::Lit(1));
        let d = arena.insert(Expr::Add(a, b));
        let e = arena.insert(Expr::Sub(d, c));
        let f = arena.insert(Expr::Sub(e, a));

        fn evaluate(expr: Id<Expr>, arena: &Arena<Expr>) -> u32 {
            match *arena.get(expr).unwrap() {
                Expr::Lit(x) => x,
                Expr::Add(lhs, rhs) => evaluate(lhs, arena) + evaluate(rhs, arena),
                Expr::Sub(lhs, rhs) => evaluate(lhs, arena) - evaluate(rhs, arena),
            }
        }

        dbg!(evaluate(f, &arena));
    }
}
