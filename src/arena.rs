use std::{
    alloc::{self, dealloc, Layout},
    cell::Cell,
    marker::PhantomData,
    mem::size_of,
    ptr::{self, slice_from_raw_parts_mut},
};

pub struct Arena<'arena> {
    ptr: *mut u8,
    position: Cell<usize>,
    cap: usize,
    _phantom: PhantomData<&'arena mut u8>,
}

impl<'arena> Arena<'arena> {
    pub fn new() -> Self {
        let layout = Layout::array::<u8>(64000).unwrap();
        assert!(layout.size() == 64_000);

        let ptr = unsafe { alloc::alloc(layout) };

        Self {
            ptr,
            position: Cell::new(0),
            cap: layout.size(),
            _phantom: PhantomData,
        }
    }

    pub fn alloc_from_iter<T>(&self, iter: impl Iterator<Item = T>) -> &'arena mut [T] {
        let position = self.position.get();
        let mut count = 0;

        // FIXME: this is inefficient
        for x in iter {
            self.alloc(x);
            count += 1;
        }

        let current_ptr = unsafe { self.ptr.add(position) };
        let current_ptr = current_ptr.cast::<T>();
        unsafe { &mut *slice_from_raw_parts_mut(current_ptr, count) }
    }

    pub fn alloc<T>(&self, value: T) -> &'arena mut T {
        let position = self.position.get();
        // SAFETY: `ptr::add` requires that the start and end pointers are
        //         within an allocation. The start pointer (`self.ptr`) is
        //         immutable and returned directly from the allocator. We know
        //         that `position` is always less than `self.cap` (the size of
        //         our allocation). It follows that the resulting pointer is
        //         within our allocation.
        let current_ptr = unsafe { self.ptr.add(position) };
        let current_ptr = current_ptr.cast::<T>();

        assert!(current_ptr.is_aligned());

        let new_pos = position + size_of::<T>();
        assert!(new_pos < self.cap);

        // SAFETY: We have already ensured `current_ptr`'s validity (via
        //         `ptr::add`). We have also `assert!`ed that the pointer is
        //         aligned.
        unsafe { ptr::write(current_ptr, value) };

        self.position.set(new_pos);

        // SAFETY: We just `ptr::write`d the pointer, therefore this is ok.
        //         With regard to lifetimes, we ensure that the allocation is
        //         immutably held as long as the arena lives.
        unsafe { &mut *current_ptr }
    }
}

impl Drop for Arena<'_> {
    fn drop(&mut self) {
        let layout = Layout::array::<u8>(64000).unwrap();
        assert!(layout.size() == 64_000);

        // SAFETY: WIP
        unsafe {
            dealloc(self.ptr, layout);
        }
    }
}
