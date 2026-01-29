//! Shared memory segments with unified fetch/materialize API
//!
//! This module provides a unified API for accessing data in shared memory segments:
//!
//! - **`fetch()`** - Returns a view into shared data without copying. The view borrows
//!   from the shared segment and provides read-only access. Use this when you need to
//!   read data but don't need to own it or modify it.
//!
//! - **`materialize()`** - Copies data from the shared segment into an owned vector.
//!   Use this when you need to own the data, pass it to code that requires ownership,
//!   or when you need to modify the data.
//!
//! # When to use each
//!
//! | Scenario | Use |
//! |----------|-----|
//! | Read-only access in hot loop | `fetch()` |
//! | Need to modify data | `materialize()` |
//! | Passing to function taking `&[T]` | `fetch()` |
//! | Passing to function taking `Vec<T>` | `materialize()` |
//! | Long-lived data beyond segment lifetime | `materialize()` |
//! | Temporary access during computation | `fetch()` |
//!
//! # Example
//!
//! ```
//! use shard::shared::{SharedSegment, Fetchable, Materializable};
//!
//! // Create a shared segment (in real usage, this would be memory-mapped)
//! let data = vec![1.0, 2.0, 3.0, 4.0, 5.0];
//! let segment = SharedSegment::from_vec(data);
//!
//! // Fetch returns a view - no copy
//! let view = segment.fetch();
//! assert_eq!(view.as_slice(), &[1.0, 2.0, 3.0, 4.0, 5.0]);
//!
//! // Materialize returns owned data - copies
//! let owned: Vec<f64> = segment.materialize();
//! assert_eq!(owned, vec![1.0, 2.0, 3.0, 4.0, 5.0]);
//! ```

use std::ops::Deref;
use thiserror::Error;

/// Errors that can occur during shared memory operations
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum SharedError {
    #[error("segment has been detached")]
    Detached,

    #[error("index {index} out of bounds for length {len}")]
    OutOfBounds { index: usize, len: usize },

    #[error("slice range {start}..{end} out of bounds for length {len}")]
    SliceOutOfBounds { start: usize, end: usize, len: usize },

    #[error("segment is read-only")]
    ReadOnly,
}

/// A shared memory segment containing typed data
///
/// SharedSegment represents a contiguous block of memory that can be shared
/// across processes or threads. In the R integration, this would be backed
/// by memory-mapped files or POSIX shared memory.
///
/// The segment tracks access statistics for diagnostics:
/// - `fetch_count`: Number of times `fetch()` was called
/// - `materialize_count`: Number of times `materialize()` was called
/// - `materialized_bytes`: Total bytes copied via `materialize()`
#[derive(Debug)]
pub struct SharedSegment<T> {
    /// The underlying data
    data: Vec<T>,

    /// Whether this segment is read-only
    readonly: bool,

    /// Number of fetch() calls (for diagnostics)
    fetch_count: std::cell::Cell<u64>,

    /// Number of materialize() calls (for diagnostics)
    materialize_count: std::cell::Cell<u64>,

    /// Total bytes materialized (for diagnostics)
    materialized_bytes: std::cell::Cell<u64>,
}

impl<T: Clone> SharedSegment<T> {
    /// Create a new shared segment from a vector
    ///
    /// The segment takes ownership of the data and makes it available
    /// for shared access.
    pub fn from_vec(data: Vec<T>) -> Self {
        Self {
            data,
            readonly: true,
            fetch_count: std::cell::Cell::new(0),
            materialize_count: std::cell::Cell::new(0),
            materialized_bytes: std::cell::Cell::new(0),
        }
    }

    /// Create a new writable shared segment
    pub fn from_vec_writable(data: Vec<T>) -> Self {
        Self {
            data,
            readonly: false,
            fetch_count: std::cell::Cell::new(0),
            materialize_count: std::cell::Cell::new(0),
            materialized_bytes: std::cell::Cell::new(0),
        }
    }

    /// Returns the number of elements in the segment
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns true if the segment is empty
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns true if the segment is read-only
    pub fn is_readonly(&self) -> bool {
        self.readonly
    }

    /// Returns the number of fetch() calls made
    pub fn fetch_count(&self) -> u64 {
        self.fetch_count.get()
    }

    /// Returns the number of materialize() calls made
    pub fn materialize_count(&self) -> u64 {
        self.materialize_count.get()
    }

    /// Returns the total bytes materialized
    pub fn materialized_bytes(&self) -> u64 {
        self.materialized_bytes.get()
    }

    /// Reset all diagnostic counters
    pub fn reset_counters(&self) {
        self.fetch_count.set(0);
        self.materialize_count.set(0);
        self.materialized_bytes.set(0);
    }
}

/// A view into shared data
///
/// SharedView provides read-only access to data in a SharedSegment without
/// copying. The view borrows from the segment and cannot outlive it.
///
/// This is the return type of `fetch()` - use it when you need temporary
/// read-only access to shared data.
#[derive(Debug)]
pub struct SharedView<'a, T> {
    data: &'a [T],
}

impl<'a, T> SharedView<'a, T> {
    /// Returns the data as a slice
    pub fn as_slice(&self) -> &[T] {
        self.data
    }

    /// Returns the number of elements
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns true if empty
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Get element at index
    pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(index)
    }

    /// Get a sub-slice view
    pub fn slice(&self, start: usize, end: usize) -> Result<SharedView<'a, T>, SharedError> {
        if end > self.data.len() || start > end {
            return Err(SharedError::SliceOutOfBounds {
                start,
                end,
                len: self.data.len(),
            });
        }
        Ok(SharedView {
            data: &self.data[start..end],
        })
    }
}

impl<'a, T> Deref for SharedView<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl<'a, T> AsRef<[T]> for SharedView<'a, T> {
    fn as_ref(&self) -> &[T] {
        self.data
    }
}

/// Trait for types that support fetch() - returning a view without copying
///
/// Implement this trait for types that can provide read-only access to their
/// data without copying. The returned view borrows from self.
pub trait Fetchable<T> {
    /// Fetch a view into the data without copying
    ///
    /// Returns a SharedView that provides read-only access to the underlying
    /// data. The view borrows from the source and cannot outlive it.
    ///
    /// # When to use
    ///
    /// - Reading data in a hot loop
    /// - Passing data to functions that take `&[T]`
    /// - Temporary access during computation
    /// - When you want to avoid allocation overhead
    fn fetch(&self) -> SharedView<'_, T>;

    /// Fetch a slice of the data
    fn fetch_slice(&self, start: usize, end: usize) -> Result<SharedView<'_, T>, SharedError>;
}

/// Trait for types that support materialize() - copying data to owned form
///
/// Implement this trait for types that can copy their data to an owned
/// vector. This is the "eager" counterpart to fetch().
pub trait Materializable<T: Clone> {
    /// Materialize the data as an owned vector
    ///
    /// Returns a new Vec containing a copy of the data. The returned vector
    /// is independent and can outlive the source.
    ///
    /// # When to use
    ///
    /// - When you need to modify the data
    /// - Passing to functions that require `Vec<T>` ownership
    /// - When the data needs to outlive the shared segment
    /// - When building results that will be returned
    fn materialize(&self) -> Vec<T>;

    /// Materialize a slice of the data
    fn materialize_slice(&self, start: usize, end: usize) -> Result<Vec<T>, SharedError>;
}

impl<T: Clone> Fetchable<T> for SharedSegment<T> {
    fn fetch(&self) -> SharedView<'_, T> {
        self.fetch_count.set(self.fetch_count.get() + 1);
        SharedView { data: &self.data }
    }

    fn fetch_slice(&self, start: usize, end: usize) -> Result<SharedView<'_, T>, SharedError> {
        if end > self.data.len() || start > end {
            return Err(SharedError::SliceOutOfBounds {
                start,
                end,
                len: self.data.len(),
            });
        }
        self.fetch_count.set(self.fetch_count.get() + 1);
        Ok(SharedView {
            data: &self.data[start..end],
        })
    }
}

impl<T: Clone> Materializable<T> for SharedSegment<T> {
    fn materialize(&self) -> Vec<T> {
        self.materialize_count.set(self.materialize_count.get() + 1);
        let bytes = std::mem::size_of_val(self.data.as_slice());
        self.materialized_bytes
            .set(self.materialized_bytes.get() + bytes as u64);
        self.data.clone()
    }

    fn materialize_slice(&self, start: usize, end: usize) -> Result<Vec<T>, SharedError> {
        if end > self.data.len() || start > end {
            return Err(SharedError::SliceOutOfBounds {
                start,
                end,
                len: self.data.len(),
            });
        }
        self.materialize_count.set(self.materialize_count.get() + 1);
        let slice = &self.data[start..end];
        let bytes = std::mem::size_of_val(slice);
        self.materialized_bytes
            .set(self.materialized_bytes.get() + bytes as u64);
        Ok(slice.to_vec())
    }
}

// Also implement for SharedView to allow chained operations
impl<'a, T: Clone> Materializable<T> for SharedView<'a, T> {
    fn materialize(&self) -> Vec<T> {
        self.data.to_vec()
    }

    fn materialize_slice(&self, start: usize, end: usize) -> Result<Vec<T>, SharedError> {
        if end > self.data.len() || start > end {
            return Err(SharedError::SliceOutOfBounds {
                start,
                end,
                len: self.data.len(),
            });
        }
        Ok(self.data[start..end].to_vec())
    }
}

/// Diagnostics for shared segment access patterns
#[derive(Debug, Clone, Default)]
pub struct SharedDiagnostics {
    /// Number of fetch() calls
    pub fetch_count: u64,

    /// Number of materialize() calls
    pub materialize_count: u64,

    /// Total bytes materialized
    pub materialized_bytes: u64,
}

impl<T: Clone> From<&SharedSegment<T>> for SharedDiagnostics {
    fn from(segment: &SharedSegment<T>) -> Self {
        Self {
            fetch_count: segment.fetch_count(),
            materialize_count: segment.materialize_count(),
            materialized_bytes: segment.materialized_bytes(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fetch_returns_view() {
        let segment = SharedSegment::from_vec(vec![1, 2, 3, 4, 5]);
        let view = segment.fetch();

        assert_eq!(view.as_slice(), &[1, 2, 3, 4, 5]);
        assert_eq!(view.len(), 5);
        assert!(!view.is_empty());
    }

    #[test]
    fn test_materialize_returns_owned() {
        let segment = SharedSegment::from_vec(vec![1.0, 2.0, 3.0]);
        let owned = segment.materialize();

        assert_eq!(owned, vec![1.0, 2.0, 3.0]);

        // Verify it's actually a copy by checking segment still has data
        assert_eq!(segment.len(), 3);
    }

    #[test]
    fn test_fetch_slice() {
        let segment = SharedSegment::from_vec(vec![10, 20, 30, 40, 50]);
        let view = segment.fetch_slice(1, 4).unwrap();

        assert_eq!(view.as_slice(), &[20, 30, 40]);
    }

    #[test]
    fn test_materialize_slice() {
        let segment = SharedSegment::from_vec(vec![10, 20, 30, 40, 50]);
        let owned = segment.materialize_slice(2, 5).unwrap();

        assert_eq!(owned, vec![30, 40, 50]);
    }

    #[test]
    fn test_slice_bounds_error() {
        let segment = SharedSegment::from_vec(vec![1, 2, 3]);

        assert!(matches!(
            segment.fetch_slice(0, 10),
            Err(SharedError::SliceOutOfBounds { .. })
        ));

        assert!(matches!(
            segment.materialize_slice(2, 1),
            Err(SharedError::SliceOutOfBounds { .. })
        ));
    }

    #[test]
    fn test_diagnostics_tracking() {
        let segment = SharedSegment::from_vec(vec![1i32, 2, 3, 4]);

        // Initial state
        assert_eq!(segment.fetch_count(), 0);
        assert_eq!(segment.materialize_count(), 0);
        assert_eq!(segment.materialized_bytes(), 0);

        // Fetch doesn't copy
        let _view = segment.fetch();
        assert_eq!(segment.fetch_count(), 1);
        assert_eq!(segment.materialized_bytes(), 0);

        // Materialize copies
        let _owned = segment.materialize();
        assert_eq!(segment.materialize_count(), 1);
        assert_eq!(segment.materialized_bytes(), 16); // 4 * sizeof(i32) = 16

        // Materialize slice
        let _slice = segment.materialize_slice(0, 2).unwrap();
        assert_eq!(segment.materialize_count(), 2);
        assert_eq!(segment.materialized_bytes(), 24); // 16 + 2 * sizeof(i32) = 24
    }

    #[test]
    fn test_view_deref() {
        let segment = SharedSegment::from_vec(vec![1, 2, 3]);
        let view = segment.fetch();

        // Can use deref to get slice
        let sum: i32 = view.iter().sum();
        assert_eq!(sum, 6);
    }

    #[test]
    fn test_view_to_materialize() {
        let segment = SharedSegment::from_vec(vec![1, 2, 3, 4, 5]);

        // Fetch a view, then materialize part of it
        let view = segment.fetch();
        let sub_view = view.slice(1, 4).unwrap();
        let owned = sub_view.materialize();

        assert_eq!(owned, vec![2, 3, 4]);
    }

    #[test]
    fn test_readonly_flag() {
        let readonly = SharedSegment::from_vec(vec![1, 2, 3]);
        let writable = SharedSegment::from_vec_writable(vec![1, 2, 3]);

        assert!(readonly.is_readonly());
        assert!(!writable.is_readonly());
    }

    #[test]
    fn test_diagnostics_struct() {
        let segment = SharedSegment::from_vec(vec![1i64, 2, 3]);
        let _v = segment.fetch();
        let _m = segment.materialize();

        let diag = SharedDiagnostics::from(&segment);
        assert_eq!(diag.fetch_count, 1);
        assert_eq!(diag.materialize_count, 1);
        assert_eq!(diag.materialized_bytes, 24); // 3 * sizeof(i64)
    }

    #[test]
    fn test_reset_counters() {
        let segment = SharedSegment::from_vec(vec![1, 2, 3]);
        let _v = segment.fetch();
        let _m = segment.materialize();

        segment.reset_counters();

        assert_eq!(segment.fetch_count(), 0);
        assert_eq!(segment.materialize_count(), 0);
        assert_eq!(segment.materialized_bytes(), 0);
    }

    #[test]
    fn test_empty_segment() {
        let segment: SharedSegment<i32> = SharedSegment::from_vec(vec![]);

        assert!(segment.is_empty());
        assert_eq!(segment.len(), 0);

        let view = segment.fetch();
        assert!(view.is_empty());

        let owned = segment.materialize();
        assert!(owned.is_empty());
    }

    #[test]
    fn test_view_get() {
        let segment = SharedSegment::from_vec(vec![10, 20, 30]);
        let view = segment.fetch();

        assert_eq!(view.get(0), Some(&10));
        assert_eq!(view.get(2), Some(&30));
        assert_eq!(view.get(3), None);
    }
}
