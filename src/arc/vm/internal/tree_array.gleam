/// Functional array with O(log n) get/set — Erlang's `array` module.
///
/// Used for JS array elements (DenseElements). Unlike tuple_array:
/// - set is O(log n) instead of O(n) — sequential append is n·log(n) not n²
/// - ~5× memory overhead vs tuple, ~15× less than dict
/// - get is O(log n) instead of O(1), but the constant is tiny
///
/// tuple_array remains for bytecode/locals/constants where reads dominate
/// and writes are rare.
import gleam/option.{type Option}

pub type TreeArray(a)

/// Empty array with given default value for unset slots.
@external(erlang, "arc_vm_ffi", "tree_array_new")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_new")
pub fn new(default: a) -> TreeArray(a)

/// Build from list. O(n).
@external(erlang, "arc_vm_ffi", "tree_array_from_list")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_from_list")
pub fn from_list(items: List(a), default: a) -> TreeArray(a)

/// Convert to list of all set elements (0..size-1). O(n).
@external(erlang, "arc_vm_ffi", "tree_array_to_list")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_to_list")
pub fn to_list(arr: TreeArray(a)) -> List(a)

/// Read at index. Returns default for unset/out-of-bounds. O(log n).
@external(erlang, "arc_vm_ffi", "tree_array_get")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_get")
pub fn get(index: Int, arr: TreeArray(a)) -> a

/// Read as Option — None for unset slots. O(log n).
@external(erlang, "arc_vm_ffi", "tree_array_get_option")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_get_option")
pub fn get_option(index: Int, arr: TreeArray(a)) -> Option(a)

/// Write at index. Grows if needed. O(log n).
@external(erlang, "arc_vm_ffi", "tree_array_set")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_set")
pub fn set(index: Int, value: a, arr: TreeArray(a)) -> TreeArray(a)

/// Largest set index + 1. O(1).
@external(erlang, "arc_vm_ffi", "tree_array_size")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_size")
pub fn size(arr: TreeArray(a)) -> Int

/// Shrink to new_size. Unsets all indices >= new_size. O(log n).
@external(erlang, "arc_vm_ffi", "tree_array_resize")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_resize")
pub fn resize(arr: TreeArray(a), new_size: Int) -> TreeArray(a)

/// Reset slot to default value (creates a hole). O(log n).
@external(erlang, "arc_vm_ffi", "tree_array_reset")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_reset")
pub fn reset(index: Int, arr: TreeArray(a)) -> TreeArray(a)

/// Fold over non-default entries only, in ascending index order. Skips holes.
/// O(k) where k is the number of set entries.
@external(erlang, "arc_vm_ffi", "tree_array_sparse_fold")
@external(javascript, "../arc_vm_ffi.mjs", "tree_array_sparse_fold")
pub fn sparse_fold(f: fn(Int, a, b) -> b, initial: b, arr: TreeArray(a)) -> b
