import gleam/option.{type Option}

pub type TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "tree_array_new")
pub fn new(default: a) -> TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "tree_array_from_list")
pub fn from_list(items: List(a), default: a) -> TreeArray(a)

/// none for unset, negative or out of bounds
@external(erlang, "arc_tree_array_ffi", "tree_array_get_option")
pub fn get_option(index: Int, arr: TreeArray(a)) -> Option(a)

/// negative index crashes, never a silent no-op
@external(erlang, "arc_tree_array_ffi", "tree_array_set")
pub fn set(index: Int, value: a, arr: TreeArray(a)) -> TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "tree_array_size")
pub fn size(arr: TreeArray(a)) -> Int

/// negative size crashes
@external(erlang, "arc_tree_array_ffi", "tree_array_resize")
pub fn resize(arr: TreeArray(a), new_size: Int) -> TreeArray(a)

/// past the end is a no-op, negative crashes
@external(erlang, "arc_tree_array_ffi", "tree_array_reset")
pub fn reset(index: Int, arr: TreeArray(a)) -> TreeArray(a)

/// set entries only, ascending
@external(erlang, "arc_tree_array_ffi", "tree_array_sparse_fold")
pub fn sparse_fold(f: fn(Int, a, b) -> b, initial: b, arr: TreeArray(a)) -> b
