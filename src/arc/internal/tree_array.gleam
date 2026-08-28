import gleam/option.{type Option}

/// dense js element store, unset slots are holes
pub type TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "new")
pub fn new() -> TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "from_list")
pub fn from_list(items: List(a)) -> TreeArray(a)

/// none for hole, negative or out of bounds
@external(erlang, "arc_tree_array_ffi", "get_option")
pub fn get_option(index: Int, arr: TreeArray(a)) -> Option(a)

/// negative index crashes, never a silent no-op
@external(erlang, "arc_tree_array_ffi", "set")
pub fn set(index: Int, value: a, arr: TreeArray(a)) -> TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "size")
pub fn size(arr: TreeArray(a)) -> Int

/// shrink only, negative size crashes
@external(erlang, "arc_tree_array_ffi", "resize")
pub fn resize(arr: TreeArray(a), new_size: Int) -> TreeArray(a)

/// past the end is a no-op
@external(erlang, "arc_tree_array_ffi", "reset")
pub fn reset(index: Int, arr: TreeArray(a)) -> TreeArray(a)

/// set entries only, ascending
@external(erlang, "arc_tree_array_ffi", "sparse_fold")
pub fn sparse_fold(f: fn(Int, a, b) -> b, initial: b, arr: TreeArray(a)) -> b
