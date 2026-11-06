import gleam/option.{type Option}

// a dense js element store where unset slots are holes
pub type TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "new")
pub fn new() -> TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "from_list")
pub fn from_list(items: List(a)) -> TreeArray(a)

// none for a hole or a negative or out of bounds index
@external(erlang, "arc_tree_array_ffi", "get")
pub fn get(index: Int, arr: TreeArray(a)) -> Option(a)

// a negative index crashes rather than silently doing nothing
@external(erlang, "arc_tree_array_ffi", "set")
pub fn set(index: Int, value: a, arr: TreeArray(a)) -> TreeArray(a)

@external(erlang, "arc_tree_array_ffi", "size")
pub fn size(arr: TreeArray(a)) -> Int

// only shrinks, and a negative size crashes
@external(erlang, "arc_tree_array_ffi", "resize")
pub fn resize(arr: TreeArray(a), new_size: Int) -> TreeArray(a)

// past the end is a no-op
@external(erlang, "arc_tree_array_ffi", "reset")
pub fn reset(index: Int, arr: TreeArray(a)) -> TreeArray(a)

// folds over set entries only, in ascending order
@external(erlang, "arc_tree_array_ffi", "sparse_fold")
pub fn sparse_fold(f: fn(Int, a, b) -> b, initial: b, arr: TreeArray(a)) -> b

// first len values when none is unset
@external(erlang, "arc_tree_array_ffi", "dense_list")
pub fn dense_list(arr: TreeArray(a), len: Int) -> Option(List(a))

@external(erlang, "arc_tree_array_ffi", "append_list")
pub fn append_list(arr: TreeArray(a), items: List(a)) -> TreeArray(a)

// count values from index from when none is unset
@external(erlang, "arc_tree_array_ffi", "range_list")
pub fn range_list(arr: TreeArray(a), from: Int, count: Int) -> Option(List(a))
