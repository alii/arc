import gleam/option.{type Option}

/// O(1) indexed array. Erlang: tuple-backed. JS: native Array.
/// Used for bytecode, constants, locals, and function tables in the VM.
pub type Array(a)

/// Convert a list to an array. O(n).
@external(erlang, "erlang", "list_to_tuple")
@external(javascript, "../../../arc_vm_ffi.mjs", "array_from_list")
pub fn from_list(items: List(a)) -> Array(a)

/// Convert an array back to a list. O(n).
@external(erlang, "erlang", "tuple_to_list")
@external(javascript, "../../../arc_vm_ffi.mjs", "array_to_list")
pub fn to_list(arr: Array(a)) -> List(a)

/// Read element at index (0-based). O(1).
@external(erlang, "arc_vm_ffi", "array_get")
@external(javascript, "../../../arc_vm_ffi.mjs", "array_get")
pub fn get(index: Int, arr: Array(a)) -> Option(a)

/// Write element at index (0-based), returning a new array. O(n) copy.
@external(erlang, "arc_vm_ffi", "array_set")
@external(javascript, "../../../arc_vm_ffi.mjs", "array_set")
pub fn set(index: Int, value: a, arr: Array(a)) -> Result(Array(a), Nil)

/// Number of elements. O(1).
@external(erlang, "erlang", "tuple_size")
@external(javascript, "../../../arc_vm_ffi.mjs", "array_size")
pub fn size(arr: Array(a)) -> Int

/// Create an array of `count` elements all set to `value`. O(n).
@external(erlang, "arc_vm_ffi", "array_repeat")
@external(javascript, "../../../arc_vm_ffi.mjs", "array_repeat")
pub fn repeat(value: a, count: Int) -> Array(a)

/// Grow an array to `new_size`, filling new slots with `default`. O(n).
/// If `new_size` <= current size, returns the array unchanged.
@external(erlang, "arc_vm_ffi", "array_grow")
@external(javascript, "../../../arc_vm_ffi.mjs", "array_grow")
pub fn grow(arr: Array(a), new_size: Int, default: a) -> Array(a)
