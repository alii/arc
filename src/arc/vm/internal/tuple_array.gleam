import gleam/option.{type Option}

/// O(1) indexed array. Erlang: tuple-backed. JS: native Array.
/// Used for bytecode, constants, locals, and function tables in the VM.
pub type TupleArray(a)

/// Convert a list to an array. O(n).
@external(erlang, "erlang", "list_to_tuple")
@external(javascript, "../arc_vm_ffi.mjs", "array_from_list")
pub fn from_list(items: List(a)) -> TupleArray(a)

/// Convert an array back to a list. O(n).
@external(erlang, "erlang", "tuple_to_list")
@external(javascript, "../arc_vm_ffi.mjs", "array_to_list")
pub fn to_list(arr: TupleArray(a)) -> List(a)

/// Read element at index (0-based). O(1).
@external(erlang, "arc_vm_ffi", "array_get")
@external(javascript, "../arc_vm_ffi.mjs", "array_get")
pub fn get(index: Int, arr: TupleArray(a)) -> Option(a)

/// Read element at index with no bounds check. O(1), zero allocation.
///
/// CALLER MUST GUARANTEE `0 <= index < size(arr)` or BEAM will badarg.
/// Use only for compiler-generated indices (bytecode PC, constant pool,
/// local slots, function table) where the invariant holds by construction.
/// For untrusted indices use `get` which returns Option.
@external(erlang, "arc_vm_ffi", "array_unsafe_get")
@external(javascript, "../arc_vm_ffi.mjs", "array_unsafe_get")
pub fn unsafe_get(index: Int, arr: TupleArray(a)) -> a

/// Write element at index (0-based), returning a new array. O(n) copy.
@external(erlang, "arc_vm_ffi", "array_set")
@external(javascript, "../arc_vm_ffi.mjs", "array_set")
pub fn set(
  index: Int,
  value: a,
  arr: TupleArray(a),
) -> Result(TupleArray(a), Nil)

/// Write element at index with no bounds check. O(n) copy.
///
/// CALLER MUST GUARANTEE `0 <= index < size(arr)` or BEAM will badarg.
/// Use only for compiler-generated indices. For untrusted indices use `set`.
@external(erlang, "arc_vm_ffi", "array_set_unchecked")
@external(javascript, "../arc_vm_ffi.mjs", "array_set_unchecked")
pub fn set_unchecked(index: Int, value: a, arr: TupleArray(a)) -> TupleArray(a)

/// Number of elements. O(1).
@external(erlang, "erlang", "tuple_size")
@external(javascript, "../arc_vm_ffi.mjs", "array_size")
pub fn size(arr: TupleArray(a)) -> Int

/// Create an array of `count` elements all set to `value`. O(n).
@external(erlang, "arc_vm_ffi", "array_repeat")
@external(javascript, "../arc_vm_ffi.mjs", "array_repeat")
pub fn repeat(value: a, count: Int) -> TupleArray(a)
