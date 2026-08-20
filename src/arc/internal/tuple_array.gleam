import gleam/option.{type Option}

/// O(1) indexed array. Erlang: tuple-backed. JS: native Array.
/// Used for bytecode, constants, locals, and function tables in the VM.
pub type TupleArray(a)

/// Convert a list to an array. O(n).
@external(erlang, "erlang", "list_to_tuple")
pub fn from_list(items: List(a)) -> TupleArray(a)

/// Convert an array back to a list. O(n).
@external(erlang, "erlang", "tuple_to_list")
pub fn to_list(arr: TupleArray(a)) -> List(a)

/// Read element at index (0-based). O(1).
@external(erlang, "arc_tuple_array_ffi", "array_get")
pub fn get(index: Int, arr: TupleArray(a)) -> Option(a)

/// Read element at index with no bounds check. O(1), zero allocation.
///
/// CALLER MUST GUARANTEE `0 <= index < size(arr)` or BEAM will badarg.
/// Use only for compiler-generated indices (bytecode PC, constant pool,
/// local slots, function table) where the invariant holds by construction.
/// For untrusted indices use `get`, which returns Option.
@external(erlang, "arc_tuple_array_ffi", "array_get_unchecked")
pub fn get_unchecked(index: Int, arr: TupleArray(a)) -> a

/// Write element at index with no bounds check. O(n) copy.
///
/// CALLER MUST GUARANTEE `0 <= index < size(arr)` or BEAM will badarg.
/// Use only for compiler-generated indices (locals, constant pool), where
/// the invariant holds by construction.
@external(erlang, "arc_tuple_array_ffi", "array_set_unchecked")
pub fn set_unchecked(index: Int, value: a, arr: TupleArray(a)) -> TupleArray(a)

/// Read the element at 1-based `position`, bound straight to the
/// `element/2` BIF so the call site gets the inlined instruction rather
/// than a remote call. Same contract as `get_unchecked` (badarg when out
/// of bounds); for the dispatch loop's fetch and slot reads only.
@external(erlang, "erlang", "element")
pub fn element(position: Int, arr: TupleArray(a)) -> a

/// Write the element at 1-based `position` (`setelement/3` bound
/// directly). Same contract as `set_unchecked`.
@external(erlang, "erlang", "setelement")
pub fn set_element(position: Int, arr: TupleArray(a), value: a) -> TupleArray(a)

/// Number of elements. O(1).
@external(erlang, "erlang", "tuple_size")
pub fn size(arr: TupleArray(a)) -> Int

/// Create an array of `count` elements all set to `value`. O(n).
@external(erlang, "arc_tuple_array_ffi", "array_repeat")
pub fn repeat(value: a, count: Int) -> TupleArray(a)
