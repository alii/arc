import gleam/option.{type Option}

pub type TupleArray(a)

@external(erlang, "erlang", "list_to_tuple")
pub fn from_list(items: List(a)) -> TupleArray(a)

@external(erlang, "erlang", "tuple_to_list")
pub fn to_list(arr: TupleArray(a)) -> List(a)

@external(erlang, "arc_tuple_array_ffi", "array_get")
pub fn get(index: Int, arr: TupleArray(a)) -> Option(a)

/// badarg unless 0 <= index < size
@external(erlang, "arc_tuple_array_ffi", "array_get_unchecked")
pub fn get_unchecked(index: Int, arr: TupleArray(a)) -> a

/// badarg unless 0 <= index < size, o(n) copy
@external(erlang, "arc_tuple_array_ffi", "array_set_unchecked")
pub fn set_unchecked(index: Int, value: a, arr: TupleArray(a)) -> TupleArray(a)

/// 1-based, inlined bif
@external(erlang, "erlang", "element")
pub fn element(position: Int, arr: TupleArray(a)) -> a

/// 1-based, inlined bif
@external(erlang, "erlang", "setelement")
pub fn set_element(position: Int, arr: TupleArray(a), value: a) -> TupleArray(a)

@external(erlang, "erlang", "tuple_size")
pub fn size(arr: TupleArray(a)) -> Int

@external(erlang, "arc_tuple_array_ffi", "array_repeat")
pub fn repeat(value: a, count: Int) -> TupleArray(a)
