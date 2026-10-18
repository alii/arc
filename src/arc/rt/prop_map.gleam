//// An object's own string-keyed properties. Everything that reads or writes
//// them from Gleam comes through here, so how they are stored is the business
//// of arc_rt_prop_map_ffi alone (and of the Erlang kernels, which know the
//// layout it describes).

import arc/bytecode/key.{type PropertyKey}

pub type PropMap(v)

@external(erlang, "maps", "new")
pub fn new() -> PropMap(v)

@external(erlang, "arc_rt_prop_map_ffi", "from_list")
pub fn from_list(entries: List(#(PropertyKey, v))) -> PropMap(v)

@external(erlang, "arc_rt_prop_map_ffi", "get")
pub fn get(props: PropMap(v), key: PropertyKey) -> Result(v, Nil)

@external(erlang, "arc_rt_prop_map_ffi", "has_key")
pub fn has_key(props: PropMap(v), key: PropertyKey) -> Bool

@external(erlang, "arc_rt_prop_map_ffi", "insert")
pub fn insert(props: PropMap(v), key: PropertyKey, value: v) -> PropMap(v)

@external(erlang, "arc_rt_prop_map_ffi", "delete")
pub fn delete(props: PropMap(v), key: PropertyKey) -> PropMap(v)

@external(erlang, "maps", "size")
pub fn size(props: PropMap(v)) -> Int

pub fn is_empty(props: PropMap(v)) -> Bool {
  size(props) == 0
}

/// in no particular order, like the rest of these
@external(erlang, "arc_rt_prop_map_ffi", "to_list")
pub fn to_list(props: PropMap(v)) -> List(#(PropertyKey, v))

@external(erlang, "arc_rt_prop_map_ffi", "keys")
pub fn keys(props: PropMap(v)) -> List(PropertyKey)

@external(erlang, "arc_rt_prop_map_ffi", "values")
pub fn values(props: PropMap(v)) -> List(v)

@external(erlang, "arc_rt_prop_map_ffi", "fold")
pub fn fold(
  props: PropMap(v),
  from initial: acc,
  with f: fn(acc, PropertyKey, v) -> acc,
) -> acc

@external(erlang, "arc_rt_prop_map_ffi", "map_values")
pub fn map_values(props: PropMap(v), f: fn(PropertyKey, v) -> v) -> PropMap(v)

@external(erlang, "arc_rt_prop_map_ffi", "filter")
pub fn filter(props: PropMap(v), keep: fn(PropertyKey, v) -> Bool) -> PropMap(v)
