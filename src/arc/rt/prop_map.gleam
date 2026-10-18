//// An object's own string-keyed properties. Everything that reads or writes
//// them from Gleam comes through here, so how they are stored is this module's
//// business alone (and that of the Erlang kernels, which know the layout).

import arc/bytecode/key.{type PropertyKey}
import gleam/dict.{type Dict}

pub type PropMap(v)

@external(erlang, "gleam_stdlib", "identity")
fn wrap(stored: Dict(PropertyKey, v)) -> PropMap(v)

@external(erlang, "gleam_stdlib", "identity")
fn stored(props: PropMap(v)) -> Dict(PropertyKey, v)

pub fn new() -> PropMap(v) {
  wrap(dict.new())
}

pub fn from_list(entries: List(#(PropertyKey, v))) -> PropMap(v) {
  wrap(dict.from_list(entries))
}

pub fn get(props: PropMap(v), key: PropertyKey) -> Result(v, Nil) {
  dict.get(stored(props), key)
}

pub fn has_key(props: PropMap(v), key: PropertyKey) -> Bool {
  dict.has_key(stored(props), key)
}

pub fn insert(props: PropMap(v), key: PropertyKey, value: v) -> PropMap(v) {
  wrap(dict.insert(stored(props), key, value))
}

pub fn delete(props: PropMap(v), key: PropertyKey) -> PropMap(v) {
  wrap(dict.delete(stored(props), key))
}

pub fn size(props: PropMap(v)) -> Int {
  dict.size(stored(props))
}

pub fn is_empty(props: PropMap(v)) -> Bool {
  dict.is_empty(stored(props))
}

/// in no particular order, like the rest of these
pub fn to_list(props: PropMap(v)) -> List(#(PropertyKey, v)) {
  dict.to_list(stored(props))
}

pub fn keys(props: PropMap(v)) -> List(PropertyKey) {
  dict.keys(stored(props))
}

pub fn values(props: PropMap(v)) -> List(v) {
  dict.values(stored(props))
}

pub fn fold(
  props: PropMap(v),
  from initial: acc,
  with f: fn(acc, PropertyKey, v) -> acc,
) -> acc {
  dict.fold(stored(props), initial, f)
}

pub fn map_values(props: PropMap(v), f: fn(PropertyKey, v) -> v) -> PropMap(v) {
  wrap(dict.map_values(stored(props), f))
}

pub fn filter(
  props: PropMap(v),
  keep: fn(PropertyKey, v) -> Bool,
) -> PropMap(v) {
  wrap(dict.filter(stored(props), keep))
}
