//// after a full collect, every dynamic key still reachable must have a text

import arc/bytecode/key.{type Key}
import arc/rt/gc as rt_gc
import arc/rt/types.{type Agent}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/string

@external(erlang, "names_gc_check_ffi", "dyn_keys")
fn dyn_keys(term: Dynamic, acc: Dict(Key, Nil)) -> Dict(Key, Nil)

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: anything) -> Dynamic

pub fn collect_and_check(st: Agent) -> Agent {
  let st = rt_gc.t_collect_full(st, [])
  check(st, [])
  st
}

pub fn check(st: Agent, frame_terms: List(Dynamic)) -> Nil {
  let realms = dict.insert(st.realms, st.realm.id, st.realm) |> dict.values
  let found =
    dyn_keys(to_dynamic(st), dict.new())
    |> list.fold(frame_terms, _, fn(acc, t) { dyn_keys(t, acc) })
    |> list.fold(realms, _, fn(acc, realm) {
      dict.fold(realm.lexical_globals, acc, fn(a, k, _) {
        dyn_keys(to_dynamic(types.StringKey(k)), a)
      })
    })
  let names = st.store.names
  let lost =
    dict.keys(found)
    |> list.filter(fn(k) {
      !dict.has_key(names.texts, k) && !dict.has_key(names.pinned, k)
    })
  case lost {
    [] -> Nil
    _ ->
      panic as {
        "reachable keys with no text: "
        <> string.join(list.map(lost, int.to_string), ", ")
      }
  }
}
