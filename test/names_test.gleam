import arc/rt/names
import arc/rt/snapshot
import arc/rt/store as rt_store
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string
import rt_helpers

@external(erlang, "arc_rt_names_ffi", "fixed_count")
fn ffi_fixed_count() -> Int

@external(erlang, "arc_rt_names_ffi", "fixed_number")
fn ffi_fixed_number(text: String) -> Dynamic

@external(erlang, "arc_rt_names_ffi", "fixed_text")
fn ffi_fixed_text(n: Int) -> String

@external(erlang, "arc_names_test_ffi", "macro_fixed_count")
fn macro_fixed_count() -> Int

@external(erlang, "arc_names_test_ffi", "macro_length")
fn macro_length() -> Int

@external(erlang, "arc_names_test_ffi", "macro_proto")
fn macro_proto() -> Int

@external(erlang, "arc_names_test_ffi", "name_key")
fn name_key(n: Int) -> Int

@external(erlang, "arc_names_test_ffi", "index_key")
fn index_key(i: Int) -> Int

@external(erlang, "gleam_stdlib", "identity")
fn dyn(x: a) -> Dynamic

pub fn fixed_unique_test() {
  assert list.length(names.fixed) == set.size(set.from_list(names.fixed))
  assert list.length(names.fixed) == names.fixed_count()
}

pub fn fixed_in_sync_test() {
  list.index_map(names.fixed, fn(text, i) {
    assert names.fixed_text(i) == text
    assert names.fixed_number(text) == Some(i)
    assert ffi_fixed_text(i) == text
    assert ffi_fixed_number(text) == dyn(i)
  })
  assert names.fixed_number("zzz_not_fixed") == None
  assert ffi_fixed_number("zzz_not_fixed") == dyn(None)
}

pub fn macros_test() {
  assert macro_fixed_count() == names.fixed_count()
  assert ffi_fixed_count() == names.fixed_count()
  assert names.fixed_text(macro_length()) == "length"
  assert names.fixed_text(macro_proto()) == "__proto__"
  assert name_key(3) == 12
  assert index_key(0) == -1
  assert index_key(7) == -8
}

pub fn no_fixed_name_is_an_array_index_test() {
  use text <- list.each(names.fixed)
  let canonical = case int.parse(text) {
    Ok(n) -> int.to_string(n) == text && n >= 0 && n <= 4_294_967_294
    Error(Nil) -> False
  }
  assert !canonical
  assert !string.is_empty(text)
}

pub fn dynamic_names_test() {
  let js = rt_store.t_store_new()
  let count = names.fixed_count()
  assert js.next_name == count
  let assert Some(length) = names.fixed_number("length")
  let #(n, js) = rt_store.name_number(js, "length")
  assert n == length
  assert js.next_name == count
  assert rt_store.find_name(js, "zzz_not_fixed") == None
  assert js.next_name == count
  let #(a, js) = rt_store.name_number(js, "zzz_not_fixed")
  assert a == count
  assert js.next_name == count + 1
  let #(b, js) = rt_store.name_number(js, "zzz_not_fixed")
  assert b == a
  assert js.next_name == count + 1
  assert rt_store.find_name(js, "zzz_not_fixed") == Some(a)
  assert rt_store.name_text(js, a) == "zzz_not_fixed"
  assert rt_store.name_text(js, length) == "length"
  let #(c, js) = rt_store.name_number(js, "zzz_other")
  assert c == count + 1
  assert rt_store.name_text(js, c) == "zzz_other"
}

pub fn agent_wrappers_test() {
  let st = rt_helpers.agent()
  let #(a, st) = rt_store.t_name_number(st, "zzz_not_fixed")
  assert a == names.fixed_count()
  assert rt_store.t_name_text(st, a) == "zzz_not_fixed"
  assert rt_store.t_name_text(st, 0) == names.fixed_text(0)
}

pub fn snapshot_roundtrip_keeps_names_test() {
  let st = rt_helpers.agent()
  let #(a, st) = rt_store.t_name_number(st, "zzz_not_fixed")
  let #(b, st) = rt_store.t_name_number(st, "zzz_other")
  let assert Ok(bin) = snapshot.serialize(st)
  let assert Ok(st) = snapshot.deserialize(bin, rt_helpers.quiet_hooks())
  assert st.store.next_name == names.fixed_count() + 2
  assert rt_store.t_name_text(st, a) == "zzz_not_fixed"
  assert rt_store.t_name_text(st, b) == "zzz_other"
  let #(b2, st) = rt_store.t_name_number(st, "zzz_other")
  assert b2 == b
  let #(c, _) = rt_store.t_name_number(st, "zzz_third")
  assert c == names.fixed_count() + 2
}
