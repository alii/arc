import arc/bytecode/key
import arc/rt/name_keys as nk
import arc/rt/names
import arc/rt/snapshot
import arc/rt/store as rt_store
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import gleam/string
import rt_helpers

@external(erlang, "arc_rt_names_ffi", "fixed_count")
fn ffi_fixed_count() -> Int

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

@external(erlang, "arc_names_test_ffi", "index_of_text")
fn ffi_index_of_text(text: String) -> Option(Int)

pub fn fixed_unique_test() {
  assert list.length(names.fixed) == set.size(set.from_list(names.fixed))
  assert list.length(names.fixed) == names.fixed_count()
}

pub fn fixed_in_sync_test() {
  list.index_map(names.fixed, fn(text, i) {
    assert names.fixed_text(i) == text
    assert names.fixed_number(text) == Some(i)
    assert ffi_fixed_text(i) == text
  })
  assert names.fixed_number("zzz_not_fixed") == None
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
  assert js.names.next == count
  let assert Some(length) = names.fixed_number("length")
  let #(n, js) = rt_store.name_number(js, "length")
  assert n == length
  assert js.names.next == count
  assert rt_store.find_name(js, "zzz_not_fixed") == None
  assert js.names.next == count
  let #(a, js) = rt_store.name_number(js, "zzz_not_fixed")
  assert a == count
  assert js.names.next == count + 1
  let #(b, js) = rt_store.name_number(js, "zzz_not_fixed")
  assert b == a
  assert js.names.next == count + 1
  assert rt_store.find_name(js, "zzz_not_fixed") == Some(a)
  assert rt_store.name_text(js, a) == "zzz_not_fixed"
  assert rt_store.name_text(js, length) == "length"
  let #(c, js) = rt_store.name_number(js, "zzz_other")
  assert c == count + 1
  assert rt_store.name_text(js, c) == "zzz_other"
}

pub fn agent_wrappers_test() {
  let st = rt_helpers.agent()
  let before = st.store.names.next
  let #(a, st) = rt_store.t_key(st, "zzz_not_fixed")
  assert key.name_number(a) == before
  assert rt_store.t_key_text(st, a) == "zzz_not_fixed"
  assert rt_store.t_key_text(st, key.name(0)) == names.fixed_text(0)
  assert rt_store.t_key_text(st, key.index(12)) == "12"
}

pub fn snapshot_roundtrip_keeps_names_test() {
  let st = rt_helpers.agent()
  let before = st.store.names.next
  let #(a, st) = rt_store.t_key(st, "zzz_not_fixed")
  let #(b, st) = rt_store.t_key(st, "zzz_other")
  let #(p, st) = rt_store.t_new_private_key(st, "#zzz")
  let assert Ok(bin) = snapshot.serialize(st)
  let assert Ok(st) = snapshot.deserialize(bin, rt_helpers.quiet_hooks())
  assert st.store.names.next == before + 2
  assert rt_store.t_key_text(st, a) == "zzz_not_fixed"
  assert rt_store.t_key_text(st, b) == "zzz_other"
  assert rt_store.t_key_text(st, p) == "#zzz"
  let #(b2, st) = rt_store.t_key(st, "zzz_other")
  assert b2 == b
  let #(c, _) = rt_store.t_key(st, "zzz_third")
  assert key.name_number(c) == before + 2
}

// name_keys.gleam is generated from the same list
pub fn name_keys_in_sync_test() {
  assert nk.length == key.name(0)
  let assert Some(proto) = names.fixed_number("__proto__")
  assert nk.n__proto__ == key.name(proto)
  let assert Some(to_string) = names.fixed_number("toString")
  assert nk.to_string == key.name(to_string)
  let assert Some(last) = names.fixed_number("zonedDateTimeISO")
  assert nk.zoned_date_time_iso == key.name(last)
  assert last == names.fixed_count() - 1
}

pub fn canonical_index_rule_test() {
  assert key.index_of_text("0") == Some(0)
  assert key.index_of_text("7") == Some(7)
  assert key.index_of_text("4294967294") == Some(4_294_967_294)
  assert key.index_of_text("4294967295") == None
  assert key.index_of_text("00") == None
  assert key.index_of_text("01") == None
  assert key.index_of_text("-0") == None
  assert key.index_of_text("1e3") == None
  assert key.index_of_text("") == None
  assert key.index_of_text(" 1") == None
  assert key.index_of_text("1 ") == None
  assert key.index_of_text("+1") == None
  assert key.index_of_text("1.0") == None
  use text <- list.each([
    "0", "7", "4294967294", "4294967295", "00", "01", "-0", "1e3", "", " 1",
    "1 ", "+1", "1.0", "x",
  ])
  assert ffi_index_of_text(text) == key.index_of_text(text)
}

pub fn find_does_not_allocate_test() {
  let st = rt_helpers.agent()
  let before = st.store.names.next
  assert rt_store.t_find_key(st, "zzz_never") == None
  assert rt_store.t_find_key(st, "12") == Some(key.index(12))
  assert rt_store.t_find_key(st, "length") == Some(nk.length)
  let #(k, st2) = rt_store.t_key(st, "zzz_never")
  assert st2.store.names.next == before + 1
  assert rt_store.t_find_key(st2, "zzz_never") == Some(k)
  assert key.is_name(k)
}
