import arc/rt/async as rt_async
import arc/rt/builtins/regexp as b_regexp
import arc/rt/obj as rt_obj
import arc/rt/snapshot.{
  IncompatibleSnapshot, MalformedBinary, SnapshotContainsCompiledCode,
  SnapshotContainsHostJob,
}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, HostJob, JInt, KBool, KHandle, KNum, KStr, RegExpObj, SObject,
  StringKey, classify, mk_number, mk_string,
}
import gleam/dict
import rt_helpers

fn roundtrip(st: Agent) -> Agent {
  let assert Ok(bin) = snapshot.serialize(st)
  let assert Ok(st) = snapshot.deserialize(bin, rt_helpers.quiet_hooks())
  st
}

pub fn roundtrip_keeps_globals_and_properties_test() {
  let st = rt_helpers.agent()
  let st = rt_obj.t_global_set(st, <<"n">>, mk_number(JInt(42)))
  let st = rt_obj.t_global_set(st, <<"s">>, mk_string("hello"))
  let #(obj, st) = rt_obj.t_new_object_literal(st)
  let #(ka, st) = rt_store.t_key(st, "a")
  let #(kb, st) = rt_store.t_key(st, "b")
  let #(kc, st) = rt_store.t_key(st, "c")
  let #(_, st) = rt_obj.t_set_prop(st, obj, StringKey(ka), mk_number(JInt(1)))
  let #(inner, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = rt_obj.t_set_prop(st, inner, StringKey(kc), mk_string("deep"))
  let #(_, st) = rt_obj.t_set_prop(st, obj, StringKey(kb), inner)
  let st = rt_obj.t_global_set(st, <<"obj">>, obj)

  let st = roundtrip(st)
  let #(n, st) = rt_helpers.global(st, "n")
  assert classify(n) == KNum(JInt(42))
  let #(s, st) = rt_helpers.global(st, "s")
  assert classify(s) == KStr("hello")
  let #(obj, st) = rt_helpers.global(st, "obj")
  let #(a, st) = rt_helpers.get(st, obj, "a")
  assert classify(a) == KNum(JInt(1))
  let #(b, st) = rt_helpers.get(st, obj, "b")
  let #(c, _st) = rt_helpers.get(st, b, "c")
  assert classify(c) == KStr("deep")
}

pub fn roundtrip_keeps_arrays_test() {
  let st = rt_helpers.agent()
  let #(arr, st) =
    rt_obj.t_new_array(st, [
      mk_number(JInt(10)),
      mk_number(JInt(20)),
      mk_number(JInt(30)),
    ])
  let st = rt_obj.t_global_set(st, <<"arr">>, arr)

  let st = roundtrip(st)
  let #(arr, st) = rt_helpers.global(st, "arr")
  let #(len, st) = rt_helpers.get(st, arr, "length")
  assert classify(len) == KNum(JInt(3))
  let #(joined, _st) = rt_helpers.call_method(st, arr, "join", [mk_string("-")])
  assert classify(joined) == KStr("10-20-30")
}

pub fn natives_work_after_roundtrip_test() {
  let st = roundtrip(rt_helpers.agent())
  let #(math, st) = rt_helpers.global(st, "Math")
  let #(max, st) =
    rt_helpers.call_method(st, math, "max", [
      mk_number(JInt(1)),
      mk_number(JInt(5)),
      mk_number(JInt(3)),
    ])
  assert classify(max) == KNum(JInt(5))
  let #(array, st) = rt_helpers.global(st, "Array")
  let #(empty, st) = rt_obj.t_new_array(st, [])
  let #(is_array, _st) = rt_helpers.call_method(st, array, "isArray", [empty])
  assert classify(is_array) == KBool(True)
}

pub fn roundtrip_is_repeatable_test() {
  let st = rt_helpers.agent()
  let st = rt_obj.t_global_set(st, <<"x">>, mk_number(JInt(1)))
  let st = roundtrip(st)
  let #(x, st) = rt_helpers.global(st, "x")
  let assert KNum(JInt(x)) = classify(x)
  let st = rt_obj.t_global_set(st, <<"x">>, mk_number(JInt(x + 10)))
  let st = roundtrip(st)
  let #(x, _st) = rt_helpers.global(st, "x")
  assert classify(x) == KNum(JInt(11))
}

pub fn regexp_matcher_is_dropped_and_rebuilt_test() {
  let st = rt_helpers.agent()
  let #(re, st) = b_regexp.regexp_create_literal(st, "a+b", "")
  let assert KHandle(h) = classify(re)
  let st = rt_obj.t_global_set(st, <<"re">>, re)
  let matcher_cached = fn(st) {
    let assert SObject(kind: RegExpObj(compiled:, ..), ..) =
      rt_store.t_cell_get(st, h)
    compiled != b_regexp.uncompiled_regexp()
  }
  assert !matcher_cached(st)
  let #(hit, st) = rt_helpers.call_method(st, re, "test", [mk_string("caab")])
  assert classify(hit) == KBool(True)
  assert matcher_cached(st)

  let st = roundtrip(st)
  let #(re, st) = rt_helpers.global(st, "re")
  let assert KHandle(h2) = classify(re)
  assert h2 == h
  assert !matcher_cached(st)
  let #(hit, st) = rt_helpers.call_method(st, re, "test", [mk_string("caab")])
  assert classify(hit) == KBool(True)
  let #(miss, st) = rt_helpers.call_method(st, re, "test", [mk_string("ccc")])
  assert classify(miss) == KBool(False)
  assert matcher_cached(st)
}

pub fn deserialize_rebinds_hooks_and_drops_host_fns_test() {
  let st = rt_helpers.agent()
  let entry =
    types.HostFnEntry(name: "f", call: fn(st, _, _, _) {
      #(st, Ok(mk_number(JInt(0))))
    })
  let st = types.Agent(..st, host_fns: dict.from_list([#(0, entry)]))
  let st = roundtrip(st)
  assert st.host_fns == dict.new()
  assert st.frames == []
}

pub fn same_agent_same_bytes_test() {
  let st = rt_helpers.agent()
  let assert Ok(a) = snapshot.serialize(st)
  let assert Ok(b) = snapshot.serialize(st)
  assert a == b
}

pub fn compiled_function_is_refused_test() {
  let st = rt_helpers.agent()
  let #(f, st) = rt_helpers.func(st, fn(st, _) { #(mk_number(JInt(1)), st) })
  let assert KHandle(h) = classify(f)
  let st = rt_obj.t_global_set(st, <<"f">>, f)
  assert snapshot.serialize(st) == Error(SnapshotContainsCompiledCode(h))
}

pub fn queued_host_job_is_refused_test() {
  let st = rt_helpers.agent()
  let st = rt_async.t_enqueue_job(st, HostJob(run: fn(st) { st }))
  assert snapshot.serialize(st) == Error(SnapshotContainsHostJob)
}

pub fn garbage_is_malformed_test() {
  let hooks = rt_helpers.quiet_hooks()
  assert snapshot.deserialize(<<"definitely not a snapshot":utf8>>, hooks)
    == Error(MalformedBinary)
  assert snapshot.deserialize(<<1:size(3)>>, hooks) == Error(MalformedBinary)
  // term_to_binary({1, 2, 3}) with no header
  assert snapshot.deserialize(<<131, 104, 3, 97, 1, 97, 2, 97, 3>>, hooks)
    == Error(MalformedBinary)
}

pub fn other_version_is_incompatible_test() {
  let hooks = rt_helpers.quiet_hooks()
  assert snapshot.deserialize(
      <<"arc-engine":utf8, 999_999:32, 131, 106>>,
      hooks,
    )
    == Error(IncompatibleSnapshot)
}

pub fn corrupt_payload_behind_header_is_incompatible_test() {
  let hooks = rt_helpers.quiet_hooks()
  let v = snapshot.abi_version
  assert snapshot.deserialize(<<"arc-engine":utf8, v:32, 1, 2, 3>>, hooks)
    == Error(IncompatibleSnapshot)
  assert snapshot.deserialize(<<"arc-engine":utf8, v:32, 131, 106>>, hooks)
    == Error(IncompatibleSnapshot)
}
