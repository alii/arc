import arc/rt/builtins as rt_builtins
import arc/rt/call.{type Frame, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/lang as rt_lang
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/types.{
  type Agent, type CompiledFn, type JsVal, FnFlags, JInt, KBool, KHandle, KNum,
  KStr, KUndef, StringKey, canonical_key, classify, mk_bool, mk_null, mk_number,
  mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import rt_helpers

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn t_apply_protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(rt_call.Completion, Agent)

@external(erlang, "arc_rt_store_ffi", "identity")
fn as_code(f: fn(Agent, Frame, List(JsVal)) -> #(JsVal, Agent)) -> CompiledFn

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
}

fn int(i: Int) -> JsVal {
  mk_number(JInt(i))
}

fn str(s: String) -> JsVal {
  mk_string(s)
}

fn key(name: String) {
  StringKey(canonical_key(name))
}

fn global(st: Agent, name: String) -> JsVal {
  let #(v, _) = rt_obj.t_global_get(st, <<name:utf8>>)
  v
}

fn get(st: Agent, obj: JsVal, name: String) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, obj, key(name))
}

fn get_(st: Agent, obj: JsVal, name: String) -> JsVal {
  get(st, obj, name).0
}

fn set(st: Agent, obj: JsVal, name: String, v: JsVal) -> Agent {
  let #(_, st) = rt_obj.t_set_prop(st, obj, key(name), v)
  st
}

fn object(st: Agent) -> #(JsVal, Agent) {
  rt_obj.t_new_object_literal(st)
}

fn record(st: Agent, entries: List(#(String, JsVal))) -> #(JsVal, Agent) {
  let #(o, st) = object(st)
  #(o, list.fold(entries, st, fn(st, e) { set(st, o, e.0, e.1) }))
}

fn handle(v: JsVal) {
  let assert KHandle(h) = classify(v)
  h
}

fn func(
  st: Agent,
  body: fn(Agent, List(JsVal)) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let flags =
    FnFlags(
      is_constructor: False,
      is_class_constructor: False,
      is_derived_constructor: False,
      is_arrow: True,
      is_method: False,
      is_generator: False,
      is_async: False,
      is_strict: True,
    )
  let code = as_code(fn(st, _frame, args) { body(st, args) })
  let #(h, st) = rt_call.t_fn_new(st, code, flags, "", 0, None, None)
  #(mk_object(h), st)
}

fn static(
  st: Agent,
  ns: String,
  name: String,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(f, st) = get(st, global(st, ns), name)
  rt_call.t_call_checked(st, f, global(st, ns), args)
}

fn proxy(st: Agent, target: JsVal, handler: JsVal) -> #(JsVal, Agent) {
  let ctor = global(st, "Proxy")
  let #(h, st) = rt_call.t_construct(st, ctor, [target, handler], ctor)
  #(mk_object(h), st)
}

fn logging_handler(st: Agent, log: JsVal) -> #(JsVal, Agent) {
  let names = [
    "getPrototypeOf", "setPrototypeOf", "isExtensible", "preventExtensions",
    "getOwnPropertyDescriptor", "defineProperty", "has", "get", "set",
    "deleteProperty", "ownKeys", "apply", "construct",
  ]
  let #(h, st) = object(st)
  let st =
    list.fold(names, st, fn(st, name) {
      let #(trap, st) =
        func(st, fn(st, args) {
          let #(_, st) =
            rt_call.t_call_method(st, log, key("push"), [str(name)])
          static(st, "Reflect", name, args)
        })
      set(st, h, name, trap)
    })
  #(h, st)
}

fn drain(st: Agent, log: JsVal) -> #(String, Agent) {
  let #(joined, st) = rt_call.t_call_method(st, log, key("join"), [str(",")])
  let st = set(st, log, "length", int(0))
  let assert KStr(s) = classify(joined)
  #(s, st)
}

fn throws(st: Agent, body: fn(Agent) -> #(a, Agent)) -> String {
  let #(c, st) =
    t_apply_protected(st, fn(st) {
      let #(_, st) = body(st)
      #(mk_undefined(), st)
    })
  let assert ThrowCompletion(err) = c
  let ctor = get_(st, err, "constructor")
  let assert KStr(name) = classify(get_(st, ctor, "name"))
  name
}

fn as_string(v: JsVal) -> String {
  let assert KStr(s) = classify(v)
  s
}

pub fn each_internal_method_reaches_its_trap_test() {
  let st = agent()
  let #(log, st) = rt_obj.t_new_array(st, [])
  let #(target, st) = record(st, [#("a", int(1))])
  let #(handler, st) = logging_handler(st, log)
  let #(p, st) = proxy(st, target, handler)
  let ph = handle(p)

  let #(v, st) = get(st, p, "a")
  assert classify(v) == KNum(JInt(1))
  let #(seen, st) = drain(st, log)
  assert seen == "get"

  let st = set(st, p, "b", int(2))
  let #(seen, st) = drain(st, log)
  assert seen == "set,getOwnPropertyDescriptor,defineProperty"
  assert classify(get_(st, target, "b")) == KNum(JInt(2))
  let #(_, st) = drain(st, log)

  let #(has, st) = rt_obj.t_has_prop(st, p, key("a"))
  assert has
  let #(seen, st) = drain(st, log)
  assert seen == "has"

  let #(ok, st) = rt_obj.t_delete_prop(st, ph, key("b"))
  assert ok
  let #(seen, st) = drain(st, log)
  assert seen == "deleteProperty"

  let #(_, st) = rt_obj.t_own_keys(st, ph)
  let #(seen, st) = drain(st, log)
  assert seen == "ownKeys"

  let #(_, st) = rt_obj.t_get_own_property(st, ph, key("a"))
  let #(seen, st) = drain(st, log)
  assert seen == "getOwnPropertyDescriptor"

  let #(ok, st) =
    rt_obj.t_define_own_data(st, ph, key("c"), int(3), True, True, True)
  assert ok
  let #(seen, st) = drain(st, log)
  assert seen == "defineProperty"

  let #(_, st) = rt_obj.t_get_prototype_of(st, ph)
  let #(seen, st) = drain(st, log)
  assert seen == "getPrototypeOf"

  let #(ok, st) = rt_obj.t_set_prototype(st, ph, None)
  assert ok
  let #(seen, st) = drain(st, log)
  assert seen == "setPrototypeOf"
  let #(proto, st) = rt_obj.t_get_prototype_of(st, handle(target))
  assert proto == None
  let #(_, st) = drain(st, log)

  let #(ext, st) = rt_obj.t_is_extensible(st, ph)
  assert ext
  let #(seen, st) = drain(st, log)
  assert seen == "isExtensible"

  let #(ok, st) = rt_obj.t_prevent_extensions(st, ph)
  assert ok
  let #(seen, st) = drain(st, log)
  assert seen == "preventExtensions"
  let #(ext, _) = rt_obj.t_is_extensible(st, handle(target))
  assert !ext
}

pub fn call_and_construct_traps_test() {
  let st = agent()
  let #(log, st) = rt_obj.t_new_array(st, [])
  let #(handler, st) = logging_handler(st, log)
  let #(target, st) =
    func(st, fn(st, args) {
      let assert [x, ..] = args
      let assert KNum(JInt(n)) = classify(x)
      #(int(n + 1), st)
    })
  let #(p, st) = proxy(st, target, handler)
  let #(v, st) = rt_call.t_call_checked(st, p, mk_undefined(), [int(41)])
  assert classify(v) == KNum(JInt(42))
  let #(seen, st) = drain(st, log)
  assert seen == "apply"
  let #(pa, st) = proxy(st, global(st, "Array"), handler)
  let #(arr_h, st) = rt_call.t_construct(st, pa, [int(3)], pa)
  let #(seen, st) = drain(st, log)
  assert seen == "construct,get"
  assert classify(get_(st, mk_object(arr_h), "length")) == KNum(JInt(3))
  let #(bad, st) = handler_of(st, "construct", int(1))
  let #(pb, st) = proxy(st, global(st, "Array"), bad)
  assert throws(st, fn(st) { rt_call.t_construct(st, pb, [], pb) })
    == "TypeError"
  let #(plain, st) = object(st)
  let #(pp, st) = proxy(st, plain, handler)
  assert throws(st, fn(st) { rt_call.t_call(st, pp, mk_undefined(), []) |> ok })
    == "TypeError"
}

fn ok(r: #(rt_call.Completion, Agent)) -> #(JsVal, Agent) {
  case r {
    #(NormalCompletion(v), st) -> #(v, st)
    #(ThrowCompletion(e), st) -> rt_call.t_call_checked(st, e, e, [])
  }
}

pub fn absent_traps_forward_to_target_test() {
  let st = agent()
  let #(target, st) = record(st, [#("a", int(1))])
  let #(handler, st) = object(st)
  let #(p, st) = proxy(st, target, handler)
  let ph = handle(p)
  assert classify(get_(st, p, "a")) == KNum(JInt(1))
  let st = set(st, p, "b", int(2))
  assert classify(get_(st, target, "b")) == KNum(JInt(2))
  let #(has, st) = rt_obj.t_has_prop(st, p, key("b"))
  assert has
  let #(keys, st) = rt_obj.t_own_keys(st, ph)
  assert keys == [key("a"), key("b")]
  let #(ok, st) = rt_obj.t_delete_prop(st, ph, key("a"))
  assert ok
  let #(has, st) = rt_obj.t_has_prop(st, target, key("a"))
  assert !has
  let #(proto, st) = rt_obj.t_get_prototype_of(st, ph)
  assert proto == Some(st.realm.object.prototype)
}

pub fn non_callable_trap_is_type_error_test() {
  let st = agent()
  let #(target, st) = object(st)
  let #(handler, st) = record(st, [#("get", int(1))])
  let #(p, st) = proxy(st, target, handler)
  assert throws(st, get(_, p, "x")) == "TypeError"
  let #(handler2, st) = record(st, [#("get", mk_null())])
  let #(p2, st) = proxy(st, target, handler2)
  assert classify(get_(st, p2, "x")) == KUndef
}

pub fn object_keys_uses_own_keys_then_descriptors_test() {
  let st = agent()
  let #(log, st) = rt_obj.t_new_array(st, [])
  let #(target, st) = record(st, [#("a", int(1)), #("b", int(2))])
  let #(handler, st) = logging_handler(st, log)
  let #(p, st) = proxy(st, target, handler)
  let #(keys, st) = static(st, "Object", "keys", [p])
  let #(joined, st) = rt_call.t_call_method(st, keys, key("join"), [str(",")])
  assert as_string(joined) == "a,b"
  let #(seen, st) = drain(st, log)
  assert seen == "ownKeys,getOwnPropertyDescriptor,getOwnPropertyDescriptor"
  let #(_, st) = static(st, "Object", "entries", [p])
  let #(seen, st) = drain(st, log)
  assert seen
    == "ownKeys,getOwnPropertyDescriptor,get,getOwnPropertyDescriptor,get"
  let #(dest, st) = object(st)
  let #(_, st) = static(st, "Object", "assign", [dest, p])
  let #(seen, st) = drain(st, log)
  assert seen
    == "ownKeys,getOwnPropertyDescriptor,get,getOwnPropertyDescriptor,get"
  assert classify(get_(st, dest, "b")) == KNum(JInt(2))
  let #(_, st) = static(st, "Object", "getOwnPropertyDescriptors", [p])
  let #(seen, st) = drain(st, log)
  assert seen == "ownKeys,getOwnPropertyDescriptor,getOwnPropertyDescriptor"
  let #(_, st) = static(st, "Object", "freeze", [p])
  let #(seen, st) = drain(st, log)
  assert seen
    == "preventExtensions,ownKeys,getOwnPropertyDescriptor,defineProperty,getOwnPropertyDescriptor,defineProperty"
  let #(frozen, st) = static(st, "Object", "isFrozen", [p])
  assert classify(frozen) == KBool(True)
  let #(seen, _) = drain(st, log)
  assert seen
    == "isExtensible,ownKeys,getOwnPropertyDescriptor,getOwnPropertyDescriptor"
}

pub fn for_in_and_spread_go_through_traps_test() {
  let st = agent()
  let #(log, st) = rt_obj.t_new_array(st, [])
  let #(target, st) = record(st, [#("x", int(1)), #("y", int(2))])
  let #(handler, st) = logging_handler(st, log)
  let #(p, st) = proxy(st, target, handler)
  let #(keys, st) = rt_obj.t_for_in_keys(st, p)
  assert list.map(keys, as_string) == ["x", "y"]
  let #(seen, st) = drain(st, log)
  assert seen
    == "ownKeys,getOwnPropertyDescriptor,getOwnPropertyDescriptor,getPrototypeOf"
  let #(dest, st) = object(st)
  let #(_, st) = rt_lang.t_copy_data_props(st, dest, p)
  let #(seen, st) = drain(st, log)
  assert seen
    == "ownKeys,getOwnPropertyDescriptor,get,getOwnPropertyDescriptor,get"
  assert classify(get_(st, dest, "y")) == KNum(JInt(2))
}

pub fn json_stringify_through_proxy_test() {
  let st = agent()
  let #(log, st) = rt_obj.t_new_array(st, [])
  let #(target, st) = record(st, [#("a", int(1)), #("b", str("s"))])
  let #(handler, st) = logging_handler(st, log)
  let #(p, st) = proxy(st, target, handler)
  let #(out, st) = static(st, "JSON", "stringify", [p])
  assert as_string(out) == "{\"a\":1,\"b\":\"s\"}"
  let #(seen, st) = drain(st, log)
  assert string.starts_with(
    seen,
    "get,ownKeys,getOwnPropertyDescriptor,getOwnPropertyDescriptor,get,get",
  )
  let #(arr, st) = rt_obj.t_new_array(st, [int(1), int(2)])
  let #(empty, st) = object(st)
  let #(pa, st) = proxy(st, arr, empty)
  let #(out, _) = static(st, "JSON", "stringify", [pa])
  assert as_string(out) == "[1,2]"
}

pub fn is_array_sees_through_proxies_test() {
  let st = agent()
  let #(arr, st) = rt_obj.t_new_array(st, [])
  let #(empty, st) = object(st)
  let #(p, st) = proxy(st, arr, empty)
  let #(pp, st) = proxy(st, p, empty)
  let #(r, st) = static(st, "Array", "isArray", [pp])
  assert classify(r) == KBool(True)
  let #(po, st) = proxy(st, empty, empty)
  let #(r, st) = static(st, "Array", "isArray", [po])
  assert classify(r) == KBool(False)
  let #(rv, st) = static(st, "Proxy", "revocable", [arr, empty])
  let revocable_proxy = get_(st, rv, "proxy")
  let #(_, st) = rt_call.t_call_method(st, rv, key("revoke"), [])
  assert throws(st, static(_, "Array", "isArray", [revocable_proxy]))
    == "TypeError"
  assert throws(st, static(_, "JSON", "stringify", [revocable_proxy]))
    == "TypeError"
}

pub fn instanceof_uses_get_prototype_of_trap_test() {
  let st = agent()
  let #(log, st) = rt_obj.t_new_array(st, [])
  let #(handler, st) = logging_handler(st, log)
  let #(target, st) = object(st)
  let #(p, st) = proxy(st, target, handler)
  let #(r, st) = rt_ops.t_instance_of(st, p, global(st, "Object"))
  assert r == 1
  let #(seen, st) = drain(st, log)
  assert seen == "getPrototypeOf"
  let array_proto = mk_object(st.realm.array.prototype)
  let #(liar, st) = handler_of(st, "getPrototypeOf", array_proto)
  let #(p2, st) = proxy(st, target, liar)
  let #(r, st) = rt_ops.t_instance_of(st, p2, global(st, "Array"))
  assert r == 1
  let #(is_proto, _) =
    rt_call.t_call_method(st, array_proto, key("isPrototypeOf"), [p2])
  assert classify(is_proto) == KBool(True)
}

pub fn descriptor_argument_is_read_through_traps_test() {
  let st = agent()
  let #(log, st) = rt_obj.t_new_array(st, [])
  let #(handler, st) = logging_handler(st, log)
  let #(desc, st) =
    record(st, [#("value", int(7)), #("enumerable", mk_bool(True))])
  let #(pd, st) = proxy(st, desc, handler)
  let #(o, st) = object(st)
  let #(_, st) = static(st, "Object", "defineProperty", [o, str("k"), pd])
  assert classify(get_(st, o, "k")) == KNum(JInt(7))
  let #(seen, _) = drain(st, log)
  assert seen == "has,get,has,has,get,has,has,has"
}

pub fn revoked_proxy_throws_on_every_operation_test() {
  let st = agent()
  let #(target, st) = func(st, fn(st, _) { #(mk_undefined(), st) })
  let #(empty, st) = object(st)
  let #(rv, st) = static(st, "Proxy", "revocable", [target, empty])
  let p = get_(st, rv, "proxy")
  let ph = handle(p)
  let #(_, st) = rt_call.t_call_method(st, rv, key("revoke"), [])
  let #(_, st) = rt_call.t_call_method(st, rv, key("revoke"), [])
  assert throws(st, get(_, p, "x")) == "TypeError"
  assert throws(st, rt_obj.t_set_prop(_, p, key("x"), int(1))) == "TypeError"
  assert throws(st, rt_obj.t_has_prop(_, p, key("x"))) == "TypeError"
  assert throws(st, rt_obj.t_delete_prop(_, ph, key("x"))) == "TypeError"
  assert throws(st, rt_obj.t_own_keys(_, ph)) == "TypeError"
  assert throws(st, rt_obj.t_get_own_property(_, ph, key("x"))) == "TypeError"
  assert throws(st, fn(st) {
      rt_obj.t_define_own_data(st, ph, key("x"), int(1), True, True, True)
    })
    == "TypeError"
  assert throws(st, rt_obj.t_get_prototype_of(_, ph)) == "TypeError"
  assert throws(st, rt_obj.t_set_prototype(_, ph, None)) == "TypeError"
  assert throws(st, rt_obj.t_is_extensible(_, ph)) == "TypeError"
  assert throws(st, rt_obj.t_prevent_extensions(_, ph)) == "TypeError"
  assert throws(st, fn(st) { rt_call.t_call(st, p, mk_undefined(), []) |> ok })
    == "TypeError"
  assert throws(st, rt_call.t_construct(_, p, [], p)) == "TypeError"
  assert throws(st, rt_obj.t_for_in_keys(_, p)) == "TypeError"
  assert throws(st, static(_, "Object", "keys", [p])) == "TypeError"
  let #(ty, _) = rt_val.t_type_of(st, p)
  assert ty == "function"
}

fn locked_target(st: Agent, sealed: Bool) -> #(JsVal, Agent) {
  let #(t, st) = object(st)
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      handle(t),
      key("k"),
      int(1),
      False,
      True,
      False,
    )
  let st = case sealed {
    True -> rt_obj.t_prevent_extensions(st, handle(t)).1
    False -> st
  }
  #(t, st)
}

fn handler_of(st: Agent, name: String, result: JsVal) -> #(JsVal, Agent) {
  let #(f, st) = func(st, fn(st, _) { #(result, st) })
  record(st, [#(name, f)])
}

pub fn get_invariant_test() {
  let st = agent()
  let #(t, st) = locked_target(st, False)
  let #(h, st) = handler_of(st, "get", int(2))
  let #(p, st) = proxy(st, t, h)
  assert throws(st, get(_, p, "k")) == "TypeError"
  let #(h, st) = handler_of(st, "get", int(1))
  let #(p, st) = proxy(st, t, h)
  assert classify(get_(st, p, "k")) == KNum(JInt(1))
}

pub fn set_invariant_test() {
  let st = agent()
  let #(t, st) = locked_target(st, False)
  let #(h, st) = handler_of(st, "set", mk_bool(True))
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_set_prop(_, p, key("k"), int(2))) == "TypeError"
  let #(ok, st) = rt_obj.t_set_prop(st, p, key("k"), int(1))
  assert ok
  let #(h, st) = handler_of(st, "set", mk_bool(False))
  let #(p, st) = proxy(st, t, h)
  let #(ok, _) = rt_obj.t_set_prop(st, p, key("other"), int(2))
  assert !ok
}

pub fn has_invariant_test() {
  let st = agent()
  let #(t, st) = locked_target(st, False)
  let #(h, st) = handler_of(st, "has", mk_bool(False))
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_has_prop(_, p, key("k"))) == "TypeError"
  let #(t2, st) = record(st, [#("j", int(1))])
  let st = rt_obj.t_prevent_extensions(st, handle(t2)).1
  let #(p2, st) = proxy(st, t2, h)
  assert throws(st, rt_obj.t_has_prop(_, p2, key("j"))) == "TypeError"
  let #(has, _) = rt_obj.t_has_prop(st, p2, key("zz"))
  assert !has
}

pub fn delete_invariant_test() {
  let st = agent()
  let #(t, st) = locked_target(st, False)
  let #(h, st) = handler_of(st, "deleteProperty", mk_bool(True))
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_delete_prop(_, handle(p), key("k"))) == "TypeError"
  let #(t2, st) = record(st, [#("j", int(1))])
  let st = rt_obj.t_prevent_extensions(st, handle(t2)).1
  let #(p2, st) = proxy(st, t2, h)
  assert throws(st, rt_obj.t_delete_prop(_, handle(p2), key("j")))
    == "TypeError"
}

pub fn get_own_property_descriptor_invariants_test() {
  let st = agent()
  let #(t, st) = locked_target(st, False)
  let #(h, st) = handler_of(st, "getOwnPropertyDescriptor", mk_undefined())
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_get_own_property(_, handle(p), key("k")))
    == "TypeError"
  let #(h, st) = handler_of(st, "getOwnPropertyDescriptor", int(1))
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_get_own_property(_, handle(p), key("k")))
    == "TypeError"
  let #(fake, st) =
    record(st, [#("value", int(1)), #("configurable", mk_bool(False))])
  let #(h, st) = handler_of(st, "getOwnPropertyDescriptor", fake)
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_get_own_property(_, handle(p), key("nope")))
    == "TypeError"
  let #(fake2, st) = record(st, [#("value", int(2))])
  let #(h, st) = handler_of(st, "getOwnPropertyDescriptor", fake2)
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_get_own_property(_, handle(p), key("k")))
    == "TypeError"
  let #(real, st) =
    record(st, [#("value", int(1)), #("enumerable", mk_bool(True))])
  let #(h, st) = handler_of(st, "getOwnPropertyDescriptor", real)
  let #(p, st) = proxy(st, t, h)
  let #(desc, _) = rt_obj.t_get_own_property(st, handle(p), key("k"))
  let assert Some(types.DataProperty(writable: False, configurable: False, ..)) =
    desc
}

pub fn define_property_invariants_test() {
  let st = agent()
  let #(h, st) = handler_of(st, "defineProperty", mk_bool(True))
  let #(t, st) = locked_target(st, True)
  let #(p, st) = proxy(st, t, h)
  assert throws(st, fn(st) {
      rt_obj.t_define_own_data(
        st,
        handle(p),
        key("new"),
        int(1),
        True,
        True,
        True,
      )
    })
    == "TypeError"
  let #(t2, st) = object(st)
  let #(p2, st) = proxy(st, t2, h)
  assert throws(st, fn(st) {
      rt_obj.t_define_own_data(
        st,
        handle(p2),
        key("x"),
        int(1),
        True,
        True,
        False,
      )
    })
    == "TypeError"
  let #(hf, st) = handler_of(st, "defineProperty", mk_bool(False))
  let #(pf, st) = proxy(st, t2, hf)
  let #(desc, st) = record(st, [#("value", int(1))])
  assert throws(st, static(_, "Object", "defineProperty", [pf, str("x"), desc]))
    == "TypeError"
  let #(r, _) = static(st, "Reflect", "defineProperty", [pf, str("x"), desc])
  assert classify(r) == KBool(False)
}

pub fn own_keys_invariants_test() {
  let st = agent()
  let #(t, st) = locked_target(st, False)
  let #(empty_arr, st) = rt_obj.t_new_array(st, [])
  let #(h, st) = handler_of(st, "ownKeys", empty_arr)
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_own_keys(_, handle(p))) == "TypeError"
  let #(dups, st) = rt_obj.t_new_array(st, [str("k"), str("k")])
  let #(h, st) = handler_of(st, "ownKeys", dups)
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_own_keys(_, handle(p))) == "TypeError"
  let #(bad, st) = rt_obj.t_new_array(st, [str("k"), int(1)])
  let #(h, st) = handler_of(st, "ownKeys", bad)
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_own_keys(_, handle(p))) == "TypeError"
  let #(t2, st) = locked_target(st, True)
  let #(extra, st) = rt_obj.t_new_array(st, [str("k"), str("zz")])
  let #(h, st) = handler_of(st, "ownKeys", extra)
  let #(p, st) = proxy(st, t2, h)
  assert throws(st, rt_obj.t_own_keys(_, handle(p))) == "TypeError"
  let #(like, st) = record(st, [#("length", int(1)), #("0", str("k"))])
  let #(h, st) = handler_of(st, "ownKeys", like)
  let #(p, st) = proxy(st, t2, h)
  let #(keys, _) = rt_obj.t_own_keys(st, handle(p))
  assert keys == [key("k")]
}

pub fn prototype_and_extensibility_invariants_test() {
  let st = agent()
  let #(t, st) = locked_target(st, True)
  let #(h, st) = handler_of(st, "getPrototypeOf", mk_null())
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_get_prototype_of(_, handle(p))) == "TypeError"
  let #(h, st) = handler_of(st, "getPrototypeOf", int(1))
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_get_prototype_of(_, handle(p))) == "TypeError"
  let #(h, st) = handler_of(st, "setPrototypeOf", mk_bool(True))
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_set_prototype(_, handle(p), None)) == "TypeError"
  assert throws(st, static(_, "Object", "setPrototypeOf", [p, mk_null()]))
    == "TypeError"
  let #(h, st) = handler_of(st, "isExtensible", mk_bool(True))
  let #(p, st) = proxy(st, t, h)
  assert throws(st, rt_obj.t_is_extensible(_, handle(p))) == "TypeError"
  let #(t2, st) = object(st)
  let #(h, st) = handler_of(st, "preventExtensions", mk_bool(True))
  let #(p, st) = proxy(st, t2, h)
  assert throws(st, rt_obj.t_prevent_extensions(_, handle(p))) == "TypeError"
  let #(h, st) = handler_of(st, "preventExtensions", mk_bool(False))
  let #(p, st) = proxy(st, t2, h)
  assert throws(st, static(_, "Object", "preventExtensions", [p]))
    == "TypeError"
  let #(r, _) = static(st, "Reflect", "preventExtensions", [p])
  assert classify(r) == KBool(False)
}

pub fn proxy_as_prototype_traps_on_inherited_access_test() {
  let st = agent()
  let #(log, st) = rt_obj.t_new_array(st, [])
  let #(target, st) = record(st, [#("inherited", int(9))])
  let #(handler, st) = logging_handler(st, log)
  let #(p, st) = proxy(st, target, handler)
  let #(child_h, st) = rt_obj.t_new_object(st, Some(handle(p)))
  let child = mk_object(child_h)
  let #(v, st) = get(st, child, "inherited")
  assert classify(v) == KNum(JInt(9))
  let #(seen, st) = drain(st, log)
  assert seen == "get"
  let #(has, st) = rt_obj.t_has_prop(st, child, key("inherited"))
  assert has
  let #(seen, st) = drain(st, log)
  assert seen == "has"
  let st = set(st, child, "fresh", int(1))
  let #(seen, st) = drain(st, log)
  assert seen == "set"
  let #(own, _) = rt_obj.t_get_own_property(st, child_h, key("fresh"))
  assert option.is_some(own)
}
