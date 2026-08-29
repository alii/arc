import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/lang as rt_lang
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type Realm, AccessorProperty, Agent, JInt,
  JsOps, JsStore, KHandle, ScriptEval, StringKey, classify, mk_number, mk_object,
  mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/int
import gleam/option.{Some}
import rt_helpers

fn two_realms() -> #(Realm, Agent) {
  rt_builtins.create_realm(rt_helpers.agent())
}

fn handle(v: JsVal) -> Handle {
  let assert KHandle(h) = classify(v)
  h
}

fn proto_of(st: Agent, v: JsVal) -> Handle {
  let assert #(Some(p), _) = rt_obj.t_get_prototype_of(st, handle(v))
  p
}

fn get(st: Agent, recv: JsVal, name: String) -> JsVal {
  rt_helpers.get(st, recv, name).0
}

pub fn create_realm_registers_a_distinct_realm_test() {
  let st0 = rt_helpers.agent()
  let #(other, st) = rt_builtins.create_realm(st0)
  assert st.realm.id == 0
  assert other.id == 1
  assert dict.size(st.realms) == 2
  assert rt_call.realm_by_id(st, 1) == other
  assert rt_call.realm_by_id(st, 0) == st.realm
  assert other.object.prototype != st.realm.object.prototype
  assert other.global_object != st.realm.global_object
  let other_global = mk_object(other.global_object)
  let other_object_ctor = get(st, other_global, "Object")
  assert other_object_ctor == mk_object(other.object.constructor)
  let #(o, st) =
    rt_call.t_construct(st, other_object_ctor, [], other_object_ctor)
  assert proto_of(st, mk_object(o)) == other.object.prototype
  assert proto_of(st, other_object_ctor) == other.function.prototype
}

pub fn with_realm_swaps_and_restores_test() {
  let #(other, st) = two_realms()
  let #(seen, st) =
    rt_realm.with_realm(st, other.id, fn(st) { #(st.realm, st) })
  assert seen == other
  assert st.realm.id == 0
  let #(thrower, st) =
    rt_helpers.func(st, fn(st, _args) {
      use st <- rt_realm.with_realm(st, other.id)
      rt_val.t_throw_type_error(st, "inside")
    })
  let #(outcome, st) = rt_call.t_call(st, thrower, mk_undefined(), [])
  let assert ThrowCompletion(e) = outcome
  assert st.realm.id == 0
  assert proto_of(st, e) == other.type_error.prototype
}

pub fn lexical_globals_persist_across_switches_test() {
  let #(other, st) = two_realms()
  let #(_, st) =
    rt_realm.with_realm(st, other.id, fn(st) {
      let realm =
        types.Realm(
          ..st.realm,
          lexical_globals: dict.insert(
            st.realm.lexical_globals,
            nk.value,
            types.Let(mk_string("v")),
          ),
        )
      #(Nil, Agent(..st, realm:))
    })
  assert dict.get(rt_call.realm_by_id(st, other.id).lexical_globals, nk.value)
    == Ok(types.Let(mk_string("v")))
  assert dict.get(st.realm.lexical_globals, nk.value) == Error(Nil)
}

pub fn install_262_and_create_realm_test() {
  let st = rt_helpers.agent()
  let #(dollar_h, st) = rt_realm.install_262(st, st.realm)
  let dollar = mk_object(dollar_h)
  let #(g, st) = rt_helpers.global(st, "$262")
  assert g == dollar
  assert get(st, dollar, "global") == mk_object(st.realm.global_object)
  let #(agent_obj, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = rt_obj.t_set_prop(st, dollar, StringKey(nk.agent), agent_obj)
  let #(child, st) = rt_helpers.call_method(st, dollar, "createRealm", [])
  assert st.realm.id == 0
  assert dict.size(st.realms) == 2
  let child_global = get(st, child, "global")
  assert child_global == mk_object(rt_call.realm_by_id(st, 1).global_object)
  assert child_global != mk_object(st.realm.global_object)
  assert get(st, child_global, "$262") == child
  assert get(st, child, "agent") == agent_obj
  assert proto_of(st, child) == rt_call.realm_by_id(st, 1).object.prototype
  let #(r, st) = rt_helpers.call_method(st, child, "gc", [])
  assert r == mk_undefined()
  let #(ab_ctor, st) = rt_helpers.global(st, "ArrayBuffer")
  let #(buf, st) =
    rt_call.t_construct(st, ab_ctor, [mk_number(JInt(8))], ab_ctor)
  let #(_, st) =
    rt_helpers.call_method(st, dollar, "detachArrayBuffer", [mk_object(buf)])
  assert get(st, mk_object(buf), "detached") == types.mk_bool(True)
}

pub fn eval_script_runs_in_its_realm_test() {
  let st = rt_helpers.agent()
  let hook = fn(st: Agent, source: String, kind) {
    assert kind == ScriptEval
    #(mk_string(source <> "@" <> int.to_string(st.realm.id)), st)
  }
  let st =
    Agent(
      ..st,
      store: JsStore(..st.store, ops: JsOps(..st.store.ops, eval_hook: hook)),
    )
  let #(dollar_h, st) = rt_realm.install_262(st, st.realm)
  let #(child, st) =
    rt_helpers.call_method(st, mk_object(dollar_h), "createRealm", [])
  let #(r, st) =
    rt_helpers.call_method(st, child, "evalScript", [mk_string("1+1")])
  assert r == mk_string("1+1@1")
  assert st.realm.id == 0
  let #(r, st) =
    rt_helpers.call_method(st, mk_object(dollar_h), "evalScript", [
      mk_string("2"),
    ])
  assert r == mk_string("2@0")
  assert st.realm.id == 0
}

pub fn json_is_attributed_to_its_own_realm_test() {
  let #(other, st) = two_realms()
  let other_json = get(st, mk_object(other.global_object), "JSON")
  let parse = get(st, other_json, "parse")
  let #(outcome, st) = rt_call.t_call(st, parse, other_json, [mk_string("{")])
  let assert ThrowCompletion(e) = outcome
  assert proto_of(st, e) == other.syntax_error.prototype
  assert st.realm.id == 0
  let #(outcome, st) =
    rt_call.t_call(st, parse, mk_undefined(), [mk_string("{\"a\":[1]}")])
  let assert NormalCompletion(obj) = outcome
  assert proto_of(st, obj) == other.object.prototype
  assert proto_of(st, get(st, obj, "a")) == other.array.prototype
  let #(reviver, st) =
    rt_helpers.func(st, fn(st, args) {
      let assert [_, v, ..] = args
      rt_helpers.record(st.realm.id)
      #(v, st)
    })
  let #(outcome, st) =
    rt_call.t_call(st, parse, mk_undefined(), [mk_string("[7]"), reviver])
  let assert NormalCompletion(_) = outcome
  assert rt_helpers.recorded() == [0, 0]
  let #(json, st) = rt_helpers.global(st, "JSON")
  let #(outcome, st) =
    rt_call.t_call(st, get(st, json, "parse"), json, [mk_string("{")])
  let assert ThrowCompletion(e) = outcome
  assert proto_of(st, e) == st.realm.syntax_error.prototype
}

pub fn stack_setter_uses_its_own_realm_test() {
  let #(other, st) = two_realms()
  let setter = fn(r: Realm) {
    let assert Some(AccessorProperty(set: Some(s), ..)) =
      rt_obj.t_ordinary_own_property(st, r.error.prototype, StringKey(nk.stack))
    s
  }
  let set_a = setter(st.realm)
  let set_b = setter(other)
  assert set_a != set_b
  let b_error_ctor = mk_object(other.error.constructor)
  let #(err_b, st) =
    rt_call.t_construct(st, b_error_ctor, [mk_string("m")], b_error_ctor)
  let #(outcome, st) =
    rt_call.t_call(st, set_a, mk_object(err_b), [mk_string("sentinel")])
  let assert NormalCompletion(_) = outcome
  assert get(st, mk_object(err_b), "stack") == mk_string("sentinel")
  let #(outcome, st) =
    rt_call.t_call(st, set_a, mk_object(other.error.prototype), [
      mk_string("x"),
    ])
  let assert ThrowCompletion(e) = outcome
  assert proto_of(st, e) == other.type_error.prototype
  assert st.realm.id == 0
  let #(outcome, st) =
    rt_call.t_call(st, set_a, mk_object(st.realm.error.prototype), [
      mk_string("x"),
    ])
  let assert ThrowCompletion(e) = outcome
  assert proto_of(st, e) == st.realm.type_error.prototype
}

pub fn template_objects_are_cached_per_realm_test() {
  let #(other, st) = two_realms()
  let tpl = fn(st) {
    rt_lang.t_get_template_object(st, "site-1", [mk_string("a")], ["a"])
  }
  let #(t0, st) = tpl(st)
  let #(t0_again, st) = tpl(st)
  assert t0 == t0_again
  let #(t1, st) = rt_realm.with_realm(st, other.id, tpl)
  assert t1 != t0
  assert proto_of(st, t1) == other.array.prototype
  let #(t1_again, _) = rt_realm.with_realm(st, other.id, tpl)
  assert t1_again == t1
}

pub fn species_create_ignores_a_foreign_array_constructor_test() {
  let #(other, st) = two_realms()
  let #(arr, st) = rt_obj.t_new_array(st, [mk_number(JInt(1))])
  let other_array = mk_object(other.array.constructor)
  let #(_, st) =
    rt_obj.t_set_prop(st, arr, StringKey(nk.constructor), other_array)
  let #(identity, st) =
    rt_helpers.func(st, fn(st, args) {
      let assert [v, ..] = args
      #(v, st)
    })
  let #(mapped, st) = rt_helpers.call_method(st, arr, "map", [identity])
  assert proto_of(st, mapped) == st.realm.array.prototype
  let #(concat, st) = rt_helpers.call_method(st, arr, "concat", [])
  assert proto_of(st, concat) == st.realm.array.prototype
}

pub fn construct_defaults_to_the_new_target_realm_intrinsic_test() {
  let #(other, st) = two_realms()
  let #(new_target, st) =
    rt_helpers.call_method(st, mk_object(other.array.constructor), "bind", [
      mk_undefined(),
    ])
  let construct = fn(st: Agent, pair: types.BuiltinPair, args) {
    let #(h, st) =
      rt_call.t_construct(st, mk_object(pair.constructor), args, new_target)
    #(mk_object(h), st)
  }
  let here = st.realm
  let #(m, st) = construct(st, here.map, [])
  assert proto_of(st, m) == other.map.prototype
  let #(e, st) = construct(st, here.type_error, [])
  assert proto_of(st, e) == other.type_error.prototype
  let #(u, st) = construct(st, here.uri_error, [])
  assert proto_of(st, u) == other.uri_error.prototype
  let #(a, st) = construct(st, here.array, [])
  assert proto_of(st, a) == other.array.prototype
  let #(d, st) = construct(st, here.date, [])
  assert proto_of(st, d) == other.date.prototype
  let #(b, st) = construct(st, here.array_buffer, [mk_number(JInt(0))])
  assert proto_of(st, b) == other.array_buffer.prototype
  let #(p, st) =
    rt_call.t_construct(
      st,
      mk_object(here.map.constructor),
      [],
      mk_object(other.set.constructor),
    )
  assert proto_of(st, mk_object(p)) == other.set.prototype
}
