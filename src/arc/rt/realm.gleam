import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/call as rt_call
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type Realm, type Test262Native, Agent,
  ArrayBufferDetach262, ArrayBufferN, DataProperty, KHandle, NoElements,
  Ordinary, SObject, ScriptEval, StringKey, Test262CreateRealm,
  Test262EvalScript, Test262Gc, Test262N, classify, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}

type Outcome(a) {
  NormalCompletion(a)
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(st: Agent, body: fn(Agent) -> #(a, Agent)) -> #(Outcome(a), Agent)

// restores the caller's realm even when body throws
pub fn with_realm(
  st: Agent,
  id: Int,
  body: fn(Agent) -> #(a, Agent),
) -> #(a, Agent) {
  use <- bool.lazy_guard(id == st.realm.id, fn() { body(st) })
  let origin = st.realm.id
  let #(outcome, after) = protected(enter(st, id), body)
  let restored = enter(after, origin)
  case outcome {
    NormalCompletion(v) -> #(v, restored)
    ThrowCompletion(e) -> rt_store.t_throw(restored, e)
  }
}

fn enter(st: Agent, id: Int) -> Agent {
  let realms = dict.insert(st.realms, st.realm.id, st.realm)
  case dict.get(realms, id) {
    Ok(realm) -> Agent(..st, realm:, realms:)
    Error(Nil) ->
      panic as { "rt/realm.enter: no realm with id " <> int.to_string(id) }
  }
}

pub fn install_262(st: Agent, realm: Realm) -> #(Handle, Agent) {
  let fn_proto = realm.function.prototype
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("evalScript", Test262N(Test262EvalScript(realm: realm.id)), 1),
      #("createRealm", Test262N(Test262CreateRealm(realm: realm.id)), 0),
      #("gc", Test262N(Test262Gc), 0),
      #("detachArrayBuffer", ArrayBufferN(ArrayBufferDetach262), 1),
    ])
  let #(global_prop, st) =
    common.builtin_property(st, mk_object(realm.global_object))
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: Some(realm.object.prototype),
        props: common.named_props([#("global", global_prop), ..methods]),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = rt_store.t_pin_root(st, h)
  let #(_new, st) =
    rt_obj.t_define_own_data(
      st,
      realm.global_object,
      StringKey(nk.x24262),
      mk_object(h),
      True,
      True,
      True,
    )
  #(h, st)
}

pub fn dispatch_262(
  st: Agent,
  native: Test262Native,
  _this: JsVal,
  args: List(JsVal),
  create_realm: fn(Agent) -> #(Realm, Agent),
) -> #(JsVal, Agent) {
  case native {
    Test262EvalScript(realm:) -> eval_script(st, realm, args)
    Test262CreateRealm(realm:) -> create_realm_262(st, realm, create_realm)
    // gc only runs at safepoints, nothing to do here
    Test262Gc -> #(mk_undefined(), st)
  }
}

fn eval_script(st: Agent, realm: Int, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(source, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  use st <- with_realm(st, realm)
  st.store.ops.eval_hook(st, source, ScriptEval)
}

fn create_realm_262(
  st: Agent,
  parent: Int,
  create_realm: fn(Agent) -> #(Realm, Agent),
) -> #(JsVal, Agent) {
  let #(realm, st) = create_realm(st)
  let #(dollar, st) = install_262(st, realm)
  let parent_global = rt_call.realm_by_id(st, parent).global_object
  let agent =
    own_data(st, parent_global, "$262")
    |> option.then(as_handle)
    |> option.then(own_data(st, _, "agent"))
  let st = case agent {
    Some(v) -> {
      let #(prop, st) = common.builtin_property(st, v)
      common.add_named_property(st, dollar, "agent", prop)
    }
    None -> st
  }
  #(mk_object(dollar), st)
}

fn own_data(st: Agent, h: Handle, name: String) -> Option(JsVal) {
  use k <- option.then(rt_store.t_find_key(st, name))
  case rt_obj.t_ordinary_own_property(st, h, StringKey(k)) {
    Some(DataProperty(value:, ..)) -> Some(value)
    _ -> None
  }
}

fn as_handle(v: JsVal) -> Option(Handle) {
  case classify(v) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}
