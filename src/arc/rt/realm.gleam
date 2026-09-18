import arc/bytecode/key.{Named}
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type Realm, type Test262Native, Agent,
  ArrayBufferDetach262, ArrayBufferN, DataProperty, Ordinary, ScriptEval,
  StringKey, Test262CreateRealm, Test262EvalScript, Test262Gc, Test262N,
  mk_object, mk_undefined, plain_object,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}

// restores the caller's realm even when body throws
pub fn with_realm(
  st: Agent,
  id: Int,
  body: fn(Agent) -> #(a, Agent),
) -> #(a, Agent) {
  use <- bool.lazy_guard(id == st.realm.id, fn() { body(st) })
  let origin = st.realm.id
  let #(outcome, after) = rt_call.try_run(enter(st, id), body)
  let restored = enter(after, origin)
  case outcome {
    NormalCompletion(v) -> #(v, restored)
    ThrowCompletion(e) -> rt_store.throw(restored, e)
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
    rt_store.builtin_property(st, mk_object(realm.global_object))
  let #(h, st) =
    rt_store.cell_new(
      st,
      plain_object(
        Ordinary,
        Some(realm.object.prototype),
        common.named_props([#("global", global_prop), ..methods]),
      ),
    )
  let st = rt_store.pin_root(st, h)
  let #(_new, st) =
    rt_obj.define_own_data(
      st,
      realm.global_object,
      StringKey(Named("$262")),
      mk_object(h),
      writable: True,
      enumerable: True,
      configurable: True,
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
  let #(source, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
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
  let agent_obj =
    own_data(st, parent_global, "$262")
    |> option.then(rt_val.handle_of)
    |> option.then(own_data(st, _, "agent"))
  let st = case agent_obj {
    Some(v) -> {
      let #(prop, st) = rt_store.builtin_property(st, v)
      common.add_named_property(st, dollar, "agent", prop)
    }
    None -> st
  }
  #(mk_object(dollar), st)
}

fn own_data(st: Agent, h: Handle, name: String) -> Option(JsVal) {
  case rt_obj.ordinary_own_property(st, h, StringKey(Named(name))) {
    Some(DataProperty(value:, ..)) -> Some(value)
    _ -> None
  }
}
