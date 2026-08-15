//// Multi-realm support (§9.3 Realms): the realm registry on `Agent`,
//// entering another realm for the duration of a native's body, and the
//// test262 host-defined `$262` object whose `createRealm`/`evalScript` are
//// the only way script code reaches a second realm.
////
//// Model: `Agent.realm` is the running execution context's Realm Record and
//// `Agent.realms` holds every realm by `Realm.id`. Function objects carry no
//// [[Realm]] slot; a realm-attributed native (JSON, the `$262` methods, the
//// `Error.prototype.stack` setter) carries its realm id in its `NativeToken`
//// and enters it with `with_realm`. `eval` and `Function` are not attributed:
//// they run in whichever realm is current.

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type Realm, type Test262Native, Agent,
  ArrayBufferDetach262, ArrayBufferN, DataProperty, KHandle, Named, NoElements,
  Ordinary, SObject, ScriptEval, StringKey, Test262CreateRealm,
  Test262EvalScript, Test262Gc, Test262N, classify, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}

// ── registry ────────────────────────────────────────────────────────────────

/// The Realm Record for `id`. The current realm answers for its own id (its
/// registry entry may be stale); any other id must be registered.
pub fn lookup(st: Agent, id: Int) -> Realm {
  use <- bool.guard(id == st.realm.id, st.realm)
  case dict.get(st.realms, id) {
    Ok(r) -> r
    // Ids are only minted by `init_realm`, which registers them, and are
    // never removed: an unknown id is a corrupt token, not a JS error.
    Error(Nil) ->
      panic as { "rt/realm.lookup: no realm with id " <> int.to_string(id) }
  }
}

// ── entering a realm ────────────────────────────────────────────────────────

/// `arc/rt/call.Completion` widened over the normal value, so `with_realm`
/// bodies may return any type. Same wire shape the call ffi builds.
type Outcome(a) {
  NormalCompletion(a)
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(st: Agent, body: fn(Agent) -> #(a, Agent)) -> #(Outcome(a), Agent)

/// Run `body` with realm `id` as the current realm (its intrinsics, global
/// object and lexical globals), then make the caller's realm current again —
/// also when `body` throws, before the throw continues. Both realms' records
/// are written back to `realms` at each switch, so nested entries in either
/// direction see each other's global mutations. Entering the realm that is
/// already current is a plain call.
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

/// Make realm `id` current: park the running realm's record in the registry
/// and load `id`'s from it.
fn enter(st: Agent, id: Int) -> Agent {
  let realms = dict.insert(st.realms, st.realm.id, st.realm)
  case dict.get(realms, id) {
    Ok(realm) -> Agent(..st, realm:, realms:)
    Error(Nil) ->
      panic as { "rt/realm.enter: no realm with id " <> int.to_string(id) }
  }
}

// ── $262 (test262 INTERPRETING.md host-defined functions) ───────────────────

/// Build `realm`'s `$262` object — `global`, `evalScript`, `createRealm`,
/// `gc`, `detachArrayBuffer` — pin it, define it as `$262` on the realm's
/// global object, and return it. The harness adds `agent` and anything else
/// host-side to the returned object; `createRealm` carries `agent` over to
/// the realms it makes.
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
      StringKey(Named("$262")),
      mk_object(h),
      True,
      True,
      True,
    )
  #(h, st)
}

/// Dispatch a `$262` method. `create_realm` is `arc/rt/builtins.create_realm`,
/// passed in because that module imports this one.
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
    // Collection only runs at safepoints where every live value is rooted;
    // a native call is not one, so this is a hint with nothing to do.
    Test262Gc -> #(mk_undefined(), st)
  }
}

/// `$262.evalScript(source)`: ToString(source), then §16.1.6
/// ScriptEvaluation in `realm` through the interpreter's eval hook (parse
/// errors surface as that realm's SyntaxError). Returns the completion value.
fn eval_script(st: Agent, realm: Int, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(source, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  use st <- with_realm(st, realm)
  st.store.ops.eval_hook(st, source, ScriptEval)
}

/// `$262.createRealm()`: a fresh realm with its own `$262`, which is
/// returned. The parent `$262`'s `agent` (the harness's per-agent API) is
/// shared with the child, as arc's `extend_262` hook re-installed it there.
fn create_realm_262(
  st: Agent,
  parent: Int,
  create_realm: fn(Agent) -> #(Realm, Agent),
) -> #(JsVal, Agent) {
  let #(realm, st) = create_realm(st)
  let #(dollar, st) = install_262(st, realm)
  let parent_global = lookup(st, parent).global_object
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

/// The value of `h`'s own DATA property `name`, without getters or traps.
fn own_data(st: Agent, h: Handle, name: String) -> Option(JsVal) {
  case rt_obj.t_ordinary_own_property(st, h, StringKey(Named(name))) {
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
