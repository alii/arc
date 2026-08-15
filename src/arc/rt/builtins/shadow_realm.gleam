//// proposal-shadowrealm: `ShadowRealm ( )`, `ShadowRealm.prototype.
//// evaluate` / `importValue`, and the wrapped function exotic objects that
//// carry callables across the realm boundary, over `Agent.realms`.
////
//// A ShadowRealm instance owns a fresh realm (`ShadowRealmObj(realm:)` is
//// the [[ShadowRealm]] slot, as a realm id). `evaluate` runs eval code in
//// that realm and returns the completion value through GetWrappedValue:
//// primitives cross the boundary as-is, callables cross as wrapped
//// functions, anything else is a TypeError. The methods are realm-attributed
//// natives: the realm id in their token is the spec's callerRealm, whose
//// intrinsics brand every error and wrapper a call produces.

import arc/compiler
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/inspect
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type Realm,
  type ShadowRealmNative, IndirectEval, JInt, JPosInf, KHandle, KNum, KStr,
  Named, ShadowRealmConstructor, ShadowRealmEvaluate, ShadowRealmImportValue,
  ShadowRealmN, ShadowRealmObj, StringKey, TypeErr, WrappedFunctionCall,
  classify, mk_number, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

// ── init ────────────────────────────────────────────────────────────────────

/// %ShadowRealm% + %ShadowRealm.prototype% for realm `realm`.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
  realm: Int,
) -> #(BuiltinPair, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("evaluate", ShadowRealmN(ShadowRealmEvaluate(realm:)), 1),
      #("importValue", ShadowRealmN(ShadowRealmImportValue(realm:)), 2),
    ])
  let #(shadow_realm, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      methods,
      fn(proto) { ShadowRealmN(ShadowRealmConstructor(proto:)) },
      "ShadowRealm",
      0,
      [],
    )
  let st = common.add_to_string_tag(st, shadow_realm.prototype, "ShadowRealm")
  #(shadow_realm, st)
}

// ── dispatch ────────────────────────────────────────────────────────────────

pub fn dispatch(
  st: Agent,
  n: ShadowRealmNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    ShadowRealmConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor ShadowRealm requires 'new'")
    ShadowRealmEvaluate(realm:) -> evaluate(st, realm, this, args)
    ShadowRealmImportValue(realm:) -> import_value(st, realm, this, args)
    WrappedFunctionCall(target:, caller_realm:, target_realm:) ->
      wrapped_function_call(st, target, caller_realm, target_realm, this, args)
  }
}

/// `create_realm` is `arc/rt/builtins.create_realm`, passed in because that
/// module imports this one.
pub fn dispatch_construct(
  st: Agent,
  n: ShadowRealmNative,
  new_target: JsVal,
  create_realm: fn(Agent) -> #(Realm, Agent),
) -> #(Handle, Agent) {
  case n {
    ShadowRealmConstructor(proto:) ->
      construct(st, proto, new_target, create_realm)
    ShadowRealmEvaluate(..)
    | ShadowRealmImportValue(..)
    | WrappedFunctionCall(..) ->
      rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// ── ShadowRealm ( ) — §3.2.1 ────────────────────────────────────────────────

fn construct(
  st: Agent,
  fallback_proto: Handle,
  new_target: JsVal,
  create_realm: fn(Agent) -> #(Realm, Agent),
) -> #(Handle, Agent) {
  // Step 2: OrdinaryCreateFromConstructor(NewTarget, "%ShadowRealm.prototype%",
  // « [[ShadowRealm]] »). The realm record has no %ShadowRealm% slot, so the
  // intrinsic default is the constructor's own.
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) {
      fallback_proto
    })
  // Steps 3-12: CreateRealm + SetRealmGlobalObject + SetDefaultGlobalBindings.
  let #(realm, st) = create_realm(st)
  realm_ops.alloc_wrapper(st, ShadowRealmObj(realm: realm.id), proto)
}

/// §3.1.1 ValidateShadowRealmObject: read the [[ShadowRealm]] slot off
/// `this`, else a TypeError naming `method`.
fn require_shadow_realm(st: Agent, this: JsVal, method: String) -> Int {
  let brand =
    helpers.brand_of(st, this, fn(kind) {
      case kind {
        ShadowRealmObj(realm:) -> Some(realm)
        _ -> None
      }
    })
  case brand {
    Some(#(realm, _)) -> realm
    None ->
      rt_val.t_throw_type_error(
        st,
        "ShadowRealm.prototype." <> method <> " called on incompatible receiver",
      )
  }
}

// ── crossing the boundary ───────────────────────────────────────────────────

/// `rt_call.Completion` widened over the normal value. Same wire shape the
/// call ffi builds.
type Outcome(a) {
  NormalCompletion(a)
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(st: Agent, body: fn(Agent) -> #(a, Agent)) -> #(Outcome(a), Agent)

/// Run `body` with realm `id` current and catch what it throws; the current
/// realm is current again either way.
fn protected_in_realm(
  st: Agent,
  id: Int,
  body: fn(Agent) -> #(a, Agent),
) -> #(Outcome(a), Agent) {
  use st <- protected(st)
  rt_realm.with_realm(st, id, body)
}

/// §2.3 GetWrappedValue ( targetRealm, value ). `from` is the realm the value
/// comes from, `into` the one it is passed into (the new wrapper's [[Realm]]).
/// The TypeError belongs to the running realm.
fn get_wrapped_value(
  st: Agent,
  from: Int,
  into: Int,
  val: JsVal,
) -> #(JsVal, Agent) {
  case classify(val) {
    KHandle(h) ->
      case rt_call.is_callable(st, val) {
        True -> wrapped_function_create(st, from, into, h)
        False ->
          rt_val.t_throw_type_error(
            st,
            "value crossing the ShadowRealm boundary must be callable or primitive",
          )
      }
    _ -> #(val, st)
  }
}

/// GetWrappedValue over a list.
fn wrap_all(
  st: Agent,
  from: Int,
  into: Int,
  vals: List(JsVal),
) -> #(List(JsVal), Agent) {
  let #(wrapped, st) =
    list.fold(vals, #([], st), fn(acc, v) {
      let #(done, st) = acc
      let #(w, st) = get_wrapped_value(st, from, into, v)
      #([w, ..done], st)
    })
  #(list.reverse(wrapped), st)
}

/// §2.2 WrappedFunctionCreate ( callerRealm, Target ), including
/// CopyNameAndLength (§2.4). `into` is callerRealm: it supplies the wrapper's
/// %Function.prototype% and becomes its [[Realm]]. Any abrupt completion from
/// the observable Gets on Target becomes a TypeError of the running realm.
fn wrapped_function_create(
  st: Agent,
  from: Int,
  into: Int,
  target: Handle,
) -> #(JsVal, Agent) {
  // The name/length Gets are observable (accessors run) — execute them in
  // the target's own realm so getter code resolves globals there.
  let #(copied, st) =
    protected_in_realm(st, from, copy_name_and_length(_, target))
  case copied {
    ThrowCompletion(_thrown) ->
      rt_val.t_throw_type_error(
        st,
        "wrapped function could not copy target name and length",
      )
    NormalCompletion(#(name, length)) -> {
      let fn_proto = rt_call.realm_by_id(st, into).function.prototype
      let tag =
        ShadowRealmN(WrappedFunctionCall(
          target:,
          caller_realm: into,
          target_realm: from,
        ))
      let #(h, st) =
        rt_call.t_native_new_computed_length(
          st,
          Some(fn_proto),
          tag,
          name,
          length,
        )
      #(mk_object(h), st)
    }
  }
}

/// §2.4 CopyNameAndLength ( F, Target ), steps 2-7 (the reads). Returns the
/// name string and the length value to define on the wrapper.
fn copy_name_and_length(
  st: Agent,
  target: Handle,
) -> #(#(String, JsVal), Agent) {
  let target_v = mk_object(target)
  // Step 3: targetHasLength = ? HasOwnProperty(Target, "length") — via
  // [[GetOwnProperty]] so proxy getOwnPropertyDescriptor traps (and revoked
  // proxies) are observable.
  let #(len_desc, st) =
    rt_obj.t_get_own_property(st, target, StringKey(Named("length")))
  // Step 4: if present, targetLen = ? Get(Target, "length").
  let #(len_val, st) = case len_desc {
    Some(_) -> rt_obj.t_get_prop(st, target_v, StringKey(Named("length")))
    None -> #(mk_undefined(), st)
  }
  let length = case classify(len_val) {
    KNum(JPosInf) -> mk_number(JPosInf)
    // ToIntegerOrInfinity then max(L, 0); NaN and -∞ come out as 0.
    KNum(n) ->
      mk_number(JInt(int.max(rt_val.jsnum_to_integer_or_infinity(n), 0)))
    _ -> mk_number(JInt(0))
  }
  // Step 6: targetName = ? Get(Target, "name"); non-strings become "".
  let #(name_val, st) =
    rt_obj.t_get_prop(st, target_v, StringKey(Named("name")))
  let name = case classify(name_val) {
    KStr(s) -> s
    _ -> ""
  }
  #(#(name, length), st)
}

// ── ShadowRealm.prototype.evaluate ( sourceText ) — §3.4.1 ──────────────────

fn evaluate(
  st: Agent,
  own_realm: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // The method's own realm is the spec's callerRealm: it brands every error
  // and wrapper this call produces (a built-in runs in its own realm even
  // when invoked from another one).
  use st <- rt_realm.with_realm(st, own_realm)
  // Step 2: ValidateShadowRealmObject(O).
  let eval_realm = require_shadow_realm(st, this, "evaluate")
  // Step 3: If sourceText is not a String, throw a TypeError (no coercion).
  case classify(first_arg_or_undefined(args)) {
    KStr(source) -> perform_shadow_realm_eval(st, source, own_realm, eval_realm)
    _ ->
      rt_val.t_throw_type_error(
        st,
        "ShadowRealm.prototype.evaluate expects a string",
      )
  }
}

/// §3.1.3 PerformShadowRealmEval ( sourceText, callerRealm, evalRealm ), with
/// callerRealm current. Parse in the caller context (early errors surface as
/// the caller realm's SyntaxError), execute as eval code in the shadow realm,
/// wrap the completion value for the caller.
fn perform_shadow_realm_eval(
  st: Agent,
  source: String,
  caller_realm: Int,
  eval_realm: Int,
) -> #(JsVal, Agent) {
  // Steps 2-3: the early-error pass, here, before any context switch (the
  // eval hook repeats it in the shadow realm, where it can no longer fail).
  let early = {
    use #(body, sb) <- result.try(
      parser.parse_script(source)
      |> result.map_error(parser.parse_error_to_string),
    )
    compiler.compile_eval(body, sb) |> result.map_error(compiler.error_message)
  }
  let st = case early {
    Ok(_template) -> st
    Error(message) -> rt_val.t_throw_syntax_error(st, message)
  }
  // Steps 8-21: evaluate the body in evalRealm's global environment, then
  // make callerRealm current again whatever the completion.
  let #(outcome, st) =
    protected_in_realm(st, eval_realm, fn(st) {
      st.store.ops.eval_hook(st, source, IndirectEval)
    })
  case outcome {
    // Step 23: GetWrappedValue(callerRealm, result.[[Value]]).
    NormalCompletion(v) -> get_wrapped_value(st, eval_realm, caller_realm, v)
    // Step 22: an abrupt completion becomes the caller realm's TypeError
    // (the original error must not cross the boundary).
    ThrowCompletion(thrown) ->
      rt_val.t_throw_type_error(
        st,
        "ShadowRealm.prototype.evaluate threw: "
          <> inspect.format_error(st, thrown),
      )
  }
}

// ── wrapped function exotic object [[Call]] — §2.1 ──────────────────────────

fn wrapped_function_call(
  st: Agent,
  target: Handle,
  caller_realm: Int,
  target_realm: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // Steps 1-3: the callee context's Realm is F.[[Realm]] — every TypeError
  // thrown here belongs to the caller realm.
  use st <- rt_realm.with_realm(st, caller_realm)
  // Steps 6-7: wrap thisArgument and every argument into the target realm.
  let #(wrapped_args, st) = wrap_all(st, caller_realm, target_realm, args)
  let #(wrapped_this, st) =
    get_wrapped_value(st, caller_realm, target_realm, this)
  // Step 8: Call(target, wrappedThisArgument, wrappedArgs) in the target
  // function's realm.
  let #(outcome, st) =
    protected_in_realm(st, target_realm, fn(st) {
      rt_call.t_call_checked(st, mk_object(target), wrapped_this, wrapped_args)
    })
  case outcome {
    // Step 9: GetWrappedValue(callerRealm, result).
    NormalCompletion(v) -> get_wrapped_value(st, target_realm, caller_realm, v)
    // Step 10: any abrupt completion becomes the caller realm's TypeError.
    ThrowCompletion(thrown) ->
      rt_val.t_throw_type_error(
        st,
        "wrapped function threw: " <> inspect.format_error(st, thrown),
      )
  }
}

// ── ShadowRealm.prototype.importValue ( specifier, exportName ) — §3.4.2 ───

/// Validation is fully implemented; the module load itself rejects, as a host
/// without a ShadowRealm module loader does (HostLoadImportedModule may fail:
/// the returned promise rejects with a TypeError).
fn import_value(
  st: Agent,
  own_realm: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // As in evaluate: the method's own realm brands errors and the promise.
  use st <- rt_realm.with_realm(st, own_realm)
  // Step 2: ValidateShadowRealmObject(O).
  let _eval_realm = require_shadow_realm(st, this, "importValue")
  let #(specifier, export_name) = helpers.two_args_or_undefined(args)
  // Step 3: ToString(specifier) — abrupt completions propagate as-is.
  let #(_specifier, st) = rt_val.t_to_string(st, specifier)
  // Step 4: exportName must already be a String (no coercion).
  case classify(export_name) {
    KStr(_) -> {
      let #(err, st) =
        st.store.ops.new_error(
          st,
          TypeErr,
          "ShadowRealm.prototype.importValue: module loading is not "
            <> "available in this host",
        )
      let #(promise, st) = rt_async.t_new_promise(st)
      let st = rt_async.t_promise_reject(st, promise, err)
      #(mk_object(promise), st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "ShadowRealm.prototype.importValue: exportName must be a string",
      )
  }
}
