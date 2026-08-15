//// `rt_call` — the JS `[[Call]]`/`[[Construct]]` MOP (SPEC §7.M-CALL).
////
//// Port of `arc/vm/exec/call.gleam:1258-1716` (`do_construct`/`call_value`) +
//// the constructor return-override rules from `arc/vm/exec/interpreter.gleam:
//// 3034-3071`, re-expressed over the threaded `Agent` model.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).**
////
//// **D7:** ops that throw JS errors RAISE via `rt_store.t_throw` (never
//// `Result`). `t_call` alone CATCHES the raise into a `Completion` so callers
//// (promise-reaction jobs, iterator drivers) can inspect the outcome without
//// installing their own try/catch; `t_call_checked` re-raises so a throw
//// propagates unchanged, and is the fn `init_realm` seeds into `JsOps.call`.
////
//// **D5 / R7:** `Frame` at the wire level is the plain untagged Erlang
//// 4-tuple `{This, ActiveFunc, HomeObject, NewTarget}` — the compiled
//// function prologue reads it via `element(N, Frame)` with 0-based logical
//// indices (R7). It is opaque to Gleam and built via the FFI `mk_frame/4`.

import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type CompiledFn, type FnFlags, type Handle, type JsOps, type JsVal,
  type NativeToken, type ObjKind, type Property, ArrayObj, DataProperty, Dense,
  JInt, JPosInf, KBound, KBytecode, KCompiled, KHandle, KNative, KNull, KNum,
  KStr, KTdz, KUndef, Named, NoElements, ProxyObj, ReferenceErr, SObject,
  StringKey, TypeErr, classify, mk_number, mk_object, mk_string, mk_tdz,
  mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import arc/vm/internal/tree_array
import arc/vm/limits
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// ── Frame / Completion ──────────────────────────────────────────────────────

/// Opaque call-frame. Wire = plain Erlang 4-tuple `{This, ActiveFunc,
/// HomeObject, NewTarget}` (D5/R7) — NOT a tagged Gleam record, so the
/// compiled prologue's `element(1..4, Frame)` reads fields directly.
pub type Frame

/// Build a Frame at wire level. All four positions are `JsVal` wire terms.
@external(erlang, "arc_rt_call_ffi", "mk_frame")
fn mk_frame(
  this: JsVal,
  active_func: JsVal,
  home_object: JsVal,
  new_target: JsVal,
) -> Frame

@external(erlang, "erlang", "element")
fn frame_element(n: Int, frame: Frame) -> JsVal

/// The active function of `frame` (the callee whose body it runs).
pub fn frame_active_func(frame: Frame) -> JsVal {
  frame_element(2, frame)
}

/// A JS call outcome — abrupt completions folded to just Throw (Return/Break/
/// Continue never cross a call boundary). `t_call` returns this so a caller
/// can observe a throw without a try/catch; `t_call_checked` re-raises Throw.
pub type Completion {
  NormalCompletion(JsVal)
  ThrowCompletion(JsVal)
}

// ── FFI seams ───────────────────────────────────────────────────────────────

/// Apply a `CompiledFn` under a try/catch, folding a `{wasm_exn,0,[St,V]}`
/// raise into `ThrowCompletion` (SPEC §7.M-CALL FFI; R2 payload order).
@external(erlang, "arc_rt_call_ffi", "t_call_protected")
fn t_call_protected(
  st: Agent,
  code: CompiledFn,
  frame: Frame,
  args: List(JsVal),
) -> #(Completion, Agent)

/// Run a Gleam thunk under the same try/catch as `t_call_protected` — for
/// native/bound/proxy dispatch, whose bodies may `t_throw` mid-evaluation.
@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn t_apply_protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(Completion, Agent)

/// M6 native-method dispatch (giant `case tag`). Forward-declared: gleam
/// check does not resolve `@external` targets (SPEC assumption).
@external(erlang, "arc_rt_builtins_ffi", "dispatch_native")
fn dispatch_native(
  st: Agent,
  tag: NativeToken,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent)

/// M6 native-constructor dispatch (`new Map()` etc). Forward-declared.
@external(erlang, "arc_rt_builtins_ffi", "dispatch_native_construct")
fn dispatch_native_construct(
  st: Agent,
  tag: NativeToken,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent)

// ── private access / throw helpers ──────────────────────────────────────────
//
// Realm intrinsics (u-gap-decisions / G18): `JsStore` has NO `realm` field; the
// `Realm` handle-record lives on `Agent.realm`, seeded once by `init_realm`.

/// The seeded `JsOps` upcall table (D17).
fn js_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

/// Allocate a native error of `kind(msg)` and RAISE it (D7). Never returns.
fn throw_error(st: Agent, kind: rt_types.ErrorKind, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, kind, msg)
  rt_store.t_throw(st, e)
}

/// `SObject(kind:)` at `h`, or `None` for a data cell (never callable or
/// constructible).
fn read_obj_kind(st: Agent, h: Handle) -> Option(ObjKind) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind:, ..) -> Some(kind)
    rt_types.SShapedObject(..) -> Some(rt_types.Ordinary)
    _ -> None
  }
}

// ── §7.2.3 IsCallable / §7.2.4 IsConstructor ────────────────────────────────

/// §7.2.3 IsCallable — thin wrapper over `rt_val.t_is_callable` returning
/// a bare Bool (R9: JRead).
pub fn is_callable(st: Agent, v: JsVal) -> Bool {
  let #(b, _) = rt_val.t_is_callable(st, v)
  b
}

/// §7.2.4 IsConstructor. `KCompiled` → `flags.is_constructor`; `KNative` →
/// `constructible`; `KBound` → recurse on target (§10.4.1.2); `ProxyObj` →
/// recurse on target (§10.5.13; a revoked proxy has no `[[Construct]]`).
/// R9: JRead — pure heap read.
pub fn is_constructor(st: Agent, v: JsVal) -> Bool {
  case classify(v) {
    KHandle(h) -> handle_is_constructor(st, h)
    _ -> False
  }
}

fn handle_is_constructor(st: Agent, h: Handle) -> Bool {
  case read_obj_kind(st, h) {
    Some(KCompiled(flags:, ..)) | Some(KBytecode(flags:, ..)) ->
      flags.is_constructor
    Some(KNative(constructible:, ..)) -> constructible
    Some(KBound(target:, ..)) -> handle_is_constructor(st, target)
    // §10.5.15 ProxyCreate step 7: [[Construct]] is installed iff the target
    // has it — and STAYS installed after revocation (arc `object.gleam:
    // 3106-3107`); §10.5.13 step 2 makes the CALL throw, not IsConstructor.
    Some(ProxyObj(target:, ..)) -> handle_is_constructor(st, target)
    _ -> False
  }
}

// ── `t_kfn_code` — CallClosure fast-path probe (JRead) ──────────────────────

/// Fast-path probe for the M9 `CallClosure` lowering. Returns
/// `{code, resolved_this}` iff `callee` is an ORDINARY user `KCompiled` —
/// not a class constructor, generator, async fn, or a method carrying a
/// `[[HomeObject]]` (whose [[Call]] needs the full `t_call_checked` MOP so
/// `super.x` resolves). Every other shape (native, bound, proxy, non-object,
/// non-callable) → `undefined`, and the emitted `TermTest(IsTuple, ·)` guard
/// falls back to `host("call")`. Folds §10.2.1.2 OrdinaryCallBindThis into
/// the SAME heap read so the fast path pays one `t_cell_get`, not two.
/// JRead — pure heap read, no St mutation. Implemented as an FFI so the
/// per-call hot path is one dict lookup + native pattern matches, no
/// cross-module `classify`/`t_realm`/`mk_object` chain.
@external(erlang, "arc_rt_call_ffi", "t_kfn_code")
pub fn t_kfn_code(st: Agent, callee: JsVal, this: JsVal) -> JsVal

// ── `t_call` — the ONE re-entry point (§10.2.1) ─────────────────────────────

/// §10.2.1 `[[Call]]`. Applies `callee(this, ...args)`, catching a JS throw
/// into `ThrowCompletion` — the ONE catching entry point every rt_js module
/// that runs user code goes through. Bracketed with `t_enter_call` /
/// `t_leave_call` so `call_depth > 0` gates the D11 GC safepoint. At
/// `limits.max_call_depth` the call is refused with a RangeError completion
/// (arc `call.gleam:174-179`, thrown in the caller's frame).
pub fn t_call(
  st: Agent,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent) {
  case st.store.call_depth >= limits.max_call_depth {
    True -> t_apply_protected(st, rt_store.stack_overflow)
    False -> {
      let st = rt_store.t_enter_call(st)
      let #(c, st) = do_call(st, callee, this, args)
      #(c, rt_store.t_leave_call(st))
    }
  }
}

fn do_call(
  st: Agent,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent) {
  case classify(callee) {
    KHandle(h) ->
      case read_obj_kind(st, h) {
        Some(KCompiled(code:, home_object:, flags:, ..)) ->
          call_kfunction(st, h, code, home_object, flags, this, args)
        // Interpreted function: the interpreter runs a fresh activation.
        Some(KBytecode(..)) ->
          t_apply_protected(st, fn(st) {
            js_ops(st).call_bytecode(st, h, this, args, mk_undefined())
          })
        Some(KNative(tag:, ..)) ->
          t_apply_protected(st, fn(st) { dispatch_native(st, tag, this, args) })
        // §10.4.1.1: [[BoundThis]] replaces `this`; bound args prepend.
        Some(KBound(target:, bound_this:, bound_args:)) ->
          do_call(
            st,
            mk_object(target),
            bound_this,
            list.append(bound_args, args),
          )
        // §10.5.12 Proxy [[Call]].
        Some(ProxyObj(target:, handler:, revoked:)) ->
          call_proxy(st, callee, target, handler, revoked, this, args)
        _ -> not_a_function(st, callee)
      }
    _ -> not_a_function(st, callee)
  }
}

fn call_kfunction(
  st: Agent,
  callee_h: Handle,
  code: CompiledFn,
  home_object: Option(Handle),
  flags: FnFlags,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent) {
  // §10.2.1 step 2: class constructors have no [[Call]] behaviour.
  case flags.is_class_constructor {
    True ->
      t_apply_protected(st, fn(st) {
        throw_error(
          st,
          TypeErr,
          "Class constructor cannot be invoked without 'new'",
        )
      })
    False -> {
      let home = case home_object {
        Some(h) -> mk_object(h)
        None -> mk_undefined()
      }
      let #(this_resolved, st) = resolve_this(st, flags, this)
      let frame =
        mk_frame(this_resolved, mk_object(callee_h), home, mk_undefined())
      t_call_protected(st, code, frame, args)
    }
  }
}

/// §10.2.1.2 OrdinaryCallBindThis. SPEC §7.M-CALL invariant: `this`
/// resolution happens HERE, not in the compiled prologue. Port of arc
/// `frame.bind_this` (`exec/frame.gleam:145-178`); arrows have lexical
/// `this` (step 2) and keep the caller-supplied frame value.
fn resolve_this(st: Agent, flags: FnFlags, this: JsVal) -> #(JsVal, Agent) {
  use <- bool.guard(flags.is_arrow, #(this, st))
  case flags.is_strict {
    // Step 5: thisMode is STRICT -> thisValue = thisArgument (no coercion).
    True -> #(this, st)
    // Step 6: Sloppy mode coercion.
    False ->
      case classify(this) {
        // Step 6a: undefined/null -> globalThis.
        KUndef | KNull -> #(mk_object(st.realm.global_object), st)
        // Step 6b: Objects pass through (ToObject is identity for objects).
        KHandle(_) -> #(this, st)
        // The TDZ sentinel is never a JS value: it is the OTHER input
        // ToObject rejects, and it must be matched here rather than falling
        // into the `_` arm below and escaping into user code as `this`.
        KTdz -> panic as "TDZ sentinel escaped as `this` in resolve_this"
        // Step 6b: Primitives -> ToObject wrapper (boxing). Every remaining
        // variant (string/number/bool/symbol/bigint) boxes.
        _ -> {
          let #(h, st) = js_ops(st).to_object(st, this)
          #(mk_object(h), st)
        }
      }
  }
}

/// §10.5.12 Proxy `[[Call]]`.
fn call_proxy(
  st: Agent,
  callee: JsVal,
  target: Handle,
  handler: Handle,
  revoked: Bool,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent) {
  t_apply_protected(st, fn(st) {
    // §10.5.14 step 6: only a proxy whose target is callable HAS [[Call]]
    // (installed at ProxyCreate time) — checked before revocation so a
    // revoked non-callable proxy is still "not a function".
    use <- bool.lazy_guard(!is_callable(st, mk_object(target)), fn() {
      not_a_function_raise(st, callee)
    })
    // Steps 1-5: revocation check + GetMethod(handler, "apply").
    let #(trap, st) = proxy_trap(st, handler, revoked, "apply")
    case trap {
      // Step 6: no trap → Call(target, thisArgument, argumentsList).
      None -> t_call_checked(st, mk_object(target), this, args)
      // Steps 7-8: Call(trap, handler, « target, thisArgument, argArray »).
      Some(trap_fn) -> {
        let #(args_arr, st) = alloc_args_array(st, args)
        t_call_checked(st, trap_fn, mk_object(handler), [
          mk_object(target),
          this,
          mk_object(args_arr),
        ])
      }
    }
  })
}

/// §10.5.14 ValidateNonRevokedProxy + §7.3.10 GetMethod(handler, name) for
/// the two call-side traps. `None` when the handler leaves the trap
/// undefined/null (forward to the target); TypeError on a revoked proxy or a
/// non-callable trap. Mirrors `rt_obj`'s private `proxy_trap`.
fn proxy_trap(
  st: Agent,
  handler: Handle,
  revoked: Bool,
  name: String,
) -> #(Option(JsVal), Agent) {
  use <- bool.lazy_guard(revoked, fn() {
    throw_error(
      st,
      TypeErr,
      "Cannot perform '" <> name <> "' on a proxy that has been revoked",
    )
  })
  let #(trap, st) =
    rt_obj.t_get_prop(st, mk_object(handler), StringKey(Named(name)))
  case classify(trap) {
    KUndef | KNull -> #(None, st)
    _ ->
      case is_callable(st, trap) {
        True -> #(Some(trap), st)
        False ->
          throw_error(
            st,
            TypeErr,
            "'" <> name <> "' trap of proxy handler is not a function",
          )
      }
  }
}

fn not_a_function(st: Agent, callee: JsVal) -> #(Completion, Agent) {
  t_apply_protected(st, fn(st) { not_a_function_raise(st, callee) })
}

fn not_a_function_raise(st: Agent, callee: JsVal) -> a {
  let #(ty, _) = rt_val.t_type_of(st, callee)
  throw_error(st, TypeErr, ty <> " is not a function")
}

/// `t_call`, then on `ThrowCompletion` re-raise via `t_throw` so the throw
/// propagates unchanged. This is the fn seeded into `JsOps.call` (D17) — its
/// `#(JsVal, st)` shape matches the field type.
pub fn t_call_checked(
  st: Agent,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case t_call(st, callee, this, args) {
    #(NormalCompletion(v), st) -> #(v, st)
    #(ThrowCompletion(e), st) -> rt_store.t_throw(st, e)
  }
}

/// §7.3.21 Invoke — `t_get_prop(recv, key)` then `t_call_checked` with
/// `this = recv`.
pub fn t_call_method(
  st: Agent,
  recv: JsVal,
  key: rt_types.ObjectKey,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(callee, st) = rt_obj.t_get_prop(st, recv, key)
  t_call_checked(st, callee, recv, args)
}

// ── `t_construct` — §10.2.2 [[Construct]] + return-override ─────────────────

/// §10.2.2 `[[Construct]]`. Gates on `IsConstructor` (§7.2.4) FIRST, then
/// dispatches on the callee's `ObjKind`. Return type is `#(Handle, St')` —
/// `[[Construct]]` always yields an object (§6.1.7.2), so a non-object
/// completion is coerced per the return-override rules and any residual
/// non-object is a TypeError.
pub fn t_construct(
  st: Agent,
  callee: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case classify(callee) {
    KHandle(callee_h) ->
      case handle_is_constructor(st, callee_h) {
        False -> not_a_constructor(st, callee)
        True -> construct_by_kind(st, callee_h, args, new_target)
      }
    _ -> not_a_constructor(st, callee)
  }
}

fn not_a_constructor(st: Agent, callee: JsVal) -> a {
  let #(ty, _) = rt_val.t_type_of(st, callee)
  throw_error(st, TypeErr, ty <> " is not a constructor")
}

/// Dispatch after the IsConstructor gate — every branch runs with
/// IsConstructor(callee) already true.
fn construct_by_kind(
  st: Agent,
  callee_h: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case read_obj_kind(st, callee_h) {
    Some(KCompiled(code:, home_object:, flags:, fields_init:, ..)) ->
      construct_kfunction(
        st,
        callee_h,
        code,
        home_object,
        flags,
        fields_init,
        args,
        new_target,
      )
    Some(KBytecode(..)) ->
      js_ops(st).construct_bytecode(st, callee_h, args, new_target)
    Some(KNative(tag:, ..)) ->
      dispatch_native_construct(st, tag, args, new_target)
    // §10.4.1.2 BoundFunction [[Construct]]: prepend bound args; if
    // SameValue(F, newTarget) then newTarget ← target.
    Some(KBound(target:, bound_args:, ..)) -> {
      let nt = case classify(new_target) {
        KHandle(nt_h) if nt_h == callee_h -> mk_object(target)
        _ -> new_target
      }
      t_construct(st, mk_object(target), list.append(bound_args, args), nt)
    }
    // §10.5.13 Proxy [[Construct]].
    Some(ProxyObj(target:, handler:, revoked:)) ->
      construct_proxy(st, target, handler, revoked, args, new_target)
    // Unreachable: IsConstructor admitted only the five kinds above.
    _ ->
      panic as "t_construct: IsConstructor passed but ObjKind not constructible"
  }
}

/// §10.2.2 ordinary-function [[Construct]] + the return-override rules from
/// arc `interpreter.gleam:3034-3071`.
fn construct_kfunction(
  st: Agent,
  callee_h: Handle,
  code: CompiledFn,
  home_object: Option(Handle),
  flags: FnFlags,
  fields_init: Option(Handle),
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let callee_v = mk_object(callee_h)
  // arc `call.gleam:1286/1298/1336`: [[Construct]] threads home_object into
  // the frame just like [[Call]] — `super.m()` in a ctor body reads it.
  let home = case home_object {
    Some(h) -> mk_object(h)
    None -> mk_undefined()
  }
  case flags.is_derived_constructor {
    // Derived: `this` starts in TDZ; `super()` (via SuperCall op) writes it.
    True -> {
      let frame = mk_frame(mk_tdz(), callee_v, home, new_target)
      let #(c, st) = apply_ctor(st, code, frame, args)
      derived_return_override(st, c)
    }
    // Base: §10.1.13 OrdinaryCreateFromConstructor, run field initializers,
    // then apply body.
    False -> {
      let #(proto, st) = get_prototype_from_constructor(st, new_target)
      let #(new_this, st) = rt_obj.t_new_object(st, Some(proto))
      let st = run_fields_init(st, fields_init, new_this)
      let frame = mk_frame(mk_object(new_this), callee_v, home, new_target)
      let #(c, st) = apply_ctor(st, code, frame, args)
      base_return_override(st, c, new_this)
    }
  }
}

/// Apply a constructor body under `t_call_protected`, bracketed with the D11
/// call-depth guard, and re-raise a Throw. Returns the body's `[[Value]]` as
/// a Completion so the caller applies return-override BEFORE re-raising —
/// but a Throw here IS re-raised (constructors never observe their own body
/// throw as a return-override input).
fn apply_ctor(
  st: Agent,
  code: CompiledFn,
  frame: Frame,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let st = rt_store.t_enter_call(st)
  let #(c, st) = t_call_protected(st, code, frame, args)
  let st = rt_store.t_leave_call(st)
  case c {
    NormalCompletion(v) -> #(v, st)
    ThrowCompletion(e) -> rt_store.t_throw(st, e)
  }
}

/// Base-constructor return override (§10.2.2 step 13 + arc
/// `interpreter.gleam:3037-3046`): an object result overrides `this`;
/// anything else — including `undefined` — yields the freshly allocated
/// `this`.
fn base_return_override(
  st: Agent,
  result: JsVal,
  new_this: Handle,
) -> #(Handle, Agent) {
  case classify(result) {
    KHandle(h) -> #(h, st)
    _ -> #(new_this, st)
  }
}

/// Derived-constructor return override (§10.2.2 steps 11-13 + arc
/// `interpreter.gleam:3048-3066`). M18 contract: emit lowers EVERY derived-
/// ctor return — bare `return;`, fall-through, AND `return <expr>` — to
/// `return is_undefined(v) ? this_local : v`, so the value here is always
/// the body's `this` binding or a non-undefined explicit return. Object →
/// return it; TDZ → ReferenceError (super never called); other primitive →
/// TypeError.
fn derived_return_override(st: Agent, result: JsVal) -> #(Handle, Agent) {
  case classify(result) {
    KHandle(h) -> #(h, st)
    KTdz ->
      throw_error(
        st,
        ReferenceErr,
        "Must call super constructor in derived class before returning from derived constructor",
      )
    // Unreachable under the M18 contract above — KUndef arriving here means
    // emit failed to substitute `this_local` for an undefined return.
    KUndef ->
      panic as "derived ctor returned KUndef — M18 return-lowering contract violated"
    _ ->
      throw_error(
        st,
        TypeErr,
        "Derived constructors may only return object or undefined",
      )
  }
}

/// §10.1.13.2 GetPrototypeFromConstructor: `? Get(newTarget, "prototype")`;
/// if not an object, fall back to `%Object.prototype%` (realm intrinsic via
/// `st.realm`).
fn get_prototype_from_constructor(
  st: Agent,
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_obj.t_get_prop(st, new_target, StringKey(Named("prototype")))
  case classify(proto) {
    KHandle(h) -> #(h, st)
    _ -> #(st.realm.object.prototype, st)
  }
}

/// §7.3.33 InitializeInstanceElements — call the class's synthesized
/// field-initializer function (if any) with `this = new_this`.
fn run_fields_init(
  st: Agent,
  fields_init: Option(Handle),
  new_this: Handle,
) -> Agent {
  case fields_init {
    None -> st
    Some(init_h) -> {
      let #(_, st) =
        t_call_checked(st, mk_object(init_h), mk_object(new_this), [])
      st
    }
  }
}

/// §10.5.13 Proxy `[[Construct]]`.
fn construct_proxy(
  st: Agent,
  target: Handle,
  handler: Handle,
  revoked: Bool,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  // Steps 1-5: revocation check + GetMethod(handler, "construct").
  let #(trap, st) = proxy_trap(st, handler, revoked, "construct")
  case trap {
    // Step 6: no trap → ? Construct(target, argumentsList, newTarget).
    None -> t_construct(st, mk_object(target), args, new_target)
    Some(trap_fn) -> {
      // Steps 7-8: Call(trap, handler, « target, argArray, newTarget »).
      let #(args_arr, st) = alloc_args_array(st, args)
      let #(res, st) =
        t_call_checked(st, trap_fn, mk_object(handler), [
          mk_object(target),
          mk_object(args_arr),
          new_target,
        ])
      // Step 9: If newObj is not an Object, throw a TypeError.
      case classify(res) {
        KHandle(h) -> #(h, st)
        _ ->
          throw_error(
            st,
            TypeErr,
            "'construct' on proxy: trap returned non-object",
          )
      }
    }
  }
}

// ── local helpers ───────────────────────────────────────────────────────────

/// Allocate a fresh dense `Array` holding `items` (proxy trap arg-arrays).
/// Proto = `%Array.prototype%` via `st.realm`.
fn alloc_args_array(st: Agent, items: List(JsVal)) -> #(Handle, Agent) {
  let len = list.length(items)
  let elements = case items {
    [] -> NoElements
    _ -> Dense(tree_array.from_list(items, rt_types.mk_hole()))
  }
  rt_store.t_cell_new(
    st,
    SObject(
      kind: ArrayObj(length: len),
      proto: Some(st.realm.array.prototype),
      props: dict.new(),
      symbol_props: [],
      elements:,
      extensible: True,
    ),
  )
}

// ── function-object allocation (u-fn-alloc; arc common.gleam:380-560) ───────
// `t_fn_new` / `t_native_new` / `t_bound_new` allocate an `SObject` cell whose
// `ObjKind` carries the [[Call]] slot, with the standard §20.2.4 `name` /
// `length` own properties. Do NOT root — lifetime is normal GC reachability
// (M6 pins intrinsics via `t_pin_root` after `t_native_new`).
//
// Birth-time property `seq`: arc uses constants 0/1/2 in a reserved range
// below its +16-offset counter (arc `common.gleam:528-536`). 2core's
// `prop_seq` starts at 0 with NO offset (`rt_store.gleam:49`), so a
// constant 0/1 would collide with the first user-added property's seq. We
// thread `t_next_prop_seq` per birth prop instead — two extra increments per
// allocation, but preserves the §10.1.11 "birth props before any later prop"
// ordering invariant without editing the frozen store.

/// §20.2.4.1 `length` own-property — `{W:F, E:F, C:T}`. `value` is a `JsVal`
/// (not `Int`) so `t_bound_new` can install `+∞` per §20.2.3.2 step 6.b.ii.
fn fn_length_prop(st: Agent, value: JsVal) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value:,
      writable: False,
      enumerable: False,
      configurable: True,
      seq:,
    ),
    st,
  )
}

/// §20.2.4.2 `name` own-property — `{W:F, E:F, C:T}`.
fn fn_name_prop(st: Agent, name: String) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value: mk_string(name),
      writable: False,
      enumerable: False,
      configurable: True,
      seq:,
    ),
    st,
  )
}

/// Shared allocator core: an `SObject` with the given callable `ObjKind`,
/// `proto`, and `length`+`name` own props (§10.2.9 SetFunctionLength runs
/// before §10.2.8 SetFunctionName in every OrdinaryFunctionCreate path, so
/// `length` gets the earlier seq). Port of arc `alloc_fn_slot`
/// (`common.gleam:479-495`). Does NOT root.
fn alloc_fn_cell(
  st: Agent,
  proto: Option(Handle),
  kind: ObjKind,
  length_v: JsVal,
  name: String,
) -> #(Handle, Agent) {
  let #(length_prop, st) = fn_length_prop(st, length_v)
  let #(name_prop, st) = fn_name_prop(st, name)
  rt_store.t_cell_new(
    st,
    SObject(
      kind:,
      proto:,
      props: dict.from_list([
        #(Named("length"), length_prop),
        #(Named("name"), name_prop),
      ]),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

/// Allocate a `KCompiled` cell for a compiled user function (D4). An
/// `SObject{kind: KCompiled{code, home_object: home, flags, fields_init:
/// None}, proto: %Function.prototype%}` with own `length`/`name`
/// per §20.2.4. Port of arc's function-object allocation shape via
/// `alloc_fn_slot`. Does NOT allocate a `.prototype` own property — §10.2.5
/// MakeConstructor is a separate step (M7/M14 responsibility). `fields_init`
/// starts `None`; `rt_class.t_set_fields_init` sets it on a class
/// constructor after class-body evaluation.
pub fn t_fn_new(
  st: Agent,
  code: CompiledFn,
  flags: FnFlags,
  name: String,
  len: Int,
  home: Option(Handle),
  simple: Option(#(CompiledFn, Int, Bool)),
) -> #(Handle, Agent) {
  alloc_fn_cell(
    st,
    Some(st.realm.function.prototype),
    KCompiled(code:, home_object: home, flags:, fields_init: None, simple:),
    mk_number(JInt(len)),
    name,
  )
}

/// SPEC§8 `fn_new` — the closure site of every compiled function; arg order
/// `(code, flags, name, len, simple)`. `name` arrives as the raw `BitArray`
/// (arc's `ir.ConstBinary`); `len` is a boxed `Int` from `ConstI32`.
/// `[[HomeObject]]` starts unset (`t_make_method` fills it for methods). The
/// function's [[Prototype]] follows its kind (§27.3.3 %GeneratorFunction
/// .prototype%, §27.4.3 %AsyncGeneratorFunction.prototype%, §27.7.3
/// %AsyncFunction.prototype%, else %Function.prototype%), and a generator
/// function also gets its own writable `prototype` object, inheriting from
/// %GeneratorPrototype% / %AsyncGeneratorPrototype% with no `constructor`
/// back-link (§15.5.3 / §15.6.3).
pub fn t_new_function(
  st: Agent,
  code: CompiledFn,
  flags: FnFlags,
  name: BitArray,
  len: Int,
  simple: Option(#(CompiledFn, Int, Bool)),
) -> #(JsVal, Agent) {
  let name_s = case bit_array.to_string(name) {
    Ok(s) -> s
    Error(Nil) -> ""
  }
  let realm = st.realm
  let proto = case flags.is_generator, flags.is_async {
    True, False -> realm.generator_fn.prototype
    True, True ->
      case
        rt_obj.t_ordinary_own_property(
          st,
          realm.async_gen.constructor,
          StringKey(Named("prototype")),
        )
      {
        Some(DataProperty(value:, ..)) ->
          case classify(value) {
            KHandle(p) -> p
            _ -> realm.function.prototype
          }
        _ -> realm.function.prototype
      }
    False, True -> realm.async_fn.prototype
    False, False -> realm.function.prototype
  }
  let #(h, st) =
    alloc_fn_cell(
      st,
      Some(proto),
      KCompiled(code:, home_object: None, flags:, fields_init: None, simple:),
      mk_number(JInt(len)),
      name_s,
    )
  let st = case flags.is_generator {
    False -> st
    True -> {
      let gen_proto = case flags.is_async {
        True -> realm.async_gen.prototype
        False -> realm.generator.prototype
      }
      let #(own_proto, st) = rt_obj.t_new_object(st, Some(gen_proto))
      let #(_, st) =
        rt_obj.t_define_own_data(
          st,
          h,
          StringKey(Named("prototype")),
          mk_object(own_proto),
          True,
          False,
          False,
        )
      st
    }
  }
  #(mk_object(h), st)
}

/// ES2024 §10.2.5 MakeConstructor — allocate an own writable `.prototype`
/// object on a plain function (FnDecl/FnExpr only; arrows/methods/class-ctors
/// never reach here). `proto` is a fresh ordinary object whose [[Prototype]]
/// is `%Object.prototype%`, with own `constructor` → `f` {W:T,E:F,C:T}. `f`
/// gains own `prototype` → `proto` {W:T,E:F,C:F} — writable, unlike a class
/// constructor's non-writable `.prototype` (§15.7.14 step 14; see
/// `rt_class.t_class_setup`). JMut pass-through: returns `f` unchanged so
/// M14's `emit_closure_site` can tail-call this after `fn_new`.
pub fn t_make_constructor(st: Agent, f: JsVal) -> #(JsVal, Agent) {
  let assert KHandle(fh) = classify(f)
  let #(proto, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      proto,
      StringKey(Named("constructor")),
      f,
      True,
      False,
      True,
    )
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      fh,
      StringKey(Named("prototype")),
      mk_object(proto),
      True,
      False,
      False,
    )
  #(f, st)
}

/// Allocate a `KNative` cell for a built-in function (M6 realm bootstrap).
/// Port of arc `alloc_call_fn` / `alloc_native_fn_props` (`common.gleam:
/// 431-475`). `constructible` is the [[Construct]] capability — `True` for
/// constructor intrinsics, `False` for methods/standalone functions. `proto`
/// is explicit (NOT the realm accessor) because M6 calls this DURING
/// bootstrap when `%Function.prototype%` is itself being allocated — the
/// caller passes `Some(function_proto_h)` once it exists, or `None` for
/// `%Function.prototype%` itself (whose [[Prototype]] is `%Object.prototype%`,
/// wired separately). Does NOT root; M6 pins via `t_pin_root`.
pub fn t_native_new(
  st: Agent,
  proto: Option(Handle),
  tag: NativeToken,
  name: String,
  len: Int,
  constructible: Bool,
) -> #(Handle, Agent) {
  alloc_fn_cell(
    st,
    proto,
    KNative(tag:, name:, length: len, constructible:),
    mk_number(JInt(len)),
    name,
  )
}

/// ES2024 §20.2.3.2 Function.prototype.bind steps 3-10 — allocate a `KBound`
/// cell. Port of arc `call.gleam:609-680`. Step 2's IsCallable gate is the
/// CALLER's responsibility (the `FunctionBind` native, M6 — it throws
/// TypeError before reaching here). Computes `length` per steps 4-6 and
/// `name` per steps 8-10; may re-enter user code via `[[Get]]` on `target`
/// (Proxy traps / accessors on `length`/`name`), which can throw —
/// propagates via `t_throw` (D7).
pub fn t_bound_new(
  st: Agent,
  target: Handle,
  bound_this: JsVal,
  bound_args: List(JsVal),
) -> #(Handle, Agent) {
  let target_v = mk_object(target)
  // Steps 4-6: L. Step 5 is `? HasOwnProperty(Target, "length")` — the
  // target's [[GetOwnProperty]] (a `getOwnPropertyDescriptor` trap on a
  // Proxy target); step 6.a is `? Get(Target, "length")`.
  let #(own_length, st) =
    rt_obj.t_get_own_property(st, target, StringKey(Named("length")))
  let #(target_len, st) = case own_length {
    Some(_) -> rt_obj.t_get_prop(st, target_v, StringKey(Named("length")))
    None -> #(mk_undefined(), st)
  }
  let n_args = list.length(bound_args)
  let length_v = case classify(target_len) {
    // Step 6.b.ii: +∞ → L = +∞.
    KNum(JPosInf) -> mk_number(JPosInf)
    // Step 6.b.iii-iv: L = max(ToIntegerOrInfinity(targetLen) - argCount, 0).
    // NaN / -∞ → 0 via `jsnum_to_integer_or_infinity`.
    KNum(n) ->
      mk_number(
        JInt(int.max(rt_val.jsnum_to_integer_or_infinity(n) - n_args, 0)),
      )
    // Step 6.b: non-Number → L stays 0 (from step 4).
    _ -> mk_number(JInt(0))
  }
  // Steps 8-10: targetName = ? Get(Target, "name"); non-String → "".
  let #(target_name, st) =
    rt_obj.t_get_prop(st, target_v, StringKey(Named("name")))
  let bound_name = case classify(target_name) {
    KStr(s) -> "bound " <> s
    _ -> "bound "
  }
  // Step 3: BoundFunctionCreate. Proto = %Function.prototype% (§10.4.1.3
  // step 2 uses the target's [[Prototype]] — for every callable that IS
  // %Function.prototype%; a Proxy-of-function corner case is M6 territory).
  alloc_fn_cell(
    st,
    Some(st.realm.function.prototype),
    KBound(target:, bound_this:, bound_args:),
    length_v,
    bound_name,
  )
}
