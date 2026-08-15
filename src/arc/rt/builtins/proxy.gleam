//// ES2024 §28.2 The Proxy Constructor
////
//// Proxy has NO `.prototype` property (§28.2.2 "the Proxy constructor does not
//// have a prototype property"), and instances have no shared prototype — every
//// proxy's [[Prototype]] is whatever its target's [[GetPrototypeOf]] trap
//// returns. `realm.proxy` is a `BuiltinPair` for record-shape uniformity;
//// `.prototype` is set to `%Object.prototype%` and never installed on the ctor.
//// Port of arc `builtins.gleam:255-279` inline Proxy init + arc
//// `call.gleam:proxy_constructor/proxy_revocable/proxy_revoke`.

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type ProxyNative,
  BuiltinPair, KHandle, NoElements, ProxyConstructor, ProxyN, ProxyObj,
  ProxyRevocable, ProxyRevoke, SObject, classify, mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{None, Some}

// ═══════════════════════════════════════════════════════════════════════════
// Init — the Proxy constructor
// ═══════════════════════════════════════════════════════════════════════════

/// Allocate the `Proxy` constructor. §28.2.2: no `prototype` own property, so
/// bypass `common.init_type` (which always installs one) — build the ctor
/// directly with `Proxy.revocable` as its only extra static.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  // Proxy.revocable ( target, handler ) — §28.2.2.1.
  let #(revocable_h, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      ProxyN(ProxyRevocable),
      "revocable",
      2,
    )
  let #(revocable_prop, st) =
    common.builtin_property(st, mk_object(revocable_h))
  let #(len_p, st) = common.fn_length_property(st, 2)
  let #(name_p, st) = common.fn_name_property(st, "Proxy")
  // The constructor itself. `constructible: True`; NO `prototype` own prop.
  let #(ctor_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: rt_types.KNative(
          tag: ProxyN(ProxyConstructor),
          name: "Proxy",
          length: 2,
          constructible: True,
        ),
        proto: Some(fn_proto),
        props: common.named_props([
          #("length", len_p),
          #("name", name_p),
          #("revocable", revocable_prop),
        ]),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = rt_store.t_pin_root(st, ctor_h)
  #(BuiltinPair(prototype: object_proto, constructor: ctor_h), st)
}

// ═══════════════════════════════════════════════════════════════════════════
// Dispatch
// ═══════════════════════════════════════════════════════════════════════════

/// Per-module [[Call]] dispatch. `Proxy()` without `new` throws (§28.2.1.1).
pub fn dispatch(
  st: Agent,
  native: ProxyNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    ProxyConstructor ->
      rt_val.t_throw_type_error(st, "Constructor Proxy requires 'new'")
    ProxyRevocable -> proxy_revocable(st, args)
    ProxyRevoke(proxy:) -> proxy_revoke(st, proxy)
  }
}

/// Per-module [[Construct]] dispatch.
pub fn dispatch_construct(
  st: Agent,
  native: ProxyNative,
  args: List(JsVal),
  _new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    ProxyConstructor -> proxy_create(st, args)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

/// §10.5.14 ProxyCreate ( target, handler ).
fn proxy_create(st: Agent, args: List(JsVal)) -> #(Handle, Agent) {
  let #(target_v, handler_v) = helpers.two_args_or_undefined(args)
  let target = require_object(st, target_v, "Proxy target")
  let handler = require_object(st, handler_v, "Proxy handler")
  rt_store.t_cell_new(
    st,
    SObject(
      kind: ProxyObj(target:, handler:, revoked: False),
      proto: None,
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

/// §28.2.2.1 Proxy.revocable ( target, handler ).
fn proxy_revocable(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(proxy_h, st) = proxy_create(st, args)
  let realm = st.realm
  // Step 3-6: allocate the revocation function closing over the proxy. NOT
  // rooted — the revoker's lifetime follows the returned {proxy,revoke} object
  // (arc `call.gleam` uses non-rooting `alloc_call_fn`; pinning here leaks
  // revoker+proxy+target+handler forever via `proxy_native_refs`).
  let #(revoker_h, st) =
    rt_call.t_native_new(
      st,
      Some(realm.function.prototype),
      ProxyN(ProxyRevoke(proxy: proxy_h)),
      "",
      0,
      False,
    )
  // Step 7-9: `{ proxy, revoke }` — plain data props {W:T, E:T, C:T}.
  let #(result_h, st) =
    common.alloc_pojo(st, realm.object.prototype, [
      #("proxy", mk_object(proxy_h)),
      #("revoke", mk_object(revoker_h)),
    ])
  #(mk_object(result_h), st)
}

/// §28.2.2.1.1 Proxy revocation function — flips `revoked` on the closed-over
/// proxy. Idempotent (already-revoked is a no-op).
fn proxy_revoke(st: Agent, proxy: Handle) -> #(JsVal, Agent) {
  let st =
    rt_store.t_cell_update(st, proxy, fn(slot) {
      case slot {
        SObject(kind: ProxyObj(target:, handler:, ..), ..) ->
          SObject(..slot, kind: ProxyObj(target:, handler:, revoked: True))
        _ -> slot
      }
    })
  #(mk_undefined(), st)
}

fn require_object(st: Agent, v: JsVal, what: String) -> Handle {
  case classify(v) {
    KHandle(h) -> h
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Cannot create proxy with a non-object as " <> what,
      )
  }
}
