//// ES2024 §25.4 The Atomics Object
////
//// A namespace object (no constructor) — a single ordinary object with 14
//// method properties. All operations REQUIRE a TypedArray view over a
//// SharedArrayBuffer; SharedArrayBuffer is OUT of scope for this Realm shape
//// (SPEC ASSUMPTION §7.M6 — Realm has no `shared_array_buffer` field), so
//// every RMW/load/store/wait dispatch arm throws TypeError, matching arc's
//// `require_shared_int_view` on a plain-ArrayBuffer view. `isLockFree`/`pause`
//// are pure. Port of arc `builtins/atomics.gleam:68-121` init/dispatch
//// re-expressed under D7 (`Error(e)` → `t_throw`) and R1 (`#(V, St')`).

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type AtomicsNative, type Handle, type JsVal, AtomicsAdd,
  AtomicsAnd, AtomicsCompareExchange, AtomicsExchange, AtomicsIsLockFree,
  AtomicsLoad, AtomicsN, AtomicsNotify, AtomicsOr, AtomicsPause, AtomicsStore,
  AtomicsSub, AtomicsWait, AtomicsWaitAsync, AtomicsXor, JFloat, JInt, KHandle,
  KNum, KUndef, SObject, TypedArrayObj, classify, mk_bool, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/option.{None, Some}

// ═══════════════════════════════════════════════════════════════════════════
// Init — the Atomics namespace object
// ═══════════════════════════════════════════════════════════════════════════

/// Allocate the `Atomics` namespace object (§25.4). Returns the single Handle
/// for `realm.atomics`; there is no constructor/prototype pair.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(Handle, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("add", AtomicsN(AtomicsAdd), 3),
      #("and", AtomicsN(AtomicsAnd), 3),
      #("compareExchange", AtomicsN(AtomicsCompareExchange), 4),
      #("exchange", AtomicsN(AtomicsExchange), 3),
      #("isLockFree", AtomicsN(AtomicsIsLockFree), 1),
      #("load", AtomicsN(AtomicsLoad), 2),
      #("notify", AtomicsN(AtomicsNotify), 3),
      #("or", AtomicsN(AtomicsOr), 3),
      #("pause", AtomicsN(AtomicsPause), 0),
      #("store", AtomicsN(AtomicsStore), 3),
      #("sub", AtomicsN(AtomicsSub), 3),
      #("wait", AtomicsN(AtomicsWait), 4),
      #("waitAsync", AtomicsN(AtomicsWaitAsync), 4),
      #("xor", AtomicsN(AtomicsXor), 3),
    ])
  common.init_namespace(st, object_proto, "Atomics", methods)
}

// ═══════════════════════════════════════════════════════════════════════════
// Dispatch
// ═══════════════════════════════════════════════════════════════════════════

/// Per-module dispatch for Atomics native functions.
pub fn dispatch(
  st: Agent,
  native: AtomicsNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    AtomicsIsLockFree -> is_lock_free(st, args)
    AtomicsPause -> pause(st, args)
    AtomicsAdd
    | AtomicsAnd
    | AtomicsCompareExchange
    | AtomicsExchange
    | AtomicsLoad
    | AtomicsNotify
    | AtomicsOr
    | AtomicsStore
    | AtomicsSub
    | AtomicsWait
    | AtomicsWaitAsync
    | AtomicsXor -> require_shared_int_view(st, args)
  }
}

/// §25.4.10 Atomics.isLockFree ( size ) — pure predicate. arc reports 1/2/4/8
/// as lock-free (`atomics.gleam:261-274`).
fn is_lock_free(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(n, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  #(mk_bool(n == 1 || n == 2 || n == 4 || n == 8), st)
}

/// §25.4.12 Atomics.pause ( [ N ] ) — no-op, but validates N is undefined or
/// an integral Number (arc `atomics.gleam:786-800`).
fn pause(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KUndef -> #(mk_undefined(), st)
    KNum(JInt(_)) -> #(mk_undefined(), st)
    KNum(JFloat(f)) ->
      case rt_val.integral_int(f) {
        Some(_) -> #(mk_undefined(), st)
        None ->
          rt_val.t_throw_type_error(st, "Atomics.pause: not an integral number")
      }
    _ -> rt_val.t_throw_type_error(st, "Atomics.pause: not an integral number")
  }
}

/// §25.4.3.1 ValidateIntegerTypedArray → §25.4.2.3
/// ValidateSharedIntegerTypedArray. Without SharedArrayBuffer in the Realm every
/// buffer is non-shared, so this ALWAYS throws TypeError — matching arc's
/// `require_shared_int_view` on a plain-ArrayBuffer view.
fn require_shared_int_view(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(helpers.first_arg_or_undefined(args)) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: TypedArrayObj(..), ..) ->
          rt_val.t_throw_type_error(
            st,
            "Atomics operation on non-shared ArrayBuffer",
          )
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Atomics operation called on non-TypedArray",
          )
      }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "Atomics operation called on non-TypedArray",
      )
  }
}
