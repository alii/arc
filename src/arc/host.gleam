//// Helpers for writing host functions.
////
//// A host function is `fn(args, this, State(host)) -> #(State(host),
//// Result(JsVal, JsVal))`: `Ok(v)` returns `v` to JS, `Error(e)` throws `e`.
//// It runs wherever JS calls it from (script code, a builtin callback such
//// as `Array.prototype.map`, another host function) and always sees the
//// whole runtime through `State.agent`.
////
//// Validators are strict type checks that throw TypeError on mismatch,
//// designed for `use` syntax and modeled after Node's `internal/validators`.
//// Error format:
////   The "NAME" argument must be of type EXPECTED. Received type ACTUAL
////
//// Usage:
////
////     fn host_repeat(args, _this, s) {
////       use str, s <- host.validate_string(s, host.first_arg(args), "str")
////       use n, s <- host.validate_integer(s, host.arg_at(args, 1), "count", 0, 1_000_000)
////       #(s, Ok(types.mk_string(string.repeat(str, n))))
////     }

import arc/host_hooks
import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type HostTerm, type JsVal, type Property, Agent,
  HostFnEntry, JFloat, JInt, KBool, KHandle, KHost, KNum, KStr, NoElements,
  RangeErr, SObject, StringKey, TypeErr, classify, mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// -- State -------------------------------------------------------------------

/// What a host function threads through. `agent` is the whole runtime state:
/// the `arc/rt/*` operations take and return an `Agent`, so host code that
/// drops to that layer rebuilds this record with the agent it gets back.
/// `new_target` is NewTarget of the [[Construct]] this function is serving,
/// `undefined` under a plain call.
///
/// `host` is the embedder's payload type for `alloc_host_object` /
/// `read_host`. The runtime stores payloads erased (`KHost`), so this
/// parameter is the only thing tying a read back to the type that was
/// written; one engine uses one `host` throughout.
pub type State(host) {
  State(agent: Agent, new_target: JsVal)
}

/// Signature of an embedder native as `function` / `class` /
/// `define_fn` register it.
pub type HostFn(host) =
  fn(List(JsVal), JsVal, State(host)) -> #(State(host), Result(JsVal, JsVal))

/// Wrap an agent for host code that is not inside a host-function call
/// (`engine.with_state`, tests). No [[Construct]] is in progress, so
/// `new_target` is `undefined`.
pub fn from_agent(agent: Agent) -> State(host) {
  State(agent:, new_target: mk_undefined())
}

/// NewTarget of the [[Construct]] being served: the leaf class for
/// `class Sub extends HostClass {}`, the class itself for `new HostClass()`,
/// `undefined` when called without `new`.
pub fn new_target(s: State(host)) -> JsVal {
  s.new_target
}

/// Run `body` against `agent` the way a host function would see it, then
/// drain the microtask queue so promise reactions it triggered settle before
/// control returns to the embedder.
pub fn with_state(
  agent: Agent,
  body: fn(State(host)) -> #(State(host), a),
) -> #(Agent, a) {
  let #(State(agent:, ..), result) = body(from_agent(agent))
  #(rt_async.drain(agent), result)
}

// -- Argument access ---------------------------------------------------------
//
// A missing argument is `undefined`, per JS semantics.

/// The first argument, or `undefined` when the caller passed none.
pub const first_arg = helpers.first_arg_or_undefined

/// The i-th argument (0-based), or `undefined` when the caller passed fewer.
pub const arg_at = helpers.arg_at

// -- Throwing ----------------------------------------------------------------

/// Throw a `TypeError` with `msg`. The dispatch-shaped `#(state, Error(...))`.
pub fn type_error(
  s: State(host),
  msg: String,
) -> #(State(host), Result(JsVal, JsVal)) {
  throw_new(s, TypeErr, msg)
}

/// Throw a `RangeError` with `msg`. The dispatch-shaped `#(state, Error(...))`.
pub fn range_error(
  s: State(host),
  msg: String,
) -> #(State(host), Result(JsVal, JsVal)) {
  throw_new(s, RangeErr, msg)
}

fn throw_new(
  s: State(host),
  kind: rt_types.ErrorKind,
  msg: String,
) -> #(State(host), Result(JsVal, JsVal)) {
  let st = s.agent
  let #(err, st) = st.store.ops.new_error(st, kind, msg)
  #(State(..s, agent: st), Error(err))
}

// -- Validators --------------------------------------------------------------

/// Reject unless `val` is a JS string. Unwraps to the Gleam `String`.
pub fn validate_string(
  s: State(host),
  val: JsVal,
  name: String,
  cont: fn(String, State(host)) -> #(State(host), Result(JsVal, JsVal)),
) -> #(State(host), Result(JsVal, JsVal)) {
  case classify(val) {
    KStr(str) -> cont(str, s)
    _ -> invalid_arg_type(s, name, "string", val)
  }
}

/// Reject unless `val` is callable. Passes the value through unchanged;
/// hand it to `call` to invoke. Use this when you call the function more
/// than once (validate once, call many). For one-shot calls, `try_call`
/// does both in one step.
pub fn validate_function(
  s: State(host),
  val: JsVal,
  name: String,
  cont: fn(JsVal, State(host)) -> #(State(host), Result(JsVal, JsVal)),
) -> #(State(host), Result(JsVal, JsVal)) {
  case rt_call.is_callable(s.agent, val) {
    True -> cont(val, s)
    False -> invalid_arg_type(s, name, "function", val)
  }
}

/// Call `callee` with `this_val` and `args`. `Error` is the thrown value;
/// return it as-is to rethrow, or inspect it to recover.
pub fn call(
  s: State(host),
  callee: JsVal,
  this_val: JsVal,
  args: List(JsVal),
) -> #(State(host), Result(JsVal, JsVal)) {
  let #(completion, st) = rt_call.t_call(s.agent, callee, this_val, args)
  let s = State(..s, agent: st)
  case completion {
    NormalCompletion(v) -> #(s, Ok(v))
    ThrowCompletion(thrown) -> #(s, Error(thrown))
  }
}

/// Validate callability AND call: if `callee` isn't callable, throws
/// TypeError naming the argument; otherwise calls it and continues with the
/// result, or propagates the throw.
pub fn try_call(
  s: State(host),
  callee: JsVal,
  name: String,
  this_val: JsVal,
  args: List(JsVal),
  cont: fn(JsVal, State(host)) -> #(State(host), Result(JsVal, JsVal)),
) -> #(State(host), Result(JsVal, JsVal)) {
  case rt_call.is_callable(s.agent, callee) {
    False -> invalid_arg_type(s, name, "function", callee)
    True -> {
      let #(s, result) = call(s, callee, this_val, args)
      case result {
        Ok(v) -> cont(v, s)
        Error(thrown) -> #(s, Error(thrown))
      }
    }
  }
}

/// Reject unless `val` is an integer-valued JS number within `[min, max]`.
/// Unwraps to `Int`. Three rejections, and they are NOT the same error:
///
///   * not a number at all (`"3"`, `{}`, `undefined`) → **TypeError**, "must
///     be of type integer. Received type <typeof>";
///   * a number, but not an integer (`1.5`, `NaN`, `Infinity`) →
///     **RangeError**, "must be an integer";
///   * an integer outside `[min, max]` → **RangeError**, "must be >= min and
///     <= max".
pub fn validate_integer(
  s: State(host),
  val: JsVal,
  name: String,
  min: Int,
  max: Int,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsVal, JsVal)),
) -> #(State(host), Result(JsVal, JsVal)) {
  case classify(val) {
    KNum(JInt(i)) -> check_range(s, name, i, min, max, cont)
    KNum(JFloat(f) as n) ->
      case rt_val.integral_int(f) {
        Some(i) -> check_range(s, name, i, min, max, cont)
        None -> not_an_integer(s, name, rt_val.jsnum_to_string(n))
      }
    // NaN / ±Infinity: a number, just not an integral one.
    KNum(n) -> not_an_integer(s, name, rt_val.jsnum_to_string(n))
    _ -> invalid_arg_type(s, name, "integer", val)
  }
}

fn check_range(
  s: State(host),
  name: String,
  i: Int,
  min: Int,
  max: Int,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsVal, JsVal)),
) -> #(State(host), Result(JsVal, JsVal)) {
  case i >= min && i <= max {
    True -> cont(i, s)
    False ->
      range_error(
        s,
        "The value of \""
          <> name
          <> "\" is out of range. It must be >= "
          <> int.to_string(min)
          <> " and <= "
          <> int.to_string(max)
          <> ". Received "
          <> int.to_string(i),
      )
  }
}

/// Reject unless `val` is a JS boolean. Unwraps to `Bool`.
pub fn validate_boolean(
  s: State(host),
  val: JsVal,
  name: String,
  cont: fn(Bool, State(host)) -> #(State(host), Result(JsVal, JsVal)),
) -> #(State(host), Result(JsVal, JsVal)) {
  case classify(val) {
    KBool(b) -> cont(b, s)
    _ -> invalid_arg_type(s, name, "boolean", val)
  }
}

// -- Host hooks --------------------------------------------------------------

/// Re-export: one blocking sync Atomics.wait handed to the embedder.
pub type WaitRequest =
  host_hooks.WaitRequest

/// Re-export: result of an embedder blocking wait.
pub type WaitOutcome =
  host_hooks.WaitOutcome

/// Re-export: the blocking-wait capability, `fn(WaitRequest) -> WaitOutcome`.
pub type SyncWaitFn =
  host_hooks.SyncWaitFn

/// Re-export: the wake-delivery capability for claimed remote waiters.
pub type DeliverWakeFn =
  host_hooks.DeliverWakeFn

/// Re-export: opaque claimed-waiter term (pid + ref + key + byte index).
pub type ClaimedWaiter =
  host_hooks.ClaimedWaiter

/// Re-export: opaque cross-process WaiterList identity.
pub type WaiterKey =
  host_hooks.WaiterKey

/// Re-export: opaque handle to one registered waiterlist entry.
pub type WaiterHandle =
  host_hooks.WaiterHandle

/// Re-export: the bundled blocking-wait + wake-delivery capability pair.
pub type AtomicsCapabilities =
  host_hooks.AtomicsCapabilities

/// Re-export: the embedder host-capability record. Start from
/// `default_host_hooks()`, add capabilities, hand it to the engine once.
pub type HostHooks =
  host_hooks.HostHooks

/// The capability-free default: no Atomics capabilities (sync `Atomics.wait`
/// throws instead of hanging), no dynamic-import hook, and the real BEAM
/// monotonic clock / sleep.
pub fn default_host_hooks() -> HostHooks {
  host_hooks.default_host_hooks()
}

/// Install the Atomics blocking-wait + wake-delivery capabilities on `hooks`,
/// leaving every other hook as configured. Both together, always: a host
/// that blocks but cannot deliver wakes (or vice versa) deadlocks its peer
/// agents, so `HostHooks.atomics` is one `Option(AtomicsCapabilities)`.
pub fn with_atomics(
  hooks: HostHooks,
  sync_wait sync_wait: SyncWaitFn,
  deliver_wake deliver_wake: DeliverWakeFn,
) -> HostHooks {
  host_hooks.HostHooks(
    ..hooks,
    atomics: Some(host_hooks.AtomicsCapabilities(sync_wait:, deliver_wake:)),
  )
}

// -- Constructors ------------------------------------------------------------
//
// Primitives come from `arc/rt/types` (`mk_string`, `mk_number`, `mk_bool`,
// ...) and are read back with its `classify`.

/// Allocate a JS array from values. Uses the realm's Array.prototype.
pub fn array(s: State(host), values: List(JsVal)) -> #(State(host), JsVal) {
  let st = s.agent
  let #(h, st) = common.alloc_array(st, values, st.realm.array.prototype)
  #(State(..s, agent: st), mk_object(h))
}

/// Allocate a plain JS object from a property list. Uses Object.prototype.
pub fn object(
  s: State(host),
  props: List(#(String, JsVal)),
) -> #(State(host), JsVal) {
  let st = s.agent
  let #(h, st) = common.alloc_pojo(st, st.realm.object.prototype, props)
  #(State(..s, agent: st), mk_object(h))
}

// -- Opaque host values ------------------------------------------------------

@external(erlang, "gleam_stdlib", "identity")
fn erase(value: host) -> HostTerm

@external(erlang, "gleam_stdlib", "identity")
fn unerase(term: HostTerm) -> host

/// Allocate an opaque, embedder-owned object wrapping `value` (the
/// embedder's own type). The engine never inspects `value`; it renders the
/// object via the prototype's `@@toStringTag`. The object has no own
/// properties; pass `Some(proto)` to give it methods/a tag, or `None` for a
/// null-prototype value. Read it back, typed, with `read_host`.
///
/// Heap handles inside `value` are traced by the collector, so a payload
/// may hold JS objects directly.
pub fn alloc_host_object(
  s: State(host),
  value: host,
  prototype: Option(Handle),
) -> #(State(host), JsVal) {
  let #(h, st) =
    rt_store.t_cell_new(
      s.agent,
      SObject(
        kind: KHost(payload: erase(value)),
        proto: prototype,
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(State(..s, agent: st), mk_object(h))
}

/// Read the embedder value out of a host object. `None` if `val` is not one.
pub fn read_host(s: State(host), val: JsVal) -> Option(host) {
  use h <- option.then(handle_of(val))
  case rt_store.t_cell_get(s.agent, h) {
    SObject(kind: KHost(payload:), ..) -> Some(unerase(payload))
    _ -> None
  }
}

fn handle_of(val: JsVal) -> Option(Handle) {
  case classify(val) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}

// -- Native functions and classes --------------------------------------------

/// Mint a standalone native function object without installing it
/// anywhere. `impl` is an arbitrary closure, so it can capture typed host
/// data. `arity` is the reported `.length`. The object is GC-rooted.
pub fn function(
  s: State(host),
  name: String,
  arity: Int,
  impl: HostFn(host),
) -> #(State(host), JsVal) {
  let #(id, st) = register(s.agent, name, impl)
  let #(h, st) =
    common.alloc_rooted_native_fn(
      st,
      st.realm.function.prototype,
      rt_types.HostFn(id),
      name,
      arity,
    )
  #(State(..s, agent: st), mk_object(h))
}

/// `function` + `define_global`: the function becomes callable from JS as
/// `name(...)`.
pub fn define_fn(
  s: State(host),
  name: String,
  arity: Int,
  impl: HostFn(host),
) -> State(host) {
  let #(s, f) = function(s, name, arity, impl)
  define_global(s, name, f)
}

/// Install `val` on `globalThis` as a writable, configurable, non-enumerable
/// data property (the attributes every built-in global has).
pub fn define_global(s: State(host), name: String, val: JsVal) -> State(host) {
  let st = s.agent
  let #(_created, st) =
    rt_obj.t_define_own_data(
      st,
      st.realm.global_object,
      StringKey(rt_types.canonical_key(name)),
      val,
      True,
      False,
      True,
    )
  State(..s, agent: st)
}

/// Install a namespace object (like `Math`) at global `name` whose own
/// properties are the `#(name, arity, impl)` methods. It carries
/// `@@toStringTag = name` like every built-in namespace.
pub fn define_namespace(
  s: State(host),
  name: String,
  methods: List(#(String, Int, HostFn(host))),
) -> State(host) {
  let st = s.agent
  let #(props, st) = alloc_host_methods(st, methods)
  let #(ns, st) =
    common.init_namespace(st, st.realm.object.prototype, name, props)
  define_global(State(..s, agent: st), name, mk_object(ns))
}

/// Build a constructible class that JS can `new` and `extends`, and return
/// its constructor (nothing is installed on the global).
///
/// `constructor` is the [[Construct]] body: it receives `(args, this,
/// state)` with `this` undefined and `new_target(state)` set, and returns
/// the new instance, typically from `object` or `alloc_host_object`. The
/// instance is re-prototyped to `new_target.prototype`, so a plain
/// `object(s, [...])` already comes out as an instance of the class or of
/// the JS subclass being constructed. Calling the class without `new` runs
/// the same body with `new_target` undefined. `methods` go on the
/// prototype; `statics` on the constructor (and are inherited by
/// subclasses). Constructor and prototype are GC-rooted.
pub fn class(
  s: State(host),
  name: String,
  arity: Int,
  constructor: HostFn(host),
  methods: List(#(String, Int, HostFn(host))),
  statics: List(#(String, Int, HostFn(host))),
) -> #(State(host), JsVal) {
  let st = s.agent
  let realm = st.realm
  let #(proto_props, st) = alloc_host_methods(st, methods)
  let #(static_props, st) = alloc_host_methods(st, statics)
  let #(id, st) = register(st, name, constructor)
  let #(pair, st) =
    common.init_type(
      st,
      realm.object.prototype,
      realm.function.prototype,
      proto_props,
      fn(_proto) { rt_types.HostFn(id) },
      name,
      arity,
      static_props,
    )
  #(State(..s, agent: st), mk_object(pair.constructor))
}

/// Add `impl` to the agent's host-function table under the next id. Ids are
/// dense and assigned in registration order, which is what lets a
/// deserialized engine's `HostFn(id)` cells find their closures again once
/// the embedder repeats its registrations.
fn register(st: Agent, name: String, impl: HostFn(host)) -> #(Int, Agent) {
  let id = dict.size(st.host_fns)
  let entry =
    HostFnEntry(name:, call: fn(agent, args, this, new_target) {
      let #(State(agent:, ..), result) =
        impl(args, this, State(agent:, new_target:))
      #(agent, result)
    })
  #(id, Agent(..st, host_fns: dict.insert(st.host_fns, id, entry)))
}

fn alloc_host_methods(
  st: Agent,
  specs: List(#(String, Int, HostFn(host))),
) -> #(List(#(String, Property)), Agent) {
  let #(props, st) =
    list.fold(specs, #([], st), fn(acc, spec) {
      let #(props, st) = acc
      let #(name, arity, impl) = spec
      let #(id, st) = register(st, name, impl)
      let #(h, st) =
        common.alloc_rooted_native_fn(
          st,
          st.realm.function.prototype,
          rt_types.HostFn(id),
          name,
          arity,
        )
      let #(prop, st) = common.builtin_property(st, mk_object(h))
      #([#(name, prop), ..props], st)
    })
  #(list.reverse(props), st)
}

// -- Internal ----------------------------------------------------------------

/// A number that is not an integer (1.5, NaN, ±Infinity): the VALUE is out of
/// range, the type is fine.
fn not_an_integer(
  s: State(host),
  name: String,
  received: String,
) -> #(State(host), Result(JsVal, JsVal)) {
  range_error(
    s,
    "The value of \""
      <> name
      <> "\" is out of range. It must be an integer. Received "
      <> received,
  )
}

fn invalid_arg_type(
  s: State(host),
  name: String,
  expected: String,
  received: JsVal,
) -> #(State(host), Result(JsVal, JsVal)) {
  let #(actual, _) = rt_val.t_type_of(s.agent, received)
  type_error(
    s,
    "The \""
      <> name
      <> "\" argument must be of type "
      <> expected
      <> ". Received type "
      <> actual,
  )
}
