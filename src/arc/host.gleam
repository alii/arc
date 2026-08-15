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
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type HostTerm, type JsVal, type Property, Agent,
  HostFnEntry, HostJob, JFloat, JInt, KBool, KHandle, KHost, KNum, KStr,
  NoElements, PromiseObj, RangeErr, SObject, StringKey, TypeErr, classify,
  mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// -- State -------------------------------------------------------------------

/// Names the embedder's payload type `host` for `alloc_host_object` /
/// `read_host`. `Agent` is not generic over `host` and stores payloads
/// erased (`KHost`), so this key is what pins the type: every `State(host)`
/// carries one, host functions are registered under the key of the state
/// that defined them, and each host object records the key it was written
/// under. Mint it once with `new_key` and use that one value everywhere;
/// sharing the value is what makes a mistyped read a compile error. A read
/// under a different key (a second `new_key()`, or a key minted after a
/// `snapshot.deserialize` in another node) is `None`, never a mistyped value.
pub opaque type Key(host) {
  Key(id: Int)
}

/// A fresh key. Two calls give two keys that never read each other's
/// objects, even at the same `host` type, so call it once per embedding.
pub fn new_key() -> Key(host) {
  Key(id: unique_integer([Positive]))
}

type UniqueIntegerOption {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueIntegerOption)) -> Int

/// What a host function threads through. `agent` is the whole runtime state:
/// the `arc/rt/*` operations take and return an `Agent`, so host code that
/// drops to that layer rebuilds this record with the agent it gets back
/// (`State(..s, agent: st)`, which keeps `key` and so keeps `host`).
/// `new_target` is NewTarget of the [[Construct]] this function is serving,
/// `undefined` under a plain call. `key` is the payload key above.
pub type State(host) {
  State(agent: Agent, new_target: JsVal, key: Key(host))
}

/// Signature of an embedder native as `function` / `class` /
/// `define_fn` register it.
pub type HostFn(host) =
  fn(List(JsVal), JsVal, State(host)) -> #(State(host), Result(JsVal, JsVal))

/// Wrap an agent for host code that is not inside a host-function call
/// (`engine.with_state`, tests). No [[Construct]] is in progress, so
/// `new_target` is `undefined`.
pub fn from_agent(agent: Agent, key: Key(host)) -> State(host) {
  State(agent:, new_target: mk_undefined(), key:)
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
  key: Key(host),
  body: fn(State(host)) -> #(State(host), a),
) -> #(Agent, a) {
  let #(State(agent:, ..), result) = body(from_agent(agent, key))
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

// -- Suspend / resume --------------------------------------------------------
//
// The macrotask loop is the embedder's. Core only knows about Promises and
// the microtask queue. These two functions are the bridge: a host function
// hands JS a pending Promise and walks away with a settle `Ticket`; later,
// from its own loop (BEAM mailbox, libuv, epoll, whatever), it calls
// `resume` with that Ticket. `resume` queues the settlement as a microtask
// job behind whatever is already queued, so the Promise settles, and its
// reactions run, on the next drain: `with_state` drains on the way out, as
// do the engine's eval/call epilogues.
//
//     fn fetch(args, _this, s) {
//       let #(s, promise, ticket) = host.suspend(s)
//       kick_off_http(url, on_done: my_queue.push(ticket, _))
//       #(s, Ok(promise))
//     }
//     fn my_loop(agent) {
//       case my_queue.in_flight() {
//         0 -> agent
//         _ -> {
//           let #(ticket, result) = my_queue.block()
//           let #(agent, _outcome) =
//             host.with_state(agent, key, fn(s) { host.resume(s, ticket, result) })
//           my_loop(agent)
//         }
//       }
//     }

/// What `resume` did with the ticket. Embedders that don't care can bind
/// `_outcome`; embedders that want to detect their own bugs match on it.
pub type ResumeOutcome {
  /// The settlement is queued; the next drain settles the promise.
  Resumed
  /// The ticket had already been resumed once. Nothing changed.
  AlreadySettled
  /// The ticket does not name a suspended promise on this agent (it came
  /// from another engine, or from before a `deserialize`). Nothing changed.
  StaleTicket
}

/// Opaque settle handle for one `suspend`ed Promise. The ONLY way to get one
/// is from `suspend`, and the only thing to do with it is hand it back to
/// `resume`, so passing the Promise object or some unrelated handle to
/// `resume` is a compile error, not silent heap corruption.
///
/// `root` is a private cell that references the promise and is pinned from
/// `suspend` to `resume`. It is never handed to JS or the embedder, so no
/// `t_hold_roots` caller can name it and its pin cannot be confused with a
/// hold on the promise itself: the two lifetimes overlap without nesting
/// (the engine may be holding the very promise the embedder resumes).
pub opaque type Ticket {
  Ticket(promise: Handle, root: Handle)
}

/// Payload of a ticket's root cell.
type TicketRoot {
  TicketRoot(promise: Handle)
}

/// Create a pending Promise. Return the value from your host function so JS
/// can `await` it; keep the `Ticket` to pass to `resume` once your external
/// work completes. The promise is reachable from a GC root until then, so
/// everything awaiting it survives any collection in between.
pub fn suspend(s: State(host)) -> #(State(host), JsVal, Ticket) {
  let #(promise, st) = rt_async.t_new_promise(s.agent)
  let root_slot =
    host_slot(tag(ticket_key(), TicketRoot(promise:)), None, False)
  let #(root, st) = rt_store.t_cell_new(st, root_slot)
  let st = rt_store.t_pin_root(st, root)
  #(State(..s, agent: st), mk_object(promise), Ticket(promise:, root:))
}

/// Queue the settlement of the Promise behind a `suspend` Ticket as a
/// microtask job: it resolves on `Ok` (assimilating a thenable like a
/// `resolve` function does), rejects on `Error`, and the reactions run in
/// the same drain. The ticket's root is dropped here; the queued job keeps
/// the promise alive until it has run.
///
/// Resuming an already-resumed ticket is a no-op reported as
/// `AlreadySettled`, and a ticket this agent never issued is `StaleTicket`,
/// so an embedder counting `Resumed` outcomes against its suspends stays
/// honest.
pub fn resume(
  s: State(host),
  ticket: Ticket,
  outcome: Result(JsVal, JsVal),
) -> #(State(host), ResumeOutcome) {
  let Ticket(promise:, root:) = ticket
  case ticket_state(s.agent, ticket) {
    Stale -> #(s, StaleTicket)
    Spent -> #(s, AlreadySettled)
    Live -> {
      let st = rt_gc.t_release_roots(s.agent, [root.id])
      let st = rt_store.t_cell_free(st, root)
      let settle = fn(st) {
        case outcome {
          Ok(value) -> rt_async.t_promise_resolve(st, promise, value)
          Error(reason) -> rt_async.t_promise_reject(st, promise, reason)
        }
      }
      let st = rt_async.t_enqueue_job(st, HostJob(run: settle))
      #(State(..s, agent: st), Resumed)
    }
  }
}

type TicketState {
  /// Suspended and not yet resumed: the root cell still names the promise.
  Live
  /// Resumed before: the root is gone but the promise is still on this
  /// agent (it may still be pending until the queued job runs).
  Spent
  /// Not a promise of this agent at all.
  Stale
}

fn ticket_state(st: Agent, ticket: Ticket) -> TicketState {
  let Ticket(promise:, root:) = ticket
  use <- bool.guard(is_ticket_root(st, root, promise), Live)
  let spent = rt_gc.t_is_live(st, promise) && is_promise(st, promise)
  use <- bool.guard(spent, Spent)
  Stale
}

fn is_ticket_root(st: Agent, root: Handle, promise: Handle) -> Bool {
  use <- bool.guard(!rt_gc.t_is_live(st, root), False)
  case rt_store.t_cell_get(st, root) {
    SObject(kind: KHost(payload:), ..) ->
      payload == tag(ticket_key(), TicketRoot(promise:))
    _ -> False
  }
}

/// The key ticket roots are written under. `new_key` ids are positive, so
/// no embedder key is ever 0.
fn ticket_key() -> Key(TicketRoot) {
  Key(id: 0)
}

fn is_promise(st: Agent, h: Handle) -> Bool {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: PromiseObj(..), ..) -> True
    _ -> False
  }
}

// -- Host hooks --------------------------------------------------------------

/// Re-export: the embedder host-capability record. Start from
/// `default_host_hooks()`, override fields, hand it to the engine once.
pub type HostHooks =
  host_hooks.HostHooks

/// The default: [[CanBlock]] false (sync `Atomics.wait` throws instead of
/// hanging), no dynamic-import hook, and the real BEAM monotonic clock /
/// sleep.
pub fn default_host_hooks() -> HostHooks {
  host_hooks.default_host_hooks()
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
//
// The one place payload types are erased. A `KHost` payload is always a
// `Tagged`: the id of the `Key` it was written under, then the value.
// `erase` / `unerase` are unchecked casts and `tag` / `untag` are their only
// callers. `untag` claims `Tagged(host)` for whatever the cell holds, which
// is true of `key` (an Int in every `Tagged`) and becomes true of `value`
// once `key` matches the caller's `Key(host)`; on a mismatch `value` is
// dropped unread.

type Tagged(host) {
  Tagged(key: Int, value: host)
}

@external(erlang, "gleam_stdlib", "identity")
fn erase(tagged: Tagged(host)) -> HostTerm

@external(erlang, "gleam_stdlib", "identity")
fn unerase(term: HostTerm) -> Tagged(host)

fn tag(key: Key(host), value: host) -> HostTerm {
  erase(Tagged(key: key.id, value:))
}

fn untag(key: Key(host), term: HostTerm) -> Option(host) {
  let Tagged(key: id, value:) = unerase(term)
  case id == key.id {
    True -> Some(value)
    False -> None
  }
}

fn host_slot(
  payload: HostTerm,
  proto: Option(Handle),
  extensible: Bool,
) -> rt_types.JsSlot {
  SObject(
    kind: KHost(payload:),
    proto:,
    props: dict.new(),
    symbol_props: [],
    elements: NoElements,
    extensible:,
  )
}

/// Allocate an opaque, embedder-owned object wrapping `value` (the
/// embedder's own type). The engine never inspects `value`; it renders the
/// object via the prototype's `@@toStringTag`. The object has no own
/// properties; pass `Some(proto)` to give it methods/a tag, or `None` for a
/// null-prototype value. Read it back, typed, with `read_host` under the
/// same key.
///
/// Heap handles inside `value` are traced by the collector, so a payload
/// may hold JS objects directly.
pub fn alloc_host_object(
  s: State(host),
  value: host,
  prototype: Option(Handle),
) -> #(State(host), JsVal) {
  let #(h, st) =
    rt_store.t_cell_new(s.agent, host_slot(tag(s.key, value), prototype, True))
  #(State(..s, agent: st), mk_object(h))
}

/// Read the embedder value out of a host object. `None` if `val` is not one,
/// or is one written under a different key than `s` carries.
pub fn read_host(s: State(host), val: JsVal) -> Option(host) {
  use h <- option.then(handle_of(val))
  case rt_store.t_cell_get(s.agent, h) {
    SObject(kind: KHost(payload:), ..) -> untag(s.key, payload)
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
  let #(id, st) = register(s.agent, s.key, name, impl)
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
  let #(props, st) = alloc_host_methods(st, s.key, methods)
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
  let #(proto_props, st) = alloc_host_methods(st, s.key, methods)
  let #(static_props, st) = alloc_host_methods(st, s.key, statics)
  let #(id, st) = register(st, s.key, name, constructor)
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

/// Add `impl` to the agent's host-function table under the next id. This is
/// the table's only writer, so ids are dense from 0 in registration order,
/// which is what lets a deserialized engine's `HostFn(id)` cells find their
/// closures again once the embedder repeats its registrations. Every call of
/// `impl` sees the `key` it was registered under.
fn register(
  st: Agent,
  key: Key(host),
  name: String,
  impl: HostFn(host),
) -> #(Int, Agent) {
  let id = dict.size(st.host_fns)
  let entry =
    HostFnEntry(name:, call: fn(agent, args, this, new_target) {
      let #(State(agent:, ..), result) =
        impl(args, this, State(agent:, new_target:, key:))
      #(agent, result)
    })
  #(id, Agent(..st, host_fns: dict.insert(st.host_fns, id, entry)))
}

fn alloc_host_methods(
  st: Agent,
  key: Key(host),
  specs: List(#(String, Int, HostFn(host))),
) -> #(List(#(String, Property)), Agent) {
  let #(props, st) =
    list.fold(specs, #([], st), fn(acc, spec) {
      let #(props, st) = acc
      let #(name, arity, impl) = spec
      let #(id, st) = register(st, key, name, impl)
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
