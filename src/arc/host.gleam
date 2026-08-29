//// helpers for writing host functions; validators modeled on node's

import arc/bytecode/key.{canonical_key}
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

/// pins the payload type; a read under another key is none
pub opaque type Key(host) {
  Key(id: Int)
}

/// mint once per embedding
pub fn new_key() -> Key(host) {
  Key(id: unique_integer([Positive]))
}

type UniqueIntegerOption {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueIntegerOption)) -> Int

/// new_target is undefined under a plain call
pub type State(host) {
  State(agent: Agent, new_target: JsVal, key: Key(host))
}

pub type HostFn(host) =
  fn(List(JsVal), JsVal, State(host)) -> #(State(host), Result(JsVal, JsVal))

pub fn from_agent(agent: Agent, key: Key(host)) -> State(host) {
  State(agent:, new_target: mk_undefined(), key:)
}

pub fn new_target(s: State(host)) -> JsVal {
  s.new_target
}

pub fn with_state(
  agent: Agent,
  key: Key(host),
  body: fn(State(host)) -> #(State(host), a),
) -> #(Agent, a) {
  let #(State(agent:, ..), result) = body(from_agent(agent, key))
  #(rt_async.drain(agent), result)
}

/// missing args are undefined
pub const first_arg = helpers.first_arg_or_undefined

pub const arg_at = helpers.arg_at

pub fn type_error(
  s: State(host),
  msg: String,
) -> #(State(host), Result(JsVal, JsVal)) {
  throw_new(s, TypeErr, msg)
}

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

/// passes the value through; use try_call for one-shot calls
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

/// error is the thrown value
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

/// typeerror if not a number, rangeerror if not integral or out of range
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
    // nan / infinity
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

pub type ResumeOutcome {
  Resumed
  AlreadySettled
  StaleTicket
}

/// root is a private pinned cell, never handed out
pub opaque type Ticket {
  Ticket(promise: Handle, root: Handle)
}

type TicketRoot {
  TicketRoot(promise: Handle)
}

/// pending promise plus the ticket to resume it with later
pub fn suspend(s: State(host)) -> #(State(host), JsVal, Ticket) {
  let #(promise, st) = rt_async.t_new_promise(s.agent)
  let root_slot =
    host_slot(tag(ticket_key(), TicketRoot(promise:)), None, False)
  let #(root, st) = rt_store.t_cell_new(st, root_slot)
  let st = rt_store.t_pin_root(st, root)
  #(State(..s, agent: st), mk_object(promise), Ticket(promise:, root:))
}

/// queues the settlement as a microtask; drops the ticket root
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
  // root cell still names the promise
  Live
  // resumed before, promise still on this agent
  Spent
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

// new_key ids are positive so 0 is never an embedder key
fn ticket_key() -> Key(TicketRoot) {
  Key(id: 0)
}

fn is_promise(st: Agent, h: Handle) -> Bool {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: PromiseObj(..), ..) -> True
    _ -> False
  }
}

pub type HostHooks =
  host_hooks.HostHooks

pub fn default_host_hooks() -> HostHooks {
  host_hooks.default_host_hooks()
}

pub fn array(s: State(host), values: List(JsVal)) -> #(State(host), JsVal) {
  let st = s.agent
  let #(h, st) = common.alloc_array(st, values, st.realm.array.prototype)
  #(State(..s, agent: st), mk_object(h))
}

pub fn object(
  s: State(host),
  props: List(#(String, JsVal)),
) -> #(State(host), JsVal) {
  let st = s.agent
  let #(h, st) = common.alloc_pojo(st, st.realm.object.prototype, props)
  #(State(..s, agent: st), mk_object(h))
}

// the one place payload types are erased; value unread unless key matches
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

/// opaque embedder object; handles inside value are gc-traced
pub fn alloc_host_object(
  s: State(host),
  value: host,
  prototype: Option(Handle),
) -> #(State(host), JsVal) {
  let #(h, st) =
    rt_store.t_cell_new(s.agent, host_slot(tag(s.key, value), prototype, True))
  #(State(..s, agent: st), mk_object(h))
}

/// none if not a host object or written under another key
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

/// mint a rooted native function without installing it
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

pub fn define_fn(
  s: State(host),
  name: String,
  arity: Int,
  impl: HostFn(host),
) -> State(host) {
  let #(s, f) = function(s, name, arity, impl)
  define_global(s, name, f)
}

/// writable, configurable, non-enumerable like builtin globals
pub fn define_global(s: State(host), name: String, val: JsVal) -> State(host) {
  let st = s.agent
  let #(_created, st) =
    rt_obj.t_define_own_data(
      st,
      st.realm.global_object,
      StringKey(canonical_key(name)),
      val,
      True,
      False,
      True,
    )
  State(..s, agent: st)
}

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

/// constructible and extendable; instance is re-prototyped to new_target
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

// only writer of host_fns, so ids are dense in registration order
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
