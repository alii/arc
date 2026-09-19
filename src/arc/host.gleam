// helpers for writing host functions; validators modeled on node's

import arc/bytecode/error_kind.{type ErrorKind, RangeError, TypeError}
import arc/bytecode/key
import arc/internal/unsafe
import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type HostTerm, type JsVal, type Property, Agent,
  HostFnEntry, HostJob, HostObj, JFloat, JInt, KBool, KNum, KStr, NoElements,
  PromiseObj, SObject, StringKey, classify, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

/// pins the payload type; a read under another brand is none
pub opaque type Brand(host) {
  Brand(id: Int)
}

/// mint once per embedding
pub fn new_brand() -> Brand(host) {
  Brand(id: unique_integer([Positive]))
}

type UniqueIntegerOption {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueIntegerOption)) -> Int

/// new_target is undefined under a plain call
pub type Context(host) {
  Context(agent: Agent, new_target: JsVal, brand: Brand(host))
}

pub type HostFn(host) =
  fn(Context(host), List(JsVal), JsVal) ->
    #(Result(JsVal, JsVal), Context(host))

/// a named host function of a namespace or class
pub type HostMethod(host) {
  HostMethod(name: String, arity: Int, call: HostFn(host))
}

pub fn from_agent(st: Agent, brand: Brand(host)) -> Context(host) {
  Context(agent: st, new_target: mk_undefined(), brand:)
}

pub fn new_target(ctx: Context(host)) -> JsVal {
  ctx.new_target
}

pub fn with_context(
  st: Agent,
  brand: Brand(host),
  body: fn(Context(host)) -> #(a, Context(host)),
) -> #(a, Agent) {
  let #(result, Context(agent: st, ..)) = body(from_agent(st, brand))
  #(result, rt_async.drain(st))
}

/// missing args are undefined
pub const first_arg = helpers.first_arg_or_undefined

pub const arg_at = helpers.arg_at

pub fn type_error(
  ctx: Context(host),
  msg: String,
) -> #(Result(JsVal, JsVal), Context(host)) {
  throw_new(ctx, TypeError, msg)
}

pub fn range_error(
  ctx: Context(host),
  msg: String,
) -> #(Result(JsVal, JsVal), Context(host)) {
  throw_new(ctx, RangeError, msg)
}

fn throw_new(
  ctx: Context(host),
  kind: ErrorKind,
  msg: String,
) -> #(Result(JsVal, JsVal), Context(host)) {
  let st = ctx.agent
  let #(err, st) = rt_val.new_error(st, kind, msg)
  #(Error(err), Context(..ctx, agent: st))
}

pub fn validate_string(
  ctx: Context(host),
  val: JsVal,
  name: String,
  cont: fn(String, Context(host)) -> #(Result(JsVal, JsVal), Context(host)),
) -> #(Result(JsVal, JsVal), Context(host)) {
  case classify(val) {
    KStr(text) -> cont(text, ctx)
    _ -> invalid_arg_type(ctx, name, "string", val)
  }
}

/// passes the value through; use try_call for one-shot calls
pub fn validate_function(
  ctx: Context(host),
  val: JsVal,
  name: String,
  cont: fn(JsVal, Context(host)) -> #(Result(JsVal, JsVal), Context(host)),
) -> #(Result(JsVal, JsVal), Context(host)) {
  case rt_val.is_callable(ctx.agent, val) {
    True -> cont(val, ctx)
    False -> invalid_arg_type(ctx, name, "function", val)
  }
}

/// error is the thrown value
pub fn call(
  ctx: Context(host),
  callee: JsVal,
  this_val: JsVal,
  args: List(JsVal),
) -> #(Result(JsVal, JsVal), Context(host)) {
  let #(completion, st) = rt_call.try_call(ctx.agent, callee, this_val, args)
  let ctx = Context(..ctx, agent: st)
  case completion {
    NormalCompletion(v) -> #(Ok(v), ctx)
    ThrowCompletion(thrown) -> #(Error(thrown), ctx)
  }
}

pub fn try_call(
  ctx: Context(host),
  callee: JsVal,
  name: String,
  this_val: JsVal,
  args: List(JsVal),
  cont: fn(JsVal, Context(host)) -> #(Result(JsVal, JsVal), Context(host)),
) -> #(Result(JsVal, JsVal), Context(host)) {
  case rt_val.is_callable(ctx.agent, callee) {
    False -> invalid_arg_type(ctx, name, "function", callee)
    True -> {
      let #(result, ctx) = call(ctx, callee, this_val, args)
      case result {
        Ok(v) -> cont(v, ctx)
        Error(thrown) -> #(Error(thrown), ctx)
      }
    }
  }
}

/// typeerror if not a number, rangeerror if not integral or out of range
pub fn validate_integer(
  ctx: Context(host),
  val: JsVal,
  name: String,
  min: Int,
  max: Int,
  cont: fn(Int, Context(host)) -> #(Result(JsVal, JsVal), Context(host)),
) -> #(Result(JsVal, JsVal), Context(host)) {
  case classify(val) {
    KNum(JInt(i)) -> check_range(ctx, name, i, min, max, cont)
    KNum(JFloat(f) as n) ->
      case rt_val.integral_int(f) {
        Some(i) -> check_range(ctx, name, i, min, max, cont)
        None -> not_an_integer(ctx, name, rt_val.jsnum_to_string(n))
      }
    // nan / infinity
    KNum(n) -> not_an_integer(ctx, name, rt_val.jsnum_to_string(n))
    _ -> invalid_arg_type(ctx, name, "integer", val)
  }
}

fn check_range(
  ctx: Context(host),
  name: String,
  i: Int,
  min: Int,
  max: Int,
  cont: fn(Int, Context(host)) -> #(Result(JsVal, JsVal), Context(host)),
) -> #(Result(JsVal, JsVal), Context(host)) {
  case i >= min && i <= max {
    True -> cont(i, ctx)
    False ->
      range_error(
        ctx,
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
  ctx: Context(host),
  val: JsVal,
  name: String,
  cont: fn(Bool, Context(host)) -> #(Result(JsVal, JsVal), Context(host)),
) -> #(Result(JsVal, JsVal), Context(host)) {
  case classify(val) {
    KBool(b) -> cont(b, ctx)
    _ -> invalid_arg_type(ctx, name, "boolean", val)
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
pub fn suspend(ctx: Context(host)) -> #(JsVal, Ticket, Context(host)) {
  let #(promise, st) = rt_async.new_promise(ctx.agent)
  let root_cell =
    host_cell(
      tag(ticket_brand(), TicketRoot(promise:)),
      None,
      extensible: False,
    )
  let #(root, st) = rt_store.cell_new(st, root_cell)
  let st = rt_store.pin_root(st, root)
  #(mk_object(promise), Ticket(promise:, root:), Context(..ctx, agent: st))
}

/// queues the settlement as a microtask; drops the ticket root
pub fn resume(
  ctx: Context(host),
  ticket: Ticket,
  outcome: Result(JsVal, JsVal),
) -> #(ResumeOutcome, Context(host)) {
  let Ticket(promise:, root:) = ticket
  case ticket_state(ctx.agent, ticket) {
    Stale -> #(StaleTicket, ctx)
    Spent -> #(AlreadySettled, ctx)
    Live -> {
      let st = rt_gc.release_roots(ctx.agent, [root.id])
      let st = rt_store.cell_free(st, root)
      let settle = fn(st) {
        case outcome {
          Ok(value) -> rt_async.promise_resolve(st, promise, value)
          Error(reason) -> rt_async.promise_reject(st, promise, reason)
        }
      }
      let st = rt_async.enqueue_job(st, HostJob(run: settle))
      #(Resumed, Context(..ctx, agent: st))
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
  let spent = rt_gc.is_live(st, promise) && is_promise(st, promise)
  use <- bool.guard(spent, Spent)
  Stale
}

fn is_ticket_root(st: Agent, root: Handle, promise: Handle) -> Bool {
  use <- bool.guard(!rt_gc.is_live(st, root), False)
  case rt_store.cell_get(st, root) {
    SObject(kind: HostObj(payload:), ..) ->
      payload == tag(ticket_brand(), TicketRoot(promise:))
    _ -> False
  }
}

// new_brand ids are positive so 0 is never an embedder brand
fn ticket_brand() -> Brand(TicketRoot) {
  Brand(id: 0)
}

fn is_promise(st: Agent, h: Handle) -> Bool {
  case rt_store.cell_get(st, h) {
    SObject(kind: PromiseObj(..), ..) -> True
    _ -> False
  }
}

pub fn array(
  ctx: Context(host),
  values: List(JsVal),
) -> #(JsVal, Context(host)) {
  let st = ctx.agent
  let #(h, st) = common.alloc_array(st, values, st.realm.array.prototype)
  #(mk_object(h), Context(..ctx, agent: st))
}

pub fn object(
  ctx: Context(host),
  props: List(#(String, JsVal)),
) -> #(JsVal, Context(host)) {
  let st = ctx.agent
  let #(h, st) = common.alloc_plain_object(st, st.realm.object.prototype, props)
  #(mk_object(h), Context(..ctx, agent: st))
}

// the one place payload types are erased; value unread unless brand matches
type Tagged(host) {
  Tagged(brand: Int, value: host)
}

fn erase(tagged: Tagged(host)) -> HostTerm {
  unsafe.coerce(tagged)
}

fn unerase(term: HostTerm) -> Tagged(host) {
  unsafe.coerce(term)
}

fn tag(brand: Brand(host), value: host) -> HostTerm {
  erase(Tagged(brand: brand.id, value:))
}

fn untag(brand: Brand(host), term: HostTerm) -> Option(host) {
  let Tagged(brand: id, value:) = unerase(term)
  case id == brand.id {
    True -> Some(value)
    False -> None
  }
}

fn host_cell(
  payload: HostTerm,
  proto: Option(Handle),
  extensible extensible: Bool,
) -> types.Cell {
  SObject(
    kind: HostObj(payload:),
    proto:,
    props: dict.new(),
    symbol_props: [],
    elements: NoElements,
    extensible:,
  )
}

/// opaque embedder object; handles inside value are gc-traced
pub fn alloc_host_object(
  ctx: Context(host),
  value: host,
  prototype: Option(Handle),
) -> #(JsVal, Context(host)) {
  let #(h, st) =
    rt_store.cell_new(
      ctx.agent,
      host_cell(tag(ctx.brand, value), prototype, extensible: True),
    )
  #(mk_object(h), Context(..ctx, agent: st))
}

/// none if not a host object or written under another brand
pub fn read_host(ctx: Context(host), val: JsVal) -> Option(host) {
  use h <- option.then(rt_val.handle_of(val))
  case rt_store.cell_get(ctx.agent, h) {
    SObject(kind: HostObj(payload:), ..) -> untag(ctx.brand, payload)
    _ -> None
  }
}

/// mint a rooted native function without installing it
pub fn function(
  ctx: Context(host),
  name: String,
  arity: Int,
  impl: HostFn(host),
) -> #(JsVal, Context(host)) {
  let #(id, st) = register(ctx.agent, ctx.brand, impl)
  let #(h, st) =
    common.alloc_rooted_native_fn(
      st,
      st.realm.function.prototype,
      types.HostFn(id),
      name,
      arity,
    )
  #(mk_object(h), Context(..ctx, agent: st))
}

pub fn define_fn(
  ctx: Context(host),
  name: String,
  arity: Int,
  impl: HostFn(host),
) -> Context(host) {
  let #(f, ctx) = function(ctx, name, arity, impl)
  define_global(ctx, name, f)
}

/// writable, configurable, non-enumerable like builtin globals
pub fn define_global(
  ctx: Context(host),
  name: String,
  val: JsVal,
) -> Context(host) {
  let st = ctx.agent
  let #(_created, st) =
    rt_obj.define_own_data(
      st,
      st.realm.global_object,
      StringKey(key.canonical(name)),
      val,
      writable: True,
      enumerable: False,
      configurable: True,
    )
  Context(..ctx, agent: st)
}

pub fn define_namespace(
  ctx: Context(host),
  name: String,
  methods: List(HostMethod(host)),
) -> Context(host) {
  let st = ctx.agent
  let #(props, st) = alloc_host_methods(st, ctx.brand, methods)
  let #(ns, st) =
    common.init_namespace(st, st.realm.object.prototype, name, props)
  define_global(Context(..ctx, agent: st), name, mk_object(ns))
}

/// constructible and extendable; instance is re-prototyped to new_target
pub fn class(
  ctx: Context(host),
  name: String,
  arity: Int,
  constructor: HostFn(host),
  methods: List(HostMethod(host)),
  statics: List(HostMethod(host)),
) -> #(JsVal, Context(host)) {
  let st = ctx.agent
  let realm = st.realm
  let #(proto_props, st) = alloc_host_methods(st, ctx.brand, methods)
  let #(static_props, st) = alloc_host_methods(st, ctx.brand, statics)
  let #(id, st) = register(st, ctx.brand, constructor)
  let #(pair, st) =
    common.init_type(
      st,
      realm.object.prototype,
      realm.function.prototype,
      proto_props,
      fn(_proto) { types.HostFn(id) },
      name,
      arity,
      static_props,
    )
  #(mk_object(pair.constructor), Context(..ctx, agent: st))
}

// only writer of host_fns, so ids are dense in registration order
fn register(
  st: Agent,
  brand: Brand(host),
  impl: HostFn(host),
) -> #(Int, Agent) {
  let id = dict.size(st.host_fns)
  let entry =
    HostFnEntry(call: fn(st, args, this, new_target) {
      let #(result, Context(agent: st, ..)) =
        impl(Context(agent: st, new_target:, brand:), args, this)
      #(result, st)
    })
  #(id, Agent(..st, host_fns: dict.insert(st.host_fns, id, entry)))
}

fn alloc_host_methods(
  st: Agent,
  brand: Brand(host),
  specs: List(HostMethod(host)),
) -> #(List(#(String, Property)), Agent) {
  let #(props, st) =
    list.fold(specs, #([], st), fn(acc, spec) {
      let #(props, st) = acc
      let HostMethod(name:, arity:, call: impl) = spec
      let #(id, st) = register(st, brand, impl)
      let #(h, st) =
        common.alloc_rooted_native_fn(
          st,
          st.realm.function.prototype,
          types.HostFn(id),
          name,
          arity,
        )
      let #(prop, st) = rt_store.builtin_property(st, mk_object(h))
      #([#(name, prop), ..props], st)
    })
  #(list.reverse(props), st)
}

fn not_an_integer(
  ctx: Context(host),
  name: String,
  received: String,
) -> #(Result(JsVal, JsVal), Context(host)) {
  range_error(
    ctx,
    "The value of \""
      <> name
      <> "\" is out of range. It must be an integer. Received "
      <> received,
  )
}

fn invalid_arg_type(
  ctx: Context(host),
  name: String,
  expected: String,
  received: JsVal,
) -> #(Result(JsVal, JsVal), Context(host)) {
  let actual = rt_val.type_of(ctx.agent, received)
  type_error(
    ctx,
    "The \""
      <> name
      <> "\" argument must be of type "
      <> expected
      <> ". Received type "
      <> actual,
  )
}
