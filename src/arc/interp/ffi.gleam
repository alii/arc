//// Bindings for the `arc_interp_*_ffi` kernel families (and the
//// `arc_rt_ops_ffi` operator kernels): the raise adapter that turns a raising runtime call into a
//// `Result` for the interpreter's slow path, and the fused hot-path kernels
//// `fast_loop` runs before falling back to it.
////
//// Kernels are typed with their HIT type but may return the atom `miss`
//// instead. `is_miss` / `is(_, Miss)` are the one probe that knows this; a
//// caller must test it before using the value in any other way. That
//// confines the type assertion to a single probe and keeps a hit
//// allocation-free.

import arc/bytecode/key.{type PropertyKey}
import arc/bytecode/lexical.{type LexicalSlots}
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/state.{type State, type StepExit}
import arc/rt/types.{
  type Agent, type Handle, type JsSlot, type JsStore, type JsVal,
  type LexicalGlobal, type SymbolId,
}
import gleam
import gleam/dict.{type Dict}

// -- Raise adapter -------------------------------------------------------------

/// Outcome of a guarded runtime call: the value and the agent it returned,
/// or the agent and value carried by the `wasm_exn` it raised. Wire form
/// `{ok, V, Agent} | {threw, Agent, E}` as built by `arc_interp_guard_ffi:guardN`.
pub type Guarded(v) {
  Ok(value: v, agent: Agent)
  Threw(agent: Agent, thrown: JsVal)
}

/// Adopt a guarded call's agent into `st` either way; a throw becomes the
/// step function's `Error(Threw(..))`.
///
///     use #(v, state) <- result.try(guarded(
///       guard3(rt_obj.t_get_prop, state.agent, obj, key), state))
pub fn guarded(
  outcome: Guarded(v),
  st: State,
) -> Result(#(v, State), StepExit) {
  case outcome {
    Ok(value:, agent:) -> gleam.Ok(#(value, state.with_agent(st, agent)))
    Threw(agent:, thrown:) ->
      Error(state.Threw(thrown, state.with_agent(st, agent)))
  }
}

/// `guardN(f, agent, ..)` applies the arity-N value-first runtime function
/// `f(agent, ..) -> #(v, Agent)` under one `try`. Pass a module function
/// (`rt_obj.t_get_prop`), never a fresh closure: it lowers to a literal
/// remote fun reference.
@external(erlang, "arc_interp_guard_ffi", "guard1")
pub fn guard1(f: fn(Agent) -> #(v, Agent), agent: Agent) -> Guarded(v)

/// `guard1` for a body that carries its agent inside a `State`: the state
/// goes in as built, nothing is re-seated on entry.
@external(erlang, "arc_interp_guard_ffi", "guard1")
pub fn guard_state(f: fn(State) -> #(v, Agent), state: State) -> Guarded(v)

/// `guard_state` with one extra argument for the body.
@external(erlang, "arc_interp_guard_ffi", "guard2")
pub fn guard_state2(
  f: fn(State, a) -> #(v, Agent),
  state: State,
  a: a,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard2")
pub fn guard2(f: fn(Agent, a) -> #(v, Agent), agent: Agent, a: a) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard3")
pub fn guard3(
  f: fn(Agent, a, b) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard4")
pub fn guard4(
  f: fn(Agent, a, b, c) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
  c: c,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard5")
pub fn guard5(
  f: fn(Agent, a, b, c, d) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard6")
pub fn guard6(
  f: fn(Agent, a, b, c, d, e) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard7")
pub fn guard7(
  f: fn(Agent, a, b, c, d, e, g) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
  g: g,
) -> Guarded(v)

/// `guard_unitN`: the same for runtime functions that return the bare
/// `Agent`; the value is `Nil`.
@external(erlang, "arc_interp_guard_ffi", "guard_unit1")
pub fn guard_unit1(f: fn(Agent) -> Agent, agent: Agent) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit2")
pub fn guard_unit2(f: fn(Agent, a) -> Agent, agent: Agent, a: a) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit3")
pub fn guard_unit3(
  f: fn(Agent, a, b) -> Agent,
  agent: Agent,
  a: a,
  b: b,
) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit4")
pub fn guard_unit4(
  f: fn(Agent, a, b, c) -> Agent,
  agent: Agent,
  a: a,
  b: b,
  c: c,
) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit5")
pub fn guard_unit5(
  f: fn(Agent, a, b, c, d) -> Agent,
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit6")
pub fn guard_unit6(
  f: fn(Agent, a, b, c, d, e) -> Agent,
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
) -> Guarded(Nil)

// -- Fused kernels ---------------------------------------------------------------
// Each returns its result or `miss`; only `truthy` and `nullish` are total.
// A miss means the operands need something observable (ToPrimitive, a
// getter, a proxy trap, a throw): take the guarded slow path.

/// True when a kernel answered `miss` instead of a value of its result type.
@external(erlang, "arc_interp_ffi", "is_miss")
pub fn is_miss(result: a) -> Bool

/// The bare atoms the fast loop tests a term against: a kernel's `miss`
/// answer, the TDZ sentinel (`arc_rt_val_ffi:mk_tdz`) and the `undefined`
/// value. The constructors lower to those atoms.
pub type Sentinel {
  Miss
  JsTdz
  Undefined
}

/// `v =:= s`: the `is_miss` / TDZ probe as the inlined compare BIF, for the
/// fast loop where a remote call per test shows.
@external(erlang, "erlang", "=:=")
pub fn is(v: a, s: Sentinel) -> Bool

/// `v =:= b`: a value against a boolean wire term (the atoms themselves).
@external(erlang, "erlang", "=:=")
pub fn is_bool(v: JsVal, b: Bool) -> Bool

/// The cell a callee value designates, for the call arms to match on;
/// a non-object callee (or a dangling handle) misses.
@external(erlang, "arc_interp_ffi", "cell_of")
pub fn cell_of(agent: Agent, v: JsVal) -> JsSlot

/// `a + b` for numbers, strings, and a string with a pure-ToString primitive.
@external(erlang, "arc_rt_ops_ffi", "add")
pub fn add(a: JsVal, b: JsVal) -> JsVal

/// `a - b` for numbers.
@external(erlang, "arc_rt_ops_ffi", "sub")
pub fn sub(a: JsVal, b: JsVal) -> JsVal

/// `a * b` for numbers.
@external(erlang, "arc_rt_ops_ffi", "mul")
pub fn mul(a: JsVal, b: JsVal) -> JsVal

/// `a / b` for numbers.
@external(erlang, "arc_rt_ops_ffi", "div")
pub fn div(a: JsVal, b: JsVal) -> JsVal

/// `a % b` for finite numbers.
@external(erlang, "arc_rt_ops_ffi", "mod")
pub fn mod(a: JsVal, b: JsVal) -> JsVal

/// `-a` for numbers and BigInt.
@external(erlang, "arc_rt_ops_ffi", "neg")
pub fn neg(a: JsVal) -> JsVal

/// `+a` for numbers.
@external(erlang, "arc_rt_ops_ffi", "plus")
pub fn plus(a: JsVal) -> JsVal

/// `+a + delta` for a Number `a` and a small integer `delta` (the `i++` /
/// `i--` kernel).
@external(erlang, "arc_rt_ops_ffi", "step")
pub fn step(a: JsVal, delta: Int) -> JsVal

/// The compare and equality kernels answer `true | false | miss`; the
/// boolean atoms ARE the boolean wire terms, so the answer is typed as the
/// value it pushes. `a < b` etc. for number, string and BigInt pairs.
@external(erlang, "arc_rt_ops_ffi", "lt")
pub fn lt(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "le")
pub fn le(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "gt")
pub fn gt(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "ge")
pub fn ge(a: JsVal, b: JsVal) -> JsVal

/// §7.2.15 IsStrictlyEqual and its negation; a TDZ sentinel misses.
@external(erlang, "arc_rt_ops_ffi", "strict_eq")
pub fn strict_eq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "strict_neq")
pub fn strict_neq(a: JsVal, b: JsVal) -> JsVal

/// §7.2.14 IsLooselyEqual (and its negation) for the coercion-free pairs.
@external(erlang, "arc_rt_ops_ffi", "eq")
pub fn eq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "neq")
pub fn neq(a: JsVal, b: JsVal) -> JsVal

/// `!v`. Total.
@external(erlang, "arc_interp_ffi", "lnot")
pub fn lnot(v: JsVal) -> JsVal

/// `v instanceof ctor` when `ctor[@@hasInstance]` (`has_instance` is that
/// symbol) provably resolves to the Function.prototype intrinsic or to
/// nothing: OrdinaryHasInstance by identity walk. Anything observable
/// misses.
@external(erlang, "arc_interp_ffi", "instance_of")
pub fn instance_of(
  agent: Agent,
  v: JsVal,
  ctor: JsVal,
  has_instance: SymbolId,
) -> JsVal

/// §7.1.2 ToBoolean. Total.
@external(erlang, "arc_interp_ffi", "truthy")
pub fn truthy(v: JsVal) -> Bool

/// `v` is `null` or `undefined`. Total.
@external(erlang, "arc_interp_ffi", "nullish")
pub fn nullish(v: JsVal) -> Bool

/// `typeof v` for primitives; objects miss.
@external(erlang, "arc_interp_ffi", "typeof")
pub fn type_of(v: JsVal) -> String

/// `typeof v` including object cells (Proxy misses).
@external(erlang, "arc_interp_ffi", "typeof")
pub fn type_of_in(store: JsStore(Agent), v: JsVal) -> String

/// The value in the box cell a captured local holds; the TDZ sentinel or a
/// non-box slot miss.
@external(erlang, "arc_interp_ffi", "box_get")
pub fn box_get(agent: Agent, slot: JsVal) -> JsVal

/// `obj.key`: own or inherited plain data property, `undefined` if absent
/// on an all-ordinary chain; a string or number receiver reads from its
/// realm wrapper prototype. `key` is a `Named` (non-index) key, handed over
/// whole so the kernel probes with the term the opcode already holds.
@external(erlang, "arc_interp_prop_ffi", "get_field")
pub fn get_field(agent: Agent, obj: JsVal, key: PropertyKey) -> JsVal

/// A global identifier read (§9.1.1.4 GetBindingValue): an initialised
/// lexical binding from `lex`, else a plain data property on the global
/// object's chain. TDZ, accessors, exotic hops and an unresolvable name
/// miss.
@external(erlang, "arc_interp_prop_ffi", "get_global")
pub fn get_global(
  agent: Agent,
  lex: Dict(String, LexicalGlobal),
  name: String,
) -> JsVal

/// A global identifier write on the object record (no lexical binding of
/// the name): replace an own writable data property of the global object,
/// or create it when the frame is sloppy. The store with the write applied.
@external(erlang, "arc_interp_prop_ffi", "put_global")
pub fn put_global(
  store: JsStore(Agent),
  lex: Dict(String, LexicalGlobal),
  global: Handle,
  name: String,
  v: JsVal,
  strict: Bool,
) -> JsStore(Agent)

/// `obj[key]` for an integer index into an Array cell or a string key.
@external(erlang, "arc_interp_prop_ffi", "get_elem")
pub fn get_elem(store: JsStore(Agent), obj: JsVal, key: JsVal) -> JsVal

/// `get_elem` for an integer key only (GetElem2 re-pushes the key, and an
/// integer is its own canonical form).
@external(erlang, "arc_interp_prop_ffi", "get_elem2")
pub fn get_elem2(store: JsStore(Agent), obj: JsVal, key: JsVal) -> JsVal

/// `obj.key = v` over an existing own writable data property; the store
/// with the write applied.
@external(erlang, "arc_interp_prop_ffi", "put_field")
pub fn put_field(
  store: JsStore(Agent),
  obj: JsVal,
  key: PropertyKey,
  v: JsVal,
) -> JsStore(Agent)

/// `{key: v}`: CreateDataProperty of a Named key on an ordinary extensible
/// object; the store with the property defined.
@external(erlang, "arc_interp_prop_ffi", "define_field")
pub fn define_field(
  store: JsStore(Agent),
  obj: JsVal,
  key: PropertyKey,
  v: JsVal,
) -> JsStore(Agent)

/// `arr[i] = v` overwriting, hole-filling or appending on an extensible
/// Array cell; the store with the write applied.
@external(erlang, "arc_interp_prop_ffi", "put_elem")
pub fn put_elem(
  store: JsStore(Agent),
  obj: JsVal,
  index: JsVal,
  v: JsVal,
) -> JsStore(Agent)

// -- Call prologue ---------------------------------------------------------------

/// Locals tuple for an arrow / lexical-free body:
/// `env ++ seeds ++ args` fitted to `arity`, padded with `undef` to
/// `local_count`. `env` is the closure's captured environment (tuple or
/// list of values).
@external(erlang, "arc_interp_locals_ffi", "setup_locals_tuple")
pub fn setup_locals_tuple(
  env: env,
  seeds: List(JsVal),
  args: List(JsVal),
  arity: Int,
  local_count: Int,
  undef: JsVal,
) -> TupleArray(JsVal)

/// Locals tuple for a non-arrow body, seeding `this` / active function /
/// home object / new.target into its owned lexical slots.
@external(erlang, "arc_interp_locals_ffi", "setup_locals_seeded")
pub fn setup_locals_seeded(
  env: env,
  lexical: LexicalSlots,
  this: JsVal,
  active_func: JsVal,
  home_object: JsVal,
  new_target: JsVal,
  args: List(JsVal),
  arity: Int,
  local_count: Int,
  undef: JsVal,
) -> TupleArray(JsVal)
