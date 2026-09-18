// exports may answer Miss, test with is(v, Miss) before use

import arc/bytecode/binop.{type PureBinOp}
import arc/bytecode/key.{type PropertyKey}
import arc/bytecode/lexical.{type LexicalSlots}
import arc/bytecode/opcode.{type ClassifiedBinOp}
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/state.{type State, type StepExit}
import arc/rt/bytecode.{type EnvCapture, type EnvTuple}
import arc/rt/types.{
  type Agent, type Cell, type Handle, type JsVal, type LexicalGlobal,
  type Property, type Store, type SymbolId,
}
import gleam
import gleam/dict.{type Dict}
import gleam/option.{type Option}

pub type Guarded(v) {
  Ok(value: v, agent: Agent)
  Threw(agent: Agent, thrown: JsVal)
}

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

// pass a module function, never a fresh closure; st is the agent or state
@external(erlang, "arc_interp_guard_ffi", "guard1")
pub fn guard1(f: fn(st) -> #(v, Agent), st: st) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard2")
pub fn guard2(f: fn(st, a) -> #(v, Agent), st: st, a: a) -> Guarded(v)

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

pub type Sentinel {
  Miss
  JsTdz
  Undefined
  Null
}

@external(erlang, "erlang", "=:=")
pub fn is(v: a, s: Sentinel) -> Bool

@external(erlang, "erlang", "=:=")
pub fn is_bool(v: JsVal, expected expected: Bool) -> Bool

@external(erlang, "erlang", "=:=")
pub fn same(a: JsVal, b: JsVal) -> Bool

@external(erlang, "arc_interp_ffi", "cell_of")
pub fn cell_of(agent: Agent, v: JsVal) -> Cell

@external(erlang, "arc_interp_ffi", "capture_env")
pub fn capture_env(
  descriptors: List(EnvCapture),
  locals: TupleArray(JsVal),
) -> EnvTuple

@external(erlang, "arc_interp_ffi", "ctor_prototype")
pub fn ctor_prototype(agent: Agent, new_target: JsVal) -> Handle

@external(erlang, "arc_interp_ffi", "list_of")
pub fn list_of(agent: Agent, array_like: JsVal) -> List(JsVal)

@external(erlang, "arc_rt_ops_ffi", "classified_binop")
pub fn classified_binop(kind: ClassifiedBinOp, a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "pure_binop")
pub fn pure_binop(op: PureBinOp, a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "bitnot")
pub fn bitnot(a: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "add")
pub fn add(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "sub")
pub fn sub(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "mul")
pub fn mul(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "div")
pub fn div(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "mod")
pub fn mod(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "neg")
pub fn neg(a: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "plus")
pub fn plus(a: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "step")
pub fn step(a: JsVal, delta: Int) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "lt")
pub fn lt(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "le")
pub fn le(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "gt")
pub fn gt(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "ge")
pub fn ge(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "strict_eq")
pub fn strict_eq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "strict_neq")
pub fn strict_neq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "eq")
pub fn eq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "neq")
pub fn neq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_interp_ffi", "instance_of")
pub fn instance_of(
  agent: Agent,
  v: JsVal,
  ctor: JsVal,
  has_instance: SymbolId,
) -> JsVal

@external(erlang, "arc_interp_ffi", "type_of")
pub fn type_of(store: Store, v: JsVal) -> String

@external(erlang, "arc_interp_ffi", "box_get")
pub fn box_get(agent: Agent, box: JsVal) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "get_field")
pub fn get_field(agent: Agent, obj: JsVal, key: PropertyKey) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "own_data")
pub fn own_data(props: Dict(PropertyKey, Property), key: PropertyKey) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "get_global")
pub fn get_global(
  agent: Agent,
  lex: Dict(String, LexicalGlobal),
  name: String,
) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "put_global")
pub fn put_global(
  store: Store,
  lex: Dict(String, LexicalGlobal),
  global: Handle,
  name: String,
  v: JsVal,
  strict strict: Bool,
) -> Store

@external(erlang, "arc_interp_prop_ffi", "get_elem")
pub fn get_elem(store: Store, obj: JsVal, key: JsVal) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "get_elem_keep")
pub fn get_elem_keep(store: Store, obj: JsVal, key: JsVal) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "put_field")
pub fn put_field(
  store: Store,
  obj: JsVal,
  key: PropertyKey,
  v: JsVal,
  create create: Bool,
) -> Store

@external(erlang, "arc_interp_prop_ffi", "new_object")
pub fn new_object(
  store: Store,
  proto: Handle,
  keys: List(PropertyKey),
  count: Int,
  stack: List(JsVal),
) -> #(JsVal, List(JsVal), Store)

@external(erlang, "arc_interp_prop_ffi", "new_receiver")
pub fn new_receiver(agent: Agent, proto: JsVal) -> #(JsVal, Agent)

@external(erlang, "arc_interp_prop_ffi", "define_field")
pub fn define_field(
  store: Store,
  obj: JsVal,
  key: PropertyKey,
  v: JsVal,
) -> Store

@external(erlang, "arc_interp_prop_ffi", "put_elem")
pub fn put_elem(store: Store, obj: JsVal, index: JsVal, v: JsVal) -> Store

@external(erlang, "arc_interp_locals_ffi", "frame_locals")
pub fn frame_locals(
  env: env,
  lexical: LexicalSlots,
  this: JsVal,
  active_func: JsVal,
  home_object: JsVal,
  new_target: JsVal,
  args: List(JsVal),
  arity: Int,
  local_count: Int,
) -> TupleArray(JsVal)

@external(erlang, "arc_interp_locals_ffi", "sloppy_this")
pub fn sloppy_this(this: JsVal, global: Handle) -> JsVal

@external(erlang, "arc_interp_locals_ffi", "flush_regs")
pub fn flush_regs(
  locals: TupleArray(JsVal),
  a: Int,
  b: Int,
  r0: JsVal,
  r1: JsVal,
) -> TupleArray(JsVal)

// hd([atom]) folds to a constant, not a call
@external(erlang, "erlang", "hd")
pub fn val(of: List(Sentinel)) -> JsVal

@external(erlang, "erlang", "hd")
pub fn object(of: List(Handle)) -> JsVal

@external(erlang, "erlang", "hd")
pub fn handle(of: List(JsVal)) -> Handle

pub type Accessor {
  Accessor(get: Option(JsVal), set: Option(JsVal))
  NoAccessor
}

// the accessor a plain chain resolves key to, if that is what it holds
@external(erlang, "arc_interp_prop_ffi", "find_accessor")
pub fn find_accessor(agent: Agent, obj: JsVal, key: PropertyKey) -> Accessor

// for-in keeps its pending keys on the operand stack, never in the heap
pub type ForInStep {
  ForInKey(key: JsVal, rest: JsVal)
  ForInEnd
}

@external(erlang, "arc_interp_ffi", "for_in_list")
pub fn for_in_list(keys: List(JsVal)) -> JsVal

@external(erlang, "arc_interp_ffi", "for_in_next")
pub fn for_in_next(iter: JsVal) -> ForInStep
