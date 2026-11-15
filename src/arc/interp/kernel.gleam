// exports may answer Miss, test with is(v, Miss) before use

import arc/bytecode/key.{type PropertyKey}
import arc/bytecode/lexical.{type LexicalSlots}
import arc/internal/tuple_array.{type TupleArray}
import arc/rt/bytecode.{type EnvTuple, type Regs}
import arc/rt/types.{type Agent, type Cell, type Handle, type JsVal}

import gleam/option.{type Option}

pub type Sentinel {
  Miss
  JsTdz
  Undefined
  Null
}

@external(erlang, "erlang", "=:=")
pub fn is(v: a, s: Sentinel) -> Bool

@external(erlang, "arc_interp_ffi", "cell_of")
pub fn cell_of(st: Agent, v: JsVal) -> Cell

// builds what a closure captures out of `locals`. `nested` is its own
// template's nested_captures and `from` is that of the frame's function
@external(erlang, "arc_rt_env_ffi", "capture")
pub fn capture_env(
  descriptors: List(Int),
  nested: Int,
  locals: TupleArray(JsVal),
  from: Int,
) -> EnvTuple

// the captures of a frame that keeps them nested; position is always 1
@external(erlang, "erlang", "element")
pub fn nested_captures(
  position: Int,
  locals: TupleArray(JsVal),
) -> TupleArray(JsVal)

@external(erlang, "arc_interp_ffi", "list_from_array_like")
pub fn list_from_array_like(st: Agent, array_like: JsVal) -> List(JsVal)

@external(erlang, "arc_interp_locals_ffi", "frame_locals")
pub fn frame_locals(
  env: EnvTuple,
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

@external(erlang, "arc_interp_locals_ffi", "flush_registers")
pub fn flush_registers(
  locals: TupleArray(JsVal),
  regs: Regs,
  r0: JsVal,
  r1: JsVal,
  r2: JsVal,
  r3: JsVal,
) -> TupleArray(JsVal)

// hd([atom]) folds to a constant, not a call
@external(erlang, "erlang", "hd")
pub fn literal(of: List(Sentinel)) -> JsVal

@external(erlang, "erlang", "hd")
pub fn object_val(of: List(Handle)) -> JsVal

@external(erlang, "erlang", "hd")
pub fn to_handle_unchecked(of: List(JsVal)) -> Handle

pub type Accessor {
  Accessor(get: Option(JsVal), set: Option(JsVal))
  NoAccessor
}

// accessor found by a plain chain lookup of key, if any
@external(erlang, "arc_interp_prop_ffi", "find_accessor")
pub fn find_accessor(st: Agent, obj: JsVal, key: PropertyKey) -> Accessor

// for-in keeps its pending keys on the operand stack, never in the heap
@external(erlang, "arc_interp_ffi", "for_in_list")
pub fn for_in_list(keys: List(JsVal)) -> JsVal

pub type IterPlan {
  Advanced(done: Bool, value: JsVal, st: Agent)
  // an entry that still needs its pair array
  PairAdvanced(key: JsVal, value: JsVal, st: Agent)
  ResumeGenerator(gen_h: Handle)
  IterMiss
}

// steps in the kernel only when the step observes nothing
@external(erlang, "arc_rt_lang_ffi", "iter_step")
pub fn iter_step(st: Agent, rec: JsVal) -> IterPlan
