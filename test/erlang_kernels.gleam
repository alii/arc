// kernels only threaded code calls, bound here so tests can; each may answer
// kernel.Miss

import arc/bytecode/binop
import arc/bytecode/key.{type PropertyKey}
import arc/rt/types.{type Agent, type JsVal}

// in and instanceof miss
@external(erlang, "arc_rt_ops_ffi", "binop")
pub fn binop(kind: binop.BinOp, a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "add")
pub fn add(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "sub")
pub fn sub(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "mul")
pub fn mul(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "step")
pub fn step(a: JsVal, delta: Int) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "lt")
pub fn lt(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "strict_eq")
pub fn strict_eq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "strict_neq")
pub fn strict_neq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "eq")
pub fn eq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "neq")
pub fn neq(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "has_property")
pub fn has_property(st: Agent, key: JsVal, obj: JsVal) -> JsVal

@external(erlang, "arc_interp_ffi", "type_of")
pub fn type_of(st: Agent, v: JsVal) -> String

@external(erlang, "arc_interp_prop_ffi", "get_field")
pub fn get_field(st: Agent, obj: JsVal, key: PropertyKey) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "get_elem")
pub fn get_elem(st: Agent, obj: JsVal, key: JsVal) -> JsVal

@external(erlang, "arc_interp_prop_ffi", "put_field")
pub fn put_field(
  st: Agent,
  obj: JsVal,
  key: PropertyKey,
  v: JsVal,
  create create: Bool,
) -> Agent

@external(erlang, "arc_interp_prop_ffi", "put_elem")
pub fn put_elem(st: Agent, obj: JsVal, index: JsVal, v: JsVal) -> Agent
