//// continuation-passing vocabulary: let_ binds, host calls a js host op,
//// continuations take values first, emitter last

import arc_aot/emit/state.{
  type EmitResult, type Emitter, type Next, type NextWith,
}
import carder/ir

pub fn let_(e: Emitter, rhs: ir.Expr, k: NextWith(ir.Value)) -> EmitResult {
  state.let_(e, rhs, k)
}

pub fn host(
  e: Emitter,
  op: String,
  args: List(ir.Value),
  k: NextWith(ir.Value),
) -> EmitResult {
  let_(e, ir.CallHost("js", op, args), k)
}

pub fn host_unit(
  e: Emitter,
  op: String,
  args: List(ir.Value),
  k: Next,
) -> EmitResult {
  use _, e <- host(e, op, args)
  k(e)
}

pub fn cons_list(
  e: Emitter,
  vs: List(ir.Value),
  k: NextWith(ir.Value),
) -> EmitResult {
  case vs {
    [] -> host(e, "empty_list", [], k)
    [head, ..rest] -> {
      use tail, e <- cons_list(e, rest)
      let_(e, ir.TermOp(ir.MakeCons, [head, tail]), k)
    }
  }
}

pub fn each(
  e: Emitter,
  items: List(a),
  then k: Next,
  with step: fn(Emitter, a, Next) -> EmitResult,
) -> EmitResult {
  case items {
    [] -> k(e)
    [x, ..rest] -> step(e, x, fn(e) { each(e, rest, k, step) })
  }
}
