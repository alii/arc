//// Split a long straight-line function body into a chain of tail-calling
//// functions. erlc's per-function passes (erl_lint, v3_core, sys_core_fold,
//// beam_ssa_opt) are superlinear in the number of variables one function
//// binds, so a body whose top-level `Let` spine is long compiles far faster
//// as several functions of bounded spine length. Pure IR→IR: the cut is
//// always on the body's outermost `Let` spine, so no label, loop or `Try`
//// region is crossed and the tail's `Return`s keep their meaning through the
//// tail call.

import carder/ir
import carder/middle/ir_opt/loop_analysis
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string

/// Spine `Let`s per emitted function. On a 1,800-let straight-line body
/// (min of 3, load ~15) erlc took 5.9 s unsplit, 1.23 s at 128, 1.03 s at
/// 64 and 0.89 s at 32; 64 keeps the chunk count (and the tail calls) low
/// for the last few percent.
pub const chunk = 64

/// BEAM caps a function at 255 arguments and the backend adds the store;
/// a cut whose live set is wider than this waits for a narrower point.
const max_live = 250

/// Split `f` when its top-level `Let` spine is at least `2 * chunk` long.
/// Returns `f` (rewritten to tail-call the first continuation) followed by
/// the continuation functions `<name>_c<spine index>` in source order; a
/// short body comes back unchanged as `[f]`.
pub fn function(f: ir.Function) -> List(ir.Function) {
  let #(spine, tail) = unzip_spine(f.body, [])
  case list.length(spine) >= 2 * chunk {
    False -> [f]
    True -> {
      let params =
        list.fold(f.params, dict.new(), fn(acc, l) {
          dict.insert(acc, l.name, l.ty)
        })
      let env =
        list.fold(spine, params, fn(env, node) {
          bind_types(env, node.0, rhs_types(node.1, env))
        })
      let bound =
        list.fold(spine, dict.keys(params) |> set.from_list, fn(acc, node) {
          list.fold(node.0, acc, set.insert)
        })
      let init =
        Cut(
          rest: tail,
          rest_fv: loop_analysis.free_vars(tail),
          since_cut: 0,
          index: list.length(spine),
          helpers: [],
        )
      let cut =
        list.fold(list.reverse(spine), init, fn(cut, node) {
          cut_step(f, env, bound, cut, node)
        })
      [ir.Function(..f, body: cut.rest), ..cut.helpers]
    }
  }
}

/// Backward-pass state: `rest` is the rebuilt body from the current spine
/// position to the end, `rest_fv` every `Var` occurring in it minus the
/// spine names it binds, `index` the position of the next spine node from
/// the front, `helpers` the continuations emitted so far (in reverse cut
/// order, which is source order).
type Cut {
  Cut(
    rest: ir.Expr,
    rest_fv: Set(String),
    since_cut: Int,
    index: Int,
    helpers: List(ir.Function),
  )
}

fn cut_step(
  f: ir.Function,
  env: Dict(String, ir.ValType),
  bound: Set(String),
  cut: Cut,
  node: #(List(String), ir.Expr),
) -> Cut {
  let #(names, rhs) = node
  let index = cut.index - 1
  let live =
    set.intersection(cut.rest_fv, bound)
    |> set.to_list
    |> list.sort(string.compare)
  let live_types = list.try_map(live, dict.get(env, _))
  // Cut below `names` when the tail is a full chunk, we are not at the very
  // top, the live set fits a call, and every live value has a known type to
  // declare the param with.
  let ready =
    cut.since_cut >= chunk && index > 0 && list.length(live) <= max_live
  case ready, live_types {
    True, Ok(tys) -> {
      let name = f.name <> "_c" <> int.to_string(index)
      let helper =
        ir.Function(
          name:,
          params: list.map2(live, tys, ir.Local),
          result: f.result,
          locals: [],
          body: cut.rest,
        )
      let call = ir.ReturnCall(name, list.map(live, ir.Var))
      Cut(
        rest: ir.Let(names, rhs, call),
        rest_fv: set.difference(
          set.union(loop_analysis.free_vars(rhs), set.from_list(live)),
          set.from_list(names),
        ),
        since_cut: 1,
        index:,
        helpers: [helper, ..cut.helpers],
      )
    }
    _, _ ->
      Cut(
        rest: ir.Let(names, rhs, cut.rest),
        rest_fv: set.difference(
          set.union(loop_analysis.free_vars(rhs), cut.rest_fv),
          set.from_list(names),
        ),
        since_cut: cut.since_cut + 1,
        index:,
        helpers: cut.helpers,
      )
  }
}

fn unzip_spine(
  e: ir.Expr,
  acc: List(#(List(String), ir.Expr)),
) -> #(List(#(List(String), ir.Expr)), ir.Expr) {
  case e {
    ir.Let(names, rhs, body) -> unzip_spine(body, [#(names, rhs), ..acc])
    _ -> #(list.reverse(acc), e)
  }
}

fn bind_types(
  env: Dict(String, ir.ValType),
  names: List(String),
  tys: Option(List(ir.ValType)),
) -> Dict(String, ir.ValType) {
  case tys {
    Some(tys) ->
      list.zip(names, tys)
      |> list.fold(env, fn(env, p) { dict.insert(env, p.0, p.1) })
    None -> env
  }
}

/// Result types of a `Let` right-hand side where they can be read off the
/// node. `None` (numeric ops, conversions, …) only means a value bound here
/// cannot be a continuation param, so a cut waits until it is dead.
fn rhs_types(
  e: ir.Expr,
  env: Dict(String, ir.ValType),
) -> Option(List(ir.ValType)) {
  case e {
    ir.If(_, r, _, _)
    | ir.Block(_, r, _)
    | ir.Loop(_, _, r, _)
    | ir.Try(r, _, _)
    | ir.Switch(_, r, _, _) -> Some(r)
    ir.Values(vs) -> option.all(list.map(vs, value_type(env, _)))
    ir.Let(names, rhs, body) ->
      rhs_types(body, bind_types(env, names, rhs_types(rhs, env)))
    ir.Charge(_, body) -> rhs_types(body, env)
    ir.CallHost(..)
    | ir.CallDirect(..)
    | ir.CallClosure(..)
    | ir.MakeClosure(..)
    | ir.TermOp(ir.MakeTuple, _)
    | ir.TermOp(ir.TupleGet(_), _)
    | ir.TermOp(ir.MakeCons, _)
    | ir.TermOp(ir.MakeNil, _)
    | ir.TermOp(ir.ListHead, _)
    | ir.TermOp(ir.ListTail, _)
    | ir.NumTerm(ir.NAdd, _, _)
    | ir.NumTerm(ir.NSub, _, _)
    | ir.NumTerm(ir.NMul, _, _) -> Some([ir.TTerm])
    ir.TermTest(..)
    | ir.TermTag(_)
    | ir.TermOp(ir.TupleSize, _)
    | ir.TermOp(ir.IsEmptyList, _)
    | ir.NumTerm(ir.NLt, _, _)
    | ir.NumTerm(ir.NLe, _, _)
    | ir.NumTerm(ir.NGt, _, _)
    | ir.NumTerm(ir.NGe, _, _)
    | ir.NumTerm(ir.NEq, _, _) -> Some([ir.TI32])
    _ -> None
  }
}

fn value_type(
  env: Dict(String, ir.ValType),
  v: ir.Value,
) -> Option(ir.ValType) {
  case v {
    ir.Var(n) -> option.from_result(dict.get(env, n))
    ir.ConstI32(_) -> Some(ir.TI32)
    ir.ConstI64(_) -> Some(ir.TI64)
    ir.ConstF32(_) -> Some(ir.TF32)
    ir.ConstF64(_) -> Some(ir.TF64)
    ir.ConstV128(_) -> Some(ir.TV128)
    ir.ConstNull(rt) -> Some(ir.reftype_to_valtype(rt))
    ir.ConstAtom(_) | ir.ConstBinary(_) -> Some(ir.TTerm)
  }
}
