// long let spines make erlc superlinear, so split them

import carder/ir
import carder/middle/ir_opt/loop_analysis
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string

// measured sweet spot for erlc time
pub const chunk = 64

// beam caps a function at 255 args
const max_live = 250

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
  let candidate = case cut.since_cut >= chunk && index > 0 {
    False -> None
    True -> {
      let live =
        set.intersection(cut.rest_fv, bound)
        |> set.to_list
        |> list.sort(string.compare)
      case list.length(live) <= max_live, list.try_map(live, dict.get(env, _)) {
        True, Ok(tys) -> Some(#(live, tys))
        _, _ -> None
      }
    }
  }
  case candidate {
    Some(#(live, tys)) -> {
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
    None ->
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
