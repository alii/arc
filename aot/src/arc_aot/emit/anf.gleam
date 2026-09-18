import arc/bytecode/key
import arc/parser/ast
import arc/rt/val
import arc_aot/emit/state.{type Emitter, Emitter}
import carder/ir
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}

pub type Build(a) =
  fn(Emitter, fn(Emitter, a) -> #(ir.Expr, Emitter)) -> #(ir.Expr, Emitter)

pub fn wrap(
  p: #(ir.Expr, Emitter),
  f: fn(ir.Expr) -> ir.Expr,
) -> #(ir.Expr, Emitter) {
  #(f(p.0), p.1)
}

pub fn pure(v: a) -> Build(a) {
  fn(e, k) { k(e, v) }
}

pub fn then(b: Build(a), f: fn(a) -> Build(c)) -> Build(c) {
  fn(e, k) { b(e, fn(e, a) { f(a)(e, k) }) }
}

pub fn bind(rhs: ir.Expr) -> Build(ir.Value) {
  fn(e, k) {
    let #(name, e) = state.fresh_var(e)
    wrap(k(e, ir.Var(name)), ir.Let([name], rhs, _))
  }
}

pub fn bind_number(rhs: ir.Expr) -> Build(ir.Value) {
  fn(e, k) {
    let #(name, e) = state.fresh_var(e)
    wrap(k(state.mark_known_number(e, name), ir.Var(name)), ir.Let(
      [name],
      rhs,
      _,
    ))
  }
}

pub fn mark_number(v: ir.Value) -> Build(ir.Value) {
  fn(e, k) {
    case v {
      ir.Var(name) -> k(state.mark_known_number(e, name), v)
      _ -> k(e, v)
    }
  }
}

pub fn is_known_number(e: Emitter, v: ir.Value) -> Bool {
  case v {
    ir.Var(name) -> state.is_known_number(e, name)
    ir.ConstI32(_) | ir.ConstI64(_) -> True
    _ -> False
  }
}

type StrParts {
  Ascii
  Tagged(BitArray, Int, List(Int))
}

@external(erlang, "arc_aot_anf_ffi", "str_parts")
fn str_parts(s: String) -> StrParts

// non-ascii text takes the runtime's tagged form, built from constants so
// the beam compiler folds it back into a literal
pub fn str_lit(s: String) -> Build(ir.Value) {
  case str_parts(s) {
    Ascii -> pure(ir.ConstBinary(bit_array.from_string(s)))
    Tagged(bin, len, crumbs) -> {
      use crumbs <- then(make_tuple(list.map(crumbs, ir.ConstI64)))
      use v <- then(
        make_tuple([
          ir.ConstAtom("js_str"),
          ir.ConstBinary(bin),
          ir.ConstI64(len),
          crumbs,
        ]),
      )
      mark_string(v)
    }
  }
}

pub fn mark_string(v: ir.Value) -> Build(ir.Value) {
  fn(e, k) {
    case v {
      ir.Var(name) -> k(state.mark_known_string(e, name), v)
      _ -> k(e, v)
    }
  }
}

pub fn is_known_string(e: Emitter, v: ir.Value) -> Bool {
  case v {
    ir.Var(name) -> state.is_known_string(e, name)
    ir.ConstBinary(_) -> True
    _ -> False
  }
}

pub fn host(op: String, args: List(ir.Value)) -> Build(ir.Value) {
  bind(ir.CallHost("js", op, args))
}

pub fn host_unit(op: String, args: List(ir.Value)) -> Build(Nil) {
  then(host(op, args), fn(_) { pure(Nil) })
}

pub fn cons_list(vs: List(ir.Value)) -> Build(ir.Value) {
  list.fold_right(vs, bind(ir.TermOp(ir.MakeNil, [])), fn(tail_b, head) {
    then(tail_b, fn(tail) { bind(ir.TermOp(ir.MakeCons, [head, tail])) })
  })
}

pub fn make_tuple(vs: List(ir.Value)) -> Build(ir.Value) {
  bind(ir.TermOp(ir.MakeTuple, vs))
}

pub fn tuple_get(v: ir.Value, i: Int) -> ir.Expr {
  ir.TermOp(ir.TupleGet(i), [v])
}

pub fn run(b: Build(ir.Value), e: Emitter) -> #(ir.Expr, Emitter) {
  b(e, fn(ef, v) { #(ir.Values([v]), ef) })
}

pub fn run_to(
  b: Build(a),
  e: Emitter,
  tail: fn(Emitter, a) -> ir.Expr,
) -> #(ir.Expr, Emitter) {
  b(e, fn(ef, a) { #(tail(ef, a), ef) })
}

// arms that rebind a slot thread the new name out through the result
fn slots_rebound(
  before: Dict(Int, String),
  after_: Dict(Int, String),
) -> List(Int) {
  dict.fold(after_, [], fn(acc, slot, name) {
    case dict.get(before, slot) == Ok(name) {
      True -> acc
      False -> [slot, ..acc]
    }
  })
  |> list.sort(int.compare)
}

fn merge_slots(a: List(Int), b: List(Int)) -> List(Int) {
  list.append(a, b) |> list.unique |> list.sort(int.compare)
}

fn append_tail(tree: ir.Expr, extra: List(ir.Value)) -> ir.Expr {
  case tree {
    ir.Let(names, rhs, body) -> ir.Let(names, rhs, append_tail(body, extra))
    ir.Values(vs) -> ir.Values(list.append(vs, extra))
    _ -> tree
  }
}

fn widen_breaks(
  tree: ir.Expr,
  label: String,
  extra: List(ir.Value),
) -> ir.Expr {
  let go = widen_breaks(_, label, extra)
  case tree {
    ir.Let(ns, rhs, body) -> ir.Let(ns, go(rhs), go(body))
    ir.If(c, tys, t, f) -> ir.If(c, tys, go(t), go(f))
    ir.Block(l, tys, body) -> ir.Block(l, tys, go(body))
    ir.Break(l, vs) if l == label -> ir.Break(l, list.append(vs, extra))
    _ -> tree
  }
}

fn rebind_slots(e: Emitter, slots: List(Int)) -> #(Emitter, List(String)) {
  let #(e, rev) =
    list.fold(slots, #(e, []), fn(acc, slot) {
      let #(e, ns) = acc
      let #(n, e) = state.fresh_slot_var(e, slot)
      #(state.set_slot_var(e, slot, n), [n, ..ns])
    })
  #(e, list.reverse(rev))
}

fn arm_slot_vals(e_arm: Emitter, slots: List(Int)) -> List(ir.Value) {
  list.map(slots, fn(s) { ir.Var(state.get_slot_var(e_arm, s)) })
}

pub fn bind_if(
  cond: ir.Value,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  bind_if_typed(cond, ir.TTerm, t, f)
}

pub fn bind_if_i32(
  cond: ir.Value,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  bind_if_typed(cond, ir.TI32, t, f)
}

fn bind_if_typed(
  cond: ir.Value,
  head_ty: ir.ValType,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  let one = fn(b) { map(b, fn(v) { [v] }) }
  use vs <- map(bind_if_n(cond, [head_ty], one(t), one(f)))
  let assert [v] = vs
  v
}

pub fn bind_if2(
  cond: ir.Value,
  t: Build(#(ir.Value, ir.Value)),
  f: Build(#(ir.Value, ir.Value)),
) -> Build(#(ir.Value, ir.Value)) {
  let two = fn(b) { map(b, fn(p: #(ir.Value, ir.Value)) { [p.0, p.1] }) }
  use vs <- map(bind_if_n(cond, [ir.TTerm, ir.TTerm], two(t), two(f)))
  let assert [a, b] = vs
  #(a, b)
}

fn bind_if_n(
  cond: ir.Value,
  head_tys: List(ir.ValType),
  t: Build(List(ir.Value)),
  f: Build(List(ir.Value)),
) -> Build(List(ir.Value)) {
  fn(e: Emitter, k) {
    let sv0 = e.slot_vars
    let values = fn(_, vs) { ir.Values(vs) }
    let #(then_tree, e_t) = run_to(t, e, values)
    let #(else_tree, e_f) = run_to(f, Emitter(..e_t, slot_vars: sv0), values)
    let carried =
      merge_slots(
        slots_rebound(sv0, e_t.slot_vars),
        slots_rebound(sv0, e_f.slot_vars),
      )
    let e = Emitter(..e_f, slot_vars: sv0)
    let #(e, rs) = fresh_vars(e, list.length(head_tys))
    let heads = list.map(rs, ir.Var)
    case carried {
      [] ->
        wrap(k(e, heads), ir.Let(
          rs,
          ir.If(cond, head_tys, then_tree, else_tree),
          _,
        ))
      _ -> {
        let then_tree = append_tail(then_tree, arm_slot_vals(e_t, carried))
        let else_tree = append_tail(else_tree, arm_slot_vals(e_f, carried))
        let #(e, out) = rebind_slots(e, carried)
        let tys = list.append(head_tys, list.map(carried, fn(_) { ir.TTerm }))
        wrap(k(e, heads), ir.Let(
          list.append(rs, out),
          ir.If(cond, tys, then_tree, else_tree),
          _,
        ))
      }
    }
  }
}

fn fresh_vars(e: Emitter, n: Int) -> #(Emitter, List(String)) {
  case n {
    0 -> #(e, [])
    _ -> {
      let #(r, e) = state.fresh_var(e)
      let #(e, rest) = fresh_vars(e, n - 1)
      #(e, [r, ..rest])
    }
  }
}

// §7.4.8 close on throw, the original error wins
pub fn close_iter_on_throw(iter: ir.Value, body: Build(Nil)) -> Build(Nil) {
  fn(e: Emitter, k) {
    let sv0 = e.slot_vars
    let #(body_tree, e_b) = run(then(body, fn(_) { pure(e.consts.undef) }), e)
    let carried = slots_rebound(sv0, e_b.slot_vars)
    let e = Emitter(..e_b, slot_vars: sv0)
    let #(exn, e) = state.fresh_var(e)
    let #(closed, e) = state.fresh_var(e)
    let #(r, e) = state.fresh_var(e)
    let handler =
      ir.Let(
        [closed],
        ir.CallHost("js", "iter_close", [iter, e.consts.true_]),
        ir.Throw(e.consts.exn_tag, [ir.Var(exn)]),
      )
    let body_tree = append_tail(body_tree, arm_slot_vals(e_b, carried))
    let #(e, out) = rebind_slots(e, carried)
    let tys = [ir.TTerm, ..list.map(carried, fn(_) { ir.TTerm })]
    wrap(k(e, Nil), ir.Let(
      [r, ..out],
      ir.Try(result: tys, body: body_tree, handlers: [
        ir.CatchHandler(
          on: ir.OnTag(e.consts.exn_tag),
          payload: [exn],
          exnref: None,
          handler:,
        ),
      ]),
      _,
    ))
  }
}

pub fn truthy_i32(v: ir.Value) -> Build(ir.Value) {
  host("to_boolean_i32", [v])
}

pub fn truthy_if(
  v: ir.Value,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  then(truthy_i32(v), bind_if(_, t, f))
}

// ir.If tests against 0 and a bare false atom is not 0
pub fn is_true_expr(v: ir.Value) -> ir.Expr {
  ir.NumTerm(ir.NEq, v, ir.ConstAtom("true"))
}

pub fn is_true(v: ir.Value) -> Build(ir.Value) {
  bind(is_true_expr(v))
}

pub fn host_bool(op: String, args: List(ir.Value)) -> Build(ir.Value) {
  then(host(op, args), is_true)
}

pub fn nullish_if(
  v: ir.Value,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  then(host_bool("is_nullish", [v]), bind_if(_, t, f))
}

pub fn bind_block(body: fn(String) -> Build(ir.Value)) -> Build(ir.Value) {
  fn(e: Emitter, k) {
    let sv0 = e.slot_vars
    let #(label, e) = state.fresh_label(e)
    let #(body_tree, e_b) = run(body(label), e)
    let carried = slots_rebound(sv0, e_b.slot_vars)
    let e = Emitter(..e_b, slot_vars: sv0)
    let #(r, e) = state.fresh_var(e)
    case carried {
      [] ->
        wrap(k(e, ir.Var(r)), ir.Let(
          [r],
          ir.Block(label, [ir.TTerm], body_tree),
          _,
        ))
      _ -> {
        let body_tree =
          append_tail(body_tree, arm_slot_vals(e_b, carried))
          |> widen_breaks(label, arm_slot_vals(e, carried))
        let #(e, out) = rebind_slots(e, carried)
        let tys = [ir.TTerm, ..list.map(carried, fn(_) { ir.TTerm })]
        wrap(k(e, ir.Var(r)), ir.Let(
          [r, ..out],
          ir.Block(label, tys, body_tree),
          _,
        ))
      }
    }
  }
}

pub fn map(b: Build(a), f: fn(a) -> c) -> Build(c) {
  fn(e, k) { b(e, fn(e, a) { k(e, f(a)) }) }
}

pub fn seq(bs: List(Build(a))) -> Build(List(a)) {
  case bs {
    [] -> pure([])
    [b, ..rest] -> then(b, fn(a) { map(seq(rest), fn(tail) { [a, ..tail] }) })
  }
}

fn number_guard(v: ir.Value) -> Build(#(ir.Value, Bool)) {
  fn(e, k) {
    case is_known_number(e, v) {
      True -> k(e, #(ir.ConstI32(1), True))
      False ->
        bind(ir.TermTest(ir.IsNumber, v))(e, fn(e, g) { k(e, #(g, False)) })
    }
  }
}

// nested if, not IAnd: IAnd lowers to a cross-module call
fn both_numbers(a: ir.Value, b: ir.Value) -> Build(#(ir.Value, Bool)) {
  use #(ga, ea) <- then(number_guard(a))
  use #(gb, eb) <- then(number_guard(b))
  case ea, eb {
    True, True -> pure(#(ir.ConstI32(1), True))
    True, False -> pure(#(gb, False))
    False, True -> pure(#(ga, False))
    False, False ->
      map(
        bind(ir.If(ga, [ir.TI32], ir.Values([gb]), ir.Values([ir.ConstI32(0)]))),
        fn(g) { #(g, False) },
      )
  }
}

pub fn num_binop(op: String, a: ir.Value, b: ir.Value) -> Build(ir.Value) {
  let on_overflow = host(op, [a, b])
  then(int_or(op, a, b, on_overflow, on_overflow), mark_number)
}

fn int_or(
  op: String,
  a: ir.Value,
  b: ir.Value,
  on_overflow: Build(ir.Value),
  otherwise: Build(ir.Value),
) -> Build(ir.Value) {
  let arm = case op {
    "add" -> Some(int_arm(ir.NAdd, a, b, zero_sign: False, on_overflow:))
    "sub" -> Some(int_arm(ir.NSub, a, b, zero_sign: False, on_overflow:))
    "mul" -> Some(int_arm(ir.NMul, a, b, zero_sign: True, on_overflow:))
    _ -> None
  }
  case arm {
    Some(arm) -> {
      use ii <- then(both_ints(a, b))
      bind_if(ii, arm, otherwise)
    }
    None -> otherwise
  }
}

const max_safe_int = 9_007_199_254_740_991

fn both_ints(a: ir.Value, b: ir.Value) -> Build(ir.Value) {
  case is_const_int(a), is_const_int(b) {
    True, True -> pure(ir.ConstI32(1))
    True, False -> bind(ir.TermTest(ir.IsInt, b))
    False, True -> bind(ir.TermTest(ir.IsInt, a))
    False, False -> {
      use ga <- then(bind(ir.TermTest(ir.IsInt, a)))
      use gb <- then(bind(ir.TermTest(ir.IsInt, b)))
      bind(ir.If(ga, [ir.TI32], ir.Values([gb]), ir.Values([ir.ConstI32(0)])))
    }
  }
}

fn is_const_int(v: ir.Value) -> Bool {
  case v {
    ir.ConstI32(_) | ir.ConstI64(_) -> True
    _ -> False
  }
}

fn int_arm(
  op: ir.NumTermOp,
  a: ir.Value,
  b: ir.Value,
  zero_sign zero_sign: Bool,
  on_overflow on_overflow: Build(ir.Value),
) -> Build(ir.Value) {
  use r <- then(bind(ir.NumTerm(op, a, b)))
  use hi <- then(bind(ir.NumTerm(ir.NLe, r, ir.ConstI64(max_safe_int))))
  use fits <- then(bind_if_i32(
    hi,
    bind(ir.NumTerm(ir.NGe, r, ir.ConstI64(-max_safe_int))),
    pure(ir.ConstI32(0)),
  ))
  case zero_sign {
    False -> bind_if(fits, pure(r), on_overflow)
    True -> {
      use nz <- then(bind_if_i32(
        fits,
        bind(ir.NumTerm(ir.NEq, r, ir.ConstI32(0))),
        pure(ir.ConstI32(1)),
      ))
      bind_if(nz, on_overflow, pure(r))
    }
  }
}

pub fn guarded_binop(
  kernel_op: String,
  general_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  fn(e, k) {
    let general = host(general_op, [a, b])
    case is_known_number(e, a) && is_known_number(e, b) {
      True -> num_binop(kernel_op, a, b)(e, k)
      False -> {
        let str = is_known_string(e, a) || is_known_string(e, b)
        case str || non_number_const(a) || non_number_const(b), kernel_op {
          True, "add" -> {
            let add = miss_or(host("add", [a, b]), general)
            case str {
              True -> then(add, mark_string)(e, k)
              False -> add(e, k)
            }
          }
          True, _ -> general(e, k)
          False, _ -> {
            let otherwise = case kernel_op {
              "add" -> miss_or(host("add", [a, b]), general)
              _ -> if_both_numbers(kernel_op, a, b, general)
            }
            int_or(kernel_op, a, b, host(kernel_op, [a, b]), otherwise)(e, k)
          }
        }
      }
    }
  }
}

fn non_number_const(v: ir.Value) -> Bool {
  case v {
    ir.ConstBinary(_) | ir.ConstAtom(_) -> True
    _ -> False
  }
}

pub fn miss_or(
  probe: Build(ir.Value),
  general: Build(ir.Value),
) -> Build(ir.Value) {
  use r <- then(probe)
  use m <- then(bind(ir.NumTerm(ir.NEq, r, ir.ConstAtom("miss"))))
  bind_if(m, general, pure(r))
}

fn if_both_numbers(
  kernel_op: String,
  a: ir.Value,
  b: ir.Value,
  general: Build(ir.Value),
) -> Build(ir.Value) {
  use #(both, elided) <- then(both_numbers(a, b))
  case elided {
    True -> host(kernel_op, [a, b])
    False -> bind_if(both, host(kernel_op, [a, b]), general)
  }
}

pub fn guarded_div(a: ir.Value, b: ir.Value) -> Build(ir.Value) {
  if_both_numbers("div", a, b, host("div_general", [a, b]))
}

// rem matches js % except the -0 of a negative dividend
pub fn guarded_mod(a: ir.Value, b: ir.Value) -> Build(ir.Value) {
  let kernel = miss_or(host("mod", [a, b]), host("mod_general", [a, b]))
  case b {
    ir.ConstI32(c) if c > 0 -> {
      use is_i <- then(bind(ir.TermTest(ir.IsInt, a)))
      bind_if(
        is_i,
        {
          use r <- then(host("rem", [a, b]))
          use zero <- then(bind(ir.NumTerm(ir.NEq, r, ir.ConstI32(0))))
          use neg_zero <- then(bind_if_i32(
            zero,
            bind(ir.NumTerm(ir.NLt, a, ir.ConstI32(0))),
            pure(ir.ConstI32(0)),
          ))
          bind_if(neg_zero, kernel, then(pure(r), mark_number))
        },
        kernel,
      )
    }
    _ -> kernel
  }
}

pub fn guarded_neg(v: ir.Value) -> Build(ir.Value) {
  fn(e, k) {
    case is_known_number(e, v) {
      True -> then(host("neg", [v]), mark_number)(e, k)
      False ->
        {
          use is_n <- then(bind(ir.TermTest(ir.IsNumber, v)))
          bind_if(is_n, host("neg", [v]), host("neg_general", [v]))
        }(e, k)
    }
  }
}

pub fn guarded_cmp(
  term_op: ir.NumTermOp,
  general_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  fn(e, k) {
    case is_known_string(e, a) || is_known_string(e, b) {
      True -> then(host(general_op, [a, b]), i32_to_js_bool)(e, k)
      False -> guarded_cmp_numeric(term_op, general_op, a, b)(e, k)
    }
  }
}

fn guarded_cmp_numeric(
  term_op: ir.NumTermOp,
  general_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  use #(both, elided) <- then(both_numbers(a, b))
  let inline_arm = fn(e: Emitter, k) {
    let rc = e.consts
    then(bind(ir.NumTerm(term_op, a, b)), bind_if(
      _,
      pure(rc.true_),
      pure(rc.false_),
    ))(e, k)
  }
  case elided {
    True -> inline_arm
    False ->
      bind_if(both, inline_arm, then(host(general_op, [a, b]), i32_to_js_bool))
  }
}

// js comparison results are booleans, never leak a raw i32
pub fn i32_to_js_bool(v: ir.Value) -> Build(ir.Value) {
  fn(e: Emitter, k) {
    let rc = e.consts
    bind_if(v, pure(rc.true_), pure(rc.false_))(e, k)
  }
}

pub fn cond_cmp(
  term_op: ir.NumTermOp,
  general_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  fn(e, k) {
    case is_known_string(e, a) || is_known_string(e, b) {
      True ->
        then(host(general_op, [a, b]), fn(v) { host("to_boolean_i32", [v]) })(
          e,
          k,
        )
      False -> cond_cmp_numeric(term_op, general_op, a, b)(e, k)
    }
  }
}

fn cond_cmp_numeric(
  term_op: ir.NumTermOp,
  general_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  use #(both, elided) <- then(both_numbers(a, b))
  case elided {
    True -> bind(ir.NumTerm(term_op, a, b))
    False ->
      bind_if_i32(
        both,
        bind(ir.NumTerm(term_op, a, b)),
        then(host(general_op, [a, b]), fn(v) { host("to_boolean_i32", [v]) }),
      )
  }
}

// the one static key canonicalizer, output must be canonical
pub fn object_key_lit(pk: ast.PropertyKey) -> Build(ir.Value) {
  let inner = case pk {
    ast.KeyIdentifier(name:, ..) -> wire_named(name)
    ast.KeyString(value: s, ..) -> wire_prop_key(key.canonical_key(s))
    ast.KeyNumber(value: ast.FiniteNumber(f), ..) ->
      case key.array_index_of_float(f) {
        Some(i) -> wire_index(i)
        None -> wire_named(val.js_format_float(f))
      }
    ast.KeyNumber(value: ast.InfiniteNumber, ..) -> wire_named("Infinity")
    ast.KeyBigInt(value: n, ..) -> wire_prop_key(key.index_key(n))
    ast.KeyPrivate(name:, ..) ->
      ir.TermOp(ir.MakeTuple, [
        ir.ConstAtom("private"),
        ir.ConstBinary(bit_array.from_string(name)),
      ])
    ast.KeyComputed(..) ->
      panic as "object_key_lit: KeyComputed routes through host(to_property_key)"
  }
  use iv <- then(bind(inner))
  make_tuple([ir.ConstAtom("string_key"), iv])
}

fn wire_prop_key(k: key.PropertyKey) -> ir.Expr {
  case k {
    key.Index(n) -> wire_index(n)
    key.Named(s) -> wire_named(s)
    key.Private(text) ->
      ir.TermOp(ir.MakeTuple, [
        ir.ConstAtom("private"),
        ir.ConstBinary(bit_array.from_string(text)),
      ])
  }
}

fn wire_index(n: Int) -> ir.Expr {
  ir.TermOp(ir.MakeTuple, [ir.ConstAtom("index"), ir.ConstI64(n)])
}

fn wire_named(s: String) -> ir.Expr {
  ir.TermOp(ir.MakeTuple, [
    ir.ConstAtom("named"),
    ir.ConstBinary(bit_array.from_string(s)),
  ])
}
