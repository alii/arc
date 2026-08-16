//// M11: Build(a) CPS monad + ANF let-binding combinators over
//// twocore/ir — bind/host/cons_list/bind_if/guarded_binop/object_key_lit.
//// Invariant #3: `host` is the ONLY CallHost("js", ..) site in emit_2core/*.

import arc/bytecode/key
import arc/parser/ast
import arc/rt/val
import arc_aot/emit/state.{type Emitter2, Emitter2}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import twocore/ir

/// Tail continuation receives the final Emitter2 + result and returns the
/// terminal ir.Expr paired with the emitter it finished with; the builder
/// wraps the tree in Let-bindings and passes the emitter through.
pub type Build(a) =
  fn(Emitter2, fn(Emitter2, a) -> #(ir.Expr, Emitter2)) -> #(ir.Expr, Emitter2)

/// Rewrap the tree of a `#(tree, e)` continuation result, keeping the emitter.
pub fn wrap(
  p: #(ir.Expr, Emitter2),
  f: fn(ir.Expr) -> ir.Expr,
) -> #(ir.Expr, Emitter2) {
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

/// `bind` that also records the fresh var as a known BEAM number (int|float
/// term). Use for `Convert(BoxInt,·)` and other
/// rhs whose result is provably a number term — lets `guarded_binop`/`cmp`
/// elide the `is_number` TermTest on that operand.
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

/// Record an existing `ir.Var` as a known BEAM number and yield it unchanged.
/// No-op for non-`Var` values (only Var names are tracked).
pub fn mark_number(v: ir.Value) -> Build(ir.Value) {
  fn(e, k) {
    case v {
      ir.Var(name) -> k(state.mark_known_number(e, name), v)
      _ -> k(e, v)
    }
  }
}

/// True iff `v` is an `ir.Var` previously recorded via `bind_number` /
/// `mark_number`. All ir.Const* are machine-typed or non-numeric terms, so
/// only tracked Vars qualify.
pub fn is_known_number(e: Emitter2, v: ir.Value) -> Bool {
  case v {
    ir.Var(name) -> state.is_known_number(e, name)
    ir.ConstI32(_) | ir.ConstI64(_) -> True
    _ -> False
  }
}

/// Bind a `js` host call. D2: NO St arg — emit_core M9 injects instance state.
/// Invariant #3: this is the ONLY CallHost("js", ..) constructor in emit_2core/*.
pub fn host(op: String, args: List(ir.Value)) -> Build(ir.Value) {
  bind(ir.CallHost("js", op, args))
}

/// `host` for unit-typed JMut ops — let-binds the call, discards the result.
pub fn host_unit(op: String, args: List(ir.Value)) -> Build(Nil) {
  then(host(op, args), fn(_) { pure(Nil) })
}

/// Right-fold `MakeCons` over `vs` onto the `[]` literal (`ir.MakeNil` — lowers
/// to Core `CNil`, no host call; JPure `empty_list` seed cost ~40k µs/1M calls).
pub fn cons_list(vs: List(ir.Value)) -> Build(ir.Value) {
  list.fold_right(vs, bind(ir.TermOp(ir.MakeNil, [])), fn(tail_b, head) {
    then(tail_b, fn(tail) { bind(ir.TermOp(ir.MakeCons, [head, tail])) })
  })
}

/// Bind a fresh tuple `{v₁,…,vₙ}` built from `vs`.
pub fn make_tuple(vs: List(ir.Value)) -> Build(ir.Value) {
  bind(ir.TermOp(ir.MakeTuple, vs))
}

/// Project element `i` (0-based, R7) of tuple `v` as a raw Expr — caller
/// `bind`s it when a Value is needed.
pub fn tuple_get(v: ir.Value, i: Int) -> ir.Expr {
  ir.TermOp(ir.TupleGet(i), [v])
}

/// Run a Build to a Values-terminal ir.Expr and the final Emitter2 (the one
/// the tail continuation received, or the one a diverging Build stopped at).
pub fn run(b: Build(ir.Value), e: Emitter2) -> #(ir.Expr, Emitter2) {
  b(e, fn(ef, v) { #(ir.Values([v]), ef) })
}

/// Run a Build to a caller-supplied TERMINAL ir.Expr (Return/Continue/If…)
/// instead of the default `Values([v])`. M18 arm bodies end in Step-tuple
/// `Return`s / `Continue(Lresume,…)`.
pub fn run_to(
  b: Build(a),
  e: Emitter2,
  tail: fn(Emitter2, a) -> ir.Expr,
) -> #(ir.Expr, Emitter2) {
  b(e, fn(ef, a) { #(tail(ef, a), ef) })
}

// ── slot-rebind threading (SPEC§9.12 / emit.binding() opt_level) ──
// A write_slot(_, False, _) inside an arm binds a fresh name INSIDE that arm's
// tree and leaks it via e.slot_vars to the outer continuation, where it is out
// of ir.Let-scope. bind_if/bind_block therefore snapshot slot_vars, run each
// arm, and thread any rebound slot out through the wrapper's result tuple —
// the join re-binds a fresh name in the OUTER Build so downstream reads stay
// in scope under Baseline propagate/dead-let (which would otherwise erase the
// arm-local binder).

/// Slot ids whose name in `after_` differs from `before` (sorted).
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

/// Sorted-unique union of two slot lists.
fn merge_slots(a: List(Int), b: List(Int)) -> List(Int) {
  list.append(a, b) |> list.unique |> list.sort(int.compare)
}

/// Descend a `run` tree's Let-spine and widen its terminal `Values([v])` to
/// `Values([v, ..extra])`. A non-Values terminal (Break/Throw…) diverges before
/// the join and is left unchanged — that path never falls through, so the
/// wrapper's result arity is irrelevant on it.
fn append_tail(tree: ir.Expr, extra: List(ir.Value)) -> ir.Expr {
  case tree {
    ir.Let(names, rhs, body) -> ir.Let(names, rhs, append_tail(body, extra))
    ir.Values(vs) -> ir.Values(list.append(vs, extra))
    _ -> tree
  }
}

/// Widen every `Break(label, vs)` in `tree` to `Break(label, vs ++ extra)`.
/// Descends Let rhs+body, If arms, and nested Block bodies — the shapes an
/// emit_chain body can nest a Break under (Loop/Try/Switch don't occur there).
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

/// Mint a fresh var per `slots`, set_slot_var each, return names in slot order.
fn rebind_slots(e: Emitter2, slots: List(Int)) -> #(Emitter2, List(String)) {
  let #(e, rev) =
    list.fold(slots, #(e, []), fn(acc, slot) {
      let #(e, ns) = acc
      let #(n, e) = state.fresh_var(e)
      #(state.set_slot_var(e, slot, n), [n, ..ns])
    })
  #(e, list.reverse(rev))
}

/// Current ir.Var for each `slots` under `e_arm` — the arm's rebound name if
/// it wrote the slot, else the entry-snapshot name (in scope at arm entry).
fn arm_slot_vals(e_arm: Emitter2, slots: List(Int)) -> List(ir.Value) {
  list.map(slots, fn(s) { ir.Var(state.get_slot_var(e_arm, s)) })
}

pub fn bind_if(
  cond: ir.Value,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  bind_if_typed(cond, ir.TTerm, t, f)
}

/// As `bind_if` but the If's result type is `[TI32]` — for raw i32 truth
/// values fed straight to a downstream `ir.If` cond (no bool-atom round-trip).
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
  fn(e: Emitter2, k) {
    let sv0 = e.slot_vars
    let #(then_tree, e_t) = run(t, e)
    let #(else_tree, e_f) = run(f, Emitter2(..e_t, slot_vars: sv0))
    let carried =
      merge_slots(
        slots_rebound(sv0, e_t.slot_vars),
        slots_rebound(sv0, e_f.slot_vars),
      )
    let e = Emitter2(..e_f, slot_vars: sv0)
    let #(r, e) = state.fresh_var(e)
    case carried {
      [] ->
        wrap(k(e, ir.Var(r)), ir.Let(
          [r],
          ir.If(cond, [head_ty], then_tree, else_tree),
          _,
        ))
      _ -> {
        let then_tree = append_tail(then_tree, arm_slot_vals(e_t, carried))
        let else_tree = append_tail(else_tree, arm_slot_vals(e_f, carried))
        let #(e, out) = rebind_slots(e, carried)
        let tys = [head_ty, ..list.map(carried, fn(_) { ir.TTerm })]
        wrap(k(e, ir.Var(r)), ir.Let(
          [r, ..out],
          ir.If(cond, tys, then_tree, else_tree),
          _,
        ))
      }
    }
  }
}

/// Run `body` under a try that, on a JS throw, closes `iter` with an
/// abrupt completion and rethrows (§7.4.8 IteratorClose on a throw
/// completion — the close's own throw is dropped, the original wins). Slot
/// rebinds inside `body` thread out through the Try's result the way
/// `bind_if` threads an arm's.
pub fn close_iter_on_throw(iter: ir.Value, body: Build(Nil)) -> Build(Nil) {
  fn(e: Emitter2, k) {
    let sv0 = e.slot_vars
    let #(body_tree, e_b) = run(then(body, fn(_) { pure(e.consts.undef) }), e)
    let carried = slots_rebound(sv0, e_b.slot_vars)
    let e = Emitter2(..e_b, slot_vars: sv0)
    let #(exn, e) = state.fresh_var(e)
    let #(closed, e) = state.fresh_var(e)
    let #(r, e) = state.fresh_var(e)
    let handler =
      ir.Let(
        [closed],
        ir.CallHost("js", "iter_close", [iter, e.consts.true_]),
        ir.Throw(e.consts.js_tag, [ir.Var(exn)]),
      )
    let body_tree = append_tail(body_tree, arm_slot_vals(e_b, carried))
    let #(e, out) = rebind_slots(e, carried)
    let tys = [ir.TTerm, ..list.map(carried, fn(_) { ir.TTerm })]
    wrap(k(e, Nil), ir.Let(
      [r, ..out],
      ir.Try(result: tys, body: body_tree, handlers: [
        ir.CatchHandler(
          on: ir.OnTag(e.consts.js_tag),
          payload: [exn],
          exnref: None,
          handler:,
        ),
      ]),
      _,
    ))
  }
}

/// §7.1.2 ToBoolean(v) as a raw i32 for `ir.If` conds. Inlines the three
/// operand shapes richards' 21k/run truthy sites actually see —
/// `true`/`false` atoms (from `!=`/`<`/`!`) and bare Int 0|1 (from `==`'s
/// loose_eq / `||`-propagated `==` result) — so the `to_boolean_i32`
/// call_ext fires only on strings/floats/objects. Checks ordered by warm
/// frequency: `true` (both sites) → `false` (LogicalOr `!=` operand) →
/// integer (if(method()) result). Each check is one `=:=`/`is_integer` BIF
/// (~2ns) vs the ~33ns call_ext. Shared by `truthy_if` and
/// `stmt.emit_cond_i32`'s fallthrough.
pub fn truthy_i32(v: ir.Value) -> Build(ir.Value) {
  use is_t <- then(bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("true"))))
  bind_if_i32(is_t, pure(ir.ConstI32(1)), {
    use is_f <- then(bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("false"))))
    bind_if_i32(is_f, pure(ir.ConstI32(0)), {
      use is_i <- then(bind(ir.TermTest(ir.IsInt, v)))
      bind_if_i32(
        is_i,
        // §7.1.2: Int 0 → 0, any other Int → 1. `NEq(v,0)` gives 1 iff v=:=0;
        // second `NEq(·,0)` inverts. Two BIFs, no call_ext.
        {
          use z <- then(bind(ir.NumTerm(ir.NEq, v, ir.ConstI32(0))))
          bind(ir.NumTerm(ir.NEq, z, ir.ConstI32(0)))
        },
        host("truthy", [v]),
      )
    })
  })
}

/// `if (js-truthy v) then t else f`. i32 via `truthy_i32` then a single
/// `bind_if`, so `t`/`f` are NOT duplicated.
pub fn truthy_if(
  v: ir.Value,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  then(truthy_i32(v), bind_if(_, t, f))
}

/// i32 1 iff `v` is the atom `true`. An `ir.If` tests its condition against
/// 0, so the Bool result of a host op (`strict_eq`, `is_nullish`) must go
/// through this before it can be a condition: a bare `false` atom is not 0.
pub fn is_true_expr(v: ir.Value) -> ir.Expr {
  ir.NumTerm(ir.NEq, v, ir.ConstAtom("true"))
}

pub fn is_true(v: ir.Value) -> Build(ir.Value) {
  bind(is_true_expr(v))
}

/// `host(op, args)` for a Bool-returning op, as an i32 condition.
pub fn host_bool(op: String, args: List(ir.Value)) -> Build(ir.Value) {
  then(host(op, args), is_true)
}

/// `if (v is null|undefined) then t else f`.
pub fn nullish_if(
  v: ir.Value,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  then(host_bool("is_nullish", [v]), bind_if(_, t, f))
}

pub fn bind_block(body: fn(String) -> Build(ir.Value)) -> Build(ir.Value) {
  fn(e: Emitter2, k) {
    let sv0 = e.slot_vars
    let #(label, e) = state.fresh_label(e)
    let #(body_tree, e_b) = run(body(label), e)
    let carried = slots_rebound(sv0, e_b.slot_vars)
    let e = Emitter2(..e_b, slot_vars: sv0)
    let #(r, e) = state.fresh_var(e)
    case carried {
      [] ->
        wrap(k(e, ir.Var(r)), ir.Let(
          [r],
          ir.Block(label, [ir.TTerm], body_tree),
          _,
        ))
      _ -> {
        // Fall-through gets the arm's rebound names; every Break to `label`
        // (chain_guard's short-circuit) gets the entry-snapshot names — the
        // write_slot lies past the guard, so sv0's name is the live value.
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

/// Sequence a list of Build actions left-to-right, collecting results in order.
pub fn seq(bs: List(Build(a))) -> Build(List(a)) {
  case bs {
    [] -> pure([])
    [b, ..rest] -> then(b, fn(a) { map(seq(rest), fn(tail) { [a, ..tail] }) })
  }
}

/// Let-bind `rhs` once, then bind `n` fresh vars to TupleGet(0..n-1) of it
/// (0-based per R7). Returns the projection Values in index order.
pub fn bind_n(rhs: ir.Expr, n: Int) -> Build(List(ir.Value)) {
  then(bind(rhs), fn(tup) { proj_from(tup, 0, n) })
}

fn proj_from(tup: ir.Value, i: Int, n: Int) -> Build(List(ir.Value)) {
  case i < n {
    False -> pure([])
    True ->
      then(bind(tuple_get(tup, i)), fn(vi) {
        map(proj_from(tup, i + 1, n), fn(rest) { [vi, ..rest] })
      })
  }
}

// ── Numeric fast-path (HANDOFF §5 / SPEC §M11) ──────────────────────────────

/// i32 `is_number(v)` guard, or a constant `1` when `v` is statically a known
/// BEAM number — the elision seam for `guarded_binop`/`cmp`. Returns the guard
/// Value plus whether it was elided.
fn number_guard(v: ir.Value) -> Build(#(ir.Value, Bool)) {
  fn(e, k) {
    case is_known_number(e, v) {
      True -> k(e, #(ir.ConstI32(1), True))
      False ->
        bind(ir.TermTest(ir.IsNumber, v))(e, fn(e, g) { k(e, #(g, False)) })
    }
  }
}

/// i32 `is_number(a) & is_number(b)`, eliding either/both TermTests when the
/// operand is a known number. TermTest yields TI32 (ir.gleam:939). When both
/// guards are dynamic, combine via a nested `If` (`ga ? gb : 0`) rather than
/// `Num(IAnd(W32))` — the latter lowers to a cross-module `rt_num:i32_and`
/// call, the former to an inline Core `case`. Second tuple element is True
/// when BOTH were elided — caller drops the whole If.
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

/// `+ - *` on two BEAM number terms: the total `arc_rt_ops_ffi` kernel
/// (native op, then widen an integer past 2^53 - 1 to a double and keep the
/// sign of an integer zero product). The result is marked a known number.
pub fn num_binop(op: String, a: ir.Value, b: ir.Value) -> Build(ir.Value) {
  use ii <- then(both_ints(a, b))
  let slow = host(op, [a, b])
  let arm = case op {
    "num_add" -> Some(int_arm(ir.NAdd, a, b, False, slow))
    "num_sub" -> Some(int_arm(ir.NSub, a, b, False, slow))
    "num_mul" -> Some(int_arm(ir.NMul, a, b, True, slow))
    _ -> None
  }
  case arm {
    Some(fast) -> then(bind_if(ii, fast, slow), mark_number)
    None -> then(slow, mark_number)
  }
}

const max_safe_int = 9_007_199_254_740_991

/// i32 `is_integer(a) & is_integer(b)`; an integer constant needs no test.
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

/// Bare BEAM `+ - *` on two integers (never raises). The result stands when
/// it fits 2^53 - 1 either side — and, for `*`, is not the zero whose sign
/// only the kernel knows — else the kernel `slow` redoes the op.
fn int_arm(
  op: ir.NumTermOp,
  a: ir.Value,
  b: ir.Value,
  zero_sign: Bool,
  slow: Build(ir.Value),
) -> Build(ir.Value) {
  use r <- then(bind(ir.NumTerm(op, a, b)))
  use hi <- then(bind(ir.NumTerm(ir.NLe, r, ir.ConstI64(max_safe_int))))
  use fits <- then(bind_if_i32(
    hi,
    bind(ir.NumTerm(ir.NGe, r, ir.ConstI64(-max_safe_int))),
    pure(ir.ConstI32(0)),
  ))
  case zero_sign {
    False -> bind_if(fits, pure(r), slow)
    True -> {
      use nz <- then(bind_if_i32(
        fits,
        bind(ir.NumTerm(ir.NEq, r, ir.ConstI32(0))),
        pure(ir.ConstI32(1)),
      ))
      bind_if(nz, slow, pure(r))
    }
  }
}

/// JS arithmetic `+ - *`: `num_binop` fast path when both operands are BEAM
/// numbers, else the runtime slow path (handles ToPrimitive, string concat,
/// bigint, throw-on-symbol). When BOTH operands are statically known numbers
/// the guard/If/slow-arm are elided entirely — the M0 shape.
pub fn guarded_binop(
  fast_op: String,
  slow_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  use #(both, elided) <- then(both_numbers(a, b))
  case elided {
    True -> num_binop(fast_op, a, b)
    False -> bind_if(both, num_binop(fast_op, a, b), host(slow_op, [a, b]))
  }
}

/// JS relational `< <= > >= ==`: as `guarded_binop` but the fast arm's
/// NumTerm yields TI32 (gotcha #4), so it is re-branched to a JS bool atom.
pub fn guarded_cmp(
  fast: ir.NumTermOp,
  slow_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  use #(both, elided) <- then(both_numbers(a, b))
  let fast_arm = fn(e: Emitter2, k) {
    let rc = e.consts
    then(bind(ir.NumTerm(fast, a, b)), bind_if(
      _,
      pure(rc.true_),
      pure(rc.false_),
    ))(e, k)
  }
  case elided {
    True -> fast_arm
    False ->
      // The slow arm's host op (`t_lt`/`t_le`/…) returns the same i32 truth
      // value as the fast arm's NumTerm, so it needs the identical re-branch.
      bind_if(both, fast_arm, then(host(slow_op, [a, b]), i32_to_js_bool))
  }
}

/// Re-branch a wasm-style i32 truth value (`0`/`1`) into the JS boolean it
/// denotes. The IR's comparison ops and the `*_fast` probes yield TI32
/// (gotcha #4) and the `t_lt`/`t_eq`/`t_in`/`t_instance_of` host ops return
/// the same `0|1`, but the RESULT of a JS `<`/`==`/`in`/`instanceof` is a
/// **Boolean** (§13.10.1, §13.10.2, §7.2.14). The difference is observable —
/// `typeof (a instanceof B)` and `"" + (a in o)` — so an i32 must never
/// escape as a JS value. Use at the seam where the truth value becomes the
/// expression's result; NOT on a value headed straight for an `ir.If` cond
/// (see `cond_cmp`, which deliberately keeps the raw i32).
pub fn i32_to_js_bool(v: ir.Value) -> Build(ir.Value) {
  fn(e: Emitter2, k) {
    let rc = e.consts
    bind_if(v, pure(rc.true_), pure(rc.false_))(e, k)
  }
}

/// JS relational `< <= > >=` as a RAW i32 truth value for use directly as an
/// `ir.If` cond (loop conditions) — skips the bool-atom wrap + `truthy` unwrap
/// that `guarded_cmp` incurs. Slow arm coerces the JS bool result to i32.
pub fn cond_cmp(
  fast: ir.NumTermOp,
  slow_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  use #(both, elided) <- then(both_numbers(a, b))
  case elided {
    True -> bind(ir.NumTerm(fast, a, b))
    False ->
      bind_if_i32(
        both,
        bind(ir.NumTerm(fast, a, b)),
        then(host(slow_op, [a, b]), fn(v) { host("truthy", [v]) }),
      )
  }
}

/// ToNumeric fast path: `is_number(v) ? v : host("to_numeric", v)`. Skips the
/// JMut host call + St-pair unpack when `v` is already a BEAM number. When `v`
/// is a statically known number the guard itself is elided (returns `v`).
pub fn guarded_unary_numeric(v: ir.Value) -> Build(ir.Value) {
  fn(e, k) {
    case is_known_number(e, v) {
      True -> k(e, v)
      False ->
        {
          use is_n <- then(bind(ir.TermTest(ir.IsNumber, v)))
          bind_if(is_n, pure(v), host("to_numeric", [v]))
        }(e, k)
    }
  }
}

// ── Static property keys (invariant #4 / SPEC §2.3) ─────────────────────────

/// Emit the WIRE `ObjectKey` tuple (`{string_key, {index|named|private, ..}}`)
/// for a compile-time-known property key. THE one static-key canonicalizer:
/// callers `then` the result with no runtime `to_property_key` call, so the
/// output MUST be canonical (`{"5":v}`/`{5:v}`/`{5n:v}` all → `{index,5}`).
/// `KeyComputed` is a caller contract violation — M12 routes it through
/// `host("to_property_key")`.
pub fn object_key_lit(pk: ast.PropertyKey) -> Build(ir.Value) {
  let inner = case pk {
    // IdentifierName never starts with a digit → never an array-index string.
    ast.KeyIdentifier(name:, ..) -> wire_named(name)
    ast.KeyString(value: s, ..) -> wire_prop_key(key.canonical_key(s))
    ast.KeyNumber(value: ast.FiniteNumber(f), ..) ->
      case key.array_index_of_float(f) {
        Some(i) -> wire_index(i)
        None -> wire_named(val.js_format_float(f))
      }
    ast.KeyNumber(value: ast.InfiniteNumber, ..) -> wire_named("Infinity")
    ast.KeyBigInt(value: n, ..) -> wire_prop_key(key.index_key(n))
    // `name` already carries the leading '#' (ast.gleam:558). D9: the runtime
    // uid is minted at class-eval time — M12 resolves KeyPrivate via the
    // class-scope local instead, so this arm keeps the match total.
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
    // canonical_key/index_key never yield Private (key.gleam:55).
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
