//// M11: Build(a) CPS monad (R13-pinned) + ANF let-binding combinators over
//// twocore/ir — bind/host/cons_list/bind_if/guarded_binop/object_key_lit.
//// Invariant #3: `host` is the ONLY CallHost("js", ..) site in emit_2core/*.

import arc/compiler/emit_2core/state.{type Emitter2, Emitter2}
import arc/parser/ast
import arc/vm/key
import arc/vm/value
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import twocore/ir

/// R13-pinned. Tail continuation receives the final Emitter2 + result and
/// returns the terminal ir.Expr, which the builder wraps in Let-bindings.
pub type Build(a) =
  fn(Emitter2, fn(Emitter2, a) -> ir.Expr) -> ir.Expr

pub fn pure(v: a) -> Build(a) {
  fn(e, k) { k(e, v) }
}

pub fn then(b: Build(a), f: fn(a) -> Build(c)) -> Build(c) {
  fn(e, k) { b(e, fn(e, a) { f(a)(e, k) }) }
}

pub fn bind(rhs: ir.Expr) -> Build(ir.Value) {
  fn(e, k) {
    let #(name, e) = state.fresh_var(e)
    ir.Let([name], rhs, k(e, ir.Var(name)))
  }
}

/// `bind` that also records the fresh var as a known BEAM number (int|float
/// term). Use for `Convert(BoxInt,·)`, `NumTerm(NAdd/NSub/NMul,·)` and other
/// rhs whose result is provably a number term — lets `guarded_binop`/`cmp`
/// elide the `is_number` TermTest on that operand.
pub fn bind_number(rhs: ir.Expr) -> Build(ir.Value) {
  fn(e, k) {
    let #(name, e) = state.fresh_var(e)
    ir.Let([name], rhs, k(state.mark_known_number(e, name), ir.Var(name)))
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

// ── The ONE impure seam (RULINGS R13 note) ──────────────────────────────────
// erlang:make_ref/0 → guaranteed-unique per call, so nested run_arm calls
// (bind_if inside a bind_if arm) each get an independent process-dict slot —
// re-entrant by construction. Precedent: src/arc/vm/builtins/symbol.gleam:161.
type Ref

type Erased

@external(erlang, "erlang", "make_ref")
fn make_ref() -> Ref

@external(erlang, "erlang", "put")
fn pdict_put(k: Ref, v: a) -> Erased

@external(erlang, "erlang", "erase")
fn pdict_erase(k: Ref) -> a

/// Run a Build to a Values-terminal ir.Expr and recover the final Emitter2
/// the tail continuation received. Re-entrant (fresh make_ref per call).
/// Seeded with the incoming `e` so a Build that diverges before reaching `k`
/// still returns a valid Emitter2 (no M11 combinator does this today).
fn run_arm(b: Build(ir.Value), e: Emitter2) -> #(ir.Expr, Emitter2) {
  let cell = make_ref()
  let _ = pdict_put(cell, e)
  let tree =
    b(e, fn(ef, v) {
      let _ = pdict_put(cell, ef)
      ir.Values([v])
    })
  #(tree, pdict_erase(cell))
}

pub fn run(b: Build(ir.Value), e: Emitter2) -> #(ir.Expr, Emitter2) {
  run_arm(b, e)
}

/// Run a Build to a caller-supplied TERMINAL ir.Expr (Return/Continue/If…)
/// instead of the default `Values([v])`. Same re-entrant pdict seam as `run`.
/// M18 arm bodies end in Step-tuple `Return`s / `Continue(Lresume,…)`.
pub fn run_to(
  b: Build(a),
  e: Emitter2,
  tail: fn(Emitter2, a) -> ir.Expr,
) -> #(ir.Expr, Emitter2) {
  let cell = make_ref()
  let _ = pdict_put(cell, e)
  let tree =
    b(e, fn(ef, a) {
      let _ = pdict_put(cell, ef)
      tail(ef, a)
    })
  #(tree, pdict_erase(cell))
}

// ── slot-rebind threading (SPEC§9.12 / profiles.gleam js_direct opt_level) ──
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

/// Descend a run_arm tree's Let-spine and widen its terminal `Values([v])` to
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
    let #(then_tree, e_t) = run_arm(t, e)
    let #(else_tree, e_f) = run_arm(f, Emitter2(..e_t, slot_vars: sv0))
    let carried =
      merge_slots(
        slots_rebound(sv0, e_t.slot_vars),
        slots_rebound(sv0, e_f.slot_vars),
      )
    let e = Emitter2(..e_f, slot_vars: sv0)
    let #(r, e) = state.fresh_var(e)
    case carried {
      [] ->
        ir.Let(
          [r],
          ir.If(cond, [head_ty], then_tree, else_tree),
          k(e, ir.Var(r)),
        )
      _ -> {
        let then_tree = append_tail(then_tree, arm_slot_vals(e_t, carried))
        let else_tree = append_tail(else_tree, arm_slot_vals(e_f, carried))
        let #(e, out) = rebind_slots(e, carried)
        let tys = [head_ty, ..list.map(carried, fn(_) { ir.TTerm })]
        ir.Let(
          [r, ..out],
          ir.If(cond, tys, then_tree, else_tree),
          k(e, ir.Var(r)),
        )
      }
    }
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

/// `if (v is null|undefined) then t else f`.
pub fn nullish_if(
  v: ir.Value,
  t: Build(ir.Value),
  f: Build(ir.Value),
) -> Build(ir.Value) {
  then(host("is_nullish", [v]), bind_if(_, t, f))
}

/// perf7 richards lever (w2): when `body` references `miss` ≤2 times AND no
/// slot is carried, inline an α-renamed copy of `cold` at each miss site
/// instead of the l_miss Block — the l_miss letrec disappears entirely.
pub const perf7_share_dup: Bool = True

/// Emit `cold`'s IR ONCE, shared across every `miss` reference in `body`.
/// Each `miss` use lowers to a single `ir.Break` (→ join-point `apply`), not
/// a full copy of `cold`'s tree — get_prop_fast/set_prop_fast/emit_member_call
/// pass `cold` to 5-8 bind_if else-arms; without this each re-emits the whole
/// cold tier. Mechanism: nested Blocks — `body`'s hit leaves Break to l_join;
/// each `miss` Breaks to l_miss whose continuation runs `cold` exactly once.
/// Slot rebinds in either arm thread out through l_join (perf5 `_this_c`
/// hoist — `body`'s hit updates it to `nc`; `cold` re-reads pdict).
///
/// HIT-PATH COST: `to_break` sinks every `Let([r], If, Values([r]))` (bind_if's
/// wrapper) into `If(·, …→Break l_join, …)` so emit_core's `materialize` sees
/// `KJump` at each nested If — a `Let([r], If, Break(l_join,[r]))` tail is NOT
/// identity-stripped and would letrec a `fun(r)→apply` trampoline per level
/// (measured: obj_prop 20.6k→24k µs with the naive `run_to` Break-k).
pub fn share(
  cold: Build(ir.Value),
  body: fn(Build(ir.Value)) -> Build(ir.Value),
) -> Build(ir.Value) {
  fn(e: Emitter2, k) {
    let sv0 = e.slot_vars
    let #(l_join, e) = state.fresh_label(e)
    let #(l_miss, e) = state.fresh_label(e)
    // Diverging Build — same shape as chain_guard's Break arm (expr.gleam).
    let miss = fn(_e, _k) { ir.Break(l_miss, [ir.ConstI32(0)]) }
    let #(body_tree, e_b) = run_arm(body(miss), e)
    // cold runs from body's fresh-var state but the ENTRY slot snapshot —
    // body's rebinds are on the hit path only; miss falls to cold with sv0.
    let #(cold_tree, e_c) = run_arm(cold, Emitter2(..e_b, slot_vars: sv0))
    let carried =
      merge_slots(
        slots_rebound(sv0, e_b.slot_vars),
        slots_rebound(sv0, e_c.slot_vars),
      )
    let e = Emitter2(..e_c, slot_vars: sv0)
    case carried {
      [] -> {
        let n_miss = count_breaks_to(body_tree, l_miss)
        case perf7_share_dup && n_miss <= 2 {
          True -> {
            // ≤2 miss refs (incl. 0 → cold dropped): splice an α-renamed
            // cold_tree at each Break(l_miss) — no l_miss Block, hit path
            // stays straight-line, l_miss letrec never materializes.
            // Pre-convert cold to Break(l_join) so a splice at a non-tail
            // If arm (which to_break won't descend into) is already terminal.
            let #(inlined, e) =
              subst_break_with(body_tree, l_miss, to_break(cold_tree, l_join), e)
            let #(r, e) = state.fresh_var(e)
            ir.Let(
              [r],
              ir.Block(l_join, [ir.TTerm], to_break(inlined, l_join)),
              k(e, ir.Var(r)),
            )
          }
          False -> {
            let #(d, e) = state.fresh_var(e)
            let #(r, e) = state.fresh_var(e)
            let body_tree = to_break(body_tree, l_join)
            ir.Let(
              [r],
              ir.Block(
                l_join,
                [ir.TTerm],
                ir.Let([d], ir.Block(l_miss, [ir.TTerm], body_tree), cold_tree),
              ),
              k(e, ir.Var(r)),
            )
          }
        }
      }
      _ -> {
        let #(d, e) = state.fresh_var(e)
        let #(r, e) = state.fresh_var(e)
        // Widen l_join to `[v, ..carried]`. Hit-path Breaks carry body's
        // (or entry-snapshot) slot names; cold's fallthrough carries cold's.
        let body_extra = arm_slot_vals(e_b, carried)
        let cold_extra = arm_slot_vals(e_c, carried)
        let body_tree = to_break_wide(body_tree, l_join, body_extra)
        let cold_tree = append_tail(cold_tree, cold_extra)
        let #(e, out) = rebind_slots(e, carried)
        let tys = [ir.TTerm, ..list.map(carried, fn(_) { ir.TTerm })]
        ir.Let(
          [r, ..out],
          ir.Block(
            l_join,
            tys,
            ir.Let([d], ir.Block(l_miss, [ir.TTerm], body_tree), cold_tree),
          ),
          k(e, ir.Var(r)),
        )
      }
    }
  }
}

/// perf7 x-cold-outlined: emit `body_b` as a SEPARATE `ir.Function` (added
/// via `state.add_function`) and yield its name for `ir.CallDirect`. Real IC
/// shape — hit inline, miss = 1 same-module call — so the caller's fast path
/// is pure nested `bind_if` with NO Block wrapper (every level let-cases).
/// Body runs with cleared `slot_vars`/`this_c_cache` (aux fn is a fresh
/// scope; caller slot names / `_this_c` are out-of-scope there).
/// emit_core's state-reaching closure sees the body's JMut hosts and threads
/// St through both the aux fn AND every `CallDirect` to it — mirrors the
/// jsf_* body shape (func.gleam:1621), so no explicit `{V,St}` pairing here.
pub fn aux_fn(
  prefix: String,
  params: List(#(String, ir.ValType)),
  body_b: fn(List(ir.Value)) -> Build(ir.Value),
) -> Build(String) {
  fn(e: Emitter2, k) {
    let name = prefix <> int.to_string(e.next_fn)
    let e = Emitter2(..e, next_fn: e.next_fn + 1)
    let sv0 = e.slot_vars
    let tc0 = e.this_c_cache
    let arg_vals = list.map(params, fn(p) { ir.Var(p.0) })
    let #(body_tree, e_b) =
      run_to(
        body_b(arg_vals),
        Emitter2(..e, slot_vars: dict.new(), this_c_cache: None),
        fn(_, v) { ir.Return([v]) },
      )
    let e =
      state.add_function(
        Emitter2(..e_b, slot_vars: sv0, this_c_cache: tc0),
        ir.Function(
          name: name,
          params: list.map(params, fn(p) { ir.Local(p.0, p.1) }),
          result: [ir.TTerm],
          locals: [],
          body: body_tree,
        ),
      )
    k(e, name)
  }
}

/// `to_break` widened for slot-carry: hit-path leaves become
/// `Break(label, [v, ..extra])`. Only sinks single-binder identity-tail Ifs
/// (a bind_if that itself carried slots stays wrapped; one trampoline).
fn to_break_wide(
  tree: ir.Expr,
  label: String,
  extra: List(ir.Value),
) -> ir.Expr {
  let go = to_break_wide(_, label, extra)
  case tree {
    ir.Let([r], ir.If(c, ty, t, f), tail) ->
      case is_id_tail(r, tail, label) {
        True -> ir.If(c, ty, go(t), go(f))
        False -> ir.Let([r], ir.If(c, ty, t, f), go(tail))
      }
    ir.Let(ns, rhs, body) -> ir.Let(ns, rhs, go(body))
    ir.If(c, ty, t, f) -> ir.If(c, ty, go(t), go(f))
    ir.Values([v]) -> ir.Break(label, [v, ..extra])
    _ -> tree
  }
}

/// Sink `body`'s hit result into `Break(label, ·)` at every leaf, dropping the
/// `Let([r], If, Values/Break([r]))` identity wrappers bind_if emits so the
/// nested Ifs' cont stays `KJump` (see `share` doc). Only If is sunk — Block
/// rhs is left wrapped (a nested `share` inside body pays one trampoline; the
/// alternative mis-routes the inner Block's own Breaks).
fn to_break(tree: ir.Expr, label: String) -> ir.Expr {
  case tree {
    ir.Let([r], ir.If(c, ty, t, f), tail) ->
      case is_id_tail(r, tail, label) {
        True -> ir.If(c, ty, to_break(t, label), to_break(f, label))
        False -> ir.Let([r], ir.If(c, ty, t, f), to_break(tail, label))
      }
    ir.Let(ns, rhs, body) -> ir.Let(ns, rhs, to_break(body, label))
    ir.If(c, ty, t, f) -> ir.If(c, ty, to_break(t, label), to_break(f, label))
    ir.Values([v]) -> ir.Break(label, [v])
    _ -> tree
  }
}

fn is_id_tail(r: String, tail: ir.Expr, label: String) -> Bool {
  case tail {
    ir.Values([ir.Var(n)]) -> n == r
    ir.Break(l, [ir.Var(n)]) -> l == label && n == r
    _ -> False
  }
}

/// Count `Break(label, _)` occurrences in `tree`. Descends the same shapes a
/// `run_arm`-built body nests a Break under (Let rhs+body, If arms, Block body
/// — Loop/Try/Switch don't occur there; see `widen_breaks`).
pub fn count_breaks_to(tree: ir.Expr, label: String) -> Int {
  let go = count_breaks_to(_, label)
  case tree {
    ir.Let(_, rhs, body) -> go(rhs) + go(body)
    ir.If(_, _, t, f) -> go(t) + go(f)
    ir.Block(_, _, body) -> go(body)
    ir.Break(l, _) if l == label -> 1
    _ -> 0
  }
}

/// Replace every `Break(label, _)` in `tree` with a fresh α-renamed copy of
/// `replacement`. Each copy gets fresh Let-binder names (via `state.fresh_var`)
/// so ≥2 inlined copies never clash. Labels are lexically stacked in emit_core
/// so Block labels need no renaming.
pub fn subst_break_with(
  tree: ir.Expr,
  label: String,
  replacement: ir.Expr,
  e: Emitter2,
) -> #(ir.Expr, Emitter2) {
  case tree {
    ir.Let(ns, rhs, body) -> {
      let #(rhs, e) = subst_break_with(rhs, label, replacement, e)
      let #(body, e) = subst_break_with(body, label, replacement, e)
      #(ir.Let(ns, rhs, body), e)
    }
    ir.If(c, tys, t, f) -> {
      let #(t, e) = subst_break_with(t, label, replacement, e)
      let #(f, e) = subst_break_with(f, label, replacement, e)
      #(ir.If(c, tys, t, f), e)
    }
    ir.Block(l, tys, body) -> {
      let #(body, e) = subst_break_with(body, label, replacement, e)
      #(ir.Block(l, tys, body), e)
    }
    ir.Break(l, _) if l == label -> rename_expr(replacement, dict.new(), e)
    _ -> #(tree, e)
  }
}

/// α-rename: mint a fresh name for every Let binder, substituting all Var
/// references in scope. Covers the Expr shapes anf.gleam's combinators emit.
fn rename_expr(
  tree: ir.Expr,
  sub: Dict(String, String),
  e: Emitter2,
) -> #(ir.Expr, Emitter2) {
  let rv = rename_val(_, sub)
  case tree {
    ir.Let(ns, rhs, body) -> {
      // rhs is bound OUTSIDE `ns` — recurse under the incoming sub.
      let #(rhs, e) = rename_expr(rhs, sub, e)
      let #(rev, sub, e) =
        list.fold(ns, #([], sub, e), fn(acc, n) {
          let #(rev, sub, e) = acc
          let #(n2, e) = state.fresh_var(e)
          #([n2, ..rev], dict.insert(sub, n, n2), e)
        })
      let #(body, e) = rename_expr(body, sub, e)
      #(ir.Let(list.reverse(rev), rhs, body), e)
    }
    ir.If(c, tys, t, f) -> {
      let #(t, e) = rename_expr(t, sub, e)
      let #(f, e) = rename_expr(f, sub, e)
      #(ir.If(rv(c), tys, t, f), e)
    }
    ir.Block(l, tys, body) -> {
      let #(body, e) = rename_expr(body, sub, e)
      #(ir.Block(l, tys, body), e)
    }
    ir.Values(vs) -> #(ir.Values(list.map(vs, rv)), e)
    ir.Break(l, vs) -> #(ir.Break(l, list.map(vs, rv)), e)
    ir.Continue(l, vs) -> #(ir.Continue(l, list.map(vs, rv)), e)
    ir.Return(vs) -> #(ir.Return(list.map(vs, rv)), e)
    ir.CallHost(cap, name, args) -> #(
      ir.CallHost(cap, name, list.map(args, rv)),
      e,
    )
    ir.CallDirect(name, args) -> #(ir.CallDirect(name, list.map(args, rv)), e)
    ir.CallClosure(callee, args) -> #(
      ir.CallClosure(rv(callee), list.map(args, rv)),
      e,
    )
    ir.CallIndirect(t, idx, ty, args) -> #(
      ir.CallIndirect(t, rv(idx), ty, list.map(args, rv)),
      e,
    )
    ir.GlobalSet(name, v) -> #(ir.GlobalSet(name, rv(v)), e)
    ir.RefIsNull(arg) -> #(ir.RefIsNull(rv(arg)), e)
    ir.TermOp(op, args) -> #(ir.TermOp(op, list.map(args, rv)), e)
    ir.Num(op, args) -> #(ir.Num(op, list.map(args, rv)), e)
    ir.NumTerm(op, a, b) -> #(ir.NumTerm(op, rv(a), rv(b)), e)
    ir.TermTest(kind, arg) -> #(ir.TermTest(kind, rv(arg)), e)
    ir.Convert(op, arg) -> #(ir.Convert(op, rv(arg)), e)
    ir.MapOp(op, args) -> #(ir.MapOp(op, list.map(args, rv)), e)
    _ -> #(tree, e)
  }
}

fn rename_val(v: ir.Value, sub: Dict(String, String)) -> ir.Value {
  case v {
    ir.Var(n) ->
      case dict.get(sub, n) {
        Ok(n2) -> ir.Var(n2)
        Error(_) -> v
      }
    _ -> v
  }
}

pub fn bind_block(body: fn(String) -> Build(ir.Value)) -> Build(ir.Value) {
  fn(e: Emitter2, k) {
    let sv0 = e.slot_vars
    let #(label, e) = state.fresh_label(e)
    let #(body_tree, e_b) = run_arm(body(label), e)
    let carried = slots_rebound(sv0, e_b.slot_vars)
    let e = Emitter2(..e_b, slot_vars: sv0)
    let #(r, e) = state.fresh_var(e)
    case carried {
      [] -> ir.Let([r], ir.Block(label, [ir.TTerm], body_tree), k(e, ir.Var(r)))
      _ -> {
        // Fall-through gets the arm's rebound names; every Break to `label`
        // (chain_guard's short-circuit) gets the entry-snapshot names — the
        // write_slot lies past the guard, so sv0's name is the live value.
        let body_tree =
          append_tail(body_tree, arm_slot_vals(e_b, carried))
          |> widen_breaks(label, arm_slot_vals(e, carried))
        let #(e, out) = rebind_slots(e, carried)
        let tys = [ir.TTerm, ..list.map(carried, fn(_) { ir.TTerm })]
        ir.Let([r, ..out], ir.Block(label, tys, body_tree), k(e, ir.Var(r)))
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

/// JS arithmetic `+ - *`: NumTerm fast path when both operands are BEAM
/// numbers, else the `rt_js` slow path (handles ToPrimitive, string concat,
/// bigint, throw-on-symbol). NAdd/NSub/NMul yield TTerm directly. When BOTH
/// operands are statically known numbers the guard/If/slow-arm are elided
/// entirely and the NumTerm result is itself marked known — the M0 shape.
pub fn guarded_binop(
  fast: ir.NumTermOp,
  slow_op: String,
  a: ir.Value,
  b: ir.Value,
) -> Build(ir.Value) {
  use #(both, elided) <- then(both_numbers(a, b))
  case elided {
    True -> bind_number(ir.NumTerm(fast, a, b))
    False ->
      bind_if(both, bind_number(ir.NumTerm(fast, a, b)), host(slow_op, [a, b]))
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
        None -> wire_named(value.js_format_number(f))
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
