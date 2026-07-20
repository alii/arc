//// M12: JS Expression → twocore IR lowering. Faithful port of
//// arc/compiler/emit.gleam:4270-5060 (+ helpers 1790-2130, 2560-2680,
//// 4100-4250, 5330-5450, 6950-7020) into the anf.Build(ir.Value) CPS monad.
//// D2: state-invisible — NO St threading (M9 emit_core owns it).
//// D4: JS calls go through host("call", ..) — fn values are {js_cell,N}.
//// D15: with/direct-eval/Proxy → runtime UnsupportedFeature throw.
////
//// Error seam (R12 × R13 reconciliation): Build(a) is R13-pinned with no
//// Result slot, Emitter2 (frozen) has no errors field, and R12 forbids panic.
//// Resolution: every user-reachable early error AND parser-unreachable case
//// emits a diverging host("throw_*_error", [msg]) at RUNTIME (matching
//// emit.gleam's opcode.ThrowError), then continues with `undef` so k is still
//// called and the CPS chain stays total. `emit_expr` therefore always returns
//// Ok — the Result wrapper exists to satisfy state.EmitDispatch.emit_expr.

import arc/compiler/ast_util
import arc/compiler/emit_2core/anf.{type Build}
import arc/compiler/emit_2core/state.{type EmitError, type Emitter2}
import arc/compiler/scope
import arc/parser/ast
import arc/vm/key
import arc/vm/lexical
import gleam/bit_array
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import twocore/ir

// ── perf5 feature gates (bisect-raytrace-perf5) ─────────────────────────────
// Flip a gate to False → that feature falls straight through to its perf4
// (3b198d8) path. Independent toggles for raytrace 287k→683k bisection via
// `gleam run -m emit_2core_profile`. func.gleam mirrors perf5_code_t and
// owns perf5_this_c_hoist — flip both files for those two.

/// (a) inline method-IC warm ladder → bare `call_method_ic` FFI.
pub const perf5_inline_method_ic: Bool = True

/// (S) task-S `method_ic_warm` FFI probe → perf5's original inline ladder.
/// Separate from (a) so (a) bisects perf4↔perf5 exactly; this bisects
/// perf5-ladder↔task-S. Only read when (a) is True.
pub const perf5s_method_ic_ffi: Bool = True

/// (b) `{k:v,…}` → SShapedObject direct alloc → always host `new_object`.
pub const perf5_shape_obj_literals: Bool = True

/// (d) `_t` simple-ABI SimpleT dispatch → always frame_path. Pair with
/// func.gleam's perf5_code_t (skips the `_t` body emit).
pub const perf5_code_t: Bool = True

/// (e) known-handle dataflow → `is_known_handle_val` always False.
pub const perf5_known_handle: Bool = True

/// (f) JPure `to_property_key_fast` probe → always full JMut host.
pub const perf5_to_property_key_split: Bool = True

/// perf6 richards-floor lever (b): mutable-slot `_this_c`. True → the pdict
/// entry `pdict[_this_id]` IS the slot — read fresh (1 `get` BIF ≈3ns) per
/// `this.x`; set/refresh are no-ops (set_prop_fast's own `pdict_put(id,nc)`
/// and cold-tier jsv_install/evict already keep it current). NO slot_var(-1)
/// rebind → anf.bind_if/share `carried=[]` → to_break sinks every If wrapper
/// → the ~445k letrec-apply/run (≈1,780µs) slot-threading forced is gone.
/// False → perf5's slot-var threading (0-op read; each rebind widens joins).
/// With perf6_shaped_this_ffi=True the `_this` hot path bypasses read/set
/// entirely, so True here just drops the now-dead refresh rebinds.
/// func.gleam mirrors this const — flip both.
pub const perf6_mut_this_c: Bool = True

/// perf7 crypto lever (P-c, y3): array-element read+write via `{tc_arr,Id}`
/// pdict overlay. True → `get_elem_fast_p`/`set_elem_fast_p` (read installs
/// the overlay on cold; write is JPure — no St thread, no `{V,St'}` tuple;
/// ~868k×~150ns saved on crypto's am3). Slow-path (t_cell_set→jsv_evict)
/// writes back + erases. False → perf6 Store cascade / JMutMiss rebuild
/// end-to-end (no pdict probe on either side).
pub const perf7_arr_pdict: Bool = False

/// perf7 richards-floor lever (x): outline the cold tier as a real
/// `ir.Function` + `CallDirect` instead of `anf.share`'s nested-Block join.
/// True → get_prop_fast/set_prop_fast emit a `jsf_cold_*` aux fn ONCE per
/// site; every miss arm is a bare `CallDirect` (1 same-module apply). Hit
/// path is pure nested `bind_if` with NO Block wrapper → every level
/// let-cases (no l_miss letrec). False → perf6's `anf.share` Block/Break.
pub const perf7_cold_outline: Bool = True

/// perf8 crypto lever: hoist `get({tc_arr,Id})` before a for/while whose body
/// has ≥`arr_c_hoist_min` bracket reads on the SAME loop-invariant identifier
/// base with NO bracket writes on that base — `get_elem_fast_c(arr_c,obj,idx)`
/// then reads via the hoisted overlay (0 pdict-get per read; falls to
/// `get_elem_fast_p` on `undefined`/miss so cold install still fires once).
/// crypto am3 `this_array[i]` ≈2.4M reads × ~3ns pdict-get saved. Write-
/// containing bases (am3 `w_array`) are skipped so writeback stays live.
/// stmt.gleam mirrors this const — flip both.
pub const perf8_arr_c_hoist: Bool = True

/// Min bracket-read count on a base before it's worth the pre-loop bind.
pub const arr_c_hoist_min: Int = 2

/// perf8 raytrace lever (own_property_of): proto-chain data-get tier between
/// `t_ic_get` (own miss) and `t_get_prop_any`. Warm hit = one pdict compare
/// on receiver's proto-id; cold walk installs {PId,V} at a per-site `@pgp`
/// key + PathIds in TC_MC_DEPS. Targets rt_js_obj:get_from 416k×254ns +
/// own_property_of 462k×201ns (raytrace `.prototype` on KFunction — rejected
/// by peek_get's `ordinary` gate — and proto-default-field reads). False →
/// perf7's 2-tier ic_get→get_prop_any.
pub const perf8_ic_proto_get: Bool = True

/// perf8 deltablue-drift gate (y2): `l >> C`/`l << C` int-literal RHS →
/// inline erlang:bsr/bsl, `>>>` → int_fast("ushr_fast",…). False → perf6's
/// bare int_fast/anf.host("ushr",…). Ungated in a7ebc74..f289f0c; deltablue
/// has 0 shift ops (v8v7_probe:253 "no db effect") — gated for bisect only.
pub const perf8_int_const_shift: Bool = True

// ── Emitter2-access combinators (state monad over Build) ────────────────────

/// Read the threaded Emitter2. `ask` IS a Build(Emitter2): `use e <- then(ask)`.
pub fn ask(e: Emitter2, k: fn(Emitter2, Emitter2) -> ir.Expr) -> ir.Expr {
  k(e, e)
}

/// Thread a pure Emitter2→Emitter2 update through the Build chain.
pub fn modify(f: fn(Emitter2) -> Emitter2) -> Build(Nil) {
  fn(e, k) { k(f(e), Nil) }
}

/// Read `e.consts` (RealmConsts sentinels) inside a Build.
pub fn consts() -> Build(state.RealmConsts) {
  fn(e: Emitter2, k) { k(e, e.consts) }
}

// ── Error seam (R12: never panic; runtime-diverging host throw instead) ─────

/// Emit a diverging `rt_js` throw (SPEC §8 throw_* ops), then yield `undef`
/// so the CPS tail is still called. Mirrors emit.gleam's opcode.ThrowError.
pub fn throw_at_rt(op: String, msg: String) -> Build(ir.Value) {
  use _ <- anf.then(anf.host(op, [ir.ConstBinary(bit_array.from_string(msg))]))
  use rc <- anf.then(consts())
  anf.pure(rc.undef)
}

/// Parser-unreachable ast shapes (bare Super/Spread, invalid update target,
/// etc.) — R12 forbids `panic`, so surface as a runtime TypeError instead.
pub fn unreachable(why: String) -> Build(ir.Value) {
  throw_at_rt("throw_type_error", "emit_2core/expr: unreachable: " <> why)
}

fn describe_error(err: EmitError) -> String {
  case err {
    state.BreakOutsideLoop -> "break outside loop"
    state.ContinueOutsideLoop -> "continue outside loop"
    state.EarlySyntaxError(message:) -> message
    state.UnsupportedFeature(feature:) -> "unsupported: " <> feature
    state.ScopeCursorDesync(..) -> "scope cursor desync"
  }
}

/// Bridge a Result-returning dispatch call (`emit_function`/`emit_async_body`,
/// which yield `#(Emitter2, ir.Value)`) into Build. On Error, emit a runtime
/// throw + continue with `undef` (least-bad given frozen Build/Emitter2).
pub fn bridge_value(
  call: fn(Emitter2) -> Result(#(Emitter2, ir.Value), EmitError),
) -> Build(ir.Value) {
  fn(e, k) {
    case call(e) {
      Ok(#(e, v)) -> k(e, v)
      Error(err) -> throw_at_rt("throw_type_error", describe_error(err))(e, k)
    }
  }
}

/// Bridge a Result-returning dispatch call yielding `#(ir.Expr, Emitter2)`
/// (`emit_class`/`emit_destructure`/`emit_stmts`) into Build by let-binding
/// the produced expression tree.
pub fn bridge_expr(
  call: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Build(ir.Value) {
  fn(e, k) {
    case call(e) {
      Ok(#(tree, e)) -> {
        let #(name, e) = state.fresh_var(e)
        ir.Let([name], tree, k(e, ir.Var(name)))
      }
      Error(err) -> throw_at_rt("throw_type_error", describe_error(err))(e, k)
    }
  }
}

// ── Entry points ────────────────────────────────────────────────────────────

/// Lower one ast.Expression to a Build yielding its ir.Value. `named` carries
/// the NamedEvaluation hint (§8.1.15) for anonymous fn/class RHS.
fn emit(ex: ast.Expression, named: Option(String)) -> Build(ir.Value) {
  case ex {
    // ── Literals (u-literals, SPEC §7.M12) ──────────────────────────────────
    ast.NumberLiteral(_, value) -> number_literal(value)
    ast.BigIntLiteral(_, n) -> {
      use boxed <- anf.then(
        anf.bind(ir.Convert(ir.BoxInt(ir.W64), ir.ConstI64(n))),
      )
      anf.make_tuple([ir.ConstAtom("js_bigint"), boxed])
    }
    ast.StringExpression(_, s) ->
      anf.pure(ir.ConstBinary(bit_array.from_string(s)))
    ast.BooleanLiteral(_, b) -> {
      use rc <- anf.then(consts())
      anf.pure(case b {
        True -> rc.true_
        False -> rc.false_
      })
    }
    ast.NullLiteral(_) -> anf.then(consts(), fn(rc) { anf.pure(rc.null) })
    ast.UndefinedExpression(_) ->
      anf.then(consts(), fn(rc) { anf.pure(rc.undef) })
    ast.RegExpLiteral(_, pattern, flags) ->
      anf.host("regexp_new", [
        ir.ConstBinary(bit_array.from_string(pattern)),
        ir.ConstBinary(bit_array.from_string(flags)),
      ])
    // NamedEvaluation looks THROUGH parens (§13.2.1.2 / emit.gleam:5217).
    ast.ParenthesizedExpression(_, inner) -> emit(inner, named)

    // ── Binary expressions (u-binop) ────────────────────────────────────────
    // §13.10.1 RelationalExpression : PrivateIdentifier `in` ShiftExpression.
    // LHS is a NAME, not a value — do NOT recurse (bare `#x` is an early
    // error). Eval RHS, load the class-scope minted private-key local, then
    // host("private_in") — JRead per R9. Port of emit.gleam:4294-4304.
    ast.BinaryExpression(
      operator: ast.In,
      left: ast.Identifier(name: "#" <> rest, ..),
      right:,
      ..,
    ) -> {
      use r <- anf.then(expr(right))
      use k <- anf.then(emit_identifier("#" <> rest))
      anf.host("private_in", [r, k])
    }
    // Generic binary: eval left THEN right (§13 order), dispatch on op.
    // Small-int literal operands surface as bare ConstI32 (not the
    // Let-bound Convert(BoxInt) that number_literal emits) so binop's
    // fast-path detectors (loose_eq int_const_eq, int_fast) fire on
    // `x == 0` / `x & 1` as well as const-folded globals.
    ast.BinaryExpression(operator: op, left:, right:, ..) -> {
      use l <- anf.then(expr_operand(left))
      use r <- anf.then(expr_operand(right))
      binop(op, l, r)
    }

    // ── Templates (u-template, port emit.gleam:4916,5016-5047,5059-5078) ────
    ast.TemplateLiteral(parts:, ..) -> emit_template_literal(parts)

    // Tagged template (§13.3.11): rewrite to a synthetic CallExpression with
    // the per-site cached template object as arg 0 and recurse, so this-bind
    // for `obj.tag`x`` and arg lowering ride the CallExpression arm.
    // §13.3.11.1 note: never a direct eval — wrap a bare `eval` tag.
    ast.TaggedTemplateExpression(tag:, parts:, span:) -> {
      let site = unique_positive_integer()
      let template =
        ast.IntrinsicTemplateObject(
          span:,
          site:,
          quasis: ast.template_quasis(parts),
        )
      let tag = case tag {
        ast.Identifier(span: tag_span, name: "eval") ->
          ast.ParenthesizedExpression(span: tag_span, expression: tag)
        _ -> tag
      }
      emit(
        ast.CallExpression(span:, callee: tag, arguments: [
          template,
          ..ast.template_expressions(parts)
        ]),
        None,
      )
    }

    ast.IntrinsicTemplateObject(site:, quasis:, ..) ->
      emit_template_object(site, quasis)

    // ── Logical / Conditional / Sequence (u-logical-cond-seq) ───────────────
    // §13.13 short-circuit: RHS is evaluated INSIDE the If arm so its side
    // effects are guarded. Port of emit.gleam:4470-4510.
    ast.LogicalExpression(operator: op, left:, right:, ..) -> {
      use l <- anf.then(expr(left))
      case op {
        ast.LogicalAnd -> anf.truthy_if(l, expr(right), anf.pure(l))
        ast.LogicalOr -> anf.truthy_if(l, anf.pure(l), expr(right))
        ast.NullishCoalescing -> anf.nullish_if(l, expr(right), anf.pure(l))
      }
    }
    // §13.14 ConditionalExpression: `c ? t : f`.
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) -> {
      use c <- anf.then(expr(condition))
      anf.truthy_if(c, expr(consequent), expr(alternate))
    }
    // §13.16 comma operator: eval left-to-right for effects, yield last.
    ast.SequenceExpression(expressions:, ..) -> emit_sequence(expressions)

    // ── Unary expressions (u-unary-delete, port emit.gleam:4322-4389) ───────
    ast.UnaryExpression(_, op, arg) -> emit_unary(op, arg)

    // ── Member access (u-member-key, port emit.gleam:4825-4856) ─────────────
    // §13.3.7.3 super.prop / super[k] — read via [[HomeObject]].[[Prototype]]
    // with receiver = lexical this. Must precede the generic arm.
    ast.MemberExpression(_, ast.SuperExpression(_), property) ->
      emit_super_get(property)
    // A `?.` link ANYWHERE in the object chain routes the WHOLE expression
    // through the shared-short-circuit chain compiler (§13.3.9.1).
    ast.MemberExpression(_, object, property) ->
      case ast_util.chain_has_optional(object) {
        True -> emit_chain_root(ex)
        False -> {
          use ov <- anf.then(expr(object))
          emit_member_get(ov, property, is_known_handle(object))
        }
      }
    // Optional member — always a chain root.
    ast.OptionalMemberExpression(..) -> emit_chain_root(ex)

    // ── Calls (u-call-new / u-optional-chain, port emit.gleam:4636-4740) ────
    ast.CallExpression(..) ->
      case ast_util.chain_has_optional(ex) {
        True -> emit_chain_root(ex)
        False -> emit_plain_call(ex)
      }
    // Optional call — always a chain root.
    ast.OptionalCallExpression(..) -> emit_chain_root(ex)

    // new Foo(args) — SPEC §8 construct(ctor, argsL, newTarget); newTarget=ctor.
    // Fast path: new_simple probes plain-function ctor (no class/derived/gen/
    // async/fields) → alloc {proto:ctor.prototype} + apply body; miss (atom)
    // → full t_construct. Spread bypasses the probe.
    ast.NewExpression(_, callee, args) -> {
      use c <- anf.then(expr(callee))
      use args_l <- anf.then(emit_args_list(args))
      case ast_util.has_spread_arg(args) {
        True -> anf.host("construct", [c, args_l, c])
        False -> {
          use r <- anf.then(anf.host("new_simple", [c, args_l]))
          use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, r)))
          anf.bind_if(
            is_miss,
            anf.host("construct", [c, args_l, c]),
            anf.pure(r),
          )
        }
      }
    }

    // ── Identifier + lexical pseudo-bindings (u-identifier-lexical) ─────────
    ast.Identifier(name: "undefined", ..) ->
      anf.then(consts(), fn(rc) { anf.pure(rc.undef) })
    ast.Identifier(name: "#" <> _, ..) ->
      throw_at_rt(
        "throw_syntax_error",
        "private field must be declared in an enclosing class",
      )
    ast.Identifier(name:, ..) -> emit_identifier(name)
    ast.ThisExpression(_) -> emit_lexical(lexical.RefThis)
    // Bare `super` is a §13.3.7.1 early SyntaxError — parser-unreachable.
    ast.SuperExpression(_) -> unreachable("bare super")
    ast.SpreadElement(..) -> unreachable("bare spread")
    ast.MetaProperty(_, ast.NewTarget) -> emit_lexical(lexical.RefNewTarget)
    ast.MetaProperty(_, ast.ImportMeta) ->
      throw_at_rt("throw_type_error", "unsupported: import.meta")

    // ── Update / Assignment (u-assign-update) ───────────────────────────────
    ast.UpdateExpression(_, op, prefix, target) ->
      emit_update(op, prefix, target)
    ast.AssignmentExpression(_, op, left, right) ->
      emit_assignment(op, left, right)

    // ── Object / Array literals (u-object-array) ────────────────────────────
    ast.ObjectExpression(_, properties) -> emit_object(properties, named)
    ast.ArrayExpression(_, elements) -> emit_array(elements)

    // ── Delegated: fn / arrow / class (u-delegate-dispatch) ─────────────────
    // FunctionExpression: pop the analyzer-assigned child scope id, then bridge
    // e.dispatch.emit_function. `named` seeds NamedEvaluation only when the
    // expression itself is anonymous (§15.2.5 / emit.gleam:5220).
    ast.FunctionExpression(_, self_name, params, body, is_gen, is_async) -> {
      let self = ast.binding_name(self_name)
      let inferred = case self {
        Some(_) -> self
        None -> named
      }
      emit_function_expr(
        state.FnExpr(self_name: self, is_gen:, is_async:),
        inferred,
        params,
        state.StmtBody(body),
      )
    }
    ast.ArrowFunctionExpression(_, params, body, is_async) -> {
      let fn_body = case body {
        ast.ArrowBodyBlock(stmts) -> state.StmtBody(stmts)
        ast.ArrowBodyExpression(e) -> state.ExprBody(e)
      }
      emit_function_expr(state.Arrow(is_async:), named, params, fn_body)
    }
    ast.ClassExpression(_, self_name, super_class, body) -> {
      let self = ast.binding_name(self_name)
      let inferred = case self {
        Some(_) -> self
        None -> named
      }
      bridge_expr(fn(e) {
        e.dispatch.emit_class(e, self, inferred, super_class, body)
      })
    }

    // ── Coroutine ops (u-delegate-dispatch) ─────────────────────────────────
    // EmitDispatch has no emit_await/emit_yield — route through host ops per
    // SPEC §8. M18 emit_async_body owns the coroutine machinery.
    ast.AwaitExpression(_, argument) -> {
      use v <- anf.then(expr(argument))
      anf.host("await", [v])
    }
    ast.YieldExpression(_, argument, is_delegate) -> {
      use rc <- anf.then(consts())
      use v <- anf.then(case argument {
        Some(a) -> expr(a)
        None -> anf.pure(rc.undef)
      })
      case is_delegate {
        True -> anf.host("yield_star", [v])
        False -> {
          use e <- anf.then(ask)
          // §27.6.3.8: async-gen `yield x` awaits x first (emit.gleam:4936).
          case e.is_async {
            True -> {
              use awaited <- anf.then(anf.host("await", [v]))
              anf.host("yield", [awaited])
            }
            False -> anf.host("yield", [v])
          }
        }
      }
    }

    // ── Dynamic import (u-delegate-dispatch, port emit.gleam:4995) ──────────
    ast.ImportExpression(_, source, options, phase) -> {
      use rc <- anf.then(consts())
      use src <- anf.then(expr(source))
      use opts <- anf.then(case options {
        Some(o) -> expr(o)
        None -> anf.pure(rc.undef)
      })
      // M12.md:457 3-arg form; emit.gleam:4995 branches on phase.
      let phase_atom = case phase {
        ast.PhaseEvaluation -> "evaluation"
        ast.PhaseSource -> "source"
        ast.PhaseDefer -> "defer"
      }
      anf.host("dynamic_import", [src, opts, ir.ConstAtom(phase_atom)])
    }
  }
}

/// Lower an expression with no NamedEvaluation hint.
pub fn expr(ex: ast.Expression) -> Build(ir.Value) {
  emit(ex, None)
}

/// SequenceExpression helper: eval each for side effects, yield the last.
fn emit_sequence(exprs: List(ast.Expression)) -> Build(ir.Value) {
  case exprs {
    [] -> {
      use rc <- anf.then(consts())
      anf.pure(rc.undef)
    }
    [only] -> expr(only)
    [head, ..tail] -> {
      use _ <- anf.then(expr(head))
      emit_sequence(tail)
    }
  }
}

/// state.EmitDispatch.emit_expr adapter (state.gleam:215). Runs the Build to
/// a Values-terminal ir.Expr via anf.run. Always Ok — see module doc.
pub fn emit_expr(
  e: Emitter2,
  ex: ast.Expression,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  Ok(anf.run(expr(ex), e))
}

// ── Identifier read (u-identifier-lexical) ──────────────────────────────────
// Port of emit.gleam:1800-1810. D15: WithChain resolutions surface as a
// runtime UnsupportedFeature throw. Slot/lexical helpers live below.

/// Resolve `name` in the current scope and read its value. Private names
/// (`#x`) resolve as ordinary class-scope minted-key locals (D9).
pub fn emit_identifier(name: String) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case state.resolve(e, name) {
    scope.Plain(d) -> emit_direct_get(d, name)
    scope.WithChain(..) ->
      throw_at_rt("throw_type_error", "unsupported: with (" <> name <> ")")
  }
}

// ── BinaryOp → IR (u-binop, SPEC §8 op table) ───────────────────────────────
// Add/Sub/Mul and the four relationals get the both-BEAM-numbers NumTerm fast
// path via anf.guarded_binop/guarded_cmp; everything else is a straight host
// call (M9 resolve_js classifies JPure/JRead/JMut — R8/R9).

fn binop(op: ast.BinaryOp, l: ir.Value, r: ir.Value) -> Build(ir.Value) {
  case op {
    ast.Add -> anf.guarded_binop(ir.NAdd, "add", l, r)
    ast.Subtract -> anf.guarded_binop(ir.NSub, "sub", l, r)
    ast.Multiply -> anf.guarded_binop(ir.NMul, "mul", l, r)
    ast.LessThan -> anf.guarded_cmp(ir.NLt, "lt", l, r)
    ast.LessThanEqual -> anf.guarded_cmp(ir.NLe, "le", l, r)
    ast.GreaterThan -> anf.guarded_cmp(ir.NGt, "gt", l, r)
    ast.GreaterThanEqual -> anf.guarded_cmp(ir.NGe, "ge", l, r)
    ast.Divide -> anf.host("div", [l, r])
    ast.Modulo -> anf.host("mod", [l, r])
    ast.Exponentiation -> anf.host("pow", [l, r])
    // R8: strict_eq/strict_ne are JPure and yield a TTerm bool atom directly —
    // no i32_to_bool wrap. Same `host` seam; M9 does the JPure classification.
    ast.StrictEqual -> anf.host("strict_eq", [l, r])
    ast.StrictNotEqual -> anf.host("strict_ne", [l, r])
    // Abstract equality: eq_fast (JPure, 0|1|miss) covers null/undef, num×
    // num, str×str, cell×cell, bool×bool; on `miss` (any pair reaching
    // ToPrimitive or a cross-type coercion) fall to full JMut `eq`. Both
    // arms yield Int 0|1. `!=` inverts via the i32 result directly.
    // §7.2.14 IsLooselyEqual yields a Boolean — re-branch the i32 result.
    ast.Equal -> {
      use v <- anf.then(loose_eq(l, r))
      anf.i32_to_js_bool(v)
    }
    ast.NotEqual -> {
      use v <- anf.then(loose_eq(l, r))
      use rc <- anf.then(consts())
      anf.bind_if(v, anf.pure(rc.false_), anf.pure(rc.true_))
    }
    ast.LeftShift ->
      case perf8_int_const_shift {
        True -> int_const_shift("erl_bsl", "shl_fast", "shl", l, r)
        False -> int_fast("shl_fast", "shl", l, r)
      }
    ast.RightShift ->
      case perf8_int_const_shift {
        True -> int_const_shift("erl_bsr", "shr_fast", "shr", l, r)
        False -> int_fast("shr_fast", "shr", l, r)
      }
    ast.UnsignedRightShift ->
      case perf8_int_const_shift {
        True -> int_fast("ushr_fast", "ushr", l, r)
        False -> anf.host("ushr", [l, r])
      }
    ast.BitwiseAnd -> int_const_bit("erl_band", "bitand_fast", "bitand", l, r)
    ast.BitwiseOr -> int_fast("bitor_fast", "bitor", l, r)
    ast.BitwiseXor -> int_fast("bitxor_fast", "bitxor", l, r)
    // §13.10.1 RelationalExpression `in` yields a Boolean; `t_in` returns the
    // i32 truth value, so re-branch it before it escapes as a JS value.
    ast.In -> {
      use v <- anf.then(anf.host("op_in", [l, r]))
      anf.i32_to_js_bool(v)
    }
    // instanceof_fast (JRead, 0|1|miss) probes the ordinary proto-walk; on
    // atom `miss` fall to full JMut instance_of. Both arms yield Int 0|1, and
    // §13.10.2 InstanceofOperator yields a Boolean — re-branch the result.
    ast.InstanceOf -> {
      use v <- anf.then(anf.host("instanceof_fast", [l, r]))
      use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
      use i <- anf.then(
        anf.bind_if(is_miss, anf.host("instance_of", [l, r]), anf.pure(v)),
      )
      anf.i32_to_js_bool(i)
    }
  }
}

/// §7.2.14 IsLooselyEqual: JPure `eq_fast` probe (0|1|miss) → full JMut
/// `eq` on `miss`. IsAtom on the Int 0|1 is false; on `miss` true.
/// `x == null` / `null == x` (either literal null OR undefined) is the
/// dominant richards shape — collapse to two INLINED `=:=` term-tests
/// (§7.2.14 steps 2-3+14: null/undef vs anything is 1 iff the other is
/// null/undef); NAdd is safe (both can't be 1) and lowers to a BEAM `+`
/// BIF, so the whole `x == null` cond is zero call_ext.
fn loose_eq(l: ir.Value, r: ir.Value) -> Build(ir.Value) {
  case is_nullish_const(l), is_nullish_const(r) {
    True, _ -> nul_eq_inline(r)
    _, True -> nul_eq_inline(l)
    False, False ->
      // One side a NON-NEGATIVE small-int constant (const-folded global
      // or expr_operand's raw ConstI32) → inline `is_integer` + `=:=`
      // fast path; float/non-int falls to eq_fast (§7.2.14 num×num uses
      // `==`, so 1.0==1 is true — the IsInt guard misses and eq_fast
      // handles it). ConstI32 with bit 31 set is a WRAPPED negative
      // (small_int_value stores unsigned bits) — `=:=` on the raw bits
      // would mismatch, so those stay on eq_fast.
      case l, r {
        ir.ConstI32(c), _ if c >= 0 && c < 0x80000000 -> int_const_eq(r, l)
        _, ir.ConstI32(c) if c >= 0 && c < 0x80000000 -> int_const_eq(l, r)
        _, _ -> loose_eq_slow(l, r)
      }
  }
}

fn int_const_eq(v: ir.Value, c: ir.Value) -> Build(ir.Value) {
  use is_i <- anf.then(anf.bind(ir.TermTest(ir.IsInt, v)))
  anf.bind_if(is_i, anf.bind(ir.NumTerm(ir.NEq, v, c)), loose_eq_slow(v, c))
}

fn loose_eq_slow(l: ir.Value, r: ir.Value) -> Build(ir.Value) {
  use v <- anf.then(anf.host("eq_fast", [l, r]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
  anf.bind_if(is_miss, anf.host("eq", [l, r]), anf.pure(v))
}

/// A binop operand: small-int NumberLiteral → bare ConstI32 (skips the
/// Let-bound Convert(BoxInt) so `binop` sees the constant directly and
/// its fast-path detectors fire); everything else via `expr`.
fn expr_operand(ex: ast.Expression) -> Build(ir.Value) {
  case ast_util.unwrap_parens(ex) {
    ast.NumberLiteral(_, ast.FiniteNumber(f)) ->
      case small_int_value(f) {
        Some(v) -> anf.mark_number(v)
        None -> expr(ex)
      }
    _ -> expr(ex)
  }
}

fn nul_eq_inline(v: ir.Value) -> Build(ir.Value) {
  use u <- anf.then(anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("undefined"))))
  use n <- anf.then(anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("null"))))
  anf.bind(ir.NumTerm(ir.NAdd, u, n))
}

// ── const-global folding (perf-gate-richards) ───────────────────────────────
// Top-level `var NAME = <int-literal>` NEVER reassigned in the whole script
// → reads inline the literal instead of `global_get_fast` (richards' 41k/run
// STATE_*/ID_*/KIND_* reads → 0 call_ext). Computed once at compile entry,
// stored on `Emitter2.const_globals`, read by `emit_direct_get`.

/// Collect foldable-as-constant top-level `var` bindings. A binding is
/// foldable iff (a) its declarator is `var NAME = <NumberLiteral(int)>`,
/// (b) NAME never appears as an assignment/update target ANYWHERE in the
/// script (including nested function bodies), (c) `eval`/`with` are absent
/// (D15 already rejects them, so no check needed here). Limitation: a read
/// that runs before the `var` line (via a hoisted function called earlier)
/// would observe `undefined` — no v8-v7 bench does this and it's a
/// perf-only fold (a wrongly-folded read is caught by the differential
/// tests, not silently miscompiled).
pub fn analyze_const_globals(
  body: List(ast.StmtWithLine),
) -> dict.Dict(String, ir.Value) {
  // Single left-to-right pass folds `var X = <int-literal>` AND
  // `var X = A op B` / `var X = ~A` / `var X = -A` where A,B are earlier
  // folded consts (richards' STATE_SUSPENDED_RUNNABLE / STATE_NOT_HELD).
  let cands =
    list.fold(body, dict.new(), fn(acc, s) {
      case s.statement {
        ast.VariableDeclaration(kind: ast.Var, declarations:) ->
          list.fold(declarations, acc, fn(acc, d) {
            case d {
              ast.VariableDeclarator(
                id: ast.IdentifierPattern(name:, ..),
                init: Some(init),
              ) ->
                case fold_const_init(acc, init) {
                  Some(v) -> dict.insert(acc, name, v)
                  None -> acc
                }
              _ -> acc
            }
          })
        _ -> acc
      }
    })
  case dict.is_empty(cands) {
    True -> cands
    False -> {
      let assigned = list.fold(body, set.new(), stmt_assigned_globals)
      dict.filter(cands, fn(name, _) { !set.contains(assigned, name) })
    }
  }
}

/// Fold an initializer to a small-int ir.Value using already-collected
/// consts. Handles NumberLiteral(int), Identifier→prior-const, `A|B`,
/// `A&B`, `A^B`, `A+B`, `A-B`, `A*B`, `~A`, `-A`, `(E)`.
fn fold_const_init(
  known: dict.Dict(String, ir.Value),
  ex: ast.Expression,
) -> Option(ir.Value) {
  case fold_const_int(known, ex) {
    Some(i) -> small_int_value(int.to_float(i))
    None -> None
  }
}

fn fold_const_int(
  known: dict.Dict(String, ir.Value),
  ex: ast.Expression,
) -> Option(Int) {
  case ex {
    ast.NumberLiteral(_, ast.FiniteNumber(f)) -> {
      let i = float.truncate(f)
      case int.to_float(i) == f {
        True -> Some(i)
        False -> None
      }
    }
    ast.Identifier(name:, ..) ->
      case dict.get(known, name) {
        Ok(ir.ConstI32(bits)) ->
          // ConstI32 stores unsigned bits; recover signed for arithmetic.
          case bits >= 0x80000000 {
            True -> Some(bits - 0x100000000)
            False -> Some(bits)
          }
        _ -> None
      }
    ast.ParenthesizedExpression(expression:, ..) ->
      fold_const_int(known, expression)
    ast.UnaryExpression(operator: ast.BitwiseNot, argument:, ..) ->
      option.map(fold_const_int(known, argument), fn(a) {
        int.bitwise_exclusive_or(a, -1)
      })
    ast.UnaryExpression(operator: ast.Negate, argument:, ..) ->
      option.map(fold_const_int(known, argument), int.negate)
    ast.BinaryExpression(operator:, left:, right:, ..) ->
      case fold_const_int(known, left), fold_const_int(known, right) {
        Some(a), Some(b) ->
          case operator {
            ast.BitwiseOr -> Some(int.bitwise_or(a, b))
            ast.BitwiseAnd -> Some(int.bitwise_and(a, b))
            ast.BitwiseXor -> Some(int.bitwise_exclusive_or(a, b))
            ast.Add -> Some(a + b)
            ast.Subtract -> Some(a - b)
            ast.Multiply -> Some(a * b)
            _ -> None
          }
        _, _ -> None
      }
    _ -> None
  }
}

/// A finite double `f` that's an exact small int → the ir.Value the JS
/// number `f` lowers to (mirrors `number_literal`'s smi arm, but a bare
/// term-level integer since it's used where a JsVal is expected — at BEAM
/// level `Convert(BoxInt(W32), ConstI32(n))` and `ConstI32(n)` both emit
/// `CInt(n)`; the Convert is a type-level annotation only).
fn small_int_value(f: Float) -> Option(ir.Value) {
  let i = float.truncate(f)
  case int.to_float(i) == f && i >= -2_147_483_648 && i < 2_147_483_648 {
    True -> Some(ir.ConstI32(int.bitwise_and(i, 0xFFFFFFFF)))
    False -> None
  }
}

/// Full-depth (INTO function bodies) collection of every Identifier name
/// appearing as an assignment/update target — the prune set for
/// analyze_const_globals. Over-approximates (includes locals shadowing a
/// global — safe, just misses a fold).
fn stmt_assigned_globals(
  acc: set.Set(String),
  s: ast.StmtWithLine,
) -> set.Set(String) {
  case s.statement {
    ast.EmptyStatement | ast.DebuggerStatement -> acc
    ast.BreakStatement(..) | ast.ContinueStatement(..) -> acc
    ast.ExpressionStatement(expression:, ..) -> ex_assigned(acc, expression)
    ast.BlockStatement(body:) -> list.fold(body, acc, stmt_assigned_globals)
    ast.VariableDeclaration(declarations:, ..) ->
      list.fold(declarations, acc, fn(acc, d) {
        case d.init {
          Some(e) -> ex_assigned(acc, e)
          None -> acc
        }
      })
    ast.ReturnStatement(argument:) -> opt_ex_assigned(acc, argument)
    ast.ThrowStatement(argument:) -> ex_assigned(acc, argument)
    ast.IfStatement(condition:, consequent:, alternate:) -> {
      let acc = ex_assigned(acc, condition)
      let acc = st_assigned(acc, consequent)
      case alternate {
        Some(a) -> st_assigned(acc, a)
        None -> acc
      }
    }
    ast.WhileStatement(condition:, body:)
    | ast.DoWhileStatement(condition:, body:) ->
      st_assigned(ex_assigned(acc, condition), body)
    ast.ForStatement(init:, condition:, update:, body:) -> {
      let acc = case init {
        Some(ast.ForInitExpression(e)) -> ex_assigned(acc, e)
        Some(ast.ForInitDeclaration(declarations:, ..)) ->
          list.fold(declarations, acc, fn(acc, d) {
            opt_ex_assigned(acc, d.init)
          })
        Some(ast.ForInitPattern(_)) | None -> acc
      }
      let acc = opt_ex_assigned(acc, condition)
      let acc = opt_ex_assigned(acc, update)
      st_assigned(acc, body)
    }
    ast.ForInStatement(left:, right:, body:)
    | ast.ForOfStatement(left:, right:, body:, ..) -> {
      let acc = case left {
        ast.ForInitExpression(ast.Identifier(name:, ..)) ->
          set.insert(acc, name)
        _ -> acc
      }
      st_assigned(ex_assigned(acc, right), body)
    }
    ast.SwitchStatement(discriminant:, cases:) -> {
      let acc = ex_assigned(acc, discriminant)
      list.fold(cases, acc, fn(acc, c) {
        let acc = opt_ex_assigned(acc, c.condition)
        list.fold(c.consequent, acc, stmt_assigned_globals)
      })
    }
    ast.TryStatement(block:, tail:) -> {
      let acc = list.fold(block, acc, stmt_assigned_globals)
      case tail {
        ast.TryCatch(handler:) ->
          list.fold(handler.body, acc, stmt_assigned_globals)
        ast.TryFinally(finalizer:) ->
          list.fold(finalizer, acc, stmt_assigned_globals)
        ast.TryCatchFinally(handler:, finalizer:) ->
          list.fold(
            finalizer,
            list.fold(handler.body, acc, stmt_assigned_globals),
            stmt_assigned_globals,
          )
      }
    }
    ast.LabeledStatement(body:, ..) -> st_assigned(acc, body)
    ast.WithStatement(object:, body:) ->
      st_assigned(ex_assigned(acc, object), body)
    ast.FunctionDeclaration(body:, params:, ..) ->
      list.fold(
        body,
        list.fold(params, acc, pat_default_assigned),
        stmt_assigned_globals,
      )
    ast.ClassDeclaration(super_class:, body:, ..) ->
      class_body_assigned(opt_ex_assigned(acc, super_class), body)
  }
}

fn st_assigned(acc: set.Set(String), s: ast.Statement) -> set.Set(String) {
  stmt_assigned_globals(acc, ast.StmtWithLine(0, s))
}

fn opt_ex_assigned(
  acc: set.Set(String),
  e: Option(ast.Expression),
) -> set.Set(String) {
  case e {
    Some(ex) -> ex_assigned(acc, ex)
    None -> acc
  }
}

fn pat_default_assigned(
  acc: set.Set(String),
  p: ast.Pattern,
) -> set.Set(String) {
  case p {
    ast.AssignmentPattern(right:, ..) -> ex_assigned(acc, right)
    _ -> acc
  }
}

fn class_body_assigned(
  acc: set.Set(String),
  body: List(ast.ClassElement),
) -> set.Set(String) {
  list.fold(body, acc, fn(acc, el) {
    case el {
      ast.ClassMethod(value: ast.FunctionLiteral(body:, ..), ..) ->
        list.fold(body, acc, stmt_assigned_globals)
      ast.ClassField(value:, ..) -> opt_ex_assigned(acc, value)
      ast.StaticBlock(body:) -> list.fold(body, acc, stmt_assigned_globals)
    }
  })
}

fn ex_assigned(acc: set.Set(String), ex: ast.Expression) -> set.Set(String) {
  case ex {
    ast.AssignmentExpression(left:, right:, ..) ->
      ex_assigned(ex_assigned(assign_target(acc, left), left), right)
    ast.UpdateExpression(argument:, ..) ->
      ex_assigned(assign_target(acc, argument), argument)
    ast.UnaryExpression(operator: ast.Delete, argument:, ..) ->
      ex_assigned(assign_target(acc, argument), argument)
    // Descend INTO function bodies (unlike stmt.gleam's per-function walk).
    ast.FunctionExpression(body:, params:, ..) ->
      list.fold(
        body,
        list.fold(params, acc, pat_default_assigned),
        stmt_assigned_globals,
      )
    ast.ArrowFunctionExpression(body:, params:, ..) -> {
      let acc = list.fold(params, acc, pat_default_assigned)
      case body {
        ast.ArrowBodyExpression(e) -> ex_assigned(acc, e)
        ast.ArrowBodyBlock(b) -> list.fold(b, acc, stmt_assigned_globals)
      }
    }
    ast.ClassExpression(super_class:, body:, ..) ->
      class_body_assigned(opt_ex_assigned(acc, super_class), body)
    // leaves
    ast.Identifier(..)
    | ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.ThisExpression(..)
    | ast.SuperExpression(..)
    | ast.MetaProperty(..)
    | ast.RegExpLiteral(..)
    | ast.IntrinsicTemplateObject(..)
    | ast.ImportExpression(..) -> acc
    // recurse
    ast.BinaryExpression(left:, right:, ..)
    | ast.LogicalExpression(left:, right:, ..) ->
      ex_assigned(ex_assigned(acc, left), right)
    ast.UnaryExpression(argument:, ..)
    | ast.AwaitExpression(argument:, ..)
    | ast.SpreadElement(argument:, ..) -> ex_assigned(acc, argument)
    ast.YieldExpression(argument:, ..) -> opt_ex_assigned(acc, argument)
    ast.ParenthesizedExpression(expression:, ..) -> ex_assigned(acc, expression)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      ex_assigned(
        ex_assigned(ex_assigned(acc, condition), consequent),
        alternate,
      )
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) ->
      list.fold(arguments, ex_assigned(acc, callee), ex_assigned)
    ast.MemberExpression(object:, property:, ..)
    | ast.OptionalMemberExpression(object:, property:, ..) -> {
      let acc = ex_assigned(acc, object)
      case property {
        ast.Bracket(expression:) -> ex_assigned(acc, expression)
        ast.Dot(..) -> acc
      }
    }
    ast.SequenceExpression(expressions:, ..) ->
      list.fold(expressions, acc, ex_assigned)
    ast.ArrayExpression(elements:, ..) ->
      list.fold(elements, acc, fn(acc, el) { opt_ex_assigned(acc, el) })
    ast.ObjectExpression(properties:, ..) ->
      list.fold(properties, acc, fn(acc, p) {
        case p {
          ast.InitProperty(value:, ..) -> ex_assigned(acc, value)
          ast.SpreadProperty(argument:) -> ex_assigned(acc, argument)
          ast.MethodProperty(value: ast.FunctionLiteral(body:, ..), ..)
          | ast.AccessorProperty(value: ast.FunctionLiteral(body:, ..), ..) ->
            list.fold(body, acc, stmt_assigned_globals)
        }
      })
    ast.TemplateLiteral(parts:, ..) ->
      list.fold(parts.tail, acc, fn(acc, part) { ex_assigned(acc, part.0) })
    ast.TaggedTemplateExpression(tag:, parts:, ..) ->
      list.fold(parts.tail, ex_assigned(acc, tag), fn(acc, part) {
        ex_assigned(acc, part.0)
      })
  }
}

fn assign_target(acc: set.Set(String), ex: ast.Expression) -> set.Set(String) {
  case ast_util.unwrap_parens(ex) {
    ast.Identifier(name:, ..) -> set.insert(acc, name)
    _ -> acc
  }
}

fn is_nullish_const(v: ir.Value) -> Bool {
  case v {
    ir.ConstAtom("null") | ir.ConstAtom("undefined") -> True
    _ -> False
  }
}

/// §13.12/§13.9 bitwise/shift: JPure `*_fast` probe (Int|miss — gate on
/// both bare BEAM integers, ToInt32-wrap in the FFI) → full JMut slow op
/// (ToPrimitive+ToNumeric chain) on `miss`.
fn int_fast(
  fast: String,
  slow: String,
  l: ir.Value,
  r: ir.Value,
) -> Build(ir.Value) {
  use v <- anf.then(anf.host(fast, [l, r]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
  anf.bind_if(is_miss, anf.host(slow, [l, r]), anf.pure(v))
}

/// `l & C` with a NON-NEGATIVE small-int-constant operand: inline
/// `is_integer(other) ? erlang:band(other, C) : bitand_fast(…)` — the
/// erlang `band` BIF lowers to an inline instruction, zero call_ext.
/// Skipping ToInt32 on `other` is safe for `band` with `0 ≤ C < 2³¹`
/// ONLY: the mask's high bits (incl. sign) are 0, so `other band C ≡
/// w32(other) band w32(C)` for every integer `other`. Negative C or
/// `bor`/`bxor` differ on sign extension → stay on the FFI probe.
fn int_const_bit(
  bif: String,
  fast: String,
  slow: String,
  l: ir.Value,
  r: ir.Value,
) -> Build(ir.Value) {
  case l, r {
    ir.ConstI32(c), _ if c >= 0 && c < 0x80000000 ->
      int_const_bit_go(bif, fast, slow, r, l)
    _, ir.ConstI32(c) if c >= 0 && c < 0x80000000 ->
      int_const_bit_go(bif, fast, slow, l, r)
    _, _ -> int_fast(fast, slow, l, r)
  }
}

fn int_const_bit_go(
  bif: String,
  fast: String,
  slow: String,
  v: ir.Value,
  c: ir.Value,
) -> Build(ir.Value) {
  use is_i <- anf.then(anf.bind(ir.TermTest(ir.IsInt, v)))
  anf.bind_if(is_i, anf.host(bif, [v, c]), int_fast(fast, slow, v, c))
}

/// `l >> C` / `l << C` with an int-literal shift count `0 ≤ C < 32`: inline
/// `is_integer(l) ∧ (l band mask == l) ? erlang:bsr/bsl(l, C) : *_fast` —
/// the erlang shift BIFs lower to inline instructions, zero call_ext (crypto
/// am3's ~3M/run `>>14`/`>>28`/`<<14`). §13.9 correctness: bare `bsr`/`bsl`
/// ≡ `w32(l) bsr/bsl C` iff `l ∈ [0, mask]`; `>>` mask is `2³¹-1`, `<<` mask
/// is `2^(31-C)-1` (result stays `< 2³¹`). Outside that range (rare) falls
/// to the `w32`-wrapping FFI. Shifts are non-commutative — only `r` probed.
fn int_const_shift(
  bif: String,
  fast: String,
  slow: String,
  l: ir.Value,
  r: ir.Value,
) -> Build(ir.Value) {
  case r {
    ir.ConstI32(c) if c >= 0 && c < 32 -> {
      let mask = case bif {
        "erl_bsl" -> int.bitwise_shift_left(1, 31 - c) - 1
        _ -> 0x7FFFFFFF
      }
      use is_i <- anf.then(anf.bind(ir.TermTest(ir.IsInt, l)))
      anf.bind_if(
        is_i,
        {
          use m <- anf.then(anf.host("erl_band", [l, ir.ConstI32(mask)]))
          use ok <- anf.then(anf.bind(ir.NumTerm(ir.NEq, m, l)))
          anf.bind_if(ok, anf.host(bif, [l, r]), anf.host(fast, [l, r]))
        },
        int_fast(fast, slow, l, r),
      )
    }
    _ -> int_fast(fast, slow, l, r)
  }
}

// ── Number literals (u-literals, SPEC §7.M12) ───────────────────────────────

/// Integral value in `[-2³¹, 2³¹)` boxes as a W32 smi via
/// `Convert(BoxInt(W32), ConstI32)`; every other finite double routes through
/// `host("float_lit", [ConstF64(bits)])` so `-0.0`/denormals stay bit-exact
/// (D5). `1e400` — the parser's sole non-finite literal — is `pos_inf`.
fn number_literal(n: ast.LiteralNumber) -> Build(ir.Value) {
  case n {
    ast.InfiniteNumber -> anf.then(consts(), fn(rc) { anf.pure(rc.pos_inf) })
    ast.FiniteNumber(f) -> {
      let i = float.truncate(f)
      case int.to_float(i) == f && i >= -2_147_483_648 && i < 2_147_483_648 {
        True ->
          anf.bind_number(ir.Convert(
            ir.BoxInt(ir.W32),
            ir.ConstI32(int.bitwise_and(i, 0xFFFFFFFF)),
          ))
        False ->
          anf.then(
            anf.host("float_lit", [ir.ConstF64(float_bits(f))]),
            anf.mark_number,
          )
      }
    }
  }
}

/// Raw IEEE-754 binary64 bit pattern of `f` as a non-negative Int in
/// `[0, 2⁶⁴)`. Bit-array round-trip per src/arc/vm/value.gleam:419.
fn float_bits(f: Float) -> Int {
  let assert <<bits:size(64)>> = <<f:float-size(64)>>
  bits
}

// ── Templates (u-template, port emit.gleam:4916,5016-5047,5059-5078) ────────

/// Globally-unique id for a tagged-template call site (§13.2.8.4 template
/// caching key). Port of emit.gleam:40 — a monotone erlang counter, so each
/// compiled site reuses its cached template object at runtime.
@external(erlang, "arc_vm_ffi", "unique_positive_integer")
fn unique_positive_integer() -> Int

/// Per-CALLSITE pdict key for the i-prop-ic / method-ic / obj-literal shape
/// caches. An ATOM, not a binary — `erlang:get/1` on an immediate atom is
/// ~2.8ns vs ~9.0ns for a heap binary (measured; pdict hashes the boxed
/// term). At 161k `.x` reads/run for richards that's ~1ms; obj_prop pays it
/// twice per iteration. Atoms are disjoint from the integer cell-id overlay
/// key space so no `pdict[Id]` collision. Atom-table growth is bounded by
/// static-site count (a few hundred per compiled module), not by iteration.
fn ic_site_key(prefix: String) -> ir.Value {
  ir.ConstAtom(prefix <> int.to_string(unique_positive_integer()))
}

/// `` `a${x}b${y}c` `` is `TemplateParts("a", [#(x,"b"), #(y,"c")])`. §13.2.8.5
/// concatenates via ToString (string-hint), NOT `+`'s ToPrimitive — so each
/// hole goes through host("to_string") and the join is host("string_concat")
/// (SPEC §8 JPure binary concat), never guarded_binop(NAdd,"add").
fn emit_template_literal(parts: ast.TemplateParts(String)) -> Build(ir.Value) {
  let head = ir.ConstBinary(bit_array.from_string(parts.head))
  list.fold(parts.tail, anf.pure(head), fn(acc_b, part) {
    let #(sub, quasi) = part
    use acc <- anf.then(acc_b)
    use v <- anf.then(expr(sub))
    use s <- anf.then(anf.host("to_string", [v]))
    use a1 <- anf.then(anf.host("string_concat", [acc, s]))
    anf.host("string_concat", [
      a1,
      ir.ConstBinary(bit_array.from_string(quasi)),
    ])
  })
}

/// GetTemplateObject (§13.2.8.4): the runtime caches a frozen array per
/// `site`. `cooked=None` (invalid escape in a tagged template, §12.9.6.1) →
/// `undefined`. Site is boxed W64 (unique_positive_integer is unbounded).
fn emit_template_object(
  site: Int,
  quasis: List(ast.TemplateQuasi),
) -> Build(ir.Value) {
  use rc <- anf.then(consts())
  let cooked =
    list.map(quasis, fn(q) {
      case q.cooked {
        Some(s) -> ir.ConstBinary(bit_array.from_string(s))
        None -> rc.undef
      }
    })
  let raw =
    list.map(quasis, fn(q) { ir.ConstBinary(bit_array.from_string(q.raw)) })
  use site_v <- anf.then(
    anf.bind(ir.Convert(ir.BoxInt(ir.W64), ir.ConstI64(site))),
  )
  use cooked_l <- anf.then(anf.cons_list(cooked))
  use raw_l <- anf.then(anf.cons_list(raw))
  anf.host("get_template_object", [site_v, cooked_l, raw_l])
}

/// Read scope slot `slot` — a boxed slot reads through host("cell_get").
pub fn read_slot(slot: Int, boxed: Bool) -> Build(ir.Value) {
  use e <- anf.then(ask)
  let v = ir.Var(state.get_slot_var(e, slot))
  case boxed {
    True -> anf.host("cell_get", [v])
    False -> anf.pure(v)
  }
}

/// The non-with ("static") read of a resolved binding. EvalEnv → D15.
pub fn emit_direct_get(d: scope.Direct, name: String) -> Build(ir.Value) {
  case d {
    // Optimization G made top-level `var` root-local, so a const_global now
    // resolves here (root VarBinding at js_main / CaptureBinding of it in
    // nested fns — both `origin_kind: VarBinding`) instead of the Global arm.
    // Re-check const_globals so `STATE_HELD` etc. still inline as ConstI32,
    // keeping binop's int_const_eq / int_const_bit fast paths live.
    scope.Local(slot:, boxed:, origin_kind: scope.VarBinding, ..) -> {
      use e <- anf.then(ask)
      case dict.get(e.const_globals, name) {
        Ok(lit) -> anf.pure(lit)
        Error(Nil) -> read_slot(slot, boxed)
      }
    }
    scope.Local(slot:, boxed:, ..) -> read_slot(slot, boxed)
    scope.Global(name: g) -> {
      use e <- anf.then(ask)
      // Optimization G: top-level `var`/`function` declared name → boxed
      // module-local cell (cell_get, ~3ns) instead of the global object
      // (global_get_fast + IsAtom + bind_if, ~8ns × 41k/run on richards).
      case dict.get(e.slotted_globals, g) {
        Ok(slot) -> read_slot(slot, True)
        Error(Nil) ->
          case dict.get(e.const_globals, g) {
            // Top-level `var G = <literal>` never reassigned in the whole
            // script — inline the literal (analyze_const_globals proved it).
            Ok(lit) -> anf.pure(lit)
            Error(Nil) -> {
              // global_get_fast (JRead) probes the global object's poly
              // overlay for own-data props. IsAtom catches `miss` AND
              // atom-valued globals — a perf-only conflation; full JMut
              // `global_get` re-reads.
              let kb = ir.ConstBinary(bit_array.from_string(g))
              use v <- anf.then(anf.host("global_get_fast", [kb]))
              use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
              anf.bind_if(is_miss, anf.host("global_get", [kb]), anf.pure(v))
            }
          }
      }
    }
    scope.EvalEnv(..) ->
      throw_at_rt(
        "throw_type_error",
        "unsupported: direct eval (" <> name <> ")",
      )
  }
}

/// Port of emit.gleam:2570 resolve_lexical — Option-returning so top-level
/// `this` reads as `undefined` instead of scope.lookup_lexical's panic.
fn resolve_lexical(
  e: Emitter2,
  ref: lexical.LexicalRef,
) -> Option(#(Int, Bool)) {
  let info = state.fn_info(e)
  case lexical.lexical_slot(info.lexical, ref) {
    Some(slot) ->
      Some(#(slot, lexical.lexical_refs_get(info.lexical_boxed, ref)))
    None ->
      case dict.get(info.lexical_captures, ref) {
        Ok(slot) -> Some(#(slot, True))
        Error(Nil) -> None
      }
  }
}

/// Read a lexical pseudo-binding (`this`, `new.target`, home_object,
/// active_func). None → `undefined` (Script/Module root, §16.1.6).
pub fn emit_lexical(ref: lexical.LexicalRef) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case resolve_lexical(e, ref) {
    Some(#(slot, boxed)) -> read_slot(slot, boxed)
    None -> anf.pure(e.consts.undef)
  }
}

/// Write `v` into the lexical `this` slot after `super()` — §10.2.4
/// BindThisValue step 3 throws ReferenceError if already initialized (second
/// `super()`). Port of emit.gleam:2620 set_this → Put*CheckInit; no
/// `cell_set_check_init` op in SPEC §8 so the guard is open-coded here.
fn set_lexical_this(v: ir.Value) -> Build(Nil) {
  fn(e: Emitter2, k) {
    case resolve_lexical(e, lexical.RefThis) {
      None -> k(e, Nil)
      Some(#(slot, boxed)) ->
        this_check_init(slot, boxed)(e, fn(e, _) {
          case boxed {
            True ->
              anf.host("cell_set", [ir.Var(state.get_slot_var(e, slot)), v])(
                e,
                fn(e, _) { k(e, Nil) },
              )
            False -> {
              let #(name, e) = state.fresh_var(e)
              ir.Let(
                [name],
                ir.Values([v]),
                k(state.set_slot_var(e, slot, name), Nil),
              )
            }
          }
        })
    }
  }
}

/// §10.2.4 step 3: read the current `this` slot; if it is NOT still `js_tdz`,
/// throw ReferenceError. The throw diverges at runtime so the caller's write
/// (which follows unconditionally in the IR) is only reached on the tdz path.
fn this_check_init(slot: Int, boxed: Bool) -> Build(Nil) {
  use rc <- anf.then(consts())
  use cur <- anf.then(read_slot(slot, boxed))
  use is_tdz <- anf.then(anf.host("strict_eq", [cur, rc.tdz]))
  use _ <- anf.then(anf.bind_if(
    is_tdz,
    anf.pure(rc.undef),
    throw_at_rt("throw_reference_error", "'this' is already bound"),
  ))
  anf.pure(Nil)
}

// ── Property keys / member reads (u-member-key) ─────────────────────────────

/// §7.1.19 ToPropertyKey split-probe (l-jread-reclass): JPure
/// `to_property_key_fast` returns the wire key on int/str/sym (no St —
/// primitives never mutate it) and `miss` on Handle/rare; the miss arm
/// falls to JMut `to_property_key`. IsAtom on the 2-tuple key is false.
pub fn to_property_key(v: ir.Value) -> Build(ir.Value) {
  case perf5_to_property_key_split {
    False -> anf.host("to_property_key", [v])
    True -> {
      use k <- anf.then(anf.host("to_property_key_fast", [v]))
      use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, k)))
      anf.bind_if(is_miss, anf.host("to_property_key", [v]), anf.pure(k))
    }
  }
}

/// Static/computed PropertyKey → runtime key value. The four literal shapes
/// lower to the compile-time-canonical wire tuple via `anf.object_key_lit`
/// (invariant #4 — no runtime ToPropertyKey). KeyPrivate resolves the
/// class-scope minted-key local (D9). KeyComputed evaluates then coerces.
pub fn emit_key(pk: ast.PropertyKey) -> Build(ir.Value) {
  case pk {
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..) -> anf.object_key_lit(pk)
    ast.KeyPrivate(name:, ..) -> emit_identifier(name)
    ast.KeyComputed(expression:) -> {
      use v <- anf.then(expr(expression))
      to_property_key(v)
    }
  }
}

/// MemberProperty (`.x` / `[e]`) → runtime key value. `#x` resolves the
/// minted private-key local; plain dot builds the wire key tuple; bracket
/// evaluates then ToPropertyKey.
pub fn emit_key_from_prop(prop: ast.MemberProperty) -> Build(ir.Value) {
  case prop {
    ast.Dot(name: "#" <> _ as name, ..) -> emit_identifier(name)
    ast.Dot(name:, span:) -> anf.object_key_lit(ast.KeyIdentifier(name:, span:))
    ast.Bracket(expression:) -> {
      use v <- anf.then(expr(expression))
      to_property_key(v)
    }
  }
}

fn is_private_prop(prop: ast.MemberProperty) -> Bool {
  case prop {
    ast.Dot(name: "#" <> _, ..) -> True
    _ -> False
  }
}

/// `Math.m(args)` → JPure host-op name when `m` ∈ the direct-dispatch set
/// AND `args` matches the FFI arity exactly (no spread). The caller still
/// checks `Math` resolves to Global (not shadowed) before firing.
fn math_direct_op(
  obj: ast.Expression,
  prop: ast.MemberProperty,
  args: List(ast.Expression),
) -> Option(String) {
  case obj, prop, ast_util.has_spread_arg(args) {
    ast.Identifier(name: "Math", ..), ast.Dot(name: m, ..), False ->
      case m, list.length(args) {
        "sqrt", 1 -> Some("math_sqrt")
        "floor", 1 -> Some("math_floor")
        "abs", 1 -> Some("math_abs")
        "pow", 2 -> Some("math_pow")
        "min", 2 -> Some("math_min")
        "max", 2 -> Some("math_max")
        _, _ -> None
      }
    _, _, _ -> None
  }
}

/// True iff `e` evaluates to a value that is DEFINITELY a `{js_cell,_}`
/// handle at runtime — so `get_prop_fast`/`set_prop_fast` can skip the
/// `is_tuple ∧ element(1)=:=js_cell` receiver guard. `this` in a method body
/// is `element(1, Frame)`, always the receiver handle. `new F(...)` returns
/// a fresh handle. A parenthesized either is transparent.
fn is_known_handle(e: ast.Expression) -> Bool {
  case ast_util.unwrap_parens(e) {
    ast.ThisExpression(..) -> True
    ast.NewExpression(..) -> True
    ast.ObjectExpression(..) -> True
    ast.ArrayExpression(..) -> True
    _ -> False
  }
}

/// True iff `v` is an ir.Var known (via state.known_handles) to hold a
/// `{js_cell,_}` handle — dataflow through `let`/write_slot for cases
/// `is_known_handle` (AST-only) can't see (`let o={…}; o.x`).
fn is_known_handle_val(e: Emitter2, v: ir.Value) -> Bool {
  case perf5_known_handle, v {
    True, ir.Var(name) -> state.is_known_handle(e, name)
    _, _ -> False
  }
}

/// Mark `v` (if a Var) as a known `{js_cell,_}` handle.
fn mark_handle(v: ir.Value) -> Build(Nil) {
  fn(e: Emitter2, k) {
    case v {
      ir.Var(name) -> k(state.mark_known_handle(e, name), Nil)
      _ -> k(e, Nil)
    }
  }
}

/// Static `.name` key as a raw binary — the own-data-property fast-path
/// discriminator. `#x` (private) and `[e]` (computed) are excluded.
fn static_dot_key(prop: ast.MemberProperty) -> Option(BitArray) {
  case prop {
    ast.Dot(name: "#" <> _, ..) -> None
    ast.Dot(name:, ..) -> Some(bit_array.from_string(name))
    ast.Bracket(..) -> None
  }
}

/// own-data-property READ fast path (SPEC i-prop-ic). Warm hit is INLINED —
/// zero `call_ext`. Receiver guard `is_tuple ∧ element(1)=:=js_cell` (bare
/// `is_tuple` admits `{js_bigint,N}`/`{js_sym,S}` whose element-2 collides
/// with the integer cell-id key space). Then `pdict[id]`:
///   * FLAT `{s_shaped_object,Sid,P,X0,…}` (i-prop-ic overlay) — check
///     `pdict[SiteKey]={Sid,OffF}` → `element(OffF,c)` (single BIF; slots
///     inlined at positions 4..N). SiteKey is a compile-time-unique
///     ConstBinary (literal — zero alloc; disjoint from cell-id keys).
///   * `#{KeyBin=>V}` map (SObject poly overlay) — direct `map_get`.
/// Any miss → `t_ic_get` (installs shaped overlay + SiteKey; on non-shaped
/// receivers subsumes the SObject own-data path) → full `get_prop`.
fn get_prop_fast(
  obj: ir.Value,
  kb: BitArray,
  known_handle: Bool,
  slow: Build(ir.Value),
) -> Build(ir.Value) {
  let site_key = ic_site_key("@pg")
  // `cold` is referenced from ~7 bind_if miss-arms. perf7: outline it as a
  // real ir.Function + CallDirect (hit path = pure nested bind_if, no Block
  // → every level let-cases). perf6 fallback: anf.share Block/Break join.
  use cold <- with_cold_get(obj, kb, site_key, slow)
  use id, simple_this <- with_receiver_id(obj, known_handle, or: cold)
  // simple_this: use the threaded `_this_c` slot (0 pdict-get). Otherwise:
  // fresh `pdict_get(id)`.
  use c <- anf.then(case simple_this {
    True -> read_this_c(id)
    False -> anf.host("pdict_get", [id])
  })
  // Shaped inline hit — `sid` passed in so simple_this reuses the entry-
  // hoisted `_this_sid`; general path derives from `c` after tag gate.
  let shaped_hit = fn(c: ir.Value, sid: ir.Value) {
    use sk <- anf.then(anf.host("pdict_get", [site_key]))
    use is_skt <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, sk)))
    anf.bind_if(
      is_skt,
      {
        use csid <- anf.then(anf.bind(anf.tuple_get(sk, 0)))
        use hit <- anf.then(anf.bind(ir.NumTerm(ir.NEq, csid, sid)))
        anf.bind_if(
          hit,
          {
            // FLAT pdict: `c = {tag,Sid,P,X0,…}`; SiteKey caches OffF
            // (=Off+4) so the hit is a single element/2 on `c`.
            use off_f <- anf.then(anf.bind(anf.tuple_get(sk, 1)))
            anf.host("erl_element", [off_f, c])
          },
          cold,
        )
      },
      cold,
    )
  }
  // pdict[Id] shape is disjoint by tag: FLAT `{s_shaped_object,Sid,P,X0,…}`
  // (i-prop-ic overlay) | MONO `{KeyBin,V}` (perf2 SObject 1-key cache —
  // obj_prop hot path) | `#{Kb=>V}` map (SObject poly, ≥2 keys) | undefined.
  case simple_this {
    True -> {
      // Sid derived per-site from `_this_c` (element 2 after the FLAT-shaped
      // guard) — NOT threaded via a slot. Threading `_this_sid` would need
      // refresh_this_c to re-derive it (an If → emit_core cont_inline_weight
      // bails → +~100k letrec applies/run vs the ~3 BIFs saved here).
      use is_ct <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, c)))
      anf.bind_if(
        is_ct,
        {
          use t0 <- anf.then(anf.bind(anf.tuple_get(c, 0)))
          use is_shaped <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, t0)))
          anf.bind_if(
            is_shaped,
            {
              use sid <- anf.then(anf.bind(anf.tuple_get(c, 1)))
              shaped_hit(c, sid)
            },
            cold,
          )
        },
        cold,
      )
    }
    False -> {
      use is_ct <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, c)))
      anf.bind_if(
        is_ct,
        {
          // Mono `{KeyBin,V}` FIRST — element(1) IS the cached key binary.
          // obj_prop hot hit = elem(1)+`=:=`+elem(2); shaped receivers pay
          // one ~free atom-vs-binary tag-mismatch `=:=` then the shaped gate.
          use t0 <- anf.then(anf.bind(anf.tuple_get(c, 0)))
          use is_mono <- anf.then(
            anf.bind(ir.NumTerm(ir.NEq, t0, ir.ConstBinary(kb))),
          )
          anf.bind_if(is_mono, anf.bind(anf.tuple_get(c, 1)), {
            // Not mono — gate on `is_atom(t0)`: pdict[Id] tuple element(1)
            // is either the atom tag or a mono key BINARY, so a single
            // tag-test distinguishes shaped from diff-key mono (cheaper
            // than `=:=` — one BEAM tag-bit test vs a term compare).
            use is_shaped <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, t0)))
            anf.bind_if(
              is_shaped,
              {
                use sid <- anf.then(anf.bind(anf.tuple_get(c, 1)))
                shaped_hit(c, sid)
              },
              cold,
            )
          })
        },
        {
          use is_cm <- anf.then(anf.bind(ir.TermTest(ir.IsMap, c)))
          anf.bind_if(
            is_cm,
            {
              use has <- anf.then(
                anf.bind(ir.MapOp(ir.MapHas, [c, ir.ConstBinary(kb)])),
              )
              anf.bind_if(
                has,
                anf.bind(
                  ir.MapOp(ir.MapGet, [
                    c,
                    ir.ConstBinary(kb),
                    ir.ConstAtom("undefined"),
                  ]),
                ),
                cold,
              )
            },
            cold,
          )
        },
      )
    }
  }
}

/// perf5 this-id-hoist — when `obj` is the simple-ABI `_this` param
/// (func.gleam simple_this_param), yield the entry-hoisted `_this_id` with
/// NO receiver guard and `simple_this=True` (`_this` is always `{js_cell,_}`;
/// its pdict overlay may be FLAT-shaped OR mono/map — emit_call_with_pair
/// dispatches CodeT for any receiver, so callers still tag-gate on `c`).
/// Otherwise: with_handle_id guard + fresh `id = element(2,obj)`.
fn with_receiver_id(
  obj: ir.Value,
  known_handle: Bool,
  or miss: Build(ir.Value),
  then body: fn(ir.Value, Bool) -> Build(ir.Value),
) -> Build(ir.Value) {
  case obj {
    // Strings match func.gleam simple_this_param / simple_this_id_param.
    ir.Var("_this") -> body(ir.Var("_this_id"), True)
    _ -> {
      use <- with_handle_id(obj, known_handle, or: miss)
      use id <- anf.then(anf.bind(anf.tuple_get(obj, 1)))
      body(id, False)
    }
  }
}

// perf6 mut-this-c: `_this_c` = `pdict[_this_id]`. The pdict entry ITSELF is
// the mutable slot — read fresh per `this.x` (1 `get` BIF); no slot_var(-1)
// rebind so anf.bind_if/share never widen for it. perf5's slot-threading
// (below, `perf6_mut_this_c=False`) kept 0-op reads but every set/refresh
// rebound slot -1 → `carried=[-1]` at each surrounding join → to_break_wide
// can't sink multi-binder Ifs → ~445k letrec-apply/run ≈1,780µs (richards).
const this_c_slot = -1

/// `pdict[_this_id]` — the mutable `_this_c` overlay. perf6 narrowed
/// (deltablue): entry-bound `_this_c` var (0-op) while `this_c_cache` holds,
/// else fresh pdict_get — cache invalidated (never rebound) on the first
/// `this.x=`/user-JS so slot -1 stays out of `carried` (richards floor kept).
/// perf5 fallback: threaded slot -1 var if inside a `_t` body.
fn read_this_c(id: ir.Value) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case perf6_mut_this_c, e.this_c_cache, dict.get(e.slot_vars, this_c_slot) {
    True, Some(name), _ -> anf.pure(ir.Var(name))
    False, _, Ok(name) -> anf.pure(ir.Var(name))
    _, _, _ -> anf.host("pdict_get", [id])
  }
}

/// perf6: drop `this_c_cache` — caller's `pdict_put(id,nc)` (immediately
/// after) IS the store; next read_this_c falls back to a fresh pdict_get.
/// No slot -1 rebind → `carried=[]`. perf5 fallback: rebind slot -1.
fn set_this_c(nc: ir.Value) -> Build(Nil) {
  fn(e: Emitter2, k) {
    case perf6_mut_this_c, dict.has_key(e.slot_vars, this_c_slot) {
      False, True -> {
        let #(name, e) = state.fresh_var(e)
        ir.Let(
          [name],
          ir.Values([nc]),
          k(state.set_slot_var(e, this_c_slot, name), Nil),
        )
      }
      True, _ -> k(state.drop_this_c_cache(e), Nil)
      _, _ -> k(e, Nil)
    }
  }
}

/// perf6: drop `this_c_cache` — `pdict[_this_id]` is already current after
/// any user-JS call (aliased writes → pdict_put; cold ops → jsv_install/
/// evict); next read_this_c falls back to a fresh pdict_get. No slot -1
/// rebind → `carried=[]`. perf5 fallback: re-read into a fresh slot -1
/// binder (LINEAR 1-Let so cont_inline_weight ≤6 still fires).
fn refresh_this_c() -> Build(Nil) {
  fn(e: Emitter2, k) {
    case perf6_mut_this_c, dict.has_key(e.slot_vars, this_c_slot) {
      False, True -> {
        let #(cn, e) = state.fresh_var(e)
        let e = state.set_slot_var(e, this_c_slot, cn)
        ir.Let(
          [cn],
          ir.CallHost("js", "pdict_get", [ir.Var("_this_id")]),
          k(e, Nil),
        )
      }
      True, _ -> k(state.drop_this_c_cache(e), Nil)
      _, _ -> k(e, Nil)
    }
  }
}

/// Emit the receiver `is_tuple ∧ element(1)=:=js_cell` guard around `body`.
/// `known_handle` (e.g. `this` inside a method body, `new F(...)`) skips the
/// `element(1)=:=js_cell` tag check — the value is either `{js_cell,_}` or
/// (Script-root `this`) `undefined`; `is_tuple` alone gates the latter.
/// `body` runs with `obj` proven to be `{js_cell,_}`.
fn with_handle_id(
  obj: ir.Value,
  known_handle: Bool,
  or miss: Build(ir.Value),
  then body: fn() -> Build(ir.Value),
) -> Build(ir.Value) {
  use e <- anf.then(ask)
  // known_handles dataflow (`let o={x:0}` → o marked via new_object_literal
  // result): DEFINITELY `{js_cell,_}` — skip both guards. `known_handle`
  // (AST-level: ThisExpression/NewExpression) keeps `is_tuple` since script-
  // root `this` may be `undefined` and NewExpression may throw-and-return.
  case is_known_handle_val(e, obj) {
    True -> body()
    False -> {
      use is_tup <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, obj)))
      anf.bind_if(
        is_tup,
        case known_handle {
          True -> body()
          False -> {
            use tag <- anf.then(anf.bind(anf.tuple_get(obj, 0)))
            use is_h <- anf.then(
              anf.bind(ir.NumTerm(ir.NEq, tag, ir.ConstAtom("js_cell"))),
            )
            anf.bind_if(is_h, body(), miss)
          }
        },
        miss,
      )
    }
  }
}

/// perf7 gate for get_prop_fast's cold arm. True → emit `get_prop_cold` (with
/// its `slow` reconstructed against the aux fn's obj param — every caller's
/// `slow` is `named_key_from_binary(kb) → get_prop`) as a jsf_cold_pg_*
/// ir.Function; each miss = `CallDirect(name,[obj])`. False → `anf.share`.
fn with_cold_get(
  obj: ir.Value,
  kb: BitArray,
  site_key: ir.Value,
  slow: Build(ir.Value),
  body: fn(Build(ir.Value)) -> Build(ir.Value),
) -> Build(ir.Value) {
  case perf7_cold_outline {
    False -> anf.share(get_prop_cold(obj, kb, site_key, slow), body)
    True -> {
      use name <- anf.then(
        anf.aux_fn("jsf_cold_pg_", [#("_cold_obj", ir.TTerm)], fn(ps) {
          let assert [o] = ps
          use iv <- anf.then(
            anf.host("ic_get", [o, ir.ConstBinary(kb), site_key]),
          )
          use ic_miss <- anf.then(
            anf.bind(ir.NumTerm(ir.NEq, iv, ir.ConstAtom("miss"))),
          )
          anf.bind_if(
            ic_miss,
            get_prop_proto_tier(o, kb, {
              use key <- anf.then(named_key_from_binary(kb))
              anf.host("get_prop", [o, key])
            }),
            anf.pure(iv),
          )
        }),
      )
      // Mirror anf.share's over-approximation: cold's refresh_this_c
      // (get_prop_cold:1727) drops this_c_cache for the OUTER continuation
      // via e_c threading; the CallDirect Build doesn't, so drop it here.
      use r <- anf.then(body(anf.bind(ir.CallDirect(name, [obj]))))
      use _ <- anf.then(refresh_this_c())
      anf.pure(r)
    }
  }
}

/// perf7 gate for set_prop_fast's cold arm. True → emit `set_prop_cold`
/// (reconstructed against the aux fn's obj/v params) as a jsf_cold_ps_*
/// ir.Function; each miss = `CallDirect(name,[obj,v])`. False → `anf.share`.
fn with_cold_set(
  obj: ir.Value,
  kb: BitArray,
  v: ir.Value,
  site_key: ir.Value,
  body: fn(Build(ir.Value)) -> Build(ir.Value),
) -> Build(ir.Value) {
  case perf7_cold_outline {
    False -> anf.share(set_prop_cold(obj, kb, v, site_key), body)
    True -> {
      use name <- anf.then(
        anf.aux_fn(
          "jsf_cold_ps_",
          [#("_cold_obj", ir.TTerm), #("_cold_v", ir.TTerm)],
          fn(ps) {
            let assert [o, cv] = ps
            use r <- anf.then(
              anf.host("ic_set", [o, ir.ConstBinary(kb), cv, site_key]),
            )
            use ic_miss <- anf.then(
              anf.bind(ir.NumTerm(ir.NEq, r, ir.ConstAtom("miss"))),
            )
            anf.bind_if(
              ic_miss,
              {
                use key <- anf.then(named_key_from_binary(kb))
                anf.host("set_prop", [o, key, cv])
              },
              anf.pure(r),
            )
          },
        ),
      )
      // Mirror anf.share: set_prop_cold:1901's refresh_this_c drops cache
      // for the outer continuation; CallDirect path must do the same.
      use r <- anf.then(body(anf.bind(ir.CallDirect(name, [obj, v]))))
      use _ <- anf.then(refresh_this_c())
      anf.pure(r)
    }
  }
}

/// Cold tier: `t_ic_get` (installs shaped overlay + SiteKey; on non-shaped
/// receivers tails into the SObject own-data path itself) → perf8 proto tier
/// → full `get_prop`. `=:= miss` NOT IsAtom — undefined/null/true/false are
/// legitimate slot values.
fn get_prop_cold(
  obj: ir.Value,
  kb: BitArray,
  site_key: ir.Value,
  slow: Build(ir.Value),
) -> Build(ir.Value) {
  use iv <- anf.then(anf.host("ic_get", [obj, ir.ConstBinary(kb), site_key]))
  // t_ic_get on `undefined` pdict[_this_id] installs the FLAT tuple (via
  // shaped_flat_install); on `miss` t_get_prop_any → t_cell_get →
  // jsv_overlay_slot may too. Either way `_this_c` is stale.
  use _ <- anf.then(refresh_this_c())
  use ic_miss <- anf.then(
    anf.bind(ir.NumTerm(ir.NEq, iv, ir.ConstAtom("miss"))),
  )
  anf.bind_if(ic_miss, get_prop_proto_tier(obj, kb, slow), anf.pure(iv))
}

/// perf8 tier between `t_ic_get` own-miss and `t_get_prop_any`: proto-chain
/// data-get via `t_ic_proto_get` (obj_ffi.erl). Warm hit resolves in one
/// pdict compare (raytrace `.prototype` on KFunction, proto-default reads);
/// `miss` (accessor / not found / no proto) falls to `slow`. Separate `@pgp`
/// SiteKey — `@pg` already caches `{Sid,OffF}`. Emitted inside the SHARED
/// cold arm so hot inline IR is unchanged.
fn get_prop_proto_tier(
  obj: ir.Value,
  kb: BitArray,
  slow: Build(ir.Value),
) -> Build(ir.Value) {
  case perf8_ic_proto_get {
    False -> slow
    True -> {
      let pgp_key = ic_site_key("@pgp")
      use pv <- anf.then(
        anf.host("ic_proto_get", [obj, ir.ConstBinary(kb), pgp_key]),
      )
      use pmiss <- anf.then(
        anf.bind(ir.NumTerm(ir.NEq, pv, ir.ConstAtom("miss"))),
      )
      anf.bind_if(pmiss, slow, anf.pure(pv))
    }
  }
}

/// Build the wire `{string_key, {named, kb}}` PropertyKey from a raw binary
/// — the miss-branch fallback key. Deferred INTO the miss arm so the hot path
/// pays zero MakeTuple.
fn named_key_from_binary(kb: BitArray) -> Build(ir.Value) {
  use inner <- anf.then(
    anf.bind(
      ir.TermOp(ir.MakeTuple, [
        ir.ConstAtom("named"),
        ir.ConstBinary(kb),
      ]),
    ),
  )
  anf.make_tuple([ir.ConstAtom("string_key"), inner])
}

/// own-data-property WRITE fast path (SPEC i-prop-ic). Warm hit is INLINED
/// (zero `call_ext`) — same receiver + `pdict[id]` dispatch as
/// `get_prop_fast`:
///   * FLAT shaped overlay `{s_shaped_object,Sid,P,X0,…}` + `pdict[SiteKey]
///     ={Sid,OffF}` → `put(id, setelement(OffF,c,v))` — ONE tuple alloc
///     (BIFs only; St untouched — coherence via jsv_evict/flush/overlay_slot).
///   * SObject poly-map overlay + key present → `put(id, maps:put(kb,v,c))`.
/// Any miss → `t_ic_set` (JRead; installs; subsumes the SObject own-data
/// write on non-shaped receivers) → full `t_set_prop_any`. Yields `v` (the
/// assignment expression's own value).
fn set_prop_fast(
  obj: ir.Value,
  kb: BitArray,
  known_handle: Bool,
  v: ir.Value,
) -> Build(ir.Value) {
  let site_key = ic_site_key("@pg")
  use _ <- anf.then({
    use cold <- with_cold_set(obj, kb, v, site_key)
    use id, simple_this <- with_receiver_id(obj, known_handle, or: cold)
    use c <- anf.then(case simple_this {
      True -> read_this_c(id)
      False -> anf.host("pdict_get", [id])
    })
    let shaped_hit = fn(c: ir.Value, sid: ir.Value, thread_nc: Bool) {
      use sk <- anf.then(anf.host("pdict_get", [site_key]))
      use is_skt <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, sk)))
      anf.bind_if(
        is_skt,
        {
          use csid <- anf.then(anf.bind(anf.tuple_get(sk, 0)))
          use hit <- anf.then(anf.bind(ir.NumTerm(ir.NEq, csid, sid)))
          anf.bind_if(
            hit,
            {
              // FLAT pdict: `c = {tag,Sid,P,X0,…}`; write is ONE setelement
              // on `c` — no nested Slots rebuild + wrapper alloc.
              use off_f <- anf.then(anf.bind(anf.tuple_get(sk, 1)))
              use nc <- anf.then(anf.host("erl_setelement", [off_f, c, v]))
              // simple_this: rebind slot -1 to `nc` so the next `this.x`
              // read sees it without a pdict_get.
              use _ <- anf.then(case thread_nc {
                True -> set_this_c(nc)
                False -> anf.pure(Nil)
              })
              anf.host("pdict_put", [id, nc])
            },
            cold,
          )
        },
        cold,
      )
    }
    // Same disjoint pdict[Id] shape ladder as get_prop_fast; simple_this
    // derives Sid per-site (see get_prop_fast comment).
    case simple_this {
      True -> {
        use is_ct <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, c)))
        anf.bind_if(
          is_ct,
          {
            use t0 <- anf.then(anf.bind(anf.tuple_get(c, 0)))
            use is_shaped <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, t0)))
            anf.bind_if(
              is_shaped,
              {
                use sid <- anf.then(anf.bind(anf.tuple_get(c, 1)))
                shaped_hit(c, sid, True)
              },
              cold,
            )
          },
          cold,
        )
      }
      False -> {
        use is_ct <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, c)))
        anf.bind_if(
          is_ct,
          {
            // Mono `{KeyBin,_}` FIRST — same ordering as get_prop_fast.
            use t0 <- anf.then(anf.bind(anf.tuple_get(c, 0)))
            use is_mono <- anf.then(
              anf.bind(ir.NumTerm(ir.NEq, t0, ir.ConstBinary(kb))),
            )
            anf.bind_if(
              is_mono,
              {
                use nc <- anf.then(anf.make_tuple([ir.ConstBinary(kb), v]))
                anf.host("pdict_put", [id, nc])
              },
              {
                // Shaped ⇔ is_atom(t0) — see get_prop_fast.
                use is_shaped <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, t0)))
                anf.bind_if(
                  is_shaped,
                  {
                    use sid <- anf.then(anf.bind(anf.tuple_get(c, 1)))
                    shaped_hit(c, sid, False)
                  },
                  cold,
                )
              },
            )
          },
          {
            use is_cm <- anf.then(anf.bind(ir.TermTest(ir.IsMap, c)))
            anf.bind_if(
              is_cm,
              {
                use has <- anf.then(
                  anf.bind(ir.MapOp(ir.MapHas, [c, ir.ConstBinary(kb)])),
                )
                anf.bind_if(
                  has,
                  {
                    use nc <- anf.then(
                      anf.bind(ir.MapOp(ir.MapPut, [c, ir.ConstBinary(kb), v])),
                    )
                    anf.host("pdict_put", [id, nc])
                  },
                  cold,
                )
              },
              cold,
            )
          },
        )
      }
    }
  })
  anf.pure(v)
}

fn set_prop_cold(
  obj: ir.Value,
  kb: BitArray,
  v: ir.Value,
  site_key: ir.Value,
) -> Build(ir.Value) {
  use r <- anf.then(anf.host("ic_set", [obj, ir.ConstBinary(kb), v, site_key]))
  use ic_miss <- anf.then(anf.bind(ir.NumTerm(ir.NEq, r, ir.ConstAtom("miss"))))
  use r <- anf.then(anf.bind_if(
    ic_miss,
    {
      use key <- anf.then(named_key_from_binary(kb))
      anf.host("set_prop", [obj, key, v])
    },
    anf.pure(r),
  ))
  // Either arm may have `put(_this_id, …)` (t_ic_set direct; t_set_prop_any
  // → t_cell_set → jsv_evict). `_this_c` is stale on the cold path.
  use _ <- anf.then(refresh_this_c())
  anf.pure(r)
}

/// Indexed-element READ fast path (SPEC array-index-fast-path). `idx` is
/// the RAW evaluated bracket expression (a JsVal, not a PropertyKey) — the
/// FFI probe gates on it being a bare non-negative integer landing in an
/// ArrayObj's Dense/Sparse elements. On `miss` (or an atom-valued hit —
/// `IsAtom` conflates them; a perf loss only) the caller-supplied `slow`
/// path runs. Callers own ToPropertyKey so a read-modify-write LValue
/// coerces its bracket expression exactly once (§6.2.5).
fn get_elem_fast(
  obj: ir.Value,
  idx: ir.Value,
  slow: Build(ir.Value),
) -> Build(ir.Value) {
  use e <- anf.then(ask)
  // perf8_arr_c_hoist: if `obj` is a loop-invariant base whose `{tc_arr,Id}`
  // overlay was pre-fetched into `arr_c` before the loop, read via that (0
  // pdict-get per hit). `_c` FFI falls to `_p` on undefined so cold install
  // still fires once. Only reachable when perf7_arr_pdict=True (stmt.gleam
  // gates the hoist on both consts).
  let hoisted = case perf8_arr_c_hoist, perf7_arr_pdict, obj {
    True, True, ir.Var(name) -> state.lookup_hoisted_arr_c(e, name)
    _, _, _ -> None
  }
  use v <- anf.then(case hoisted {
    Some(arr_c) -> anf.host("get_elem_fast_c", [arr_c, obj, idx])
    None ->
      case perf7_arr_pdict {
        True -> anf.host("get_elem_fast_p", [obj, idx])
        False -> anf.host("get_elem_fast", [obj, idx])
      }
  })
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
  anf.bind_if(is_miss, slow, anf.pure(v))
}

/// Indexed-element WRITE fast path. JMutMiss probe returns bare `St'` (the
/// rebound state — a tuple) on hit / `miss` atom on any shape mismatch, so
/// `IsAtom(r)` distinguishes them without a `{V,St'}` alloc; the miss arm
/// runs the caller-supplied `slow` path. Yields `v` (the assignment
/// expression's own value); `r` itself is discarded past the atom test.
fn set_elem_fast(
  obj: ir.Value,
  idx: ir.Value,
  v: ir.Value,
  slow: Build(ir.Value),
) -> Build(ir.Value) {
  let host = case perf7_arr_pdict {
    True -> "set_elem_fast_p"
    False -> "set_elem_fast"
  }
  use r <- anf.then(anf.host(host, [obj, idx, v]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, r)))
  use _ <- anf.then(anf.bind_if(is_miss, slow, anf.pure(v)))
  anf.pure(v)
}

pub fn emit_member_get(
  obj: ir.Value,
  prop: ast.MemberProperty,
  known_handle: Bool,
) -> Build(ir.Value) {
  case static_dot_key(prop) {
    Some(kb) ->
      get_prop_fast(obj, kb, known_handle, {
        use k <- anf.then(named_key_from_binary(kb))
        anf.host("get_prop", [obj, k])
      })
    None ->
      case prop {
        ast.Bracket(expression:) -> {
          use idx <- anf.then(expr(expression))
          get_elem_fast(obj, idx, {
            use k <- anf.then(to_property_key(idx))
            anf.host("get_prop", [obj, k])
          })
        }
        _ -> {
          use k <- anf.then(emit_key_from_prop(prop))
          case is_private_prop(prop) {
            True -> anf.host("private_get", [obj, k])
            False -> anf.host("get_prop", [obj, k])
          }
        }
      }
  }
}

/// §13.3.7.3 super.prop / super[k] — read via [[HomeObject]].[[Prototype]]
/// with receiver = lexical this. SPEC §8 super_get(home, this, key).
pub fn emit_super_get(prop: ast.MemberProperty) -> Build(ir.Value) {
  use this <- anf.then(emit_lexical(lexical.RefThis))
  use ho <- anf.then(emit_lexical(lexical.RefHomeObject))
  use k <- anf.then(emit_key_from_prop(prop))
  anf.host("super_get", [ho, this, k])
}

// ── Args list + call/construct + super() (u-call-new; SPEC §8) ──────────────

/// Evaluate `args` left-to-right into a runtime BEAM list value. No spread →
/// eval each then `cons_list`; any SpreadElement → left-fold, appending each
/// spread iterable via SPEC §8 `spread_into_list(acc, iterable) → list`.
pub fn emit_args_list(args: List(ast.Expression)) -> Build(ir.Value) {
  case ast_util.has_spread_arg(args) {
    False -> anf.then(anf.seq(list.map(args, expr)), anf.cons_list)
    True -> {
      use acc0 <- anf.then(anf.host("empty_list", []))
      fold_args_spread(args, acc0)
    }
  }
}

fn fold_args_spread(
  args: List(ast.Expression),
  acc: ir.Value,
) -> Build(ir.Value) {
  case args {
    [] -> anf.pure(acc)
    [ast.SpreadElement(_, arg), ..rest] -> {
      use v <- anf.then(expr(arg))
      use acc <- anf.then(anf.host("spread_into_list", [acc, v]))
      fold_args_spread(rest, acc)
    }
    [arg, ..rest] -> {
      use v <- anf.then(expr(arg))
      use acc <- anf.then(anf.bind(ir.TermOp(ir.MakeCons, [v, acc])))
      fold_args_spread(rest, acc)
    }
  }
}

/// D4: JS fn values are `{js_cell,N}` handles. Fast path: `kfn_code` reads the
/// KFunction ONCE and returns `{code, resolved_this, simple}` (§10.2.1.2
/// bind-this folded in) → `TermTest(IsTuple)` guards `CallClosure`; else §8
/// `t_call_checked`. One heap read per call, not two.
pub fn emit_call(
  f: ir.Value,
  this: ir.Value,
  args_l: ir.Value,
) -> Build(ir.Value) {
  use pair <- anf.then(anf.host("kfn_code", [f, this]))
  emit_call_with_pair(pair, f, this, Consed(args_l))
}

/// Call arguments as passed to `emit_call_with_pair`. `Positional` defers the
/// args cons-list build so the simple-ABI hit path emits ZERO MakeCons.
/// `ArgsList` is an already-built cons-list Value (the frame's raw `_args`) —
/// passed verbatim, arity unknown so simple-ABI is skipped.
pub type CallArgs {
  Consed(ir.Value)
  Positional(List(ir.Value))
  ArgsList(ir.Value)
}

/// `emit_call` with the `kfn_code` triple already in hand. stmt.gleam hoists
/// that host call out of loops for loop-invariant callees and passes the pair
/// var here so the per-iteration read is elided. `Positional(pos)` (spread-free
/// caller) enables the simple-ABI fast path: `pair.2` is
/// `none | {some,{code_s,arity,needs_this}}`; on arity match the call is
/// `CallClosure(code_s, pos)` (needs_this=False) or `CallClosure(code_s,
/// [this_r, ..pos])` (needs_this=True) — no Frame tuple, no args cons.
pub fn emit_call_with_pair(
  pair: ir.Value,
  f: ir.Value,
  this: ir.Value,
  args: CallArgs,
) -> Build(ir.Value) {
  // Cons the args-list only in arms that need it — `pos` values are already
  // Let-bound so duplicating the cons across branches reorders no side effects.
  let cons_args = case args {
    Consed(v) | ArgsList(v) -> anf.pure(v)
    Positional(pos) -> anf.cons_list(pos)
  }
  use is_kfn <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, pair)))
  let fast = {
    use rc <- anf.then(consts())
    use code <- anf.then(anf.bind(anf.tuple_get(pair, 0)))
    use this_r <- anf.then(anf.bind(anf.tuple_get(pair, 1)))
    let frame_path = {
      use args_l <- anf.then(cons_args)
      use frame <- anf.then(anf.make_tuple([this_r, f, rc.undef, rc.undef]))
      anf.bind(ir.CallClosure(code, [frame, args_l]))
    }
    case args {
      Consed(_) | ArgsList(_) -> frame_path
      Positional(pos) -> {
        use simple <- anf.then(anf.bind(anf.tuple_get(pair, 2)))
        use is_some <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, simple)))
        let simple_fast = {
          use inner <- anf.then(anf.bind(anf.tuple_get(simple, 1)))
          use code_s <- anf.then(anf.bind(anf.tuple_get(inner, 0)))
          use arity <- anf.then(anf.bind(anf.tuple_get(inner, 1)))
          use needs_this <- anf.then(anf.bind(anf.tuple_get(inner, 2)))
          use n <- anf.then(
            anf.bind(ir.Convert(
              ir.BoxInt(ir.W32),
              ir.ConstI32(list.length(pos)),
            )),
          )
          use ok <- anf.then(anf.bind(ir.NumTerm(ir.NEq, arity, n)))
          anf.bind_if(
            ok,
            {
              use nt <- anf.then(
                anf.bind(ir.NumTerm(ir.NEq, needs_this, rc.true_)),
              )
              anf.bind_if(
                nt,
                anf.bind(ir.CallClosure(code_s, [this_r, ..pos])),
                anf.bind(ir.CallClosure(code_s, pos)),
              )
            },
            frame_path,
          )
        }
        anf.bind_if(is_some, simple_fast, frame_path)
      }
    }
  }
  use r <- anf.then(
    anf.bind_if(is_kfn, fast, {
      use args_l <- anf.then(cons_args)
      anf.host("call", [f, this, args_l])
    }),
  )
  // User JS ran — `pdict[_this_id]` may have been rewritten (aliased write,
  // jsv_evict via t_cell_set). Refresh the threaded `_this_c`.
  use _ <- anf.then(refresh_this_c())
  anf.pure(r)
}

/// Method call `o.prop(args)` with `o` already Let-bound (§13.3.6.2 this=obj).
/// Static-dot ∧ spread-free → fused JMut `call_method_mono` (proto walk +
/// KFunction apply in ONE FFI call, `miss` on any shape mismatch); miss and
/// every non-fusable shape fall to `emit_member_get` → `kfn_code` →
/// `emit_call_with_pair` with `Positional(pos)` so simple-ABI still applies.
/// Args evaluate ONCE, before the probe — the miss arm re-uses `pos` (no
/// re-eval); the accessor-`prop` × side-effecting-arg reorder is the only
/// observable delta and `call_method_mono` misses on accessors.
fn emit_member_call(
  o: ir.Value,
  prop: ast.MemberProperty,
  known_handle: Bool,
  args: List(ast.Expression),
) -> Build(ir.Value) {
  case ast_util.has_spread_arg(args) {
    True -> {
      use f <- anf.then(emit_member_get(o, prop, known_handle))
      use args_l <- anf.then(emit_args_list(args))
      emit_call(f, o, args_l)
    }
    False ->
      case static_dot_key(prop) {
        None -> {
          // Computed / #private — key evals BEFORE args (§13.3.6 order).
          use f <- anf.then(emit_member_get(o, prop, known_handle))
          use pos <- anf.then(anf.seq(list.map(args, expr)))
          use pair <- anf.then(anf.host("kfn_code", [f, o]))
          emit_call_with_pair(pair, f, o, Positional(pos))
        }
        Some(kb) -> {
          // Per-CALLSITE polymorphic IC: SiteKey is a compile-time-unique
          // ConstBinary pdict key (literal — zero runtime alloc; disjoint
          // from the integer/atom overlay-key space). Cache format (see
          // twocore_rt_js_obj_ffi:tc_ic_install/6): mono `{{Sid,Proto},Code,
          // FnH,SimpleT}` | poly `#{{Sid,Proto} => {Code,FnH,SimpleT}}` |
          // `mega` | undefined. Keyed on the `{Sid,Proto}` PAIR — Sid alone
          // is unsound (two ctors with identical field sequence share a Sid
          // with different protos). Eviction rides tc_mc_clear (jsv_evict on
          // any dep proto sweeps every SiteKey).
          let site_key = ic_site_key("@ic")
          use pos <- anf.then(anf.seq(list.map(args, expr)))
          let n_pos = list.length(pos)
          // Dispatch a warm-hit entry `{Code,FnH,SimpleT}`. When SimpleT is
          // `{CodeT,Arity}` (this-abi variant, needs_this=true) with Arity ==
          // len(pos): CallClosure(CodeT, [o | pos]) — ZERO frame tuple, ZERO
          // args cons (compose N+O). Otherwise fall to the frame path.
          let dispatch_entry = fn(
            code: ir.Value,
            fnh: ir.Value,
            simple_t: ir.Value,
          ) -> Build(ir.Value) {
            let frame_path = {
              use rc <- anf.then(consts())
              use args_l <- anf.then(anf.cons_list(pos))
              use frame <- anf.then(
                anf.make_tuple([o, fnh, rc.undef, rc.undef]),
              )
              anf.bind(ir.CallClosure(code, [frame, args_l]))
            }
            case perf5_code_t {
              False -> {
                let _ = simple_t
                let _ = n_pos
                frame_path
              }
              True -> {
                // frame_path referenced from both bind_if miss-arms; share it
                // (Block/Break join) so the cons_list+frame tuple emits once.
                use frame_path <- anf.share(frame_path)
                use is_st <- anf.then(
                  anf.bind(ir.TermTest(ir.IsTuple, simple_t)),
                )
                anf.bind_if(
                  is_st,
                  {
                    use code_t <- anf.then(anf.bind(anf.tuple_get(simple_t, 0)))
                    use arity <- anf.then(anf.bind(anf.tuple_get(simple_t, 1)))
                    use n <- anf.then(
                      anf.bind(ir.Convert(ir.BoxInt(ir.W32), ir.ConstI32(n_pos))),
                    )
                    use ok <- anf.then(anf.bind(ir.NumTerm(ir.NEq, arity, n)))
                    anf.bind_if(
                      ok,
                      anf.bind(ir.CallClosure(code_t, [o, ..pos])),
                      frame_path,
                    )
                  },
                  frame_path,
                )
              }
            }
          }
          // Warm hit via JPure FFI probe (SPEC S ffi-method-ic-hit) —
          // t_method_ic_warm collapses the shaped-receiver + SiteKey mono/
          // poly cache match to ONE native cascade: `{hit,Code,FnH,SimpleT}`
          // on hit, atom `miss` on any guard fail (non-cell recv / pdict[RId]
          // absent or non-shaped / SiteKey absent, mega, or shape-mismatch).
          // ~8 IR ops + 1 call_ext replaces the prior ~25-BIF inline ladder;
          // richards' 40k/run × ~13ns ≈ 526µs vs the ladder's contribution to
          // ~486k local calls. Cold tier (proto-walk + SiteKey install) is the
          // single miss arm. The CallClosure result is never the atom `miss`
          // (unrepresentable from JS), so the post-probe `=:= miss` check only
          // fires for the cold FFI's non-shaped/non-object miss.
          let cold = {
            use args_l <- anf.then(anf.cons_list(pos))
            anf.host("call_method_ic", [
              o,
              ir.ConstBinary(kb),
              args_l,
              site_key,
            ])
          }
          use r <- anf.then(case perf5_inline_method_ic {
            False -> {
              let _ = dispatch_entry
              cold
            }
            True ->
              case perf5s_method_ic_ffi {
                True -> {
                  use w <- anf.then(anf.host("method_ic_warm", [o, site_key]))
                  use is_hit <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, w)))
                  anf.bind_if(
                    is_hit,
                    {
                      use code <- anf.then(anf.bind(anf.tuple_get(w, 1)))
                      use fnh <- anf.then(anf.bind(anf.tuple_get(w, 2)))
                      use st <- anf.then(anf.bind(anf.tuple_get(w, 3)))
                      dispatch_entry(code, fnh, st)
                    },
                    cold,
                  )
                }
                // perf5's ORIGINAL inline ladder (04c63b6) — restored so
                // gate (a) bisects perf4↔perf5 exactly. anf.share emits
                // `cold` once (Block/Break join) across the 7 miss-arms.
                False -> {
                  use cold <- anf.share(cold)
                  use id, simple_this <- with_receiver_id(
                    o,
                    known_handle,
                    or: cold,
                  )
                  use c <- anf.then(case simple_this {
                    True -> read_this_c(id)
                    False -> anf.host("pdict_get", [id])
                  })
                  use is_ct <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, c)))
                  let _ = simple_this
                  anf.bind_if(
                    is_ct,
                    {
                      use t0 <- anf.then(anf.bind(anf.tuple_get(c, 0)))
                      use is_shaped <- anf.then(
                        anf.bind(ir.TermTest(ir.IsAtom, t0)),
                      )
                      anf.bind_if(
                        is_shaped,
                        {
                          use sid <- anf.then(anf.bind(anf.tuple_get(c, 1)))
                          use proto <- anf.then(anf.bind(anf.tuple_get(c, 2)))
                          use sk <- anf.then(anf.host("pdict_get", [site_key]))
                          use is_skt <- anf.then(
                            anf.bind(ir.TermTest(ir.IsTuple, sk)),
                          )
                          let poly_arm = {
                            use is_skm <- anf.then(
                              anf.bind(ir.TermTest(ir.IsMap, sk)),
                            )
                            anf.bind_if(
                              is_skm,
                              {
                                use key <- anf.then(
                                  anf.make_tuple([sid, proto]),
                                )
                                use has <- anf.then(
                                  anf.bind(ir.MapOp(ir.MapHas, [sk, key])),
                                )
                                anf.bind_if(
                                  has,
                                  {
                                    use entry <- anf.then(
                                      anf.bind(
                                        ir.MapOp(ir.MapGet, [
                                          sk,
                                          key,
                                          ir.ConstAtom("undefined"),
                                        ]),
                                      ),
                                    )
                                    use code <- anf.then(
                                      anf.bind(anf.tuple_get(entry, 0)),
                                    )
                                    use fnh <- anf.then(
                                      anf.bind(anf.tuple_get(entry, 1)),
                                    )
                                    use st <- anf.then(
                                      anf.bind(anf.tuple_get(entry, 2)),
                                    )
                                    dispatch_entry(code, fnh, st)
                                  },
                                  cold,
                                )
                              },
                              cold,
                            )
                          }
                          anf.bind_if(
                            is_skt,
                            {
                              use ck <- anf.then(anf.bind(anf.tuple_get(sk, 0)))
                              use ck_sid <- anf.then(
                                anf.bind(anf.tuple_get(ck, 0)),
                              )
                              use hit_s <- anf.then(
                                anf.bind(ir.NumTerm(ir.NEq, ck_sid, sid)),
                              )
                              anf.bind_if(
                                hit_s,
                                {
                                  use ck_p <- anf.then(
                                    anf.bind(anf.tuple_get(ck, 1)),
                                  )
                                  use hit_p <- anf.then(
                                    anf.bind(ir.NumTerm(ir.NEq, ck_p, proto)),
                                  )
                                  anf.bind_if(
                                    hit_p,
                                    {
                                      use code <- anf.then(
                                        anf.bind(anf.tuple_get(sk, 1)),
                                      )
                                      use fnh <- anf.then(
                                        anf.bind(anf.tuple_get(sk, 2)),
                                      )
                                      use st <- anf.then(
                                        anf.bind(anf.tuple_get(sk, 3)),
                                      )
                                      dispatch_entry(code, fnh, st)
                                    },
                                    cold,
                                  )
                                },
                                cold,
                              )
                            },
                            poly_arm,
                          )
                        },
                        cold,
                      )
                    },
                    cold,
                  )
                }
              }
          })
          // `=:= miss` — NOT IsAtom: undefined/null/true/false are atoms and
          // are legitimate call results; an IsAtom guard would double-call.
          use is_miss <- anf.then(
            anf.bind(ir.NumTerm(ir.NEq, r, ir.ConstAtom("miss"))),
          )
          use r <- anf.then(anf.bind_if(
            is_miss,
            {
              use f <- anf.then(emit_member_get(o, prop, known_handle))
              use pair <- anf.then(anf.host("kfn_code", [f, o]))
              emit_call_with_pair(pair, f, o, Positional(pos))
            },
            anf.pure(r),
          ))
          // Inline warm hit ran user JS via CallClosure — `_this_c` stale.
          // (miss arm already refreshed inside emit_call_with_pair.)
          use _ <- anf.then(refresh_this_c())
          anf.pure(r)
        }
      }
  }
}

/// §13.3.7.1 step 12 InitializeInstanceElements — call the captured
/// `<class_fields_init>` closure with `this` when it isn't undefined.
fn emit_field_init_call() -> Build(Nil) {
  use init_fn <- anf.then(emit_identifier(ast_util.class_fields_init))
  use rc <- anf.then(consts())
  use this <- anf.then(emit_lexical(lexical.RefThis))
  use _ <- anf.then(anf.nullish_if(
    init_fn,
    anf.pure(rc.undef),
    anf.then(anf.cons_list([]), fn(nil_args) {
      emit_call(init_fn, this, nil_args)
    }),
  ))
  anf.pure(Nil)
}

/// §13.3.7.1 SuperCall. host("super_call",[active_func, argsL, new_target])
/// (SPEC §8 t_super_call arg order) performs GetPrototypeOf(active_func) +
/// [[Construct]]; result is bound as `this` (step 8) then field-init (step 12).
fn emit_super_call(args: List(ast.Expression)) -> Build(ir.Value) {
  use af <- anf.then(emit_lexical(lexical.RefActiveFunc))
  use nt <- anf.then(emit_lexical(lexical.RefNewTarget))
  use args_l <- anf.then(emit_args_list(args))
  use inst <- anf.then(anf.host("super_call", [af, args_l, nt]))
  use _ <- anf.then(set_lexical_this(inst))
  use e <- anf.then(ask)
  use _ <- anf.then(case e.field_init {
    state.FieldInitAfterSuper -> emit_field_init_call()
    state.NoFieldInit | state.FieldInitAtStart -> anf.pure(Nil)
  })
  anf.pure(inst)
}

// ── Optional-chain compiler (§13.3.9.1; port emit.gleam:4107-4258) ──────────
// One shared short-circuit exit: a nullish base at any `?.` link makes the
// ENTIRE chain evaluate to undefined. In the Build/IR shape there is no
// depth-1/depth-2 stack cleanup; each optional link Breaks out of an enclosing
// ir.Block with `undef`, which naturally scopes over later member reads AND
// call arguments.

pub fn emit_chain_root(ex: ast.Expression) -> Build(ir.Value) {
  use rc <- anf.then(consts())
  anf.bind_block(fn(exit) { emit_chain(ex, exit, rc.undef) })
}

fn emit_chain(
  ex: ast.Expression,
  exit: String,
  undef: ir.Value,
) -> Build(ir.Value) {
  case ast_util.chain_has_optional(ex) {
    False -> expr(ex)
    True ->
      case ex {
        ast.MemberExpression(_, obj, prop)
        | ast.OptionalMemberExpression(_, obj, prop) -> {
          use o <- anf.then(chain_obj(ex, obj, exit, undef))
          emit_member_get(o, prop, False)
        }
        ast.CallExpression(_, callee, args) ->
          case callee {
            ast.MemberExpression(_, ast.SuperExpression(_), _) -> {
              use #(f, this) <- anf.then(emit_chain_callee(callee, exit, undef))
              use args_l <- anf.then(emit_args_list(args))
              emit_call(f, this, args_l)
            }
            ast.MemberExpression(_, obj, prop)
            | ast.OptionalMemberExpression(_, obj, prop) -> {
              use o <- anf.then(chain_obj(callee, obj, exit, undef))
              emit_member_call(o, prop, False, args)
            }
            _ -> {
              use #(f, this) <- anf.then(emit_chain_callee(callee, exit, undef))
              use args_l <- anf.then(emit_args_list(args))
              emit_call(f, this, args_l)
            }
          }
        ast.OptionalCallExpression(_, callee, args) -> {
          use #(f, this) <- anf.then(emit_chain_callee(callee, exit, undef))
          use f <- anf.then(chain_guard(f, exit, undef))
          use args_l <- anf.then(emit_args_list(args))
          emit_call(f, this, args_l)
        }
        // chain_has_optional=True only for the arms above (+ TaggedTemplate,
        // a §13.3.1.1 early error). Fallback: plain emission.
        _ -> expr(ex)
      }
  }
}

/// If `v` is nullish, Break `exit` with undefined; else yield `v` unchanged.
fn chain_guard(v: ir.Value, exit: String, undef: ir.Value) -> Build(ir.Value) {
  use is_nul <- anf.then(anf.host("is_nullish", [v]))
  anf.bind_if(is_nul, fn(_e, _k) { ir.Break(exit, [undef]) }, anf.pure(v))
}

fn chain_obj(
  link: ast.Expression,
  obj: ast.Expression,
  exit: String,
  undef: ir.Value,
) -> Build(ir.Value) {
  use o <- anf.then(emit_chain(obj, exit, undef))
  case link {
    ast.OptionalMemberExpression(..) -> chain_guard(o, exit, undef)
    _ -> anf.pure(o)
  }
}

/// Emit a chain call's callee. Returns `#(f, this)` — `this` is the receiver
/// for a member callee (obj / lexical-this for super), else undefined.
fn emit_chain_callee(
  callee: ast.Expression,
  exit: String,
  undef: ir.Value,
) -> Build(#(ir.Value, ir.Value)) {
  case callee {
    ast.MemberExpression(_, ast.SuperExpression(_), prop) -> {
      use f <- anf.then(emit_super_get(prop))
      use this <- anf.then(emit_lexical(lexical.RefThis))
      anf.pure(#(f, this))
    }
    ast.MemberExpression(_, obj, prop)
    | ast.OptionalMemberExpression(_, obj, prop) -> {
      use o <- anf.then(chain_obj(callee, obj, exit, undef))
      use f <- anf.then(emit_member_get(o, prop, False))
      anf.pure(#(f, o))
    }
    _ -> {
      use f <- anf.then(emit_chain(callee, exit, undef))
      anf.pure(#(f, undef))
    }
  }
}

// ── UnaryExpression (u-unary-delete, port emit.gleam:1854-1916,4322-4389) ───

/// §13.5.3 `typeof name` — an unresolvable Reference yields "undefined", never
/// throws. Port of emit.gleam:1854 emit_var_typeof (with-chain dropped per D15).
fn emit_typeof_ident(name: String) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case state.resolve(e, name) {
    scope.Plain(scope.Local(slot:, boxed:, ..)) -> {
      use v <- anf.then(read_slot(slot, boxed))
      anf.host("type_of", [v])
    }
    scope.Plain(scope.Global(name: g)) ->
      anf.host("global_typeof", [ir.ConstBinary(bit_array.from_string(g))])
    scope.Plain(scope.EvalEnv(..)) ->
      throw_at_rt("throw_type_error", "unsupported: direct eval")
    scope.WithChain(..) -> throw_at_rt("throw_type_error", "unsupported: with")
  }
}

/// §13.5.1.2 `delete name` (sloppy). Port of emit.gleam:1907 emit_var_delete
/// with the with-chain probe dropped per D15.
fn emit_delete_ident(name: String) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case state.resolve(e, name) {
    // Declarative bindings are never deletable (§9.1.1.1.7).
    scope.Plain(scope.Local(..)) -> anf.pure(e.consts.false_)
    scope.Plain(scope.Global(name: g)) ->
      anf.host("global_delete", [ir.ConstBinary(bit_array.from_string(g))])
    scope.Plain(scope.EvalEnv(..)) ->
      throw_at_rt("throw_type_error", "unsupported: direct eval")
    scope.WithChain(..) -> throw_at_rt("throw_type_error", "unsupported: with")
  }
}

/// §13.5.1 `delete UnaryExpression`. Port of emit.gleam:4336-4379.
fn emit_delete(arg: ast.Expression) -> Build(ir.Value) {
  use rc <- anf.then(consts())
  case ast_util.unwrap_parens(arg) {
    // §13.5.1.2 step 5.b — `delete super.x` / `delete super[e]` throws
    // ReferenceError. Evaluate `this` (TDZ) and any computed key for side
    // effects first (§13.3.7 ordering), then throw.
    ast.MemberExpression(_, ast.SuperExpression(_), property) -> {
      use _ <- anf.then(emit_lexical(lexical.RefThis))
      use _ <- anf.then(case property {
        ast.Bracket(expression:) -> expr(expression)
        ast.Dot(..) -> anf.pure(rc.undef)
      })
      throw_at_rt("throw_reference_error", "Unsupported reference to 'super'")
    }
    ast.MemberExpression(_, obj, prop) -> {
      use ov <- anf.then(expr(obj))
      use k <- anf.then(emit_key_from_prop(prop))
      anf.host("delete_prop", [ov, k])
    }
    ast.Identifier(name:, ..) -> emit_delete_ident(name)
    // Any other expression: evaluate for side effects, result is `true`.
    other -> {
      use _ <- anf.then(expr(other))
      anf.pure(rc.true_)
    }
  }
}

fn emit_unary(op: ast.UnaryOp, arg: ast.Expression) -> Build(ir.Value) {
  case op {
    ast.TypeOf ->
      case ast_util.unwrap_parens(arg) {
        ast.Identifier(name:, ..) -> emit_typeof_ident(name)
        inner -> {
          use v <- anf.then(expr(inner))
          anf.host("type_of", [v])
        }
      }
    ast.Delete -> emit_delete(arg)
    ast.Void -> {
      use _ <- anf.then(expr(arg))
      use rc <- anf.then(consts())
      anf.pure(rc.undef)
    }
    ast.LogicalNot -> {
      use v <- anf.then(expr(arg))
      use rc <- anf.then(consts())
      anf.truthy_if(v, anf.pure(rc.false_), anf.pure(rc.true_))
    }
    ast.Negate -> anf.then(expr(arg), fn(v) { anf.host("neg", [v]) })
    ast.UnaryPlus -> anf.then(expr(arg), fn(v) { anf.host("plus", [v]) })
    ast.BitwiseNot ->
      anf.then(expr(arg), fn(v) {
        use r <- anf.then(anf.host("bitnot_fast", [v]))
        use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, r)))
        anf.bind_if(is_miss, anf.host("bitnot", [v]), anf.pure(r))
      })
  }
}

// ── Plain (non-optional-chain) CallExpression dispatch (u-call-new) ─────────
// Port of emit.gleam:4641-4798. Reached only when
// `chain_has_optional(ex)==False`; the optional-chain path handles `?.` links.

/// General `X.apply(Y, arguments)` lowering: evaluate `X` → f, `Y` → recv,
/// then `kfn_code(f, recv)` → frame-path call with the raw `_args` cons-list
/// passed verbatim (arity unknown, so simple-ABI is skipped).
fn emit_apply_raw_general(
  inner: ast.Expression,
  recv_arg: ast.Expression,
  raw_args: ir.Value,
) -> Build(ir.Value) {
  use f <- anf.then(expr(inner))
  use recv <- anf.then(expr(recv_arg))
  use pair <- anf.then(anf.host("kfn_code", [f, recv]))
  emit_call_with_pair(pair, f, recv, ArgsList(raw_args))
}

/// `X.apply(Y, arguments)` fast-path — forwards the frame's raw `_args`
/// cons-list directly, eliding the arguments-object read + Function.prototype
/// .apply reflection (raytrace `Class.create`: 66k× per run). The tighter
/// `this.M.apply(this, arguments)` shape routes through `call_method_ic` so
/// the proto-method IC still installs; miss falls to the general form.
fn emit_apply_arguments(
  inner: ast.Expression,
  recv_arg: ast.Expression,
  raw_args: ir.Value,
) -> Build(ir.Value) {
  case ast_util.unwrap_parens(inner), ast_util.unwrap_parens(recv_arg) {
    ast.MemberExpression(_, ast.ThisExpression(_), mprop), ast.ThisExpression(_)
    ->
      case static_dot_key(mprop) {
        Some(kb) -> {
          let site_key = ic_site_key("@ic")
          use this <- anf.then(emit_lexical(lexical.RefThis))
          use r <- anf.then(
            anf.host("call_method_ic", [
              this,
              ir.ConstBinary(kb),
              raw_args,
              site_key,
            ]),
          )
          use is_miss <- anf.then(
            anf.bind(ir.NumTerm(ir.NEq, r, ir.ConstAtom("miss"))),
          )
          use r <- anf.then(anf.bind_if(
            is_miss,
            {
              use f <- anf.then(emit_member_get(this, mprop, False))
              use pair <- anf.then(anf.host("kfn_code", [f, this]))
              emit_call_with_pair(pair, f, this, ArgsList(raw_args))
            },
            anf.pure(r),
          ))
          use _ <- anf.then(refresh_this_c())
          anf.pure(r)
        }
        None -> emit_apply_raw_general(inner, recv_arg, raw_args)
      }
    _, _ -> emit_apply_raw_general(inner, recv_arg, raw_args)
  }
}

fn emit_plain_call(ex: ast.Expression) -> Build(ir.Value) {
  let assert ast.CallExpression(_, callee, args) = ex
  case callee {
    // super(args) — §13.3.7.1 SuperCall.
    ast.SuperExpression(_) -> emit_super_call(args)
    // super.m(args) / super[k](args) — §13.3.7.3 read + call with lexical this.
    ast.MemberExpression(_, ast.SuperExpression(_), prop) -> {
      use f <- anf.then(emit_super_get(prop))
      use this <- anf.then(emit_lexical(lexical.RefThis))
      use args_l <- anf.then(emit_args_list(args))
      emit_call(f, this, args_l)
    }
    // `X.apply(Y, arguments)` → direct call with raw `_args`. Gated on
    // raw_args_var (non-arrow frame-ABI body only) AND `arguments` resolving
    // to the fn-scope's implicit binding — a param/let/catch shadow means the
    // identifier is NOT the object built from `_args`, so fall through.
    ast.MemberExpression(_, inner, ast.Dot(name: "apply", ..) as prop) -> {
      use e <- anf.then(ask)
      case args, e.raw_args_var, state.arguments_is_implicit(e) {
        [recv_arg, ast.Identifier(name: "arguments", ..)], Some(raw), True ->
          emit_apply_arguments(inner, recv_arg, ir.Var(raw))
        _, _, _ -> {
          use o <- anf.then(expr(inner))
          emit_member_call(o, prop, is_known_handle(inner), args)
        }
      }
    }
    // obj.m(args) / obj[k](args) — bind `this` to obj (§13.3.6.2).
    ast.MemberExpression(_, obj, prop) ->
      case math_direct_op(obj, prop, args) {
        // Math.sqrt/floor/abs/pow/min/max → JPure FFI (raytrace hot path).
        // Skips the Math-object property lookup + KFunction dispatch. On
        // `miss` (non-number arg) coerce via ToNumber (§21.3.2 step 1) and
        // retry — args evaluate ONCE either way.
        Some(op) -> {
          use e <- anf.then(ask)
          // Fast path only when `Math` resolves to the untouched global
          // builtin: not shadowed by a local AND not G-slotted (top-level
          // `var Math = ...` still resolves as Global but reads a cell).
          case
            state.resolve(e, "Math"),
            state.lookup_slotted_global(e, "Math")
          {
            scope.Plain(scope.Global(_)), None -> {
              use pos <- anf.then(anf.seq(list.map(args, expr)))
              use v <- anf.then(anf.host(op, pos))
              // `=:= miss` (NOT IsAtom: js_nan/js_inf are also atoms).
              use is_miss <- anf.then(
                anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("miss"))),
              )
              anf.bind_if(
                is_miss,
                {
                  // §21.3.2 step 1 ToNumber. `t_plus` returns JsVal wire
                  // (bare int/float/js_nan/js_inf/js_neg_inf); `t_to_number`
                  // returns JsNum ({j_int,_}/…) which the FFI can't match.
                  use coerced <- anf.then(
                    anf.seq(list.map(pos, fn(a) { anf.host("plus", [a]) })),
                  )
                  anf.host(op, coerced)
                },
                anf.pure(v),
              )
            }
            // `Math` is shadowed or slotted — no fast path.
            _, _ -> {
              use o <- anf.then(expr(obj))
              emit_member_call(o, prop, is_known_handle(obj), args)
            }
          }
        }
        None -> {
          use o <- anf.then(expr(obj))
          emit_member_call(o, prop, is_known_handle(obj), args)
        }
      }
    // Direct-eval candidate — D15 UnsupportedFeature.
    ast.Identifier(name: "eval", ..) ->
      throw_at_rt("throw_type_error", "unsupported: direct eval")
    // Regular call f(args): thisValue = undefined (§13.3.6.2 step 1.b.iii).
    // When the callee is an unboxed local ∉ carried OR a slotted-global cell
    // never reassigned in the loop, stmt.gleam's loop emit hoists
    // `kfn_code(f, undef)` before the ir.Loop and records the pair var in
    // e.hoisted_kfn — reuse it here to skip the per-iteration heap read.
    // Slotted-global entries are keyed `-1 - slot` (disjoint from local slots).
    _ -> {
      use rc <- anf.then(consts())
      use e <- anf.then(ask)
      let hoisted = case ast_util.unwrap_parens(callee) {
        ast.Identifier(name:, ..) ->
          case state.resolve(e, name) {
            scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
              state.lookup_hoisted_kfn(e, slot)
            // Slotted-global boxed cell — keyed at `-1 - slot` (current
            // frame's slot; matches loop_invariant_callees' key).
            scope.Plain(scope.Local(slot:, boxed: True, ..)) ->
              state.lookup_hoisted_kfn(e, -1 - slot)
            _ -> None
          }
        _ -> None
      }
      use f <- anf.then(expr(callee))
      // Spread-free → keep the positional value list so `emit_call_with_pair`
      // can try the simple-ABI closure; the hoisted (or fresh) `pair` supplies
      // both the frame code AND `pair.2` — hoist × simple-abi compose.
      case ast_util.has_spread_arg(args) {
        False -> {
          use pos <- anf.then(anf.seq(list.map(args, expr)))
          use pair <- anf.then(case hoisted {
            Some(p) -> anf.pure(p)
            None -> anf.host("kfn_code", [f, rc.undef])
          })
          emit_call_with_pair(pair, f, rc.undef, Positional(pos))
        }
        True -> {
          use args_l <- anf.then(emit_args_list(args))
          case hoisted {
            Some(pair) -> emit_call_with_pair(pair, f, rc.undef, Consed(args_l))
            None -> emit_call(f, rc.undef, args_l)
          }
        }
      }
    }
  }
}

// ── u-lvalue: evaluate-once protocol for compound/update/logical-assign ─────
// Port of emit.gleam:2075-2130 emit_static_put + 2682-2727 LvalueShape,
// re-shaped for the Build monad. §13.15.2 / §13.4: base and key evaluate
// exactly once, in that order, before the RHS.

/// Unconditional store to local slot `slot`, yielding `v`. Boxed → cell_set;
/// unboxed → fresh Let-rebind + state.set_slot_var so later Identifier reads
/// see the new SSA name (SPEC gotcha 1848 unboxed-rebind).
fn write_slot(slot: Int, boxed: Bool, v: ir.Value) -> Build(ir.Value) {
  fn(e: Emitter2, k) {
    case boxed {
      True ->
        anf.then(
          anf.host("cell_set", [ir.Var(state.get_slot_var(e, slot)), v]),
          fn(_) { anf.pure(v) },
        )(e, k)
      False -> {
        let #(name, e) = state.fresh_var(e)
        // Propagate known-number through the unboxed alias so a slot re-read
        // (via read_slot → ir.Var(name)) still elides `is_number` guards.
        let e = case anf.is_known_number(e, v) {
          True -> state.mark_known_number(e, name)
          False -> e
        }
        // Same propagation for known-handle (obj_prop: `let o={x:0}` → o).
        let e = case is_known_handle_val(e, v) {
          True -> state.mark_known_handle(e, name)
          False -> e
        }
        ir.Let([name], ir.Values([v]), k(state.set_slot_var(e, slot, name), v))
      }
    }
  }
}

/// TDZ-checked store (§9.1.1.1.5 step 5). Port emit.gleam:2118: 2core reads
/// (Var / cell_get) do NOT throw on `js_tdz`, so an explicit `tdz_check` op
/// (M12 DECISION) validates the current value before the store.
fn write_slot_checked(
  slot: Int,
  boxed: Bool,
  name: String,
  v: ir.Value,
) -> Build(ir.Value) {
  use cur <- anf.then(read_slot(slot, boxed))
  use _ <- anf.then(
    anf.host("tdz_check", [
      cur,
      ir.ConstBinary(bit_array.from_string(name)),
    ]),
  )
  write_slot(slot, boxed, v)
}

/// Port of emit.gleam:2075 emit_static_put. Yields `v` (assignment result).
/// D15: EvalEnv → runtime UnsupportedFeature throw.
pub fn emit_direct_put(
  d: scope.Direct,
  name: String,
  v: ir.Value,
) -> Build(ir.Value) {
  case d {
    scope.Local(origin_kind: scope.ConstBinding, ..) ->
      throw_at_rt("throw_type_error", "Assignment to constant '" <> name <> "'")
    scope.Local(origin_kind: scope.FnNameBinding, ..) -> {
      use e <- anf.then(ask)
      case e.strict {
        True ->
          throw_at_rt(
            "throw_type_error",
            "Assignment to constant '" <> name <> "'",
          )
        False -> anf.pure(v)
      }
    }
    scope.Local(kind: scope.CaptureBinding, slot:, boxed:, ..) ->
      write_slot_checked(slot, boxed, name, v)
    scope.Local(kind: scope.LetBinding, slot:, boxed:, ..) -> {
      use e <- anf.then(ask)
      case set.contains(e.initialized, slot) {
        True -> write_slot(slot, boxed, v)
        False -> write_slot_checked(slot, boxed, name, v)
      }
    }
    scope.Local(slot:, boxed:, ..) -> write_slot(slot, boxed, v)
    scope.Global(_) -> {
      use e <- anf.then(ask)
      case dict.get(e.slotted_globals, name) {
        Ok(slot) -> write_slot(slot, True, v)
        Error(Nil) -> {
          use _ <- anf.then(
            anf.host("global_set", [
              ir.ConstBinary(bit_array.from_string(name)),
              v,
            ]),
          )
          anf.pure(v)
        }
      }
    }
    scope.EvalEnv(_) ->
      throw_at_rt("throw_type_error", "unsupported: direct eval")
  }
}

/// Store `v` to identifier `name`. Port emit.gleam:1823 emit_var_put.
/// D15: WithChain resolutions surface as a runtime UnsupportedFeature throw.
pub fn emit_identifier_put(name: String, v: ir.Value) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case state.resolve(e, name) {
    scope.Plain(d) -> emit_direct_put(d, name, v)
    scope.WithChain(..) ->
      throw_at_rt("throw_type_error", "unsupported: with (" <> name <> ")")
  }
}

/// A resolved assignment target with its base/key operands already evaluated
/// and let-bound. D15: no with-chain variant. `own_key` carries the raw key
/// binary for a static non-private `.name` (own_data fast path); `elem_idx`
/// carries the RAW evaluated bracket expression for `o[e]` (indexed-element
/// fast path) — `key` is a dead placeholder when either is Some.
pub type LValue {
  LvIdent(name: String, direct: scope.Direct)
  LvMember(
    obj: ir.Value,
    key: ir.Value,
    is_private: Bool,
    own_key: Option(BitArray),
    elem_idx: Option(ir.Value),
    known_handle: Bool,
  )
  LvSuper(home: ir.Value, this: ir.Value, key: ir.Value)
}

/// Evaluate `target` down to an `LValue`, binding base/key sub-expressions
/// exactly once. Eval order §13.15.2: object before key (before RHS).
pub fn emit_lvalue(target: ast.Expression) -> Build(LValue) {
  case ast_util.unwrap_parens(target) {
    ast.Identifier(name:, ..) -> {
      use e <- anf.then(ask)
      case state.resolve(e, name) {
        scope.Plain(d) -> anf.pure(LvIdent(name, d))
        scope.WithChain(..) -> {
          use _ <- anf.then(throw_at_rt(
            "throw_type_error",
            "unsupported: with (" <> name <> ")",
          ))
          anf.pure(LvIdent(name, scope.Global(name)))
        }
      }
    }
    ast.MemberExpression(object: ast.SuperExpression(..), property:, ..) -> {
      use home <- anf.then(emit_lexical(lexical.RefHomeObject))
      use this <- anf.then(emit_lexical(lexical.RefThis))
      use key <- anf.then(emit_key_from_prop(property))
      anf.pure(LvSuper(home:, this:, key:))
    }
    ast.MemberExpression(object:, property:, ..) -> {
      use obj <- anf.then(expr(object))
      let own_key = static_dot_key(property)
      // Fast paths (own_key / elem_idx) never need the wire PropertyKey —
      // the miss arm builds it from the raw material. `key` stays a dead
      // placeholder when either is Some (matches the LValue doc contract).
      use #(key, elem_idx) <- anf.then(case own_key, property {
        Some(_), _ -> anf.pure(#(ir.ConstAtom("undefined"), None))
        None, ast.Bracket(expression:) -> {
          use idx <- anf.then(expr(expression))
          anf.pure(#(ir.ConstAtom("undefined"), Some(idx)))
        }
        None, _ -> {
          use k <- anf.then(emit_key_from_prop(property))
          anf.pure(#(k, None))
        }
      })
      anf.pure(LvMember(
        obj:,
        key:,
        is_private: is_private_prop(property),
        own_key:,
        elem_idx:,
        known_handle: is_known_handle(object),
      ))
    }
    // Parser rejects every other assignment target as an early error; per
    // R12 this arm still cannot panic — emit a diverging runtime throw and
    // hand back a dead dummy so the type stays total.
    _ -> {
      use _ <- anf.then(throw_at_rt(
        "throw_reference_error",
        "Invalid assignment target",
      ))
      anf.pure(LvIdent("", scope.Global("")))
    }
  }
}

/// Read the current value of `lv` (the "get" half of read-modify-write).
pub fn lvalue_get(lv: LValue) -> Build(ir.Value) {
  case lv {
    LvIdent(name:, direct:) -> emit_direct_get(direct, name)
    LvMember(obj:, key:, is_private: True, ..) ->
      anf.host("private_get", [obj, key])
    LvMember(obj:, is_private: False, own_key: Some(kb), known_handle:, ..) ->
      get_prop_fast(obj, kb, known_handle, {
        use key <- anf.then(named_key_from_binary(kb))
        anf.host("get_prop", [obj, key])
      })
    LvMember(obj:, is_private: False, elem_idx: Some(idx), ..) ->
      get_elem_fast(obj, idx, {
        use k <- anf.then(to_property_key(idx))
        anf.host("get_prop", [obj, k])
      })
    LvMember(obj:, key:, is_private: False, own_key: None, elem_idx: None, ..) ->
      anf.host("get_prop", [obj, key])
    LvSuper(home:, this:, key:) -> anf.host("super_get", [home, this, key])
  }
}

/// Write `v` back into `lv` (the "put" half). Yields `v` — the assignment
/// expression's own value.
pub fn lvalue_put(lv: LValue, v: ir.Value) -> Build(ir.Value) {
  case lv {
    LvIdent(name:, direct:) -> emit_direct_put(direct, name, v)
    LvMember(obj:, key:, is_private: True, ..) -> {
      use _ <- anf.then(anf.host("private_set", [obj, key, v]))
      anf.pure(v)
    }
    LvMember(obj:, is_private: False, own_key: Some(kb), known_handle:, ..) ->
      set_prop_fast(obj, kb, known_handle, v)
    LvMember(obj:, is_private: False, elem_idx: Some(idx), ..) ->
      set_elem_fast(obj, idx, v, {
        use k <- anf.then(to_property_key(idx))
        anf.host("set_prop", [obj, k, v])
      })
    LvMember(obj:, key:, is_private: False, own_key: None, elem_idx: None, ..) -> {
      use _ <- anf.then(anf.host("set_prop", [obj, key, v]))
      anf.pure(v)
    }
    LvSuper(home:, this:, key:) -> {
      use _ <- anf.then(anf.host("super_set", [home, this, key, v]))
      anf.pure(v)
    }
  }
}

// ── Array / Object literal helpers (u-array-object) ─────────────────────────
// Port of emit.gleam:5341-5563. SPEC §8 op names: new_array, new_object,
// spread_into_list, define_prop, define_method, copy_data_props.

/// Elision sentinel (`[1,,3]`). t_new_array recognises it and leaves the index
/// absent (sparse), matching emit.gleam's ArrayFromWithHoles path.
const js_hole: ir.Value = ir.ConstAtom("js_hole")

/// No-spread path (dense or with elisions): evaluate elements L-to-R, then
/// cons_list (in-order over already-evaluated values), then `new_array`.
fn emit_array_no_spread(
  elements: List(Option(ast.Expression)),
) -> Build(ir.Value) {
  use vs <- anf.then(
    anf.seq(
      list.map(elements, fn(el) {
        case el {
          Some(e) -> expr(e)
          None -> anf.pure(js_hole)
        }
      }),
    ),
  )
  use l <- anf.then(anf.cons_list(vs))
  anf.host("new_array", [l])
}

/// Spread path: L-to-R fold building the runtime cons list in-order.
/// `spread_into_list` (SPEC §8, arity 2) appends the iterable's elements;
/// singles/holes append via `list_append_one` — mirrors fold_args_spread.
fn emit_array_slow(elements: List(Option(ast.Expression))) -> Build(ir.Value) {
  use acc0 <- anf.then(anf.host("empty_list", []))
  use l <- anf.then(
    fold_build(elements, acc0, fn(acc, el) {
      case el {
        Some(ast.SpreadElement(_, arg)) -> {
          use it <- anf.then(expr(arg))
          anf.host("spread_into_list", [acc, it])
        }
        Some(e) -> {
          use v <- anf.then(expr(e))
          anf.host("list_append_one", [acc, v])
        }
        None -> anf.host("list_append_one", [acc, js_hole])
      }
    }),
  )
  anf.host("new_array", [l])
}

/// Emit one Property against `obj`; returns `obj` so the fold threads it.
/// Port of emit.gleam:5341 emit_object_property.
fn emit_object_property(obj: ir.Value, p: ast.Property) -> Build(ir.Value) {
  case p {
    // Annex B §B.3.1: non-computed non-shorthand `__proto__:` sets [[Prototype]]
    // instead of defining an own property. Must precede the generic Init arm.
    ast.InitProperty(
      key: ast.KeyIdentifier(name: "__proto__", ..),
      value:,
      shorthand: False,
    )
    | ast.InitProperty(
        key: ast.KeyString(value: "__proto__", ..),
        value:,
        shorthand: False,
      ) -> {
      use v <- anf.then(expr(value))
      use _ <- anf.then(anf.host("set_proto", [obj, v]))
      anf.pure(obj)
    }

    // Data property (covers shorthand — parser sets value=Identifier(name)).
    // NamedEvaluation: pass the static key string as `named` (§13.2.5.5).
    ast.InitProperty(key:, value:, shorthand: _) -> {
      use k <- anf.then(emit_key(key))
      use v <- anf.then(emit(value, ast.property_key_static_name(key)))
      use _ <- anf.then(anf.host("define_prop", [obj, k, v]))
      anf.pure(obj)
    }

    // Concise method — non-constructible, records [[HomeObject]]=obj (M4 op).
    ast.MethodProperty(key:, value:) -> {
      use k <- anf.then(emit_key(key))
      use f <- anf.then(emit_method_closure(
        value,
        ast.property_key_static_name(key),
      ))
      use _ <- anf.then(
        anf.host("define_method", [obj, k, f, ir.ConstAtom("m_i_method")]),
      )
      anf.pure(obj)
    }

    // get/set accessor — inferred name is `"get "|"set " <> key`.
    ast.AccessorProperty(key:, value:, kind:) -> {
      let #(prefix, tag) = accessor_kind(kind)
      let name =
        option.map(ast.property_key_static_name(key), fn(n) { prefix <> n })
      use k <- anf.then(emit_key(key))
      use f <- anf.then(emit_method_closure(value, name))
      use _ <- anf.then(anf.host("define_method", [obj, k, f, tag]))
      anf.pure(obj)
    }

    // {...src} — CopyDataProperties (own enumerable; nullish src is a no-op).
    ast.SpreadProperty(argument:) -> {
      use src <- anf.then(expr(argument))
      use _ <- anf.then(anf.host("copy_data_props", [obj, src]))
      anf.pure(obj)
    }
  }
}

fn accessor_kind(kind: ast.AccessorKind) -> #(String, ir.Value) {
  case kind {
    ast.GetAccessor -> #("get ", ir.ConstAtom("m_i_getter"))
    ast.SetAccessor -> #("set ", ir.ConstAtom("m_i_setter"))
  }
}

/// Compile a method/accessor FunctionLiteral to a closure value via
/// dispatch.emit_function, popping the analyzer's pre-assigned fn-scope id
/// (state.pop_child_fn — walk order matches scope.child_function_scopes).
fn emit_method_closure(
  lit: ast.FunctionLiteral,
  name: Option(String),
) -> Build(ir.Value) {
  let ast.FunctionLiteral(_, params, body, is_gen, is_async) = lit
  fn(e: Emitter2, k) {
    let #(fn_scope, e) = state.pop_child_fn(e)
    bridge_expr(fn(e) {
      e.dispatch.emit_function(
        e,
        state.Method(is_gen:, is_async:),
        name,
        params,
        state.StmtBody(body),
        fn_scope,
      )
    })(e, k)
  }
}

/// Monadic left-fold: thread each `step`'s result as the next accumulator.
fn fold_build(xs: List(a), acc: b, step: fn(b, a) -> Build(b)) -> Build(b) {
  case xs {
    [] -> anf.pure(acc)
    [x, ..rest] -> anf.then(step(acc, x), fold_build(rest, _, step))
  }
}

// ── u-delegate-dispatch: fn / arrow bridge (u-object-array covers methods) ──

/// Pop the analyzer-assigned child ScopeId, then bridge
/// `e.dispatch.emit_function` (Result(#(ir.Expr, Emitter2), _) — R14) into Build.
/// Shared by FunctionExpression / ArrowFunctionExpression case arms.
pub fn emit_function_expr(
  shape: state.FnShape,
  named: Option(String),
  params: List(ast.Pattern),
  body: state.FnBody,
) -> Build(ir.Value) {
  fn(e, k) {
    let #(fn_id, e) = state.pop_child_fn(e)
    bridge_expr(fn(e) {
      e.dispatch.emit_function(e, shape, named, params, body, fn_id)
    })(e, k)
  }
}

// ── u-object-array dispatch adapters ────────────────────────────────────────

/// §13.2.5 ObjectExpression. `named` stops here — properties carry their own
/// key-derived name (§13.2.5.5).
fn emit_object(
  properties: List(ast.Property),
  _named: Option(String),
) -> Build(ir.Value) {
  let plain_path = {
    use obj <- anf.then(anf.host("new_object", []))
    fold_build(properties, obj, emit_object_property)
  }
  use obj <- anf.then(case perf5_shape_obj_literals {
    False -> plain_path
    True ->
      case shapeable_literal_keys(properties, []) {
        // All-plain named-key data props with ≥2 keys: allocate an
        // SShapedObject directly so subsequent `.k` reads hit the shaped IC.
        // Single-key `{x:v}` stays on the SObject path — the shaped WRITE hit
        // rebuilds two tuples (`setelement(off,Slots,v)` + wrapper) per write;
        // the map-overlay write is one `maps:put`, and obj_prop measured
        // shaped 23k vs SObject-map 20k.
        Some(#([_, _, ..] as keys, vs)) -> {
          let site = ic_site_key("@ol")
          use vals <- anf.then(anf.seq(vs))
          use keys_l <- anf.then(anf.cons_list(list.map(keys, ir.ConstBinary)))
          use vals_l <- anf.then(anf.cons_list(vals))
          anf.host("new_object_shaped", [site, keys_l, vals_l])
        }
        _ -> plain_path
      }
  })
  // Result is always a fresh `{js_cell,_}` — mark so `.x` on a `let o={…}`
  // binding skips the receiver guard (obj_prop hot path).
  use _ <- anf.then(mark_handle(obj))
  anf.pure(obj)
}

/// Return `Some(#(keys, value_builds))` when every property is a plain
/// `InitProperty` with a static Named string key (no computed / index /
/// __proto__ / method / accessor / spread) AND all keys are distinct — i.e.
/// the literal maps 1-1 to an SShapedObject slot tuple. Empty `{}` is
/// excluded (nothing to fast-read; first add would devolve). `seen` dedups.
fn shapeable_literal_keys(
  ps: List(ast.Property),
  seen: List(BitArray),
) -> Option(#(List(BitArray), List(Build(ir.Value)))) {
  case ps {
    [] ->
      case seen {
        [] -> None
        _ -> Some(#([], []))
      }
    // Function-valued props (`{initialize: function(){…}}` — raytrace's
    // prototype literals) reject the WHOLE literal: a shaped proto sends
    // ic_proto_walk through shape-table own_property_of instead of maps:get
    // and regressed raytrace 287k→683k. Pure-data `{x:0,y:0}` still shapes.
    [ast.InitProperty(value: ast.FunctionExpression(..), ..), ..]
    | [ast.InitProperty(value: ast.ArrowFunctionExpression(..), ..), ..] -> None
    [ast.InitProperty(key:, value:, ..), ..rest] ->
      case shapeable_key(key) {
        None -> None
        Some(kb) ->
          case list.contains(seen, kb) {
            True -> None
            False ->
              case shapeable_literal_keys(rest, [kb, ..seen]) {
                None -> None
                Some(#(kbs, vs)) ->
                  Some(
                    #([kb, ..kbs], [
                      emit(value, ast.property_key_static_name(key)),
                      ..vs
                    ]),
                  )
              }
          }
      }
    _ -> None
  }
}

/// A shapeable literal key is a static string that canonicalizes to `Named`
/// (never `Index`) and is not `__proto__` (Annex B [[SetPrototypeOf]] arm).
fn shapeable_key(k: ast.PropertyKey) -> Option(BitArray) {
  case k {
    ast.KeyIdentifier(name: "__proto__", ..)
    | ast.KeyString(value: "__proto__", ..) -> None
    ast.KeyIdentifier(name:, ..) -> Some(bit_array.from_string(name))
    ast.KeyString(value: s, ..) ->
      case key.canonical_key(s) {
        key.Named(name) -> Some(bit_array.from_string(name))
        key.Index(..) | key.Private(..) -> None
      }
    ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..)
    | ast.KeyComputed(..) -> None
  }
}

/// §13.2.4 ArrayExpression. Port of emit.gleam:4865-4875.
fn emit_array(elements: List(Option(ast.Expression))) -> Build(ir.Value) {
  case ast_util.has_spread_element(elements) {
    False -> emit_array_no_spread(elements)
    True -> emit_array_slow(elements)
  }
}

// ── u-assign-update (port emit.gleam:4392-4634, 6987-7060) ──────────────────

fn compound_binop(op: ast.AssignmentOp) -> Option(ast.BinaryOp) {
  case op {
    ast.AddAssign -> Some(ast.Add)
    ast.SubtractAssign -> Some(ast.Subtract)
    ast.MultiplyAssign -> Some(ast.Multiply)
    ast.DivideAssign -> Some(ast.Divide)
    ast.ModuloAssign -> Some(ast.Modulo)
    ast.ExponentiationAssign -> Some(ast.Exponentiation)
    ast.LeftShiftAssign -> Some(ast.LeftShift)
    ast.RightShiftAssign -> Some(ast.RightShift)
    ast.UnsignedRightShiftAssign -> Some(ast.UnsignedRightShift)
    ast.BitwiseAndAssign -> Some(ast.BitwiseAnd)
    ast.BitwiseOrAssign -> Some(ast.BitwiseOr)
    ast.BitwiseXorAssign -> Some(ast.BitwiseXor)
    ast.Assign
    | ast.LogicalAndAssign
    | ast.LogicalOrAssign
    | ast.NullishCoalesceAssign -> None
  }
}

/// §13.4 UpdateExpression `++x` / `x--`. LValue evaluated once; ToNumeric on
/// the OLD value (so `o.x = "5"; o.x++` writes 6, yields 5).
fn emit_update(
  op: ast.UpdateOp,
  prefix: Bool,
  target: ast.Expression,
) -> Build(ir.Value) {
  case ast_util.unwrap_parens(target) {
    // Annex B web-compat: `f()++` evaluates the call then throws.
    ast.CallExpression(..) as call -> {
      use _ <- anf.then(expr(call))
      throw_at_rt(
        "throw_reference_error",
        "Invalid left-hand side expression in postfix operation",
      )
    }
    ast.Identifier(..) | ast.MemberExpression(..) -> {
      use lv <- anf.then(emit_lvalue(target))
      use old <- anf.then(lvalue_get(lv))
      use one <- anf.then(number_literal(ast.FiniteNumber(1.0)))
      let #(fast_op, bop) = case op {
        ast.Increment -> #(ir.NAdd, ast.Add)
        ast.Decrement -> #(ir.NSub, ast.Subtract)
      }
      use e <- anf.then(ask)
      case anf.is_known_number(e, old) {
        // Statically known BEAM number: ToNumeric is identity and number±1
        // stays a number — emit the M0 shape (no TermTest/If/tuple).
        True -> {
          use new <- anf.then(anf.bind_number(ir.NumTerm(fast_op, old, one)))
          use _ <- anf.then(lvalue_put(lv, new))
          case prefix {
            True -> anf.pure(new)
            False -> anf.pure(old)
          }
        }
        False -> {
          // ONE is_number test drives BOTH ToNumeric and the ±1 guard: fast
          // arm knows `old` is a BEAM number so NumTerm applies directly; slow
          // arm keeps full to_numeric + guarded_binop for str/obj/bigint.
          use is_num <- anf.then(anf.bind(ir.TermTest(ir.IsNumber, old)))
          use pair <- anf.then(anf.bind_if(
            is_num,
            anf.then(anf.bind_number(ir.NumTerm(fast_op, old, one)), fn(new) {
              anf.make_tuple([old, new])
            }),
            anf.then(anf.host("to_numeric", [old]), fn(old_n) {
              anf.then(binop(bop, old_n, one), fn(new) {
                anf.make_tuple([old_n, new])
              })
            }),
          ))
          use old_n <- anf.then(anf.bind(anf.tuple_get(pair, 0)))
          use new <- anf.then(anf.bind(anf.tuple_get(pair, 1)))
          use _ <- anf.then(lvalue_put(lv, new))
          case prefix {
            True -> anf.pure(new)
            False -> anf.pure(old_n)
          }
        }
      }
    }
    _ -> unreachable("UpdateExpression on non-simple target")
  }
}

/// §13.15.2 logical assignment `x &&= v` / `x ||= v` / `x ??= v` — the write
/// (and RHS evaluation) is guarded by the test.
fn emit_logical_assign(
  logical: ast.LogicalOp,
  lv: LValue,
  right: ast.Expression,
  inferred: Option(String),
) -> Build(ir.Value) {
  use old <- anf.then(lvalue_get(lv))
  let choose = fn(taken: Build(ir.Value)) {
    case logical {
      ast.LogicalAnd -> anf.truthy_if(old, taken, anf.pure(old))
      ast.LogicalOr -> anf.truthy_if(old, anf.pure(old), taken)
      ast.NullishCoalescing -> anf.nullish_if(old, taken, anf.pure(old))
    }
  }
  case lv {
    // Unboxed-local rebind: write_slot(_, False, _) does set_slot_var and emits
    // a Let INSIDE the If arm, but bind_if threads the mutated Emitter2 out to
    // the join — later read_slot would emit an out-of-scope Var. So compute the
    // join value with NO in-arm write, then rebind once at join scope. Writing
    // `old` back on short-circuit is unobservable for a plain SSA local, so
    // §13.15.2's guarded-PutValue is preserved. Const/FnName origins are
    // excluded — their put throws, which MUST stay guarded.
    LvIdent(_, scope.Local(boxed: False, origin_kind:, ..))
      if origin_kind != scope.ConstBinding && origin_kind != scope.FnNameBinding
    -> {
      use r <- anf.then(choose(emit(right, inferred)))
      lvalue_put(lv, r)
    }
    // Member / Super / Global / boxed-Local / Const / FnName: put is a host
    // call (or throw) with no set_slot_var, so keep the write inside the arm —
    // §13.15.2 requires the setter/PutValue not fire on short-circuit.
    _ -> choose(anf.then(emit(right, inferred), lvalue_put(lv, _)))
  }
}

/// §13.15.2 AssignmentExpression. Port of emit.gleam:4483-4634 collapsed onto
/// the LValue protocol (base/key evaluated once, before RHS).
fn emit_assignment(
  op: ast.AssignmentOp,
  left: ast.Expression,
  right: ast.Expression,
) -> Build(ir.Value) {
  // §13.15.2 step 1.c: NamedEvaluation only when LHS is an unparen'd Identifier.
  let inferred = case left {
    ast.Identifier(name: "*default*", ..) -> Some("default")
    ast.Identifier(name:, ..) -> Some(name)
    _ -> None
  }
  case ast_util.unwrap_parens(left) {
    // Annex B web-compat: `f() = v` evaluates call, then throws BEFORE RHS.
    ast.CallExpression(..) as call -> {
      use _ <- anf.then(expr(call))
      throw_at_rt(
        "throw_reference_error",
        "Invalid left-hand side in assignment",
      )
    }
    // Destructuring assignment `[a,b]=rhs` / `({x}=rhs)` (§13.15.5). Result of
    // the whole expression is rhs (§13.15.2 step 6). Pattern has no
    // MemberPattern, so this stays local (emit_destructuring_assign) rather
    // than routing through EmitDispatch.emit_destructure.
    ast.ArrayExpression(..) as pat | ast.ObjectExpression(..) as pat ->
      case op {
        ast.Assign -> {
          use rv <- anf.then(expr(right))
          use _ <- anf.then(emit_destructuring_assign(pat, rv))
          anf.pure(rv)
        }
        _ -> unreachable("compound-assign to destructuring pattern")
      }
    _ -> {
      use lv <- anf.then(emit_lvalue(left))
      case op {
        ast.Assign -> anf.then(emit(right, inferred), lvalue_put(lv, _))
        ast.LogicalAndAssign ->
          emit_logical_assign(ast.LogicalAnd, lv, right, inferred)
        ast.LogicalOrAssign ->
          emit_logical_assign(ast.LogicalOr, lv, right, inferred)
        ast.NullishCoalesceAssign ->
          emit_logical_assign(ast.NullishCoalescing, lv, right, inferred)
        _ ->
          case compound_binop(op) {
            Some(bop) -> {
              use old <- anf.then(lvalue_get(lv))
              use rv <- anf.then(expr(right))
              use nv <- anf.then(binop(bop, old, rv))
              lvalue_put(lv, nv)
            }
            None -> unreachable("compound-assign operator fallthrough")
          }
      }
    }
  }
}

/// Spec-named alias for `compound_binop` (M12 SPEC calls it `compound_to_binop`).
pub fn compound_to_binop(op: ast.AssignmentOp) -> Option(ast.BinaryOp) {
  compound_binop(op)
}

// ── §13.15.5 destructuring assignment (u-assign-update) ─────────────────────
// Port of emit.gleam:6294 emit_destructuring_assign. LHS is ast.Expression
// (Array/ObjectExpression), NOT ast.Pattern — Pattern has no MemberPattern so
// EmitDispatch.emit_destructure cannot express `[a.b] = v`. Stays local.

/// Assign `src` into destructuring-assignment target (§13.15.5).
pub fn emit_destructuring_assign(
  target: ast.Expression,
  src: ir.Value,
) -> Build(Nil) {
  case ast_util.unwrap_parens(target) {
    ast.Identifier(name:, ..) ->
      anf.then(emit_identifier_put(name, src), fn(_) { anf.pure(Nil) })
    // AssignmentElement with Initializer (§13.15.5.3): default fires when
    // src === undefined. Identifier-left gets NamedEvaluation on the default.
    ast.AssignmentExpression(_, ast.Assign, inner_left, default_expr) -> {
      use rc <- anf.then(consts())
      // §13.15.5.3 NamedEvaluation gate is IsIdentifierRef — false for a
      // ParenthesizedExpression, so match the RAW left (emit.gleam:6348).
      let named = case inner_left {
        ast.Identifier(name:, ..) -> Some(name)
        _ -> None
      }
      use is_undef <- anf.then(anf.host("strict_eq", [src, rc.undef]))
      use v <- anf.then(anf.bind_if(
        is_undef,
        emit(default_expr, named),
        anf.pure(src),
      ))
      emit_destructuring_assign(inner_left, v)
    }
    ast.ArrayExpression(_, elements) -> {
      use rc <- anf.then(consts())
      use iter <- anf.then(
        anf.host("get_iterator", [src, ir.ConstAtom("sync")]),
      )
      // PORT-GAP: §13.15.5.3 step 6 — a throwing element assignment must
      // IteratorClose(iter, abrupt). anf.gleam has no Try combinator yet;
      // SPEC row 1409's OnTag("js_exn") wrap lands with M13/M17.
      use drained <- anf.then(emit_array_assign_elements(elements, iter))
      // §13.15.5.2 step 7: IteratorClose only when the pattern didn't drain.
      case drained {
        True -> anf.pure(Nil)
        False -> anf.host_unit("iter_close", [iter, rc.false_])
      }
    }
    ast.ObjectExpression(_, properties) -> {
      // §13.15.5.2 step 1 RequireObjectCoercible before any read.
      use _ <- anf.then(anf.host("require_object_coercible", [src]))
      emit_object_assign_props(properties, src, [])
    }
    ast.MemberExpression(..) as m -> {
      use lv <- anf.then(emit_lvalue(m))
      anf.then(lvalue_put(lv, src), fn(_) { anf.pure(Nil) })
    }
    // Annex B for-in/of: `for (f() of it)` — call, then ReferenceError.
    ast.CallExpression(..) as call -> {
      use _ <- anf.then(expr(call))
      use _ <- anf.then(throw_at_rt(
        "throw_reference_error",
        "Invalid left-hand side in assignment",
      ))
      anf.pure(Nil)
    }
    // §13.15.5 early error: DestructuringAssignmentTargetType must be simple.
    _ -> {
      use _ <- anf.then(throw_at_rt(
        "throw_syntax_error",
        "Invalid destructuring assignment target",
      ))
      anf.pure(Nil)
    }
  }
}

/// Port of emit.gleam:6279 classify_assign_target's Static/Computed arms —
/// True for a non-super MemberExpression target (lref-first eval order).
fn is_member_target(target: ast.Expression) -> Bool {
  case ast_util.unwrap_parens(target) {
    ast.MemberExpression(object: ast.SuperExpression(..), ..) -> False
    ast.MemberExpression(..) -> True
    _ -> False
  }
}

/// SPEC §8 iter_next → #(done, value) pair; project value (tuple_get 1, R7).
fn iter_next_value(iter: ir.Value) -> Build(ir.Value) {
  use pair <- anf.then(anf.host("iter_next", [iter]))
  anf.bind(anf.tuple_get(pair, 1))
}

/// Array assignment pattern elements (§13.15.5.3). Returns whether the
/// iterator was fully drained (rest reached), so the caller skips iter_close.
fn emit_array_assign_elements(
  elements: List(Option(ast.Expression)),
  iter: ir.Value,
) -> Build(Bool) {
  case elements {
    [] -> anf.pure(False)
    [Some(ast.SpreadElement(_, argument)), ..] -> {
      use rest <- anf.then(anf.host("iter_rest", [iter]))
      use _ <- anf.then(emit_destructuring_assign(argument, rest))
      anf.pure(True)
    }
    [None, ..tail] -> {
      // Elision — step the iterator, discard the #(done, value) pair.
      use _ <- anf.then(anf.host("iter_next", [iter]))
      emit_array_assign_elements(tail, iter)
    }
    [Some(el), ..tail] ->
      case is_member_target(el) {
        // §13.15.5.3: for a MemberExpression target the lref (base + key)
        // is evaluated BEFORE IteratorStep — `[ {}[thrower()] ] = it` must
        // throw without calling .next() (emit.gleam:6427).
        True -> {
          use lv <- anf.then(emit_lvalue(el))
          use v <- anf.then(iter_next_value(iter))
          use _ <- anf.then(lvalue_put(lv, v))
          emit_array_assign_elements(tail, iter)
        }
        False -> {
          use v <- anf.then(iter_next_value(iter))
          use _ <- anf.then(emit_destructuring_assign(el, v))
          emit_array_assign_elements(tail, iter)
        }
      }
  }
}

/// ObjectAssignmentPattern properties (§13.15.5.2). `seen` accumulates
/// evaluated keys (reverse order) for the trailing rest's exclusion set.
fn emit_object_assign_props(
  props: List(ast.Property),
  src: ir.Value,
  seen: List(ir.Value),
) -> Build(Nil) {
  case props {
    [] -> anf.pure(Nil)
    [ast.SpreadProperty(argument), ..] -> {
      use excl <- anf.then(anf.cons_list(list.reverse(seen)))
      use rest <- anf.then(anf.host("copy_data_props", [src, excl]))
      emit_destructuring_assign(argument, rest)
    }
    [ast.InitProperty(key:, value:, ..), ..tail] -> {
      use k <- anf.then(emit_key(key))
      case is_member_target(value) {
        // §13.15.5.6 step 1a: lref evaluated BEFORE GetV — `({a: this.#f}=o)`
        // before super() TDZ-throws on `this` without touching o.a's getter
        // (privatefieldset-evaluation-order-1; emit.gleam:6616).
        True -> {
          use lv <- anf.then(emit_lvalue(value))
          use v <- anf.then(anf.host("get_prop", [src, k]))
          use _ <- anf.then(lvalue_put(lv, v))
          emit_object_assign_props(tail, src, [k, ..seen])
        }
        False -> {
          use v <- anf.then(anf.host("get_prop", [src, k]))
          use _ <- anf.then(emit_destructuring_assign(value, v))
          emit_object_assign_props(tail, src, [k, ..seen])
        }
      }
    }
    // Method/accessor properties never appear in valid assignment patterns.
    [ast.MethodProperty(..), ..] | [ast.AccessorProperty(..), ..] -> {
      use _ <- anf.then(throw_at_rt(
        "throw_syntax_error",
        "Invalid destructuring assignment target",
      ))
      anf.pure(Nil)
    }
  }
}
