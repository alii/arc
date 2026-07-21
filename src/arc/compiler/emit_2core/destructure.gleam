//// M15: JS destructuring Pattern → twocore IR binding. Faithful port of
//// arc/compiler/emit.gleam:6038-6290 into the anf.Build monad style (per expr.gleam).
//// D2 (Arch-A): NO St threading — emit_core owns that. R12 Result channel.
//// SPEC row 1409/1410: array via get_iterator/iter_next/iter_rest/iter_close;
//// object via require_object_coercible/get_prop/copy_data_props.

import arc/compiler/emit_2core/anf.{type Build}
import arc/compiler/emit_2core/expr
import arc/compiler/emit_2core/state.{
  type BindMode, type EmitError, type Emitter2,
}
import arc/compiler/scope
import arc/parser/ast
import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import twocore/ir

// ── Entry point (EmitDispatch.emit_pattern / emit_destructure — SPEC:1562) ───

/// Bind (or assign, when mode = BindAssign) `pat` from evaluated `source`.
/// The returned Expr is the Let-chain of stores, terminating in Values([undef])
/// — callers let-bind and discard. Port emit.gleam:6038-6099.
pub fn emit_pattern(
  e: Emitter2,
  pat: ast.Pattern,
  source: ir.Value,
  mode: BindMode,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  Ok(
    anf.run_to(go(pat, source, mode), e, fn(ef, _) {
      ir.Values([ef.consts.undef])
    }),
  )
}

/// Recursive Build(Nil) dispatcher over ast.Pattern. `source` is already
/// evaluated and let-bound. Port emit.gleam:6038-6099 emit_destructuring_bind.
fn go(pat: ast.Pattern, source: ir.Value, mode: BindMode) -> Build(Nil) {
  case pat {
    ast.IdentifierPattern(name:, ..) -> bind_identifier(name, source, mode)
    ast.ArrayPattern(elements:) -> emit_array_pattern(elements, source, mode)
    ast.ObjectPattern(properties:) ->
      emit_object_pattern(properties, source, mode)
    ast.AssignmentPattern(left:, right: default_expr) -> {
      // §8.6.3 Initializer: evaluate default only when source === undefined.
      // R14 name-hint gap (matches stmt.gleam:598): dispatch.emit_expr has no
      // NamedEvaluation slot, so anonymous-fn defaults get name "" (accepted).
      use rc <- anf.then(expr.consts())
      use is_undef <- anf.then(anf.host("strict_eq", [source, rc.undef]))
      use v <- anf.then(anf.bind_if(
        is_undef,
        expr.bridge_expr(fn(e: Emitter2) {
          e.dispatch.emit_expr(e, default_expr)
        }),
        anf.pure(source),
      ))
      go(left, v, mode)
    }
    // Rest element outside its array/object context — arrives here only via
    // parameter-rest recursion (`function f(...x)` param slot). Identity bind.
    ast.RestElement(argument:) -> go(argument, source, mode)
  }
}

/// Store `v` into the identifier binding for `name` under `mode`. Port of
/// emit.gleam:6044-6066 (declare_lex/init_lex/emit_var_put) via the Build seam.
/// BindLet/BindConst mark the slot initialized (ends TDZ); BindVar/BindAssign
/// route through expr.emit_identifier_put (const-assign guard, TDZ check).
fn bind_identifier(name: String, v: ir.Value, mode: BindMode) -> Build(Nil) {
  case mode {
    state.BindAssign ->
      anf.then(expr.emit_identifier_put(name, v), fn(_) { anf.pure(Nil) })
    state.BindLet | state.BindConst | state.BindVar -> {
      use e <- anf.then(expr.ask)
      case state.resolve(e, name) {
        scope.Plain(scope.Local(slot:, boxed:, ..)) -> {
          let lexical = case mode {
            state.BindLet | state.BindConst -> True
            state.BindVar | state.BindAssign -> False
          }
          use _ <- anf.then(case lexical {
            True ->
              expr.modify(fn(e) {
                state.Emitter2(
                  ..e,
                  initialized: set.insert(e.initialized, slot),
                )
              })
            False -> anf.pure(Nil)
          })
          case boxed {
            True ->
              anf.host_unit("cell_set", [ir.Var(state.get_slot_var(e, slot)), v])
            False -> fn(e, k) {
              let #(n, e) = state.fresh_var(e)
              ir.Let(
                [n],
                ir.Values([v]),
                k(state.set_slot_var(e, slot, n), Nil),
              )
            }
          }
        }
        scope.Plain(scope.Global(_)) ->
          anf.host_unit("global_set", [
            ir.ConstBinary(bit_array.from_string(name)),
            v,
          ])
        scope.Plain(scope.EvalEnv(_)) ->
          anf.then(
            expr.throw_at_rt("throw_type_error", "unsupported: direct eval"),
            fn(_) { anf.pure(Nil) },
          )
        scope.WithChain(..) ->
          anf.then(
            expr.throw_at_rt(
              "throw_type_error",
              "unsupported: with (" <> name <> ")",
            ),
            fn(_) { anf.pure(Nil) },
          )
      }
    }
  }
}

// ── Array pattern (§8.6.2 / §13.15.5.2 — SPEC row 1409) ─────────────────────

/// Port emit.gleam:6823-6912 with_iterator_scaffold + emit_array_elements.
/// GetIterator once → per element left-to-right: elision steps and discards;
/// RestElement drains via iter_rest and marks drained; other patterns step,
/// project value (tuple_get 1, R7), and recurse. IteratorClose fires after
/// the loop only when the iterator was NOT drained by a rest.
/// PORT-GAP (matches expr.gleam:1537): §7.4.11 close-on-abrupt for a throwing
/// nested bind lands with the M17 Try combinator — no OnTag wrap here yet.
fn emit_array_pattern(
  elements: List(Option(ast.Pattern)),
  source: ir.Value,
  mode: BindMode,
) -> Build(Nil) {
  use rc <- anf.then(expr.consts())
  use iter <- anf.then(anf.host("get_iterator", [source, ir.ConstAtom("sync")]))
  use drained <- anf.then(emit_array_elements(elements, iter, mode))
  case drained {
    True -> anf.pure(Nil)
    False -> anf.host_unit("iter_close", [iter, rc.false_])
  }
}

/// Element loop. Returns whether a rest element drained the iterator (True →
/// caller skips iter_close). Port emit.gleam:6863-6912 emit_array_pattern_elements
/// + emit_array_elements collapsed — the 2core generic-el indirection is unneeded
/// (assignment-pattern arrays live in expr.emit_array_assign_elements).
fn emit_array_elements(
  elements: List(Option(ast.Pattern)),
  iter: ir.Value,
  mode: BindMode,
) -> Build(Bool) {
  case elements {
    [] -> anf.pure(False)
    // Elision — step iterator, discard #(done, value) pair.
    [None, ..rest] -> {
      use _ <- anf.then(anf.host("iter_next", [iter]))
      emit_array_elements(rest, iter, mode)
    }
    // Rest — [[Done]] becomes true the moment draining starts (§14.3.3.3),
    // so no iter_close on ANY completion after this. Recurse on the drained
    // array, then report drained=True. Trailing elements after rest are a
    // parser early error (§14.3.3 grammar), so `..` tail is dead here.
    [Some(ast.RestElement(argument:)), ..] -> {
      use arr <- anf.then(anf.host("iter_rest", [iter]))
      use _ <- anf.then(go(argument, arr, mode))
      anf.pure(True)
    }
    // Ordinary element — step, project value (SPEC §8 iter_next → #(done,
    // value); tuple_get 1 per R7), recurse, continue.
    [Some(p), ..rest] -> {
      use pair <- anf.then(anf.host("iter_next", [iter]))
      use v <- anf.then(anf.bind(anf.tuple_get(pair, 1)))
      use _ <- anf.then(go(p, v, mode))
      emit_array_elements(rest, iter, mode)
    }
  }
}

// ── Object pattern (§8.6.2 — SPEC row 1410) ─────────────────────────────────

/// Port emit.gleam:6135-6250 (emit_object_pattern + emit_single_object_prop +
/// emit_computed_key_prop). §8.6.2 step 1 RequireObjectCoercible BEFORE any
/// key is evaluated — `let {} = null` / `f({}){}; f(null)` throw TypeError even
/// on an empty pattern. `seen` accumulates evaluated keys (only when a rest is
/// present — emit.gleam:6180) so `copy_data_props` can exclude them; each key
/// is evaluated exactly ONCE (§13.15.5.2 step 2).
fn emit_object_pattern(
  properties: List(ast.PatternProperty),
  source: ir.Value,
  mode: BindMode,
) -> Build(Nil) {
  use _ <- anf.then(anf.host_unit("require_object_coercible", [source]))
  let has_rest =
    list.any(properties, fn(p) {
      case p {
        ast.RestProperty(..) -> True
        ast.PatternProperty(..) -> False
      }
    })
  emit_object_props(properties, source, mode, has_rest, [])
}

/// Left-to-right fold over properties. `seen` (reverse order) is threaded only
/// when `has_rest` (emit.gleam:6180-6187 — the stack machine stashed the key
/// beneath src; here it's just a Gleam list of already-bound ir.Values).
fn emit_object_props(
  props: List(ast.PatternProperty),
  source: ir.Value,
  mode: BindMode,
  has_rest: Bool,
  seen: List(ir.Value),
) -> Build(Nil) {
  case props {
    [] -> anf.pure(Nil)
    // {a, b, ...rest} — §13.15.5.3 RestBindingInitialization. Build the
    // exclusion list from every previously-evaluated key (source order), copy
    // remaining own-enumerable data properties into a fresh object, then bind
    // that to the rest identifier. RestProperty is always a plain identifier
    // (§13.3.3 grammar — no nested pattern). Trailing props after rest are a
    // parser early error, so `..` tail is dead here.
    [ast.RestProperty(name:, span:), ..] -> {
      use excl <- anf.then(anf.cons_list(list.reverse(seen)))
      use rest <- anf.then(anf.host("copy_data_props", [source, excl]))
      go(ast.IdentifierPattern(name:, span:), rest, mode)
    }
    // Static / numeric / bigint / computed key — expr.emit_key evaluates the
    // key ONCE (KeyComputed → emit expr then to_property_key) and returns the
    // canonical wire-tuple, so `k` is reused for both the get_prop and the
    // rest-exclusion set (single evaluation per §13.15.5.2 step 2, port of
    // emit.gleam:6223 GetElem2). KeyPrivate cannot appear here (§13.3.3).
    [ast.PatternProperty(key:, value:, ..), ..tail] -> {
      use k <- anf.then(expr.emit_key(key))
      use v <- anf.then(anf.host("get_prop", [source, k]))
      use _ <- anf.then(go(value, v, mode))
      let seen = case has_rest {
        True -> [k, ..seen]
        False -> seen
      }
      emit_object_props(tail, source, mode, has_rest, seen)
    }
  }
}
