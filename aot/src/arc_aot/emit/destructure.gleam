import arc/compiler/scope
import arc/parser/ast
import arc_aot/emit/anf.{type Build}
import arc_aot/emit/expr
import arc_aot/emit/state.{type BindMode, type EmitError, type Emitter2}
import carder/ir
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

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

fn go(pat: ast.Pattern, source: ir.Value, mode: BindMode) -> Build(Nil) {
  case pat {
    ast.IdentifierPattern(name:, ..) -> bind_identifier(name, source, mode)
    ast.ArrayPattern(elements:) -> emit_array_pattern(elements, source, mode)
    ast.ObjectPattern(properties:) ->
      emit_object_pattern(properties, source, mode)
    ast.AssignmentPattern(left:, right: default_expr) -> {
      let named = case left {
        ast.IdentifierPattern(name:, ..) -> Some(name)
        _ -> None
      }
      use rc <- anf.then(expr.consts())
      use is_undef <- anf.then(anf.bind(ir.NumTerm(ir.NEq, source, rc.undef)))
      use v <- anf.then(anf.bind_if(
        is_undef,
        expr.bridge_expr(fn(e: Emitter2) {
          e.dispatch.emit_expr_named(e, default_expr, named)
        }),
        anf.pure(source),
      ))
      go(left, v, mode)
    }
    // bare rest only reaches here as a rest parameter
    ast.RestElement(argument:) -> go(argument, source, mode)
  }
}

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
              let #(n, e) = state.fresh_slot_var(e, slot)
              anf.wrap(k(state.set_slot_var(e, slot, n), Nil), ir.Let(
                [n],
                ir.Values([v]),
                _,
              ))
            }
          }
        }
        scope.Plain(scope.Global(_)) -> {
          use k <- anf.then(anf.key(name))
          anf.host_unit("global_set", [k, v])
        }
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

// §8.6.2 close the iterator on abrupt unless a rest drained it
fn emit_array_pattern(
  elements: List(Option(ast.Pattern)),
  source: ir.Value,
  mode: BindMode,
) -> Build(Nil) {
  use rc <- anf.then(expr.consts())
  use iter <- anf.then(anf.host("get_iterator", [source, ir.ConstAtom("sync")]))
  let drained =
    list.any(elements, fn(el) {
      case el {
        Some(ast.RestElement(..)) -> True
        _ -> False
      }
    })
  use _ <- anf.then(
    anf.close_iter_on_throw(iter, {
      use _ <- anf.then(emit_array_elements(elements, iter, mode))
      anf.pure(Nil)
    }),
  )
  case drained {
    True -> anf.pure(Nil)
    False -> anf.host_unit("iter_close", [iter, rc.false_])
  }
}

fn emit_array_elements(
  elements: List(Option(ast.Pattern)),
  iter: ir.Value,
  mode: BindMode,
) -> Build(Bool) {
  case elements {
    [] -> anf.pure(False)
    [None, ..rest] -> {
      use _ <- anf.then(anf.host("iter_next", [iter]))
      emit_array_elements(rest, iter, mode)
    }
    // done is true once draining starts, so no close after this
    [Some(ast.RestElement(argument:)), ..] -> {
      use arr <- anf.then(anf.host("iter_rest", [iter]))
      use _ <- anf.then(go(argument, arr, mode))
      anf.pure(True)
    }
    [Some(p), ..rest] -> {
      use pair <- anf.then(anf.host("iter_next", [iter]))
      use v <- anf.then(anf.bind(anf.tuple_get(pair, 1)))
      use _ <- anf.then(go(p, v, mode))
      emit_array_elements(rest, iter, mode)
    }
  }
}

// §8.6.2 requireobjectcoercible first, then each key evaluated once
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

fn emit_object_props(
  props: List(ast.PatternProperty),
  source: ir.Value,
  mode: BindMode,
  has_rest: Bool,
  seen: List(ir.Value),
) -> Build(Nil) {
  case props {
    [] -> anf.pure(Nil)
    [ast.RestProperty(name:, span:), ..] -> {
      use excl <- anf.then(anf.cons_list(list.reverse(seen)))
      use rest <- anf.then(anf.host("object_rest", [source, excl]))
      go(ast.IdentifierPattern(name:, span:), rest, mode)
    }
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
