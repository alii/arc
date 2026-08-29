import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope
import arc/parser
import arc/parser/ast
import arc_aot/emit/anf
import arc_aot/emit/async
import arc_aot/emit/class
import arc_aot/emit/destructure
import arc_aot/emit/exn
import arc_aot/emit/expr
import arc_aot/emit/func
import arc_aot/emit/state
import arc_aot/emit/stmt
import arc_aot/host_ops
import carder/ir
import carder/runtime/instance
import carder/runtime/profiles
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

pub type SourceKind {
  AsScript
  AsModule
}

pub type CompileOpts {
  CompileOpts(module_name: String, source_kind: SourceKind, entry_name: String)
}

pub type CompiledUnit {
  CompiledUnit(module: ir.Module, tree: scope.ScopeTree, is_strict: Bool)
}

pub type EmitError =
  state.EmitError

pub const js_exn_tag = exn.js_exn_tag

pub fn binding() -> instance.Binding {
  profiles.direct(host_ops.table())
}

fn init_emitter(
  tree: scope.ScopeTree,
  is_module: Bool,
  module_name: String,
) -> state.Emitter2 {
  let dispatch =
    state.EmitDispatch(
      emit_expr: expr.emit_expr,
      emit_expr_named: expr.emit_expr_named,
      emit_stmts: stmt.emit_stmts,
      emit_pattern: destructure.emit_pattern,
      emit_destructure: destructure.emit_pattern,
      emit_function: func.emit_function,
      emit_function_site: func.emit_function_site,
      emit_class: class.emit_class,
      emit_async_body: async.emit_coroutine_fn,
    )
  state.new_emitter(tree, scope.root_scope_id, is_module, module_name, dispatch)
}

fn root_binding_prologue(
  e: state.Emitter2,
) -> #(fn(ir.Expr) -> ir.Expr, state.Emitter2) {
  let bindings =
    dict.to_list(scope.get_scope(e.tree, scope.root_scope_id).bindings)
    |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
  let #(wrap, e) = root_lexical_prologue(e)
  list.fold(bindings, #(wrap, e), fn(acc, entry) {
    let #(wrap, e) = acc
    let #(name, b) = entry
    let sv = state.slot_var_name(e, b.slot)
    let e = state.set_slot_var(e, b.slot, sv)
    let e = case b.kind {
      scope.VarBinding ->
        state.set_slotted_globals(
          e,
          dict.insert(e.slotted_globals, name, b.slot),
        )
      _ -> e
    }
    let init = case b.kind {
      scope.VarBinding -> e.consts.undef
      _ -> e.consts.tdz
    }
    let wrap = case b.is_boxed {
      True -> fn(tail) {
        wrap(ir.Let([sv], ir.CallHost("js", "cell_new", [init]), tail))
      }
      False -> fn(tail) { wrap(ir.Let([sv], ir.Values([init]), tail)) }
    }
    #(wrap, e)
  })
}

// §16.1.7 steps 17-18
fn global_var_prologue(
  e: state.Emitter2,
  body: List(ast.StmtWithLine),
  strict: Bool,
  wrap: fn(ir.Expr) -> ir.Expr,
) -> #(fn(ir.Expr) -> ir.Expr, state.Emitter2) {
  let annexb = case strict {
    True -> []
    False -> state.fn_info(e).annexb_candidates
  }
  let vars =
    list.append(ast_util.collect_hoisted_vars(body), annexb)
    |> list.map(fn(name) { #(name, "declare_global_var") })
  let fns =
    ast_util.direct_fn_names(body)
    |> list.map(fn(name) { #(name, "declare_global_fn") })
  list.append(vars, fns)
  |> list.unique
  |> list.filter(fn(entry) {
    case state.resolve(e, entry.0) {
      scope.Plain(scope.Global(_)) -> True
      _ -> False
    }
  })
  |> list.fold(#(wrap, e), fn(acc, entry) {
    let #(wrap, e) = acc
    let #(name, op) = entry
    let #(t, e) = state.fresh_var(e)
    let #(kw, key, e) = anf.run_open(anf.key(name), e)
    let w = fn(tail) {
      wrap(
        kw(ir.Let(
          [t],
          ir.CallHost("js", op, [key, ir.ConstAtom("false")]),
          tail,
        )),
      )
    }
    #(w, e)
  })
}

fn root_lexical_prologue(
  e: state.Emitter2,
) -> #(fn(ir.Expr) -> ir.Expr, state.Emitter2) {
  let info = state.fn_info(e)
  let id = fn(t: ir.Expr) { t }
  case info.lexical {
    lexical.OwnedLexicalSlots(base:) ->
      list.fold(lexical.all_lexical_refs, #(id, e), fn(acc, ref) {
        let #(wrap, e) = acc
        let slot = base + lexical.lexical_ref_offset(ref)
        let sv = state.slot_var_name(e, slot)
        let e = state.set_slot_var(e, slot, sv)
        let init = case ref {
          lexical.RefThis -> ir.CallHost("js", "global_this", [])
          _ -> ir.Values([e.consts.undef])
        }
        let wrap = case state.lexical_is_boxed(e, info, ref) {
          True -> fn(tail) {
            wrap(ir.Let(
              [sv <> "_raw"],
              init,
              ir.Let(
                [sv],
                ir.CallHost("js", "cell_new", [ir.Var(sv <> "_raw")]),
                tail,
              ),
            ))
          }
          False -> fn(tail) { wrap(ir.Let([sv], init, tail)) }
        }
        #(wrap, e)
      })
    lexical.CapturedLexicalSlots(..) | lexical.NoLexicalSlots -> #(id, e)
  }
}

// §16.1.7 step 16, hoist top-level function declarations
pub fn emit_hoists(
  e: state.Emitter2,
  prog_body: List(ast.StmtWithLine),
) -> Result(#(fn(ir.Expr) -> ir.Expr, state.Emitter2), EmitError) {
  let id = fn(t: ir.Expr) { t }
  use #(wrap, e), located <- list.try_fold(prog_body, #(id, e))
  case ast_util.peel_labels(located.statement) {
    ast.FunctionDeclaration(
      name: Some(ast.NamedBinding(name:, ..)),
      params:,
      body:,
      is_generator:,
      is_async:,
    ) -> {
      let #(child_id, e) = state.pop_child_fn(e)
      use #(ctree, e) <- result.try(e.dispatch.emit_function(
        e,
        state.FnDecl(is_gen: is_generator, is_async:),
        Some(name),
        params,
        state.StmtBody(body),
        child_id,
      ))
      let #(fn_var, e) = state.fresh_var(e)
      let #(wrap, e) = case state.resolve(e, name) {
        scope.Plain(scope.Local(slot:, boxed: True, ..)) -> {
          let #(t, e) = state.fresh_var(e)
          let cell = ir.Var(state.get_slot_var(e, slot))
          let w = fn(tail) {
            wrap(ir.Let(
              [fn_var],
              ctree,
              ir.Let(
                [t],
                ir.CallHost("js", "cell_set", [cell, ir.Var(fn_var)]),
                tail,
              ),
            ))
          }
          #(w, e)
        }
        scope.Plain(scope.Local(slot:, boxed: False, ..)) -> {
          let #(t, e) = state.fresh_slot_var(e, slot)
          let e = state.set_slot_var(e, slot, t)
          let w = fn(tail) {
            wrap(ir.Let(
              [fn_var],
              ctree,
              ir.Let([t], ir.Values([ir.Var(fn_var)]), tail),
            ))
          }
          #(w, e)
        }
        _ -> {
          let #(t, e) = state.fresh_var(e)
          let #(kw, key, e) = anf.run_open(anf.key(name), e)
          let w = fn(tail) {
            wrap(ir.Let(
              [fn_var],
              ctree,
              kw(ir.Let(
                [t],
                ir.CallHost("js", "global_set", [key, ir.Var(fn_var)]),
                tail,
              )),
            ))
          }
          #(w, e)
        }
      }
      Ok(#(wrap, e))
    }
    _ -> Ok(#(wrap, e))
  }
}

// erlc is superlinear in function size, so js_main is chunked
const chunk_budget = 100

// beam caps a function at 255 args
const max_live = 250

fn emit_top_level(
  e: state.Emitter2,
  hoists: List(ast.StmtWithLine),
  stmts: List(ast.StmtWithLine),
  start: Int,
  n: Int,
) -> Result(#(ir.Expr, state.Emitter2), EmitError) {
  case hoists, stmts {
    [h, ..rest], _ -> {
      use #(w, e) <- result.try(emit_hoists(e, [h]))
      use #(tail, e) <- result.map(cut_or_continue(e, rest, stmts, start, n))
      #(w(tail), e)
    }
    [], [] -> Ok(#(ir.Return([e.consts.undef]), e))
    // §16.1.6 a trailing expression statement is the completion value
    [],
      [
        ast.StmtWithLine(
          statement: ast.ExpressionStatement(expression:, ..),
          ..,
        ),
      ]
    -> {
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, expression))
      use e, v <- state.let_(e, tree)
      Ok(#(ir.Return([v]), e))
    }
    [], [s, ..rest] ->
      e.dispatch.emit_stmts(e, [s], fn(ef) {
        cut_or_continue(ef, [], rest, start, n)
      })
  }
}

fn cut_or_continue(
  e: state.Emitter2,
  hoists: List(ast.StmtWithLine),
  stmts: List(ast.StmtWithLine),
  start: Int,
  n: Int,
) -> Result(#(ir.Expr, state.Emitter2), EmitError) {
  let done = hoists == [] && stmts == []
  let live = [
    state.keys_var,
    ..dict.values(e.slot_vars)
    |> list.unique
    |> list.sort(string.compare)
  ]
  let ready =
    e.next_var - start >= chunk_budget && !done && list.length(live) <= max_live
  case ready {
    False -> emit_top_level(e, hoists, stmts, start, n)
    True -> {
      let name = "js_main_" <> int.to_string(n + 1)
      use #(body, e) <- result.map(emit_top_level(
        e,
        hoists,
        stmts,
        e.next_var,
        n + 1,
      ))
      let chunk =
        ir.Function(
          name:,
          params: list.map(live, ir.Local(_, ir.TTerm)),
          result: [ir.TTerm],
          locals: [],
          body:,
        )
      #(
        ir.ReturnCall(name, list.map(live, ir.Var)),
        state.add_function(e, chunk),
      )
    }
  }
}

pub fn compile_source(
  source: String,
  opts: CompileOpts,
) -> Result(CompiledUnit, EmitError) {
  let is_strict = opts.source_kind == AsModule
  use #(body, sb) <- result.try(
    parser.parse_script(source)
    |> result.map_error(fn(e) {
      state.EarlySyntaxError(parser.parse_error_to_string(e))
    }),
  )
  let tree =
    scope.finalize(
      sb,
      scope.AnalyzeOpts(
        ..scope.default_analyze_opts(),
        strict: is_strict,
        top_lex: scope.LexLocal,
        // slot globals measured slower on richards, keep off
        module_slot_globals: False,
        box_try_writes: True,
      ),
    )
  use module <- result.map(compile(ast.Script(body:), tree, opts))
  CompiledUnit(module:, tree:, is_strict:)
}

pub fn compile(
  program: ast.Program,
  tree: scope.ScopeTree,
  opts: CompileOpts,
) -> Result(ir.Module, state.EmitError) {
  let body = case program {
    ast.Script(body:) -> Ok(body)
    ast.Module(..) ->
      Error(state.UnsupportedFeature("ESM module graph (SPEC Q7 v1)"))
  }
  use body <- result.try(body)
  let strict =
    opts.source_kind == AsModule || ast_util.has_use_strict_directive(body)
  let e = init_emitter(tree, strict, opts.module_name)
  let e = state.set_const_globals(e, expr.analyze_const_globals(body))
  let #(prologue, e) = root_binding_prologue(e)
  let #(prologue, e) = global_var_prologue(e, body, strict, prologue)
  use #(top_tree, ef) <- result.try(emit_top_level(e, body, body, e.next_var, 0))
  use Nil <- result.map(case list.reverse(ef.unsupported) {
    [feature, ..] -> Error(state.UnsupportedFeature(feature))
    [] -> Ok(Nil)
  })
  let names =
    ir.TermOp(
      ir.MakeTuple,
      list.map(state.name_texts(ef), fn(text) {
        ir.ConstBinary(bit_array.from_string(text))
      }),
    )
  let js_main =
    ir.Function(
      name: "js_main",
      params: [ir.Local("_frame", ir.TTerm), ir.Local("_args", ir.TTerm)],
      result: [ir.TTerm],
      locals: [],
      body: ir.Let(
        ["_names"],
        names,
        ir.Let(
          [state.keys_var],
          ir.CallHost("js", "keys_of", [ir.Var("_names")]),
          prologue(top_tree),
        ),
      ),
    )
  ir.Module(
    name: opts.module_name,
    uses_numerics: True,
    memories: [],
    globals: [],
    imports: [],
    functions: [js_main, ..state.take_functions(ef)],
    exports: [ir.ExportFn(opts.entry_name, "js_main")],
    data_segments: [],
    tables: [],
    elements: [],
    start: None,
    tags: [ir.TagDecl(js_exn_tag, [ir.TTerm])],
  )
}
