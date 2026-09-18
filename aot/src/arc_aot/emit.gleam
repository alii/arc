import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope
import arc/parser
import arc/parser/ast
import arc_aot/emit/async
import arc_aot/emit/class
import arc_aot/emit/destructure
import arc_aot/emit/expr
import arc_aot/emit/func
import arc_aot/emit/split
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
  CompileOpts(module_name: String, source_kind: SourceKind)
}

pub fn binding() -> instance.Binding {
  profiles.direct(host_ops.table())
}

fn init_emitter(
  tree: scope.ScopeTree,
  strict strict: Bool,
  module_name module_name: String,
) -> state.Emitter {
  let dispatch =
    state.EmitDispatch(
      emit_expr: expr.emit_expr,
      emit_expr_named: expr.emit_named,
      emit_stmts: stmt.emit_stmts,
      emit_destructure: destructure.emit_pattern,
      emit_function: func.emit_function,
      emit_function_callable: func.emit_function_callable,
      emit_class: class.emit,
      emit_coroutine_fn: async.emit_coroutine_fn,
    )
  state.new_emitter(tree, scope.root_scope_id, strict, module_name, dispatch)
}

fn root_binding_prologue(
  e: state.Emitter,
) -> #(fn(ir.Expr) -> ir.Expr, state.Emitter) {
  let bindings =
    dict.to_list(scope.get(e.scope_tree, scope.root_scope_id).bindings)
    |> list.sort(fn(a, b) { int.compare({ a.1 }.slot, { b.1 }.slot) })
  let #(wrap, e) = root_lexical_prologue(e)
  list.fold(bindings, #(wrap, e), fn(acc, entry) {
    let #(wrap, e) = acc
    let #(name, b) = entry
    let sv = state.slot_base_name(e, b.slot)
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
    let wrap = case b.boxed {
      True -> fn(tail) {
        wrap(ir.Let([sv], ir.CallHost("js", "box_new", [init]), tail))
      }
      False -> fn(tail) { wrap(ir.Let([sv], ir.Values([init]), tail)) }
    }
    #(wrap, e)
  })
}

// §16.1.7 steps 17-18
fn global_var_prologue(
  e: state.Emitter,
  body: List(ast.StmtWithLine),
  strict strict: Bool,
  wrap wrap: fn(ir.Expr) -> ir.Expr,
) -> #(fn(ir.Expr) -> ir.Expr, state.Emitter) {
  let annexb = case strict {
    True -> []
    False -> state.fn_info(e).annexb_candidates
  }
  let vars =
    list.append(ast_util.var_declared_names(body), annexb)
    |> list.map(fn(name) { #(name, "create_global_var_binding") })
  let fns =
    ast_util.top_level_function_names(body)
    |> list.map(fn(name) { #(name, "create_global_fn_binding") })
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
    let kb = ir.ConstBinary(bit_array.from_string(name))
    let w = fn(tail) {
      wrap(ir.Let([t], ir.CallHost("js", op, [kb, ir.ConstAtom("false")]), tail))
    }
    #(w, e)
  })
}

fn root_lexical_prologue(
  e: state.Emitter,
) -> #(fn(ir.Expr) -> ir.Expr, state.Emitter) {
  let info = state.fn_info(e)
  let id = fn(t: ir.Expr) { t }
  case info.lexical {
    lexical.OwnedLexicalSlots(base:) ->
      list.fold(lexical.all_lexical_refs, #(id, e), fn(acc, ref) {
        let #(wrap, e) = acc
        let slot = base + lexical.ref_offset(ref)
        let sv = state.slot_base_name(e, slot)
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
                ir.CallHost("js", "box_new", [ir.Var(sv <> "_raw")]),
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
fn emit_hoist(
  e: state.Emitter,
  located: ast.StmtWithLine,
) -> Result(#(fn(ir.Expr) -> ir.Expr, state.Emitter), state.EmitError) {
  case ast_util.peel_labels(located.statement) {
    ast.FunctionDeclaration(
      name: Some(ast.NamedBinding(name:, ..)),
      params:,
      body:,
      is_generator:,
      is_async:,
    ) -> {
      let #(child_id, e) = state.pop_child_fn(e)
      use #(ctree, e) <- result.map(e.dispatch.emit_function(
        e,
        state.FnDecl(is_gen: is_generator, is_async:),
        Some(name),
        params,
        state.StmtBody(body),
        child_id,
      ))
      let #(fn_var, e) = state.fresh_var(e)
      let #(t, store, e) = case state.resolve(e, name) {
        scope.Plain(scope.Local(slot:, boxed: True, ..)) -> {
          let #(t, e) = state.fresh_var(e)
          let box = ir.Var(state.get_slot_var(e, slot))
          #(t, ir.CallHost("js", "box_set", [box, ir.Var(fn_var)]), e)
        }
        scope.Plain(scope.Local(slot:, boxed: False, ..)) -> {
          let #(t, e) = state.fresh_slot_var(e, slot)
          #(t, ir.Values([ir.Var(fn_var)]), state.set_slot_var(e, slot, t))
        }
        _ -> {
          let #(t, e) = state.fresh_var(e)
          let kb = ir.ConstBinary(bit_array.from_string(name))
          #(t, ir.CallHost("js", "global_set", [kb, ir.Var(fn_var)]), e)
        }
      }
      #(fn(tail) { ir.Let([fn_var], ctree, ir.Let([t], store, tail)) }, e)
    }
    _ -> Ok(#(fn(tail) { tail }, e))
  }
}

// erlc is superlinear in function size, so js_main is chunked
const chunk_budget = 100

fn emit_top_level(
  e: state.Emitter,
  hoists: List(ast.StmtWithLine),
  stmts: List(ast.StmtWithLine),
  chunk_first_var: Int,
  chunk_index: Int,
) -> state.EmitResult {
  case hoists, stmts {
    [h, ..rest], _ -> {
      use #(w, e) <- result.try(emit_hoist(e, h))
      use #(tail, e) <- result.map(cut_or_continue(
        e,
        rest,
        stmts,
        chunk_first_var,
        chunk_index,
      ))
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
        cut_or_continue(ef, [], rest, chunk_first_var, chunk_index)
      })
  }
}

fn cut_or_continue(
  e: state.Emitter,
  hoists: List(ast.StmtWithLine),
  stmts: List(ast.StmtWithLine),
  chunk_first_var: Int,
  chunk_index: Int,
) -> state.EmitResult {
  let done = hoists == [] && stmts == []
  let live =
    dict.values(e.slot_vars) |> list.unique |> list.sort(string.compare)
  let should_cut =
    e.next_var - chunk_first_var >= chunk_budget
    && !done
    && list.length(live) <= split.max_live_params
  case should_cut {
    False -> emit_top_level(e, hoists, stmts, chunk_first_var, chunk_index)
    True -> {
      let name = "js_main_" <> int.to_string(chunk_index + 1)
      use #(body, e) <- result.map(emit_top_level(
        e,
        hoists,
        stmts,
        e.next_var,
        chunk_index + 1,
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
) -> Result(ir.Module, state.EmitError) {
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
  compile(ast.Script(body:), tree, opts)
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
  let js_main =
    ir.Function(
      name: "js_main",
      params: [
        ir.Local(func.frame_param, ir.TTerm),
        ir.Local(func.args_param, ir.TTerm),
      ],
      result: [ir.TTerm],
      locals: [],
      body: prologue(top_tree),
    )
  ir.Module(
    name: opts.module_name,
    uses_numerics: True,
    memories: [],
    globals: [],
    imports: [],
    functions: [js_main, ..state.take_functions(ef)],
    exports: [ir.ExportFn("js_main", "js_main")],
    data_segments: [],
    tables: [],
    elements: [],
    start: None,
    tags: [ir.TagDecl(ef.consts.exn_tag, [ir.TTerm])],
  )
}
