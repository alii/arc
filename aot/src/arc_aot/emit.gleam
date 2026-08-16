//// arc AST + scope tree -> twocore IR module. Façade over emit/{state,anf}
//// and the M12-M18 emit_* passes; body is filled in by M19.

import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope
import arc/parser
import arc/parser/ast
import arc_aot/emit/async
import arc_aot/emit/class
import arc_aot/emit/destructure
import arc_aot/emit/exn
import arc_aot/emit/expr
import arc_aot/emit/func
import arc_aot/emit/state
import arc_aot/emit/stmt
import arc_aot/host_ops
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import twocore/ir
import twocore/runtime/instance
import twocore/runtime/profiles

// ── SPEC §19.1 façade types ────────────────────────────────────────────────

/// Goal symbol the source was parsed under. `AsModule` implies strict mode
/// and top-level `import`/`export` (v1: single-unit only, imports rejected).
pub type SourceKind {
  AsScript
  AsModule
}

/// Caller-supplied knobs for `compile_source` / `compile`.
pub type CompileOpts {
  CompileOpts(
    /// BEAM module name the emitted `ir.Module` carries.
    module_name: String,
    source_kind: SourceKind,
    /// Export name of the entry function (conventionally `"js_main"`).
    entry_name: String,
  )
}

/// Successful compile output: the IR module plus the finalized scope tree
/// (kept for diagnostics / the differential harness) and the strict flag.
pub type CompiledUnit {
  CompiledUnit(module: ir.Module, tree: scope.ScopeTree, is_strict: Bool)
}

/// Re-export so downstream (pipeline / harness) doesn't reach into `state`.
pub type EmitError =
  state.EmitError

/// R2: the single JS exception tag name. Source of truth is `exn.gleam`.
pub const js_exn_tag = exn.js_exn_tag

/// The 2core `Binding` every arc→BEAM compile uses: threaded state on the
/// portable tiers, no metering, and arc's own runtime as the `"js"` host.
pub fn binding() -> instance.Binding {
  profiles.direct(host_ops.table())
}

/// SPEC§19.2 / R13: wire the M12-M18 emit_* modules into the mutual-recursion
/// dispatch table, then build the initial Emitter2 rooted at the script scope.
/// `is_module` seeds strict mode (ESM top-level is always strict).
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
      emit_class: class.emit_class,
      emit_async_body: async.emit_coroutine_fn,
    )
  state.new_emitter(tree, scope.root_scope_id, is_module, module_name, dispatch)
}

/// Optimization G: seed every root-scope local binding (top-level `var` /
/// `function` when the tree was built with `module_slot_globals: True`, plus
/// top-level `let`/`const`) — Var → boxed cell holding undef, Let/Const → tdz.
/// Mirrors func.binding_prologue for the js_main frame, which had no prologue
/// before. Also builds the name→slot map for `slotted_globals`. Returns a
/// wrap so js_main's body is `let js_local_N = cell_new(undef) in …`.
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
    let sv = state.slot_var_name(b.slot)
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

/// §16.1.7 GlobalDeclarationInstantiation steps 17-18: every top-level
/// `var` name, hoisted function name and (sloppy) Annex B function-in-block
/// name that lives on the global object gets its `{undefined, W, E, C:false}`
/// binding before the body runs, so a read ahead of the declaration sees
/// `undefined` and eval code sees the binding as an existing var.
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
    let kb = ir.ConstBinary(bit_array.from_string(name))
    let w = fn(tail) {
      wrap(ir.Let([t], ir.CallHost("js", op, [kb, ir.ConstAtom("false")]), tail))
    }
    #(w, e)
  })
}

/// A Script root owns the four lexical pseudo-bindings (scope.gleam
/// `script_root_owns_lexical`): `this` is the global object (§9.1.1.4.11),
/// the other three are `undefined`. Mirrors func.unpack_frame for js_main,
/// whose `_frame` carries no caller context.
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
        let sv = state.slot_var_name(slot)
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

/// SPEC§19.5 step 3 — Script-top-level FunctionDeclaration hoisting
/// (GlobalDeclarationInstantiation §16.1.7 step 16). For each direct fn decl
/// in `prog_body`: pop its analyzer scope id, compile the closure via
/// `dispatch.emit_function`, then store into the name's slotted-global cell
/// (Optimization G) or the global object. Returns `#(wrap, e)` where `wrap`
/// nests `tail` in the hoist Let-bindings and `e` has `child_fn_cursor`
/// advanced past every fn-decl scope so the following `emit_stmts` — which
/// no-ops each FunctionDeclaration (stmt.gleam:356) — pops only fn-expr
/// scopes in source order.
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
          // Unboxed root fn slot (no nested fn captures it): fresh Let-
          // rebind + set_slot_var so later top-level reads see the closure.
          let #(t, e) = state.fresh_var(e)
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
          let kb = ir.ConstBinary(bit_array.from_string(name))
          let w = fn(tail) {
            wrap(ir.Let(
              [fn_var],
              ctree,
              ir.Let(
                [t],
                ir.CallHost("js", "global_set", [kb, ir.Var(fn_var)]),
                tail,
              ),
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

/// Parse `source`, finalize its scope tree, and lower to an `ir.Module`.
/// Thin wrapper over `compile` for callers holding raw source text.
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
        // Optimization G DISABLED (g-cell-get-regress): boxed-cell reads route
        // through rt_store.t_cell_get (JsStore Dict lookup, 62-76ns).
        // richards baseline had only 65/run global_get_fast (const-globals
        // already inline the 41k), so G traded 55 cheap reads for +40,910
        // cell_get/run ≈ +2.9ms. Re-enable only for a bench where profile
        // shows >1k/run t_global_get_fast.
        module_slot_globals: False,
        box_try_writes: True,
      ),
    )
  use module <- result.map(compile(ast.Script(body:), tree, opts))
  CompiledUnit(module:, tree:, is_strict:)
}

/// SPEC§19.5 core driver: lower a parsed Program (with its finalized scope
/// tree) into a flat `ir.Module` whose entry function is `js_main`.
pub fn compile(
  program: ast.Program,
  tree: scope.ScopeTree,
  opts: CompileOpts,
) -> Result(ir.Module, state.EmitError) {
  let body = case program {
    ast.Script(body:) -> Ok(body)
    // Q7: v1 is single-unit only; ImportDeclaration → Unsupported.
    ast.Module(..) ->
      Error(state.UnsupportedFeature("ESM module graph (SPEC Q7 v1)"))
  }
  use body <- result.try(body)
  // (1) init emitter at root; strict iff Module goal or a top-level
  // "use strict" directive (§11.2.2 Directive Prologue). Nested functions
  // inherit it or scan their own prologue in func.emit_function.
  let strict =
    opts.source_kind == AsModule || ast_util.has_use_strict_directive(body)
  let e = init_emitter(tree, strict, opts.module_name)
  let e = state.set_const_globals(e, expr.analyze_const_globals(body))
  // Optimization G: allocate boxed cells for every root-scope Local binding
  // (top-level `var`/`function` under module_slot_globals) BEFORE hoists so
  // fn-decl closures cell_set into a live cell. Also seeds slotted_globals.
  let #(prologue, e) = root_binding_prologue(e)
  let #(prologue, e) = global_var_prologue(e, body, strict, prologue)
  // (2) hoist top-level FunctionDeclarations (§16.1.7 step 16).
  use #(wrap, e) <- result.try(emit_hoists(e, body))
  // (3)+(4) statement fold; terminal K is Return(undef). The runner drains
  // microtasks and runs the GC safepoint after js_main returns.
  let terminal = fn(ef: state.Emitter2) {
    Ok(#(ir.Return([ef.consts.undef]), ef))
  }
  use #(stmts_tree, ef) <- result.try(e.dispatch.emit_stmts(e, body, terminal))
  use Nil <- result.map(case list.reverse(ef.unsupported) {
    [feature, ..] -> Error(state.UnsupportedFeature(feature))
    [] -> Ok(Nil)
  })
  // (5) js_main params match the R7 call ABI (frame 4-tuple, args list).
  let js_main =
    ir.Function(
      name: "js_main",
      params: [ir.Local("_frame", ir.TTerm), ir.Local("_args", ir.TTerm)],
      result: [ir.TTerm],
      locals: [],
      body: prologue(wrap(stmts_tree)),
    )
  // (6) all 12 ir.Module fields. Invariant: js_main is NOT in fns_acc; tags
  // has exactly one entry (R2 — every Throw/CatchTag names js_exn_tag).
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
