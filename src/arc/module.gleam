//// ES Module system for Arc.
////
//// Two-phase module lifecycle:
////   1. compile_bundle: parse + compile all modules ahead of time into a
////      ModuleBundle (a pure term, serializable via term_to_binary);
////   2. link + evaluate the bundle against an `Agent` (no parser, no I/O).
////
//// Based on ECMAScript §16.2 and QuickJS's module implementation.

import arc/compiler.{type ExportSeed}
import arc/esm
import arc/internal/tuple_array
import arc/interp/entry
import arc/interp/interpreter
import arc/interp/safepoint
import arc/interp/state.{type State, State}
import arc/link
import arc/module/graph
import arc/module/load_error.{type LoadError, type ResolveError}
import arc/module/registry
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins/reflect as rt_reflect
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/call as rt_call
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type CompiledFn, type Handle, type JsVal, type ReflectNative,
  DataProperty, FnFlags, KHandle, KStr, KTdz, ModuleNamespace, NoElements,
  PromiseFulfilled, PromisePending, PromiseRejected, ProxyObj,
  ReflectDefineProperty, ReflectDeleteProperty, ReflectGet,
  ReflectGetOwnPropertyDescriptor, ReflectHas, ReflectOwnKeys, SAsyncContext,
  SBox, SObject, SPromiseData, StepAwait, StepReturn, StepThrow, StepYield,
  StringKey, SyntaxErr, TypeErr, classify, mk_object, mk_string, mk_tdz,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

// =============================================================================
// Compiled Module Types
// =============================================================================

/// A single compiled module: everything known at compile time. No AST, no
/// source code, no runtime state.
///
/// `specifier` and every `requested_modules` entry are module identities
/// (`esm.Resolved`), while `import_bindings` is keyed by the specifier text
/// this module's SOURCE wrote (`esm.Raw`), which only `esm.resolve` through
/// `specifier_map` may turn into an identity. `ModuleBundle`'s dict keys stay
/// plain `String` (the embedder-facing API), so identities are untagged with
/// `esm.resolved_text` exactly where they index a bundle / `Linked` dict.
pub type CompiledModule {
  CompiledModule(
    specifier: esm.Resolved,
    template: FuncTemplate,
    /// (raw specifier as written, bindings): resolve through `specifier_map`.
    import_bindings: List(#(esm.Raw, List(esm.ImportBinding))),
    export_entries: List(esm.ExportEntry),
    /// Module-root name → local-slot map: the linker looks exported local
    /// names up in it to find the slot whose cell importers share.
    export_names: Dict(String, Int),
    /// This module's TOTAL raw → resolved projection.
    specifier_map: esm.SpecifierMap,
    /// [[RequestedModules]] in declaration order, each with its merged phase
    /// (`Deferred` only when EVERY reference is `import defer * as ns`).
    /// InnerModuleEvaluation (§16.2.1.5.3.1) walks this list and skips the
    /// `Deferred` entries.
    requested_modules: List(#(esm.Resolved, esm.Phase)),
    /// Exported local name → how the linker seeds its cell before the body
    /// runs (§16.2 instantiation): `undefined` for var/function, TDZ for
    /// let/const/class/default.
    export_seeds: Dict(String, ExportSeed),
    /// Top-level hoisted function declarations as (name, func_index) into
    /// template.functions. The linker instantiates the *exported* ones before
    /// any body runs, so cyclic function imports are callable (§16.2.1.6.4).
    hoisted_funcs: List(#(String, Int)),
    /// [[HasTLA]] (§16.2.1.5): the body contains a top-level `await`.
    has_tla: Bool,
  )
}

/// A host (synthetic) module: a module whose named exports are
/// embedder-provided values rather than compiled from JS source. Modelled on
/// the TC39 Synthetic Module Record: no dependencies, ready exports, a no-op
/// `[[Evaluate]]`.
pub type HostModule {
  HostModule(specifier: String, exports: List(#(String, JsVal)))
}

/// One entry of a `ModuleBundle`: EITHER compiled from JS source OR an
/// embedder-provided host (synthetic) module.
pub type BundleModule {
  SourceModule(compiled: CompiledModule)
  SyntheticModule(host: HostModule)
}

/// A complete compiled module graph, keyed by resolved specifier.
pub type ModuleBundle {
  ModuleBundle(entry: String, modules: Dict(String, BundleModule))
}

/// Run `k` on the compiled module a bundle entry holds; a host module has no
/// source, imports or body, so every source-only pass keeps `default`.
fn with_source_module(
  bundle_module: BundleModule,
  default: a,
  k: fn(CompiledModule) -> a,
) -> a {
  case bundle_module {
    SourceModule(compiled) -> k(compiled)
    SyntheticModule(_) -> default
  }
}

/// The specifiers of the bundle's SOURCE modules only: the ones with a body
/// to run, so the only ones an evaluation can leave half-done.
pub fn source_specifiers(bundle: ModuleBundle) -> List(String) {
  use acc, spec, bundle_module <- dict.fold(bundle.modules, [])
  use _compiled <- with_source_module(bundle_module, acc)
  [spec, ..acc]
}

// =============================================================================
// Errors
// =============================================================================

/// A failure of AOT compilation (`compile_bundle*`): everything that can go
/// wrong BEFORE any heap is involved.
pub type CompileBundleError {
  /// The source-graph walk failed: parse, resolve, load, or a source-phase
  /// import. Match the inner `graph.GraphError` to tell those apart.
  GraphError(error: graph.GraphError)
  /// Bytecode compilation of the module named `specifier` failed.
  CompileError(specifier: String, error: compiler.CompileError)
}

/// A failure of the LINK/EVALUATE half of the pipeline. Link-time validation
/// failures (§16.2.1.6.4) allocate their JS SyntaxError right there, so they
/// surface as `EvaluationError`. Every function that can fail hands the live
/// `Agent` back BESIDE the result.
pub type ModuleError {
  /// Evaluation asked for a resolved specifier the bundle does not contain.
  NotInBundle(specifier: String)
  /// A module threw during evaluation (or failed to link).
  EvaluationError(value: JsVal)
  /// Evaluation is parked on top-level await and the supplied `finish`
  /// driver did not settle it (the dynamic-import path). `promise` is the
  /// entry module's [[TopLevelCapability]] promise: per Evaluate() step 4 the
  /// host chains onto it rather than treating the module as failed.
  EvaluationPending(promise: Handle)
}

/// The message a module parked forever on top-level await surfaces with (cf.
/// Node's exit code 13).
const tla_never_settled_message = "module evaluation never completed: top-level await promise never settled"

/// The single renderer of a `CompileBundleError`'s detail prose.
pub fn compile_bundle_error_message(err: CompileBundleError) -> String {
  case err {
    GraphError(error: graph.ParseFailed(specifier, parse_error)) ->
      "SyntaxError in '"
      <> esm.resolved_text(specifier)
      <> "': "
      <> parser.parse_error_to_string(parse_error)
    GraphError(error: graph.ResolveFailed(raw, referrer, error)) ->
      load_error.resolve_failure_message(
        esm.raw_text(raw),
        esm.resolved_text(referrer),
        error,
      )
    GraphError(error: graph.LoadFailed(specifier, error)) ->
      load_error.load_failure_message(esm.resolved_text(specifier), error)
    GraphError(error: graph.SourcePhaseUnsupported(specifier)) ->
      "'"
      <> esm.resolved_text(specifier)
      <> "': source phase imports ('import source') are not supported"
    CompileError(specifier:, error:) ->
      compiler.error_message(error) <> " in '" <> specifier <> "'"
  }
}

/// The FULL user-facing text for a `CompileBundleError`: phase label plus
/// detail. A `ParseFailed`'s detail already opens with "SyntaxError in".
pub fn format_compile_bundle_error(err: CompileBundleError) -> String {
  let phase = case err {
    GraphError(error: graph.ParseFailed(..)) -> ""
    GraphError(error: graph.ResolveFailed(..))
    | GraphError(error: graph.LoadFailed(..)) -> "ResolutionError: "
    // A source-phase import is a link-time SyntaxError (§16.2.1.7.2).
    GraphError(error: graph.SourcePhaseUnsupported(..)) -> "LinkError: "
    CompileError(..) -> "CompileError: "
  }
  phase <> compile_bundle_error_message(err)
}

/// Which pipeline PHASE a `ModuleError` belongs to, as the prefix embedders
/// print in front of `error_message`.
pub fn module_error_phase(err: ModuleError) -> String {
  case err {
    NotInBundle(..) -> "ResolutionError: "
    EvaluationError(..) | EvaluationPending(..) -> ""
  }
}

/// The single renderer of a `ModuleError`'s user-facing prose. `st` is the
/// agent the failing call handed back beside the error.
pub fn error_message(err: ModuleError, st: Agent) -> String {
  case err {
    NotInBundle(specifier:) ->
      "Module '" <> specifier <> "' not found in bundle"
    EvaluationError(value:) -> "Uncaught " <> rt_inspect.format_error(st, value)
    EvaluationPending(promise: _) -> tla_never_settled_message
  }
}

/// A break in the LINKER's own invariants: never a guest-program error, and
/// never something a host can trigger with a well-formed bundle.
pub type LinkInvariantBroken {
  /// A module imports from a raw specifier its own (TOTAL) `specifier_map`
  /// does not cover.
  UnresolvedDependency(specifier: esm.Raw)
  /// A `bundle.modules` key has no `LinkedModule`.
  ModuleNotLinked(specifier: String)
  /// The dependency's export map has no live cell for a name that
  /// `link.validate` already accepted.
  MissingExportCell(dep: String, name: String)
  /// `import defer * as ns from dep`, but `dep` has no reserved deferred proxy.
  MissingDeferredBox(dep: String)
  /// A specifier registered as already-instantiated whose namespace handle
  /// does not read back as a Module Namespace Exotic Object.
  PreexistingNotANamespace(specifier: String, namespace: Handle)
  /// A reserved namespace / deferred-namespace box that does not hold an
  /// object.
  NamespaceBoxCorrupt(specifier: String)
}

pub fn link_invariant_message(broken: LinkInvariantBroken) -> String {
  "arc/module: linker invariant broken: " <> string.inspect(broken)
}

fn assert_link_invariant(result: Result(a, LinkInvariantBroken)) -> a {
  case result {
    Ok(v) -> v
    Error(broken) -> panic as link_invariant_message(broken)
  }
}

/// The successful result of evaluation: the entry module's completion value
/// and its Module Namespace Exotic Object (§10.4.6), the embedder's
/// `GetModuleNamespace` handle.
pub type EvaluatedBundle {
  EvaluatedBundle(value: JsVal, namespace: Handle)
}

// =============================================================================
// AOT Compilation (compile_bundle)
// =============================================================================

/// Compile a module and all its dependencies into a self-contained
/// ModuleBundle. `resolve` maps (raw_specifier, referrer) to the dependency's
/// canonical specifier; `load` reads a resolved specifier's source, once per
/// unique module.
pub fn compile_bundle(
  entry_specifier: String,
  entry_source: String,
  resolve: fn(String, String) -> Result(String, ResolveError),
  load: fn(String) -> Result(String, LoadError),
) -> Result(ModuleBundle, CompileBundleError) {
  compile_bundle_with_hosts(
    entry_specifier,
    entry_source,
    resolve,
    load,
    dict.new(),
  )
}

/// `compile_bundle` plus embedder-provided host (synthetic) modules. A
/// request whose resolved specifier is a key of `host_modules` is a leaf in
/// the graph walk (resolved, never source-loaded) and carried through so the
/// linker can bind its imports to the host values.
pub fn compile_bundle_with_hosts(
  entry_specifier: String,
  entry_source: String,
  resolve: fn(String, String) -> Result(String, ResolveError),
  load: fn(String) -> Result(String, LoadError),
  host_modules: Dict(String, HostModule),
) -> Result(ModuleBundle, CompileBundleError) {
  // Whatever the host resolver returns is by definition a canonical module
  // identity, and so is the entry specifier the embedder named.
  let resolve_request = fn(request: esm.ModuleRequest, referrer) {
    resolve(esm.raw_text(request.specifier), esm.resolved_text(referrer))
    |> result.map(esm.resolved_unchecked)
  }
  let load_source = fn(spec) { load(esm.resolved_text(spec)) }
  use source_graph <- result.try(
    graph.load(
      esm.resolved_unchecked(entry_specifier),
      entry_source,
      resolve_request,
      load_source,
      fn(spec) { dict.has_key(host_modules, esm.resolved_text(spec)) },
    )
    |> result.map_error(GraphError),
  )
  // Host modules first, then the compiled source graph on top: the only way
  // both could name the same specifier is the entry, whose source wins.
  let with_hosts =
    dict.map_values(host_modules, fn(_spec, hm) { SyntheticModule(hm) })
  use modules <- result.map(
    dict.fold(source_graph.modules, Ok(with_hosts), fn(acc, specifier, node) {
      use modules <- result.try(acc)
      use compiled <- result.map(compile_source_module(node))
      dict.insert(modules, esm.resolved_text(specifier), SourceModule(compiled))
    }),
  )
  ModuleBundle(entry: entry_specifier, modules:)
}

/// Compile one loaded module from the source graph: the import/export
/// analysis is already done (`node.summary`); this adds the bytecode stage.
fn compile_source_module(
  node: graph.SourceModule,
) -> Result(CompiledModule, CompileBundleError) {
  let graph.SourceModule(
    parsed: graph.ParsedModule(specifier:, source: _, items:, sb:, summary:),
    resolved:,
  ) = node
  use body <- result.map(
    compiler.compile_module(items, sb, summary)
    |> result.map_error(fn(error) {
      CompileError(specifier: esm.resolved_text(specifier), error:)
    }),
  )
  let requested_modules =
    list.map(resolved, fn(edge) {
      let #(request, resolved_specifier) = edge
      #(resolved_specifier, request.phase)
    })
  CompiledModule(
    specifier:,
    template: body.template,
    import_bindings: summary.imports,
    export_entries: summary.exports,
    export_names: body.export_names,
    specifier_map: graph.specifier_map(node),
    requested_modules:,
    export_seeds: body.export_seeds,
    hoisted_funcs: body.hoisted_funcs,
    has_tla: body.has_tla,
  )
}

fn seed_value(seed: ExportSeed) -> JsVal {
  case seed {
    compiler.SeedUndefined -> mk_undefined()
    compiler.SeedUninitialized -> mk_tdz()
  }
}

// =============================================================================
// Linking: ResolveExport (§16.2.1.6.3) + import/re-export checks (§16.2.1.6.4)
// =============================================================================

/// Project a compiled bundle onto the shared `link.LinkableGraph` view,
/// resolving each module's raw specifiers through its own TOTAL specifier
/// map exactly once.
fn linkable_of_bundle(bundle: ModuleBundle) -> link.LinkableGraph {
  use acc, specifier, bundle_module <- dict.fold(bundle.modules, dict.new())
  let linkable = case bundle_module {
    SourceModule(m) ->
      link.project_module(m.import_bindings, m.export_entries, m.specifier_map)
      |> result.map_error(UnresolvedDependency)
      |> assert_link_invariant
    // A host module: no imports, one LocalExport per supplied name.
    SyntheticModule(hm) ->
      link.LinkableModule(
        import_bindings: [],
        export_entries: list.map(hm.exports, fn(e) {
          link.LocalExport(export_name: e.0, local_name: e.0)
        }),
        star_exports: [],
      )
  }
  dict.insert(acc, esm.resolved_unchecked(specifier), linkable)
}

// =============================================================================
// Runtime Evaluation
// =============================================================================

/// One module's pre-allocated binding cells (§16.2 instantiation).
pub type LinkedModule {
  LinkedModule(
    /// local binding name → box cell (seeded TDZ/undefined).
    local_boxes: Dict(String, Handle),
    /// exported name → box cell (LocalExport and re-exports resolved to the
    /// owning module's cell; namespace re-exports → a box wrapping the
    /// target's namespace object).
    exports: Dict(String, Handle),
    /// A box wrapping this module's Module Namespace Exotic Object (seeded
    /// for `import * as ns`).
    namespace_box: Handle,
  )
}

/// Every module's binding cells, pre-allocated before any body runs so
/// cyclic/self imports reference the same live cells. Immutable once built.
pub type Linked {
  Linked(
    /// specifier → its `LinkedModule`; same key set as `bundle.modules`.
    modules: Dict(String, LinkedModule),
    /// specifier → a box wrapping that module's Deferred Module Namespace,
    /// for modules some importer defers (or already registered as deferred).
    deferred_boxes: Dict(String, Handle),
  )
}

/// Where a module is in its per-DFS lifecycle.
type ModuleEvalStatus {
  Evaluating
  Evaluated
  /// The body (or a dependency's) threw; the value is rethrown, never re-run.
  Failed(value: JsVal)
}

/// The post-body driver of one evaluation: `rt/async.drain` for the static
/// entry points; identity for dynamic import and deferred triggers, which
/// run inside a job or a body on the host's own microtask drain and must
/// never drain re-entrantly. Either way it runs as the body's turn epilogue
/// (`safepoint.finish_turn`): the body's completion value stays rooted while
/// it collects and drains.
pub type Finish =
  fn(Agent) -> Agent

fn no_drain(st: Agent) -> Agent {
  st
}

/// Internal evaluation state threaded through the DFS.
type EvalState {
  EvalState(
    agent: Agent,
    /// Specifier → lifecycle state, for the modules this DFS has touched. The
    /// realm registry is the fallback for modules an outer evaluation or a
    /// re-entrant deferred trigger touched (see `module_eval_status`).
    modules: Dict(String, ModuleEvalStatus),
  )
}

fn evaluated_specifiers(es: EvalState) -> Set(String) {
  use acc, spec, status <- dict.fold(es.modules, set.new())
  case status {
    Evaluated -> set.insert(acc, spec)
    Evaluating | Failed(_) -> acc
  }
}

/// A module's lifecycle state: this DFS's own view first, else the realm
/// registry. A cached ERROR wins over a stale ~evaluating~ mark: an async
/// module rejected after parking keeps its status but is `Failed`.
fn module_eval_status(
  es: EvalState,
  specifier: String,
) -> Option(ModuleEvalStatus) {
  use <- option.lazy_or(dict.get(es.modules, specifier) |> option.from_result)
  use <- option.lazy_or(
    registry.read_module_error(es.agent, specifier) |> option.map(Failed),
  )
  case registry.read_module_status(es.agent, specifier) {
    Some(registry.Evaluated) -> Some(Evaluated)
    Some(registry.Evaluating) -> Some(Evaluating)
    None -> None
  }
}

fn set_eval_status(
  es: EvalState,
  specifier: String,
  status: ModuleEvalStatus,
) -> EvalState {
  EvalState(..es, modules: dict.insert(es.modules, specifier, status))
}

fn with_agent(es: EvalState, agent: Agent) -> EvalState {
  EvalState(..es, agent:)
}

/// Fold `items` threading `s`, short-circuiting on the first Error.
fn try_fold_state(
  items: List(i),
  s: s,
  initial: a,
  f: fn(s, a, i) -> #(s, Result(a, e)),
) -> #(s, Result(a, e)) {
  use acc, item <- list.fold(items, #(s, Ok(initial)))
  case acc {
    #(_, Error(_)) -> acc
    #(s, Ok(v)) -> f(s, v, item)
  }
}

/// A linked-but-not-yet-evaluated bundle: every binding cell and namespace
/// object pre-allocated, exported hoisted functions instantiated. The entry
/// namespace is live before evaluation (`entry_namespace_of`), so a host can
/// publish it in its registry first and a re-entrant dynamic import of the
/// evaluating module resolves to it instead of re-evaluating (§16.2.1.8).
pub type LinkedBundle {
  LinkedBundle(bundle: ModuleBundle, linked: Linked)
}

/// The entry module's Module Namespace Exotic Object of a linked bundle.
pub fn entry_namespace_of(linked_bundle: LinkedBundle, st: Agent) -> Handle {
  entry_namespace(linked_bundle.linked, linked_bundle.bundle.entry, st)
}

/// Link phase (§16.2.1.6.4): resolve every import and indirect re-export
/// across the whole graph BEFORE evaluating any body; missing or ambiguous
/// exports are a SyntaxError, surfaced as `EvaluationError`. On success,
/// pre-allocates every binding cell and namespace object and instantiates
/// exported hoisted function declarations.
pub fn link_for_evaluation(
  bundle: ModuleBundle,
  st: Agent,
) -> #(Agent, Result(LinkedBundle, ModuleError)) {
  link_for_evaluation_reusing(bundle, st, dict.new(), dict.new())
}

/// `link_for_evaluation` with a registry of already-instantiated modules:
/// `preexisting` maps a resolved specifier to its existing namespace object.
/// Those modules keep their namespace identity and live export cells
/// (§16.2.1.8); only the remaining modules get fresh cells.
pub fn link_for_evaluation_reusing(
  bundle: ModuleBundle,
  st: Agent,
  preexisting: Dict(String, Handle),
  preexisting_deferred: Dict(String, Handle),
) -> #(Agent, Result(LinkedBundle, ModuleError)) {
  let lg = linkable_of_bundle(bundle)
  case link.validate(lg) {
    Error(link_error) -> {
      let #(err, st) =
        new_error(st, SyntaxErr, link.link_error_message(link_error))
      #(st, Error(EvaluationError(err)))
    }
    Ok(Nil) -> {
      // Expand each preexisting namespace into (handle, export-name → box):
      // the export map is final for an instantiated module and is exactly
      // what importers link against.
      let pre =
        dict.fold(preexisting, dict.new(), fn(acc, spec, ns) {
          case rt_store.t_cell_get(st, ns) {
            SObject(kind: ModuleNamespace(exports:), ..) ->
              dict.insert(acc, spec, #(ns, exports))
            _ ->
              assert_link_invariant(Error(PreexistingNotANamespace(spec, ns)))
          }
        })
      // A preexisting module's live export map must hold a cell for every
      // name THIS bundle's fresh parse of it exports; only a host loader that
      // served different source for a specifier it already served can break
      // that, so it is a guest-visible link error.
      case stale_reused_export(bundle, lg, pre) {
        Some(#(spec, name)) -> {
          let #(err, st) =
            new_error(st, SyntaxErr, stale_reused_export_message(spec, name))
          #(st, Error(EvaluationError(err)))
        }
        None -> {
          // Instantiate: pre-allocate every binding cell + namespace object,
          // then create exported function-declaration closures so cyclic
          // function imports are callable before any body runs (§16.2.1.6.4
          // step 9).
          let #(st, linked, deferred_to_fill) =
            build_linked(bundle, st, pre, preexisting_deferred)
          let st =
            list.fold(deferred_to_fill, st, fn(st, pair) {
              let #(spec, proxy) = pair
              fill_deferred_namespace(st, bundle, linked, spec, proxy)
            })
          let st =
            instantiate_hoisted_functions(
              bundle,
              linked,
              st,
              set.from_list(dict.keys(pre)),
            )
          #(st, Ok(LinkedBundle(bundle:, linked:)))
        }
      }
    }
  }
}

/// Evaluation phase: execute module bodies in DFS post-order (dependencies
/// first). Static entry points drive a draining `finish`, so a module still
/// pending here can never settle: that surfaces as a TypeError.
pub fn evaluate_linked(
  linked_bundle: LinkedBundle,
  st: Agent,
  finish: Finish,
) -> #(Agent, Result(EvaluatedBundle, ModuleError)) {
  let #(st, _evaluated, res) =
    evaluate_linked_tracking(linked_bundle, st, finish, set.new())
  case res {
    Error(EvaluationPending(promise: _)) -> {
      let #(err, st) = new_error(st, TypeErr, tla_never_settled_message)
      #(st, Error(EvaluationError(value: err)))
    }
    other -> #(st, other)
  }
}

/// `evaluate_linked` for registry-aware hosts: modules in `already_evaluated`
/// are treated as done. Also returns the set of specifiers whose bodies this
/// DFS completed, so the host can register exactly those.
pub fn evaluate_linked_tracking(
  linked_bundle: LinkedBundle,
  st: Agent,
  finish: Finish,
  already_evaluated: Set(String),
) -> #(Agent, Set(String), Result(EvaluatedBundle, ModuleError)) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  let modules =
    set.fold(already_evaluated, dict.new(), fn(acc, spec) {
      dict.insert(acc, spec, Evaluated)
    })
  let es = EvalState(agent: st, modules:)
  let #(es, res) = eval_module_inner(bundle, linked, es, bundle.entry, finish)
  // Surface the entry namespace alongside the completion value (post-eval,
  // so its bindings are initialized).
  let res = {
    use value <- result.map(res)
    EvaluatedBundle(
      value:,
      namespace: entry_namespace(linked, bundle.entry, es.agent),
    )
  }
  #(es.agent, evaluated_specifiers(es), res)
}

/// Read a reserved namespace / deferred-namespace box as the object it holds.
fn read_namespace_box(
  st: Agent,
  spec: String,
  box: Handle,
) -> Result(Handle, LinkInvariantBroken) {
  case rt_store.t_cell_get(st, box) {
    SBox(value:) ->
      case classify(value) {
        KHandle(ns) -> Ok(ns)
        _ -> Error(NamespaceBoxCorrupt(specifier: spec))
      }
    _ -> Error(NamespaceBoxCorrupt(specifier: spec))
  }
}

fn read_box_dict(
  boxes: Dict(String, Handle),
  st: Agent,
) -> List(#(String, Handle)) {
  use acc, spec, box <- dict.fold(boxes, [])
  let ns = read_namespace_box(st, spec, box) |> assert_link_invariant
  [#(spec, ns), ..acc]
}

/// Every module in a linked bundle paired with its namespace object: what a
/// registry-keeping host records so a later import of any graph module reuses
/// the same record (§16.2.1.8).
pub fn linked_namespaces(
  linked_bundle: LinkedBundle,
  st: Agent,
) -> List(#(String, Handle)) {
  use acc, spec, lm <- dict.fold(linked_bundle.linked.modules, [])
  let ns =
    read_namespace_box(st, spec, lm.namespace_box) |> assert_link_invariant
  [#(spec, ns), ..acc]
}

/// Every Deferred Module Namespace in a linked bundle, for the registry.
pub fn linked_deferred_namespaces(
  linked_bundle: LinkedBundle,
  st: Agent,
) -> List(#(String, Handle)) {
  read_box_dict(linked_bundle.linked.deferred_boxes, st)
}

/// Why `get_or_create_deferred_namespace` could not hand back a namespace.
pub type DeferredNamespaceError {
  DeferredSpecifierNotInBundle(specifier: String)
}

/// The Deferred Module Namespace for `spec`, creating one if no importer in
/// the bundle deferred it statically (the dynamic `import.defer()` path).
/// Host (synthetic) modules qualify: their `[[Evaluate]]` is a no-op, so a
/// deferred namespace over one is a namespace whose trigger does nothing.
pub fn get_or_create_deferred_namespace(
  st: Agent,
  linked_bundle: LinkedBundle,
  spec: String,
) -> #(Agent, Result(Handle, DeferredNamespaceError)) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  use <- bool.lazy_guard(!dict.has_key(bundle.modules, spec), fn() {
    #(st, Error(DeferredSpecifierNotInBundle(specifier: spec)))
  })
  case dict.get(linked.deferred_boxes, spec) {
    Ok(box) -> #(
      st,
      Ok(read_namespace_box(st, spec, box) |> assert_link_invariant),
    )
    Error(Nil) -> {
      let #(proxy, st) = reserve_cell(st)
      let st = fill_deferred_namespace(st, bundle, linked, spec, proxy)
      #(st, Ok(proxy))
    }
  }
}

/// Link then evaluate a compiled bundle. Returns the entry module's
/// completion value and namespace.
pub fn evaluate_bundle(
  bundle: ModuleBundle,
  st: Agent,
  finish: Finish,
) -> #(Agent, Result(EvaluatedBundle, ModuleError)) {
  case link_for_evaluation(bundle, st) {
    #(st, Error(err)) -> #(st, Error(err))
    #(st, Ok(linked_bundle)) -> evaluate_linked(linked_bundle, st, finish)
  }
}

fn entry_namespace(linked: Linked, entry: String, st: Agent) -> Handle {
  let lm =
    dict.get(linked.modules, entry)
    |> result.replace_error(ModuleNotLinked(entry))
    |> assert_link_invariant
  read_namespace_box(st, entry, lm.namespace_box) |> assert_link_invariant
}

/// Read a named export off a Module Namespace Exotic Object without a JS
/// context: the export's live binding value, or `None` if `namespace` isn't a
/// module namespace, has no such export, or the binding is still in TDZ
/// (where the in-language [[Get]] would throw a ReferenceError).
pub fn read_export(st: Agent, namespace: JsVal, name: String) -> Option(JsVal) {
  use ns <- option.then(case classify(namespace) {
    KHandle(h) -> Some(h)
    _ -> None
  })
  use exports <- option.then(case rt_store.t_cell_get(st, ns) {
    SObject(kind: ModuleNamespace(exports:), ..) -> Some(exports)
    _ -> None
  })
  use box <- option.then(dict.get(exports, name) |> option.from_result)
  case rt_store.t_cell_get(st, box) {
    SBox(value:) ->
      case classify(value) {
        KTdz -> None
        _ -> Some(value)
      }
    _ -> None
  }
}

/// DFS post-order evaluation of a single module and its dependencies.
fn eval_module_inner(
  bundle: ModuleBundle,
  linked: Linked,
  es: EvalState,
  specifier: String,
  finish: Finish,
) -> #(EvalState, Result(JsVal, ModuleError)) {
  case dict.get(bundle.modules, specifier) {
    Error(Nil) -> #(es, Error(NotInBundle(specifier:)))
    // A host (synthetic) module's `[[Evaluate]]` is a no-op.
    Ok(SyntheticModule(_)) -> #(es, Ok(mk_undefined()))
    Ok(SourceModule(compiled)) ->
      case module_eval_status(es, specifier) {
        // Body already ran (this DFS, a re-entrant deferred trigger, or an
        // earlier bundle sharing this realm).
        Some(Evaluated) -> #(es, Ok(mk_undefined()))
        // Cached error: re-throw, never re-evaluate.
        Some(Failed(err)) -> #(es, Error(EvaluationError(err)))
        // Circular dependency: return without re-entering.
        Some(Evaluating) -> #(es, Ok(mk_undefined()))
        None ->
          eval_module_body(bundle, linked, es, specifier, compiled, finish)
      }
  }
}

/// Evaluate a module's dependencies and then its body.
fn eval_module_body(
  bundle: ModuleBundle,
  linked: Linked,
  es: EvalState,
  specifier: String,
  compiled: CompiledModule,
  finish: Finish,
) -> #(EvalState, Result(JsVal, ModuleError)) {
  let es = set_eval_status(es, specifier, Evaluating)

  // Dependencies first (DFS post-order), following the §16.2.1.5.3.1
  // InnerModuleEvaluation evaluationList: an ~evaluation~-phase request
  // evaluates the module itself, a ~defer~-phase request only its
  // ASYNCHRONOUS transitive dependencies.
  let #(es, dep_result) = {
    use es, Nil, #(resolved_dep, phase) <- try_fold_state(
      compiled.requested_modules,
      es,
      Nil,
    )
    let dep_specifier = esm.resolved_text(resolved_dep)
    let to_evaluate = case phase {
      esm.Evaluation -> [dep_specifier]
      esm.Deferred ->
        gather_async_transitive_deps(bundle, es, dep_specifier, set.new()).0
    }
    use es, Nil, dep <- try_fold_state(to_evaluate, es, Nil)
    let #(es, r) = eval_module_inner(bundle, linked, es, dep, finish)
    #(es, result.replace(r, Nil))
  }

  case dep_result {
    // A dependency parked on top-level await is not a failure: propagate
    // without caching an error (it may still complete later).
    Error(EvaluationPending(promise: _) as err) -> #(es, Error(err))
    Error(err) -> {
      // Dependency failed: cache the error on this module too. A dependency
      // missing from the bundle has no thrown value, so allocate a TypeError.
      let #(error_val, st) = case err {
        EvaluationError(value: v) -> #(v, es.agent)
        NotInBundle(..) | EvaluationPending(..) ->
          new_error(es.agent, TypeErr, error_message(err, es.agent))
      }
      let st = registry.write_module_error(st, specifier, error_val)
      let es =
        with_agent(es, st) |> set_eval_status(specifier, Failed(error_val))
      #(es, Error(err))
    }
    Ok(Nil) -> {
      // Seed the slots: imports as boxed captures in slots 0..N-1 (each the
      // *exporter's* live cell), plus this module's own export cells in their
      // declared slots.
      let seeds =
        import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
        |> assert_link_invariant
        |> list.append(own_export_seeds(linked, compiled))
      // Publish [[Status]] = ~evaluating~ in the registry so a deferred
      // namespace trigger firing inside this body observes the cycle.
      let st =
        registry.write_module_status(es.agent, specifier, registry.Evaluating)
      let #(outcome, st) =
        run_module_body(st, specifier, compiled, seeds, finish)
      case outcome {
        BodyThrew(thrown) -> {
          let st =
            st
            |> registry.clear_module_status(specifier)
            |> registry.write_module_error(specifier, thrown)
          let es =
            with_agent(es, st) |> set_eval_status(specifier, Failed(thrown))
          #(es, Error(EvaluationError(thrown)))
        }
        BodyReturned(v) -> {
          let st =
            registry.write_module_status(st, specifier, registry.Evaluated)
          let es = with_agent(es, st) |> set_eval_status(specifier, Evaluated)
          #(es, Ok(v))
        }
        // Parked on top-level await (non-draining driver): not evaluated,
        // not errored; the body resumes on the host's microtask drain.
        BodyPending(promise) -> #(
          with_agent(es, st),
          Error(EvaluationPending(promise:)),
        )
      }
    }
  }
}

// =============================================================================
// Running one body
// =============================================================================

/// How one module body's evaluation ended after its `finish` driver ran.
type BodyOutcome {
  BodyReturned(JsVal)
  BodyThrew(JsVal)
  /// Parked on top-level await; the [[TopLevelCapability]] promise object.
  BodyPending(Handle)
}

/// A root activation of the module `template` over `locals`. Module `this` is
/// undefined (§16.2.1.5.2).
fn module_activation(
  agent: Agent,
  template: FuncTemplate,
  seeds: List(#(Int, JsVal)),
) -> State {
  let locals =
    list.fold(
      seeds,
      tuple_array.repeat(mk_undefined(), template.local_count),
      fn(acc, seed) { tuple_array.set_unchecked(seed.0, seed.1, acc) },
    )
  State(
    agent:,
    pc: 0,
    stack: [],
    locals:,
    code: template.bytecode,
    constants: template.constants,
    func: template,
    call_stack: [],
    try_stack: [],
    this: mk_undefined(),
    new_target: mk_undefined(),
    home_object: mk_undefined(),
    call_args: [],
    eval_env: None,
  )
}

/// Run a module body with the registry's active referrer set to its resolved
/// specifier (§16.2.1.8 HostLoadImportedModule: an ImportCall inside
/// captures it as its referencingScriptOrModule) across the synchronous part
/// of the body AND `finish`, so the turns of a body parked on top-level await
/// that `finish` resumes still import relative to the module; then classify
/// the outcome. A body that awaits at top level becomes ExecuteAsyncModule
/// (§16.2.1.5.3.4).
fn run_module_body(
  st: Agent,
  specifier: String,
  compiled: CompiledModule,
  seeds: List(#(Int, JsVal)),
  finish: Finish,
) -> #(BodyOutcome, Agent) {
  let outer_referrer = registry.read_active_referrer(st)
  let st = registry.write_active_referrer(st, Some(specifier))
  let #(outcome, st) = run_module_turns(st, compiled, seeds, finish)
  #(outcome, registry.write_active_referrer(st, outer_referrer))
}

fn run_module_turns(
  st: Agent,
  compiled: CompiledModule,
  seeds: List(#(Int, JsVal)),
  finish: Finish,
) -> #(BodyOutcome, Agent) {
  let #(step, st) =
    entry.run_turn(module_activation(st, compiled.template, seeds))
  case step {
    StepReturn(v) -> #(BodyReturned(v), safepoint.finish_turn(st, [v], finish))
    StepThrow(e) -> #(BodyThrew(e), safepoint.finish_turn(st, [e], finish))
    StepAwait(awaited, resume) ->
      drive_top_level_await(st, awaited, resume, finish)
    // `yield` cannot occur outside a generator body.
    StepYield(..) -> {
      let #(err, st) =
        new_error(st, TypeErr, "InternalError: module body yielded")
      #(BodyThrew(err), safepoint.finish_turn(st, [err], finish))
    }
  }
}

/// ExecuteAsyncModule (§16.2.1.5.3.4): the body behaves like an async
/// function whose result lands in a fresh promise capability. The parked
/// frame goes in an `SAsyncContext` and the awaited value is hooked with the
/// same resume machinery async functions use, so settling it resumes the body
/// (re-parking on each further await) until it settles the capability. After
/// `finish`, the capability says how evaluation ended.
fn drive_top_level_await(
  st: Agent,
  awaited: JsVal,
  resume: types.Resume,
  finish: Finish,
) -> #(BodyOutcome, Agent) {
  let #(promise, st) = rt_async.t_new_promise(st)
  // Held past drains from Gleam only: pin so a between-jobs collection with
  // an unreachable awaited promise cannot reclaim it.
  let st = rt_store.t_pin_root(st, promise)
  // The host always inspects this capability below: mark it handled so a
  // rejection is not also reported as unhandled.
  let #(data, pstate, _) = rt_async.promise_data(st, promise)
  let st = rt_store.t_cell_set(st, data, SPromiseData(pstate, True))
  let #(ctx, st) = rt_store.t_cell_new(st, SAsyncContext(resume:, promise:))
  let st = rt_async.t_await(st, ctx, awaited)
  let st = safepoint.finish_turn(st, [], finish)
  case rt_async.promise_data(st, promise) {
    #(_, PromiseFulfilled(v), _) -> #(BodyReturned(v), st)
    #(_, PromiseRejected(reason), _) -> #(BodyThrew(reason), st)
    // Still pending after `finish`: never settles for a draining driver,
    // legitimately mid-flight for a non-draining one. Either way NOT
    // evaluated; the caller decides (static entry points: an error;
    // ContinueDynamicImport: a promise to chain onto).
    #(_, PromisePending(_), _) -> #(BodyPending(promise), st)
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

fn new_error(st: Agent, kind: types.ErrorKind, msg: String) -> #(JsVal, Agent) {
  st.store.ops.new_error(st, kind, msg)
}

/// Allocate a GC-pinned box holding `val`. Module binding cells live for the
/// duration of the module graph and are held from Gleam (`Linked`) between a
/// dependency being linked and its importer's body running.
fn alloc_box(st: Agent, val: JsVal) -> #(Handle, Agent) {
  let #(box, st) = rt_store.t_cell_new(st, SBox(val))
  #(box, rt_store.t_pin_root(st, box))
}

/// Reserve a pinned cell to be overwritten once its contents are known.
fn reserve_cell(st: Agent) -> #(Handle, Agent) {
  alloc_box(st, mk_undefined())
}

/// For each spec: allocate a pinned namespace box. If `preexisting` yields a
/// handle the box wraps it; otherwise reserve a fresh object cell, box it, and
/// record `#(spec, obj)` for the caller to fill.
fn reserve_ns_boxes(
  st: Agent,
  specs: List(String),
  preexisting: fn(String) -> Result(Handle, Nil),
) -> #(Agent, Dict(String, Handle), List(#(String, Handle))) {
  use #(st, boxes, fresh), spec <- list.fold(specs, #(st, dict.new(), []))
  case preexisting(spec) {
    Ok(existing) -> {
      let #(box, st) = alloc_box(st, mk_object(existing))
      #(st, dict.insert(boxes, spec, box), fresh)
    }
    Error(Nil) -> {
      let #(obj, st) = reserve_cell(st)
      let #(box, st) = alloc_box(st, mk_object(obj))
      #(st, dict.insert(boxes, spec, box), [#(spec, obj), ..fresh])
    }
  }
}

// -----------------------------------------------------------------------------
// Instantiation: pre-allocate every binding cell + namespace object (§16.2).
// -----------------------------------------------------------------------------

/// Build the whole graph's binding cells before any body runs. `preexisting`
/// (specifier → (namespace object, its export map)) marks modules
/// instantiated by an earlier bundle in the same realm: their namespace
/// object and export cells are reused as-is.
fn build_linked(
  bundle: ModuleBundle,
  st: Agent,
  preexisting: Dict(String, #(Handle, Dict(String, Handle))),
  preexisting_deferred: Dict(String, Handle),
) -> #(Agent, Linked, List(#(String, Handle))) {
  let #(st, local_boxes) = preallocate_local_boxes(bundle, st, preexisting)
  let specs = dict.keys(bundle.modules)
  // Reserve a namespace object per module, then a box wrapping it, up front
  // so cyclic / star-reached namespace re-exports resolve to a handle.
  let #(st, namespace_boxes, ns_to_fill) =
    reserve_ns_boxes(st, specs, fn(spec) {
      dict.get(preexisting, spec) |> result.map(fn(p) { p.0 })
    })
  // Deferred namespaces (`import defer * as ns`): reserve a proxy cell per
  // deferred-imported module; registered ones are reused for identity.
  let #(st, deferred_boxes, deferred_to_fill) =
    reserve_ns_boxes(st, needed_deferred_specs(bundle), dict.get(
      preexisting_deferred,
      _,
    ))
  // Resolve every exported name to a cell: a local binding's box, or the
  // target's namespace box (`export * as ns`).
  let lg = linkable_of_bundle(bundle)
  let exports =
    list.fold(specs, dict.new(), fn(all, spec) {
      case dict.get(preexisting, spec) {
        Ok(#(_, existing_exports)) -> dict.insert(all, spec, existing_exports)
        Error(Nil) -> {
          let key = esm.resolved_unchecked(spec)
          let map =
            link.exported_names(lg, key)
            |> list.fold(dict.new(), fn(map, name) {
              case link.resolve_export(lg, key, name) {
                // §16.2.1.6.3: a name reached only through `export *` that
                // resolves ambiguously (or not at all) is NOT an exported name.
                link.Unresolvable | link.Ambiguous -> map
                link.ResolvedTo(owner, binding) ->
                  dict.get(local_boxes, esm.resolved_text(owner))
                  |> result.try(dict.get(_, binding))
                  |> result.replace_error(MissingExportCell(
                    esm.resolved_text(owner),
                    binding,
                  ))
                  |> assert_link_invariant
                  |> dict.insert(map, name, _)
                link.ResolvedNamespace(target) -> {
                  // Reserved over the same key set: cannot miss.
                  let assert Ok(box) =
                    dict.get(namespace_boxes, esm.resolved_text(target))
                  dict.insert(map, name, box)
                }
                link.ResolvedDeferredNamespace(target) ->
                  dict.get(deferred_boxes, esm.resolved_text(target))
                  |> result.replace_error(
                    MissingDeferredBox(esm.resolved_text(target)),
                  )
                  |> assert_link_invariant
                  |> dict.insert(map, name, _)
              }
            })
          dict.insert(all, spec, map)
        }
      }
    })
  // Write each reserved namespace object now that its export map is complete.
  let st =
    list.fold(ns_to_fill, st, fn(st, pair) {
      let #(spec, obj) = pair
      let assert Ok(exp) = dict.get(exports, spec)
      rt_store.t_cell_set(st, obj, namespace_slot(exp, "Module"))
    })
  let modules =
    dict.map_values(bundle.modules, fn(spec, _bm) {
      let assert Ok(lb) = dict.get(local_boxes, spec)
      let assert Ok(exp) = dict.get(exports, spec)
      let assert Ok(ns_box) = dict.get(namespace_boxes, spec)
      LinkedModule(local_boxes: lb, exports: exp, namespace_box: ns_box)
    })
  #(st, Linked(modules:, deferred_boxes:), deferred_to_fill)
}

/// GatherAsynchronousTransitiveDependencies(module): the modules in `spec`'s
/// whole dependency graph (BOTH phases) that have top-level await and are
/// not already evaluated/evaluating. Returns (discovery order, seen).
fn gather_async_transitive_deps(
  bundle: ModuleBundle,
  es: EvalState,
  spec: String,
  seen: Set(String),
) -> #(List(String), Set(String)) {
  use <- bool.guard(set.contains(seen, spec), #([], seen))
  let seen = set.insert(seen, spec)
  // "Started": this DFS's own view, or the registry. A FAILED module is not
  // started: its cached error is rethrown by whichever eval reaches it.
  let already_started = case dict.get(es.modules, spec) {
    Ok(Evaluated) | Ok(Evaluating) -> True
    Ok(Failed(_)) | Error(Nil) ->
      registry.read_module_status(es.agent, spec) != None
  }
  use <- bool.guard(already_started, #([], seen))
  case dict.get(bundle.modules, spec) {
    Error(Nil) | Ok(SyntheticModule(_)) -> #([], seen)
    Ok(SourceModule(m)) ->
      case m.has_tla {
        True -> #([spec], seen)
        False ->
          list.fold(m.requested_modules, #([], seen), fn(acc, request) {
            let #(found, seen) = acc
            let #(more, seen) =
              gather_async_transitive_deps(
                bundle,
                es,
                esm.resolved_text(request.0),
                seen,
              )
            #(list.append(found, more), seen)
          })
      }
  }
}

/// The ~defer~ arm of ContinueDynamicImport: Evaluate() each of the entry's
/// asynchronous transitive dependencies with the (non-draining) `finish`. A
/// module parked on top-level await surfaces in the `Ok` list as (specifier,
/// its [[TopLevelCapability]] promise) so the host can chain onto it; a throw
/// stops the walk.
pub fn evaluate_async_transitive_deps(
  linked_bundle: LinkedBundle,
  st: Agent,
  finish: Finish,
) -> #(Agent, Result(List(#(String, Handle)), ModuleError)) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  let es = EvalState(agent: st, modules: dict.new())
  let #(to_evaluate, _seen) =
    gather_async_transitive_deps(bundle, es, bundle.entry, set.new())
  let #(es, res) = {
    use es, pendings, dep <- try_fold_state(to_evaluate, es, [])
    case eval_module_inner(bundle, linked, es, dep, finish) {
      #(es, Ok(_)) -> #(es, Ok(pendings))
      #(es, Error(EvaluationPending(promise:))) -> #(
        es,
        Ok([#(dep, promise), ..pendings]),
      )
      #(es, Error(err)) -> #(es, Error(err))
    }
  }
  #(es.agent, result.map(res, list.reverse))
}

/// The resolved specifiers some module in the bundle imports with
/// `import defer * as ns`.
fn needed_deferred_specs(bundle: ModuleBundle) -> List(String) {
  dict.fold(bundle.modules, [], fn(acc, _spec, bundle_module) {
    use m <- with_source_module(bundle_module, acc)
    list.fold(m.import_bindings, acc, fn(acc, entry) {
      let #(raw_dep, bindings) = entry
      let is_deferred =
        list.any(bindings, fn(binding) {
          case binding {
            esm.NamespaceImport(phase: esm.Deferred, ..) -> True
            _ -> False
          }
        })
      use <- bool.guard(!is_deferred, acc)
      let dep =
        esm.resolve(m.specifier_map, raw_dep)
        |> option.to_result(UnresolvedDependency(raw_dep))
        |> assert_link_invariant
      [esm.resolved_text(dep), ..acc]
    })
  })
  |> list.unique
}

/// §16.2.1.6.4 step 9 (InstantiateFunctionObject): create closures for every
/// module's EXPORTED hoisted function declarations and write them into their
/// shared export cells BEFORE any body runs. Bodies still re-create their own
/// closures; these are the link-time values importers see until then.
fn instantiate_hoisted_functions(
  bundle: ModuleBundle,
  linked: Linked,
  st: Agent,
  already_evaluated: Set(String),
) -> Agent {
  dict.fold(bundle.modules, st, fn(st, spec, bundle_module) {
    use compiled <- with_source_module(bundle_module, st)
    // A preexisting module's export cells hold their final values.
    use <- bool.guard(set.contains(already_evaluated, spec), st)
    let lm =
      dict.get(linked.modules, spec)
      |> result.replace_error(ModuleNotLinked(spec))
      |> assert_link_invariant
    // Reconstruct the module's seeded frame so closures capture the same
    // cells a body run would.
    let seeds =
      import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
      |> assert_link_invariant
      |> list.append(own_export_seeds(linked, compiled))
    let locals = module_activation(st, compiled.template, seeds).locals
    list.fold(compiled.hoisted_funcs, st, fn(st, hf) {
      let #(name, func_idx) = hf
      // Only exported functions have a shared cell; the rest are body-local.
      case dict.get(lm.local_boxes, name) {
        Error(Nil) -> st
        Ok(box) -> {
          let child =
            tuple_array.get_unchecked(func_idx, compiled.template.functions)
          let captured =
            list.map(child.env_descriptors, fn(desc) {
              tuple_array.get_unchecked(desc.parent_index, locals)
            })
          let #(closure, st) = interpreter.make_closure(st, child, captured)
          rt_store.t_cell_set(st, box, SBox(mk_object(closure)))
        }
      }
    })
  })
}

/// The first name a preexisting module's fresh parse EXPORTS that its LIVE
/// export map has no cell for, as `#(specifier, export name)`. Compared over
/// the whole exported-name set (local exports AND re-exports); names that do
/// not resolve (§16.2.1.6.3) never get a cell in any bundle and are excluded.
fn stale_reused_export(
  bundle: ModuleBundle,
  lg: link.LinkableGraph,
  preexisting: Dict(String, #(Handle, Dict(String, Handle))),
) -> Option(#(String, String)) {
  dict.to_list(bundle.modules)
  |> list.find_map(fn(entry) {
    let #(spec, bundle_module) = entry
    case bundle_module, dict.get(preexisting, spec) {
      SourceModule(_), Ok(#(_, existing_exports)) -> {
        let key = esm.resolved_unchecked(spec)
        link.exported_names(lg, key)
        |> list.find_map(fn(name) {
          case link.resolve_export(lg, key, name) {
            link.Unresolvable | link.Ambiguous -> Error(Nil)
            link.ResolvedTo(..)
            | link.ResolvedNamespace(..)
            | link.ResolvedDeferredNamespace(..) ->
              case dict.has_key(existing_exports, name) {
                True -> Error(Nil)
                False -> Ok(#(spec, name))
              }
          }
        })
      }
      _, _ -> Error(Nil)
    }
  })
  |> option.from_result
}

fn stale_reused_export_message(specifier: String, name: String) -> String {
  "module '"
  <> specifier
  <> "' was re-loaded with an export '"
  <> name
  <> "' its already-instantiated namespace does not have: a loader must return "
  <> "the same source for a specifier it has already served"
}

/// One box per exported local, seeded with its instantiation value. Keyed
/// specifier → local name → box. Preexisting modules reuse their live cells,
/// translated back to local names via the module's export entries.
fn preallocate_local_boxes(
  bundle: ModuleBundle,
  st: Agent,
  preexisting: Dict(String, #(Handle, Dict(String, Handle))),
) -> #(Agent, Dict(String, Dict(String, Handle))) {
  dict.fold(bundle.modules, #(st, dict.new()), fn(acc, spec, bundle_module) {
    let #(st, all) = acc
    let existing =
      dict.get(preexisting, spec)
      |> option.from_result
      |> option.map(fn(p) { p.1 })
    let #(st, boxes) = case bundle_module, existing {
      SourceModule(m), Some(existing_exports) -> #(
        st,
        list.fold(m.export_entries, dict.new(), fn(boxes, e) {
          case e {
            // `stale_reused_export` already rejected the one host-controlled
            // way this map can miss a local export.
            esm.LocalExport(export_name:, local_name:) ->
              dict.get(existing_exports, export_name)
              |> result.replace_error(MissingExportCell(spec, export_name))
              |> assert_link_invariant
              |> dict.insert(boxes, local_name, _)
            _ -> boxes
          }
        }),
      )
      SourceModule(m), None ->
        dict.fold(m.export_seeds, #(st, dict.new()), fn(a, local, seed) {
          let #(st, boxes) = a
          let #(box, st) = alloc_box(st, seed_value(seed))
          #(st, dict.insert(boxes, local, box))
        })
      // A host module: one box per export, seeded with the host value itself
      // (the seed IS the final binding). Its local names ARE its export names.
      SyntheticModule(hm), existing_exports ->
        list.fold(hm.exports, #(st, dict.new()), fn(a, export) {
          let #(st, boxes) = a
          let #(name, val) = export
          let reused =
            option.then(existing_exports, fn(ex) {
              dict.get(ex, name) |> option.from_result
            })
          case reused {
            Some(box) -> #(st, dict.insert(boxes, name, box))
            None -> {
              let #(box, st) = alloc_box(st, val)
              #(st, dict.insert(boxes, name, box))
            }
          }
        })
    }
    #(st, dict.insert(all, spec, boxes))
  })
}

/// §10.4.6 Module Namespace Exotic Object slot over the export name → box
/// map, null prototype, non-extensible, @@toStringTag = `tag` with all-false
/// attributes (§28.3.1; "Deferred Module" for the ~defer~ phase namespace).
fn namespace_slot(exports: Dict(String, Handle), tag: String) -> types.JsSlot {
  SObject(
    kind: ModuleNamespace(exports:),
    proto: None,
    props: dict.new(),
    symbol_props: [
      #(
        types.symbol_to_string_tag,
        DataProperty(
          value: mk_string(tag),
          writable: False,
          enumerable: False,
          configurable: False,
          seq: 0,
        ),
      ),
    ],
    elements: NoElements,
    extensible: False,
  )
}

// -----------------------------------------------------------------------------
// Deferred Module Namespaces (defer-import-eval proposal)
// -----------------------------------------------------------------------------
//
// A deferred namespace is a Proxy exotic object whose target is a Module
// Namespace object for the SAME live export cells (tagged "Deferred Module"),
// and whose handler implements the deferred MOP: string-keyed [[Get]] /
// [[GetOwnProperty]] / [[HasProperty]] / [[DefineOwnProperty]] / [[Delete]]
// (except the key "then") and every [[OwnPropertyKeys]] first perform
// EnsureDeferredNamespaceEvaluation, then forward to the target. Symbol keys
// and "then" (IsSymbolLikeNamespaceKey) never trigger evaluation. Routing
// through the Proxy machinery means every builtin hits the deferred behavior
// without per-call-site changes.

/// A Gleam closure as compiled-function code over `(st, frame, args)`.
@external(erlang, "arc_rt_store_ffi", "identity")
fn as_code(
  f: fn(Agent, rt_call.Frame, List(JsVal)) -> #(JsVal, Agent),
) -> CompiledFn

fn trap_flags() -> types.FnFlags {
  FnFlags(
    is_constructor: False,
    is_class_constructor: False,
    is_derived_constructor: False,
    is_arrow: True,
    is_method: False,
    is_generator: False,
    is_async: False,
    is_strict: True,
  )
}

/// Allocate a plain function object whose body is the Gleam `body(st,
/// args)`, raising to throw. The module system's reaction handlers and
/// deferred-namespace traps are these.
pub fn alloc_host_fn(
  st: Agent,
  name: String,
  arity: Int,
  body: fn(Agent, List(JsVal)) -> #(JsVal, Agent),
) -> #(Handle, Agent) {
  let code = as_code(fn(st, _frame, args) { body(st, args) })
  rt_call.t_fn_new(st, code, trap_flags(), name, arity, None, None)
}

/// Write the reserved `proxy` cell as a Deferred Module Namespace for `spec`.
fn fill_deferred_namespace(
  st: Agent,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
  proxy: Handle,
) -> Agent {
  let lm =
    dict.get(linked.modules, spec)
    |> result.replace_error(ModuleNotLinked(spec))
    |> assert_link_invariant
  let #(target, st) =
    rt_store.t_cell_new(st, namespace_slot(lm.exports, "Deferred Module"))
  let #(handler, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let st =
    [
      #("get", 3, ReflectGet, False),
      #("has", 2, ReflectHas, False),
      #("deleteProperty", 2, ReflectDeleteProperty, False),
      #("defineProperty", 3, ReflectDefineProperty, False),
      #("getOwnPropertyDescriptor", 2, ReflectGetOwnPropertyDescriptor, False),
      #("ownKeys", 1, ReflectOwnKeys, True),
    ]
    |> list.fold(st, fn(st, t) {
      let #(fn_h, st) = alloc_deferred_trap(st, t, bundle, linked, spec)
      let #(_, st) =
        rt_obj.t_define_own_data(
          st,
          handler,
          StringKey(types.Named(t.0)),
          mk_object(fn_h),
          True,
          True,
          True,
        )
      st
    })
  rt_store.t_cell_set(
    st,
    proxy,
    SObject(
      kind: ProxyObj(target:, handler:, revoked: False),
      proto: None,
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      // A proxy's observable [[IsExtensible]] goes through the trap
      // machinery (the non-extensible target), but internal checks
      // (PrivateFieldAdd's non-extensible rejection) read this field, and a
      // deferred namespace must reject private stamping without evaluating.
      extensible: False,
    ),
  )
}

/// One deferred-namespace trap: trigger EnsureDeferredNamespaceEvaluation
/// when the operation observes exports (always for ownKeys; for keyed traps
/// only when the key is a string other than "then"), then forward to the
/// target via the corresponding Reflect builtin.
fn alloc_deferred_trap(
  st: Agent,
  trap: #(String, Int, ReflectNative, Bool),
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
) -> #(Handle, Agent) {
  let #(name, arity, native, always_triggers) = trap
  use st, args <- alloc_host_fn(
    st,
    "%DeferredNamespace[" <> name <> "]%",
    arity,
  )
  let string_key = case args {
    [_, key, ..] ->
      case classify(key) {
        KStr(k) -> Some(k)
        _ -> None
      }
    _ -> None
  }
  let triggers =
    always_triggers
    || case string_key {
      Some(k) -> k != "then"
      None -> False
    }
  case triggers, native, string_key {
    // IsSymbolLikeNamespaceKey: the string key "then" must NEVER be
    // observable on a deferred namespace: [[Get]] is undefined even when the
    // module exports `then`, even after evaluation. Only the get trap can
    // honor it: has/deleteProperty/getOwnPropertyDescriptor for an exported
    // "then" keep forwarding, or they would violate proxy invariants.
    False, ReflectGet, Some("then") -> #(mk_undefined(), st)
    False, _, _ -> rt_reflect.dispatch(st, native, mk_undefined(), args)
    True, _, _ -> {
      let st = ensure_deferred_evaluated(st, bundle, linked, spec)
      rt_reflect.dispatch(st, native, mk_undefined(), args)
    }
  }
}

/// EnsureDeferredNamespaceEvaluation(O): if `spec` is already evaluated,
/// done; if its evaluation previously threw, rethrow that error; if it is not
/// ReadyForSyncExecution (mid-evaluation, or top-level await in the
/// unevaluated subgraph), throw a TypeError; otherwise EvaluateSync. Raises.
fn ensure_deferred_evaluated(
  st: Agent,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
) -> Agent {
  use <- bool.guard(
    registry.read_module_status(st, spec) == Some(registry.Evaluated),
    st,
  )
  case registry.read_module_error(st, spec) {
    Some(err) -> rt_store.t_throw(st, err)
    None ->
      case ready_for_sync_execution(bundle, st, spec, set.new()).0 {
        False ->
          rt_val.t_throw_type_error(
            st,
            "Cannot synchronously evaluate deferred module: '"
              <> spec
              <> "' is still evaluating or has an unevaluated async dependency",
          )
        True -> evaluate_deferred_subgraph(st, bundle, linked, spec)
      }
  }
}

/// ReadyForSyncExecution(module, seen): every module in the unevaluated eager
/// dependency graph is neither mid-evaluation nor a top-level-await module.
fn ready_for_sync_execution(
  bundle: ModuleBundle,
  st: Agent,
  spec: String,
  seen: Set(String),
) -> #(Bool, Set(String)) {
  use <- bool.guard(set.contains(seen, spec), #(True, seen))
  let seen = set.insert(seen, spec)
  case registry.read_module_status(st, spec) {
    Some(registry.Evaluated) -> #(True, seen)
    Some(registry.Evaluating) -> #(False, seen)
    None ->
      case dict.get(bundle.modules, spec) {
        // Not in this bundle (registry-shared) or a host module: ready.
        Error(Nil) | Ok(SyntheticModule(_)) -> #(True, seen)
        Ok(SourceModule(m)) ->
          case m.has_tla {
            True -> #(False, seen)
            False ->
              // Only the ~evaluation~-phase requests: a ~defer~ request's
              // module is not part of this module's synchronous subgraph.
              list.fold(m.requested_modules, #(True, seen), fn(acc, request) {
                case acc.0, request.1 {
                  False, _ -> acc
                  True, esm.Deferred -> acc
                  True, esm.Evaluation ->
                    ready_for_sync_execution(
                      bundle,
                      st,
                      esm.resolved_text(request.0),
                      acc.1,
                    )
                }
              })
          }
      }
  }
}

/// EvaluateSync: run the deferred module's unevaluated (eager) subgraph via
/// the normal DFS evaluator. `module_eval_status` reads the registry for any
/// module this DFS has not itself touched, so bodies that already ran (this
/// bundle, an earlier bundle, an earlier trigger) are not re-run. Jobs the
/// bodies enqueue stay on the agent's queue for the running drain: nothing is
/// drained re-entrantly. Raises the evaluation error.
fn evaluate_deferred_subgraph(
  st: Agent,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
) -> Agent {
  let es = EvalState(agent: st, modules: dict.new())
  case eval_module_inner(bundle, linked, es, spec, no_drain) {
    #(es, Ok(_)) -> es.agent
    #(es, Error(EvaluationError(value:))) -> rt_store.t_throw(es.agent, value)
    // A deferred subgraph is only entered when ReadyForSyncExecution said yes,
    // so no body in it can park on top-level await; and the specifier came out
    // of this bundle. Both unreachable, but rendered through `error_message`.
    #(es, Error(NotInBundle(..) as other))
    | #(es, Error(EvaluationPending(..) as other)) ->
      rt_val.t_throw_type_error(
        es.agent,
        "Failed to evaluate deferred module '"
          <> spec
          <> "': "
          <> error_message(other, es.agent),
      )
  }
}

// -----------------------------------------------------------------------------
// Seeding: place the pre-allocated cells into a module's local slots.
// -----------------------------------------------------------------------------

/// Import bindings seeded into capture slots 0..N-1, in declaration order
/// (matching esm.import_local_names). Named/default forward the exporting
/// module's live cell; namespace imports get the shared namespace box.
fn import_seeds(
  linked: Linked,
  specifier_map: esm.SpecifierMap,
  import_bindings: List(#(esm.Raw, List(esm.ImportBinding))),
) -> Result(List(#(Int, JsVal)), LinkInvariantBroken) {
  use per_dep <- result.map(
    list.try_map(import_bindings, fn(entry) {
      let #(raw_dep, bindings) = entry
      use dep <- result.try(
        esm.resolve(specifier_map, raw_dep)
        |> option.to_result(UnresolvedDependency(raw_dep)),
      )
      let dep = esm.resolved_text(dep)
      use lm <- result.try(
        dict.get(linked.modules, dep)
        |> result.replace_error(ModuleNotLinked(dep)),
      )
      list.try_map(bindings, fn(binding) {
        case binding {
          esm.NamedImport(imported:, ..) ->
            forward_box(lm.exports, dep, imported)
          esm.DefaultImport(..) -> forward_box(lm.exports, dep, "default")
          esm.NamespaceImport(phase: esm.Deferred, ..) ->
            dict.get(linked.deferred_boxes, dep)
            |> result.replace_error(MissingDeferredBox(dep))
            |> result.map(mk_object)
          esm.NamespaceImport(phase: esm.Evaluation, ..) ->
            Ok(mk_object(lm.namespace_box))
        }
      })
    }),
  )
  per_dep
  |> list.flatten
  |> list.index_map(fn(box, idx) { #(idx, box) })
}

fn forward_box(
  dep_exports: Dict(String, Handle),
  dep: String,
  name: String,
) -> Result(JsVal, LinkInvariantBroken) {
  dict.get(dep_exports, name)
  |> result.replace_error(MissingExportCell(dep, name))
  |> result.map(mk_object)
}

/// This module's own export cells, placed into their declared local slots.
/// Locals that are IMPORT bindings (`import * as ns ...; export { ns }`) are
/// excluded: their slot keeps the import seed, and importers resolve such
/// exports through the dependency anyway.
fn own_export_seeds(
  linked: Linked,
  compiled: CompiledModule,
) -> List(#(Int, JsVal)) {
  let import_locals =
    esm.binding_local_names(compiled.import_bindings) |> set.from_list
  let spec = esm.resolved_text(compiled.specifier)
  let lm =
    dict.get(linked.modules, spec)
    |> result.replace_error(ModuleNotLinked(spec))
    |> assert_link_invariant
  lm.local_boxes
  |> dict.to_list
  |> list.filter_map(fn(pair) {
    let #(local_name, box) = pair
    use <- bool.guard(set.contains(import_locals, local_name), Error(Nil))
    dict.get(compiled.export_names, local_name)
    |> result.map(fn(index) { #(index, mk_object(box)) })
  })
}
