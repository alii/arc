/// ES Module system for Arc.
///
/// Two-phase module lifecycle:
///   1. compile_bundle: Parse + compile all modules AOT into a ModuleBundle
///   2. evaluate_bundle: Execute the bundle at runtime (no parser, no disk I/O)
///
/// The ModuleBundle is a pure Erlang term, serializable via term_to_binary.
///
/// Based on ECMAScript §16.2 and QuickJS's module implementation.
import arc/compiler
import arc/esm
import arc/link
import arc/module/graph
import arc/module/load_error.{type LoadError, type ResolveError}
import arc/module/registry
import arc/parser
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/reflect
import arc/vm/exec/entry
import arc/vm/exec/frame
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/host_hooks
import arc/vm/internal/elements
import arc/vm/internal/job_queue
import arc/vm/internal/tuple_array
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State, vm_error_message}
import arc/vm/value.{
  type JsValue, type Ref, BoxSlot, JsObject, JsString, JsUndefined, ObjectSlot,
}
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

/// A single compiled module — everything known at compile time.
/// No AST, no source code, no runtime state.
///
/// The Raw/Resolved distinction is carried on the record itself: `specifier`
/// and every `requested_modules` entry are module identities (`esm.Resolved`),
/// while `import_bindings` is keyed by the specifier text this module's SOURCE
/// wrote (`esm.Raw`), which only `esm.resolve` through `specifier_map` may turn
/// into an identity. Handing one where the other belongs is a type error rather
/// than a silent lookup miss. `ModuleBundle`'s dict keys stay plain `String` —
/// that is the public, embedder-facing API — so identities are untagged with
/// `esm.resolved_text` exactly where they index a bundle / `Linked` dict.
pub type CompiledModule {
  CompiledModule(
    specifier: esm.Resolved,
    template: value.FuncTemplate,
    /// (raw specifier as written, bindings) — resolve through `specifier_map`.
    import_bindings: List(#(esm.Raw, List(esm.ImportBinding))),
    export_entries: List(esm.ExportEntry),
    /// Module-root name → local-slot map (compiler.CompiledModuleBody.
    /// export_names): the linker looks exported local names up in it to
    /// find the slot whose BoxSlot importers share.
    export_names: Dict(String, Int),
    /// This module's TOTAL raw → resolved projection: `esm.resolve` is the only
    /// way to turn a specifier this module's source wrote into a bundle key.
    specifier_map: esm.SpecifierMap,
    /// [[RequestedModules]] in declaration order: each request's RESOLVED
    /// specifier paired with its merged phase (exactly what `esm.analyze`'s
    /// merge_requests computed — `Deferred` only when EVERY reference is
    /// `import defer * as ns`, since a single eager reference forces
    /// evaluation). One list, so "eager subset of requested" cannot drift:
    /// InnerModuleEvaluation (§16.2.1.5.3.1) walks this list and skips the
    /// `Deferred` entries.
    requested_modules: List(#(esm.Resolved, esm.Phase)),
    /// Exported local name → value the linker seeds into its BoxSlot before the
    /// body runs: `uninitialized` (TDZ) for let/const/class/default, `undefined`
    /// for var/function. Computed by the compiler and carried on
    /// `compiler.CompiledModuleBody.export_seeds`.
    export_seeds: Dict(String, JsValue),
    /// Top-level hoisted function declarations as (name, func_index) into
    /// template.functions. The linker instantiates the *exported* ones before
    /// any body runs, so cyclic function imports are callable (§16.2.1.6.4).
    hoisted_funcs: List(#(String, Int)),
    /// Whether the module body contains a top-level `await`
    /// ([[HasTLA]], §16.2.1.5) — ReadyForSyncExecution returns false for it,
    /// so touching a deferred namespace over it throws a TypeError.
    has_tla: Bool,
  )
}

/// A host (synthetic) module: a module identified by a specifier whose named
/// exports are embedder-provided `JsValue`s (host-defined class constructors,
/// host functions) rather than compiled from JS source. Modelled on the TC39
/// Synthetic Module Record — no dependencies, eager/ready exports, a no-op
/// `[[Evaluate]]`. `import { x } from "<specifier>"` binds straight to the
/// supplied value.
pub type HostModule {
  HostModule(specifier: String, exports: List(#(String, JsValue)))
}

/// One entry of a `ModuleBundle`: the module a specifier names is EITHER
/// compiled from JS source OR an embedder-provided host (synthetic) module —
/// never both, and never neither. The two kinds share one key space, so a
/// specifier cannot be registered as both and no lookup has to pick a winner.
pub type BundleModule {
  SourceModule(compiled: CompiledModule)
  SyntheticModule(host: HostModule)
}

/// A complete compiled module graph — the output of AOT compilation.
/// Pure Erlang term, serializable via term_to_binary. `modules` holds both the
/// compiled source modules and the embedder-provided host (synthetic) ones
/// (see `BundleModule`), keyed by resolved specifier.
pub type ModuleBundle {
  ModuleBundle(entry: String, modules: Dict(String, BundleModule))
}

/// Run `k` on the compiled module a bundle entry holds. A host (synthetic)
/// module carries no source, no imports and no body, so every source-only pass
/// (deferred-import scan, hoisted-function instantiation) skips it and keeps
/// `default`.
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

/// The specifiers of the bundle's SOURCE modules only — the ones that have a
/// body to run, and so the only ones an evaluation phase can leave half-done.
/// A host (synthetic) module's cells are permanently initialized at link time,
/// so a caller rolling back a failed evaluation must not touch it.
pub fn source_specifiers(bundle: ModuleBundle) -> List(String) {
  use acc, spec, bundle_module <- dict.fold(bundle.modules, [])
  use _compiled <- with_source_module(bundle_module, acc)
  [spec, ..acc]
}

// =============================================================================
// Errors
// =============================================================================

/// A failure of AOT compilation (`compile_bundle*`) — everything that can go
/// wrong BEFORE any heap exists. Each variant carries the structured cause
/// from the layer that produced it (graph walk, bytecode compiler); dispatch
/// on the variant, and render the prose with `compile_bundle_error_message`.
///
/// Deliberately carries no `host` type parameter: compilation never allocates
/// in a heap, so it can never produce a heap-carrying error, and its callers
/// can no longer be forced to invent a phantom heap type for it.
pub type CompileBundleError {
  /// The source-graph walk failed: a module failed to parse, a request failed
  /// to resolve or load, or a source-phase import was requested. Match the
  /// inner `graph.GraphError` to tell those apart.
  GraphError(error: graph.GraphError)
  /// Bytecode compilation of the module named `specifier` failed.
  CompileError(specifier: String, error: compiler.CompileError)
}

/// A failure of the LINK/EVALUATE half of the pipeline, where a heap exists.
///
/// Link-time validation failures (§16.2.1.6.4, `link.LinkError`) have no
/// variant of their own: `link_for_evaluation` allocates the JS SyntaxError
/// in the heap right there (its identity and stack ARE the module's rejection
/// value), so they surface as `EvaluationError`.
///
/// NO variant carries a heap. Every function that can fail hands the live heap
/// back BESIDE the result — `#(Heap(host), Result(_, ModuleError))` — so there
/// is exactly one heap channel out of it and no caller can pick the wrong one
/// of two. `EvaluationError`'s value is a `Ref` into that heap.
pub type ModuleError {
  /// Evaluation asked for a resolved specifier the bundle does not contain.
  NotInBundle(specifier: String)
  /// A module threw during evaluation. The thrown value is a `Ref` into the
  /// heap returned alongside this error.
  EvaluationError(value: JsValue)
  /// Evaluation is parked on top-level await and the supplied `finish`
  /// driver did not settle it (only reachable with a non-draining driver —
  /// the dynamic-import path). `promise_data_ref` is the entry module's
  /// [[TopLevelCapability]] promise data: per Evaluate() step 4 the host
  /// must chain onto this promise (and hand any returned jobs to its own
  /// event loop) rather than treat the module as failed.
  EvaluationPending(promise_data_ref: Ref)
}

/// The message a module parked forever on top-level await surfaces with (cf.
/// Node's exit code 13). Shared by `error_message` and the static entry
/// points, which turn `EvaluationPending` into a thrown TypeError.
const tla_never_settled_message = "module evaluation never completed: top-level await promise never settled"

/// The single renderer of a `CompileBundleError`'s user-facing prose. Every
/// layer's own message lives in that layer (`parser.parse_error_to_string`,
/// `compiler.error_message`); this adds the module-pipeline framing (which
/// module, which phase) exactly once.
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

/// Which pipeline PHASE a `CompileBundleError` failed in, as the prefix
/// embedders print in front of `compile_bundle_error_message`. Lives here, next
/// to the variants it maps over, so a new `CompileBundleError` variant is a
/// compile error in exactly one place instead of silently inheriting whatever
/// label a distant caller's `case` fell through to.
pub fn compile_bundle_error_phase(err: CompileBundleError) -> String {
  case err {
    GraphError(error: graph.ParseFailed(..)) -> "SyntaxError: "
    GraphError(error: graph.ResolveFailed(..))
    | GraphError(error: graph.LoadFailed(..)) -> "ResolutionError: "
    // A source-phase import is a link-time SyntaxError (§16.2.1.7.2).
    GraphError(error: graph.SourcePhaseUnsupported(..)) -> "LinkError: "
    CompileError(..) -> "CompileError: "
  }
}

/// The single renderer of a `ModuleError`'s user-facing prose. NOTHING may
/// `string.inspect` a `ModuleError`: its Gleam debug repr structurally
/// contains a whole `Heap`, and would leak Gleam constructor syntax into a
/// guest-visible JS error message.
/// `heap` is the heap the failing call handed back beside the error — the one
/// an `EvaluationError`'s thrown value lives in.
pub fn error_message(err: ModuleError, heap: Heap(host)) -> String {
  case err {
    NotInBundle(specifier:) ->
      "Module '" <> specifier <> "' not found in bundle"
    EvaluationError(value:) -> "Uncaught " <> object.format_error(value, heap)
    EvaluationPending(promise_data_ref: _) -> tla_never_settled_message
  }
}

/// A break in the LINKER's own invariants — never a guest-program error, and
/// never something a host can trigger with a well-formed bundle. Every variant
/// means `build_linked` and the seeding phase disagree about the shape of the
/// graph they just built together.
///
/// These paths used to substitute a wrong-but-plausible value (`JsUndefined`,
/// or a silently dropped map entry), so a linker bug surfaced as an
/// `undefined` binding arbitrarily far away from its cause. They now fail
/// loudly and by name (`assert_link_invariant`).
pub type LinkInvariantBroken {
  /// A module imports from a raw specifier its own `specifier_map` — which is
  /// TOTAL over the module's requests — does not cover.
  UnresolvedDependency(specifier: esm.Raw)
  /// A module of the bundle has no export map. `build_linked` builds one per
  /// `bundle.modules` key, so a miss is a linker bug — and the empty map that
  /// used to stand in for it is the plausible lie "this module exports nothing".
  MissingExportMap(specifier: String)
  /// A module of the bundle has no local-binding map. `preallocate_local_boxes`
  /// builds one per `bundle.modules` key; the empty map that used to stand in
  /// silently unseeds every one of the module's own bindings.
  MissingLocalBoxes(specifier: String)
  /// The dependency's export map has no live cell for a name that
  /// `link.validate` already accepted.
  MissingExportCell(dep: String, name: String)
  /// `import * as ns from dep`, but `dep` has no reserved namespace box —
  /// `reserve_ns_boxes` covers every module in the bundle.
  MissingNamespaceBox(dep: String)
  /// `import defer * as ns from dep`, but `dep` has no reserved deferred proxy
  /// — `needed_deferred_specs` collects exactly these imports.
  MissingDeferredBox(dep: String)
  /// A specifier registered as already-instantiated whose namespace ref does
  /// not read back as a Module Namespace Exotic Object. Reusing nothing here
  /// would silently re-link the module to FRESH cells while its evaluated body
  /// still holds the old ones.
  PreexistingNotANamespace(specifier: String, ref: Ref)
  /// A reserved namespace / deferred-namespace box that does not hold an
  /// object — linking fills every box it reserves, so this is a linker bug.
  /// Dropping the entry would silently deny a registry-keeping host the
  /// module record it needs to reuse on a later import.
  NamespaceBoxCorrupt(specifier: String)
}

/// Renders a `LinkInvariantBroken` for the panic message. Internal-error prose,
/// never guest-visible (cf. compiler.gleam's `panic as "scope analyzer …"`).
pub fn link_invariant_message(broken: LinkInvariantBroken) -> String {
  "arc/module: linker invariant broken: " <> string.inspect(broken)
}

/// Unwrap a linker-internal result. There is no recovery: an invariant break
/// means the linked graph is already inconsistent.
fn assert_link_invariant(result: Result(a, LinkInvariantBroken)) -> a {
  case result {
    Ok(value) -> value
    Error(broken) -> panic as { link_invariant_message(broken) }
  }
}

/// The successful result of `evaluate_bundle`: the entry module's completion
/// value and the entry module's Module Namespace Exotic Object (§10.4.6). Read
/// named exports off `namespace` via its [[Get]] — a live, TDZ-throwing,
/// write-protected view of the export bindings. This is the embedder's
/// `GetModuleNamespace` handle (cf. V8 `Module::GetModuleNamespace`, QuickJS
/// `JS_GetModuleNamespace`).
///
/// `namespace` is not optional: the linker reserves a namespace box for every
/// module of the bundle, so a bundle that evaluated has one. The heap it lives
/// in is the one returned beside this record.
pub type EvaluatedBundle {
  EvaluatedBundle(value: JsValue, namespace: Ref)
}

// =============================================================================
// AOT Compilation (compile_bundle)
// =============================================================================

/// Compile a module and all its dependencies into a self-contained ModuleBundle.
/// `resolve` maps (raw_specifier, referrer) to the dependency's canonical
/// specifier; `load` reads a resolved specifier's source — called once per
/// unique module.
///
/// Composes `graph.load` (resolve/parse/analyze the whole graph) with a
/// per-module bytecode compile. Source-level consumers (bundlers, dev tools)
/// can call `graph.load` themselves and skip the compile entirely — both
/// paths share one graph walk with identical resolution semantics.
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

/// `compile_bundle` plus embedder-provided host (synthetic) modules. A request
/// whose resolved specifier is a key of `host_modules` is treated as a leaf in
/// the graph walk — resolved but never source-loaded — and carried through to
/// the bundle so the linker can bind its imports to the host values.
pub fn compile_bundle_with_hosts(
  entry_specifier: String,
  entry_source: String,
  resolve: fn(String, String) -> Result(String, ResolveError),
  load: fn(String) -> Result(String, LoadError),
  host_modules: Dict(String, HostModule),
) -> Result(ModuleBundle, CompileBundleError) {
  // The host talks in plain strings; the graph walk talks in `esm.Raw` /
  // `esm.Resolved`. This is the boundary: whatever the host resolver returns is
  // by definition a canonical module identity, and so is the entry specifier the
  // embedder named. `esm.resolved_unchecked` records that assertion.
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
  // Host modules first, then the compiled source graph on top: the graph walk
  // treats a host specifier as a leaf, so the only way both could name the same
  // specifier is the entry — whose source the embedder handed us, so it wins.
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

/// Compile one loaded module from the source graph — the import/export
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
  let compiler.CompiledModuleBody(
    template:,
    export_names:,
    hoisted_funcs:,
    export_seeds:,
    has_tla:,
  ) = body

  // [[RequestedModules]] with the phase esm.analyze already merged, keyed by
  // the specifier the host resolved each request to — evaluation never has to
  // look a raw specifier back up.
  let requested_modules =
    list.map(resolved, fn(edge) {
      let #(request, resolved_specifier) = edge
      #(resolved_specifier, request.phase)
    })

  CompiledModule(
    specifier:,
    template:,
    import_bindings: summary.imports,
    export_entries: summary.exports,
    export_names:,
    specifier_map: graph.specifier_map(node),
    requested_modules:,
    export_seeds:,
    hoisted_funcs:,
    has_tla:,
  )
}

// =============================================================================
// Linking — ResolveExport (§16.2.1.6.3) + import/re-export checks (§16.2.1.6.4)
// =============================================================================
//
// The resolve algorithm and the SyntaxError-producing import/re-export checks
// live in `arc/link`, operating on a minimal `link.LinkableGraph` view. The VM
// projects a bundle into that view (`linkable_of_bundle`) and calls
// `link.validate` / `link.resolve_export` / `link.exported_names`.

/// Project a compiled bundle onto the shared `link.LinkableGraph` view.
/// `link.project_module` resolves each module's raw specifiers through its own
/// (TOTAL) specifier map exactly here, once — so nothing on the linking path
/// downstream can meet an unresolved dependency. An uncovered specifier is the
/// linker-invariant break `import_seeds` fails on; fail at its cause.
///
/// A `ModuleBundle` is keyed by module identity — every key came out of a
/// `graph.load` walk (or is a host module the embedder registered under one) —
/// so re-tagging the keys as `esm.Resolved` restates a fact, it does not assume
/// one.
fn linkable_of_bundle(bundle: ModuleBundle) -> link.LinkableGraph {
  use acc, specifier, bundle_module <- dict.fold(bundle.modules, dict.new())
  let linkable = case bundle_module {
    SourceModule(m) ->
      link.project_module(m.import_bindings, m.export_entries, m.specifier_map)
      |> result.map_error(UnresolvedDependency)
      |> assert_link_invariant
    // A host module projects as a module with no imports and a LocalExport per
    // supplied name (its cells are pre-seeded with the host values at link
    // time), so the resolver's import/re-export checks and `exported_names`
    // see it.
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
// Module status / error registry
// =============================================================================
//
// Deferred evaluation needs module evaluation state that BOTH the link-time
// DFS evaluator and a deferred namespace's runtime trigger (which fires while
// some other module's body is mid-execution) can observe. Gleam data is
// immutable, so that shared state lives in the heap, behind the typed
// accessors of `arc/module/registry` — the single owner of every hidden
// global-object cache the module system keeps (status, errors, namespaces).

// =============================================================================
// Runtime Evaluation (evaluate_bundle)
// =============================================================================

/// The result of linking: every module's binding cells, pre-allocated before
/// any body runs (§16.2 instantiation) so cyclic/self imports reference the
/// same live cells. Immutable once built; threaded read-only through the DFS.
pub type Linked {
  Linked(
    /// specifier → local binding name → BoxSlot ref (seeded TDZ/undefined).
    local_boxes: Dict(String, Dict(String, Ref)),
    /// specifier → exported name → BoxSlot ref (LocalExport and re-exports
    /// resolved to the owning module's cell; namespace re-exports → a box
    /// wrapping the target's namespace object).
    exports: Dict(String, Dict(String, Ref)),
    /// specifier → a BoxSlot wrapping that module's cached Module Namespace
    /// Exotic Object (seeded for `import * as ns`).
    namespace_boxes: Dict(String, Ref),
    /// specifier → a BoxSlot wrapping that module's Deferred Module Namespace
    /// (seeded for `import defer * as ns`). Only present for modules some
    /// importer in the bundle defers, or that were already registered as
    /// deferred in the realm. The object is a proxy whose traps trigger
    /// EvaluateSync on first relevant access (see build_deferred_namespaces).
    deferred_boxes: Dict(String, Ref),
  )
}

/// Where a module is in its per-DFS lifecycle. ONE state per module — the
/// three flavours are mutually exclusive by construction, so "evaluated and
/// still evaluating" or "errored but still evaluating" cannot be built.
type ModuleEvalStatus {
  /// The body is running (or a re-entrant trigger observed the cycle).
  Evaluating
  /// The body completed.
  Evaluated
  /// The body (or a dependency's) threw; the value is rethrown, never re-run.
  Failed(value: JsValue)
}

/// The immutable per-realm boot inputs every module body's freshly booted
/// State is constructed from. Bundling them means the DFS chain
/// (`eval_module_inner` ↔ `eval_module_body` → `run_module_with_referrer`)
/// threads ONE param, and adding a boot input is one field here plus the
/// handful of construction sites — not seven signatures.
pub type BootCtx(host) {
  BootCtx(
    builtins: Builtins,
    global_object: Ref,
    host_hooks: host_hooks.HostHooks,
    can_block: Bool,
    extend_262: Option(state.Extend262(host)),
    finish: fn(state.State(host)) -> state.State(host),
  )
}

/// Internal evaluation state threaded through the DFS.
type EvalState(host) {
  EvalState(
    heap: Heap(host),
    /// Specifier → its lifecycle state, for the modules this DFS has touched.
    /// The realm-wide heap registry (`arc/module/registry`) is the fallback
    /// for modules an *outer* evaluation or a re-entrant deferred trigger
    /// touched — see `module_eval_status`.
    modules: Dict(String, ModuleEvalStatus),
    /// Promise jobs left queued by module bodies when the `finish` driver
    /// does not drain (dynamic import) — handed back to the host event loop.
    jobs: List(value.Job),
  )
}

/// Every specifier this DFS finished evaluating — what a registry-keeping
/// host records so their bodies never re-run.
fn evaluated_specifiers(state: EvalState(host)) -> Set(String) {
  use acc, spec, status <- dict.fold(state.modules, set.new())
  case status {
    Evaluated -> set.insert(acc, spec)
    Evaluating | Failed(_) -> acc
  }
}

/// A module's lifecycle state: this DFS's own view first, else the realm's
/// heap-resident registry (an outer evaluation, an earlier bundle, or a
/// deferred-namespace trigger that fired mid-body). A cached ERROR wins over a
/// stale ~evaluating~ mark: an async module rejected after parking on
/// top-level await keeps its status but is `Failed`.
fn module_eval_status(
  state: EvalState(host),
  global_object: Ref,
  specifier: String,
) -> Option(ModuleEvalStatus) {
  use <- option.lazy_or(
    dict.get(state.modules, specifier) |> option.from_result,
  )
  use <- option.lazy_or(
    registry.read_module_error(state.heap, global_object, specifier)
    |> option.map(Failed),
  )
  case registry.read_module_status(state.heap, global_object, specifier) {
    Some(registry.Evaluated) -> Some(Evaluated)
    Some(registry.Evaluating) -> Some(Evaluating)
    None -> None
  }
}

/// Record `specifier`'s new lifecycle state in this DFS's view.
fn set_eval_status(
  state: EvalState(host),
  specifier: String,
  status: ModuleEvalStatus,
) -> EvalState(host) {
  EvalState(..state, modules: dict.insert(state.modules, specifier, status))
}

/// Fold `items` while threading `state`, short-circuiting on the first Error.
/// `f` receives the running state, the accumulated Ok-value, and the item.
fn try_fold_state(
  items: List(i),
  state: s,
  initial: a,
  f: fn(s, a, i) -> #(s, Result(a, e)),
) -> #(s, Result(a, e)) {
  use acc, item <- list.fold(items, #(state, Ok(initial)))
  case acc {
    #(_, Error(_)) -> acc
    #(state, Ok(value)) -> f(state, value, item)
  }
}

/// A linked-but-not-yet-evaluated bundle: every binding cell and namespace
/// object pre-allocated (§16.2 instantiation), exported hoisted functions
/// instantiated. The entry module's Module Namespace Exotic Object is live
/// before evaluation — read it out of the heap with `entry_namespace_of` (a
/// function, not a stored field, so it cannot go stale against a heap the
/// caller has since advanced), so a host can publish it in its module registry
/// first and a re-entrant dynamic import of the evaluating module resolves to
/// the same namespace instead of re-evaluating (§16.2.1.8).
pub type LinkedBundle {
  LinkedBundle(bundle: ModuleBundle, linked: Linked)
}

/// The entry module's Module Namespace Exotic Object of a linked bundle, read
/// live out of `heap`.
pub fn entry_namespace_of(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
) -> Ref {
  entry_namespace(linked_bundle.linked, linked_bundle.bundle.entry, heap)
}

/// Link phase of `evaluate_bundle` (§16.2.1.6.4): resolve every import and
/// indirect re-export across the whole graph BEFORE evaluating any body —
/// missing or ambiguous exports are a SyntaxError at link time, surfaced as
/// `EvaluationError`. On success, pre-allocates every binding cell and
/// namespace object and instantiates exported hoisted function declarations.
///
/// The returned heap is the live one either way: on failure it holds the
/// freshly allocated JS error object the `EvaluationError` names.
pub fn link_for_evaluation(
  bundle: ModuleBundle,
  heap: Heap(host),
  builtins: Builtins,
) -> #(Heap(host), Result(LinkedBundle, ModuleError)) {
  link_for_evaluation_reusing(bundle, heap, builtins, dict.new(), dict.new())
}

/// `link_for_evaluation` with a registry of already-instantiated modules:
/// `preexisting` maps a resolved specifier to its existing Module Namespace
/// Exotic Object. Those modules keep their namespace identity and live export
/// cells (§16.2.1.8 requires the same module record for the same specifier);
/// only the remaining modules get fresh cells. Callers seed the evaluation
/// with the same specifiers (see `evaluate_linked_tracking`) so preexisting
/// bodies never re-run.
pub fn link_for_evaluation_reusing(
  bundle: ModuleBundle,
  heap: Heap(host),
  builtins: Builtins,
  preexisting: Dict(String, Ref),
  preexisting_deferred: Dict(String, Ref),
) -> #(Heap(host), Result(LinkedBundle, ModuleError)) {
  let lg = linkable_of_bundle(bundle)
  case link.validate(lg) {
    Error(link_error) -> {
      let #(heap, err) =
        common.make_error(
          heap,
          builtins,
          common.SyntaxErr,
          link.link_error_message(link_error),
        )
      #(heap, Error(EvaluationError(err)))
    }
    Ok(Nil) -> {
      // Expand each preexisting namespace ref into (ref, export-name → box):
      // the export map is final for an instantiated module and is exactly
      // what importers link against. A ref that is not a namespace object
      // cannot be silently skipped — that module would then get FRESH cells
      // from `build_linked` while its already-evaluated body still holds the
      // old ones, and every importer would read `undefined`.
      let pre =
        dict.fold(preexisting, dict.new(), fn(acc, spec, ns_ref) {
          case heap.read(heap, ns_ref) {
            Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
              dict.insert(acc, spec, #(ns_ref, exports))
            _ ->
              assert_link_invariant(
                Error(PreexistingNotANamespace(spec, ns_ref)),
              )
          }
        })
      // A preexisting module's live export map must hold a cell for every name
      // THIS bundle's fresh parse of it exports. Only a host loader that served
      // different source for a specifier it already served (a file edited
      // between two `import()`s) can break that, so it is a guest-visible link
      // error, not a linker invariant — and it must be caught before
      // `build_linked`, whose reuse of the live map assumes the two agree.
      case stale_reused_export(bundle, lg, pre) {
        Some(#(spec, name)) -> {
          let #(heap, err) =
            common.make_error(
              heap,
              builtins,
              common.SyntaxErr,
              stale_reused_export_message(spec, name),
            )
          #(heap, Error(EvaluationError(err)))
        }
        None -> {
          // Instantiate: pre-allocate every binding cell + namespace object,
          // then create exported function-declaration closures so cyclic
          // function imports are callable before any body runs (§16.2.1.6.4
          // step 9).
          let #(heap, linked, deferred_to_fill) =
            build_linked(bundle, heap, pre, preexisting_deferred)
          // Now that the full Linked record exists, write the reserved deferred
          // namespace proxies — their traps capture the complete bundle+linked
          // so a first access can run the deferred subgraph's evaluation.
          let heap =
            list.fold(deferred_to_fill, heap, fn(heap, pair) {
              let #(spec, proxy_ref) = pair
              fill_deferred_namespace(
                heap,
                builtins,
                bundle,
                linked,
                spec,
                proxy_ref,
              )
            })
          let heap =
            instantiate_hoisted_functions(
              bundle,
              linked,
              builtins,
              heap,
              set.from_list(dict.keys(pre)),
            )
          #(heap, Ok(LinkedBundle(bundle:, linked:)))
        }
      }
    }
  }
}

/// Evaluation phase of `evaluate_bundle`: execute module bodies in DFS
/// post-order (dependencies first). Returns the live heap and, beside it, the
/// entry module's completion value and post-evaluation namespace.
pub fn evaluate_linked(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
  boot: BootCtx(host),
) -> #(Heap(host), Result(EvaluatedBundle, ModuleError)) {
  let #(heap, _evaluated, _jobs, result) =
    evaluate_linked_tracking(linked_bundle, heap, boot, set.new())
  // Static entry points drive a draining `finish`, so pending here means an
  // awaited promise can never settle — keep the historical "never completed"
  // throw (cf. Node's exit code 13 for unsettled top-level await), as a real
  // JS TypeError object rather than a bare string.
  case result {
    Error(EvaluationPending(promise_data_ref: _)) -> {
      let #(heap, err) =
        common.make_error(
          heap,
          boot.builtins,
          common.TypeErr,
          tla_never_settled_message,
        )
      #(heap, Error(EvaluationError(value: err)))
    }
    other -> #(heap, other)
  }
}

/// `evaluate_linked` for registry-aware hosts: modules in `already_evaluated`
/// are treated as done (their bodies never run — pair with
/// `link_for_evaluation_reusing` so their cells are the live preexisting
/// ones). Returns the live heap, then the full set of successfully evaluated
/// specifiers alongside the result, so the host can register exactly the
/// modules whose bodies ran (even when a later module's evaluation threw).
pub fn evaluate_linked_tracking(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
  boot: BootCtx(host),
  already_evaluated: Set(String),
) -> #(
  Heap(host),
  Set(String),
  List(value.Job),
  Result(EvaluatedBundle, ModuleError),
) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  let modules =
    set.fold(already_evaluated, dict.new(), fn(acc, spec) {
      dict.insert(acc, spec, Evaluated)
    })
  let state = EvalState(heap:, modules:, jobs: [])
  let #(state, result) =
    eval_module_inner(bundle, linked, state, bundle.entry, boot)
  // Surface the entry namespace alongside the completion value (post-eval,
  // so its bindings are initialized — no TDZ for the embedder to hit).
  // `EvalState.heap` is the live heap on every path, error or not.
  let result = {
    use completion_value <- result.map(result)
    EvaluatedBundle(
      value: completion_value,
      namespace: entry_namespace(linked, bundle.entry, state.heap),
    )
  }
  #(state.heap, evaluated_specifiers(state), state.jobs, result)
}

/// Read a reserved namespace / deferred-namespace box as the object it holds.
/// Linking fills every box it reserves, so a box that holds anything else (or
/// nothing) is a linker invariant break — the ONE reading of a reserved box, so
/// no caller can decide for itself that a corrupt box merely means "no
/// namespace here".
fn read_namespace_box(
  heap: Heap(host),
  spec: String,
  box: Ref,
) -> Result(Ref, LinkInvariantBroken) {
  case heap.read_box(heap, box) {
    Some(JsObject(ns_ref)) -> Ok(ns_ref)
    Some(_) | None -> Error(NamespaceBoxCorrupt(specifier: spec))
  }
}

/// Read every box in `boxes` as the object it holds — a corrupt box is a linker
/// invariant break, not an entry to skip.
fn read_box_dict(
  boxes: Dict(String, Ref),
  heap: Heap(host),
) -> List(#(String, Ref)) {
  use acc, spec, box <- dict.fold(boxes, [])
  let ns_ref = read_namespace_box(heap, spec, box) |> assert_link_invariant
  [#(spec, ns_ref), ..acc]
}

/// Every module in a linked bundle paired with its Module Namespace Exotic
/// Object — what a registry-keeping host records so a later import of any
/// graph module (entry or dependency) reuses the same record (§16.2.1.8).
pub fn linked_namespaces(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
) -> List(#(String, Ref)) {
  read_box_dict(linked_bundle.linked.namespace_boxes, heap)
}

/// Every Deferred Module Namespace in a linked bundle, for the registry: a
/// later `import defer` / `import.defer()` of the same module must yield the
/// identical object ([[DeferredNamespace]] is per module record).
pub fn linked_deferred_namespaces(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
) -> List(#(String, Ref)) {
  read_box_dict(linked_bundle.linked.deferred_boxes, heap)
}

/// Why `get_or_create_deferred_namespace` could not hand back a Deferred
/// Module Namespace: the caller named a specifier the bundle does not contain.
/// That is the ONLY failure a caller can act on — a reserved box that does not
/// hold its namespace is a linker bug, and is panicked on as
/// `NamespaceBoxCorrupt` like every other reserved-box read.
pub type DeferredNamespaceError {
  /// `spec` names neither a source nor a host module of the linked bundle.
  DeferredSpecifierNotInBundle(specifier: String)
}

/// The Deferred Module Namespace for `spec`, creating one if no importer in
/// the bundle deferred it statically (the dynamic `import.defer()` path).
///
/// Host (synthetic) modules qualify: they have export cells and namespace
/// objects like any source module, and their `[[Evaluate]]` is a no-op, so a
/// deferred namespace over one is a namespace whose trigger does nothing.
pub fn get_or_create_deferred_namespace(
  heap: Heap(host),
  builtins: Builtins,
  linked_bundle: LinkedBundle,
  spec: String,
) -> #(Heap(host), Result(Ref, DeferredNamespaceError)) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  use <- bool.lazy_guard(dict.has_key(bundle.modules, spec) == False, fn() {
    #(heap, Error(DeferredSpecifierNotInBundle(specifier: spec)))
  })
  case dict.get(linked.deferred_boxes, spec) {
    Ok(box) -> #(
      heap,
      Ok(read_namespace_box(heap, spec, box) |> assert_link_invariant),
    )
    Error(Nil) -> {
      let #(heap, proxy_ref) = heap.reserve(heap)
      let heap = heap.root(heap, proxy_ref)
      let heap =
        fill_deferred_namespace(heap, builtins, bundle, linked, spec, proxy_ref)
      #(heap, Ok(proxy_ref))
    }
  }
}

/// Evaluate a compiled module bundle. Links the whole graph (pre-allocating
/// every binding cell), then executes module bodies in DFS post-order
/// (dependencies first). Returns the entry module's completion value.
///
/// Module bodies boot with `host_hooks.default_host_hooks()` (no embedder host
/// capabilities); embedders that supply Atomics capabilities use
/// `evaluate_bundle_with_hooks`.
pub fn evaluate_bundle(
  bundle: ModuleBundle,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  finish: fn(state.State(host)) -> state.State(host),
) -> #(Heap(host), Result(EvaluatedBundle, ModuleError)) {
  evaluate_bundle_with_hooks(
    bundle,
    heap,
    BootCtx(
      builtins:,
      global_object:,
      host_hooks: host_hooks.default_host_hooks(),
      can_block: True,
      extend_262: None,
      finish:,
    ),
  )
}

/// `evaluate_bundle` with the embedder's `host_hooks.HostHooks` (host
/// capabilities such as the Atomics blocking wait / wake delivery). Each
/// module body boots a FRESH State, so the hooks are threaded into every
/// body's `RealmCtx` at construction rather than installed after the fact —
/// a module's top level may hit a blocking `Atomics.wait` before any host
/// function has run.
pub fn evaluate_bundle_with_hooks(
  bundle: ModuleBundle,
  heap: Heap(host),
  boot: BootCtx(host),
) -> #(Heap(host), Result(EvaluatedBundle, ModuleError)) {
  case link_for_evaluation(bundle, heap, boot.builtins) {
    #(heap, Error(err)) -> #(heap, Error(err))
    #(heap, Ok(linked_bundle)) -> evaluate_linked(linked_bundle, heap, boot)
  }
}

/// The entry module's Module Namespace Exotic Object, read out of the rooted
/// box the linker reserved for it (`build_linked`). This is what
/// `evaluate_bundle` hands back as `EvaluatedBundle.namespace`. `build_linked`
/// reserves a namespace box for EVERY module of the bundle, entry included, so
/// a missing box is a linker bug — never "this bundle has no namespace".
fn entry_namespace(linked: Linked, entry: String, heap: Heap(host)) -> Ref {
  let box =
    dict.get(linked.namespace_boxes, entry)
    |> result.replace_error(MissingNamespaceBox(entry))
    |> assert_link_invariant
  read_namespace_box(heap, entry, box) |> assert_link_invariant
}

/// Read a named export off a Module Namespace Exotic Object (the `namespace`
/// from `EvaluatedBundle`), at the heap level — the embedder's `ns.name`,
/// without needing a VM `State`. Returns the export's live binding value, or
/// `None` if `namespace` isn't a module namespace, has no such export, or the
/// binding is still uninitialized (TDZ). Unlike the in-VM namespace [[Get]] (§
/// 10.4.6.8), an uninitialized binding yields `None` rather than throwing a
/// ReferenceError — there is no JS context to throw into here, and after a
/// completed `evaluate_bundle` an entry export is always initialized anyway.
pub fn read_export(
  heap: Heap(host),
  namespace: JsValue,
  name: String,
) -> Option(JsValue) {
  use ref <- option.then(case namespace {
    JsObject(ref) -> Some(ref)
    _ -> None
  })
  use exports <- option.then(case heap.read(heap, ref) {
    Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) -> Some(exports)
    _ -> None
  })
  use box <- option.then(dict.get(exports, name) |> option.from_result)
  case heap.read_box(heap, box) {
    Some(value.JsUninitialized) | None -> None
    Some(v) -> Some(v)
  }
}

/// DFS post-order evaluation of a single module and its dependencies. The
/// returned `EvalState.heap` is the live heap on every path — an error's values
/// are Refs into it, so nothing needs to carry a heap of its own.
fn eval_module_inner(
  bundle: ModuleBundle,
  linked: Linked,
  state: EvalState(host),
  specifier: String,
  boot: BootCtx(host),
) -> #(EvalState(host), Result(JsValue, ModuleError)) {
  // One lookup, three outcomes — the module is absent, has no body, or has one.
  case dict.get(bundle.modules, specifier) {
    // Nothing to evaluate and no cells to reach: whatever the realm's status
    // registry says about `specifier`, THIS bundle cannot run it.
    Error(Nil) -> #(state, Error(NotInBundle(specifier:)))
    // A host (synthetic) module has no body and ready exports, so its
    // `[[Evaluate]]` is a no-op — always "done".
    Ok(SyntheticModule(_)) -> #(state, Ok(JsUndefined))
    Ok(SourceModule(compiled)) ->
      case module_eval_status(state, boot.global_object, specifier) {
        // Body already ran (in this DFS, or re-entrantly via a
        // deferred-namespace trigger, or in an earlier bundle sharing this
        // realm).
        Some(Evaluated) -> #(state, Ok(JsUndefined))
        // Cached error — re-throw, never re-evaluate.
        Some(Failed(err_val)) -> #(state, Error(EvaluationError(err_val)))
        // Circular dependency (in this DFS, or in an outer evaluation a
        // deferred-namespace trigger re-entered from) — return without
        // re-entering.
        Some(Evaluating) -> #(state, Ok(JsUndefined))
        None ->
          eval_module_body(bundle, linked, state, specifier, compiled, boot)
      }
  }
}

/// Evaluate a module's dependencies and then its body.
fn eval_module_body(
  bundle: ModuleBundle,
  linked: Linked,
  state: EvalState(host),
  specifier: String,
  compiled: CompiledModule,
  boot: BootCtx(host),
) -> #(EvalState(host), Result(JsValue, ModuleError)) {
  let BootCtx(builtins:, global_object:, ..) = boot
  // Mark as evaluating
  let state = set_eval_status(state, specifier, Evaluating)

  // Evaluate dependencies first (DFS post-order), following the
  // §16.2.1.5.3.1 InnerModuleEvaluation evaluationList: requests are visited
  // in declaration order; an ~evaluation~-phase request evaluates the module
  // itself, while a ~defer~-phase request only evaluates the module's
  // ASYNCHRONOUS transitive dependencies (its synchronous evaluation is
  // triggered later, by first access on the deferred namespace).
  let #(state, dep_result) = {
    use state, Nil, #(resolved_dep, phase) <- try_fold_state(
      compiled.requested_modules,
      state,
      Nil,
    )
    let dep_specifier = esm.resolved_text(resolved_dep)
    let to_evaluate = case phase {
      esm.Evaluation -> [dep_specifier]
      esm.Deferred ->
        gather_async_transitive_deps(
          bundle,
          state,
          global_object,
          dep_specifier,
          set.new(),
        ).0
    }
    use state, Nil, dep <- try_fold_state(to_evaluate, state, Nil)
    let #(state, r) = eval_module_inner(bundle, linked, state, dep, boot)
    #(state, result.replace(r, Nil))
  }

  case dep_result {
    // A dependency parked on top-level await is not a failure — propagate
    // without caching an error (the dependency may still complete later).
    Error(EvaluationPending(promise_data_ref: _) as err) -> #(state, Error(err))
    Error(err) -> {
      // Dependency failed — cache the error on this module too. A dependency
      // that isn't in the bundle at all has no thrown value of its own, so
      // allocate a real TypeError for it (never a Gleam debug repr).
      let #(heap, error_val) = case err {
        EvaluationError(value: val) -> #(state.heap, val)
        NotInBundle(..) | EvaluationPending(..) ->
          common.make_error(
            state.heap,
            builtins,
            common.TypeErr,
            error_message(err, state.heap),
          )
      }
      let heap =
        registry.write_module_error(heap, global_object, specifier, error_val)
      let state = EvalState(..state, heap:)
      let state = set_eval_status(state, specifier, Failed(error_val))
      #(state, Error(err))
    }
    Ok(Nil) -> {
      // Bindings already exist (pre-allocated at link time). Seed the slots:
      // imports as boxed captures in slots 0..N-1 (each the *exporter's* live
      // cell), plus this module's own export cells into their declared slots so
      // the body reads/writes through the shared boxes.
      let seeds =
        import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
        |> assert_link_invariant
        |> list.append(own_export_seeds(linked, compiled))

      // Publish [[Status]] = ~evaluating~ in the realm's heap-resident status
      // registry so a deferred-namespace trigger firing inside this body (or
      // a nested one) observes the cycle and throws instead of re-entering.
      let heap =
        registry.write_module_status(
          state.heap,
          global_object,
          specifier,
          registry.Evaluating,
        )
      case
        run_module_with_referrer(
          specifier,
          compiled.template,
          heap,
          seeds,
          boot,
        )
      {
        entry.ModuleError(error: vm_err) -> {
          let #(heap, error_val) =
            common.make_error(
              heap,
              builtins,
              common.TypeErr,
              "InternalError: " <> vm_error_message(vm_err),
            )
          let heap =
            registry.clear_module_status(heap, global_object, specifier)
          let heap =
            registry.write_module_error(
              heap,
              global_object,
              specifier,
              error_val,
            )
          let state = EvalState(..state, heap:)
          let state = set_eval_status(state, specifier, Failed(error_val))
          #(state, Error(EvaluationError(error_val)))
        }
        entry.ModuleThrow(value: thrown_val, heap: new_heap, jobs:) -> {
          let new_heap =
            registry.clear_module_status(new_heap, global_object, specifier)
          let new_heap =
            registry.write_module_error(
              new_heap,
              global_object,
              specifier,
              thrown_val,
            )
          let state =
            EvalState(
              ..state,
              heap: new_heap,
              jobs: list.append(state.jobs, jobs),
            )
          let state = set_eval_status(state, specifier, Failed(thrown_val))
          #(state, Error(EvaluationError(thrown_val)))
        }
        entry.ModuleOk(value: val, heap: new_heap, locals: _, jobs:) -> {
          let new_heap =
            registry.write_module_status(
              new_heap,
              global_object,
              specifier,
              registry.Evaluated,
            )
          let state =
            EvalState(
              ..state,
              heap: new_heap,
              jobs: list.append(state.jobs, jobs),
            )
          let state = set_eval_status(state, specifier, Evaluated)
          #(state, Ok(val))
        }
        // Parked on top-level await (non-draining driver): not evaluated, not
        // errored — the body will resume on the host's event loop. Hand back
        // any queued jobs and surface the [[TopLevelCapability]] promise.
        entry.ModulePending(promise_data_ref:, heap: new_heap, jobs:) -> {
          let state =
            EvalState(
              ..state,
              heap: new_heap,
              jobs: list.append(state.jobs, jobs),
            )
          #(state, Error(EvaluationPending(promise_data_ref:)))
        }
      }
    }
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Run a module body with `RealmCtx.import_referrer` set to its resolved
/// specifier on the body's freshly booted State. An ImportCall inside the
/// body captures this as its referencingScriptOrModule (§16.2.1.8
/// HostLoadImportedModule), so nested dynamic imports resolve relative to the
/// importing MODULE, not the realm's entry script.
///
/// The referrer is per-boot ENGINE state on `RealmCtx`, not a mutable
/// globalThis property, so there is nothing to save/restore around the body
/// and guest JS can neither read nor forge it.
fn run_module_with_referrer(
  specifier: String,
  template: value.FuncTemplate,
  heap: Heap(host),
  seeds: List(#(Int, JsValue)),
  boot: BootCtx(host),
) -> entry.ModuleResult(host) {
  entry.run_module(
    template,
    heap,
    boot.builtins,
    boot.global_object,
    seeds,
    boot.host_hooks,
    Some(specifier),
    boot.can_block,
    boot.extend_262,
    boot.finish,
  )
}

/// Allocate a GC-rooted BoxSlot holding `val`. Module binding cells are rooted
/// so they survive collection between a dependency being linked and its
/// importer's body running (they live for the duration of the module graph).
fn alloc_box(heap: Heap(host), val: JsValue) -> #(Heap(host), Ref) {
  let #(heap, ref) = heap.alloc(heap, BoxSlot(val))
  #(heap.root(heap, ref), ref)
}

/// For each spec: allocate a rooted namespace box. If `preexisting` yields a
/// ref the box wraps it; otherwise reserve+root a fresh object ref, box it,
/// and record `#(spec, obj)` in the returned fill-list for the caller to
/// write once the object's contents are known.
fn reserve_ns_boxes(
  heap: Heap(host),
  specs: List(String),
  preexisting: fn(String) -> Result(Ref, Nil),
) -> #(Heap(host), Dict(String, Ref), List(#(String, Ref))) {
  use #(heap, boxes, fresh), spec <- list.fold(specs, #(heap, dict.new(), []))
  case preexisting(spec) {
    Ok(existing) -> {
      let #(heap, box) = alloc_box(heap, JsObject(existing))
      #(heap, dict.insert(boxes, spec, box), fresh)
    }
    Error(Nil) -> {
      let #(heap, obj) = heap.reserve(heap)
      let heap = heap.root(heap, obj)
      let #(heap, box) = alloc_box(heap, JsObject(obj))
      #(heap, dict.insert(boxes, spec, box), [#(spec, obj), ..fresh])
    }
  }
}

// -----------------------------------------------------------------------------
// Instantiation — pre-allocate every binding cell + namespace object (§16.2).
// -----------------------------------------------------------------------------

/// Build the whole graph's binding cells before any body runs, so cyclic and
/// self imports reference the same live cells (and observe TDZ correctly).
///
/// `preexisting` (specifier → (namespace object, its export map)) marks
/// modules instantiated by an earlier bundle in the same realm: their
/// namespace object and export cells are reused as-is, so identity and live
/// bindings are shared across bundles.
fn build_linked(
  bundle: ModuleBundle,
  heap: Heap(host),
  preexisting: Dict(String, #(Ref, Dict(String, Ref))),
  preexisting_deferred: Dict(String, Ref),
) -> #(Heap(host), Linked, List(#(String, Ref))) {
  let #(heap, local_boxes) = preallocate_local_boxes(bundle, heap, preexisting)
  // Host (synthetic) modules join the source modules: they need namespace
  // objects (for `import * as ns`) and export maps built below, exactly like a
  // source module whose cells are already initialized.
  let specs = dict.keys(bundle.modules)
  // Reserve a namespace object ref per module, then a rooted box wrapping it,
  // up front so cyclic / star-reached namespace re-exports resolve to a ref.
  // Preexisting modules reuse their registered namespace object instead.
  let #(heap, namespace_boxes, ns_to_fill) =
    reserve_ns_boxes(heap, specs, fn(spec) {
      dict.get(preexisting, spec) |> result.map(fn(p) { p.0 })
    })
  // Deferred namespaces (`import defer * as ns`): reserve a rooted proxy ref
  // per deferred-imported module. Realm-registered deferred namespaces are
  // reused for identity (§ GetModuleNamespace: one [[DeferredNamespace]] per
  // module record); fresh ones are written by the caller once the complete
  // Linked record exists (their traps capture it).
  let #(heap, deferred_boxes, deferred_to_fill) =
    reserve_ns_boxes(heap, needed_deferred_specs(bundle), dict.get(
      preexisting_deferred,
      _,
    ))
  // Resolve every exported name to a cell: a local binding's box (ResolvedTo)
  // or the target's namespace box (ResolvedNamespace — `export * as ns`,
  // including names reached transitively through `export *`). A preexisting
  // module's export map is already final — use it directly.
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
                // §16.2.1.6.3 ResolveExport: a name reached only through
                // `export *` that resolves ambiguously (or not at all) is NOT
                // an exported name of the namespace — deliberately excluded, no
                // cell. Importing it directly is a link error, already caught by
                // `link.validate` above.
                link.Unresolvable | link.Ambiguous -> map
                // Resolved, so a cell MUST exist: every module has local boxes,
                // a namespace box, and (if deferred-imported) a deferred proxy.
                link.ResolvedTo(owner, binding) ->
                  dict.get(local_boxes, esm.resolved_text(owner))
                  |> result.try(dict.get(_, binding))
                  |> result.replace_error(MissingExportCell(
                    esm.resolved_text(owner),
                    binding,
                  ))
                  |> assert_link_invariant
                  |> dict.insert(map, name, _)
                link.ResolvedNamespace(target) ->
                  dict.get(namespace_boxes, esm.resolved_text(target))
                  |> result.replace_error(
                    MissingNamespaceBox(esm.resolved_text(target)),
                  )
                  |> assert_link_invariant
                  |> dict.insert(map, name, _)
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
  // Write each reserved namespace object now that its export map is complete
  // (preexisting modules were never reserved — their objects are untouched).
  let heap =
    list.fold(ns_to_fill, heap, fn(heap, entry) {
      let #(spec, obj) = entry
      // `exports` was just folded over these very specifiers, so a miss is a
      // linker bug — an empty namespace would read as "exports nothing".
      let exp =
        dict.get(exports, spec)
        |> result.replace_error(MissingExportMap(spec))
        |> assert_link_invariant
      heap.write(heap, obj, namespace_slot(exp))
    })
  #(
    heap,
    Linked(local_boxes:, exports:, namespace_boxes:, deferred_boxes:),
    deferred_to_fill,
  )
}

/// GatherAsynchronousTransitiveDependencies ( module ): the modules in
/// `spec`'s whole dependency graph (following BOTH phases) that have
/// top-level await and are not already evaluated/evaluating — a ~defer~
/// request pre-evaluates exactly these, so a later synchronous trigger never
/// needs to run an async body. Returns (modules in discovery order, seen).
fn gather_async_transitive_deps(
  bundle: ModuleBundle,
  state: EvalState(host),
  global_object: Ref,
  spec: String,
  seen: Set(String),
) -> #(List(String), Set(String)) {
  use <- bool.guard(set.contains(seen, spec), #([], seen))
  let seen = set.insert(seen, spec)
  // "Started" — this DFS's own view, or the realm's status registry. A module
  // whose evaluation FAILED is not "started": its cached error is rethrown by
  // whichever eval_module_inner reaches it.
  let already_started = case dict.get(state.modules, spec) {
    Ok(Evaluated) | Ok(Evaluating) -> True
    Ok(Failed(_)) | Error(Nil) ->
      registry.read_module_status(state.heap, global_object, spec) != None
  }
  use <- bool.guard(already_started, #([], seen))
  case dict.get(bundle.modules, spec) {
    // Not in this bundle, or a host module (no body, so no top-level await).
    Error(Nil) | Ok(SyntheticModule(_)) -> #([], seen)
    Ok(SourceModule(m)) ->
      case m.has_tla {
        True -> #([spec], seen)
        False ->
          list.fold(m.requested_modules, #([], seen), fn(acc, request) {
            let #(found, seen) = acc
            let #(dep_specifier, _phase) = request
            let #(more, seen) =
              gather_async_transitive_deps(
                bundle,
                state,
                global_object,
                esm.resolved_text(dep_specifier),
                seen,
              )
            #(list.append(found, more), seen)
          })
      }
  }
}

/// The ~defer~ arm of the proposal's ContinueDynamicImport:
/// GatherAsynchronousTransitiveDependencies(entry), then Evaluate() each
/// gathered module — a dynamic `import.defer()` pre-runs the entry's
/// asynchronous transitive dependencies so a later synchronous trigger never
/// has to execute an async body. Bodies run with the (non-draining) `finish`
/// driver; a module parked on top-level await is not an error — it surfaces
/// in the `Ok` list as (gathered specifier, its [[TopLevelCapability]]
/// promise data ref) so the host can chain the import promise's settlement
/// onto it (an empty list means every gathered body completed
/// synchronously). A throw during evaluation stops the walk and surfaces as
/// `Error`.
pub fn evaluate_async_transitive_deps(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
  boot: BootCtx(host),
) -> #(Heap(host), List(value.Job), Result(List(#(String, Ref)), ModuleError)) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  let eval_state = EvalState(heap:, modules: dict.new(), jobs: [])
  let #(to_evaluate, _seen) =
    gather_async_transitive_deps(
      bundle,
      eval_state,
      boot.global_object,
      bundle.entry,
      set.new(),
    )
  let #(eval_state, result) = {
    use eval_state, pendings, dep <- try_fold_state(to_evaluate, eval_state, [])
    let #(eval_state, dep_result) =
      eval_module_inner(bundle, linked, eval_state, dep, boot)
    case dep_result {
      Ok(_) -> #(eval_state, Ok(pendings))
      // Parked on top-level await: record the capability and keep
      // going — the spec Evaluate()s every gathered module before
      // waiting on all their promises.
      Error(EvaluationPending(promise_data_ref:)) -> #(
        eval_state,
        Ok([#(dep, promise_data_ref), ..pendings]),
      )
      Error(err) -> #(eval_state, Error(err))
    }
  }
  let result = result.map(result, list.reverse)
  #(eval_state.heap, eval_state.jobs, result)
}

/// The resolved specifiers some module in the bundle imports with
/// `import defer * as ns` — each needs a Deferred Module Namespace.
fn needed_deferred_specs(bundle: ModuleBundle) -> List(String) {
  dict.fold(bundle.modules, [], fn(acc, _spec, bundle_module) {
    // A host module imports nothing, so it can never defer anything.
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
      // `specifier_map` is TOTAL over `m`'s own requests, so an unresolvable
      // deferred import is the same invariant break `import_seeds` fails on —
      // fail here, at its cause, rather than one phase downstream.
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
/// shared export cells, BEFORE any body runs. This is what makes a cyclic
/// function import callable during the cycle. Bodies still re-create their own
/// closures (the final, fully-correct ones); these are the link-time values
/// importers see until then.
fn instantiate_hoisted_functions(
  bundle: ModuleBundle,
  linked: Linked,
  builtins: Builtins,
  heap: Heap(host),
  already_evaluated: Set(String),
) -> Heap(host) {
  dict.fold(bundle.modules, heap, fn(heap, spec, bundle_module) {
    // A host module has no body and no hoisted functions.
    use compiled <- with_source_module(bundle_module, heap)
    // A preexisting module's export cells hold their final values — don't
    // overwrite them with link-time placeholder closures.
    use <- bool.guard(set.contains(already_evaluated, spec), heap)
    let local_boxes =
      dict.get(linked.local_boxes, spec)
      |> result.replace_error(MissingLocalBoxes(spec))
      |> assert_link_invariant
    // Reconstruct the module's seeded frame so closures capture the same cells
    // a body run would (imports in slots 0..N-1, own exports in their slots).
    let seeds =
      import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
      |> assert_link_invariant
      |> list.append(own_export_seeds(linked, compiled))
    let locals = frame.init_module_locals(compiled.template, seeds)
    list.fold(compiled.hoisted_funcs, heap, fn(heap, hf) {
      let #(name, func_idx) = hf
      // Only exported functions have a shared cell; the rest are body-local.
      case dict.get(local_boxes, name) {
        Error(Nil) -> heap
        Ok(box) -> {
          let child =
            tuple_array.get_unchecked(func_idx, compiled.template.functions)
          let captures =
            list.map(child.env_descriptors, fn(desc) {
              tuple_array.get_unchecked(desc.parent_index, locals)
            })
          let #(heap, closure) =
            interpreter.make_closure(heap, builtins, child, captures)
          heap.write(heap, box, BoxSlot(JsObject(closure)))
        }
      }
    })
  })
}

/// The first name a preexisting module's fresh parse EXPORTS that the module's
/// LIVE export map has no cell for, as `#(specifier, export name)`.
///
/// The comparison is over the module's whole exported-name set — its own local
/// exports AND everything it re-exports (`export {x} from`, `export * from`) —
/// because that is the set `build_linked` would build cells for and the set
/// `import_seeds` reads back out of the reused map. Checking only local exports
/// let a fresh parse that changed only its re-exports through, and it panicked
/// on `MissingExportCell` in the linker instead.
///
/// Names that do not RESOLVE (§16.2.1.6.3: reached only through an ambiguous or
/// dead `export *`) never get a cell in any bundle's map, so they are excluded
/// from both sides.
///
/// Reachable only when the host loader served different source for a specifier
/// it had already served (arc's own CLI loader re-reads the file from disk on
/// every graph walk), so this is a host-contract violation, not a linker bug:
/// `link_for_evaluation_reusing` turns it into a link-time SyntaxError instead
/// of letting the linker panic on the missing cell.
fn stale_reused_export(
  bundle: ModuleBundle,
  lg: link.LinkableGraph,
  preexisting: Dict(String, #(Ref, Dict(String, Ref))),
) -> Option(#(String, String)) {
  dict.to_list(bundle.modules)
  |> list.find_map(fn(entry) {
    let #(spec, bundle_module) = entry
    case bundle_module, dict.get(preexisting, spec) {
      SourceModule(_), Ok(#(_ns_ref, existing_exports)) -> {
        let key = esm.resolved_unchecked(spec)
        link.exported_names(lg, key)
        |> list.find_map(fn(name) {
          case link.resolve_export(lg, key, name) {
            // No cell exists for these in ANY bundle — nothing to compare.
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

/// The prose of a `stale_reused_export` link failure — guest-visible, so it
/// names the contract the loader broke rather than blaming the linker.
fn stale_reused_export_message(specifier: String, name: String) -> String {
  "module '"
  <> specifier
  <> "' was re-loaded with an export '"
  <> name
  <> "' its already-instantiated namespace does not have: a loader must return "
  <> "the same source for a specifier it has already served"
}

/// One BoxSlot per exported local, seeded with its instantiation value:
/// `uninitialized` (TDZ) for let/const/class/default, `undefined` for
/// var/function (hoisted). Keyed specifier → local name → box.
///
/// Preexisting modules reuse their live cells: their export map (export
/// name → box) is translated back to local names via the module's export
/// entries, so re-exports from other bundles resolve to the original cells.
fn preallocate_local_boxes(
  bundle: ModuleBundle,
  heap: Heap(host),
  preexisting: Dict(String, #(Ref, Dict(String, Ref))),
) -> #(Heap(host), Dict(String, Dict(String, Ref))) {
  dict.fold(bundle.modules, #(heap, dict.new()), fn(acc, spec, bundle_module) {
    let #(heap, all) = acc
    let existing =
      dict.get(preexisting, spec)
      |> option.from_result
      |> option.map(fn(p) { p.1 })
    let #(heap, boxes) = case bundle_module, existing {
      SourceModule(m), Some(existing_exports) -> #(
        heap,
        list.fold(m.export_entries, dict.new(), fn(boxes, e) {
          case e {
            // `stale_reused_export` already rejected the one host-controlled
            // way this map can miss a local export, so a miss here is a linker
            // bug. Dropping the entry instead would leave `own_export_seeds`
            // with nothing to seed, and this module's body would write its
            // binding into a box no importer ever reads.
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
        dict.fold(m.export_seeds, #(heap, dict.new()), fn(a, local, seed) {
          let #(heap, boxes) = a
          let #(heap, box) = alloc_box(heap, seed)
          #(heap, dict.insert(boxes, local, box))
        })
      // A host module: one box per export, seeded with the host value itself
      // (no body ever runs to fill it — the seed IS the final binding). Its
      // local names ARE its export names, so a preexisting cell reuses direct.
      SyntheticModule(hm), existing_exports ->
        list.fold(hm.exports, #(heap, dict.new()), fn(a, export) {
          let #(heap, boxes) = a
          let #(name, val) = export
          let reused =
            option.then(existing_exports, fn(ex) {
              dict.get(ex, name) |> option.from_result
            })
          case reused {
            Some(box) -> #(heap, dict.insert(boxes, name, box))
            None -> {
              let #(heap, box) = alloc_box(heap, val)
              #(heap, dict.insert(boxes, name, box))
            }
          }
        })
    }
    #(heap, dict.insert(all, spec, boxes))
  })
}

/// §10.4.6 Module Namespace Exotic Object slot: a ModuleNamespace kind holding
/// the export name → BoxSlot-ref map (so [[Get]] re-reads the live cell and
/// throws on TDZ), null prototype, non-extensible, with @@toStringTag = "Module"
/// (§28.3.1, the all-false attributes of value.data).
fn namespace_slot(exports: Dict(String, Ref)) -> state.HeapSlot(host) {
  namespace_slot_tagged(exports, "Module")
}

/// `namespace_slot` with an explicit @@toStringTag — ModuleNamespaceCreate
/// uses "Deferred Module" for the ~defer~ phase namespace.
fn namespace_slot_tagged(
  exports: Dict(String, Ref),
  tag: String,
) -> state.HeapSlot(host) {
  ObjectSlot(
    kind: value.ModuleNamespace(exports:),
    properties: dict.new(),
    elements: elements.new(),
    prototype: None,
    symbol_properties: [
      #(value.symbol_to_string_tag, value.data(JsString(tag))),
    ],
    extensible: False,
  )
}

// -----------------------------------------------------------------------------
// Deferred Module Namespaces (defer-import-eval proposal)
// -----------------------------------------------------------------------------
//
// A deferred namespace is implemented as a Proxy exotic object whose target is
// a Module Namespace object for the SAME live export cells (but tagged
// @@toStringTag "Deferred Module"), and whose handler implements the deferred
// MOP: string-keyed [[Get]]/[[GetOwnProperty]]/[[HasProperty]]/
// [[DefineOwnProperty]]/[[Delete]] (except the key "then") and every
// [[OwnPropertyKeys]] first perform EnsureDeferredNamespaceEvaluation —
// synchronously evaluating the module's still-unevaluated subgraph — then
// forward to the target. Symbol keys and "then" are "symbol-like" (the spec's
// IsSymbolLikeNamespaceKey) and never trigger evaluation. Routing through the
// Proxy machinery means every builtin (Reflect, Object.*, `in`, `delete`,
// for-in, super.*) hits the deferred behavior without per-call-site changes,
// and the proxy invariant checks are satisfied because the traps forward to a
// genuine namespace target.

/// Write the reserved `proxy_ref` as a Deferred Module Namespace for `spec`.
fn fill_deferred_namespace(
  h: Heap(host),
  builtins: Builtins,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
  proxy_ref: Ref,
) -> Heap(host) {
  // Both callers only reach here for a `bundle.modules` key, and `build_linked`
  // gives every one of those an export map.
  let exports =
    dict.get(linked.exports, spec)
    |> result.replace_error(MissingExportMap(spec))
    |> assert_link_invariant
  let #(h, target_ref) =
    heap.alloc(h, namespace_slot_tagged(exports, "Deferred Module"))
  let h = heap.root(h, target_ref)
  let #(h, traps) =
    [
      #("get", 3, value.ReflectGet, False),
      #("has", 2, value.ReflectHas, False),
      #("deleteProperty", 2, value.ReflectDeleteProperty, False),
      #("defineProperty", 3, value.ReflectDefineProperty, False),
      #(
        "getOwnPropertyDescriptor",
        2,
        value.ReflectGetOwnPropertyDescriptor,
        False,
      ),
      #("ownKeys", 1, value.ReflectOwnKeys, True),
    ]
    |> list.map_fold(h, fn(h, t) {
      let #(h, ref) = alloc_deferred_trap(h, builtins, t, bundle, linked, spec)
      #(h, #(t.0, value.data_property(JsObject(ref))))
    })
  let #(h, handler_ref) = common.alloc_pojo(h, builtins.object.prototype, traps)
  let h = heap.root(h, handler_ref)
  heap.write(
    h,
    proxy_ref,
    ObjectSlot(
      kind: value.ProxyObject(
        slots: Some(value.ProxySlots(target: target_ref, handler: handler_ref)),
        callable: False,
        constructable: False,
      ),
      properties: dict.new(),
      elements: elements.new(),
      prototype: None,
      symbol_properties: [],
      // A proxy's observable [[IsExtensible]] goes through the trap
      // machinery (here: the non-extensible namespace target), never this
      // field — but internal heap-level checks (PrivateFieldAdd's
      // non-extensible rejection) read it directly, and a deferred namespace
      // must reject private stamping without triggering evaluation.
      extensible: False,
    ),
  )
}

/// One deferred-namespace trap: trigger EnsureDeferredNamespaceEvaluation when
/// the operation observes exports (always for ownKeys; for keyed traps only
/// when the key is a string other than "then" — IsSymbolLikeNamespaceKey),
/// then forward to the target via the corresponding Reflect builtin.
fn alloc_deferred_trap(
  h: Heap(host),
  builtins: Builtins,
  trap: #(String, Int, value.ReflectNativeFn, Bool),
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
) -> #(Heap(host), Ref) {
  let #(name, arity, native, always_triggers) = trap
  common.alloc_rooted_host_fn(
    h,
    builtins.function.prototype,
    fn(args, _this, state) {
      let triggers =
        always_triggers
        || case args {
          [_, JsString(key), ..] -> key != "then"
          _ -> False
        }
      case triggers {
        False ->
          // IsSymbolLikeNamespaceKey: the string key "then" must NEVER be
          // observable on a deferred namespace — [[Get]] is ! OrdinaryGet
          // (undefined, the namespace has no ordinary properties) even when
          // the module exports `then`, and even after evaluation. A promise
          // resolution adopting a still-unevaluated module's exported `then`
          // closure is exactly the hazard this carve-out prevents. Only the
          // get trap can honor it: has/deleteProperty/getOwnPropertyDescriptor
          // for an exported "then" must keep forwarding, or they would
          // violate proxy invariants against a target that owns a
          // non-configurable "then" export (get may diverge because export
          // data properties are writable).
          case native, args {
            value.ReflectGet, [_, JsString("then"), ..] -> #(
              state,
              Ok(JsUndefined),
            )
            _, _ -> reflect.dispatch(native, args, JsUndefined, state)
          }
        True ->
          case
            ensure_deferred_evaluated(state, bundle, linked, spec, builtins)
          {
            Ok(state) -> reflect.dispatch(native, args, JsUndefined, state)
            Error(#(err, state)) -> #(state, Error(err))
          }
      }
    },
    "%DeferredNamespace[" <> name <> "]%",
    arity,
  )
}

/// EnsureDeferredNamespaceEvaluation ( O ): if `spec` is already evaluated,
/// done; if its evaluation previously threw, rethrow that error; if it is not
/// ReadyForSyncExecution (mid-evaluation, or top-level await in the
/// unevaluated subgraph), throw a TypeError; otherwise EvaluateSync — run the
/// deferred subgraph's bodies right now, on the current VM state.
fn ensure_deferred_evaluated(
  state: State(host),
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
  builtins: Builtins,
) -> Result(State(host), #(JsValue, State(host))) {
  let global_object = state.ctx.global_object
  case registry.read_module_status(state.heap, global_object, spec) {
    Some(registry.Evaluated) -> Ok(state)
    _ ->
      case registry.read_module_error(state.heap, global_object, spec) {
        Some(err) -> Error(#(err, state))
        None -> {
          let #(ready, _seen) =
            ready_for_sync_execution(
              bundle,
              state.heap,
              global_object,
              spec,
              set.new(),
            )
          case ready {
            False ->
              state.type_error_op(
                state,
                "Cannot synchronously evaluate deferred module: '"
                  <> spec
                  <> "' is still evaluating or has an unevaluated async dependency",
              )
            True ->
              evaluate_deferred_subgraph(state, bundle, linked, spec, builtins)
          }
        }
      }
  }
}

/// ReadyForSyncExecution ( module, seen ): a module can be evaluated
/// synchronously when every module in its unevaluated eager dependency graph
/// is neither mid-evaluation nor a top-level-await module.
fn ready_for_sync_execution(
  bundle: ModuleBundle,
  h: Heap(host),
  global_object: Ref,
  spec: String,
  seen: Set(String),
) -> #(Bool, Set(String)) {
  use <- bool.guard(set.contains(seen, spec), #(True, seen))
  let seen = set.insert(seen, spec)
  case registry.read_module_status(h, global_object, spec) {
    Some(registry.Evaluated) -> #(True, seen)
    Some(registry.Evaluating) -> #(False, seen)
    _ ->
      case dict.get(bundle.modules, spec) {
        // Not in this bundle (registry-shared module) — its body either ran
        // already or will be loaded by its own bundle; treat as ready. A host
        // module has no body at all, so it is always ready.
        Error(Nil) | Ok(SyntheticModule(_)) -> #(True, seen)
        Ok(SourceModule(m)) ->
          case m.has_tla {
            True -> #(False, seen)
            False ->
              // Only the ~evaluation~-phase requests: a ~defer~ request's
              // module is not part of this module's synchronous subgraph.
              list.fold(m.requested_modules, #(True, seen), fn(acc, request) {
                let #(ok, seen) = acc
                let #(dep_specifier, phase) = request
                case ok, phase {
                  False, _ -> #(False, seen)
                  True, esm.Deferred -> #(True, seen)
                  True, esm.Evaluation ->
                    ready_for_sync_execution(
                      bundle,
                      h,
                      global_object,
                      esm.resolved_text(dep_specifier),
                      seen,
                    )
                }
              })
          }
      }
  }
}

/// EvaluateSync: run the deferred module's unevaluated (eager) subgraph via
/// the normal DFS evaluator, seeded with the realm's heap-resident evaluation
/// state so bodies that already ran (in this bundle, an earlier bundle, or an
/// earlier trigger) are not re-run. Jobs enqueued by the bodies are handed to
/// the currently running VM's job queue — never drained re-entrantly.
fn evaluate_deferred_subgraph(
  state: State(host),
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
  builtins: Builtins,
) -> Result(State(host), #(JsValue, State(host))) {
  let boot =
    BootCtx(
      builtins:,
      global_object: state.ctx.global_object,
      host_hooks: state.ctx.host_hooks,
      can_block: state.can_block,
      extend_262: state.ctx.extend_262,
      finish: fn(s) { s },
    )
  // Nothing to seed: `module_eval_status` reads the realm's heap-resident
  // registry for any module this DFS has not itself touched, so bodies that
  // already ran (in this bundle, an earlier bundle, or an earlier trigger)
  // are recognized without copying the registry into Gleam data first.
  let st = EvalState(heap: state.heap, modules: dict.new(), jobs: [])
  let #(st, result) = eval_module_inner(bundle, linked, st, spec, boot)
  let state =
    State(
      ..state,
      heap: st.heap,
      job_queue: job_queue.append(state.job_queue, st.jobs),
    )
  case result {
    Ok(_) -> Ok(state)
    // `state` already carries `st.heap`, the heap the throw left behind.
    Error(EvaluationError(value:)) -> Error(#(value, state))
    // A deferred subgraph is only entered when ReadyForSyncExecution said yes,
    // so no body in it can park on top-level await; and the specifier came out
    // of this bundle, so it is in it. Both are unreachable — but rendered
    // through `error_message`, never a Gleam debug repr.
    Error(NotInBundle(..) as other) | Error(EvaluationPending(..) as other) ->
      state.type_error_op(
        state,
        "Failed to evaluate deferred module '"
          <> spec
          <> "': "
          <> error_message(other, state.heap),
      )
  }
}

// -----------------------------------------------------------------------------
// Seeding — place the pre-allocated cells into a module's local slots.
// -----------------------------------------------------------------------------

/// Import bindings seeded into capture slots 0..N-1, in declaration order
/// (matching esm.import_local_names). Named/default forward the
/// exporting module's live cell; namespace imports get the shared namespace box.
fn import_seeds(
  linked: Linked,
  specifier_map: esm.SpecifierMap,
  import_bindings: List(#(esm.Raw, List(esm.ImportBinding))),
) -> Result(List(#(Int, JsValue)), LinkInvariantBroken) {
  use per_dep <- result.map(
    list.try_map(import_bindings, fn(entry) {
      let #(raw_dep, bindings) = entry
      // `specifier_map` is TOTAL over the module's own requests, so `None` is
      // an invariant break — it means "no such dependency", never "the raw
      // specifier happens to be a bundle key too".
      use dep <- result.try(
        esm.resolve(specifier_map, raw_dep)
        |> option.to_result(UnresolvedDependency(raw_dep)),
      )
      let dep = esm.resolved_text(dep)
      // An empty export map here would turn every named import of `dep` into
      // the far-away lie "no such export"; report the missing map instead.
      use dep_exports <- result.try(
        dict.get(linked.exports, dep)
        |> result.replace_error(MissingExportMap(dep)),
      )
      // Every binding gets a slot: these are positional captures 0..N-1.
      list.try_map(bindings, fn(binding) {
        case binding {
          esm.NamedImport(imported:, ..) ->
            forward_box(dep_exports, dep, imported)
          esm.DefaultImport(..) -> forward_box(dep_exports, dep, "default")
          esm.NamespaceImport(phase: esm.Deferred, ..) ->
            dict.get(linked.deferred_boxes, dep)
            |> result.replace_error(MissingDeferredBox(dep))
            |> result.map(JsObject)
          esm.NamespaceImport(phase: esm.Evaluation, ..) ->
            dict.get(linked.namespace_boxes, dep)
            |> result.replace_error(MissingNamespaceBox(dep))
            |> result.map(JsObject)
        }
      })
    }),
  )
  per_dep
  |> list.flatten
  |> list.index_map(fn(box, idx) { #(idx, box) })
}

/// The exporting module's live cell for `name` (link-checked to exist).
fn forward_box(
  dep_exports: Dict(String, Ref),
  dep: String,
  name: String,
) -> Result(JsValue, LinkInvariantBroken) {
  dict.get(dep_exports, name)
  |> result.replace_error(MissingExportCell(dep, name))
  |> result.map(JsObject)
}

/// This module's own export cells, placed into their declared local slots so
/// the body initializes and reads/writes them through the shared boxes.
/// Locals that are IMPORT bindings (`import * as ns ...; export { ns }`) are
/// excluded — their slot must keep the import seed (the dependency's live
/// cell / namespace box), and importers resolve such exports through the
/// dependency anyway (resolve_local_export).
fn own_export_seeds(
  linked: Linked,
  compiled: CompiledModule,
) -> List(#(Int, JsValue)) {
  let import_locals =
    esm.binding_local_names(compiled.import_bindings) |> set.from_list
  let spec = esm.resolved_text(compiled.specifier)
  dict.get(linked.local_boxes, spec)
  |> result.replace_error(MissingLocalBoxes(spec))
  |> assert_link_invariant
  |> dict.to_list
  |> list.filter_map(fn(pair) {
    let #(local_name, box) = pair
    use <- bool.guard(set.contains(import_locals, local_name), Error(Nil))
    case dict.get(compiled.export_names, local_name) {
      Ok(index) -> Ok(#(index, JsObject(box)))
      Error(Nil) -> Error(Nil)
    }
  })
}
