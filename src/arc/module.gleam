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
import arc/module/registry
import arc/parser
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/reflect
import arc/vm/exec/entry
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/job_queue
import arc/vm/internal/tuple_array
import arc/vm/opcode
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
pub type CompiledModule {
  CompiledModule(
    specifier: String,
    template: value.FuncTemplate,
    import_bindings: List(#(String, List(esm.ImportBinding))),
    export_entries: List(esm.ExportEntry),
    scope_dict: Dict(String, Int),
    specifier_map: Dict(String, String),
    requested_modules: List(String),
    /// Exported local name → value the linker seeds into its BoxSlot before the
    /// body runs: `uninitialized` (TDZ) for let/const/class/default, `undefined`
    /// for var/function. See compiler.module_export_seeds.
    export_seeds: Dict(String, JsValue),
    /// Top-level hoisted function declarations as (name, func_index) into
    /// template.functions. The linker instantiates the *exported* ones before
    /// any body runs, so cyclic function imports are callable (§16.2.1.6.4).
    hoisted_funcs: List(#(String, Int)),
    /// Raw specifiers this module requests with phase ~evaluation~ at least
    /// once: bare imports, named/default/eager-namespace imports, and every
    /// re-export. Requests appearing ONLY as `import defer * as ns` are
    /// excluded — InnerModuleEvaluation skips them (§16.2.1.5.3.1), deferring
    /// the dependency's evaluation to first namespace access.
    eager_requests: List(String),
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

/// A complete compiled module graph — the output of AOT compilation.
/// Pure Erlang term, serializable via term_to_binary. `host_modules` are
/// embedder-provided native modules (see `HostModule`); they carry no source
/// and live alongside the compiled source `modules`.
pub type ModuleBundle {
  ModuleBundle(
    entry: String,
    modules: Dict(String, CompiledModule),
    host_modules: Dict(String, HostModule),
  )
}

// =============================================================================
// Errors
// =============================================================================

/// A failure anywhere in the module pipeline. Each pre-evaluation variant
/// carries the structured cause from the layer that produced it (graph walk,
/// bytecode compiler, bundle lookup) — dispatch on the variant, and render
/// the user-facing prose with `error_message`.
///
/// Link-time validation failures (§16.2.1.6.4, `link.LinkError`) have no
/// variant of their own: `link_for_evaluation` allocates the JS SyntaxError
/// in the heap right there (its identity and stack ARE the module's rejection
/// value), so they surface as `EvaluationError`.
pub type ModuleError(host) {
  /// The source-graph walk failed: a module failed to parse, a request failed
  /// to resolve or load, or a source-phase import was requested. Match the
  /// inner `graph.GraphError` to tell those apart.
  GraphError(error: graph.GraphError)
  /// Bytecode compilation of the module named `specifier` failed.
  CompileError(specifier: String, error: compiler.CompileError)
  /// Evaluation asked for a resolved specifier the bundle does not contain.
  ModuleNotInBundle(specifier: String)
  /// A module threw during evaluation. Carries both the thrown value and the
  /// heap it was allocated in — the value is a Ref into this heap, so callers
  /// must use it (not a pre-evaluation heap) to inspect the thrown object.
  EvaluationError(value: JsValue, heap: Heap(host))
  /// Evaluation is parked on top-level await and the supplied `finish`
  /// driver did not settle it (only reachable with a non-draining driver —
  /// the dynamic-import path). `promise_data_ref` is the entry module's
  /// [[TopLevelCapability]] promise data: per Evaluate() step 4 the host
  /// must chain onto this promise (and hand any returned jobs to its own
  /// event loop) rather than treat the module as failed.
  EvaluationPending(promise_data_ref: Ref, heap: Heap(host))
}

/// The single renderer of a `ModuleError`'s user-facing prose. Every layer's
/// own message lives in that layer (`parser.parse_error_to_string`,
/// `compiler.error_message`); this adds the module-pipeline framing (which
/// module, which phase) exactly once.
pub fn error_message(err: ModuleError(host)) -> String {
  case err {
    GraphError(error: graph.ParseFailed(specifier, parse_error)) ->
      "SyntaxError in '"
      <> specifier
      <> "': "
      <> parser.parse_error_to_string(parse_error)
    GraphError(error: graph.ResolveFailed(raw, referrer, message)) ->
      "Cannot resolve module '"
      <> raw
      <> "' from '"
      <> referrer
      <> "': "
      <> message
    GraphError(error: graph.LoadFailed(specifier, message)) ->
      "Cannot load module '" <> specifier <> "': " <> message
    GraphError(error: graph.SourcePhaseUnsupported(specifier)) ->
      "'"
      <> specifier
      <> "': source phase imports ('import source') are not supported"
    CompileError(specifier:, error:) ->
      compiler.error_message(error) <> " in '" <> specifier <> "'"
    ModuleNotInBundle(specifier:) ->
      "Module '" <> specifier <> "' not found in bundle"
    EvaluationError(value:, heap:) ->
      "Uncaught " <> object.format_error(value, heap)
    EvaluationPending(promise_data_ref: _, heap: _) ->
      "module evaluation never completed: top-level await promise never settled"
  }
}

/// The successful result of `evaluate_bundle`: the entry module's completion
/// value, the resulting heap, and the entry module's Module Namespace Exotic
/// Object (§10.4.6). Read named exports off `namespace` via its [[Get]] — a
/// live, TDZ-throwing, write-protected view of the export bindings. This is the
/// embedder's `GetModuleNamespace` handle (cf. V8 `Module::GetModuleNamespace`,
/// QuickJS `JS_GetModuleNamespace`).
pub type EvaluatedBundle(host) {
  EvaluatedBundle(value: JsValue, heap: Heap(host), namespace: Option(JsValue))
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
  resolve: fn(String, String) -> Result(String, String),
  load: fn(String) -> Result(String, String),
) -> Result(ModuleBundle, ModuleError(host)) {
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
  resolve: fn(String, String) -> Result(String, String),
  load: fn(String) -> Result(String, String),
  host_modules: Dict(String, HostModule),
) -> Result(ModuleBundle, ModuleError(host)) {
  // Adapt the runtime's raw-specifier resolver to the graph layer's
  // request-taking one.
  let resolve_request = fn(request: esm.ModuleRequest, referrer) {
    resolve(request.specifier, referrer)
  }
  use source_graph <- result.try(
    graph.load(entry_specifier, entry_source, resolve_request, load, fn(spec) {
      dict.has_key(host_modules, spec)
    })
    |> result.map_error(GraphError),
  )
  use modules <- result.map(
    dict.fold(source_graph.modules, Ok(dict.new()), fn(acc, specifier, node) {
      use modules <- result.try(acc)
      use compiled <- result.map(compile_source_module(node))
      dict.insert(modules, specifier, compiled)
    }),
  )
  ModuleBundle(entry: entry_specifier, modules:, host_modules:)
}

/// Compile one loaded module from the source graph — the import/export
/// analysis is already done (`node.summary`); this adds the bytecode stage.
fn compile_source_module(
  node: graph.SourceModule,
) -> Result(CompiledModule, ModuleError(host)) {
  let graph.SourceModule(
    specifier:,
    source: _,
    program:,
    sb:,
    summary:,
    specifier_map:,
  ) = node
  use #(template, scope_dict, hoisted_funcs) <- result.map(
    compiler.compile_module(program, sb, summary)
    |> result.map_error(fn(error) { CompileError(specifier:, error:) }),
  )

  // The bundle format keeps flat specifier lists; both project out of the
  // summary's merged ModuleRequests.
  let requested_modules =
    list.map(summary.requested, fn(request) { request.specifier })
  let eager_requests =
    list.filter_map(summary.requested, fn(request) {
      case request.phase {
        esm.Default -> Ok(request.specifier)
        esm.Deferred -> Error(Nil)
      }
    })

  CompiledModule(
    specifier:,
    template:,
    import_bindings: summary.imports,
    export_entries: summary.exports,
    scope_dict:,
    specifier_map:,
    requested_modules:,
    export_seeds: compiler.module_export_seeds(program, summary),
    hoisted_funcs:,
    eager_requests:,
    has_tla: module_has_tla(template),
  )
}

/// [[HasTLA]]: whether the module body contains a top-level `await`. `await`
/// inside nested functions compiles into child templates, so an Await opcode
/// in the module's own bytecode is exactly a top-level await.
fn module_has_tla(template: value.FuncTemplate) -> Bool {
  tuple_array.to_list(template.bytecode)
  |> list.any(fn(op) { op == opcode.Await })
}

// =============================================================================
// Linking — ResolveExport (§16.2.1.6.3) + import/re-export checks (§16.2.1.6.4)
// =============================================================================
//
// The resolve algorithm and the SyntaxError-producing import/re-export checks
// live in `arc/link`, operating on a minimal `link.LinkableGraph` view. The VM
// projects a bundle into that view (`linkable_of_bundle`) and calls
// `link.validate` / `link.resolve_export` / `link.exported_names`.

/// Project a compiled bundle onto the shared `link.LinkableGraph` view: a
/// trivial per-module field copy of the three string-level fields the resolver
/// reads.
fn linkable_of_bundle(bundle: ModuleBundle) -> link.LinkableGraph {
  let source =
    dict.map_values(bundle.modules, fn(_specifier, m) {
      link.LinkableModule(
        import_bindings: m.import_bindings,
        export_entries: m.export_entries,
        specifier_map: m.specifier_map,
      )
    })
  // A host module projects as a module with no imports and a LocalExport per
  // supplied name (its cells are pre-seeded with the host values at link time),
  // so the resolver's import/re-export checks and `exported_names` see it.
  use acc, specifier, hm <- dict.fold(bundle.host_modules, source)
  dict.insert(
    acc,
    specifier,
    link.LinkableModule(
      import_bindings: [],
      export_entries: list.map(hm.exports, fn(e) {
        esm.LocalExport(export_name: e.0, local_name: e.0)
      }),
      specifier_map: dict.new(),
    ),
  )
}

/// The resolved specifier a raw specifier maps to within module `m` (VM-side;
/// reads the per-module specifier_map, not the bundle).
fn resolved_specifier(m: CompiledModule, raw: String) -> String {
  dict.get(m.specifier_map, raw) |> result.unwrap(raw)
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

/// Internal evaluation state threaded through the DFS.
type EvalState(host) {
  EvalState(
    heap: Heap(host),
    /// Specifiers whose body has finished successfully.
    evaluated: Set(String),
    /// Specifier → cached error for modules that threw during evaluation.
    errors: Dict(String, JsValue),
    /// Currently evaluating (cycle detection).
    evaluating: Set(String),
    /// Promise jobs left queued by module bodies when the `finish` driver
    /// does not drain (dynamic import) — handed back to the host event loop.
    jobs: List(value.Job),
  )
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
/// instantiated. `namespace` is the entry module's Module Namespace Exotic
/// Object — live before evaluation, so a host can publish it in its module
/// registry first and a re-entrant dynamic import of the evaluating module
/// resolves to the same namespace instead of re-evaluating (§16.2.1.8).
pub type LinkedBundle {
  LinkedBundle(bundle: ModuleBundle, linked: Linked, namespace: Option(JsValue))
}

/// Link phase of `evaluate_bundle` (§16.2.1.6.4): resolve every import and
/// indirect re-export across the whole graph BEFORE evaluating any body —
/// missing or ambiguous exports are a SyntaxError at link time, surfaced as
/// `EvaluationError`. On success, pre-allocates every binding cell and
/// namespace object and instantiates exported hoisted function declarations.
pub fn link_for_evaluation(
  bundle: ModuleBundle,
  heap: Heap(host),
  builtins: Builtins,
) -> Result(#(Heap(host), LinkedBundle), ModuleError(host)) {
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
) -> Result(#(Heap(host), LinkedBundle), ModuleError(host)) {
  case link.validate(linkable_of_bundle(bundle)) {
    Error(link_error) -> {
      let #(heap, err) =
        common.make_syntax_error(
          heap,
          builtins,
          link.link_error_message(link_error),
        )
      Error(EvaluationError(err, heap))
    }
    Ok(Nil) -> {
      // Expand each preexisting namespace ref into (ref, export-name → box):
      // the export map is final for an instantiated module and is exactly
      // what importers link against.
      let pre =
        dict.fold(preexisting, dict.new(), fn(acc, spec, ns_ref) {
          case heap.read(heap, ns_ref) {
            Some(ObjectSlot(kind: value.ModuleNamespace(exports:), ..)) ->
              dict.insert(acc, spec, #(ns_ref, exports))
            _ -> acc
          }
        })
      // Instantiate: pre-allocate every binding cell + namespace object, then
      // create exported function-declaration closures so cyclic function
      // imports are callable before any body runs (§16.2.1.6.4 step 9).
      let #(heap, linked, deferred_to_fill) =
        build_linked(bundle, heap, pre, preexisting_deferred)
      // Now that the full Linked record exists, write the reserved deferred
      // namespace proxies — their traps capture the complete bundle+linked so
      // a first access can run the deferred subgraph's evaluation.
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
      let namespace = entry_namespace(linked, bundle.entry, heap)
      Ok(#(heap, LinkedBundle(bundle:, linked:, namespace:)))
    }
  }
}

/// Evaluation phase of `evaluate_bundle`: execute module bodies in DFS
/// post-order (dependencies first). Returns the entry module's completion
/// value and post-evaluation namespace.
pub fn evaluate_linked(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  host_hooks: state.HostHooks,
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(EvaluatedBundle(host), ModuleError(host)) {
  let #(_evaluated, _jobs, result) =
    evaluate_linked_tracking(
      linked_bundle,
      heap,
      builtins,
      global_object,
      host_hooks,
      finish,
      set.new(),
    )
  // Static entry points drive a draining `finish`, so pending here means an
  // awaited promise can never settle — keep the historical "never completed"
  // throw (cf. Node's exit code 13 for unsettled top-level await).
  case result {
    Error(EvaluationPending(promise_data_ref: _, heap: pending_heap)) ->
      Error(EvaluationError(
        value: JsString(
          "module evaluation never completed: top-level await promise never settled",
        ),
        heap: pending_heap,
      ))
    other -> other
  }
}

/// `evaluate_linked` for registry-aware hosts: modules in `already_evaluated`
/// are treated as done (their bodies never run — pair with
/// `link_for_evaluation_reusing` so their cells are the live preexisting
/// ones). Returns the full set of successfully evaluated specifiers alongside
/// the result, so the host can register exactly the modules whose bodies ran
/// (even when a later module's evaluation threw).
pub fn evaluate_linked_tracking(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  host_hooks: state.HostHooks,
  finish: fn(state.State(host)) -> state.State(host),
  already_evaluated: Set(String),
) -> #(
  Set(String),
  List(value.Job),
  Result(EvaluatedBundle(host), ModuleError(host)),
) {
  let LinkedBundle(bundle:, linked:, namespace: _) = linked_bundle
  let state =
    EvalState(
      heap:,
      evaluated: already_evaluated,
      errors: dict.new(),
      evaluating: set.new(),
      jobs: [],
    )
  let #(state, result) =
    eval_module_inner(
      bundle,
      linked,
      state,
      bundle.entry,
      builtins,
      global_object,
      host_hooks,
      finish,
    )
  // Surface the entry namespace alongside the completion value (post-eval,
  // so its bindings are initialized — no TDZ for the embedder to hit).
  let result = {
    use #(completion_value, final_heap) <- result.map(result)
    EvaluatedBundle(
      value: completion_value,
      heap: final_heap,
      namespace: entry_namespace(linked, bundle.entry, final_heap),
    )
  }
  #(state.evaluated, state.jobs, result)
}

fn read_box_dict(
  boxes: Dict(String, Ref),
  heap: Heap(host),
) -> List(#(String, JsValue)) {
  use acc, spec, box <- dict.fold(boxes, [])
  case heap.read_box(heap, box) {
    Some(ns) -> [#(spec, ns), ..acc]
    None -> acc
  }
}

/// Every module in a linked bundle paired with its Module Namespace Exotic
/// Object — what a registry-keeping host records so a later import of any
/// graph module (entry or dependency) reuses the same record (§16.2.1.8).
pub fn linked_namespaces(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
) -> List(#(String, JsValue)) {
  read_box_dict(linked_bundle.linked.namespace_boxes, heap)
}

/// Every Deferred Module Namespace in a linked bundle, for the registry: a
/// later `import defer` / `import.defer()` of the same module must yield the
/// identical object ([[DeferredNamespace]] is per module record).
pub fn linked_deferred_namespaces(
  linked_bundle: LinkedBundle,
  heap: Heap(host),
) -> List(#(String, JsValue)) {
  read_box_dict(linked_bundle.linked.deferred_boxes, heap)
}

/// The Deferred Module Namespace for `spec`, creating one if no importer in
/// the bundle deferred it statically (the dynamic `import.defer()` path).
pub fn get_or_create_deferred_namespace(
  heap: Heap(host),
  builtins: Builtins,
  linked_bundle: LinkedBundle,
  spec: String,
) -> #(Heap(host), Option(JsValue)) {
  let LinkedBundle(bundle:, linked:, namespace: _) = linked_bundle
  use <- bool.lazy_guard(dict.has_key(bundle.modules, spec) == False, fn() {
    #(heap, None)
  })
  case dict.get(linked.deferred_boxes, spec) {
    Ok(box) -> #(heap, heap.read_box(heap, box))
    Error(Nil) -> {
      let #(heap, proxy_ref) = heap.reserve(heap)
      let heap = heap.root(heap, proxy_ref)
      let heap =
        fill_deferred_namespace(heap, builtins, bundle, linked, spec, proxy_ref)
      #(heap, Some(JsObject(proxy_ref)))
    }
  }
}

/// Evaluate a compiled module bundle. Links the whole graph (pre-allocating
/// every binding cell), then executes module bodies in DFS post-order
/// (dependencies first). Returns the entry module's completion value.
///
/// Module bodies boot with `state.default_host_hooks()` (no embedder host
/// capabilities); embedders that supply Atomics capabilities use
/// `evaluate_bundle_with_hooks`.
pub fn evaluate_bundle(
  bundle: ModuleBundle,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(EvaluatedBundle(host), ModuleError(host)) {
  evaluate_bundle_with_hooks(
    bundle,
    heap,
    builtins,
    global_object,
    state.default_host_hooks(),
    finish,
  )
}

/// `evaluate_bundle` with the embedder's `state.HostHooks` (host
/// capabilities such as the Atomics blocking wait / wake delivery). Each
/// module body boots a FRESH State, so the hooks are threaded into every
/// body's `RealmCtx` at construction rather than installed after the fact —
/// a module's top level may hit a blocking `Atomics.wait` before any host
/// function has run.
pub fn evaluate_bundle_with_hooks(
  bundle: ModuleBundle,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  host_hooks: state.HostHooks,
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(EvaluatedBundle(host), ModuleError(host)) {
  use #(heap, linked_bundle) <- result.try(link_for_evaluation(
    bundle,
    heap,
    builtins,
  ))
  evaluate_linked(
    linked_bundle,
    heap,
    builtins,
    global_object,
    host_hooks,
    finish,
  )
}

/// The entry module's Module Namespace Exotic Object, read out of the rooted
/// box the linker reserved for it (`build_linked`). This is what `evaluate_bundle`
/// hands back as `EvaluatedBundle.namespace`. Falls back to `undefined` only if
/// the entry has no namespace box, which shouldn't happen for a linked bundle.
fn entry_namespace(
  linked: Linked,
  entry: String,
  heap: Heap(host),
) -> Option(JsValue) {
  case dict.get(linked.namespace_boxes, entry) {
    Ok(box) -> heap.read_box(heap, box)
    Error(Nil) -> None
  }
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

/// DFS post-order evaluation of a single module and its dependencies.
fn eval_module_inner(
  bundle: ModuleBundle,
  linked: Linked,
  state: EvalState(host),
  specifier: String,
  builtins: Builtins,
  global_object: Ref,
  host_hooks: state.HostHooks,
  finish: fn(state.State(host)) -> state.State(host),
) -> #(EvalState(host), Result(#(JsValue, Heap(host)), ModuleError(host))) {
  // Already evaluated successfully — either in this DFS or recorded in the
  // realm's heap-resident status registry (a deferred-namespace trigger may
  // have evaluated it mid-DFS, re-entrantly). A host (synthetic) module has no
  // body and ready exports, so its `[[Evaluate]]` is a no-op — always "done".
  let already_evaluated =
    dict.has_key(bundle.host_modules, specifier)
    || set.contains(state.evaluated, specifier)
    || registry.read_module_status(state.heap, global_object, specifier)
    == Some(registry.Evaluated)
  case already_evaluated {
    True -> #(state, Ok(#(JsUndefined, state.heap)))
    False ->
      // Cached error — re-throw, never re-evaluate
      case
        dict.get(state.errors, specifier)
        |> option.from_result
        |> option.or(registry.read_module_error(
          state.heap,
          global_object,
          specifier,
        ))
      {
        Some(err_val) -> #(state, Error(EvaluationError(err_val, state.heap)))
        None ->
          // Circular dependency (in this DFS, or in an outer evaluation a
          // deferred-namespace trigger re-entered from) — return without
          // re-entering
          case
            set.contains(state.evaluating, specifier)
            || registry.read_module_status(state.heap, global_object, specifier)
            == Some(registry.Evaluating)
          {
            True -> #(state, Ok(#(JsUndefined, state.heap)))
            False ->
              case dict.get(bundle.modules, specifier) {
                Error(Nil) -> #(state, Error(ModuleNotInBundle(specifier:)))
                Ok(compiled) ->
                  eval_module_body(
                    bundle,
                    linked,
                    state,
                    specifier,
                    compiled,
                    builtins,
                    global_object,
                    host_hooks,
                    finish,
                  )
              }
          }
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
  builtins: Builtins,
  global_object: Ref,
  host_hooks: state.HostHooks,
  finish: fn(state.State(host)) -> state.State(host),
) -> #(EvalState(host), Result(#(JsValue, Heap(host)), ModuleError(host))) {
  // Mark as evaluating
  let state =
    EvalState(..state, evaluating: set.insert(state.evaluating, specifier))

  // Evaluate dependencies first (DFS post-order), following the
  // §16.2.1.5.3.1 InnerModuleEvaluation evaluationList: requests are visited
  // in declaration order; an ~evaluation~-phase request evaluates the module
  // itself, while a ~defer~-phase request only evaluates the module's
  // ASYNCHRONOUS transitive dependencies (its synchronous evaluation is
  // triggered later, by first access on the deferred namespace).
  let requests = ordered_requests(compiled)
  let #(state, dep_result) = {
    use state, Nil, #(raw_dep, eager) <- try_fold_state(requests, state, Nil)
    let dep_specifier =
      dict.get(compiled.specifier_map, raw_dep)
      |> result.unwrap(raw_dep)
    let to_evaluate = case eager {
      True -> [dep_specifier]
      False ->
        gather_async_transitive_deps(
          bundle,
          state,
          global_object,
          dep_specifier,
          set.new(),
        ).0
    }
    use state, Nil, dep <- try_fold_state(to_evaluate, state, Nil)
    let #(state, r) =
      eval_module_inner(
        bundle,
        linked,
        state,
        dep,
        builtins,
        global_object,
        host_hooks,
        finish,
      )
    #(state, result.replace(r, Nil))
  }

  case dep_result {
    // A dependency parked on top-level await is not a failure — propagate
    // without caching an error (the dependency may still complete later).
    Error(EvaluationPending(promise_data_ref: _, heap: _) as err) -> #(
      state,
      Error(err),
    )
    Error(err) -> {
      // Dependency failed — cache the error on this module too
      let error_val = case err {
        EvaluationError(value: val, ..) -> val
        _ -> JsString(string.inspect(err))
      }
      let heap =
        registry.write_module_error(
          state.heap,
          global_object,
          specifier,
          error_val,
        )
      let state =
        EvalState(
          ..state,
          heap:,
          errors: dict.insert(state.errors, specifier, error_val),
        )
      #(state, Error(err))
    }
    Ok(Nil) -> {
      // Bindings already exist (pre-allocated at link time). Seed the slots:
      // imports as boxed captures in slots 0..N-1 (each the *exporter's* live
      // cell), plus this module's own export cells into their declared slots so
      // the body reads/writes through the shared boxes.
      let seeds =
        import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
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
          builtins,
          global_object,
          seeds,
          host_hooks,
          finish,
        )
      {
        entry.ModuleError(error: vm_err) -> {
          let error_val =
            JsString("InternalError: " <> vm_error_message(vm_err))
          let heap =
            registry.clear_module_status(heap, global_object, specifier)
          let heap =
            registry.write_module_error(
              heap,
              global_object,
              specifier,
              error_val,
            )
          let state =
            EvalState(
              ..state,
              heap:,
              errors: dict.insert(state.errors, specifier, error_val),
            )
          #(state, Error(EvaluationError(error_val, state.heap)))
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
              errors: dict.insert(state.errors, specifier, thrown_val),
              jobs: list.append(state.jobs, jobs),
            )
          #(state, Error(EvaluationError(thrown_val, new_heap)))
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
              evaluated: set.insert(state.evaluated, specifier),
              evaluating: set.delete(state.evaluating, specifier),
              jobs: list.append(state.jobs, jobs),
            )
          #(state, Ok(#(val, new_heap)))
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
          #(state, Error(EvaluationPending(promise_data_ref:, heap: new_heap)))
        }
      }
    }
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Run a module body with `host_hooks.import_referrer` set to its resolved
/// specifier on the body's freshly booted State. An ImportCall inside the
/// body captures this as its referencingScriptOrModule (§16.2.1.8
/// HostLoadImportedModule), so nested dynamic imports resolve relative to the
/// importing MODULE, not the realm's entry script.
///
/// The referrer is per-boot ENGINE state on `RealmCtx.host_hooks`, not a
/// mutable globalThis property, so there is nothing to save/restore around
/// the body and guest JS can neither read nor forge it.
fn run_module_with_referrer(
  specifier: String,
  template: value.FuncTemplate,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  seeds: List(#(Int, JsValue)),
  host_hooks: state.HostHooks,
  finish: fn(state.State(host)) -> state.State(host),
) -> entry.ModuleResult(host) {
  entry.run_module(
    template,
    heap,
    builtins,
    global_object,
    seeds,
    state.HostHooks(..host_hooks, import_referrer: Some(specifier)),
    finish,
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
  let specs =
    list.append(dict.keys(bundle.modules), dict.keys(bundle.host_modules))
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
          let map =
            link.exported_names(lg, spec)
            |> list.fold(dict.new(), fn(map, name) {
              let found = case link.resolve_export(lg, spec, name) {
                link.ResolvedTo(owner, binding) ->
                  dict.get(local_boxes, owner)
                  |> result.try(dict.get(_, binding))
                link.ResolvedNamespace(target) ->
                  dict.get(namespace_boxes, target)
                link.ResolvedDeferredNamespace(target) ->
                  dict.get(deferred_boxes, target)
                link.Unresolvable | link.Ambiguous -> Error(Nil)
              }
              result.map(found, dict.insert(map, name, _))
              |> result.unwrap(map)
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
      let exp = dict.get(exports, spec) |> result.unwrap(dict.new())
      heap.write(heap, obj, namespace_slot(exp))
    })
  #(
    heap,
    Linked(local_boxes:, exports:, namespace_boxes:, deferred_boxes:),
    deferred_to_fill,
  )
}

/// A module's [[RequestedModules]] in declaration order as
/// (raw specifier, eager?) pairs. Both halves were computed by esm.analyze's
/// merge_requests at compile time: `requested_modules` is the unique
/// specifiers in source order, `eager_requests` is the subset whose merged
/// phase is ~evaluation~ (a specifier is Deferred only if EVERY reference
/// is `import defer * as ns`).
fn ordered_requests(compiled: CompiledModule) -> List(#(String, Bool)) {
  let eager = set.from_list(compiled.eager_requests)
  use specifier <- list.map(compiled.requested_modules)
  #(specifier, set.contains(eager, specifier))
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
  let already_done =
    set.contains(state.evaluated, spec)
    || set.contains(state.evaluating, spec)
    || registry.read_module_status(state.heap, global_object, spec) != None
  use <- bool.guard(already_done, #([], seen))
  case dict.get(bundle.modules, spec) {
    Error(Nil) -> #([], seen)
    Ok(m) ->
      case m.has_tla {
        True -> #([spec], seen)
        False ->
          list.fold(m.requested_modules, #([], seen), fn(acc, raw) {
            let #(found, seen) = acc
            let #(more, seen) =
              gather_async_transitive_deps(
                bundle,
                state,
                global_object,
                resolved_specifier(m, raw),
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
  builtins: Builtins,
  global_object: Ref,
  host_hooks: state.HostHooks,
  finish: fn(state.State(host)) -> state.State(host),
) -> #(
  Heap(host),
  List(value.Job),
  Result(List(#(String, Ref)), ModuleError(host)),
) {
  let LinkedBundle(bundle:, linked:, namespace: _) = linked_bundle
  let eval_state =
    EvalState(
      heap:,
      evaluated: set.new(),
      errors: dict.new(),
      evaluating: set.new(),
      jobs: [],
    )
  let #(to_evaluate, _seen) =
    gather_async_transitive_deps(
      bundle,
      eval_state,
      global_object,
      bundle.entry,
      set.new(),
    )
  let #(eval_state, result) = {
    use eval_state, pendings, dep <- try_fold_state(to_evaluate, eval_state, [])
    let #(eval_state, dep_result) =
      eval_module_inner(
        bundle,
        linked,
        eval_state,
        dep,
        builtins,
        global_object,
        host_hooks,
        finish,
      )
    case dep_result {
      Ok(_) -> #(eval_state, Ok(pendings))
      // Parked on top-level await: record the capability and keep
      // going — the spec Evaluate()s every gathered module before
      // waiting on all their promises.
      Error(EvaluationPending(promise_data_ref:, heap: _)) -> #(
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
  dict.fold(bundle.modules, [], fn(acc, _spec, m) {
    list.fold(m.import_bindings, acc, fn(acc, entry) {
      let #(raw_dep, bindings) = entry
      let is_deferred =
        list.any(bindings, fn(binding) {
          case binding {
            esm.NamespaceImport(phase: esm.Deferred, ..) -> True
            _ -> False
          }
        })
      case is_deferred {
        True -> [resolved_specifier(m, raw_dep), ..acc]
        False -> acc
      }
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
  dict.fold(bundle.modules, heap, fn(heap, spec, compiled) {
    // A preexisting module's export cells hold their final values — don't
    // overwrite them with link-time placeholder closures.
    use <- bool.guard(set.contains(already_evaluated, spec), heap)
    let local_boxes =
      dict.get(linked.local_boxes, spec) |> result.unwrap(dict.new())
    // Reconstruct the module's seeded frame so closures capture the same cells
    // a body run would (imports in slots 0..N-1, own exports in their slots).
    let seeds =
      import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
      |> list.append(own_export_seeds(linked, compiled))
    let frame = interpreter.init_module_locals(compiled.template, seeds)
    list.fold(compiled.hoisted_funcs, heap, fn(heap, hf) {
      let #(name, func_idx) = hf
      // Only exported functions have a shared cell; the rest are body-local.
      case dict.get(local_boxes, name) {
        Error(Nil) -> heap
        Ok(box) -> {
          let child =
            tuple_array.unsafe_get(func_idx, compiled.template.functions)
          let captures =
            list.map(child.env_descriptors, fn(desc) {
              case desc {
                value.CaptureLocal(i) -> tuple_array.unsafe_get(i, frame)
                value.CaptureEnv(_) -> JsUndefined
              }
            })
          let #(heap, closure) =
            interpreter.make_closure(heap, builtins, child, captures)
          heap.write(heap, box, BoxSlot(JsObject(closure)))
        }
      }
    })
  })
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
  // Host modules first: one box per export, seeded with the host value itself
  // (no body ever runs to fill it — the seed IS the final binding).
  let #(heap, with_hosts) =
    dict.fold(bundle.host_modules, #(heap, dict.new()), fn(acc, spec, hm) {
      let #(heap, all) = acc
      let #(heap, boxes) =
        list.fold(hm.exports, #(heap, dict.new()), fn(a, export) {
          let #(heap, boxes) = a
          let #(name, val) = export
          let #(heap, box) = alloc_box(heap, val)
          #(heap, dict.insert(boxes, name, box))
        })
      #(heap, dict.insert(all, spec, boxes))
    })
  dict.fold(bundle.modules, #(heap, with_hosts), fn(acc, spec, m) {
    let #(heap, all) = acc
    case dict.get(preexisting, spec) {
      Ok(#(_, existing_exports)) -> {
        let boxes =
          list.fold(m.export_entries, dict.new(), fn(boxes, e) {
            case e {
              esm.LocalExport(export_name:, local_name:) ->
                case dict.get(existing_exports, export_name) {
                  Ok(box) -> dict.insert(boxes, local_name, box)
                  Error(Nil) -> boxes
                }
              _ -> boxes
            }
          })
        #(heap, dict.insert(all, spec, boxes))
      }
      Error(Nil) -> {
        let #(heap, boxes) =
          dict.fold(m.export_seeds, #(heap, dict.new()), fn(a, local, seed) {
            let #(heap, boxes) = a
            let #(heap, box) = alloc_box(heap, seed)
            #(heap, dict.insert(boxes, local, box))
          })
        #(heap, dict.insert(all, spec, boxes))
      }
    }
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
  let exports = dict.get(linked.exports, spec) |> result.unwrap(dict.new())
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
        target: Some(target_ref),
        handler: Some(handler_ref),
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
  common.alloc_host_fn(
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
              Error(state.type_error_value(
                state,
                "Cannot synchronously evaluate deferred module: '"
                  <> spec
                  <> "' is still evaluating or has an unevaluated async dependency",
              ))
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
        // already or will be loaded by its own bundle; treat as ready.
        Error(Nil) -> #(True, seen)
        Ok(m) ->
          case m.has_tla {
            True -> #(False, seen)
            False ->
              list.fold(m.eager_requests, #(True, seen), fn(acc, raw) {
                let #(ok, seen) = acc
                case ok {
                  False -> #(False, seen)
                  True ->
                    ready_for_sync_execution(
                      bundle,
                      h,
                      global_object,
                      resolved_specifier(m, raw),
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
  let global_object = state.ctx.global_object
  let specs = dict.keys(bundle.modules)
  let #(evaluated, evaluating) =
    list.fold(specs, #(set.new(), set.new()), fn(acc, s) {
      let #(done, running) = acc
      case registry.read_module_status(state.heap, global_object, s) {
        Some(registry.Evaluated) -> #(set.insert(done, s), running)
        Some(registry.Evaluating) -> #(done, set.insert(running, s))
        _ -> acc
      }
    })
  let errors =
    list.fold(specs, dict.new(), fn(acc, s) {
      case registry.read_module_error(state.heap, global_object, s) {
        Some(err) -> dict.insert(acc, s, err)
        None -> acc
      }
    })
  let st =
    EvalState(heap: state.heap, evaluated:, errors:, evaluating:, jobs: [])
  let #(st, result) =
    eval_module_inner(
      bundle,
      linked,
      st,
      spec,
      builtins,
      global_object,
      state.ctx.host_hooks,
      fn(s) { s },
    )
  let state =
    State(
      ..state,
      heap: st.heap,
      job_queue: job_queue.append(state.job_queue, st.jobs),
    )
  case result {
    Ok(_) -> Ok(state)
    Error(EvaluationError(value:, heap:)) ->
      Error(#(value, State(..state, heap:)))
    Error(other) ->
      Error(state.type_error_value(
        state,
        "Failed to evaluate deferred module '"
          <> spec
          <> "': "
          <> string.inspect(other),
      ))
  }
}

// -----------------------------------------------------------------------------
// Seeding — place the pre-allocated cells into a module's local slots.
// -----------------------------------------------------------------------------

/// Import bindings seeded into capture slots 0..N-1, in declaration order
/// (matching compiler.import_local_names). Named/default forward the
/// exporting module's live cell; namespace imports get the shared namespace box.
fn import_seeds(
  linked: Linked,
  specifier_map: Dict(String, String),
  import_bindings: List(#(String, List(esm.ImportBinding))),
) -> List(#(Int, JsValue)) {
  list.flat_map(import_bindings, fn(entry) {
    let #(raw_dep, bindings) = entry
    let dep = dict.get(specifier_map, raw_dep) |> result.unwrap(raw_dep)
    let dep_exports = dict.get(linked.exports, dep) |> result.unwrap(dict.new())
    list.map(bindings, fn(binding) {
      case binding {
        esm.NamedImport(imported:, ..) -> forward_box(dep_exports, imported)
        esm.DefaultImport(..) -> forward_box(dep_exports, "default")
        esm.NamespaceImport(phase: esm.Deferred, ..) ->
          case dict.get(linked.deferred_boxes, dep) {
            Ok(box) -> JsObject(box)
            Error(Nil) -> JsUndefined
          }
        esm.NamespaceImport(phase: esm.Default, ..) ->
          case dict.get(linked.namespace_boxes, dep) {
            Ok(box) -> JsObject(box)
            Error(Nil) -> JsUndefined
          }
      }
    })
  })
  |> list.index_map(fn(box, idx) { #(idx, box) })
}

/// The exporting module's live cell for `name` (link-checked to exist).
fn forward_box(dep_exports: Dict(String, Ref), name: String) -> JsValue {
  case dict.get(dep_exports, name) {
    Ok(box) -> JsObject(box)
    Error(Nil) -> JsUndefined
  }
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
    set.from_list(compiler.import_local_names(compiled.import_bindings))
  dict.get(linked.local_boxes, compiled.specifier)
  |> result.unwrap(dict.new())
  |> dict.to_list
  |> list.filter_map(fn(pair) {
    let #(local_name, box) = pair
    use <- bool.guard(set.contains(import_locals, local_name), Error(Nil))
    case dict.get(compiled.scope_dict, local_name) {
      Ok(index) -> Ok(#(index, JsObject(box)))
      Error(Nil) -> Error(Nil)
    }
  })
}
