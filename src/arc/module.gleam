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
import arc/module/graph
import arc/parser
import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/reflect
import arc/vm/exec/dynamic_import
import arc/vm/exec/entry
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/job_queue
import arc/vm/internal/tuple_array
import arc/vm/opcode
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
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

/// A complete compiled module graph — the output of AOT compilation.
/// Pure Erlang term, serializable via term_to_binary.
pub type ModuleBundle {
  ModuleBundle(entry: String, modules: Dict(String, CompiledModule))
}

// =============================================================================
// Errors
// =============================================================================

pub type ModuleError {
  ParseError(String)
  CompileError(String)
  ResolutionError(String)
  LinkError(String)
  /// A module threw during evaluation. Carries both the thrown value and the
  /// heap it was allocated in — the value is a Ref into this heap, so callers
  /// must use it (not a pre-evaluation heap) to inspect the thrown object.
  EvaluationError(value: JsValue, heap: Heap)
  /// Evaluation is parked on top-level await and the supplied `finish`
  /// driver did not settle it (only reachable with a non-draining driver —
  /// the dynamic-import path). `promise_data_ref` is the entry module's
  /// [[TopLevelCapability]] promise data: per Evaluate() step 4 the host
  /// must chain onto this promise (and hand any returned jobs to its own
  /// event loop) rather than treat the module as failed.
  EvaluationPending(promise_data_ref: Ref, heap: Heap)
}

/// The successful result of `evaluate_bundle`: the entry module's completion
/// value, the resulting heap, and the entry module's Module Namespace Exotic
/// Object (§10.4.6). Read named exports off `namespace` via its [[Get]] — a
/// live, TDZ-throwing, write-protected view of the export bindings. This is the
/// embedder's `GetModuleNamespace` handle (cf. V8 `Module::GetModuleNamespace`,
/// QuickJS `JS_GetModuleNamespace`).
pub type EvaluatedBundle {
  EvaluatedBundle(value: JsValue, heap: Heap, namespace: Option(JsValue))
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
) -> Result(ModuleBundle, ModuleError) {
  // Adapt the runtime's raw-specifier resolver to the graph layer's
  // request-taking one.
  let resolve_request = fn(request: esm.ModuleRequest, referrer) {
    resolve(request.specifier, referrer)
  }
  use source_graph <- result.try(
    graph.load(entry_specifier, entry_source, resolve_request, load)
    |> result.map_error(graph_error),
  )
  use modules <- result.map(
    dict.fold(source_graph.modules, Ok(dict.new()), fn(acc, specifier, node) {
      use modules <- result.try(acc)
      use compiled <- result.map(compile_source_module(node))
      dict.insert(modules, specifier, compiled)
    }),
  )
  ModuleBundle(entry: entry_specifier, modules:)
}

fn graph_error(error: graph.GraphError) -> ModuleError {
  case error {
    graph.ParseFailed(specifier, parse_error) ->
      ParseError(
        "SyntaxError in '"
        <> specifier
        <> "': "
        <> parser.parse_error_to_string(parse_error),
      )
    graph.ResolveFailed(raw, referrer, message) ->
      ResolutionError(
        "Cannot resolve module '"
        <> raw
        <> "' from '"
        <> referrer
        <> "': "
        <> message,
      )
    graph.LoadFailed(specifier, message) ->
      ResolutionError("Cannot load module '" <> specifier <> "': " <> message)
    // Spec-wise a link-time SyntaxError: GetModuleSource for a Source Text
    // Module Record always throws (§16.2.1.7.2), and this host has no other
    // module kinds. LinkError surfaces to JS as a SyntaxError rejection.
    graph.SourcePhaseUnsupported(specifier) ->
      LinkError(
        "'"
        <> specifier
        <> "': source phase imports ('import source') are not supported",
      )
  }
}

/// Compile one loaded module from the source graph — the import/export
/// analysis is already done (`node.summary`); this adds the bytecode stage.
fn compile_source_module(
  node: graph.SourceModule,
) -> Result(CompiledModule, ModuleError) {
  let graph.SourceModule(
    specifier:,
    source: _,
    program:,
    summary:,
    specifier_map:,
  ) = node
  use #(template, scope_dict, hoisted_funcs) <- result.map(
    compiler.compile_module(program, summary)
    |> result.map_error(fn(err) {
      case err {
        compiler.Unsupported(desc) ->
          CompileError("Unsupported in '" <> specifier <> "': " <> desc)
        compiler.BreakOutsideLoop ->
          CompileError("break outside loop in '" <> specifier <> "'")
        compiler.ContinueOutsideLoop ->
          CompileError("continue outside loop in '" <> specifier <> "'")
      }
    }),
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
/// inside nested functions compiles into CHILD templates, so an Await opcode
/// in the module's own bytecode is exactly a top-level await.
fn module_has_tla(template: value.FuncTemplate) -> Bool {
  tuple_array.to_list(template.bytecode)
  |> list.any(fn(op) { op == opcode.Await })
}

// =============================================================================
// Linking — ResolveExport (§16.2.1.6.3) + import/re-export checks (§16.2.1.6.4)
// =============================================================================

/// Result of resolving an export name through a module graph.
type ExportResolution {
  /// Resolves to a concrete binding `binding` owned by module `module`.
  ResolvedTo(module: String, binding: String)
  /// Resolves to a module namespace object (`export * as ns from`, or
  /// `export { ns }` of an `import * as ns` binding).
  ResolvedNamespace(module: String)
  /// Resolves to a DEFERRED module namespace (`export { ns }` of an
  /// `import defer * as ns` binding) — importers receive the deferred proxy.
  ResolvedDeferredNamespace(module: String)
  /// No export of this name exists (or only via a circular path).
  Unresolvable
  /// Two distinct `export *` sources provide the name — ambiguous.
  Ambiguous
}

/// §16.2.1.6.3 ResolveExport. `resolve_set` guards circular re-exports.
fn resolve_export(
  bundle: ModuleBundle,
  specifier: String,
  name: String,
  resolve_set: Set(#(String, String)),
) -> ExportResolution {
  case set.contains(resolve_set, #(specifier, name)) {
    // Already resolving this exact export → circular request, not resolvable.
    True -> Unresolvable
    False ->
      case dict.get(bundle.modules, specifier) {
        Error(Nil) -> Unresolvable
        Ok(m) ->
          resolve_export_in(
            bundle,
            m,
            specifier,
            name,
            set.insert(resolve_set, #(specifier, name)),
          )
      }
  }
}

fn resolve_export_in(
  bundle: ModuleBundle,
  m: CompiledModule,
  specifier: String,
  name: String,
  resolve_set: Set(#(String, String)),
) -> ExportResolution {
  // Direct local export, then named/namespace re-export, take priority over
  // `export *` (§16.2.1.6.3 steps 4–6 before step 7).
  let direct =
    list.find_map(m.export_entries, fn(e) {
      case e {
        esm.LocalExport(export_name:, local_name:) if export_name == name ->
          Ok(resolve_local_export(bundle, m, specifier, local_name, resolve_set))
        esm.ReExport(export_name:, imported_name:, source_specifier:)
          if export_name == name
        -> {
          let src = resolved_specifier(m, source_specifier)
          Ok(resolve_export(bundle, src, imported_name, resolve_set))
        }
        esm.ReExportNamespace(export_name:, source_specifier:)
          if export_name == name
        -> Ok(ResolvedNamespace(resolved_specifier(m, source_specifier)))
        _ -> Error(Nil)
      }
    })
  case direct {
    Ok(resolution) -> resolution
    // `default` is never provided by `export *` (step 6).
    Error(Nil) ->
      case name {
        "default" -> Unresolvable
        _ -> resolve_star_exports(bundle, m, name, resolve_set)
      }
  }
}

/// A LocalExport (`export { x }` / `export let x`) whose local name is an
/// IMPORT binding is really a re-export: `import { a } from "m"; export { a }`
/// resolves through "m" (§16.2.1.6.3 — the binding is the imported one), and
/// `import [defer] * as ns from "m"; export { ns }` resolves to "m"'s
/// (deferred) namespace. Genuine local bindings resolve to their own cell.
fn resolve_local_export(
  bundle: ModuleBundle,
  m: CompiledModule,
  specifier: String,
  local_name: String,
  resolve_set: Set(#(String, String)),
) -> ExportResolution {
  let import_binding =
    list.find_map(m.import_bindings, fn(entry) {
      let #(raw_dep, bindings) = entry
      list.find_map(bindings, fn(binding) {
        case binding {
          esm.NamedImport(local:, ..) if local == local_name ->
            Ok(#(raw_dep, binding))
          esm.DefaultImport(local:) if local == local_name ->
            Ok(#(raw_dep, binding))
          esm.NamespaceImport(local:, ..) if local == local_name ->
            Ok(#(raw_dep, binding))
          _ -> Error(Nil)
        }
      })
    })
  case import_binding {
    Error(Nil) -> ResolvedTo(specifier, local_name)
    Ok(#(raw_dep, binding)) -> {
      let dep = resolved_specifier(m, raw_dep)
      case binding {
        esm.NamedImport(imported:, ..) ->
          resolve_export(bundle, dep, imported, resolve_set)
        esm.DefaultImport(..) ->
          resolve_export(bundle, dep, "default", resolve_set)
        esm.NamespaceImport(phase: esm.Deferred, ..) ->
          ResolvedDeferredNamespace(dep)
        esm.NamespaceImport(phase: esm.Default, ..) -> ResolvedNamespace(dep)
      }
    }
  }
}

/// §16.2.1.6.3 step 7: gather across `export *` sources, flagging ambiguity.
fn resolve_star_exports(
  bundle: ModuleBundle,
  m: CompiledModule,
  name: String,
  resolve_set: Set(#(String, String)),
) -> ExportResolution {
  let star_sources =
    list.filter_map(m.export_entries, fn(e) {
      case e {
        esm.ReExportAll(source_specifier:) ->
          Ok(resolved_specifier(m, source_specifier))
        _ -> Error(Nil)
      }
    })
  list.fold(star_sources, Unresolvable, fn(acc, src) {
    case acc {
      Ambiguous -> Ambiguous
      _ ->
        case resolve_export(bundle, src, name, resolve_set), acc {
          Ambiguous, _ -> Ambiguous
          Unresolvable, _ -> acc
          found, Unresolvable -> found
          ResolvedTo(m1, b1), ResolvedTo(m2, b2) ->
            case m1 == m2 && b1 == b2 {
              True -> acc
              False -> Ambiguous
            }
          ResolvedNamespace(m1), ResolvedNamespace(m2) ->
            case m1 == m2 {
              True -> acc
              False -> Ambiguous
            }
          ResolvedDeferredNamespace(m1), ResolvedDeferredNamespace(m2) ->
            case m1 == m2 {
              True -> acc
              False -> Ambiguous
            }
          _, _ -> Ambiguous
        }
    }
  })
}

fn resolved_specifier(m: CompiledModule, raw: String) -> String {
  dict.get(m.specifier_map, raw) |> result.unwrap(raw)
}

/// Verify every import and indirect re-export in the graph resolves to a
/// unique binding. Returns the SyntaxError message for the first failure.
fn link_bundle(bundle: ModuleBundle) -> Result(Nil, String) {
  list.try_each(dict.to_list(bundle.modules), fn(entry) {
    let #(specifier, m) = entry
    use _ <- result.try(check_imports(bundle, m))
    check_indirect_exports(bundle, specifier, m)
  })
}

fn check_imports(
  bundle: ModuleBundle,
  m: CompiledModule,
) -> Result(Nil, String) {
  list.try_each(m.import_bindings, fn(entry) {
    let #(raw_dep, bindings) = entry
    let dep = resolved_specifier(m, raw_dep)
    list.try_each(bindings, fn(binding) {
      case binding {
        // `import * as ns` always resolves (the namespace gathers names).
        esm.NamespaceImport(..) -> Ok(Nil)
        esm.NamedImport(imported:, ..) ->
          check_resolves(bundle, dep, imported, raw_dep)
        esm.DefaultImport(..) -> check_resolves(bundle, dep, "default", raw_dep)
      }
    })
  })
}

fn check_indirect_exports(
  bundle: ModuleBundle,
  specifier: String,
  m: CompiledModule,
) -> Result(Nil, String) {
  list.try_each(m.export_entries, fn(e) {
    case e {
      // `export { x } from 'mod'` — resolve THIS module's export name, which
      // recurses into the source (§16.2.1.6.4 step 1).
      esm.ReExport(export_name:, source_specifier:, ..) ->
        check_resolves(bundle, specifier, export_name, source_specifier)
      _ -> Ok(Nil)
    }
  })
}

fn check_resolves(
  bundle: ModuleBundle,
  specifier: String,
  name: String,
  raw_dep: String,
) -> Result(Nil, String) {
  case resolve_export(bundle, specifier, name, set.new()) {
    ResolvedTo(..) | ResolvedNamespace(..) | ResolvedDeferredNamespace(..) ->
      Ok(Nil)
    Unresolvable ->
      Error(
        "The requested module '"
        <> raw_dep
        <> "' does not provide an export named '"
        <> name
        <> "'",
      )
    Ambiguous ->
      Error(
        "The requested module '"
        <> raw_dep
        <> "' provides an ambiguous export named '"
        <> name
        <> "'",
      )
  }
}

// =============================================================================
// Module status registry (heap-resident, on hidden global-object properties)
// =============================================================================
//
// Deferred evaluation needs module evaluation state that BOTH the link-time
// DFS evaluator and a deferred namespace's runtime trigger (which fires while
// some other module's body is mid-execution) can observe. Gleam data is
// immutable, so that shared state lives in the heap: hidden objects on the
// global, keyed by resolved specifier — the same technique module_host uses
// for its namespace registry.

/// Hidden global property: resolved specifier → JsString("evaluating") while
/// the module's body runs / is parked on top-level await, JsString("evaluated")
/// once it completes. Absent/undefined = not yet started ([[Status]] ~linked~).
pub const status_property = "__arc_module_status__"

/// Hidden global property: resolved specifier → the value the module's
/// evaluation threw. Sticky — re-evaluation attempts rethrow it (§16.2.1.5.3).
/// Shared with module_host's dynamic-import error cache.
pub const error_cache_property = "__arc_module_errors__"

const status_evaluating = "evaluating"

const status_evaluated = "evaluated"

fn read_hidden_cache(
  h: Heap,
  global_object: Ref,
  property: String,
  key: String,
) -> Option(JsValue) {
  case object.get_own_property(h, global_object, value.Named(property)) {
    Some(value.DataProperty(value: JsObject(cache_ref), ..)) ->
      case object.get_own_property(h, cache_ref, value.Named(key)) {
        Some(value.DataProperty(value: cached, ..)) -> Some(cached)
        _ -> None
      }
    _ -> None
  }
}

fn write_hidden_cache(
  h: Heap,
  global_object: Ref,
  property: String,
  key: String,
  val: JsValue,
) -> Heap {
  let #(h, cache_ref) = case
    object.get_own_property(h, global_object, value.Named(property))
  {
    Some(value.DataProperty(value: JsObject(cache_ref), ..)) -> #(h, cache_ref)
    _ -> {
      let #(h, cache_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: value.OrdinaryObject,
            properties: dict.new(),
            elements: elements.new(),
            prototype: None,
            symbol_properties: [],
            extensible: True,
          ),
        )
      let h =
        object.define_method_property(
          h,
          global_object,
          value.Named(property),
          JsObject(cache_ref),
        )
      #(h, cache_ref)
    }
  }
  let #(h, _) = object.set_property(h, cache_ref, value.Named(key), val)
  h
}

/// Public view of a module's evaluation status for registry-keeping hosts:
/// Some("evaluated") once its body completed, Some("evaluating") while it
/// runs (or is parked on top-level await), None when it has not started.
pub fn evaluation_status(
  h: Heap,
  global_object: Ref,
  spec: String,
) -> Option(String) {
  read_module_status(h, global_object, spec)
}

fn read_module_status(
  h: Heap,
  global_object: Ref,
  spec: String,
) -> Option(String) {
  case read_hidden_cache(h, global_object, status_property, spec) {
    Some(JsString(s)) -> Some(s)
    Some(_) | None -> None
  }
}

fn write_module_status(
  h: Heap,
  global_object: Ref,
  spec: String,
  status: String,
) -> Heap {
  write_hidden_cache(h, global_object, status_property, spec, JsString(status))
}

fn clear_module_status(h: Heap, global_object: Ref, spec: String) -> Heap {
  write_hidden_cache(h, global_object, status_property, spec, JsUndefined)
}

fn read_module_error(
  h: Heap,
  global_object: Ref,
  spec: String,
) -> Option(JsValue) {
  case read_hidden_cache(h, global_object, error_cache_property, spec) {
    Some(JsUndefined) | None -> None
    Some(err) -> Some(err)
  }
}

fn write_module_error(
  h: Heap,
  global_object: Ref,
  spec: String,
  err: JsValue,
) -> Heap {
  write_hidden_cache(h, global_object, error_cache_property, spec, err)
}

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
type EvalState {
  EvalState(
    heap: Heap,
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
  heap: Heap,
  builtins: Builtins,
) -> Result(#(Heap, LinkedBundle), ModuleError) {
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
  heap: Heap,
  builtins: Builtins,
  preexisting: Dict(String, Ref),
  preexisting_deferred: Dict(String, Ref),
) -> Result(#(Heap, LinkedBundle), ModuleError) {
  case link_bundle(bundle) {
    Error(message) -> {
      let #(heap, err) = common.make_syntax_error(heap, builtins, message)
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
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  prepare: fn(state.State) -> state.State,
  finish: fn(state.State) -> state.State,
) -> Result(EvaluatedBundle, ModuleError) {
  let #(_evaluated, _jobs, result) =
    evaluate_linked_tracking(
      linked_bundle,
      heap,
      builtins,
      global_object,
      prepare,
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
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  prepare: fn(state.State) -> state.State,
  finish: fn(state.State) -> state.State,
  already_evaluated: Set(String),
) -> #(Set(String), List(value.Job), Result(EvaluatedBundle, ModuleError)) {
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
      prepare,
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

/// Every module in a linked bundle paired with its Module Namespace Exotic
/// Object — what a registry-keeping host records so a later import of any
/// graph module (entry or dependency) reuses the same record (§16.2.1.8).
pub fn linked_namespaces(
  linked_bundle: LinkedBundle,
  heap: Heap,
) -> List(#(String, JsValue)) {
  dict.fold(linked_bundle.linked.namespace_boxes, [], fn(acc, spec, box) {
    case heap.read_box(heap, box) {
      Some(ns) -> [#(spec, ns), ..acc]
      None -> acc
    }
  })
}

/// Every Deferred Module Namespace in a linked bundle, for the registry: a
/// later `import defer` / `import.defer()` of the same module must yield the
/// identical object ([[DeferredNamespace]] is per module record).
pub fn linked_deferred_namespaces(
  linked_bundle: LinkedBundle,
  heap: Heap,
) -> List(#(String, JsValue)) {
  dict.fold(linked_bundle.linked.deferred_boxes, [], fn(acc, spec, box) {
    case heap.read_box(heap, box) {
      Some(ns) -> [#(spec, ns), ..acc]
      None -> acc
    }
  })
}

/// The Deferred Module Namespace for `spec`, creating one if no importer in
/// the bundle deferred it statically (the dynamic `import.defer()` path).
pub fn get_or_create_deferred_namespace(
  heap: Heap,
  builtins: Builtins,
  linked_bundle: LinkedBundle,
  spec: String,
) -> #(Heap, Option(JsValue)) {
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
pub fn evaluate_bundle(
  bundle: ModuleBundle,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  finish: fn(state.State) -> state.State,
) -> Result(EvaluatedBundle, ModuleError) {
  evaluate_bundle_prepared(
    bundle,
    heap,
    builtins,
    global_object,
    fn(s) { s },
    finish,
  )
}

/// `evaluate_bundle` with an embedder `prepare` hook applied to each module
/// body's freshly booted State before it executes (bodies get fresh States,
/// so this is per-body) — the injection point for the host Atomics
/// capabilities when an embedder macrotask loop (e.g. `arc/beam.run`)
/// drives evaluation: a module's top level may hit a blocking
/// `Atomics.wait` before any host function has run.
pub fn evaluate_bundle_prepared(
  bundle: ModuleBundle,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  prepare: fn(state.State) -> state.State,
  finish: fn(state.State) -> state.State,
) -> Result(EvaluatedBundle, ModuleError) {
  use #(heap, linked_bundle) <- result.try(link_for_evaluation(
    bundle,
    heap,
    builtins,
  ))
  evaluate_linked(linked_bundle, heap, builtins, global_object, prepare, finish)
}

/// The entry module's Module Namespace Exotic Object, read out of the rooted
/// box the linker reserved for it (`build_linked`). This is what `evaluate_bundle`
/// hands back as `EvaluatedBundle.namespace`. Falls back to `undefined` only if
/// the entry has no namespace box, which shouldn't happen for a linked bundle.
fn entry_namespace(
  linked: Linked,
  entry: String,
  heap: Heap,
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
  heap: Heap,
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
  state: EvalState,
  specifier: String,
  builtins: Builtins,
  global_object: Ref,
  prepare: fn(state.State) -> state.State,
  finish: fn(state.State) -> state.State,
) -> #(EvalState, Result(#(JsValue, Heap), ModuleError)) {
  // Already evaluated successfully — either in this DFS or recorded in the
  // realm's heap-resident status registry (a deferred-namespace trigger may
  // have evaluated it mid-DFS, re-entrantly).
  let already_evaluated =
    set.contains(state.evaluated, specifier)
    || read_module_status(state.heap, global_object, specifier)
    == Some(status_evaluated)
  case already_evaluated {
    True -> #(state, Ok(#(JsUndefined, state.heap)))
    False ->
      // Cached error — re-throw, never re-evaluate
      case
        dict.get(state.errors, specifier)
        |> option.from_result
        |> option.or(read_module_error(state.heap, global_object, specifier))
      {
        Some(err_val) -> #(state, Error(EvaluationError(err_val, state.heap)))
        None ->
          // Circular dependency (in this DFS, or in an outer evaluation a
          // deferred-namespace trigger re-entered from) — return without
          // re-entering
          case
            set.contains(state.evaluating, specifier)
            || read_module_status(state.heap, global_object, specifier)
            == Some(status_evaluating)
          {
            True -> #(state, Ok(#(JsUndefined, state.heap)))
            False ->
              case dict.get(bundle.modules, specifier) {
                Error(Nil) -> #(
                  state,
                  Error(ResolutionError(
                    "Module '" <> specifier <> "' not found in bundle",
                  )),
                )
                Ok(compiled) ->
                  eval_module_body(
                    bundle,
                    linked,
                    state,
                    specifier,
                    compiled,
                    builtins,
                    global_object,
                    prepare,
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
  state: EvalState,
  specifier: String,
  compiled: CompiledModule,
  builtins: Builtins,
  global_object: Ref,
  prepare: fn(state.State) -> state.State,
  finish: fn(state.State) -> state.State,
) -> #(EvalState, Result(#(JsValue, Heap), ModuleError)) {
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
  let #(state, dep_result) =
    list.fold(requests, #(state, Ok(Nil)), fn(acc, request) {
      let #(state, prev) = acc
      let #(raw_dep, eager) = request
      case prev {
        Error(_) -> acc
        Ok(Nil) -> {
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
          list.fold(to_evaluate, #(state, Ok(Nil)), fn(acc, dep) {
            let #(state, prev) = acc
            case prev {
              Error(_) -> acc
              Ok(Nil) -> {
                let #(state, result) =
                  eval_module_inner(
                    bundle,
                    linked,
                    state,
                    dep,
                    builtins,
                    global_object,
                    prepare,
                    finish,
                  )
                case result {
                  Ok(_) -> #(state, Ok(Nil))
                  Error(err) -> #(state, Error(err))
                }
              }
            }
          })
        }
      }
    })

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
        write_module_error(state.heap, global_object, specifier, error_val)
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
        write_module_status(
          state.heap,
          global_object,
          specifier,
          status_evaluating,
        )
      case
        run_module_with_referrer(
          specifier,
          compiled.template,
          heap,
          builtins,
          global_object,
          seeds,
          prepare,
          finish,
        )
      {
        entry.ModuleError(error: vm_err) -> {
          let error_val = JsString("InternalError: " <> string.inspect(vm_err))
          let heap = clear_module_status(heap, global_object, specifier)
          let heap =
            write_module_error(heap, global_object, specifier, error_val)
          let state =
            EvalState(
              ..state,
              heap:,
              errors: dict.insert(state.errors, specifier, error_val),
            )
          #(state, Error(EvaluationError(error_val, state.heap)))
        }
        entry.ModuleThrow(value: thrown_val, heap: new_heap, jobs:) -> {
          let new_heap = clear_module_status(new_heap, global_object, specifier)
          let new_heap =
            write_module_error(new_heap, global_object, specifier, thrown_val)
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
            write_module_status(
              new_heap,
              global_object,
              specifier,
              status_evaluated,
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

/// Run a module body with the hidden referrer marker set to its resolved
/// specifier, restoring the previous marker afterwards. An ImportCall inside
/// the body captures this marker as its referencingScriptOrModule
/// (§16.2.1.8 HostLoadImportedModule), so nested dynamic imports resolve
/// relative to the importing MODULE, not the realm's entry script.
fn run_module_with_referrer(
  specifier: String,
  template: value.FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  seeds: List(#(Int, JsValue)),
  prepare: fn(state.State) -> state.State,
  finish: fn(state.State) -> state.State,
) -> entry.ModuleResult {
  let key = value.Named(dynamic_import.referrer_property)
  let previous = case object.get_own_property(heap, global_object, key) {
    Some(value.DataProperty(value: v, ..)) -> v
    _ -> JsUndefined
  }
  let heap =
    object.define_method_property(heap, global_object, key, JsString(specifier))
  let restore = fn(h) {
    object.define_method_property(h, global_object, key, previous)
  }
  case
    entry.run_module(
      template,
      heap,
      builtins,
      global_object,
      seeds,
      prepare,
      finish,
    )
  {
    entry.ModuleOk(value:, heap:, locals:, jobs:) ->
      entry.ModuleOk(value:, heap: restore(heap), locals:, jobs:)
    entry.ModuleThrow(value:, heap:, jobs:) ->
      entry.ModuleThrow(value:, heap: restore(heap), jobs:)
    entry.ModuleError(error:) -> entry.ModuleError(error:)
    entry.ModulePending(promise_data_ref:, heap:, jobs:) ->
      entry.ModulePending(promise_data_ref:, heap: restore(heap), jobs:)
  }
}

/// Allocate a GC-rooted BoxSlot holding `val`. Module binding cells are rooted
/// so they survive collection between a dependency being linked and its
/// importer's body running (they live for the duration of the module graph).
fn alloc_box(heap: Heap, val: JsValue) -> #(Heap, Ref) {
  let #(heap, ref) = heap.alloc(heap, BoxSlot(val))
  #(heap.root(heap, ref), ref)
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
  heap: Heap,
  preexisting: Dict(String, #(Ref, Dict(String, Ref))),
  preexisting_deferred: Dict(String, Ref),
) -> #(Heap, Linked, List(#(String, Ref))) {
  let #(heap, local_boxes) = preallocate_local_boxes(bundle, heap, preexisting)
  let specs = dict.keys(bundle.modules)
  // Reserve a namespace object ref per module, then a rooted box wrapping it,
  // up front so cyclic / star-reached namespace re-exports resolve to a ref.
  // Preexisting modules reuse their registered namespace object instead.
  let #(heap, ns_obj, namespace_boxes) =
    list.fold(specs, #(heap, dict.new(), dict.new()), fn(acc, spec) {
      let #(heap, objs, boxes) = acc
      case dict.get(preexisting, spec) {
        Ok(#(existing_obj, _)) -> {
          let #(heap, box) = alloc_box(heap, JsObject(existing_obj))
          #(heap, objs, dict.insert(boxes, spec, box))
        }
        Error(Nil) -> {
          let #(heap, obj) = heap.reserve(heap)
          let heap = heap.root(heap, obj)
          let #(heap, box) = alloc_box(heap, JsObject(obj))
          #(heap, dict.insert(objs, spec, obj), dict.insert(boxes, spec, box))
        }
      }
    })
  // Deferred namespaces (`import defer * as ns`): reserve a rooted proxy ref
  // per deferred-imported module. Realm-registered deferred namespaces are
  // reused for identity (§ GetModuleNamespace: one [[DeferredNamespace]] per
  // module record); fresh ones are written by the caller once the complete
  // Linked record exists (their traps capture it).
  let #(heap, deferred_boxes, deferred_to_fill) =
    list.fold(
      needed_deferred_specs(bundle),
      #(heap, dict.new(), []),
      fn(acc, spec) {
        let #(heap, boxes, fill) = acc
        case dict.get(preexisting_deferred, spec) {
          Ok(existing) -> {
            let #(heap, box) = alloc_box(heap, JsObject(existing))
            #(heap, dict.insert(boxes, spec, box), fill)
          }
          Error(Nil) -> {
            let #(heap, obj) = heap.reserve(heap)
            let heap = heap.root(heap, obj)
            let #(heap, box) = alloc_box(heap, JsObject(obj))
            #(heap, dict.insert(boxes, spec, box), [#(spec, obj), ..fill])
          }
        }
      },
    )
  // Resolve every exported name to a cell: a local binding's box (ResolvedTo)
  // or the target's namespace box (ResolvedNamespace — `export * as ns`,
  // including names reached transitively through `export *`). A preexisting
  // module's export map is already final — use it directly.
  let exports =
    list.fold(specs, dict.new(), fn(all, spec) {
      case dict.get(preexisting, spec) {
        Ok(#(_, existing_exports)) -> dict.insert(all, spec, existing_exports)
        Error(Nil) -> {
          let map =
            get_exported_names(bundle, spec, set.new())
            |> list.fold(dict.new(), fn(map, name) {
              case resolve_export(bundle, spec, name, set.new()) {
                ResolvedTo(owner, binding) ->
                  case
                    dict.get(local_boxes, owner)
                    |> result.try(dict.get(_, binding))
                  {
                    Ok(box) -> dict.insert(map, name, box)
                    Error(Nil) -> map
                  }
                ResolvedNamespace(target) ->
                  case dict.get(namespace_boxes, target) {
                    Ok(box) -> dict.insert(map, name, box)
                    Error(Nil) -> map
                  }
                ResolvedDeferredNamespace(target) ->
                  case dict.get(deferred_boxes, target) {
                    Ok(box) -> dict.insert(map, name, box)
                    Error(Nil) -> map
                  }
                Unresolvable | Ambiguous -> map
              }
            })
          dict.insert(all, spec, map)
        }
      }
    })
  // Write each reserved namespace object now that its export map is complete
  // (preexisting modules were never reserved — their objects are untouched).
  let heap =
    list.fold(specs, heap, fn(heap, spec) {
      let exp = dict.get(exports, spec) |> result.unwrap(dict.new())
      case dict.get(ns_obj, spec) {
        Ok(obj) -> heap.write(heap, obj, namespace_slot(exp))
        Error(Nil) -> heap
      }
    })
  #(
    heap,
    Linked(local_boxes:, exports:, namespace_boxes:, deferred_boxes:),
    deferred_to_fill,
  )
}

/// A module's requests in declaration order as (raw specifier, eager) pairs,
/// deduplicated by (specifier, phase) — the spec's [[RequestedModules]] list.
/// A bare/named/default/eager-namespace import declaration is an
/// ~evaluation~-phase request; a lone `import defer * as ns` declaration is a
/// ~defer~-phase request; re-exports are ~evaluation~-phase requests.
fn ordered_requests(compiled: CompiledModule) -> List(#(String, Bool)) {
  let import_requests =
    list.map(compiled.import_bindings, fn(entry) {
      let #(raw_dep, bindings) = entry
      let eager = case bindings {
        [] -> True
        _ ->
          list.any(bindings, fn(binding) {
            case binding {
              esm.NamespaceImport(phase: esm.Deferred, ..) -> False
              _ -> True
            }
          })
      }
      #(raw_dep, eager)
    })
  let reexport_requests =
    list.filter_map(compiled.export_entries, fn(entry) {
      case entry {
        esm.ReExport(source_specifier:, ..) -> Ok(#(source_specifier, True))
        esm.ReExportAll(source_specifier:) -> Ok(#(source_specifier, True))
        esm.ReExportNamespace(source_specifier:, ..) ->
          Ok(#(source_specifier, True))
        esm.LocalExport(..) -> Error(Nil)
      }
    })
  list.append(import_requests, reexport_requests)
  |> list.unique
}

/// GatherAsynchronousTransitiveDependencies ( module ): the modules in
/// `spec`'s whole dependency graph (following BOTH phases) that have
/// top-level await and are not already evaluated/evaluating — a ~defer~
/// request pre-evaluates exactly these, so a later synchronous trigger never
/// needs to run an async body. Returns (modules in discovery order, seen).
fn gather_async_transitive_deps(
  bundle: ModuleBundle,
  state: EvalState,
  global_object: Ref,
  spec: String,
  seen: Set(String),
) -> #(List(String), Set(String)) {
  use <- bool.guard(set.contains(seen, spec), #([], seen))
  let seen = set.insert(seen, spec)
  let already_done =
    set.contains(state.evaluated, spec)
    || set.contains(state.evaluating, spec)
    || read_module_status(state.heap, global_object, spec) != None
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
  heap: Heap,
  builtins: Builtins,
  global_object: Ref,
  finish: fn(state.State) -> state.State,
) -> #(Heap, List(value.Job), Result(List(#(String, Ref)), ModuleError)) {
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
  let #(eval_state, result) =
    list.fold(to_evaluate, #(eval_state, Ok([])), fn(acc, dep) {
      let #(eval_state, prev) = acc
      case prev {
        Error(_) -> acc
        Ok(pendings) -> {
          let #(eval_state, dep_result) =
            eval_module_inner(
              bundle,
              linked,
              eval_state,
              dep,
              builtins,
              global_object,
              fn(s) { s },
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
      }
    })
  let result = result.map(result, list.reverse)
  #(eval_state.heap, eval_state.jobs, result)
}

/// Record `spec`'s body as completed in the realm's heap-resident status
/// registry — what the host calls when a module parked on top-level await
/// settles its [[TopLevelCapability]] (AsyncModuleExecutionFulfilled), so a
/// later deferred-namespace trigger sees ~evaluated~ instead of a stuck
/// ~evaluating~.
pub fn record_module_evaluated(
  h: Heap,
  global_object: Ref,
  spec: String,
) -> Heap {
  write_module_status(h, global_object, spec, status_evaluated)
}

/// Record `spec`'s evaluation error in the realm's heap-resident error
/// registry (AsyncModuleExecutionRejected) — later imports and
/// deferred-namespace triggers rethrow the same error.
pub fn record_module_error(
  h: Heap,
  global_object: Ref,
  spec: String,
  err: JsValue,
) -> Heap {
  write_module_error(h, global_object, spec, err)
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
  heap: Heap,
  already_evaluated: Set(String),
) -> Heap {
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
  heap: Heap,
  preexisting: Dict(String, #(Ref, Dict(String, Ref))),
) -> #(Heap, Dict(String, Dict(String, Ref))) {
  dict.fold(bundle.modules, #(heap, dict.new()), fn(acc, spec, m) {
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
fn namespace_slot(exports: Dict(String, Ref)) -> state.HeapSlot {
  namespace_slot_tagged(exports, "Module")
}

/// `namespace_slot` with an explicit @@toStringTag — ModuleNamespaceCreate
/// uses "Deferred Module" for the ~defer~ phase namespace.
fn namespace_slot_tagged(
  exports: Dict(String, Ref),
  tag: String,
) -> state.HeapSlot {
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
  h: Heap,
  builtins: Builtins,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
  proxy_ref: Ref,
) -> Heap {
  let exports = dict.get(linked.exports, spec) |> result.unwrap(dict.new())
  let #(h, target_ref) =
    heap.alloc(h, namespace_slot_tagged(exports, "Deferred Module"))
  let h = heap.root(h, target_ref)
  let trap = fn(h, name, arity, native, always_triggers) {
    alloc_deferred_trap(
      h,
      builtins,
      name,
      arity,
      native,
      always_triggers,
      bundle,
      linked,
      spec,
    )
  }
  let #(h, get_ref) = trap(h, "get", 3, value.ReflectGet, False)
  let #(h, has_ref) = trap(h, "has", 2, value.ReflectHas, False)
  let #(h, delete_ref) =
    trap(h, "deleteProperty", 2, value.ReflectDeleteProperty, False)
  let #(h, define_ref) =
    trap(h, "defineProperty", 3, value.ReflectDefineProperty, False)
  let #(h, gopd_ref) =
    trap(
      h,
      "getOwnPropertyDescriptor",
      2,
      value.ReflectGetOwnPropertyDescriptor,
      False,
    )
  let #(h, own_keys_ref) = trap(h, "ownKeys", 1, value.ReflectOwnKeys, True)
  let #(h, handler_ref) =
    common.alloc_pojo(h, builtins.object.prototype, [
      #("get", value.data_property(JsObject(get_ref))),
      #("has", value.data_property(JsObject(has_ref))),
      #("deleteProperty", value.data_property(JsObject(delete_ref))),
      #("defineProperty", value.data_property(JsObject(define_ref))),
      #("getOwnPropertyDescriptor", value.data_property(JsObject(gopd_ref))),
      #("ownKeys", value.data_property(JsObject(own_keys_ref))),
    ])
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
  h: Heap,
  builtins: Builtins,
  name: String,
  arity: Int,
  native: value.ReflectNativeFn,
  always_triggers: Bool,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
) -> #(Heap, Ref) {
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
  state: State,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
  builtins: Builtins,
) -> Result(State, #(JsValue, State)) {
  let global_object = state.ctx.global_object
  case read_module_status(state.heap, global_object, spec) {
    Some("evaluated") -> Ok(state)
    _ ->
      case read_module_error(state.heap, global_object, spec) {
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
  h: Heap,
  global_object: Ref,
  spec: String,
  seen: Set(String),
) -> #(Bool, Set(String)) {
  use <- bool.guard(set.contains(seen, spec), #(True, seen))
  let seen = set.insert(seen, spec)
  case read_module_status(h, global_object, spec) {
    Some("evaluated") -> #(True, seen)
    Some("evaluating") -> #(False, seen)
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
  state: State,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
  builtins: Builtins,
) -> Result(State, #(JsValue, State)) {
  let global_object = state.ctx.global_object
  let specs = dict.keys(bundle.modules)
  let #(evaluated, evaluating) =
    list.fold(specs, #(set.new(), set.new()), fn(acc, s) {
      let #(done, running) = acc
      case read_module_status(state.heap, global_object, s) {
        Some("evaluated") -> #(set.insert(done, s), running)
        Some("evaluating") -> #(done, set.insert(running, s))
        _ -> acc
      }
    })
  let errors =
    list.fold(specs, dict.new(), fn(acc, s) {
      case read_module_error(state.heap, global_object, s) {
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
      fn(s) { s },
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

/// §16.2.1.6.2 GetExportedNames — local + indirect names, plus `export *` names
/// (excluding `default`), de-duplicated. `star_set` guards `export *` cycles.
fn get_exported_names(
  bundle: ModuleBundle,
  spec: String,
  star_set: Set(String),
) -> List(String) {
  case set.contains(star_set, spec), dict.get(bundle.modules, spec) {
    True, _ | _, Error(Nil) -> []
    False, Ok(m) -> {
      let star_set = set.insert(star_set, spec)
      let direct =
        list.filter_map(m.export_entries, fn(e) {
          case e {
            esm.LocalExport(export_name:, ..) -> Ok(export_name)
            esm.ReExport(export_name:, ..) -> Ok(export_name)
            esm.ReExportNamespace(export_name:, ..) -> Ok(export_name)
            esm.ReExportAll(..) -> Error(Nil)
          }
        })
      let star =
        list.flat_map(m.export_entries, fn(e) {
          case e {
            esm.ReExportAll(source_specifier:) ->
              get_exported_names(
                bundle,
                resolved_specifier(m, source_specifier),
                star_set,
              )
              |> list.filter(fn(n) { n != "default" })
            _ -> []
          }
        })
      list.append(direct, star) |> list.unique
    }
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
