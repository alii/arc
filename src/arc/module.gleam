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
import arc/internal/erlang
import arc/parser
import arc/vm/builtins/common.{type Builtins}
import arc/vm/exec/entry
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/tuple_array
import arc/vm/state.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, BoxSlot, JsObject, JsString, JsUndefined, ObjectSlot,
}
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
    import_bindings: List(#(String, List(compiler.ImportBinding))),
    export_entries: List(compiler.ExportEntry),
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
/// The resolve_and_load callback provides source code for dependencies:
///   fn(raw_specifier, parent_specifier) -> Result(#(resolved_path, source), error)
pub fn compile_bundle(
  entry_specifier: String,
  entry_source: String,
  resolve_and_load: fn(String, String) -> Result(#(String, String), String),
) -> Result(ModuleBundle, ModuleError) {
  use entry_compiled <- result.try(compile_single(entry_specifier, entry_source))
  let modules = dict.from_list([#(entry_specifier, entry_compiled)])
  use modules <- result.map(resolve_and_compile_deps(
    entry_specifier,
    entry_compiled.requested_modules,
    resolve_and_load,
    modules,
  ))
  ModuleBundle(entry: entry_specifier, modules:)
}

/// Parse and compile a single module from source.
fn compile_single(
  specifier: String,
  source: String,
) -> Result(CompiledModule, ModuleError) {
  use program <- result.try(
    parser.parse(source, parser.Module)
    |> result.map_error(fn(err) {
      ParseError(
        "SyntaxError in '"
        <> specifier
        <> "': "
        <> parser.parse_error_to_string(err),
      )
    }),
  )

  let import_bindings = compiler.extract_module_imports(program)
  let import_specifiers = list.map(import_bindings, fn(entry) { entry.0 })
  let export_entries = compiler.extract_module_exports(program)

  let reexport_specifiers =
    list.filter_map(export_entries, fn(entry) {
      case entry {
        compiler.ReExport(source_specifier:, ..) -> Ok(source_specifier)
        compiler.ReExportAll(source_specifier:) -> Ok(source_specifier)
        compiler.ReExportNamespace(source_specifier:, ..) ->
          Ok(source_specifier)
        compiler.LocalExport(..) -> Error(Nil)
      }
    })
  let requested_modules =
    list.unique(list.append(import_specifiers, reexport_specifiers))

  use #(template, scope_dict, hoisted_funcs) <- result.map(
    compiler.compile_module(program)
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

  CompiledModule(
    specifier:,
    template:,
    import_bindings:,
    export_entries:,
    scope_dict:,
    specifier_map: dict.new(),
    requested_modules:,
    export_seeds: compiler.module_export_seeds(program),
    hoisted_funcs:,
  )
}

/// Recursively resolve and compile all dependencies of a parent module.
fn resolve_and_compile_deps(
  parent_specifier: String,
  requested_modules: List(String),
  resolve_and_load: fn(String, String) -> Result(#(String, String), String),
  modules: Dict(String, CompiledModule),
) -> Result(Dict(String, CompiledModule), ModuleError) {
  list.try_fold(requested_modules, modules, fn(modules, raw_dep) {
    case resolve_and_load(raw_dep, parent_specifier) {
      Error(err) ->
        Error(ResolutionError(
          "Cannot resolve module '"
          <> raw_dep
          <> "' from '"
          <> parent_specifier
          <> "': "
          <> err,
        ))
      Ok(#(resolved_path, source)) -> {
        // Record raw→resolved mapping in the parent module
        let modules =
          update_compiled_specifier_map(
            modules,
            parent_specifier,
            raw_dep,
            resolved_path,
          )
        // Skip if already compiled (handles cycles + shared deps)
        case dict.has_key(modules, resolved_path) {
          True -> Ok(modules)
          False -> {
            use dep_compiled <- result.try(compile_single(resolved_path, source))
            let modules = dict.insert(modules, resolved_path, dep_compiled)
            resolve_and_compile_deps(
              resolved_path,
              dep_compiled.requested_modules,
              resolve_and_load,
              modules,
            )
          }
        }
      }
    }
  })
}

/// Update a compiled module's specifier_map in the modules dict.
fn update_compiled_specifier_map(
  modules: Dict(String, CompiledModule),
  parent: String,
  raw: String,
  resolved: String,
) -> Dict(String, CompiledModule) {
  case dict.get(modules, parent) {
    Ok(m) ->
      dict.insert(
        modules,
        parent,
        CompiledModule(
          ..m,
          specifier_map: dict.insert(m.specifier_map, raw, resolved),
        ),
      )
    Error(Nil) -> modules
  }
}

// =============================================================================
// Linking — ResolveExport (§16.2.1.6.3) + import/re-export checks (§16.2.1.6.4)
// =============================================================================

/// Result of resolving an export name through a module graph.
type ExportResolution {
  /// Resolves to a concrete binding `binding` owned by module `module`.
  ResolvedTo(module: String, binding: String)
  /// Resolves to a module namespace object (`export * as ns from`).
  ResolvedNamespace(module: String)
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
        compiler.LocalExport(export_name:, local_name:)
          if export_name == name
        -> Ok(ResolvedTo(specifier, local_name))
        compiler.ReExport(export_name:, imported_name:, source_specifier:)
          if export_name == name
        -> {
          let src = resolved_specifier(m, source_specifier)
          Ok(resolve_export(bundle, src, imported_name, resolve_set))
        }
        compiler.ReExportNamespace(export_name:, source_specifier:)
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
        compiler.ReExportAll(source_specifier:) ->
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
        compiler.NamespaceImport(..) -> Ok(Nil)
        compiler.NamedImport(imported:, ..) ->
          check_resolves(bundle, dep, imported, raw_dep)
        compiler.DefaultImport(..) ->
          check_resolves(bundle, dep, "default", raw_dep)
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
      compiler.ReExport(export_name:, source_specifier:, ..) ->
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
    ResolvedTo(..) | ResolvedNamespace(..) -> Ok(Nil)
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
// Runtime Evaluation (evaluate_bundle)
// =============================================================================

/// The result of linking: every module's binding cells, pre-allocated before
/// any body runs (§16.2 instantiation) so cyclic/self imports reference the
/// same live cells. Immutable once built; threaded read-only through the DFS.
type Linked {
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
  )
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
  // Link phase (§16.2.1.6.4): resolve every import and indirect re-export
  // across the whole graph BEFORE evaluating any body. Missing or ambiguous
  // exports are a SyntaxError raised at link time, not a runtime ReferenceError.
  case link_bundle(bundle) {
    Error(message) -> {
      let #(heap, err) = common.make_syntax_error(heap, builtins, message)
      Error(EvaluationError(err, heap))
    }
    Ok(Nil) -> {
      // Instantiate: pre-allocate every binding cell + namespace object, then
      // create exported function-declaration closures so cyclic function
      // imports are callable before any body runs (§16.2.1.6.4 step 9).
      let #(heap, linked) = build_linked(bundle, heap)
      let heap = instantiate_hoisted_functions(bundle, linked, builtins, heap)
      let state =
        EvalState(
          heap:,
          evaluated: set.new(),
          errors: dict.new(),
          evaluating: set.new(),
        )
      let #(_state, result) =
        eval_module_inner(
          bundle,
          linked,
          state,
          bundle.entry,
          builtins,
          global_object,
          finish,
        )
      // Surface the entry namespace alongside the completion value (post-eval,
      // so its bindings are initialized — no TDZ for the embedder to hit).
      use #(completion_value, final_heap) <- result.map(result)
      EvaluatedBundle(
        value: completion_value,
        heap: final_heap,
        namespace: entry_namespace(linked, bundle.entry, final_heap),
      )
    }
  }
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
  finish: fn(state.State) -> state.State,
) -> #(EvalState, Result(#(JsValue, Heap), ModuleError)) {
  // Already evaluated successfully
  case set.contains(state.evaluated, specifier) {
    True -> #(state, Ok(#(JsUndefined, state.heap)))
    False ->
      // Cached error — re-throw, never re-evaluate
      case dict.get(state.errors, specifier) {
        Ok(err_val) -> #(state, Error(EvaluationError(err_val, state.heap)))
        Error(Nil) ->
          // Circular dependency — return without re-entering
          case set.contains(state.evaluating, specifier) {
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
  finish: fn(state.State) -> state.State,
) -> #(EvalState, Result(#(JsValue, Heap), ModuleError)) {
  // Mark as evaluating
  let state =
    EvalState(..state, evaluating: set.insert(state.evaluating, specifier))

  // Evaluate dependencies first (DFS post-order)
  let #(state, dep_result) =
    list.fold(compiled.requested_modules, #(state, Ok(Nil)), fn(acc, raw_dep) {
      let #(state, prev) = acc
      case prev {
        Error(_) -> acc
        Ok(Nil) -> {
          let dep_specifier =
            dict.get(compiled.specifier_map, raw_dep)
            |> result.unwrap(raw_dep)
          let #(state, result) =
            eval_module_inner(
              bundle,
              linked,
              state,
              dep_specifier,
              builtins,
              global_object,
              finish,
            )
          case result {
            Ok(_) -> #(state, Ok(Nil))
            Error(err) -> #(state, Error(err))
          }
        }
      }
    })

  case dep_result {
    Error(err) -> {
      // Dependency failed — cache the error on this module too
      let error_val = case err {
        EvaluationError(value: val, ..) -> val
        _ -> JsString(string.inspect(err))
      }
      let state =
        EvalState(
          ..state,
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
        |> list.append(own_export_seeds(linked, specifier, compiled.scope_dict))

      case
        entry.run_module(
          compiled.template,
          state.heap,
          builtins,
          global_object,
          seeds,
          finish,
        )
      {
        entry.ModuleError(error: vm_err) -> {
          let error_val = JsString("InternalError: " <> string.inspect(vm_err))
          let state =
            EvalState(
              ..state,
              errors: dict.insert(state.errors, specifier, error_val),
            )
          #(state, Error(EvaluationError(error_val, state.heap)))
        }
        entry.ModuleThrow(value: thrown_val, heap: new_heap) -> {
          let state =
            EvalState(
              ..state,
              heap: new_heap,
              errors: dict.insert(state.errors, specifier, thrown_val),
            )
          #(state, Error(EvaluationError(thrown_val, new_heap)))
        }
        entry.ModuleOk(value: val, heap: new_heap, locals: _) -> {
          let state =
            EvalState(
              ..state,
              heap: new_heap,
              evaluated: set.insert(state.evaluated, specifier),
              evaluating: set.delete(state.evaluating, specifier),
            )
          #(state, Ok(#(val, new_heap)))
        }
      }
    }
  }
}

// =============================================================================
// Serialization
// =============================================================================

/// Serialize a ModuleBundle to a binary (Erlang term_to_binary).
pub fn serialize_bundle(bundle: ModuleBundle) -> BitArray {
  erlang.term_to_binary(bundle)
}

/// Deserialize a ModuleBundle from a binary (Erlang binary_to_term).
pub fn deserialize_bundle(data: BitArray) -> ModuleBundle {
  erlang.binary_to_term(data)
}

// =============================================================================
// Helper Functions
// =============================================================================

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
fn build_linked(bundle: ModuleBundle, heap: Heap) -> #(Heap, Linked) {
  let #(heap, local_boxes) = preallocate_local_boxes(bundle, heap)
  let specs = dict.keys(bundle.modules)
  // Reserve a namespace object ref per module, then a rooted box wrapping it,
  // up front so cyclic / star-reached namespace re-exports resolve to a ref.
  let #(heap, ns_obj, namespace_boxes) =
    list.fold(specs, #(heap, dict.new(), dict.new()), fn(acc, spec) {
      let #(heap, objs, boxes) = acc
      let #(heap, obj) = heap.reserve(heap)
      let heap = heap.root(heap, obj)
      let #(heap, box) = alloc_box(heap, JsObject(obj))
      #(heap, dict.insert(objs, spec, obj), dict.insert(boxes, spec, box))
    })
  // Resolve every exported name to a cell: a local binding's box (ResolvedTo)
  // or the target's namespace box (ResolvedNamespace — `export * as ns`,
  // including names reached transitively through `export *`).
  let exports =
    list.fold(specs, dict.new(), fn(all, spec) {
      let map =
        get_exported_names(bundle, spec, set.new())
        |> list.fold(dict.new(), fn(map, name) {
          case resolve_export(bundle, spec, name, set.new()) {
            ResolvedTo(owner, binding) ->
              case
                dict.get(local_boxes, owner) |> result.try(dict.get(_, binding))
              {
                Ok(box) -> dict.insert(map, name, box)
                Error(Nil) -> map
              }
            ResolvedNamespace(target) ->
              case dict.get(namespace_boxes, target) {
                Ok(box) -> dict.insert(map, name, box)
                Error(Nil) -> map
              }
            _ -> map
          }
        })
      dict.insert(all, spec, map)
    })
  // Write each reserved namespace object now that its export map is complete.
  let heap =
    list.fold(specs, heap, fn(heap, spec) {
      let exp = dict.get(exports, spec) |> result.unwrap(dict.new())
      case dict.get(ns_obj, spec) {
        Ok(obj) -> heap.write(heap, obj, namespace_slot(exp))
        Error(Nil) -> heap
      }
    })
  #(heap, Linked(local_boxes:, exports:, namespace_boxes:))
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
) -> Heap {
  dict.fold(bundle.modules, heap, fn(heap, spec, compiled) {
    let local_boxes =
      dict.get(linked.local_boxes, spec) |> result.unwrap(dict.new())
    // Reconstruct the module's seeded frame so closures capture the same cells
    // a body run would (imports in slots 0..N-1, own exports in their slots).
    let seeds =
      import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
      |> list.append(own_export_seeds(linked, spec, compiled.scope_dict))
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
fn preallocate_local_boxes(
  bundle: ModuleBundle,
  heap: Heap,
) -> #(Heap, Dict(String, Dict(String, Ref))) {
  dict.fold(bundle.modules, #(heap, dict.new()), fn(acc, spec, m) {
    let #(heap, all) = acc
    let #(heap, boxes) =
      dict.fold(m.export_seeds, #(heap, dict.new()), fn(a, local, seed) {
        let #(heap, boxes) = a
        let #(heap, box) = alloc_box(heap, seed)
        #(heap, dict.insert(boxes, local, box))
      })
    #(heap, dict.insert(all, spec, boxes))
  })
}

/// §10.4.6 Module Namespace Exotic Object slot: a ModuleNamespace kind holding
/// the export name → BoxSlot-ref map (so [[Get]] re-reads the live cell and
/// throws on TDZ), null prototype, non-extensible, with @@toStringTag = "Module"
/// (§28.3.1, the all-false attributes of value.data).
fn namespace_slot(exports: Dict(String, Ref)) -> state.HeapSlot {
  ObjectSlot(
    kind: value.ModuleNamespace(exports:),
    properties: dict.new(),
    elements: elements.new(),
    prototype: None,
    symbol_properties: [
      #(value.symbol_to_string_tag, value.data(JsString("Module"))),
    ],
    extensible: False,
  )
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
            compiler.LocalExport(export_name:, ..) -> Ok(export_name)
            compiler.ReExport(export_name:, ..) -> Ok(export_name)
            compiler.ReExportNamespace(export_name:, ..) -> Ok(export_name)
            compiler.ReExportAll(..) -> Error(Nil)
          }
        })
      let star =
        list.flat_map(m.export_entries, fn(e) {
          case e {
            compiler.ReExportAll(source_specifier:) ->
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
  import_bindings: List(#(String, List(compiler.ImportBinding))),
) -> List(#(Int, JsValue)) {
  list.flat_map(import_bindings, fn(entry) {
    let #(raw_dep, bindings) = entry
    let dep = dict.get(specifier_map, raw_dep) |> result.unwrap(raw_dep)
    let dep_exports = dict.get(linked.exports, dep) |> result.unwrap(dict.new())
    list.map(bindings, fn(binding) {
      case binding {
        compiler.NamedImport(imported:, ..) ->
          forward_box(dep_exports, imported)
        compiler.DefaultImport(..) -> forward_box(dep_exports, "default")
        compiler.NamespaceImport(..) ->
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
fn own_export_seeds(
  linked: Linked,
  specifier: String,
  scope_dict: Dict(String, Int),
) -> List(#(Int, JsValue)) {
  dict.get(linked.local_boxes, specifier)
  |> result.unwrap(dict.new())
  |> dict.to_list
  |> list.filter_map(fn(pair) {
    let #(local_name, box) = pair
    case dict.get(scope_dict, local_name) {
      Ok(index) -> Ok(#(index, JsObject(box)))
      Error(Nil) -> Error(Nil)
    }
  })
}
