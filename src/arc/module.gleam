/// ES Module system for Arc.
///
/// Implements the module lifecycle: load → link → evaluate.
/// Synchronous only (no top-level await).
///
/// Based on ECMAScript §16.2 (Source Text Module Records) and
/// QuickJS's module implementation (quickjs.c).
import arc/ast
import arc/compiler
import arc/parser
import arc/vm/array
import arc/vm/builtins/common.{type Builtins}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type FuncTemplate, type JsValue, DataProperty, JsObject, JsString, JsUndefined,
  ObjectSlot, OrdinaryObject,
}
import arc/vm/vm
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

// =============================================================================
// Module Status (ES §16.2.1.5 Source Text Module Records)
// =============================================================================

/// Module lifecycle status. Matches the ECMAScript spec exactly
/// (minus EVALUATING_ASYNC since we don't support TLA).
pub type ModuleStatus {
  /// Parsed but not yet linked.
  Unlinked
  /// Currently being processed by the linking DFS.
  Linking
  /// Successfully linked — all imports resolved.
  Linked
  /// Currently being executed by the evaluation DFS.
  Evaluating
  /// Evaluation complete (check eval_error for success/failure).
  Evaluated
}

// =============================================================================
// Module Record
// =============================================================================

/// A module record tracks the full lifecycle of a single ES module.
///
/// For builtin modules (like "arc"), the record is pre-populated with
/// status = Evaluated and exports already filled in.
///
/// For source text modules, the record progresses through:
/// Unlinked → Linking → Linked → Evaluating → Evaluated
pub type ModuleRecord {
  /// A module backed by JS source code.
  SourceModule(
    /// Normalized specifier (absolute path or builtin name).
    specifier: String,
    /// Current lifecycle status.
    status: ModuleStatus,
    /// The compiled bytecode template for the module body.
    template: FuncTemplate,
    /// The parsed AST (needed for extract_module_imports/exports).
    program: ast.Program,
    /// Module specifiers this module imports from.
    requested_modules: List(String),
    /// Import bindings extracted from the AST.
    import_bindings: List(#(String, List(compiler.ImportBinding))),
    /// Export entries extracted from the AST (export_name → local_name).
    export_entries: List(compiler.ExportEntry),
    /// Scope dict mapping local variable names → local slot indices.
    /// Computed during compilation, used for export extraction.
    scope_dict: Dict(String, Int),
    /// Maps raw import specifiers to resolved paths.
    /// Populated during resolve_dependencies.
    specifier_map: Dict(String, String),
    /// Export values (populated after evaluation).
    exports: Dict(String, JsValue),
    /// Cached evaluation error. If Some, the module threw during evaluation
    /// and will re-throw this error on any subsequent import.
    /// Per spec: a module that threw is NEVER re-evaluated.
    eval_error: Option(JsValue),
    /// DFS state for linking/evaluation (Tarjan's algorithm).
    dfs_index: Int,
    dfs_ancestor_index: Int,
  )
  /// A builtin module with pre-populated exports (e.g. "arc").
  /// Always in Evaluated status, never has errors.
  BuiltinModule(specifier: String, exports: Dict(String, JsValue))
}

/// Get the exports from any module record.
pub fn exports(record: ModuleRecord) -> Dict(String, JsValue) {
  case record {
    SourceModule(exports:, ..) -> exports
    BuiltinModule(exports:, ..) -> exports
  }
}

/// Get the evaluation error, if any.
pub fn eval_error(record: ModuleRecord) -> Option(JsValue) {
  case record {
    SourceModule(eval_error:, ..) -> eval_error
    BuiltinModule(..) -> None
  }
}

// =============================================================================
// Module Store
// =============================================================================

/// The module store maps normalized specifiers to module records.
/// Each module is loaded and cached exactly once.
pub type ModuleStore {
  ModuleStore(modules: Dict(String, ModuleRecord))
}

/// Create a new module store with builtin modules pre-registered.
pub fn new_store(h: Heap, b: Builtins) -> ModuleStore {
  // Read the Arc object's properties as module exports
  let arc_exports = case heap.read(h, b.arc) {
    Some(ObjectSlot(properties: props, ..)) ->
      dict.fold(props, dict.new(), fn(acc, name, prop) {
        case prop {
          DataProperty(value: v, ..) -> dict.insert(acc, name, v)
          _ -> acc
        }
      })
    _ -> dict.new()
  }
  let arc_module = BuiltinModule(specifier: "arc", exports: arc_exports)
  ModuleStore(modules: dict.from_list([#("arc", arc_module)]))
}

/// Look up a module by specifier.
pub fn get_module(store: ModuleStore, specifier: String) -> Option(ModuleRecord) {
  dict.get(store.modules, specifier) |> option.from_result
}

/// Insert or update a module in the store.
pub fn put_module(store: ModuleStore, record: ModuleRecord) -> ModuleStore {
  ModuleStore(modules: dict.insert(store.modules, record.specifier, record))
}

// =============================================================================
// Module Loading (Parse + Compile + Register)
// =============================================================================

pub type ModuleError {
  ParseError(String)
  CompileError(String)
  ResolutionError(String)
  LinkError(String)
  EvaluationError(JsValue)
}

/// Load a module from source code, compile it, and register it in the store.
/// Does NOT link or evaluate — just parses, compiles, and caches.
/// If already loaded, returns the existing record.
pub fn load_module(
  store: ModuleStore,
  specifier: String,
  source: String,
) -> Result(#(ModuleStore, ModuleRecord), ModuleError) {
  // Check if already loaded
  case get_module(store, specifier) {
    Some(record) -> Ok(#(store, record))
    None -> {
      // Parse as Module
      case parser.parse(source, parser.Module) {
        Error(err) ->
          Error(ParseError(
            "SyntaxError in '"
            <> specifier
            <> "': "
            <> parser.parse_error_to_string(err),
          ))
        Ok(program) -> {
          // Extract import info from AST
          let import_bindings = compiler.extract_module_imports(program)
          let import_specifiers =
            list.map(import_bindings, fn(entry) { entry.0 })

          // Extract export entries from AST
          let export_entries = compiler.extract_module_exports(program)

          // Collect all requested modules: imports + re-export sources
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

          // Compile (returns template + scope dict for export extraction)
          case compiler.compile_module(program) {
            Error(compiler.Unsupported(desc)) ->
              Error(CompileError(
                "Unsupported in '" <> specifier <> "': " <> desc,
              ))
            Error(compiler.BreakOutsideLoop) ->
              Error(CompileError("break outside loop in '" <> specifier <> "'"))
            Error(compiler.ContinueOutsideLoop) ->
              Error(CompileError(
                "continue outside loop in '" <> specifier <> "'",
              ))
            Ok(#(template, scope_dict)) -> {
              let record =
                SourceModule(
                  specifier:,
                  status: Unlinked,
                  template:,
                  program:,
                  requested_modules:,
                  import_bindings:,
                  export_entries:,
                  scope_dict:,
                  specifier_map: dict.new(),
                  exports: dict.new(),
                  eval_error: None,
                  dfs_index: -1,
                  dfs_ancestor_index: -1,
                )
              let store = put_module(store, record)
              Ok(#(store, record))
            }
          }
        }
      }
    }
  }
}

// =============================================================================
// Module Resolution
// =============================================================================

/// Resolve all dependencies of a module recursively.
/// The resolve_and_load function takes (raw_specifier, parent_specifier)
/// and returns Ok(#(resolved_path, source)) or Error(reason).
/// Builtin modules (e.g. "arc") are skipped automatically.
pub fn resolve_dependencies(
  store: ModuleStore,
  specifier: String,
  resolve_and_load: fn(String, String) -> Result(#(String, String), String),
) -> Result(ModuleStore, ModuleError) {
  resolve_dependencies_inner(store, specifier, resolve_and_load, set.new())
}

fn resolve_dependencies_inner(
  store: ModuleStore,
  specifier: String,
  resolve_and_load: fn(String, String) -> Result(#(String, String), String),
  visited: set.Set(String),
) -> Result(ModuleStore, ModuleError) {
  // Avoid infinite recursion on circular imports
  case set.contains(visited, specifier) {
    True -> Ok(store)
    False -> {
      let visited = set.insert(visited, specifier)
      case get_module(store, specifier) {
        None ->
          Error(ResolutionError("Module '" <> specifier <> "' not loaded"))
        Some(BuiltinModule(..)) -> Ok(store)
        Some(SourceModule(requested_modules:, ..)) ->
          list.try_fold(requested_modules, store, fn(store, dep_specifier) {
            case get_module(store, dep_specifier) {
              Some(BuiltinModule(..)) -> Ok(store)
              Some(_) ->
                resolve_dependencies_inner(
                  store,
                  dep_specifier,
                  resolve_and_load,
                  visited,
                )
              None -> {
                case resolve_and_load(dep_specifier, specifier) {
                  Error(err) ->
                    Error(ResolutionError(
                      "Cannot resolve module '"
                      <> dep_specifier
                      <> "' from '"
                      <> specifier
                      <> "': "
                      <> err,
                    ))
                  Ok(#(resolved_path, source)) -> {
                    let store =
                      update_specifier_map(
                        store,
                        specifier,
                        dep_specifier,
                        resolved_path,
                      )
                    use #(store, _record) <- result.try(load_module(
                      store,
                      resolved_path,
                      source,
                    ))
                    resolve_dependencies_inner(
                      store,
                      resolved_path,
                      resolve_and_load,
                      visited,
                    )
                  }
                }
              }
            }
          })
      }
    }
  }
}

/// Record a raw→resolved specifier mapping on a parent module.
fn update_specifier_map(
  store: ModuleStore,
  parent_specifier: String,
  raw_specifier: String,
  resolved_path: String,
) -> ModuleStore {
  case get_module(store, parent_specifier) {
    Some(SourceModule(..) as record) ->
      put_module(
        store,
        SourceModule(
          ..record,
          specifier_map: dict.insert(
            record.specifier_map,
            raw_specifier,
            resolved_path,
          ),
        ),
      )
    _ -> store
  }
}

/// Look up a dependency's resolved path using the parent's specifier_map.
/// Falls back to the raw specifier if no mapping exists (for builtins, etc).
fn resolve_dep_specifier(
  store: ModuleStore,
  parent_specifier: String,
  raw_specifier: String,
) -> String {
  case get_module(store, parent_specifier) {
    Some(SourceModule(specifier_map:, ..)) ->
      dict.get(specifier_map, raw_specifier) |> result.unwrap(raw_specifier)
    _ -> raw_specifier
  }
}

// =============================================================================
// Module Evaluation
// =============================================================================

/// Evaluate a module and all its dependencies in DFS post-order.
/// This is the main entry point for running a module.
///
/// Per spec:
/// - Dependencies are evaluated before the module itself
/// - If a module has already been evaluated (success or error), return cached result
/// - If a module throws, the error is permanently cached
/// - Circular dependencies see partially-initialized bindings (TDZ)
pub fn evaluate_module(
  store: ModuleStore,
  specifier: String,
  heap: Heap,
  builtins: Builtins,
  globals: Dict(String, JsValue),
) -> #(ModuleStore, Result(#(JsValue, Heap), ModuleError)) {
  let #(store, _index, result) =
    inner_evaluate(store, specifier, 0, heap, builtins, globals)
  case result {
    Ok(#(val, heap)) -> #(store, Ok(#(val, heap)))
    Error(err) -> #(store, Error(err))
  }
}

/// Inner evaluation result carries heap alongside value on success.
type EvalOk =
  #(JsValue, Heap)

/// Inner DFS evaluation. Always returns the updated store (even on error)
/// so that error caching is preserved in our immutable world.
fn inner_evaluate(
  store: ModuleStore,
  specifier: String,
  index: Int,
  heap: Heap,
  builtins: Builtins,
  globals: Dict(String, JsValue),
) -> #(ModuleStore, Int, Result(EvalOk, ModuleError)) {
  case get_module(store, specifier) {
    None -> #(
      store,
      index,
      Error(ResolutionError("Module '" <> specifier <> "' not found")),
    )

    // Builtin modules are always already evaluated
    Some(BuiltinModule(..)) -> #(store, index, Ok(#(JsUndefined, heap)))

    Some(SourceModule(status: Evaluated, eval_error: Some(err), ..)) ->
      // Cached error — re-throw, never re-evaluate
      #(store, index, Error(EvaluationError(err)))

    Some(SourceModule(status: Evaluated, eval_error: None, ..)) ->
      // Already evaluated successfully
      #(store, index, Ok(#(JsUndefined, heap)))

    Some(SourceModule(status: Evaluating, ..)) ->
      // Circular dependency — return without re-entering
      #(store, index, Ok(#(JsUndefined, heap)))

    Some(
      SourceModule(
        status: _,
        template:,
        requested_modules:,
        import_bindings:,
        ..,
      ) as record,
    ) -> {
      // Mark as Evaluating
      let record = SourceModule(..record, status: Evaluating, dfs_index: index)
      let store = put_module(store, record)
      let index = index + 1

      // Evaluate dependencies first (DFS post-order)
      // Use fold (not try_fold) so we always thread the store through.
      // Accumulator carries: store, dfs index, Result of heap
      let #(store, index, dep_result) =
        list.fold(
          requested_modules,
          #(store, index, Ok(heap)),
          fn(acc, raw_dep) {
            let #(store, index, prev_result) = acc
            case prev_result {
              Error(_) -> #(store, index, prev_result)
              Ok(heap) -> {
                // Resolve raw import specifier to its actual path
                let dep_specifier =
                  resolve_dep_specifier(store, specifier, raw_dep)
                let #(store, index, result) =
                  inner_evaluate(
                    store,
                    dep_specifier,
                    index,
                    heap,
                    builtins,
                    globals,
                  )
                case result {
                  Ok(#(_, dep_heap)) -> #(store, index, Ok(dep_heap))
                  Error(err) -> #(store, index, Error(err))
                }
              }
            }
          },
        )

      case dep_result {
        Error(err) -> {
          // Dependency failed — cache the error on this module too
          let error_val = case err {
            EvaluationError(val) -> val
            _ -> JsString(string.inspect(err))
          }
          let store = cache_eval_error(store, specifier, error_val)
          #(store, index, Error(err))
        }
        Ok(heap) -> {
          // All deps evaluated — now resolve imports and execute this module
          let #(heap, module_globals) =
            resolve_imports(store, specifier, import_bindings, heap, globals)

          case vm.run_module(template, heap, builtins, module_globals) {
            vm.ModuleError(error: vm_err) -> {
              let error_val =
                JsString("InternalError: " <> string.inspect(vm_err))
              let store = cache_eval_error(store, specifier, error_val)
              #(store, index, Error(EvaluationError(error_val)))
            }
            vm.ModuleThrow(value: thrown_val, ..) -> {
              // Module threw — cache the error permanently
              let store = cache_eval_error(store, specifier, thrown_val)
              #(store, index, Error(EvaluationError(thrown_val)))
            }
            vm.ModuleOk(value: val, heap: new_heap, locals:) -> {
              // Success — collect exports from locals, mark evaluated
              let assert Some(SourceModule(export_entries:, scope_dict:, ..)) =
                get_module(store, specifier)
              let #(module_exports, new_heap) =
                collect_exports(
                  store,
                  specifier,
                  export_entries,
                  scope_dict,
                  locals,
                  new_heap,
                )
              let store =
                mark_evaluated_with_exports(store, specifier, module_exports)
              #(store, index, Ok(#(val, new_heap)))
            }
          }
        }
      }
    }
  }
}

/// Cache an evaluation error on a module record and mark it Evaluated.
fn cache_eval_error(
  store: ModuleStore,
  specifier: String,
  error_val: JsValue,
) -> ModuleStore {
  case get_module(store, specifier) {
    Some(SourceModule(..) as record) ->
      put_module(
        store,
        SourceModule(..record, status: Evaluated, eval_error: Some(error_val)),
      )
    _ -> store
  }
}

/// Mark a module as successfully evaluated with its exports populated.
fn mark_evaluated_with_exports(
  store: ModuleStore,
  specifier: String,
  module_exports: Dict(String, JsValue),
) -> ModuleStore {
  case get_module(store, specifier) {
    Some(SourceModule(..) as record) ->
      put_module(
        store,
        SourceModule(..record, status: Evaluated, exports: module_exports),
      )
    _ -> store
  }
}

/// Collect export values from the module's locals array after evaluation.
/// Uses the export entries (from AST) and scope dict (from compilation)
/// to map export names to their runtime values.
/// Re-exports are resolved by looking up the source module's exports in the store.
fn collect_exports(
  store: ModuleStore,
  parent_specifier: String,
  export_entries: List(compiler.ExportEntry),
  scope_dict: Dict(String, Int),
  locals: array.Array(JsValue),
  heap: Heap,
) -> #(Dict(String, JsValue), Heap) {
  list.fold(export_entries, #(dict.new(), heap), fn(acc, entry) {
    let #(acc, heap) = acc
    case entry {
      compiler.LocalExport(export_name:, local_name:) ->
        case dict.get(scope_dict, local_name) {
          Ok(index) ->
            case array.get(index, locals) {
              Some(val) -> #(dict.insert(acc, export_name, val), heap)
              None -> #(acc, heap)
            }
          Error(Nil) -> #(acc, heap)
        }
      compiler.ReExport(export_name:, imported_name:, source_specifier:) -> {
        let resolved =
          resolve_dep_specifier(store, parent_specifier, source_specifier)
        case get_module(store, resolved) {
          Some(record) ->
            case dict.get(record |> exports, imported_name) {
              Ok(val) -> #(dict.insert(acc, export_name, val), heap)
              Error(Nil) -> #(acc, heap)
            }
          None -> #(acc, heap)
        }
      }
      compiler.ReExportAll(source_specifier:) -> {
        let resolved =
          resolve_dep_specifier(store, parent_specifier, source_specifier)
        case get_module(store, resolved) {
          Some(record) ->
            // Re-export all except "default" (per spec §16.2.1.12.1)
            dict.fold(record |> exports, acc, fn(acc, name, val) {
              case name {
                "default" -> acc
                _ -> dict.insert(acc, name, val)
              }
            })
            |> fn(acc) { #(acc, heap) }
          None -> #(acc, heap)
        }
      }
      compiler.ReExportNamespace(export_name:, source_specifier:) -> {
        let resolved =
          resolve_dep_specifier(store, parent_specifier, source_specifier)
        case get_module(store, resolved) {
          Some(record) -> {
            let dep_exports = record |> exports
            let properties =
              dict.fold(dep_exports, dict.new(), fn(props, name, val) {
                dict.insert(props, name, value.builtin_property(val))
              })
            let #(heap, ref) =
              heap.alloc(
                heap,
                ObjectSlot(
                  kind: OrdinaryObject,
                  properties:,
                  elements: js_elements.new(),
                  prototype: None,
                  symbol_properties: dict.new(),
                  extensible: False,
                ),
              )
            #(dict.insert(acc, export_name, JsObject(ref)), heap)
          }
          None -> #(acc, heap)
        }
      }
    }
  })
}

/// Resolve import bindings for a module, looking up exports from the store.
/// Uses the parent module's specifier_map to resolve raw import specifiers
/// to their actual module paths.
fn resolve_imports(
  store: ModuleStore,
  parent_specifier: String,
  import_bindings: List(#(String, List(compiler.ImportBinding))),
  heap: Heap,
  base_globals: Dict(String, JsValue),
) -> #(Heap, Dict(String, JsValue)) {
  list.fold(import_bindings, #(heap, base_globals), fn(acc, entry) {
    let #(heap, globals) = acc
    let #(raw_dep, bindings) = entry
    let dep_specifier = resolve_dep_specifier(store, parent_specifier, raw_dep)
    case get_module(store, dep_specifier) {
      None -> #(heap, globals)
      Some(record) -> {
        let dep_exports = exports(record)
        list.fold(bindings, #(heap, globals), fn(acc, binding) {
          let #(heap, globals) = acc
          case binding {
            compiler.NamedImport(imported:, local:) -> {
              let val =
                dict.get(dep_exports, imported) |> result.unwrap(JsUndefined)
              #(heap, dict.insert(globals, local, val))
            }
            compiler.DefaultImport(local:) -> {
              let val =
                dict.get(dep_exports, "default") |> result.unwrap(JsUndefined)
              #(heap, dict.insert(globals, local, val))
            }
            compiler.NamespaceImport(local:) -> {
              let properties =
                dict.fold(dep_exports, dict.new(), fn(props, name, val) {
                  dict.insert(props, name, value.builtin_property(val))
                })
              // Per spec §10.4.6, Module Namespace Exotic Objects
              // have a null prototype and are not extensible.
              let #(heap, ref) =
                heap.alloc(
                  heap,
                  ObjectSlot(
                    kind: OrdinaryObject,
                    properties:,
                    elements: js_elements.new(),
                    prototype: None,
                    symbol_properties: dict.new(),
                    extensible: False,
                  ),
                )
              #(heap, dict.insert(globals, local, JsObject(ref)))
            }
          }
        })
      }
    }
  })
}
