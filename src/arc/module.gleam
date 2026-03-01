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
import arc/erlang
import arc/parser
import arc/vm/array
import arc/vm/builtins/common.{type Builtins}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, DataProperty, JsObject, JsString, JsUndefined, ObjectSlot,
  OrdinaryObject,
}
import arc/vm/vm
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
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
  EvaluationError(JsValue)
}

// =============================================================================
// AOT Compilation (compile_bundle)
// =============================================================================

/// Compile a module and all its dependencies into a self-contained ModuleBundle.
/// The resolve_and_load callback provides source code for dependencies:
///   fn(raw_specifier, parent_specifier) -> Result(#(resolved_path, source), error)
/// Builtin modules ("arc") are referenced but not included in the bundle.
pub fn compile_bundle(
  entry_specifier: String,
  entry_source: String,
  resolve_and_load: fn(String, String) -> Result(#(String, String), String),
) -> Result(ModuleBundle, ModuleError) {
  use entry_compiled <- result.try(compile_single(entry_specifier, entry_source))
  let modules = dict.from_list([#(entry_specifier, entry_compiled)])
  let visited = set.from_list([entry_specifier])
  use modules <- result.map(resolve_and_compile_deps(
    entry_specifier,
    entry_compiled.requested_modules,
    resolve_and_load,
    modules,
    visited,
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

  use #(template, scope_dict) <- result.map(
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
  )
}

/// Recursively resolve and compile all dependencies of a parent module.
fn resolve_and_compile_deps(
  parent_specifier: String,
  requested_modules: List(String),
  resolve_and_load: fn(String, String) -> Result(#(String, String), String),
  modules: Dict(String, CompiledModule),
  visited: Set(String),
) -> Result(Dict(String, CompiledModule), ModuleError) {
  list.try_fold(requested_modules, modules, fn(modules, raw_dep) {
    // Skip builtin modules — they're not in the bundle
    case raw_dep == "arc" {
      True -> Ok(modules)
      False ->
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
            case set.contains(visited, resolved_path) {
              True -> Ok(modules)
              False -> {
                use dep_compiled <- result.try(compile_single(
                  resolved_path,
                  source,
                ))
                let modules = dict.insert(modules, resolved_path, dep_compiled)
                let visited = set.insert(visited, resolved_path)
                resolve_and_compile_deps(
                  resolved_path,
                  dep_compiled.requested_modules,
                  resolve_and_load,
                  modules,
                  visited,
                )
              }
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
// Runtime Evaluation (evaluate_bundle)
// =============================================================================

/// Internal evaluation state threaded through the DFS.
type EvalState {
  EvalState(
    heap: Heap,
    /// Specifier → exports dict for successfully evaluated modules.
    evaluated: Dict(String, Dict(String, JsValue)),
    /// Specifier → cached error for modules that threw during evaluation.
    errors: Dict(String, JsValue),
    /// Currently evaluating (cycle detection).
    evaluating: Set(String),
  )
}

/// Evaluate a compiled module bundle. Executes all modules in DFS post-order
/// (dependencies first). Returns the entry module's completion value.
pub fn evaluate_bundle(
  bundle: ModuleBundle,
  heap: Heap,
  builtins: Builtins,
  globals: Dict(String, JsValue),
) -> Result(#(JsValue, Heap), ModuleError) {
  let builtin_exports = extract_builtin_exports(heap, builtins)
  let state =
    EvalState(
      heap:,
      evaluated: dict.from_list([#("arc", builtin_exports)]),
      errors: dict.new(),
      evaluating: set.new(),
    )
  let #(_state, result) =
    eval_module_inner(bundle, state, bundle.entry, builtins, globals)
  result
}

/// DFS post-order evaluation of a single module and its dependencies.
fn eval_module_inner(
  bundle: ModuleBundle,
  state: EvalState,
  specifier: String,
  builtins: Builtins,
  globals: Dict(String, JsValue),
) -> #(EvalState, Result(#(JsValue, Heap), ModuleError)) {
  // Already evaluated successfully
  case dict.has_key(state.evaluated, specifier) {
    True -> #(state, Ok(#(JsUndefined, state.heap)))
    False ->
      // Cached error — re-throw, never re-evaluate
      case dict.get(state.errors, specifier) {
        Ok(err_val) -> #(state, Error(EvaluationError(err_val)))
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
                    state,
                    specifier,
                    compiled,
                    builtins,
                    globals,
                  )
              }
          }
      }
  }
}

/// Evaluate a module's dependencies and then its body.
fn eval_module_body(
  bundle: ModuleBundle,
  state: EvalState,
  specifier: String,
  compiled: CompiledModule,
  builtins: Builtins,
  globals: Dict(String, JsValue),
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
            eval_module_inner(bundle, state, dep_specifier, builtins, globals)
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
        EvaluationError(val) -> val
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
      // All deps evaluated — resolve imports and execute this module
      let #(heap, module_globals) =
        resolve_imports(
          state.evaluated,
          compiled.specifier_map,
          compiled.import_bindings,
          state.heap,
          globals,
        )

      case vm.run_module(compiled.template, heap, builtins, module_globals) {
        vm.ModuleError(error: vm_err) -> {
          let error_val = JsString("InternalError: " <> string.inspect(vm_err))
          let state =
            EvalState(
              ..state,
              errors: dict.insert(state.errors, specifier, error_val),
            )
          #(state, Error(EvaluationError(error_val)))
        }
        vm.ModuleThrow(value: thrown_val, ..) -> {
          let state =
            EvalState(
              ..state,
              errors: dict.insert(state.errors, specifier, thrown_val),
            )
          #(state, Error(EvaluationError(thrown_val)))
        }
        vm.ModuleOk(value: val, heap: new_heap, locals:) -> {
          let #(module_exports, new_heap) =
            collect_exports(
              state.evaluated,
              compiled.specifier_map,
              compiled.export_entries,
              compiled.scope_dict,
              locals,
              new_heap,
            )
          let state =
            EvalState(
              ..state,
              heap: new_heap,
              evaluated: dict.insert(state.evaluated, specifier, module_exports),
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

/// Extract builtin module exports from the heap (for the "arc" module).
fn extract_builtin_exports(h: Heap, b: Builtins) -> Dict(String, JsValue) {
  case heap.read(h, b.arc) {
    Some(ObjectSlot(properties: props, ..)) ->
      dict.fold(props, dict.new(), fn(acc, name, prop) {
        case prop {
          DataProperty(value: v, ..) -> dict.insert(acc, name, v)
          _ -> acc
        }
      })
    _ -> dict.new()
  }
}

/// Resolve import bindings for a module, looking up exports from
/// already-evaluated modules.
fn resolve_imports(
  evaluated: Dict(String, Dict(String, JsValue)),
  specifier_map: Dict(String, String),
  import_bindings: List(#(String, List(compiler.ImportBinding))),
  heap: Heap,
  base_globals: Dict(String, JsValue),
) -> #(Heap, Dict(String, JsValue)) {
  list.fold(import_bindings, #(heap, base_globals), fn(acc, entry) {
    let #(heap, globals) = acc
    let #(raw_dep, bindings) = entry
    let dep_specifier =
      dict.get(specifier_map, raw_dep) |> result.unwrap(raw_dep)
    case dict.get(evaluated, dep_specifier) {
      Error(Nil) -> #(heap, globals)
      Ok(dep_exports) ->
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
  })
}

/// Collect export values from a module's locals array after evaluation.
/// Uses the export entries (from AST) and scope dict (from compilation)
/// to map export names to their runtime values.
/// Re-exports are resolved by looking up already-evaluated module exports.
fn collect_exports(
  evaluated: Dict(String, Dict(String, JsValue)),
  specifier_map: Dict(String, String),
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
          dict.get(specifier_map, source_specifier)
          |> result.unwrap(source_specifier)
        case dict.get(evaluated, resolved) {
          Ok(dep_exports) ->
            case dict.get(dep_exports, imported_name) {
              Ok(val) -> #(dict.insert(acc, export_name, val), heap)
              Error(Nil) -> #(acc, heap)
            }
          Error(Nil) -> #(acc, heap)
        }
      }
      compiler.ReExportAll(source_specifier:) -> {
        let resolved =
          dict.get(specifier_map, source_specifier)
          |> result.unwrap(source_specifier)
        case dict.get(evaluated, resolved) {
          Ok(dep_exports) ->
            // Re-export all except "default" (per spec §16.2.1.12.1)
            dict.fold(dep_exports, acc, fn(acc, name, val) {
              case name {
                "default" -> acc
                _ -> dict.insert(acc, name, val)
              }
            })
            |> fn(acc) { #(acc, heap) }
          Error(Nil) -> #(acc, heap)
        }
      }
      compiler.ReExportNamespace(export_name:, source_specifier:) -> {
        let resolved =
          dict.get(specifier_map, source_specifier)
          |> result.unwrap(source_specifier)
        case dict.get(evaluated, resolved) {
          Ok(dep_exports) -> {
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
          Error(Nil) -> #(acc, heap)
        }
      }
    }
  })
}
