//// The embed-Arc library facade.
////
//// Hides the heap/builtins/globals bootstrapping dance behind a small API and
//// is the single front door for embedders: stand up an engine, compose host
//// functions onto it, then run JS — as a script (`eval`), as an ES module
//// (`eval_module`), or by invoking a value you already hold (`call`).
////
//// `Engine` is the named `heap + builtins + global` triple that every lower
//// layer (`entry`, `module`, the binding helpers) threads through. Callers who
//// just want to run some JS should go through here instead of wiring up heap +
//// builtins + entry directly.

import arc/compiler
import arc/internal/erlang
import arc/module
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{type Completion}
import arc/vm/exec/entry
import arc/vm/exec/event_loop
import arc/vm/heap
import arc/vm/ops/object
import arc/vm/state.{type Heap, type HostFn}
import arc/vm/value.{type JsValue, type Ref, JsObject, Named}
import gleam/option.{type Option}
import gleam/result
import gleam/string

// ----------------------------------------------------------------------------
// Engine type
// ----------------------------------------------------------------------------

/// An initialized JS engine — heap, builtins, global object.
///
/// Opaque so callers can't reach inside and mutate pieces independently;
/// advance an engine via `eval`/`eval_module`/`call`, which thread the heap
/// forward and hand back a new `Engine`. Read-only access to the parts is via
/// the `heap`/`builtins`/`global` accessors.
pub opaque type Engine {
  Engine(heap: Heap, builtins: Builtins, global: Ref)
}

/// Errors from the parse → compile → run pipeline, across both the script
/// (`eval`) and module (`eval_module`) paths. `call` only ever surfaces
/// `VmError`.
pub type EvalError {
  ParseError(parser.ParseError)
  CompileError(compiler.CompileError)
  VmError(state.VmError)
  ModuleError(module.ModuleError)
}

/// The result of evaluating an ES module: the entry module's completion value
/// plus its Module Namespace object (§10.4.6), if any. Read named exports off
/// `namespace` with `read_export`. `namespace` is `None` only for a degenerate
/// bundle that produced no entry namespace.
pub type EvaluatedModule {
  EvaluatedModule(value: JsValue, namespace: Option(JsValue))
}

// ----------------------------------------------------------------------------
// Constructors
// ----------------------------------------------------------------------------

/// Create a fresh engine with a new heap and all builtins installed. The single
/// bootstrap site — every other entry point threads an existing engine.
pub fn new() -> Engine {
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, global) = builtins.globals(b, h)
  Engine(heap: h, builtins: b, global:)
}

// ----------------------------------------------------------------------------
// Host FFI — extend the engine with embedder-provided globals
// ----------------------------------------------------------------------------

/// Add a top-level global native function.
///
/// The function becomes callable from JS as `name(...)`. `arity` is the
/// reported `.length` property; the impl still receives all passed args.
pub fn define_fn(
  engine: Engine,
  name: String,
  arity: Int,
  impl: HostFn,
) -> Engine {
  let #(h, fn_ref) =
    common.alloc_host_fn(
      engine.heap,
      engine.builtins.function.prototype,
      impl,
      name,
      arity,
    )
  set_global(engine, h, name, JsObject(fn_ref))
}

/// Add a top-level namespace object (like `Math` or `JSON`) with methods.
///
/// Creates a tagged object at the given global name whose own properties are
/// the supplied methods. Each method spec is `#(name, arity, impl)`. Routed
/// through `common.init_namespace` so the namespace carries `@@toStringTag`
/// (`Object.prototype.toString.call(ns)` → `"[object name]"`), matching every
/// built-in namespace.
pub fn define_namespace(
  engine: Engine,
  name: String,
  methods: List(#(String, Int, HostFn)),
) -> Engine {
  let #(h, props) =
    common.alloc_host_methods(
      engine.heap,
      engine.builtins.function.prototype,
      methods,
    )
  let #(h, ns_ref) =
    common.init_namespace(h, engine.builtins.object.prototype, name, props)
  set_global(engine, h, name, JsObject(ns_ref))
}

/// Add a raw JsValue as a top-level global binding.
///
/// For constants or pre-built objects that don't fit `define_fn` or
/// `define_namespace`. The value is installed as a writable, configurable,
/// non-enumerable data property on `globalThis`.
pub fn define_global(engine: Engine, name: String, val: JsValue) -> Engine {
  set_global(engine, engine.heap, name, val)
}

/// Install `val` as a builtin property `name` on the engine's global object,
/// returning the engine with the updated heap. The one place global bindings
/// are written — reuses the canonical `object.define_method_property` primitive.
fn set_global(engine: Engine, h: Heap, name: String, val: JsValue) -> Engine {
  Engine(
    ..engine,
    heap: object.define_method_property(h, engine.global, Named(name), val),
  )
}

// ----------------------------------------------------------------------------
// Script evaluation
// ----------------------------------------------------------------------------

/// Parse, compile, and run a JS source string. Returns the completion
/// (normal return value or uncaught exception) plus a new engine carrying
/// the updated heap.
///
/// Drains the microtask queue only — there is no macrotask loop in core.
/// If your host functions use `host.suspend`, drive your own loop via
/// `eval_with` (e.g. `eval_with(eng, src, beam.run)` for the Erlang
/// mailbox loop).
pub fn eval(
  engine: Engine,
  source: String,
) -> Result(#(Completion, Engine), EvalError) {
  eval_with(engine, source, event_loop.finish)
}

/// Like `eval` but the caller supplies the post-script driver. `finish`
/// is handed the State after the top-level script returns and must drain
/// microtasks plus whatever macrotask loop the embedder owns.
pub fn eval_with(
  engine: Engine,
  source: String,
  finish: fn(state.State) -> state.State,
) -> Result(#(Completion, Engine), EvalError) {
  eval_prepared_with(engine, source, fn(s) { s }, finish)
}

/// Like `eval_with` but with an embedder `prepare` hook applied to the
/// freshly booted State BEFORE the top-level script executes. Embedders
/// whose `finish` driver installs host capabilities (e.g. `beam.run`'s
/// Atomics blocking-wait/wake-delivery) must install them here too —
/// `finish` only takes over after the script returns, and a top-level
/// blocking `Atomics.wait` needs them mid-script:
///
///     engine.eval_prepared_with(
///       eng, src, beam.install_atomics_capabilities, beam.run)
pub fn eval_prepared_with(
  engine: Engine,
  source: String,
  prepare: fn(state.State) -> state.State,
  finish: fn(state.State) -> state.State,
) -> Result(#(Completion, Engine), EvalError) {
  use program <- result.try(
    parser.parse(source, parser.Script)
    |> result.map_error(ParseError),
  )
  use template <- result.try(
    compiler.compile(program)
    |> result.map_error(CompileError),
  )
  use completion <- result.map(
    entry.run_prepared(
      template,
      engine.heap,
      engine.builtins,
      engine.global,
      prepare,
      finish,
    )
    |> result.map_error(VmError),
  )
  #(completion, Engine(..engine, heap: completion_heap(completion)))
}

// ----------------------------------------------------------------------------
// Module evaluation
// ----------------------------------------------------------------------------

/// Compile and evaluate an ES module bundle, draining microtasks afterwards.
/// `resolve` provides source for imported specifiers
/// (`fn(raw, parent) -> Result(#(resolved, source), error)`). Returns the entry
/// module's value + namespace, plus a new engine carrying the updated heap.
///
/// A module that throws at top level surfaces as `Error(ModuleError(...))`
/// (mirroring `module.evaluate_bundle`), not a `ThrowCompletion` — read its
/// thrown value via `eval_error_message`.
pub fn eval_module(
  engine: Engine,
  specifier: String,
  source: String,
  resolve: fn(String, String) -> Result(#(String, String), String),
) -> Result(#(EvaluatedModule, Engine), EvalError) {
  eval_module_with(engine, specifier, source, resolve, event_loop.finish)
}

/// Like `eval_module` but the caller supplies the post-evaluation driver
/// (e.g. `beam.run` for a macrotask loop).
pub fn eval_module_with(
  engine: Engine,
  specifier: String,
  source: String,
  resolve: fn(String, String) -> Result(#(String, String), String),
  finish: fn(state.State) -> state.State,
) -> Result(#(EvaluatedModule, Engine), EvalError) {
  eval_module_prepared_with(
    engine,
    specifier,
    source,
    resolve,
    fn(s) { s },
    finish,
  )
}

/// Like `eval_module_with` but with an embedder `prepare` hook applied to
/// each module body's freshly booted State before it executes — the module
/// counterpart of `eval_prepared_with` (a module's top level may hit a
/// blocking `Atomics.wait` before any host function has run).
pub fn eval_module_prepared_with(
  engine: Engine,
  specifier: String,
  source: String,
  resolve: fn(String, String) -> Result(#(String, String), String),
  prepare: fn(state.State) -> state.State,
  finish: fn(state.State) -> state.State,
) -> Result(#(EvaluatedModule, Engine), EvalError) {
  use bundle <- result.try(
    module.compile_bundle(specifier, source, resolve)
    |> result.map_error(ModuleError),
  )
  use evaluated <- result.map(
    module.evaluate_bundle_prepared(
      bundle,
      engine.heap,
      engine.builtins,
      engine.global,
      prepare,
      finish,
    )
    |> result.map_error(ModuleError),
  )
  let module.EvaluatedBundle(value:, heap:, namespace:) = evaluated
  #(EvaluatedModule(value:, namespace:), Engine(..engine, heap:))
}

/// Read a named export off a Module Namespace object (the `namespace` from
/// `eval_module`). `None` if the namespace isn't a module namespace, has no
/// such export, or the binding is still uninitialized (TDZ).
pub fn read_export(
  engine: Engine,
  namespace: JsValue,
  name: String,
) -> Option(JsValue) {
  module.read_export(engine.heap, namespace, name)
}

// ----------------------------------------------------------------------------
// Calling a held value
// ----------------------------------------------------------------------------

/// Call a JS function value with `this` and `args`, draining microtasks. The
/// counterpart to `eval` for a callable you already hold — e.g. a `receive`
/// export read off a module namespace — invoked repeatedly across turns, each
/// call threading the heap forward via the returned engine. A thrown value is a
/// `ThrowCompletion`; an engine `VmError` is `Error(VmError(..))`.
pub fn call(
  engine: Engine,
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
) -> Result(#(Completion, Engine), EvalError) {
  call_with(engine, callee, this, args, event_loop.finish)
}

/// Like `call` but the caller supplies the post-call driver.
pub fn call_with(
  engine: Engine,
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
  finish: fn(state.State) -> state.State,
) -> Result(#(Completion, Engine), EvalError) {
  use completion <- result.map(
    entry.run_export(
      callee,
      this,
      args,
      engine.heap,
      engine.builtins,
      engine.global,
      finish,
    )
    |> result.map_error(VmError),
  )
  #(completion, Engine(..engine, heap: completion_heap(completion)))
}

// ----------------------------------------------------------------------------
// Serialization
// ----------------------------------------------------------------------------

/// Serialize the entire engine state to a binary.
///
/// Host function closures stored in the heap will NOT survive — their Ref
/// slots persist but the Erlang closure data is lost. Embedders must
/// re-register host functions after `deserialize`.
pub fn serialize(engine: Engine) -> BitArray {
  erlang.term_to_binary(#(engine.heap, engine.builtins, engine.global))
}

/// Restore an engine from a binary produced by `serialize`.
pub fn deserialize(data: BitArray) -> Engine {
  let #(heap, builtins, global) = erlang.binary_to_term(data)
  Engine(heap:, builtins:, global:)
}

// ----------------------------------------------------------------------------
// Accessors
// ----------------------------------------------------------------------------

/// Peek at the engine's heap. Useful for inspecting returned JsValues
/// (since most are heap refs).
pub fn heap(engine: Engine) -> Heap {
  engine.heap
}

/// The engine's builtins registry (prototypes + constructors). Needed by
/// callers that drop to the lower `entry`/`module` layers with the raw triple.
pub fn builtins(engine: Engine) -> Builtins {
  engine.builtins
}

/// The engine's global object ref (`globalThis`).
pub fn global(engine: Engine) -> Ref {
  engine.global
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

fn completion_heap(c: Completion) -> Heap {
  case c {
    completion.NormalCompletion(_, h) -> h
    completion.ThrowCompletion(_, h) -> h
    completion.YieldCompletion(_, h) -> h
    completion.AwaitCompletion(_, h) -> h
  }
}

fn compile_error_message(err: compiler.CompileError) -> String {
  case err {
    compiler.BreakOutsideLoop -> "break outside loop"
    compiler.ContinueOutsideLoop -> "continue outside loop"
    compiler.Unsupported(desc) -> "unsupported: " <> desc
  }
}

fn vm_error_message(err: state.VmError) -> String {
  case err {
    state.PcOutOfBounds(pc) -> "pc out of bounds: " <> string.inspect(pc)
    state.StackUnderflow(op) -> "stack underflow in " <> op
    state.Unimplemented(op) -> "unimplemented opcode: " <> op
  }
}

fn module_error_message(err: module.ModuleError) -> String {
  case err {
    module.ParseError(m) -> "SyntaxError: " <> m
    module.CompileError(m) -> "CompileError: " <> m
    module.ResolutionError(m) -> "ResolutionError: " <> m
    module.LinkError(m) -> "LinkError: " <> m
    module.EvaluationError(val, heap) ->
      "Uncaught " <> object.format_error(val, heap)
    // Only reachable with a non-draining finish driver; static entry points
    // convert pending to EvaluationError inside module.evaluate_linked.
    module.EvaluationPending(promise_data_ref: _, heap: _) ->
      "module evaluation never completed: top-level await promise never settled"
  }
}

pub fn eval_error_message(err: EvalError) -> String {
  case err {
    ParseError(e) -> parser.parse_error_to_string(e)
    CompileError(e) -> compile_error_message(e)
    VmError(e) -> vm_error_message(e)
    ModuleError(e) -> module_error_message(e)
  }
}
