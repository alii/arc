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
import arc/module/graph
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/common.{type Builtins}
import arc/vm/exec/entry
import arc/vm/exec/event_loop
import arc/vm/exec/interpreter
import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/ops/object
import arc/vm/state.{type Heap, type HostFn}
import arc/vm/value.{type JsValue, type Ref, JsObject}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/result

// ----------------------------------------------------------------------------
// Engine type
// ----------------------------------------------------------------------------

/// An initialized JS engine — heap, builtins, global object, host hooks.
///
/// Opaque so callers can't reach inside and mutate pieces independently;
/// advance an engine via `eval`/`eval_module`/`call`, which thread the heap
/// forward and hand back a new `Engine`. Read-only access to the parts is via
/// the `heap`/`builtins`/`global` accessors.
///
/// `host_hooks` carries the embedder's host capabilities (e.g. the Atomics
/// blocking-wait / wake-delivery contract). Supplied once via
/// `with_host_hooks` at construction time, it is threaded into every State the
/// engine boots — scripts, module bodies (static and dynamic imports), calls
/// into held values — and inherited by every derived realm.
pub opaque type Engine(host) {
  Engine(
    heap: Heap(host),
    builtins: Builtins,
    global: Ref,
    host_hooks: state.HostHooks,
    /// Embedder-provided native (synthetic) modules, keyed by specifier. An
    /// `import … from "<specifier>"` in any module evaluated through this engine
    /// resolves here instead of being loaded as source. Set via
    /// `register_host_module`; see `module.HostModule`.
    host_modules: Dict(String, module.HostModule),
  )
}

/// Errors from the parse → compile → run pipeline, across both the script
/// (`eval`) and module (`eval_module`) paths. `call` only ever surfaces
/// `VmError`.
pub type EvalError(host) {
  ParseError(parser.ParseError)
  CompileError(compiler.CompileError)
  VmError(state.VmError)
  ModuleError(module.ModuleError(host))
}

/// How a top-level run of JS ended, once every VM-internal state has been
/// driven to rest: the script (or called function) returned a value, or it
/// threw one.
///
/// These are the only two ways a settled top-level run can end — the entry
/// layer proves that before handing the result back (an internal Yield/Await
/// completion escaping to the top is a compiler bug and panics there). Keeping
/// the public boundary at exactly two variants means an embedder never has to
/// write a dead "unexpected completion" arm.
pub type Outcome {
  /// Normal completion: the top-level value.
  Returned(value: JsValue)
  /// An uncaught exception: the thrown JS value. Render it for humans with
  /// `format_error`.
  Threw(error: JsValue)
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
///
/// The engine starts with `state.default_host_hooks()` (no capabilities — an
/// agent that cannot block). Embedders that need to grant host capabilities
/// (e.g. a real blocking `Atomics.wait`) compose `with_host_hooks` on top.
///
/// Host values are assumed to hold no engine refs; use `new_with_host_refs`
/// if your `HostObject` payloads point back into the JS heap.
pub fn new() -> Engine(host) {
  new_from_heap(heap.new())
}

/// Like `new`, but installs the GC hook that reports the engine refs reachable
/// from a host value. Use this when your `HostObject` payloads point back into
/// the JS heap, so GC traces them explicitly instead of relying on a "refs go
/// in properties" convention.
pub fn new_with_host_refs(host_refs: fn(host) -> List(Ref)) -> Engine(host) {
  new_from_heap(heap.new_with_host_refs(host_refs))
}

fn new_from_heap(h: state.Heap(host)) -> Engine(host) {
  let #(h, b) = builtins.init(h)
  let #(h, global) = builtins.globals(b, h)
  Engine(
    heap: h,
    builtins: b,
    global:,
    host_hooks: state.default_host_hooks(),
    host_modules: dict.new(),
  )
}

/// Install the embedder's host capabilities on the engine.
///
/// This is the single injection point for `state.HostHooks` — set them once
/// here, before running any JS, and every State the engine subsequently boots
/// (scripts via `eval`, module bodies via `eval_module` including dynamic
/// `import()`, calls via `call`, plus all derived realms: `eval`/`Function`
/// realms, `$262.createRealm` children, agents, ShadowRealms) inherits them.
/// There is no per-call install to forget.
pub fn with_host_hooks(
  engine: Engine(host),
  hooks: state.HostHooks,
) -> Engine(host) {
  Engine(..engine, host_hooks: hooks)
}

// ----------------------------------------------------------------------------
// Host FFI — extend the engine with embedder-provided globals
// ----------------------------------------------------------------------------

/// Add a top-level global native function.
///
/// The function becomes callable from JS as `name(...)`. `arity` is the
/// reported `.length` property; the impl still receives all passed args.
pub fn define_fn(
  engine: Engine(host),
  name: String,
  arity: Int,
  impl: HostFn(host),
) -> Engine(host) {
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
  engine: Engine(host),
  name: String,
  methods: List(#(String, Int, HostFn(host))),
) -> Engine(host) {
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
pub fn define_global(
  engine: Engine(host),
  name: String,
  val: JsValue,
) -> Engine(host) {
  set_global(engine, engine.heap, name, val)
}

/// Install `val` as a builtin property `name` on the engine's global object,
/// returning the engine with the updated heap. The one place global bindings
/// are written — reuses the canonical `object.define_method_property` primitive.
fn set_global(
  engine: Engine(host),
  h: Heap(host),
  name: String,
  val: JsValue,
) -> Engine(host) {
  Engine(
    ..engine,
    heap: object.define_method_property(h, engine.global, Named(name), val),
  )
}

/// Mint a host-provided native function ref into the engine's heap and hand back
/// its value — WITHOUT installing it as a global. The "return me the ref" twin of
/// `define_fn`, for building values to place elsewhere (e.g. as host-module
/// exports via `register_host_module`, or methods on a `define_class`). The ref
/// is GC-rooted by `alloc_host_fn`.
pub fn host_fn(
  engine: Engine(host),
  name: String,
  arity: Int,
  impl: HostFn(host),
) -> #(Engine(host), JsValue) {
  let #(h, fn_ref) =
    common.alloc_host_fn(
      engine.heap,
      engine.builtins.function.prototype,
      impl,
      name,
      arity,
    )
  #(Engine(..engine, heap: h), JsObject(fn_ref))
}

/// Build a host-defined, **constructible** JS class — a base class that embedder
/// JS can `extends` — and hand back its constructor value.
///
/// `constructor` is the class's `[[Construct]]` body. Like any host fn it
/// receives `(args, this, state)`; it reads `state.new_target` to learn the leaf
/// subclass (for `class Sub extends This {}`, `new_target` is `Sub`), allocates
/// and returns the `this` object (e.g. via
/// `common.ordinary_create_from_constructor(heap, new_target, proto)`), and the
/// engine re-prototypes the result to the subclass's `prototype`. `methods` are
/// installed on the class prototype; `statics` on the constructor itself (and so
/// inherited by subclasses — a static can read `this.name`).
///
/// Unlike `define_fn`/`define_namespace`, this does NOT install a global: it
/// returns the constructor so the caller places it (e.g. a `register_host_module`
/// export). The constructor and its prototype are GC-rooted by `init_type`.
pub fn define_class(
  engine: Engine(host),
  name: String,
  arity: Int,
  constructor: HostFn(host),
  methods: List(#(String, Int, HostFn(host))),
  statics: List(#(String, Int, HostFn(host))),
) -> #(Engine(host), JsValue) {
  let #(h, proto_props) =
    common.alloc_host_methods(
      engine.heap,
      engine.builtins.function.prototype,
      methods,
    )
  let #(h, static_props) =
    common.alloc_host_methods(h, engine.builtins.function.prototype, statics)
  let #(h, bt) =
    common.init_type(
      h,
      engine.builtins.object.prototype,
      engine.builtins.function.prototype,
      proto_props,
      fn(_proto) { value.Host(constructor) },
      name,
      arity,
      static_props,
    )
  #(Engine(..engine, heap: h), JsObject(bt.constructor))
}

/// Run host-side `body` against a live `State` — the escape hatch for an
/// embedder that needs to allocate JS values, invoke held functions via
/// `state.call`/`state.construct`, and marshal data in/out, all of which require
/// a `State` the public API otherwise only exposes inside a host fn.
///
/// Stands up a fresh root frame (via `interpreter.root_state`), runs `body`,
/// drains the microtask queue, then folds the resulting heap back into the
/// engine and returns `body`'s value directly. Replaces the older
/// "install a one-shot global shim and call it, smuggling the result out through
/// a side channel" idiom — no global is touched.
pub fn with_state(
  engine: Engine(host),
  body: fn(state.State(host)) -> #(state.State(host), a),
) -> #(Engine(host), a) {
  let s =
    interpreter.root_state(
      engine.heap,
      engine.builtins,
      engine.global,
      engine.host_hooks,
    )
  let #(s, result) = body(s)
  let s = event_loop.finish(s)
  #(Engine(..engine, heap: s.heap), result)
}

/// Register an embedder-provided native (synthetic) module under `specifier`.
///
/// `exports` are `(name, value)` pairs — typically `define_class` constructors
/// and `host_fn` values. Afterwards, any module evaluated through this engine
/// that does `import { name } from "<specifier>"` (or `import * as ns from
/// "<specifier>"`) binds straight to these values, with NO source loaded for
/// `specifier`. The export refs are GC-rooted here: they are held only on the
/// engine (Gleam-side), which the heap collector does not otherwise trace.
///
/// First cut: static imports only. A dynamic `import("<specifier>")` is not
/// resolved to a host module.
pub fn register_host_module(
  engine: Engine(host),
  specifier: String,
  exports: List(#(String, JsValue)),
) -> Engine(host) {
  let heap =
    list.fold(exports, engine.heap, fn(h, export) {
      case export.1 {
        JsObject(ref) -> heap.root(h, ref)
        _ -> h
      }
    })
  Engine(
    ..engine,
    heap:,
    host_modules: dict.insert(
      engine.host_modules,
      specifier,
      module.HostModule(specifier:, exports:),
    ),
  )
}

// ----------------------------------------------------------------------------
// Script evaluation
// ----------------------------------------------------------------------------

/// Parse, compile, and run a JS source string. Returns the outcome
/// (`Returned(value)` or `Threw(error)`) plus a new engine carrying the
/// updated heap.
///
/// Drains the microtask queue only — there is no macrotask loop in core.
/// If your host functions use `host.suspend`, drive your own loop via
/// `eval_with`, passing an embedder-supplied `finish` driver.
pub fn eval(
  engine: Engine(host),
  source: String,
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  eval_with(engine, source, event_loop.finish)
}

/// Like `eval` but the caller supplies the post-script driver. `finish`
/// is handed the State after the top-level script returns and must drain
/// microtasks plus whatever macrotask loop the embedder owns.
///
/// The freshly booted State carries the engine's `host_hooks`, so host
/// capabilities (e.g. the Atomics blocking-wait/wake-delivery contract in
/// `arc/host`) are present from the first instruction — a top-level blocking
/// `Atomics.wait` works without any per-call install. Set them once with
/// `with_host_hooks`.
pub fn eval_with(
  engine: Engine(host),
  source: String,
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  use #(program, sb) <- result.try(
    parser.parse(source, parser.Script)
    |> result.map_error(ParseError),
  )
  use template <- result.try(
    compiler.compile(program, sb)
    |> result.map_error(CompileError),
  )
  use #(settled, heap) <- result.map(
    entry.run_with_hooks(
      template,
      engine.heap,
      engine.builtins,
      engine.global,
      engine.host_hooks,
      finish,
    )
    |> result.map_error(VmError),
  )
  #(outcome_of(settled), Engine(..engine, heap:))
}

// ----------------------------------------------------------------------------
// Module evaluation
// ----------------------------------------------------------------------------

/// Compile and evaluate an ES module bundle, draining microtasks afterwards.
/// `resolve` maps (raw, referrer) to the dependency's canonical specifier and
/// `load` reads a resolved specifier's source (called once per unique
/// module). Returns the entry module's value + namespace, plus a new engine
/// carrying the updated heap.
///
/// A module that throws at top level surfaces as `Error(ModuleError(...))`
/// (mirroring `module.evaluate_bundle`), not a `Threw` outcome — read its
/// thrown value via `eval_error_message`.
pub fn eval_module(
  engine: Engine(host),
  specifier: String,
  source: String,
  resolve: fn(String, String) -> Result(String, String),
  load: fn(String) -> Result(String, String),
) -> Result(#(EvaluatedModule, Engine(host)), EvalError(host)) {
  eval_module_with(engine, specifier, source, resolve, load, event_loop.finish)
}

/// Like `eval_module` but the caller supplies the post-evaluation driver
/// (an embedder macrotask loop, or `event_loop.finish` for microtasks only).
///
/// Every module body in the bundle — including ones reached via dynamic
/// `import()` — boots with the engine's `host_hooks`, so a module's top level
/// may hit a blocking `Atomics.wait` before any host function has run. Set the
/// hooks once with `with_host_hooks`.
pub fn eval_module_with(
  engine: Engine(host),
  specifier: String,
  source: String,
  resolve: fn(String, String) -> Result(String, String),
  load: fn(String) -> Result(String, String),
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(#(EvaluatedModule, Engine(host)), EvalError(host)) {
  use bundle <- result.try(
    module.compile_bundle_with_hosts(
      specifier,
      source,
      resolve,
      load,
      engine.host_modules,
    )
    |> result.map_error(ModuleError),
  )
  use evaluated <- result.map(
    module.evaluate_bundle_with_hooks(
      bundle,
      engine.heap,
      engine.builtins,
      engine.global,
      engine.host_hooks,
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
  engine: Engine(host),
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
/// call threading the heap forward via the returned engine. A thrown value is
/// `Threw(error)`; an engine `VmError` is `Error(VmError(..))`.
pub fn call(
  engine: Engine(host),
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  call_with(engine, callee, this, args, event_loop.finish)
}

/// Like `call` but the caller supplies the post-call driver.
pub fn call_with(
  engine: Engine(host),
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  use #(settled, heap) <- result.map(
    entry.run_export(
      callee,
      this,
      args,
      engine.heap,
      engine.builtins,
      engine.global,
      engine.host_hooks,
      finish,
    )
    |> result.map_error(VmError),
  )
  #(outcome_of(settled), Engine(..engine, heap:))
}

// ----------------------------------------------------------------------------
// Serialization
// ----------------------------------------------------------------------------

/// The `#(tag, version, …)` envelope every `serialize` payload is wrapped in.
/// Bump `snapshot_version` whenever the shape of the serialized state changes;
/// `deserialize` then rejects older snapshots as `IncompatibleSnapshot`
/// instead of misreading them.
const snapshot_tag = "arc-engine"

// Version history:
//   1 — initial versioned envelope.
//   2 — `value.SymbolId`'s well-known variant carries a `WellKnown` sum
//       member instead of an Int, changing the serialized term shape of
//       every well-known symbol in the heap.
const snapshot_version = 2

/// Why `deserialize` rejected a binary.
pub type DeserializeError {
  /// The bytes are not an Erlang external-term payload at all (truncated,
  /// corrupted, or never produced by `serialize`).
  MalformedBinary
  /// The bytes decoded to a term, but not to a snapshot envelope this build
  /// understands: wrong tag, unknown version, or a pre-versioned / foreign
  /// shape.
  IncompatibleSnapshot
}

/// Decode a snapshot binary into its versioned envelope, or say why not.
/// Implemented in Erlang so that both failure modes of `binary_to_term` —
/// garbage bytes (badarg) and a well-formed term of the wrong shape — are
/// caught there and can never surface in Gleam as a badmatch.
@external(erlang, "arc_snapshot_ffi", "decode")
fn decode_snapshot(
  data: BitArray,
) -> Result(#(String, Int, Heap(host), Builtins, Ref), DeserializeError)

/// Serialize the entire engine state to a binary.
///
/// The payload is wrapped in a versioned envelope so `deserialize` can tell a
/// stale or foreign binary apart from a current one.
///
/// Host function closures stored in the heap will NOT survive — their Ref
/// slots persist but the Erlang closure data is lost. Embedders must
/// re-register host functions after `deserialize`. `host_hooks` are closures
/// too and are deliberately NOT serialized.
pub fn serialize(engine: Engine(host)) -> BitArray {
  erlang.term_to_binary(#(
    snapshot_tag,
    snapshot_version,
    engine.heap,
    engine.builtins,
    engine.global,
  ))
}

/// Restore an engine from a binary produced by `serialize`.
///
/// Fails with `MalformedBinary` if the bytes aren't a serialized term at all,
/// and with `IncompatibleSnapshot` if they decode to something other than a
/// version-`snapshot_version` engine snapshot (e.g. a snapshot written by an
/// older or newer build).
///
/// The restored engine carries `state.default_host_hooks()` and no host modules
/// — both hold embedder closures that cannot round-trip through `serialize`.
/// Re-install hooks with `with_host_hooks` and host modules with
/// `register_host_module`, alongside re-registering host functions.
pub fn deserialize(data: BitArray) -> Result(Engine(host), DeserializeError) {
  use #(tag, version, heap, builtins, global) <- result.try(decode_snapshot(
    data,
  ))
  case tag == snapshot_tag && version == snapshot_version {
    True ->
      Ok(Engine(
        heap:,
        builtins:,
        global:,
        host_hooks: state.default_host_hooks(),
        host_modules: dict.new(),
      ))
    False -> Error(IncompatibleSnapshot)
  }
}

// ----------------------------------------------------------------------------
// Inspecting values
// ----------------------------------------------------------------------------

/// Render a `JsValue` produced by this engine as a human-readable string
/// (REPL / `console.log` style), resolving heap refs against the engine's
/// heap. Read-only — never re-enters JS (no `toString`/`valueOf` calls).
///
/// This is the supported way for an embedder to display values from
/// `Returned`/`Threw`; there is no need to reach into `arc/vm/ops/object`
/// with the raw heap.
pub fn inspect(engine: Engine(host), value: JsValue) -> String {
  object.inspect(value, engine.heap)
}

/// Format a thrown value (a `Threw(error)`) the way an uncaught-exception
/// report would: `Error` instances become `"Name: message"` (or their stack),
/// thrown strings are shown raw, anything else falls back to `inspect`.
/// Read-only — never re-enters JS.
pub fn format_error(engine: Engine(host), error: JsValue) -> String {
  object.format_error(error, engine.heap)
}

// ----------------------------------------------------------------------------
// Accessors
// ----------------------------------------------------------------------------

/// Peek at the engine's heap. Useful for inspecting returned JsValues
/// (since most are heap refs).
pub fn heap(engine: Engine(host)) -> Heap(host) {
  engine.heap
}

/// The engine's builtins registry (prototypes + constructors). Needed by
/// callers that drop to the lower `entry`/`module` layers with the raw triple.
pub fn builtins(engine: Engine(host)) -> Builtins {
  engine.builtins
}

/// The engine's global object ref (`globalThis`).
pub fn global(engine: Engine(host)) -> Ref {
  engine.global
}

/// The engine's host hooks (whatever `with_host_hooks` installed, or
/// `state.default_host_hooks()`). Needed by callers that drop to the lower
/// `entry`/`module` layers and must thread the hooks themselves.
pub fn host_hooks(engine: Engine(host)) -> state.HostHooks {
  engine.host_hooks
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

/// Narrow the entry layer's settled `Result(value, thrown)` into the public
/// `Outcome`.
fn outcome_of(settled: Result(JsValue, JsValue)) -> Outcome {
  case settled {
    Ok(value) -> Returned(value)
    Error(thrown) -> Threw(thrown)
  }
}

/// Prefix `module.error_message`'s prose with the pipeline phase that failed.
/// Evaluation results carry no phase label: an `EvaluationError` renders as
/// the uncaught thrown value, and `EvaluationPending` is only reachable with a
/// non-draining finish driver (static entry points convert pending to
/// `EvaluationError` inside `module.evaluate_linked`).
fn module_error_message(err: module.ModuleError(host)) -> String {
  let phase = case err {
    module.GraphError(error: graph.ParseFailed(..)) -> "SyntaxError: "
    module.GraphError(error: graph.ResolveFailed(..))
    | module.GraphError(error: graph.LoadFailed(..))
    | module.ModuleNotInBundle(..) -> "ResolutionError: "
    // A source-phase import is a link-time SyntaxError (§16.2.1.7.2).
    module.GraphError(error: graph.SourcePhaseUnsupported(..)) -> "LinkError: "
    module.CompileError(..) -> "CompileError: "
    module.EvaluationError(..) | module.EvaluationPending(..) -> ""
  }
  phase <> module.error_message(err)
}

pub fn eval_error_message(err: EvalError(host)) -> String {
  case err {
    ParseError(e) -> parser.parse_error_to_string(e)
    CompileError(e) -> compiler.error_message(e)
    VmError(e) -> state.vm_error_message(e)
    ModuleError(e) -> module_error_message(e)
  }
}
