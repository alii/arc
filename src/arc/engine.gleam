//// The embed-Arc library facade.
////
//// The single front door for embedders: stand up an engine, compose host
//// functions onto it, then run JS as a script (`eval`), as an ES module
//// (`eval_module`), or by invoking a value you already hold (`call`).
////
//// An `Engine` is one `Agent` of the shared runtime (`arc/rt`: store,
//// realms, microtask queue, host hooks, host-function table) with the
//// bytecode interpreter (`arc/interp`) linked into it, plus the embedder's
//// payload key and host modules. Every entry point threads the agent forward
//// and hands back a new `Engine`; each ends its turn the same way: one
//// microtask drain and one GC safepoint (`interp/safepoint.finish_turn`).
////
//// Values cross the boundary as the opaque `JsValue`. Read one with
//// `classify`; build primitives with `arc/rt/types` (`mk_string`,
//// `mk_number`, `mk_bool`, ...) and objects through `arc/host` inside a host
//// function or `with_state`.

import arc/compiler
import arc/host
import arc/host_hooks
import arc/interp/entry
import arc/interp/safepoint
import arc/module
import arc/module_host
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/snapshot
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type Realm, Agent, JFloat, JInt, JNan,
  JNegInf, JPosInf, JsCell, KBig, KBool, KHandle, KNull, KNum, KStr, KSym, KTdz,
  KUndef, mk_object,
}
import arc/vm/compile_task
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ----------------------------------------------------------------------------
// Values
// ----------------------------------------------------------------------------

/// A JS value as the engine hands it out and takes it back: the runtime's
/// opaque wire value. Never match on it; `classify` it.
pub type JsValue =
  JsVal

/// A heap object reference (the payload of `JsObject`).
pub type Ref =
  Handle

/// A JS Number as `classify` reports it.
pub type Number {
  Finite(Float)
  NaN
  Infinity
  NegInfinity
}

/// What a `JsValue` is, for embedder code to branch on.
pub type JsValueKind {
  JsUndefined
  JsNull
  JsBool(Bool)
  JsNumber(Number)
  JsString(String)
  JsObject(Ref)
  JsSymbol
  JsBigInt(Int)
}

/// Decode a `JsValue`. Integral and fractional numbers both come back as
/// `Finite`.
pub fn classify(value: JsValue) -> JsValueKind {
  case types.classify(value) {
    KUndef -> JsUndefined
    KNull -> JsNull
    KBool(b) -> JsBool(b)
    KNum(JInt(i)) -> JsNumber(Finite(int.to_float(i)))
    KNum(JFloat(f)) -> JsNumber(Finite(f))
    KNum(JNan) -> JsNumber(NaN)
    KNum(JPosInf) -> JsNumber(Infinity)
    KNum(JNegInf) -> JsNumber(NegInfinity)
    KStr(s) -> JsString(s)
    KBig(n) -> JsBigInt(n)
    KSym(_) -> JsSymbol
    KHandle(h) -> JsObject(h)
    // The TDZ sentinel never leaves the runtime: no entry point returns it
    // and `read_export` filters it.
    KTdz -> panic as "arc/engine.classify: uninitialized binding sentinel"
  }
}

// ----------------------------------------------------------------------------
// Engine type
// ----------------------------------------------------------------------------

/// An initialized JS engine.
///
/// Opaque so callers can't reach inside and mutate pieces independently;
/// advance an engine via `eval`/`eval_module`/`call`/`with_state`, which
/// thread the agent forward and hand back a new `Engine`.
///
/// `Agent` is not generic over the embedder's payload type, so `host` is
/// pinned by `key` (see `arc/host.Key`): every host function this engine
/// registers and every `with_state` body sees a `host.State(host)` carrying
/// it, and host objects are written and read under it.
pub opaque type Engine(host) {
  Engine(
    agent: Agent,
    key: host.Key(host),
    /// Embedder-provided native (synthetic) modules, keyed by specifier. An
    /// `import … from "<specifier>"` in any module evaluated through this
    /// engine resolves here instead of being loaded as source. Set via
    /// `register_host_module`; see `module.HostModule`.
    host_modules: Dict(String, module.HostModule),
  )
}

/// Errors from the parse → compile → run pipeline, across both the script
/// (`eval`) and module (`eval_module`) paths. A run that reaches bytecode
/// always completes: engine faults inside it surface as a thrown TypeError.
pub type EvalError(host) {
  ParseError(parser.ParseError)
  CompileError(compiler.CompileError)
  /// AOT compilation of a module graph failed (parse / resolve / load /
  /// bytecode). Nothing has been allocated yet.
  ModuleCompileError(module.CompileBundleError)
  /// Linking a module graph failed, or its evaluation could not settle. A
  /// module whose top level THREW is not this: it comes back as an
  /// `Ok(ModuleThrew(..))`, like every other run.
  ///
  /// `engine` is the engine as the failing call left it: rendering the
  /// `ModuleError` needs it, and it stays usable.
  ModuleError(error: module.ModuleError, engine: Engine(host))
}

/// How a top-level run of JS ended once the turn has settled: the script
/// (or called function) returned a value, or it threw one.
pub type Outcome {
  /// Normal completion: the top-level value.
  Returned(value: JsValue)
  /// An uncaught exception: the thrown JS value. Render it for humans with
  /// `format_error`.
  Threw(error: JsValue)
}

/// A module's Module Namespace object (§10.4.6), as minted by `eval_module` /
/// `eval_module_with`. Opaque and unforgeable: `read_export` cannot be handed
/// something that isn't a namespace.
pub opaque type Namespace {
  Namespace(ref: Ref)
}

/// The result of evaluating an ES module: either the entry module's top level
/// completed normally, carrying its return value and Module Namespace object
/// (read named exports off it with `read_export`), or it threw.
pub type EvaluatedModule {
  ModuleReturned(value: JsValue, namespace: Namespace)
  ModuleThrew(error: JsValue)
}

// ----------------------------------------------------------------------------
// Constructors
// ----------------------------------------------------------------------------

/// Create a fresh engine: a new agent with realm 0 fully initialised, the
/// interpreter linked in, `host_hooks.default_host_hooks()` (no capabilities:
/// an agent that cannot block) and a fresh payload key. Embedders that need
/// host capabilities compose `with_host_hooks` on top.
pub fn new() -> Engine(host) {
  from_agent(rt_builtins.new_agent(host_hooks.default_host_hooks()))
}

/// Like `new`. The collector traces heap references inside host payloads on
/// its own (`arc/rt/gc` walks every term a `KHost` cell holds), so
/// `host_refs` is no longer consulted; the signature stays for embedders
/// written against the tracing-hook contract.
pub fn new_with_host_refs(host_refs: fn(host) -> List(Ref)) -> Engine(host) {
  let _unused = host_refs
  new()
}

fn from_agent(agent: Agent) -> Engine(host) {
  Engine(
    agent: entry.link(agent),
    key: host.new_key(),
    host_modules: dict.new(),
  )
}

/// Install the embedder's host capabilities on the engine. The hooks live on
/// the agent, so everything the engine subsequently runs (scripts, module
/// bodies including dynamic `import()`, calls, every realm) sees them.
pub fn with_host_hooks(
  engine: Engine(host),
  hooks: host_hooks.HostHooks,
) -> Engine(host) {
  let agent = engine.agent
  Engine(..engine, agent: Agent(..agent, hooks:))
}

// ----------------------------------------------------------------------------
// Host FFI — extend the engine with embedder-provided globals
// ----------------------------------------------------------------------------

/// The engine's agent as host code sees it outside a call: no [[Construct]]
/// in progress, this engine's payload key.
fn host_state(engine: Engine(host)) -> host.State(host) {
  host.from_agent(engine.agent, engine.key)
}

fn adopt(engine: Engine(host), s: host.State(host)) -> Engine(host) {
  Engine(..engine, agent: s.agent)
}

/// Add a top-level global native function.
///
/// The function becomes callable from JS as `name(...)`. `arity` is the
/// reported `.length` property; the impl still receives all passed args.
/// Exactly `host_fn` + `define_global`.
pub fn define_fn(
  engine: Engine(host),
  name: String,
  arity: Int,
  impl: host.HostFn(host),
) -> Engine(host) {
  adopt(engine, host.define_fn(host_state(engine), name, arity, impl))
}

/// Add a top-level namespace object (like `Math` or `JSON`) with methods.
/// Each method spec is `#(name, arity, impl)`; the namespace carries
/// `@@toStringTag = name` like every built-in namespace.
pub fn define_namespace(
  engine: Engine(host),
  name: String,
  methods: List(#(String, Int, host.HostFn(host))),
) -> Engine(host) {
  adopt(engine, host.define_namespace(host_state(engine), name, methods))
}

/// Add a raw JsValue as a top-level global binding: a writable,
/// configurable, non-enumerable data property on `globalThis`.
pub fn define_global(
  engine: Engine(host),
  name: String,
  val: JsValue,
) -> Engine(host) {
  adopt(engine, host.define_global(host_state(engine), name, val))
}

/// Mint a host-provided native function and hand back its value WITHOUT
/// installing it as a global, for building values to place elsewhere (a
/// `register_host_module` export, a method table). Its closure lives on the
/// agent's host-function table under the next id (`NativeToken.HostFn(id)`);
/// the object is GC-rooted.
pub fn host_fn(
  engine: Engine(host),
  name: String,
  arity: Int,
  impl: host.HostFn(host),
) -> #(Engine(host), JsValue) {
  let #(s, f) = host.function(host_state(engine), name, arity, impl)
  #(adopt(engine, s), f)
}

/// Build a host-defined, constructible JS class (a base class embedder JS
/// can `extends`) and hand back its constructor value; nothing is installed
/// on the global. See `host.class` for the constructor contract
/// (`host.new_target`, re-prototyping of the returned instance). `methods`
/// go on the prototype, `statics` on the constructor.
pub fn host_class(
  engine: Engine(host),
  name: String,
  arity: Int,
  constructor: host.HostFn(host),
  methods: List(#(String, Int, host.HostFn(host))),
  statics: List(#(String, Int, host.HostFn(host))),
) -> #(Engine(host), JsValue) {
  let #(s, ctor) =
    host.class(host_state(engine), name, arity, constructor, methods, statics)
  #(adopt(engine, s), ctor)
}

/// Run host-side `body` against a live `host.State`: allocate JS values,
/// invoke held functions via `host.call`, marshal data in/out. Ends the
/// turn like `eval` (microtask drain, GC safepoint) and returns `body`'s
/// value directly. Heap references inside that value are kept alive through
/// the epilogue; make them reachable (`define_global`, a host module) before
/// the next turn.
pub fn with_state(
  engine: Engine(host),
  body: fn(host.State(host)) -> #(host.State(host), a),
) -> #(Engine(host), a) {
  with_state_with(engine, body, rt_async.drain)
}

/// Like `with_state` but the caller supplies the turn-end driver, as with
/// `eval_with`.
pub fn with_state_with(
  engine: Engine(host),
  body: fn(host.State(host)) -> #(host.State(host), a),
  finish: fn(Agent) -> Agent,
) -> #(Engine(host), a) {
  let #(s, result) = body(host_state(engine))
  let held =
    rt_gc.push_term_refs(to_dynamic(result), [])
    |> list.map(fn(id) { mk_object(JsCell(id)) })
  let agent = safepoint.finish_turn(s.agent, held, finish)
  #(Engine(..engine, agent:), result)
}

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: anything) -> Dynamic

/// Register an embedder-provided native (synthetic) module under `specifier`.
///
/// `exports` are `(name, value)` pairs, typically `host_class` constructors
/// and `host_fn` values. Afterwards any module evaluated through this engine
/// that does `import { name } from "<specifier>"` binds straight to these
/// values, with no source loaded for `specifier`. Object exports are pinned
/// as GC roots: until a module imports them they are held only here.
pub fn register_host_module(
  engine: Engine(host),
  specifier: String,
  exports: List(#(String, JsValue)),
) -> Engine(host) {
  let agent =
    list.fold(exports, engine.agent, fn(agent, export) {
      case types.classify(export.1) {
        KHandle(h) -> rt_store.t_pin_root(agent, h)
        _ -> agent
      }
    })
  Engine(
    ..engine,
    agent:,
    host_modules: dict.insert(
      engine.host_modules,
      specifier,
      module.HostModule(specifier:, exports:),
    ),
  )
}

// ----------------------------------------------------------------------------
// Turn epilogue
// ----------------------------------------------------------------------------

/// The one turn end: hold the completion value, collect if due, run
/// `finish` (the microtask drain, or an embedder loop that drains), release.
fn settle(
  engine: Engine(host),
  completion: Completion,
  agent: Agent,
  finish: fn(Agent) -> Agent,
) -> #(Outcome, Engine(host)) {
  let #(outcome, held) = case completion {
    NormalCompletion(v) -> #(Returned(v), v)
    ThrowCompletion(e) -> #(Threw(e), e)
  }
  let agent = safepoint.finish_turn(agent, [held], finish)
  #(outcome, Engine(..engine, agent:))
}

// ----------------------------------------------------------------------------
// Script evaluation
// ----------------------------------------------------------------------------

/// Parse, compile, and run a JS source string in the engine's current realm
/// (§16.1.6 ScriptEvaluation), then drain microtasks. Consecutive scripts
/// share one global environment. There is no macrotask loop in core: if
/// your host functions use `host.suspend`, drive your own loop via
/// `eval_with`.
pub fn eval(
  engine: Engine(host),
  source: String,
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  eval_with(engine, source, rt_async.drain)
}

/// Like `eval` but the caller supplies the turn-end driver. `finish` is
/// handed the agent after the top-level script returns and must drain
/// microtasks (`rt/async.drain`) plus whatever macrotask loop the embedder
/// owns.
pub fn eval_with(
  engine: Engine(host),
  source: String,
  finish: fn(Agent) -> Agent,
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  // Big sources parse+compile in a heap-sized scratch process (see
  // arc/vm/compile_task); only the compact FuncTemplate (or error) crosses
  // back.
  use template <- result.map(
    compile_task.run(string.byte_size(source), fn() {
      use #(body, sb) <- result.try(
        parser.parse_script(source) |> result.map_error(ParseError),
      )
      compiler.compile(body, sb) |> result.map_error(CompileError)
    }),
  )
  let #(completion, agent) = entry.run_script(engine.agent, template)
  settle(engine, completion, agent, finish)
}

// ----------------------------------------------------------------------------
// Module evaluation
// ----------------------------------------------------------------------------

/// Compile and evaluate an ES module bundle, draining microtasks after each
/// module body. `resolve` maps (raw, referrer) to the dependency's canonical
/// specifier and `load` reads a resolved specifier's source (once per unique
/// module). Returns the entry module's outcome + namespace and the engine.
///
/// A module that throws at top level is a normal `Ok(ModuleThrew(value))`;
/// `Error(ModuleError(..))` is reserved for link failures and evaluations
/// that cannot settle.
pub fn eval_module(
  engine: Engine(host),
  specifier: String,
  source: String,
  resolve: module_host.ResolveFn,
  load: module_host.LoadFn,
) -> Result(#(EvaluatedModule, Engine(host)), EvalError(host)) {
  eval_module_with(engine, specifier, source, resolve, load, rt_async.drain)
}

/// Like `eval_module` but the caller supplies the driver each module body's
/// turn ends with (`rt/async.drain`, or an embedder loop that drains).
pub fn eval_module_with(
  engine: Engine(host),
  specifier: String,
  source: String,
  resolve: module_host.ResolveFn,
  load: module_host.LoadFn,
  finish: fn(Agent) -> Agent,
) -> Result(#(EvaluatedModule, Engine(host)), EvalError(host)) {
  use bundle <- result.try(
    module.compile_bundle_with_hosts(
      specifier,
      source,
      resolve,
      load,
      engine.host_modules,
    )
    |> result.map_error(ModuleCompileError),
  )
  // Each module body ends its own turn through `finish` (see
  // `module.Finish`), so no further epilogue runs here.
  let #(agent, res) = module.evaluate_bundle(bundle, engine.agent, finish)
  let engine = Engine(..engine, agent:)
  case res {
    Ok(module.EvaluatedBundle(value:, namespace:)) ->
      Ok(#(ModuleReturned(value:, namespace: Namespace(namespace)), engine))
    Error(module.EvaluationError(value: thrown)) ->
      Ok(#(ModuleThrew(error: thrown), engine))
    Error(err) -> Error(ModuleError(error: err, engine:))
  }
}

/// Read a named export off a module's `Namespace` (from `eval_module`).
/// `None` if there is no such export, or the binding is still uninitialized
/// (TDZ).
pub fn read_export(
  engine: Engine(host),
  namespace: Namespace,
  name: String,
) -> Option(JsValue) {
  module.read_export(engine.agent, mk_object(namespace.ref), name)
}

// ----------------------------------------------------------------------------
// REPL sessions
// ----------------------------------------------------------------------------

/// An engine driven one input at a time. Inputs compile in REPL mode, so
/// top-level `let`/`const`/`class` declarations land in the realm's global
/// lexical record (`Realm.lexical_globals`) and later inputs see them.
pub opaque type Repl(host) {
  Repl(engine: Engine(host))
}

/// Start a REPL session on `engine`. Host functions, hooks and modules
/// already installed on the engine are visible to every input.
pub fn repl(engine: Engine(host)) -> Repl(host) {
  Repl(engine:)
}

/// The session's engine as of the last input, for rendering values the
/// session produced (`inspect`, `format_error`, `dump_object`).
pub fn repl_engine(repl: Repl(host)) -> Engine(host) {
  repl.engine
}

/// Parse, compile (in REPL mode) and run one input, draining microtasks.
/// The outcome is the input's completion value, so `1 + 1` is
/// `Returned(2)`. On `Error` nothing ran and the session passed in is still
/// the current one.
pub fn repl_eval(
  repl: Repl(host),
  source: String,
) -> Result(#(Outcome, Repl(host)), EvalError(host)) {
  use #(body, sb) <- result.try(
    parser.parse_script(source) |> result.map_error(ParseError),
  )
  use template <- result.map(
    compiler.compile_repl(body, sb) |> result.map_error(CompileError),
  )
  let engine = repl.engine
  let #(completion, agent) = entry.run_script(engine.agent, template)
  let #(outcome, engine) = settle(engine, completion, agent, rt_async.drain)
  #(outcome, Repl(engine:))
}

// ----------------------------------------------------------------------------
// Calling a held value
// ----------------------------------------------------------------------------

/// Call a JS function value with `this` and `args`, then drain microtasks:
/// the counterpart to `eval` for a callable you already hold (a module
/// export, say), each call threading the agent forward via the returned
/// engine. A non-callable `callee` is a thrown TypeError like any other
/// `Threw`.
pub fn call(
  engine: Engine(host),
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
) -> #(Outcome, Engine(host)) {
  call_with(engine, callee, this, args, rt_async.drain)
}

/// Like `call` but the caller supplies the turn-end driver.
pub fn call_with(
  engine: Engine(host),
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
  finish: fn(Agent) -> Agent,
) -> #(Outcome, Engine(host)) {
  let #(completion, agent) = rt_call.t_call(engine.agent, callee, this, args)
  settle(engine, completion, agent, finish)
}

// ----------------------------------------------------------------------------
// Serialization
// ----------------------------------------------------------------------------

/// Serialize the entire engine state to a binary (`arc/rt/snapshot`).
///
/// Not written, and re-bound after `deserialize`: host functions (their
/// objects survive, the closures do not; re-register them in the same
/// order), host hooks, the dynamic-import hook, host modules. Fails with
/// `SnapshotContainsCompiledCode` when the heap holds a function whose body
/// is compiled BEAM code, and with `SnapshotContainsHostJob` while a
/// `host.resume` settlement is still queued.
pub fn serialize(
  engine: Engine(host),
) -> Result(BitArray, snapshot.SnapshotError) {
  snapshot.serialize(engine.agent)
}

/// Restore an engine from a binary produced by `serialize`.
///
/// Fails with `MalformedBinary` if the bytes carry no snapshot header, and
/// with `IncompatibleSnapshot` if they name a different ABI version or hide a
/// corrupt payload. The restored engine carries the default host hooks, no
/// host functions, no dynamic-import hook, no host modules and a fresh
/// payload key: re-install them with `with_host_hooks`, `define_fn`/`host_fn`
/// (in the original order: that alone decides which closure a surviving
/// function object reaches), `module_host.install_import_hook` and
/// `register_host_module`, each independently of the others. Host objects
/// written under the old key read as `None`.
pub fn deserialize(
  data: BitArray,
) -> Result(Engine(host), snapshot.DeserializeError) {
  snapshot.deserialize(data, host_hooks.default_host_hooks())
  |> result.map(from_agent)
}

// ----------------------------------------------------------------------------
// Inspecting values
// ----------------------------------------------------------------------------

/// Render a value as a human-readable string (REPL / `console.log` style).
/// Read-only: never re-enters JS.
pub fn inspect(engine: Engine(host), value: JsValue) -> String {
  rt_inspect.inspect(engine.agent, value)
}

/// Format a thrown value the way an uncaught-exception report would:
/// `Error` instances become `"Name: message"` (or their stack), thrown
/// strings are shown raw, anything else falls back to `inspect`.
pub fn format_error(engine: Engine(host), error: JsValue) -> String {
  rt_inspect.format_error(engine.agent, error)
}

/// The raw store slot behind an object value, rendered as the Gleam term: a
/// debugging view (the CLI's `/heap`). `None` when `val` is not an object; a
/// dangling reference reads `<collected>`.
pub fn dump_object(engine: Engine(host), val: JsValue) -> Option(String) {
  case types.classify(val) {
    KHandle(h) ->
      case rt_gc.t_is_live(engine.agent, h) {
        True -> Some(string.inspect(rt_store.t_cell_get(engine.agent, h)))
        False -> Some("<collected>")
      }
    _ -> None
  }
}

// ----------------------------------------------------------------------------
// Accessors
// ----------------------------------------------------------------------------

/// The engine's agent: store, realms, microtask queue, hooks. What every
/// `arc/rt` operation takes.
pub fn heap(engine: Engine(host)) -> Agent {
  engine.agent
}

/// The current realm's intrinsics (prototypes, constructors, global object).
pub fn builtins(engine: Engine(host)) -> Realm {
  engine.agent.realm
}

/// The current realm's global object (`globalThis`).
pub fn global(engine: Engine(host)) -> Ref {
  engine.agent.realm.global_object
}

/// The engine's host hooks (whatever `with_host_hooks` installed, or the
/// defaults).
pub fn host_hooks(engine: Engine(host)) -> host_hooks.HostHooks {
  engine.agent.hooks
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

pub fn eval_error_message(err: EvalError(host)) -> String {
  case err {
    ParseError(e) -> parser.parse_error_to_string(e)
    CompileError(e) -> compiler.error_message(e)
    ModuleCompileError(e) -> module.format_compile_bundle_error(e)
    ModuleError(error:, engine:) ->
      module.module_error_phase(error)
      <> module.error_message(error, engine.agent)
  }
}
