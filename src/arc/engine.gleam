import arc/compiler
import arc/compiler/compile_task
import arc/host
import arc/host_hooks
import arc/interp/entry
import arc/interp/safepoint
import arc/module
import arc/module/loader
import arc/parser
import arc/rt/async.{type Drain} as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/snapshot
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type Realm, Agent, Handle, KHandle,
  mk_object,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub opaque type Engine(host) {
  Engine(
    agent: Agent,
    brand: host.Brand(host),
    host_modules: Dict(String, module.HostModule),
  )
}

pub type EvalError(host) {
  ParseError(parser.ParseError)
  CompileError(compiler.CompileError)
  ModuleCompileError(module.CompileBundleError)
  ModuleError(error: module.ModuleError, engine: Engine(host))
}

pub type Outcome {
  Returned(value: JsVal)
  Threw(error: JsVal)
}

pub opaque type Namespace {
  Namespace(handle: Handle)
}

pub type EvaluatedModule {
  ModuleReturned(value: JsVal, namespace: Namespace)
  ModuleThrew(error: JsVal)
}

pub fn new() -> Engine(host) {
  from_agent(rt_builtins.new_agent(host_hooks.default_host_hooks()))
}

/// deprecated: host_refs is ignored, gc traces payloads itself
pub fn new_with_host_refs(host_refs: fn(host) -> List(Handle)) -> Engine(host) {
  let _unused = host_refs
  new()
}

fn from_agent(st: Agent) -> Engine(host) {
  Engine(
    agent: entry.link(st),
    brand: host.new_brand(),
    host_modules: dict.new(),
  )
}

pub fn with_host_hooks(
  engine: Engine(host),
  hooks: host_hooks.HostHooks,
) -> Engine(host) {
  let st = engine.agent
  Engine(..engine, agent: Agent(..st, hooks:))
}

fn host_context(engine: Engine(host)) -> host.Context(host) {
  host.from_agent(engine.agent, engine.brand)
}

fn adopt(engine: Engine(host), ctx: host.Context(host)) -> Engine(host) {
  Engine(..engine, agent: ctx.agent)
}

pub fn define_fn(
  engine: Engine(host),
  name: String,
  arity: Int,
  impl: host.HostFn(host),
) -> Engine(host) {
  adopt(engine, host.define_fn(host_context(engine), name, arity, impl))
}

pub fn define_namespace(
  engine: Engine(host),
  name: String,
  methods: List(#(String, Int, host.HostFn(host))),
) -> Engine(host) {
  adopt(engine, host.define_namespace(host_context(engine), name, methods))
}

pub fn define_global(
  engine: Engine(host),
  name: String,
  val: JsVal,
) -> Engine(host) {
  adopt(engine, host.define_global(host_context(engine), name, val))
}

/// mint a native function without installing it as a global
pub fn host_fn(
  engine: Engine(host),
  name: String,
  arity: Int,
  impl: host.HostFn(host),
) -> #(JsVal, Engine(host)) {
  let #(f, ctx) = host.function(host_context(engine), name, arity, impl)
  #(f, adopt(engine, ctx))
}

/// build a constructible class; nothing is installed
pub fn host_class(
  engine: Engine(host),
  name: String,
  arity: Int,
  constructor: host.HostFn(host),
  methods: List(#(String, Int, host.HostFn(host))),
  statics: List(#(String, Int, host.HostFn(host))),
) -> #(JsVal, Engine(host)) {
  let #(ctor, ctx) =
    host.class(host_context(engine), name, arity, constructor, methods, statics)
  #(ctor, adopt(engine, ctx))
}

/// run host code against the engine, then end the turn
pub fn with_context(
  engine: Engine(host),
  body: fn(host.Context(host)) -> #(a, host.Context(host)),
) -> #(a, Engine(host)) {
  with_context_with(engine, body, rt_async.drain)
}

pub fn with_context_with(
  engine: Engine(host),
  body: fn(host.Context(host)) -> #(a, host.Context(host)),
  drain: Drain,
) -> #(a, Engine(host)) {
  let #(result, ctx) = body(host_context(engine))
  let held =
    rt_gc.push_refs(result, [])
    |> list.map(fn(id) { mk_object(Handle(id)) })
  let st = safepoint.finish_turn(ctx.agent, held, drain)
  #(result, Engine(..engine, agent: st))
}

pub fn register_host_module(
  engine: Engine(host),
  specifier: String,
  exports: List(#(String, JsVal)),
) -> Engine(host) {
  let st =
    list.fold(exports, engine.agent, fn(st, export) {
      case types.classify(export.1) {
        KHandle(h) -> rt_store.pin_root(st, h)
        _ -> st
      }
    })
  Engine(
    ..engine,
    agent: st,
    host_modules: dict.insert(
      engine.host_modules,
      specifier,
      module.HostModule(specifier:, exports:),
    ),
  )
}

fn settle(
  engine: Engine(host),
  completion: Completion(JsVal),
  st: Agent,
  drain: Drain,
) -> #(Outcome, Engine(host)) {
  let #(outcome, held) = case completion {
    NormalCompletion(v) -> #(Returned(v), v)
    ThrowCompletion(e) -> #(Threw(e), e)
  }
  let st = safepoint.finish_turn(st, [held], drain)
  #(outcome, Engine(..engine, agent: st))
}

/// §16.1.6 run a script then drain microtasks
pub fn eval(
  engine: Engine(host),
  source: String,
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  eval_with(engine, source, rt_async.drain)
}

/// drain must run microtasks plus any embedder loop
pub fn eval_with(
  engine: Engine(host),
  source: String,
  drain: Drain,
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  use template <- result.map(
    compile_task.run(string.byte_size(source), fn() {
      use #(body, sb) <- result.try(
        parser.parse_script(source) |> result.map_error(ParseError),
      )
      compiler.compile_script(body, sb) |> result.map_error(CompileError)
    }),
  )
  let #(completion, st) = entry.run_script(engine.agent, template)
  settle(engine, completion, st, drain)
}

/// a top-level throw is Ok(ModuleThrew), not Error
pub fn eval_module(
  engine: Engine(host),
  specifier: String,
  source: String,
  resolve: loader.ResolveFn,
  load: loader.LoadFn,
) -> Result(#(EvaluatedModule, Engine(host)), EvalError(host)) {
  eval_module_with(engine, specifier, source, resolve, load, rt_async.drain)
}

pub fn eval_module_with(
  engine: Engine(host),
  specifier: String,
  source: String,
  resolve: loader.ResolveFn,
  load: loader.LoadFn,
  drain: Drain,
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
  let #(res, st) = module.evaluate_bundle(engine.agent, bundle, drain)
  let engine = Engine(..engine, agent: st)
  case res {
    Ok(module.EvaluatedBundle(value:, namespace:)) ->
      Ok(#(ModuleReturned(value:, namespace: Namespace(namespace)), engine))
    Error(module.EvaluationError(value: thrown)) ->
      Ok(#(ModuleThrew(error: thrown), engine))
    Error(err) -> Error(ModuleError(error: err, engine:))
  }
}

/// none if missing or still in tdz
pub fn read_export(
  engine: Engine(host),
  namespace: Namespace,
  name: String,
) -> Option(JsVal) {
  module.read_export(engine.agent, mk_object(namespace.handle), name)
}

pub opaque type Repl(host) {
  Repl(engine: Engine(host))
}

pub fn repl(engine: Engine(host)) -> Repl(host) {
  Repl(engine:)
}

pub fn repl_engine(repl: Repl(host)) -> Engine(host) {
  repl.engine
}

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
  let #(completion, st) = entry.run_script(engine.agent, template)
  let #(outcome, engine) = settle(engine, completion, st, rt_async.drain)
  #(outcome, Repl(engine:))
}

pub fn call(
  engine: Engine(host),
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(Outcome, Engine(host)) {
  call_with(engine, callee, this, args, rt_async.drain)
}

pub fn call_with(
  engine: Engine(host),
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
  drain: Drain,
) -> #(Outcome, Engine(host)) {
  let #(completion, st) = rt_call.try_call(engine.agent, callee, this, args)
  settle(engine, completion, st, drain)
}

/// host fns, hooks and host modules are not written
pub fn serialize(
  engine: Engine(host),
) -> Result(BitArray, snapshot.SnapshotError) {
  snapshot.serialize(engine.agent)
}

/// re-register host fns in the original order afterwards
pub fn deserialize(
  data: BitArray,
) -> Result(Engine(host), snapshot.DeserializeError) {
  snapshot.deserialize(data, host_hooks.default_host_hooks())
  |> result.map(from_agent)
}

pub fn inspect(engine: Engine(host), value: JsVal) -> String {
  rt_inspect.inspect(engine.agent, value)
}

pub fn format_error(engine: Engine(host), error: JsVal) -> String {
  rt_inspect.format_error(engine.agent, error)
}

/// debug view of the raw store cell
pub fn dump_object(engine: Engine(host), val: JsVal) -> Option(String) {
  case types.classify(val) {
    KHandle(h) ->
      case rt_gc.is_live(engine.agent, h) {
        True -> Some(string.inspect(rt_store.cell_get(engine.agent, h)))
        False -> Some("<collected>")
      }
    _ -> None
  }
}

/// the agent every arc/rt operation takes
pub fn agent(engine: Engine(host)) -> Agent {
  engine.agent
}

pub fn realm(engine: Engine(host)) -> Realm {
  engine.agent.realm
}

pub fn global(engine: Engine(host)) -> Handle {
  engine.agent.realm.global_object
}

pub fn host_hooks(engine: Engine(host)) -> host_hooks.HostHooks {
  engine.agent.hooks
}

pub fn eval_error_message(err: EvalError(host)) -> String {
  case err {
    ParseError(e) -> parser.parse_error_to_string(e)
    CompileError(e) -> compiler.error_message(e)
    ModuleCompileError(e) -> module.format_compile_bundle_error(e)
    ModuleError(error:, engine:) ->
      module.module_error_phase(error)
      <> module.error_message(engine.agent, error)
  }
}
