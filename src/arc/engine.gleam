import arc/compiler
import arc/compiler/compile_task
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
  type Agent, type Handle, type JsVal, type Realm, Agent, Handle, JFloat, JInt,
  JNan, JNegInf, JPosInf, KBig, KBool, KHandle, KNull, KNum, KStr, KSym, KTdz,
  KUndef, mk_object,
}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// opaque; read with classify
pub type JsValue =
  JsVal

pub type Ref =
  Handle

pub type Number {
  Finite(Float)
  NaN
  Infinity
  NegInfinity
}

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
    // tdz sentinel never leaves the runtime
    KTdz -> panic as "arc/engine.classify: uninitialized binding sentinel"
  }
}

pub opaque type Engine(host) {
  Engine(
    agent: Agent,
    key: host.Key(host),
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
  Returned(value: JsValue)
  Threw(error: JsValue)
}

pub opaque type Namespace {
  Namespace(ref: Ref)
}

pub type EvaluatedModule {
  ModuleReturned(value: JsValue, namespace: Namespace)
  ModuleThrew(error: JsValue)
}

pub fn new() -> Engine(host) {
  from_agent(rt_builtins.new_agent(host_hooks.default_host_hooks()))
}

/// deprecated: host_refs is ignored, gc traces payloads itself
pub fn new_with_host_refs(host_refs: fn(host) -> List(Ref)) -> Engine(host) {
  let _unused = host_refs
  new()
}

fn from_agent(st: Agent) -> Engine(host) {
  Engine(agent: entry.link(st), key: host.new_key(), host_modules: dict.new())
}

pub fn with_host_hooks(
  engine: Engine(host),
  hooks: host_hooks.HostHooks,
) -> Engine(host) {
  let st = engine.agent
  Engine(..engine, agent: Agent(..st, hooks:))
}

fn host_context(engine: Engine(host)) -> host.Context(host) {
  host.from_agent(engine.agent, engine.key)
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
  val: JsValue,
) -> Engine(host) {
  adopt(engine, host.define_global(host_context(engine), name, val))
}

/// mint a native function without installing it as a global
pub fn host_fn(
  engine: Engine(host),
  name: String,
  arity: Int,
  impl: host.HostFn(host),
) -> #(Engine(host), JsValue) {
  let #(ctx, f) = host.function(host_context(engine), name, arity, impl)
  #(adopt(engine, ctx), f)
}

/// build a constructible class; nothing is installed
pub fn host_class(
  engine: Engine(host),
  name: String,
  arity: Int,
  constructor: host.HostFn(host),
  methods: List(#(String, Int, host.HostFn(host))),
  statics: List(#(String, Int, host.HostFn(host))),
) -> #(Engine(host), JsValue) {
  let #(ctx, ctor) =
    host.class(host_context(engine), name, arity, constructor, methods, statics)
  #(adopt(engine, ctx), ctor)
}

/// run host code against the engine, then end the turn
pub fn with_context(
  engine: Engine(host),
  body: fn(host.Context(host)) -> #(host.Context(host), a),
) -> #(Engine(host), a) {
  with_context_with(engine, body, rt_async.drain)
}

pub fn with_context_with(
  engine: Engine(host),
  body: fn(host.Context(host)) -> #(host.Context(host), a),
  finish: fn(Agent) -> Agent,
) -> #(Engine(host), a) {
  let #(ctx, result) = body(host_context(engine))
  let held =
    rt_gc.push_refs(result, [])
    |> list.map(fn(id) { mk_object(Handle(id)) })
  let st = safepoint.finish_turn(ctx.agent, held, finish)
  #(Engine(..engine, agent: st), result)
}

pub fn register_host_module(
  engine: Engine(host),
  specifier: String,
  exports: List(#(String, JsValue)),
) -> Engine(host) {
  let st =
    list.fold(exports, engine.agent, fn(st, export) {
      case types.classify(export.1) {
        KHandle(h) -> rt_store.t_pin_root(st, h)
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
  finish: fn(Agent) -> Agent,
) -> #(Outcome, Engine(host)) {
  let #(outcome, held) = case completion {
    NormalCompletion(v) -> #(Returned(v), v)
    ThrowCompletion(e) -> #(Threw(e), e)
  }
  let st = safepoint.finish_turn(st, [held], finish)
  #(outcome, Engine(..engine, agent: st))
}

/// §16.1.6 run a script then drain microtasks
pub fn eval(
  engine: Engine(host),
  source: String,
) -> Result(#(Outcome, Engine(host)), EvalError(host)) {
  eval_with(engine, source, rt_async.drain)
}

/// finish must drain microtasks plus any embedder loop
pub fn eval_with(
  engine: Engine(host),
  source: String,
  finish: fn(Agent) -> Agent,
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
  settle(engine, completion, st, finish)
}

/// a top-level throw is Ok(ModuleThrew), not Error
pub fn eval_module(
  engine: Engine(host),
  specifier: String,
  source: String,
  resolve: module_host.ResolveFn,
  load: module_host.LoadFn,
) -> Result(#(EvaluatedModule, Engine(host)), EvalError(host)) {
  eval_module_with(engine, specifier, source, resolve, load, rt_async.drain)
}

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
  let #(st, res) = module.evaluate_bundle(bundle, engine.agent, finish)
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
) -> Option(JsValue) {
  module.read_export(engine.agent, mk_object(namespace.ref), name)
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
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
) -> #(Outcome, Engine(host)) {
  call_with(engine, callee, this, args, rt_async.drain)
}

pub fn call_with(
  engine: Engine(host),
  callee: JsValue,
  this: JsValue,
  args: List(JsValue),
  finish: fn(Agent) -> Agent,
) -> #(Outcome, Engine(host)) {
  let #(completion, st) = rt_call.t_try_call(engine.agent, callee, this, args)
  settle(engine, completion, st, finish)
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

pub fn inspect(engine: Engine(host), value: JsValue) -> String {
  rt_inspect.inspect(engine.agent, value)
}

pub fn format_error(engine: Engine(host), error: JsValue) -> String {
  rt_inspect.format_error(engine.agent, error)
}

/// debug view of the raw store cell
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

/// the agent every arc/rt operation takes
pub fn heap(engine: Engine(host)) -> Agent {
  engine.agent
}

pub fn builtins(engine: Engine(host)) -> Realm {
  engine.agent.realm
}

pub fn global(engine: Engine(host)) -> Ref {
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
      <> module.error_message(error, engine.agent)
  }
}
