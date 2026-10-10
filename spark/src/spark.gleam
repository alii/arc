import arc/bytecode/key
import arc/engine.{type Engine, type EvalError, type Outcome, Returned, Threw}
import arc/host
import arc/module/loader
import arc/rt/async as rt_async
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, JFloat, JInt, KBool, KNum, StringKey, classify,
  mk_bool, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict.{type Dict}
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Answer, type Failure, type Io, type ReplySlip}
import spark/js

/// an engine plus a registry of builtin modules
///
/// sans-io: nothing here waits, sleeps, or touches files, sockets or the
/// mailbox; scripts reach the outside only through the `Io` passed to start,
/// and the embedder's loop drives timers with `turn`
pub opaque type Runtime(host) {
  Runtime(
    engine: Engine(host),
    core: Core,
    // specifier to export names
    builtins: Dict(String, List(String)),
  )
}

type Core {
  Core(
    register: JsVal,
    require: JsVal,
    install_globals: JsVal,
    run_commonjs: JsVal,
    run_tick: JsVal,
    step: JsVal,
    wait: JsVal,
    alive: JsVal,
  )
}

pub type StartError {
  BootstrapFailed(message: String)
  BuiltinFailed(specifier: String, message: String)
  /// a builtin names a dependency that was not passed to start
  MissingDependency(specifier: String, dependency: String)
  DuplicateBuiltin(specifier: String)
}

/// what the embedder's loop should do next
pub type Next {
  /// nothing is left to wait for
  Finished
  /// a timer is due in this many ms; call `turn` again then
  WakeIn(ms: Int)
}

const runtime_specifier = "spark:runtime"

const core_specifiers = ["spark:loop", "spark:errors"]

/// an engine with spark's own js compiled into it and none of it run
///
/// it knows nothing about a machine, so one of these can be made once and
/// every runtime started from it skips the parsing and compiling
pub opaque type Compiled(host) {
  Compiled(
    engine: Engine(host),
    bootstrap: JsVal,
    // by source text; a builtin with any other source compiles on first use
    factories: Dict(String, JsVal),
  )
}

const shipped_sources = [
  js.errors,
  js.async_hooks,
  js.timers,
  js.fs,
  js.fs_promises,
  js.process,
  js.os,
  js.encoding,
  js.perf_hooks,
  js.global,
  js.domexception,
  js.dom_events,
  js.buffer,
  js.path,
  js.path_posix,
  js.path_win32,
  js.net,
  js.querystring_internal,
  js.querystring,
  js.url,
]

/// compiles the js of every builtin spark ships; installs and runs nothing
pub fn compile(eng: Engine(host)) -> Result(Compiled(host), StartError) {
  use compiled <- result.try(compile_bootstrap(eng))
  use compiled, source <- list.try_fold(shipped_sources, compiled)
  use #(factory, eng) <- result.map(compile_function(
    compiled.engine,
    "(function (exports, require, module, native) {\n" <> source <> "\n})",
  ))
  Compiled(
    ..compiled,
    engine: eng,
    factories: dict.insert(compiled.factories, source, factory),
  )
}

fn compile_bootstrap(eng: Engine(host)) -> Result(Compiled(host), StartError) {
  use #(bootstrap, eng) <- result.map(compile_function(eng, js.bootstrap))
  Compiled(engine: eng, bootstrap:, factories: dict.new())
}

// source is one function expression; the function is kept alive in the engine
fn compile_function(
  eng: Engine(host),
  source: String,
) -> Result(#(JsVal, Engine(host)), StartError) {
  case engine.eval(eng, source) {
    Ok(#(Returned(function), eng)) -> {
      let #(Nil, eng) =
        engine.with_context(eng, fn(ctx) {
          let st = case rt_val.handle_of(function) {
            Some(handle) -> rt_store.pin_root(ctx.agent, handle)
            None -> ctx.agent
          }
          #(Nil, host.Context(..ctx, agent: st))
        })
      Ok(#(function, eng))
    }
    Ok(#(Threw(thrown), eng)) ->
      Error(BootstrapFailed(engine.format_error(eng, thrown)))
    Error(err) -> Error(BootstrapFailed(engine.eval_error_message(err)))
  }
}

/// an engine plus exactly these builtins, with all their io going through `io`
///
/// a builtin never brings another one with it: a dependency that is not in
/// the list is a MissingDependency error
pub fn start(
  eng: Engine(host),
  builtins: List(Builtin(host)),
  io: Io,
) -> Result(Runtime(host), StartError) {
  use compiled <- result.try(compile_bootstrap(eng))
  start_compiled(compiled, builtins, io)
}

/// `start`, from an engine whose spark js is already compiled
pub fn start_compiled(
  compiled: Compiled(host),
  builtins: List(Builtin(host)),
  io: Io,
) -> Result(Runtime(host), StartError) {
  use Nil <- result.try(check(builtins))
  use rt <- result.try(boot(compiled))
  let register = fn(rt, builtin, hidden) {
    register(rt, builtin, io, compiled.factories, hidden:)
  }
  use rt <- result.try(register(rt, errors_builtin(), True))
  use rt <- result.try(
    list.try_fold(builtins, rt, fn(rt, builtin) { register(rt, builtin, False) }),
  )
  list.try_fold(builtins, rt, install_globals)
}

// node-shaped errors for the bundled js; only builtins can require it
fn errors_builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:errors",
    source: js.errors,
    natives: fn(_io) {
      [
        host.HostMethod("inspect", 1, fn(ctx, args, _this) {
          let text = rt_inspect.describe(ctx.agent, host.first_arg(args))
          #(Ok(mk_string(text)), ctx)
        }),
      ]
    },
    dependencies: [],
    exports: [],
    globals: [],
  )
}

fn check(builtins: List(Builtin(host))) -> Result(Nil, StartError) {
  let specifiers = list.map(builtins, fn(builtin) { builtin.specifier })
  use Nil <- result.try(case duplicate(specifiers) {
    Some(specifier) -> Error(DuplicateBuiltin(specifier:))
    None -> Ok(Nil)
  })
  let known = list.append(core_specifiers, specifiers)
  list.try_each(builtins, fn(builtin) {
    list.try_each(builtin.dependencies, fn(dependency) {
      case list.contains(known, dependency) {
        True -> Ok(Nil)
        False ->
          Error(MissingDependency(specifier: builtin.specifier, dependency:))
      }
    })
  })
}

fn duplicate(specifiers: List(String)) -> Option(String) {
  case specifiers {
    [] -> None
    [first, ..rest] ->
      case list.contains(rest, first) {
        True -> Some(first)
        False -> duplicate(rest)
      }
  }
}

fn boot(compiled: Compiled(host)) -> Result(Runtime(host), StartError) {
  use #(core, eng) <- result.try(
    case engine.call(compiled.engine, compiled.bootstrap, mk_undefined(), []) {
      #(Returned(core), eng) -> Ok(#(core, eng))
      #(Threw(thrown), eng) ->
        Error(BootstrapFailed(engine.format_error(eng, thrown)))
    },
  )
  let #(core, eng) =
    engine.with_context(eng, fn(ctx) {
      let st = case rt_val.handle_of(core) {
        Some(handle) -> rt_store.pin_root(ctx.agent, handle)
        None -> ctx.agent
      }
      let get = fn(st, name) {
        rt_obj.get_prop(st, core, StringKey(key.canonical(name)))
      }
      let #(register, st) = get(st, "register")
      let #(require, st) = get(st, "require")
      let #(install_globals, st) = get(st, "installGlobals")
      let #(run_commonjs, st) = get(st, "runCommonJs")
      let #(run_tick, st) = get(st, "runTick")
      let #(step, st) = get(st, "step")
      let #(wait, st) = get(st, "wait")
      let #(alive, st) = get(st, "alive")
      let core =
        Core(
          register:,
          require:,
          install_globals:,
          run_commonjs:,
          run_tick:,
          step:,
          wait:,
          alive:,
        )
      #(core, host.Context(..ctx, agent: st))
    })
  let eng =
    engine.register_host_module(eng, runtime_specifier, [
      #("require", core.require),
    ])
  Ok(Runtime(engine: eng, core:, builtins: dict.new()))
}

// nothing runs yet; the source is compiled on first require
fn register(
  rt: Runtime(host),
  builtin: Builtin(host),
  io: Io,
  factories: Dict(String, JsVal),
  hidden hidden: Bool,
) -> Result(Runtime(host), StartError) {
  let factory =
    dict.get(factories, builtin.source) |> result.unwrap(mk_undefined())
  let #(args, eng) =
    engine.with_context(rt.engine, fn(ctx) {
      let #(natives, ctx) =
        list.fold(builtin.natives(io), #([], ctx), fn(acc, native) {
          let #(natives, ctx) = acc
          let host.HostMethod(name:, arity:, call:) = native
          let #(function, ctx) = host.function(ctx, name, arity, call)
          #([#(name, function), ..natives], ctx)
        })
      let #(natives, ctx) = host.object(ctx, list.reverse(natives))
      let #(dependencies, ctx) =
        host.array(ctx, list.map(builtin.dependencies, mk_string))
      #(
        [
          mk_string(builtin.specifier),
          mk_string(builtin.source),
          natives,
          dependencies,
          mk_bool(hidden),
          factory,
        ],
        ctx,
      )
    })
  let rt = Runtime(..rt, engine: eng)
  use rt <- result.map(setup_call(rt, builtin, rt.core.register, args))
  case hidden {
    True -> rt
    False ->
      Runtime(
        ..rt,
        builtins: dict.insert(rt.builtins, builtin.specifier, builtin.exports),
      )
  }
}

fn install_globals(
  rt: Runtime(host),
  builtin: Builtin(host),
) -> Result(Runtime(host), StartError) {
  case builtin.globals {
    [] -> Ok(rt)
    [_, ..] -> {
      let #(globals, eng) =
        engine.with_context(rt.engine, fn(ctx) {
          host.array(ctx, list.map(builtin.globals, mk_string))
        })
      setup_call(Runtime(..rt, engine: eng), builtin, rt.core.install_globals, [
        mk_string(builtin.specifier),
        globals,
      ])
    }
  }
}

fn setup_call(
  rt: Runtime(host),
  builtin: Builtin(host),
  callee: JsVal,
  args: List(JsVal),
) -> Result(Runtime(host), StartError) {
  case engine.call_with(rt.engine, callee, mk_undefined(), args, drain(rt)) {
    #(Returned(_), eng) -> Ok(Runtime(..rt, engine: eng))
    #(Threw(thrown), eng) ->
      Error(BuiltinFailed(
        specifier: builtin.specifier,
        message: engine.format_error(eng, thrown),
      ))
  }
}

pub fn engine(rt: Runtime(host)) -> Engine(host) {
  rt.engine
}

/// the require scripts see
pub fn require_function(rt: Runtime(host)) -> JsVal {
  rt.core.require
}

/// run every timer and immediate that is due now, with microtasks between
/// them, then say what to wait for; it never waits itself
pub fn turn(rt: Runtime(host)) -> #(Next, Runtime(host)) {
  let #(ran, rt) = ask(rt, rt.core.step)
  case classify(ran) {
    KBool(False) -> {
      let #(alive, rt) = ask(rt, rt.core.alive)
      let #(wait, rt) = ask(rt, rt.core.wait)
      case classify(alive), wait_ms(wait) {
        KBool(True), Some(ms) -> #(WakeIn(ms:), rt)
        _, _ -> #(Finished, rt)
      }
    }
    _ -> turn(rt)
  }
}

// what ends every call into js: all queued ticks, then all promise jobs,
// again until a pass runs no tick; node's order
fn drain(rt: Runtime(host)) -> rt_async.Drain {
  let run_tick = rt.core.run_tick
  fn(st) { run_ticks(st, run_tick) }
}

fn run_ticks(st: Agent, run_tick: JsVal) -> Agent {
  let #(ran, st) = each_tick(st, run_tick, False)
  let st = rt_async.drain(st)
  case ran {
    True -> run_ticks(st, run_tick)
    False -> st
  }
}

fn each_tick(st: Agent, run_tick: JsVal, ran: Bool) -> #(Bool, Agent) {
  case rt_call.try_call(st, run_tick, mk_undefined(), []) {
    #(NormalCompletion(more), st) ->
      case classify(more) {
        KBool(False) -> #(ran, st)
        _ -> each_tick(st, run_tick, True)
      }
    #(ThrowCompletion(thrown), st) -> {
      st.hooks.report_uncaught(
        "Uncaught " <> rt_inspect.format_error(st, thrown),
      )
      each_tick(st, run_tick, True)
    }
  }
}

fn wait_ms(wait: JsVal) -> Option(Int) {
  case classify(wait) {
    KNum(JInt(ms)) if ms >= 0 -> Some(ms)
    KNum(JFloat(ms)) if ms >=. 0.0 -> Some(float.truncate(float.ceiling(ms)))
    _ -> None
  }
}

fn ask(rt: Runtime(host), source: JsVal) -> #(JsVal, Runtime(host)) {
  case engine.call_with(rt.engine, source, mk_undefined(), [], drain(rt)) {
    #(Returned(value), eng) -> #(value, Runtime(..rt, engine: eng))
    #(Threw(thrown), eng) -> {
      engine.host_hooks(eng).report_uncaught(
        "Uncaught " <> engine.format_error(eng, thrown),
      )
      #(mk_undefined(), Runtime(..rt, engine: eng))
    }
  }
}

/// answer one want that went through the `later` door, then run every
/// microtask that follows from it before returning
///
/// a slip is answered once; a second answer changes nothing and comes back
/// as AlreadySettled
pub fn answer(
  rt: Runtime(host),
  slip: ReplySlip,
  result: Result(Answer, Failure),
) -> #(host.ResumeOutcome, Runtime(host)) {
  let #(outcome, eng) =
    engine.with_context_with(
      rt.engine,
      fn(ctx) { io.settle(slip, ctx, result) },
      drain(rt),
    )
  #(outcome, Runtime(..rt, engine: eng))
}

/// run a script and its microtasks; timers wait for `turn`
pub fn eval(
  rt: Runtime(host),
  source: String,
) -> Result(#(Outcome, Runtime(host)), EvalError(host)) {
  use #(outcome, eng) <- result.map(engine.eval_with(
    rt.engine,
    source,
    drain(rt),
  ))
  #(outcome, Runtime(..rt, engine: eng))
}

/// call a js function and run its microtasks
pub fn call(
  rt: Runtime(host),
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(Outcome, Runtime(host)) {
  let #(outcome, eng) =
    engine.call_with(rt.engine, callee, this, args, drain(rt))
  #(outcome, Runtime(..rt, engine: eng))
}

/// run source as a commonjs module; returns its module.exports
pub fn eval_commonjs(
  rt: Runtime(host),
  filename: String,
  source: String,
) -> #(Outcome, Runtime(host)) {
  call(rt, rt.core.run_commonjs, mk_undefined(), [
    mk_string(filename),
    mk_string(dirname(filename)),
    mk_string(source),
  ])
}

/// run an es module whose imports may name builtins
///
/// a module is not done until its top-level awaits settle, so the embedder's
/// loop (turn, wait, answer) is passed in and runs while they are pending
pub fn eval_module(
  rt: Runtime(host),
  specifier: String,
  source: String,
  resolve: loader.ResolveFn,
  load: loader.LoadFn,
  loop: fn(Runtime(host)) -> Runtime(host),
) -> Result(#(engine.EvaluatedModule, Runtime(host)), EvalError(host)) {
  let builtins = rt.builtins
  let resolve = fn(raw, referrer) {
    case builtin_specifier(builtins, raw) {
      Some(found) -> Ok(found)
      None -> resolve(raw, referrer)
    }
  }
  let load = fn(resolved, attributes) {
    case dict.get(builtins, resolved) {
      Ok(exports) -> Ok(loader.SourceText(import_shim(resolved, exports)))
      Error(Nil) -> load(resolved, attributes)
    }
  }
  let ticks = drain(rt)
  let drain =
    engine.drain_with(rt.engine, fn(eng) {
      let #(Nil, eng) =
        engine.with_context_with(eng, fn(ctx) { #(Nil, ctx) }, ticks)
      loop(Runtime(..rt, engine: eng)).engine
    })
  use #(evaluated, eng) <- result.map(engine.eval_module_with(
    rt.engine,
    specifier,
    source,
    resolve,
    load,
    drain,
  ))
  #(evaluated, Runtime(..rt, engine: eng))
}

fn builtin_specifier(
  builtins: Dict(String, List(String)),
  raw: String,
) -> Option(String) {
  let prefixed = "node:" <> raw
  case raw == runtime_specifier || dict.has_key(builtins, raw) {
    True -> Some(raw)
    False ->
      case dict.has_key(builtins, prefixed) {
        True -> Some(prefixed)
        False -> None
      }
  }
}

fn import_shim(specifier: String, exports: List(String)) -> String {
  let named =
    list.map(exports, fn(name) {
      "export const " <> name <> " = builtin." <> name <> ";\n"
    })
  "import { require } from \""
  <> runtime_specifier
  <> "\";\nconst builtin = require(\""
  <> specifier
  <> "\");\nexport default builtin;\n"
  <> string.concat(named)
}

fn dirname(path: String) -> String {
  case string.split(path, "/") |> list.reverse {
    [_file, ..rest] if rest != [] -> string.join(list.reverse(rest), "/")
    _ -> "."
  }
}

pub fn start_error_message(err: StartError) -> String {
  case err {
    BootstrapFailed(message:) -> "spark: bootstrap failed: " <> message
    BuiltinFailed(specifier:, message:) ->
      "spark: builtin " <> specifier <> " failed: " <> message
    MissingDependency(specifier:, dependency:) ->
      "spark: builtin "
      <> specifier
      <> " needs "
      <> dependency
      <> ", which was not passed to start"
    DuplicateBuiltin(specifier:) ->
      "spark: builtin " <> specifier <> " was passed twice"
  }
}
