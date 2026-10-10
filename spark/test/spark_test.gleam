import arc/engine.{type Engine, Returned, Threw}
import arc/host.{AlreadySettled, Resumed}
import arc/host_hooks.{HostHooks}
import arc/module/loader
import arc/rt/types.{KStr, classify}
import gleam/bit_array
import gleam/list
import gleam/option
import gleam/string
import spark.{
  type Runtime, DuplicateBuiltin, Finished, MissingDependency, WakeIn,
}
import spark/async_hooks
import spark/buffer
import spark/builtin.{type Builtin, Builtin}
import spark/dom_events
import spark/domexception
import spark/encoding
import spark/fs
import spark/global
import spark/io.{
  type Io, type ReplySlip, type Want, Access, Done, Failure, Io, Realpath,
  Stderr, Stdout, Text, Write,
}
import spark/net
import spark/os.{BigEndian, Os}
import spark/path
import spark/perf_hooks
import spark/platform.{type Platform, Darwin, Linux}
import spark/process.{Process}
import spark/timers

@external(erlang, "spark_test_ffi", "main")
pub fn main() -> Nil

@external(erlang, "spark_test_ffi", "note")
fn note(text: String) -> Nil

@external(erlang, "spark_test_ffi", "notes")
fn notes() -> List(String)

@external(erlang, "spark_test_ffi", "keep")
fn keep(asked: #(Want, ReplySlip)) -> Nil

@external(erlang, "spark_test_ffi", "kept")
fn kept() -> List(#(Want, ReplySlip))

fn builtins() -> List(Builtin(Nil)) {
  [
    async_hooks.builtin(),
    timers.builtin(),
    fs.builtin(),
    fs.promises_builtin(),
    process.builtin(made_up_process()),
    os.builtin(made_up_os(Linux)),
    encoding.builtin(),
    perf_hooks.builtin(),
    domexception.builtin(),
    dom_events.builtin(),
    buffer.builtin(),
    path.builtin(),
    path.posix_builtin(),
    path.win32_builtin(),
    net.builtin(),
  ]
}

fn made_up_os(platform: Platform) -> os.Os {
  Os(
    platform:,
    arch: "pdp11",
    release: "4th edition",
    version: "#1 a made up kernel",
    machine: "pdp11/70",
    hostname: "spark-host",
    homedir: "/usr/glenda",
    tmpdir: "/tmp/made-up",
    endianness: BigEndian,
    parallelism: 3,
  )
}

// nothing here comes from the machine running the tests
fn made_up_process() -> process.Process {
  Process(
    env: [#("HOME", "/home/spark"), #("EMPTY", "")],
    argv: ["spark", "/app/main.cjs", "--flag"],
    cwd: "/work",
    platform: Linux,
    arch: "pdp11",
    pid: 4242,
    version: "v99.1.2",
    versions: [#("node", "99.1.2"), #("spark", "0.1.0")],
    stdout_is_tty: True,
    stderr_is_tty: False,
  )
}

// a made-up disk: /there exists, /secret is refused, nothing else exists;
// wants through the later door are kept until the test answers them
fn fake_io() -> Io {
  Io(
    now: fn(want) {
      case want {
        Access(path: "/there", ..) -> Ok(Done)
        Access(path: "/secret", ..) -> Error(Failure("EACCES"))
        Access(..) -> Error(Failure("ENOENT"))
        Realpath(path: "/link") -> Ok(Text("/real/place"))
        Realpath(..) -> Error(Failure("ENOENT"))
        Write(stream: Stdout, data:) -> {
          note("stdout " <> text(data))
          Ok(Done)
        }
        Write(stream: Stderr, data: <<"refuse":utf8, _:bits>>) ->
          Error(Failure("EPIPE"))
        Write(stream: Stderr, data:) -> {
          note("stderr " <> text(data))
          Ok(Done)
        }
      }
    },
    later: fn(want, slip) { keep(#(want, slip)) },
  )
}

fn text(data: BitArray) -> String {
  let assert Ok(text) = bit_array.to_string(data)
  text
}

fn start_on(eng: Engine(Nil), builtins: List(Builtin(Nil))) -> Runtime(Nil) {
  let assert Ok(rt) = spark.start(eng, builtins, fake_io())
  rt
}

fn node_runtime() -> Runtime(Nil) {
  start_on(engine.new(), builtins())
}

// a clock that never moves, so no timer ever comes due
fn frozen_runtime() -> Runtime(Nil) {
  let hooks = HostHooks(..host_hooks.default(), monotonic_now: fn() { 0 })
  start_on(engine.new() |> engine.with_host_hooks(hooks), builtins())
}

// the embedder's loop: spark says how long, the caller does the waiting
fn run_to_end(rt: Runtime(Nil)) -> Runtime(Nil) {
  case spark.turn(rt) {
    #(Finished, rt) -> rt
    #(WakeIn(ms:), rt) -> {
      host_hooks.default().sleep_ms(ms)
      run_to_end(rt)
    }
  }
}

fn run(rt: Runtime(Nil), source: String) -> #(String, Runtime(Nil)) {
  case spark.eval(rt, source) {
    Ok(#(Returned(value), rt)) -> #(engine.inspect(spark.engine(rt), value), rt)
    Ok(#(Threw(thrown), rt)) -> #(
      "threw " <> engine.format_error(spark.engine(rt), thrown),
      rt,
    )
    Error(err) -> panic as engine.eval_error_message(err)
  }
}

// runs the source as commonjs; timers have not run yet
fn run_commonjs(rt: Runtime(Nil), source: String) -> Runtime(Nil) {
  let prelude = "globalThis.out = [];\n"
  case spark.eval_commonjs(rt, "/app/main.cjs", prelude <> source) {
    #(Returned(_), rt) -> rt
    #(Threw(thrown), rt) ->
      panic as engine.format_error(spark.engine(rt), thrown)
  }
}

fn out(rt: Runtime(Nil)) -> String {
  run(rt, "out.join()").0
}

// runs the source and the loop to the end, then reads `out`
fn out_after(source: String) -> String {
  out(run_to_end(run_commonjs(node_runtime(), source)))
}

pub fn nothing_waits_inside_spark_test() {
  let rt =
    run_commonjs(
      frozen_runtime(),
      "
      setTimeout(() => out.push('timer'), 50);
      setTimeout(() => out.push('unrefed'), 20).unref();
      setImmediate(() => out.push('immediate'));
      Promise.resolve().then(() => out.push('microtask'));
      ",
    )
  assert out(rt) == "'microtask'"
  let #(next, rt) = spark.turn(rt)
  assert next == WakeIn(20)
  assert out(rt) == "'microtask,immediate'"
  let #(next, rt) = spark.turn(rt)
  assert next == WakeIn(20)
  assert out(rt) == "'microtask,immediate'"
}

pub fn a_loop_with_only_unrefed_timers_is_finished_test() {
  let rt = run_commonjs(frozen_runtime(), "setTimeout(() => {}, 20).unref();")
  assert spark.turn(rt).0 == Finished
  assert spark.turn(node_runtime()).0 == Finished
}

pub fn timers_fire_in_deadline_order_test() {
  assert out_after(
      "
      setTimeout((a, b) => out.push('late' + a + b), 200, 1, 2);
      setTimeout(() => out.push('soon'), 1);
      out.push('sync');
      ",
    )
    == "'sync,soon,late12'"
}

pub fn an_immediate_gets_its_arguments_test() {
  assert out_after(
      "setImmediate((x, y) => out.push('immediate' + x + y), 1, 2);",
    )
    == "'immediate12'"
}

pub fn interval_repeats_until_cleared_test() {
  assert out_after(
      "
      var n = 0;
      var interval = setInterval(() => {
        out.push(++n);
        if (n === 3) clearInterval(interval);
      }, 1);
      ",
    )
    == "'1,2,3'"
}

pub fn cleared_and_unrefed_timers_do_not_hold_the_loop_test() {
  let clock = host_hooks.default().monotonic_now
  let started = clock()
  assert out_after(
      "
      clearTimeout(setTimeout(() => out.push('cleared'), 60000));
      clearTimeout(+setTimeout(() => out.push('cleared by id'), 60000));
      setTimeout(() => out.push('unrefed'), 60000).unref();
      clearImmediate(setImmediate(() => out.push('immediate')));
      out.push('done');
      ",
    )
    == "'done'"
  assert clock() - started < 5000
}

pub fn refresh_pushes_the_deadline_back_test() {
  assert out_after(
      "
      var first = setTimeout(() => out.push('first'), 100);
      setTimeout(() => out.push('second'), 150);
      setTimeout(() => first.refresh(), 75);
      ",
    )
    == "'second,first'"
}

pub fn microtasks_run_between_timers_test() {
  assert out_after(
      "
      setTimeout(() => { out.push('a'); Promise.resolve().then(() => out.push('a.then')) }, 1);
      setTimeout(() => out.push('b'), 1);
      ",
    )
    == "'a,a.then,b'"
}

pub fn async_local_storage_follows_awaits_and_timers_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const { AsyncLocalStorage } = require('node:async_hooks');
      const store = new AsyncLocalStorage();
      const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
      async function request(name, ms) {
        await sleep(ms);
        out.push(name + ':' + store.getStore());
        setImmediate(() => out.push(name + ' immediate:' + store.getStore()));
      }
      store.run('one', () => request('a', 10));
      store.run('two', () => request('b', 1));
      request('c', 5);
      ",
    )
  assert run(run_to_end(rt), "out.sort().join()").0
    == "'a immediate:one,a:one,b immediate:two,b:two,c immediate:undefined,c:undefined'"
}

pub fn async_local_storage_scopes_test() {
  assert out_after(
      "
      const { AsyncLocalStorage, AsyncResource } = require('async_hooks');
      const store = new AsyncLocalStorage({ defaultValue: 'none', name: 'ctx' });
      out.push(store.name, store.getStore());
      out.push(store.run('outer', () => store.run('inner', () => store.getStore()) + '/' + store.getStore()));
      out.push(store.run('outer', () => store.exit(() => String(store.getStore()))));
      const snapshot = store.run('snap', () => AsyncLocalStorage.snapshot());
      out.push(snapshot(() => store.getStore()));
      const resource = store.run('resource', () => new AsyncResource('test'));
      out.push(resource.runInAsyncScope(function (a) { return store.getStore() + a + this.tag }, { tag: '!' }, '?'));
      const bound = store.run('bound', () => AsyncLocalStorage.bind((a) => store.getStore() + a));
      out.push(bound(1), store.getStore());
      store.enterWith('entered');
      out.push(store.getStore());
      store.disable();
      out.push(store.getStore());
      ",
    )
    == "'ctx,none,inner/outer,undefined,snap,resource?!,bound1,none,entered,none'"
}

pub fn require_caches_and_reports_missing_modules_test() {
  assert out_after(
      "
      out.push(require('timers') === require('node:timers'));
      out.push(require('timers').setTimeout === setTimeout);
      try { require('missing') } catch (error) { out.push(error.code, error.message) }
      ",
    )
    == "'true,true,MODULE_NOT_FOUND,Cannot find module \\'missing\\''"
}

pub fn commonjs_gets_its_module_scope_test() {
  let #(outcome, rt) =
    spark.eval_commonjs(
      node_runtime(),
      "/app/main.cjs",
      "exports.where = __dirname + '|' + __filename; exports.same = module.exports === exports && this === exports;",
    )
  let assert Returned(exports) = outcome
  let eng = spark.engine(rt)
  assert engine.inspect(eng, exports)
    == "{ where: '/app|/app/main.cjs', same: true }"
}

pub fn modules_import_builtins_test() {
  let #(resolve, load) = loader.no_imports()
  let assert Ok(#(engine.ModuleReturned(namespace:, ..), rt)) =
    spark.eval_module(
      node_runtime(),
      "/app/main.js",
      "
      import hooks, { AsyncLocalStorage } from 'async_hooks';
      import { setTimeout as later } from 'node:timers';
      const store = new AsyncLocalStorage();
      export let seen = 'not yet';
      store.run('module', () => later(() => { seen = store.getStore() }, 1));
      export const same = hooks.AsyncLocalStorage === AsyncLocalStorage;
      ",
      resolve,
      load,
      run_to_end,
    )
  let eng = spark.engine(rt)
  let assert option.Some(seen) = engine.read_export(eng, namespace, "seen")
  let assert option.Some(same) = engine.read_export(eng, namespace, "same")
  assert engine.inspect(eng, seen) == "'module'"
  assert engine.inspect(eng, same) == "true"
}

pub fn declared_exports_match_the_module_test() {
  let rt = node_runtime()
  list.each(builtins(), fn(builtin) {
    let rt =
      run_commonjs(
        rt,
        "out = Object.keys(require('" <> builtin.specifier <> "')).sort();",
      )
    let declared =
      list.sort(builtin.exports, string.compare) |> string.join(",")
    assert out(rt) == "'" <> declared <> "'"
  })
}

pub fn a_throwing_timer_is_reported_and_the_loop_goes_on_test() {
  let hooks = HostHooks(..host_hooks.default(), report_uncaught: note)
  let rt =
    run_commonjs(
      start_on(engine.new() |> engine.with_host_hooks(hooks), builtins()),
      "
      setTimeout(() => { throw new Error('boom') }, 1);
      setTimeout(() => out.push('after'), 5);
      ",
    )
  assert out(run_to_end(rt)) == "'after'"
  let assert [report] = notes()
  assert string.starts_with(report, "Uncaught Error: boom")
}

fn script_builtin(
  specifier: String,
  source: String,
  dependencies: List(String),
) -> Builtin(Nil) {
  Builtin(
    specifier:,
    source:,
    natives: fn(_io) { [] },
    dependencies:,
    exports: [],
    globals: [],
  )
}

fn delay_builtin() -> Builtin(Nil) {
  script_builtin(
    "test:delay",
    "const { setTimeout } = require('node:timers');
     module.exports = { delay: (callback) => { setTimeout(callback, 1) } };",
    ["node:timers"],
  )
}

pub fn a_builtin_requires_its_declared_dependency_test() {
  let rt = start_on(engine.new(), [timers.builtin(), delay_builtin()])
  let rt =
    run_commonjs(rt, "require('test:delay').delay(() => out.push('ran'));")
  assert out(run_to_end(rt)) == "'ran'"
}

pub fn a_builtin_reaches_only_its_declared_dependencies_test() {
  let greedy =
    script_builtin(
      "test:greedy",
      "module.exports = require('node:timers');",
      [],
    )
  let rt =
    run_commonjs(
      start_on(engine.new(), [timers.builtin(), greedy]),
      "try { require('test:greedy') } catch (error) { out.push(error.message) }",
    )
  assert out(rt) == "'test:greedy has no dependency on \\'node:timers\\''"
}

pub fn the_loop_module_is_hidden_from_scripts_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "try { require('spark:loop') } catch (error) { out.push(error.code) }",
    )
  assert out(rt) == "'MODULE_NOT_FOUND'"
}

pub fn a_dependency_must_be_passed_too_test() {
  let assert Error(MissingDependency(specifier:, dependency:)) =
    spark.start(engine.new(), [delay_builtin()], fake_io())
  assert specifier == "test:delay"
  assert dependency == "node:timers"
}

pub fn a_builtin_is_passed_once_test() {
  let assert Error(DuplicateBuiltin(specifier:)) =
    spark.start(engine.new(), [timers.builtin(), timers.builtin()], fake_io())
  assert specifier == "node:timers"
}

// `out`, one entry per line, without the quoting inspect adds
fn out_lines(rt: Runtime(Nil)) -> List(String) {
  let assert Ok(#(Returned(joined), _)) = spark.eval(rt, "out.join('\\n')")
  let assert KStr(text) = classify(joined)
  string.split(text, "\n")
}

const describe = "
  function describe(run) {
    try { return 'ok ' + run() } catch (e) { return String(e) + ' ' + Object.keys(e).join() }
  }
"

pub fn the_now_door_answers_sync_calls_test() {
  let rt = run_commonjs(node_runtime(), describe <> "
        const fs = require('fs');
        out.push(fs.existsSync('/there'), fs.existsSync('/nope'), fs.existsSync(5));
        out.push(describe(() => fs.accessSync('/there')));
        out.push(describe(() => fs.accessSync('/nope')));
        out.push(describe(() => fs.accessSync('/secret', fs.constants.R_OK)));
        try { fs.accessSync('/nope') } catch (e) { out.push([e.errno, e.code, e.syscall, e.path].join()) }
        ")
  assert out_lines(rt)
    == [
      "true", "false", "false", "ok undefined",
      "Error: ENOENT: no such file or directory, access '/nope' errno,code,syscall,path",
      "Error: EACCES: permission denied, access '/secret' errno,code,syscall,path",
      "-2,ENOENT,access,/nope",
    ]
}

pub fn a_sync_want_is_answered_under_a_builtin_callback_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const fs = require('fs');
      out.push(['/there', '/nope'].map(path => fs.existsSync(path)).join('+'));
      out.push(['/nope', '/there'].sort((a, b) => fs.existsSync(b) - fs.existsSync(a)).join('+'));
      out.push(JSON.parse('[\"/there\"]', (key, value) => typeof value === 'string' ? fs.existsSync(value) : value).join());
      ",
    )
  assert out_lines(rt) == ["true+false", "/there+/nope", "true"]
}

pub fn arguments_are_checked_like_node_test() {
  let rt = run_commonjs(node_runtime(), describe <> "
        const fs = require('fs');
        out.push(describe(() => fs.accessSync(5)));
        out.push(describe(() => fs.accessSync({})));
        out.push(describe(() => fs.accessSync(function foo() {})));
        out.push(describe(() => fs.accessSync(Symbol('s'))));
        out.push(describe(() => fs.accessSync('/there', 'x')));
        out.push(describe(() => fs.accessSync('/there', 9)));
        out.push(describe(() => fs.accessSync('/there', NaN)));
        out.push(describe(() => fs.accessSync('/there', 0.5)));
        out.push(describe(() => fs.access('/there')));
        out.push(describe(() => setTimeout(5)));
        ")
  let path =
    "The \"path\" argument must be of type string or an instance of Buffer or URL. Received "
  assert out_lines(rt)
    == [
      "TypeError [ERR_INVALID_ARG_TYPE]: " <> path <> "type number (5) code",
      "TypeError [ERR_INVALID_ARG_TYPE]: "
        <> path
        <> "an instance of Object code",
      "TypeError [ERR_INVALID_ARG_TYPE]: " <> path <> "function foo code",
      "TypeError [ERR_INVALID_ARG_TYPE]: "
        <> path
        <> "type symbol (Symbol(s)) code",
      "TypeError: mode must be int32 or null/undefined code",
      "RangeError: mode is out of range: >= 0 && <= 7 code",
      "RangeError: mode is out of range code",
      "ok undefined",
      "TypeError [ERR_INVALID_ARG_TYPE]: The \"cb\" argument must be of type function. Received undefined code",
      "TypeError [ERR_INVALID_ARG_TYPE]: The \"callback\" argument must be of type function. Received type number (5) code",
    ]
}

pub fn the_later_door_answers_when_the_embedder_says_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const fs = require('fs');
      fs.promises.access('/anything', fs.constants.W_OK).then(
        () => out.push('granted'),
        error => out.push(error.code + ' ' + error.message),
      );
      out.push('asked');
      ",
    )
  assert out_lines(rt) == ["asked"]
  let assert [#(want, slip)] = kept()
  assert want == Access(path: "/anything", mode: 2)
  let #(outcome, rt) = spark.answer(rt, slip, Ok(Done))
  assert outcome == Resumed
  assert out_lines(rt) == ["asked", "granted"]
  let #(outcome, rt) = spark.answer(rt, slip, Error(Failure("EIO")))
  assert outcome == AlreadySettled
  assert out_lines(rt) == ["asked", "granted"]
}

pub fn a_failure_through_the_later_door_rejects_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const fs = require('fs');
      fs.promises.access('/x').catch(e => out.push([e.message, e.errno, e.code, e.syscall, e.path].join()));
      fs.access('/y', error => out.push('callback ' + error.code));
      fs.access('/z', error => out.push('callback ' + error));
      fs.exists('/w', found => out.push('exists ' + found));
      ",
    )
  let assert [#(_, x), #(_, y), #(_, z), #(_, w)] = kept()
  let #(_, rt) = spark.answer(rt, x, Error(Failure("EROFS")))
  let #(_, rt) = spark.answer(rt, y, Error(Failure("ENOENT")))
  let #(_, rt) = spark.answer(rt, z, Ok(Done))
  let #(_, rt) = spark.answer(rt, w, Ok(Done))
  assert out_lines(rt)
    == [
      "EROFS: read-only file system, access '/x',-30,EROFS,access,/x",
      "callback ENOENT", "callback null", "exists true",
    ]
}

pub fn each_answer_runs_its_microtasks_before_the_next_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const { access } = require('fs/promises');
      access('/a').then(() => out.push('a1')).then(() => out.push('a2'));
      access('/b').then(() => out.push('b1')).then(() => out.push('b2'));
      ",
    )
  let assert [#(_, a), #(_, b)] = kept()
  let #(_, rt) = spark.answer(rt, b, Ok(Done))
  assert out_lines(rt) == ["b1", "b2"]
  let #(_, rt) = spark.answer(rt, a, Ok(Done))
  assert out_lines(rt) == ["b1", "b2", "a1", "a2"]
}

pub fn fs_promises_is_the_same_object_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const fs = require('fs');
      out.push(require('fs/promises') === fs.promises, fs.promises.constants === fs.constants);
      out.push([fs.F_OK, fs.R_OK, fs.W_OK, fs.X_OK].join(''));
      ",
    )
  assert out_lines(rt) == ["true", "true", "0421"]
}

pub fn a_module_waits_on_its_top_level_await_through_the_loop_test() {
  let #(resolve, load) = loader.no_imports()
  let assert Ok(#(engine.ModuleReturned(namespace:, ..), rt)) =
    spark.eval_module(
      node_runtime(),
      "/app/main.js",
      "
      import { setTimeout } from 'node:timers';
      export const waited = await new Promise(resolve => setTimeout(() => resolve('waited'), 5));
      ",
      resolve,
      load,
      run_to_end,
    )
  let eng = spark.engine(rt)
  let assert option.Some(waited) = engine.read_export(eng, namespace, "waited")
  assert engine.inspect(eng, waited) == "'waited'"
}

pub fn process_reports_only_what_it_was_given_test() {
  let rt =
    run_commonjs(
      frozen_runtime(),
      "
      const process = require('process');
      out.push(process.env.HOME, JSON.stringify(process.env.EMPTY), String(process.env.PATH), Object.keys(process.env).join('+'));
      out.push(process.argv.join(' '), process.cwd(), process.platform, process.arch, process.pid);
      out.push(process.version, process.versions.node, process.versions.spark);
      out.push(process.stdout.isTTY, process.stdout.fd, String(process.stderr.isTTY), process.stderr.fd);
      out.push(require('node:process') === process, process.uptime());
      ",
    )
  assert out_lines(rt)
    == [
      "/home/spark", "\"\"", "undefined", "HOME+EMPTY",
      "spark /app/main.cjs --flag", "/work", "linux", "pdp11", "4242", "v99.1.2",
      "99.1.2", "0.1.0", "true", "1", "undefined", "2", "true", "0",
    ]
}

pub fn stdout_and_stderr_writes_go_through_the_now_door_test() {
  let rt = run_commonjs(node_runtime(), describe <> "
        const { stdout, stderr } = require('process');
        out.push(stdout.write('hello'), stderr.write('oops'));
        stdout.write('later', error => out.push('callback ' + error));
        stderr.write('refuse me', 'utf8', error => out.push('callback ' + error.code + ' ' + error.message));
        out.push(describe(() => stderr.write('refuse me')));
        out.push(describe(() => stdout.write(5)));
        ")
  assert notes() == ["stdout hello", "stderr oops", "stdout later"]
  assert out_lines(rt)
    == [
      "true", "true", "Error: EPIPE: unknown error, write code,syscall",
      "TypeError [ERR_INVALID_ARG_TYPE]: The \"chunk\" argument must be of type string or an instance of Buffer, TypedArray, or DataView. Received type number (5) code",
      "callback null", "callback EPIPE EPIPE: unknown error, write",
    ]
}

pub fn ticks_run_before_promise_jobs_like_node_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const { nextTick } = require('process');
      Promise.resolve().then(() => { out.push('p1'); nextTick(() => out.push('t2')); }).then(() => out.push('p2'));
      nextTick((a, b) => out.push('t1' + a + b), 1, 2);
      nextTick(() => { out.push('t3'); nextTick(() => out.push('t4')); Promise.resolve().then(() => out.push('p3')); });
      out.push('sync');
      ",
    )
  assert out(rt) == "'sync,t112,t3,t4,p1,p3,p2,t2'"
}

pub fn ticks_run_between_timers_test() {
  assert out_after(
      "
      const { nextTick } = require('process');
      setTimeout(() => { out.push('timerA'); nextTick(() => out.push('tickA')); Promise.resolve().then(() => out.push('promA')); }, 1);
      setTimeout(() => out.push('timerB'), 1);
      ",
    )
    == "'timerA,tickA,promA,timerB'"
}

pub fn a_tick_keeps_its_async_context_and_a_throw_is_reported_test() {
  let hooks = HostHooks(..host_hooks.default(), report_uncaught: note)
  let rt =
    run_commonjs(
      start_on(engine.new() |> engine.with_host_hooks(hooks), builtins()),
      describe <> "
        const { nextTick } = require('process');
        const { AsyncLocalStorage } = require('async_hooks');
        const store = new AsyncLocalStorage();
        store.run('ctx', () => nextTick(() => out.push('tick sees ' + store.getStore())));
        nextTick(() => { throw new Error('tick boom') });
        nextTick(() => out.push('after the throw'));
        out.push(describe(() => nextTick(5)));
        ",
    )
  assert out_lines(rt)
    == [
      "TypeError [ERR_INVALID_ARG_TYPE]: The \"callback\" argument must be of type function. Received type number (5) code",
      "tick sees ctx", "after the throw",
    ]
  let assert [report] = notes()
  assert string.starts_with(report, "Uncaught Error: tick boom")
}

pub fn os_reports_only_what_it_was_given_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const os = require('os');
      out.push(os.platform(), os.arch(), os.type(), os.release(), os.version(), os.machine());
      out.push(os.hostname(), os.homedir(), os.tmpdir(), os.endianness(), os.availableParallelism());
      out.push(`${os.platform} ${os.homedir} ${os.endianness}`, os.availableParallelism + 1);
      out.push(JSON.stringify(os.EOL), os.devNull);
      out.push(Object.getOwnPropertyDescriptor(os, 'EOL').writable, require('node:os') === os);
      ",
    )
  assert out_lines(rt)
    == [
      "linux", "pdp11", "Linux", "4th edition", "#1 a made up kernel",
      "pdp11/70", "spark-host", "/usr/glenda", "/tmp/made-up", "BE", "3",
      "linux /usr/glenda BE", "4", "\"\\n\"", "/dev/null", "false", "true",
    ]
}

const read_constants = "
  const { constants, type } = require('os');
  const { signals, errno, priority, dlopen } = constants;
  out.push(type(), Object.keys(constants).join());
  out.push([signals.SIGINT, signals.SIGKILL, signals.SIGTERM, signals.SIGUSR1, signals.SIGCHLD].join());
  out.push([signals.SIGINFO, signals.SIGPWR, Object.keys(signals).length].join());
  out.push([errno.ENOENT, errno.EACCES, errno.EAGAIN, errno.ELOOP, errno.ENOTEMPTY, Object.keys(errno).length].join());
  out.push([priority.PRIORITY_LOW, priority.PRIORITY_HIGHEST, dlopen.RTLD_GLOBAL, constants.UV_UDP_REUSEADDR].join());
"

pub fn os_constants_are_linux_numbers_on_linux_test() {
  let rt =
    run_commonjs(
      start_on(engine.new(), [os.builtin(made_up_os(Linux))]),
      read_constants,
    )
  assert out_lines(rt)
    == [
      "Linux", "UV_UDP_REUSEADDR,dlopen,errno,signals,priority", "2,9,15,10,17",
      ",30,33", "2,13,11,40,39,79", "19,-20,256,4",
    ]
}

pub fn os_constants_are_macos_numbers_on_darwin_test() {
  let rt =
    run_commonjs(
      start_on(engine.new(), [os.builtin(made_up_os(Darwin))]),
      read_constants,
    )
  assert out_lines(rt)
    == [
      "Darwin", "UV_UDP_REUSEADDR,dlopen,errno,signals,priority", "2,9,15,30,20",
      "29,,31", "2,13,35,62,66,79", "19,-20,8,4",
    ]
}

pub fn realpath_asks_the_embedder_test() {
  let rt = run_commonjs(node_runtime(), describe <> "
        const fs = require('fs');
        out.push(fs.realpathSync('/link'), fs.realpathSync.native('/link', 'utf8'), fs.realpathSync('/link', { encoding: 'utf-8' }));
        out.push(describe(() => fs.realpathSync('/nope')));
        try { fs.realpathSync('/nope') } catch (e) { out.push([e.errno, e.code, e.syscall, e.path].join()) }
        out.push(describe(() => fs.realpathSync.native(5)));
        out.push(describe(() => fs.realpathSync(5)));
        out.push(describe(() => fs.realpathSync('/link', 'buffer')));
        fs.promises.realpath('/a').then(path => out.push('promise ' + path));
        fs.realpath('/b', (error, path) => out.push('callback ' + error + ' ' + path));
        fs.realpath.native('/c', error => out.push('callback ' + error.message));
        ")
  let path =
    "The \"path\" argument must be of type string or an instance of Buffer or URL. Received "
  assert out_lines(rt)
    == [
      "/real/place",
      "/real/place",
      "/real/place",
      "Error: ENOENT: no such file or directory, realpath '/nope' errno,code,syscall,path",
      "-2,ENOENT,realpath,/nope",
      "TypeError [ERR_INVALID_ARG_TYPE]: " <> path <> "type number (5) code",
      "Error: ENOENT: no such file or directory, realpath '5' errno,code,syscall,path",
      "Error: fs: only the utf8 encoding is supported, got buffer ",
    ]
  let assert [#(a, slip_a), #(b, slip_b), #(c, slip_c)] = kept()
  assert [a, b, c] == [Realpath("/a"), Realpath("/b"), Realpath("/c")]
  let #(_, rt) = spark.answer(rt, slip_a, Ok(Text("/real/a")))
  let #(_, rt) = spark.answer(rt, slip_b, Ok(Text("/real/b")))
  let #(_, rt) = spark.answer(rt, slip_c, Error(Failure("ELOOP")))
  assert list.drop(out_lines(rt), 8)
    == [
      "promise /real/a", "callback null /real/b",
      "callback ELOOP: too many symbolic links encountered, realpath '/c'",
    ]
}

pub fn global_is_the_global_object_only_when_asked_for_test() {
  let with = start_on(engine.new(), [global.builtin()])
  assert run(with, "global === globalThis && global.global === global").0
    == "true"
  let without = start_on(engine.new(), [])
  assert run(without, "typeof global").0 == "'undefined'"
}

const hex = "
  const hex = bytes => Array.from(bytes).map(byte => byte.toString(16)).join(' ');
"

pub fn text_encoder_gives_utf8_bytes_test() {
  let rt = run_commonjs(node_runtime(), hex <> describe <> "
        const encoder = new TextEncoder();
        out.push(encoder.encoding, hex(encoder.encode('a\\u20ac')), encoder.encode().length, encoder.encode(12).length);
        out.push(encoder.encode('x') instanceof Uint8Array);
        const small = new Uint8Array(5);
        out.push(JSON.stringify(encoder.encodeInto('a\\u20ac\\u{1f600}', small)), hex(small));
        const roomy = new Uint8Array(10);
        out.push(encoder.encodeInto('a\\u20ac\\u{1f600}', roomy).written, hex(roomy));
        out.push(describe(() => encoder.encodeInto('x', [])));
        out.push(describe(() => encoder.encodeInto(5, small)));
        out.push(describe(() => TextEncoder.prototype.encode.call({}, 'x')));
        ")
  assert out_lines(rt)
    == [
      "utf-8", "61 e2 82 ac", "0", "2", "true", "{\"read\":2,\"written\":4}",
      "61 e2 82 ac 0", "8", "61 e2 82 ac f0 9f 98 80 0 0",
      "TypeError [ERR_INVALID_ARG_TYPE]: The \"dest\" argument must be an instance of Uint8Array. Received an instance of Array code",
      "TypeError [ERR_INVALID_ARG_TYPE]: The \"src\" argument must be of type string. Received type number (5) code",
      "TypeError: Cannot read private member #encode from an object whose class did not declare it ",
    ]
}

pub fn text_decoder_replaces_bad_bytes_like_node_test() {
  let rt = run_commonjs(node_runtime(), describe <> "
        const show = text => JSON.stringify(text);
        const decoder = new TextDecoder();
        out.push(decoder.encoding, decoder.fatal, decoder.ignoreBOM, new TextDecoder(' UTF8 ').encoding);
        out.push(show(decoder.decode(new Uint8Array([0x61, 0xff, 0xe2, 0x82, 0x62, 0xf0, 0x9f, 0x98, 0xc3]))));
        out.push(show(decoder.decode(new Uint8Array([0xed, 0xa0, 0x80, 0xc0, 0xaf, 0xf4, 0x90]))));
        out.push(show(decoder.decode(new Uint8Array([0xef, 0xbb, 0xbf, 0x61]))));
        out.push(show(new TextDecoder('utf8', { ignoreBOM: true }).decode(new Uint8Array([0xef, 0xbb, 0xbf, 0x61]))));
        out.push(show(decoder.decode()), show(decoder.decode(new ArrayBuffer(2))));
        out.push(show(decoder.decode(new DataView(new Uint8Array([104, 105]).buffer))));
        out.push(show(decoder.decode(new Uint16Array([0x6968]))));
        out.push(show(decoder.decode(new Uint8Array([120, 104, 105, 121]).subarray(1, 3))));
        out.push(describe(() => new TextDecoder('utf-8', { fatal: true }).decode(new Uint8Array([0xff]))));
        out.push(describe(() => new TextDecoder('bogus')));
        out.push(describe(() => decoder.decode(5)));
        out.push(describe(() => TextDecoder.prototype.decode.call({})));
        ")
  assert out_lines(rt)
    == [
      "utf-8", "false", "false", "utf-8",
      "\"a\u{fffd}\u{fffd}b\u{fffd}\u{fffd}\"",
      "\"\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\"", "\"a\"",
      "\"\u{feff}a\"", "\"\"", "\"\\u0000\\u0000\"", "\"hi\"", "\"hi\"",
      "\"hi\"",
      "TypeError [ERR_ENCODING_INVALID_ENCODED_DATA]: The encoded data was not valid for encoding utf-8 code",
      "RangeError [ERR_ENCODING_NOT_SUPPORTED]: The \"bogus\" encoding is not supported code",
      "TypeError [ERR_INVALID_ARG_TYPE]: The \"input\" argument must be an instance of ArrayBuffer or ArrayBufferView. Received type number (5) code",
      "TypeError [ERR_INVALID_THIS]: Value of \"this\" must be of type TextDecoder code",
    ]
}

pub fn text_decoder_streams_across_chunks_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const show = text => JSON.stringify(text);
      const decoder = new TextDecoder();
      out.push(show(decoder.decode(new Uint8Array([0xe2, 0x82]), { stream: true })));
      out.push(show(decoder.decode(new Uint8Array([0xac]), { stream: true })));
      out.push(show(decoder.decode(new Uint8Array([0xe2]), { stream: true })));
      out.push(show(decoder.decode()));
      const bom = new TextDecoder();
      out.push(show(bom.decode(new Uint8Array([0xef, 0xbb]), { stream: true }) + bom.decode(new Uint8Array([0xbf, 0x68]), { stream: true }) + bom.decode(new Uint8Array([0x69]))));
      ",
    )
  assert out_lines(rt)
    == ["\"\"", "\"\u{20ac}\"", "\"\"", "\"\u{fffd}\"", "\"hi\""]
}

pub fn encoding_globals_exist_only_when_asked_for_test() {
  let without = start_on(engine.new(), [])
  assert run(without, "typeof TextEncoder + typeof TextDecoder").0
    == "'undefinedundefined'"
}

pub fn performance_reads_the_engine_clocks_test() {
  let hooks =
    HostHooks(
      ..host_hooks.default(),
      monotonic_now: fn() { 5000 },
      wall_clock_ms: fn() { 1_700_000_000_000 },
    )
  let rt =
    run_commonjs(
      start_on(engine.new() |> engine.with_host_hooks(hooks), builtins()),
      describe <> "
        const hooks = require('perf_hooks');
        out.push(hooks.performance === performance, performance.now(), performance.timeOrigin);
        out.push(JSON.stringify(performance), Object.prototype.toString.call(performance));
        out.push(describe(() => new performance.constructor()));
        ",
    )
  assert out_lines(rt)
    == [
      "true", "0", "1700000000000", "{\"timeOrigin\":1700000000000}",
      "[object Performance]",
      "TypeError [ERR_ILLEGAL_CONSTRUCTOR]: Illegal constructor code",
    ]
}

pub fn performance_now_moves_with_real_time_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const before = performance.now();
      setTimeout(() => out.push(performance.now() - before >= 20), 25);
      ",
    )
  assert out(run_to_end(rt)) == "'true'"
}

fn compiled_runtime(builtins: List(Builtin(Nil))) -> Runtime(Nil) {
  let assert Ok(compiled) = spark.compile(engine.new())
  let assert Ok(rt) = spark.start_compiled(compiled, builtins, fake_io())
  rt
}

const tour = "
  const os = require('os');
  const fs = require('fs');
  out.push(typeof setTimeout, os.platform(), fs.existsSync('/there'), fs.existsSync('/nope'));
  out.push(new TextEncoder().encode('a\\u20ac').length, require('fs/promises') === fs.promises);
  out.push(require('process').argv.length, typeof performance.now());
  setTimeout(() => out.push('timer'), 1);
  require('process').nextTick(() => out.push('tick'));
"

pub fn a_runtime_from_compiled_js_behaves_like_a_started_one_test() {
  let started = out_lines(run_to_end(run_commonjs(node_runtime(), tour)))
  let from_compiled =
    out_lines(run_to_end(run_commonjs(compiled_runtime(builtins()), tour)))
  assert from_compiled == started
  assert list.length(started) == 10
}

pub fn compiling_installs_nothing_test() {
  let rt = compiled_runtime([])
  assert run(rt, "typeof setTimeout + typeof TextEncoder + typeof performance").0
    == "'undefinedundefinedundefined'"
  let #(found, _) =
    run(rt, "try { require('fs') } catch (error) { error.name }")
  assert found == "'ReferenceError'"
}

pub fn runtimes_from_one_compiled_engine_share_no_state_test() {
  let assert Ok(compiled) = spark.compile(engine.new())
  let assert Ok(first) = spark.start_compiled(compiled, builtins(), fake_io())
  let assert Ok(second) = spark.start_compiled(compiled, builtins(), fake_io())
  let first =
    run_commonjs(
      first,
      "globalThis.mark = 1; require('process').env.MARK = 'first'; require('fs').mark = 1;",
    )
  let second =
    run_commonjs(
      second,
      "out.push(typeof mark, String(require('process').env.MARK), typeof require('fs').mark);",
    )
  assert out_lines(second) == ["undefined", "undefined", "undefined"]
  assert run(first, "mark").0 == "1"
}

pub fn a_builtin_with_its_own_source_works_from_compiled_js_test() {
  let rt =
    compiled_runtime([
      timers.builtin(),
      script_builtin(
        "test:greeting",
        "module.exports = { hello: 'hi', timer: typeof require('node:timers').setTimeout };",
        ["node:timers"],
      ),
    ])
  let rt =
    run_commonjs(
      rt,
      "const greeting = require('test:greeting'); out.push(greeting.hello, greeting.timer);",
    )
  assert out_lines(rt) == ["hi", "function"]
}

// the three scripts below were also run by node 26.11.1 itself, and every
// expected line is what node printed

const dom_exception_script = "
const show = (label, run) => { try { out.push(label + \": \" + run()); } catch (error) { out.push(label + \" threw \" + error.name + \" \" + error.code + \" \" + JSON.stringify(error.message) + \" | \" + String(error)); } };
const e = new DOMException(\"boom\", \"AbortError\");
show(\"basics\", () => [e.name, e.message, e.code, e instanceof Error, e instanceof DOMException, Object.prototype.toString.call(e), String(e), typeof e.stack].join(\" | \"));
show(\"constants\", () => [DOMException.ABORT_ERR, e.ABORT_ERR, DOMException.DATA_CLONE_ERR, DOMException.INDEX_SIZE_ERR, Object.keys(DOMException).length, Object.keys(DOMException.prototype).join()].join(\" | \"));
show(\"defaults\", () => { const d = new DOMException(); return [JSON.stringify(d.name), JSON.stringify(d.message), d.code].join(\" | \"); });
show(\"options object\", () => { const d = new DOMException(\"m\", { name: \"TimeoutError\", cause: 5 }); return [d.name, d.code, d.cause, Object.getOwnPropertyDescriptor(d, \"cause\").enumerable].join(\" | \"); });
show(\"no cause key\", () => \"cause\" in new DOMException(\"m\", { name: \"X\" }));
show(\"codes\", () => [\"ValidationError\", \"Nope\", \"SyntaxError\", \"DataCloneError\", \"DOMStringSizeError\", \"QuotaExceededError\"].map((n) => new DOMException(\"x\", n).code).join());
show(\"coercion\", () => { const d = new DOMException(12, 34); return [d.message, d.name, typeof d.message].join(\" | \"); });
show(\"name getter on wrong this\", () => Object.getOwnPropertyDescriptor(DOMException.prototype, \"name\").get.call({}));
show(\"code getter on wrong this\", () => Object.getOwnPropertyDescriptor(DOMException.prototype, \"code\").get.call(new Error(\"x\")));
show(\"descriptors\", () => [\"name\", \"message\", \"code\"].map((k) => { const d = Object.getOwnPropertyDescriptor(DOMException.prototype, k); return k + \":\" + d.enumerable + d.configurable + typeof d.get + typeof d.set; }).join());
show(\"constant is frozen\", () => { \"use strict\"; const d = Object.getOwnPropertyDescriptor(DOMException, \"ABORT_ERR\"); return [d.writable, d.enumerable, d.configurable].join(); });
show(\"subclass\", () => { class Mine extends DOMException { constructor() { super(\"mine\", \"NotFoundError\"); } } const m = new Mine(); return [m instanceof Mine, m instanceof DOMException, m.name, m.code, m.constructor === Mine].join(); });
show(\"proto chain\", () => Object.getPrototypeOf(DOMException.prototype) === Error.prototype);
show(\"length and name\", () => [DOMException.length, DOMException.name].join());
show(\"call without new\", () => DOMException(\"x\"));
show(\"quota basics\", () => { const q = new QuotaExceededError(\"full\", { quota: 5, requested: 10 }); return [typeof QuotaExceededError, q.name, q.message, q.code, q.quota, q.requested, q instanceof DOMException, q instanceof Error, Object.prototype.toString.call(q), String(q)].join(\" | \"); });
show(\"quota defaults\", () => { const q = new QuotaExceededError(); return [JSON.stringify(q.message), q.quota, q.requested, new QuotaExceededError(\"m\", null).quota, new QuotaExceededError(\"m\", { quota: \"7\" }).quota].join(\" | \"); });
show(\"quota not a number\", () => new QuotaExceededError(\"m\", { quota: \"x\" }));
show(\"quota infinite\", () => new QuotaExceededError(\"m\", { requested: Infinity }));
show(\"quota negative\", () => new QuotaExceededError(\"m\", { quota: -1 }));
show(\"requested negative\", () => new QuotaExceededError(\"m\", { requested: -1 }));
show(\"requested under quota\", () => new QuotaExceededError(\"m\", { quota: 5, requested: 2 }));
show(\"quota getter on wrong this\", () => Object.getOwnPropertyDescriptor(QuotaExceededError.prototype, \"quota\").get.call(new DOMException(\"x\")));
show(\"quota keys\", () => { const keys = []; for (const key in new QuotaExceededError()) keys.push(key); return keys.filter((k) => !/_ERR$/.test(k)).sort().join(); });
show(\"quota length\", () => [QuotaExceededError.length, QuotaExceededError.name, Object.getPrototypeOf(QuotaExceededError) === DOMException].join());
"

pub fn dom_exception_matches_node_test() {
  let rt = run_commonjs(node_runtime(), dom_exception_script)
  assert out_lines(rt)
    == [
      "basics: AbortError | boom | 20 | true | true | [object DOMException] | AbortError: boom | string",
      "constants: 20 | 20 | 25 | 1 | 25 | name,message,code,INDEX_SIZE_ERR,DOMSTRING_SIZE_ERR,HIERARCHY_REQUEST_ERR,WRONG_DOCUMENT_ERR,INVALID_CHARACTER_ERR,NO_DATA_ALLOWED_ERR,NO_MODIFICATION_ALLOWED_ERR,NOT_FOUND_ERR,NOT_SUPPORTED_ERR,INUSE_ATTRIBUTE_ERR,INVALID_STATE_ERR,SYNTAX_ERR,INVALID_MODIFICATION_ERR,NAMESPACE_ERR,INVALID_ACCESS_ERR,VALIDATION_ERR,TYPE_MISMATCH_ERR,SECURITY_ERR,NETWORK_ERR,ABORT_ERR,URL_MISMATCH_ERR,QUOTA_EXCEEDED_ERR,TIMEOUT_ERR,INVALID_NODE_TYPE_ERR,DATA_CLONE_ERR",
      "defaults: \"Error\" | \"\" | 0",
      "options object: TimeoutError | 23 | 5 | false",
      "no cause key: false",
      "codes: 0,0,12,25,0,22",
      "coercion: 12 | 34 | string",
      "name getter on wrong this threw TypeError ERR_INVALID_THIS \"Value of \\\"this\\\" must be of DOMException\" | TypeError [ERR_INVALID_THIS]: Value of \"this\" must be of DOMException",
      "code getter on wrong this threw TypeError ERR_INVALID_THIS \"Value of \\\"this\\\" must be of DOMException\" | TypeError [ERR_INVALID_THIS]: Value of \"this\" must be of DOMException",
      "descriptors: name:truetruefunctionundefined,message:truetruefunctionundefined,code:truetruefunctionundefined",
      "constant is frozen: false,true,false",
      "subclass: true,true,NotFoundError,8,true",
      "proto chain: true",
      "length and name: 0,DOMException",
      "call without new threw TypeError undefined \"Class constructor DOMException cannot be invoked without 'new'\" | TypeError: Class constructor DOMException cannot be invoked without 'new'",
      "quota basics: function | QuotaExceededError | full | 22 | 5 | 10 | true | true | [object QuotaExceededError] | QuotaExceededError: full",
      "quota defaults: \"\" |  |  |  | 7",
      "quota not a number threw TypeError undefined \"Cannot convert options.quota to a double: the value is NaN\" | TypeError: Cannot convert options.quota to a double: the value is NaN",
      "quota infinite threw TypeError undefined \"Cannot convert options.requested to a double: the value is Infinity\" | TypeError: Cannot convert options.requested to a double: the value is Infinity",
      "quota negative threw RangeError undefined \"options.quota must not be negative\" | RangeError: options.quota must not be negative",
      "requested negative threw RangeError undefined \"options.requested must not be negative\" | RangeError: options.requested must not be negative",
      "requested under quota threw RangeError undefined \"options.requested must not be less than options.quota\" | RangeError: options.requested must not be less than options.quota",
      "quota getter on wrong this threw TypeError ERR_INVALID_THIS \"Value of \\\"this\\\" must be of QuotaExceededError\" | TypeError [ERR_INVALID_THIS]: Value of \"this\" must be of QuotaExceededError",
      "quota keys: code,message,name,quota,requested",
      "quota length: 0,QuotaExceededError,true",
    ]
}

const dom_events_script = "
const show = (label, run) => { try { out.push(label + \": \" + run()); } catch (error) { out.push(label + \" threw \" + error.name + \" \" + error.code + \" \" + JSON.stringify(error.message)); } };

// ---- Event
show(\"event basics\", () => { const e = new Event(\"ping\", { cancelable: true, bubbles: true }); return [e.type, e.bubbles, e.cancelable, e.composed, e.defaultPrevented, e.target, e.currentTarget, e.eventPhase, e.isTrusted, typeof e.timeStamp, e.returnValue, e.cancelBubble, JSON.stringify(e.composedPath())].join(\" | \"); });
show(\"event tags\", () => [Object.prototype.toString.call(new Event(\"x\")), Event.NONE, Event.AT_TARGET, Event.BUBBLING_PHASE, Object.keys(Event).join(), new Event(5).type].join(\" | \"));
show(\"event keys\", () => { const keys = []; for (const key in new Event(\"x\")) keys.push(key); return keys.sort().join(); });
show(\"preventDefault\", () => { const a = new Event(\"x\"); a.preventDefault(); const b = new Event(\"x\", { cancelable: true }); b.preventDefault(); return [a.defaultPrevented, b.defaultPrevented, b.returnValue].join(); });
show(\"no type\", () => new Event());
show(\"bad options\", () => new Event(\"x\", 5));
show(\"getter on wrong this\", () => Object.getOwnPropertyDescriptor(Event.prototype, \"type\").get.call({}));
show(\"custom event\", () => { const c = new CustomEvent(\"c\", { detail: { n: 1 } }); return [c.type, c.detail.n, new CustomEvent(\"d\").detail, c instanceof Event, Object.prototype.toString.call(c)].join(\" | \"); });

// ---- EventTarget
show(\"dispatch order and this\", () => {
  const target = new EventTarget(); const log = [];
  target.addEventListener(\"go\", function (e) { log.push(\"a:\" + (this === target) + \":\" + (e.target === target) + \":\" + (e.currentTarget === target) + \":\" + e.eventPhase); });
  target.addEventListener(\"go\", { handleEvent(e) { log.push(\"b:\" + (this !== target)); } });
  const e = new Event(\"go\"); const result = target.dispatchEvent(e);
  return [log.join(\",\"), result, e.target === target, e.currentTarget, e.eventPhase].join(\" | \");
});
show(\"once, duplicate, remove\", () => {
  const target = new EventTarget(); let n = 0; const inc = () => n++;
  target.addEventListener(\"x\", inc); target.addEventListener(\"x\", inc); target.addEventListener(\"x\", inc, { once: true });
  target.addEventListener(\"x\", inc, true); target.addEventListener(\"x\", () => n += 10, { once: true });
  target.dispatchEvent(new Event(\"x\")); const first = n;
  target.dispatchEvent(new Event(\"x\")); const second = n;
  target.removeEventListener(\"x\", inc); target.dispatchEvent(new Event(\"x\")); const third = n;
  target.removeEventListener(\"x\", inc, { capture: true }); target.dispatchEvent(new Event(\"x\"));
  return [first, second, third, n].join();
});
show(\"stopImmediatePropagation\", () => { const t = new EventTarget(); const log = []; t.addEventListener(\"x\", (e) => { log.push(1); e.stopImmediatePropagation(); }); t.addEventListener(\"x\", () => log.push(2)); t.dispatchEvent(new Event(\"x\")); return log.join(); });
show(\"cancel result\", () => { const t = new EventTarget(); t.addEventListener(\"x\", (e) => e.preventDefault()); return [t.dispatchEvent(new Event(\"x\", { cancelable: true })), t.dispatchEvent(new Event(\"x\"))].join(); });
show(\"passive ignores preventDefault\", () => { const t = new EventTarget(); t.addEventListener(\"x\", (e) => e.preventDefault(), { passive: true }); return t.dispatchEvent(new Event(\"x\", { cancelable: true })); });
show(\"null listener\", () => { const t = new EventTarget(); t.addEventListener(\"x\", null); t.removeEventListener(\"x\", null); return t.dispatchEvent(new Event(\"x\")); });
show(\"bad listener\", () => new EventTarget().addEventListener(\"x\", 5));
show(\"missing listener\", () => new EventTarget().addEventListener(\"x\"));
show(\"dispatch non-event\", () => new EventTarget().dispatchEvent({ type: \"x\" }));
show(\"dispatch nothing\", () => new EventTarget().dispatchEvent());
show(\"redispatch during dispatch\", () => { const t = new EventTarget(); let seen; t.addEventListener(\"x\", (e) => { try { t.dispatchEvent(e); } catch (error) { seen = error.name + \" \" + error.code + \" \" + error.message; } }); t.dispatchEvent(new Event(\"x\")); return seen; });
show(\"wrong this\", () => EventTarget.prototype.addEventListener.call({}, \"x\", () => {}));
show(\"target tags\", () => [Object.prototype.toString.call(new EventTarget()), Object.keys(EventTarget.prototype).join()].join(\" | \"));
show(\"listener added during dispatch runs\", () => { const t = new EventTarget(); const log = []; t.addEventListener(\"x\", () => { log.push(1); t.addEventListener(\"x\", () => log.push(2)); }); t.dispatchEvent(new Event(\"x\")); return log.join(); });
show(\"signal option\", () => { const t = new EventTarget(); const c = new AbortController(); let n = 0; t.addEventListener(\"x\", () => n++, { signal: c.signal }); t.dispatchEvent(new Event(\"x\")); c.abort(); t.dispatchEvent(new Event(\"x\")); const d = new AbortController(); d.abort(); t.addEventListener(\"x\", () => n += 100, { signal: d.signal }); t.dispatchEvent(new Event(\"x\")); return n; });
show(\"bad signal option\", () => new EventTarget().addEventListener(\"x\", () => {}, { signal: 5 }));
show(\"subclass\", () => { class Bus extends EventTarget {} const b = new Bus(); let got; b.addEventListener(\"m\", (e) => got = e.detail); b.dispatchEvent(new CustomEvent(\"m\", { detail: 7 })); return got; });

// ---- AbortController / AbortSignal
show(\"controller basics\", () => { const c = new AbortController(); const s = c.signal; const before = [s.aborted, s.reason, s === c.signal].join(); const log = []; s.addEventListener(\"abort\", (e) => log.push(\"listener:\" + e.type + \":\" + e.isTrusted + \":\" + (e.target === s))); s.onabort = (e) => log.push(\"onabort:\" + (e.target === s)); c.abort(); c.abort(); return [before, s.aborted, s.reason.name, s.reason.message, s.reason.code, s.reason instanceof DOMException, log.join(\",\")].join(\" | \"); });
show(\"custom reason\", () => { const c = new AbortController(); c.abort(\"why\"); let thrown; try { c.signal.throwIfAborted(); } catch (e) { thrown = e; } return [c.signal.reason, thrown].join(); });
show(\"throwIfAborted when fine\", () => new AbortController().signal.throwIfAborted());
show(\"onabort slot\", () => { const s = new AbortController().signal; const before = s.onabort; const f = () => {}; s.onabort = f; const same = s.onabort === f; s.onabort = null; return [before, same, s.onabort].join(); });
show(\"signal constructor\", () => new AbortSignal());
show(\"AbortSignal.abort\", () => { const s = AbortSignal.abort(); const t = AbortSignal.abort(42); return [s.aborted, s.reason.name, t.reason].join(); });
show(\"tags\", () => [Object.prototype.toString.call(new AbortController()), Object.prototype.toString.call(AbortSignal.abort()), AbortSignal.abort() instanceof EventTarget].join(\" | \"));
show(\"timeout bad delay\", () => AbortSignal.timeout(-1));
show(\"timeout not a number\", () => AbortSignal.timeout(\"5\"));
show(\"timeout not an integer\", () => AbortSignal.timeout(1.5));
show(\"any of nothing\", () => AbortSignal.any([]).aborted);
show(\"any already aborted\", () => { const s = AbortSignal.any([new AbortController().signal, AbortSignal.abort(\"first\")]); return [s.aborted, s.reason].join(); });
show(\"any follows\", () => { const a = new AbortController(); const b = new AbortController(); const s = AbortSignal.any([a.signal, b.signal]); const log = []; s.addEventListener(\"abort\", () => log.push(\"fired:\" + s.reason)); b.abort(\"b\"); a.abort(\"a\"); return [s.aborted, s.reason, log.join()].join(\" | \"); });
show(\"any read without listener\", () => { const a = new AbortController(); const s = AbortSignal.any([a.signal]); a.abort(\"late\"); return [s.aborted, s.reason].join(); });
show(\"any of any\", () => { const a = new AbortController(); const inner = AbortSignal.any([a.signal]); const outer = AbortSignal.any([inner]); let fired = 0; outer.addEventListener(\"abort\", () => fired++); a.abort(\"deep\"); return [outer.aborted, outer.reason, fired].join(); });
show(\"any bad input\", () => AbortSignal.any(5));
show(\"any bad member\", () => AbortSignal.any([5]));
show(\"remove with a boolean capture\", () => { const t = new EventTarget(); let n = 0; const f = () => n++; t.addEventListener(\"x\", f, true); t.removeEventListener(\"x\", f, true); t.dispatchEvent(new Event(\"x\")); t.addEventListener(\"x\", f, { capture: true }); t.removeEventListener(\"x\", f, false); t.dispatchEvent(new Event(\"x\")); return n; });
show(\"onabort takes only functions and objects\", () => { const s = new AbortController().signal; s.onabort = 5; const a = s.onabort; s.onabort = \"x\"; const b = s.onabort; const o = {}; s.onabort = o; const c = s.onabort === o; s.onabort = undefined; return [a, b, c, s.onabort].join(); });
show(\"any wants real signals\", () => AbortSignal.any([Object.create(AbortSignal.prototype)]));
show(\"any of an already aborted any\", () => { const a = new AbortController(); const inner = AbortSignal.any([a.signal]); a.abort(\"gone\"); const outer = AbortSignal.any([inner]); return [outer.aborted, outer.reason].join(); });
show(\"any keeps the first reason\", () => { const a = new AbortController(); const b = new AbortController(); const s = AbortSignal.any([a.signal, b.signal]); a.abort(\"a first\"); b.abort(\"b second\"); return s.reason; });
show(\"aborted getter on wrong this\", () => Object.getOwnPropertyDescriptor(AbortSignal.prototype, \"aborted\").get.call({}));
show(\"listener error does not stop others\", () => { const t = new EventTarget(); const log = []; t.addEventListener(\"x\", () => log.push(1)); t.addEventListener(\"x\", () => log.push(2)); t.dispatchEvent(new Event(\"x\")); return log.join(); });

const timed = AbortSignal.timeout(20);
const timedLog = [];
timed.addEventListener(\"abort\", () => timedLog.push(timed.reason.name + \":\" + timed.reason.message + \":\" + timed.reason.code));
setTimeout(() => { out.push(\"timeout signal: \" + [timed.aborted, timedLog.join()].join(\" | \")); }, 120);
"

pub fn dom_events_match_node_test() {
  let rt = run_to_end(run_commonjs(node_runtime(), dom_events_script))
  assert out_lines(rt)
    == [
      "event basics: ping | true | true | false | false |  |  | 0 | false | number | true | false | []",
      "event tags: [object Event] | 0 | 2 | 3 | NONE,CAPTURING_PHASE,AT_TARGET,BUBBLING_PHASE | 5",
      "event keys: bubbles,cancelBubble,cancelable,composed,composedPath,currentTarget,defaultPrevented,eventPhase,initEvent,isTrusted,preventDefault,returnValue,srcElement,stopImmediatePropagation,stopPropagation,target,timeStamp,type",
      "preventDefault: false,true,false",
      "no type threw TypeError ERR_MISSING_ARGS \"The \\\"type\\\" argument must be specified\"",
      "bad options threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"options\\\" argument must be of type object. Received type number (5)\"",
      "getter on wrong this threw TypeError ERR_INVALID_THIS \"Value of \\\"this\\\" must be of type Event\"",
      "custom event: c | 1 |  | true | [object CustomEvent]",
      "dispatch order and this: a:true:true:true:2,b:true | true | true |  | 0",
      "once, duplicate, remove: 12,14,15,15",
      "stopImmediatePropagation: 1",
      "cancel result: false,true",
      "passive ignores preventDefault: true",
      "null listener: true",
      "bad listener threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"listener\\\" argument must be an instance of EventListener. Received type number (5)\"",
      "missing listener threw TypeError ERR_MISSING_ARGS \"The \\\"type\\\" and \\\"listener\\\" arguments must be specified\"",
      "dispatch non-event threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"event\\\" argument must be an instance of Event. Received an instance of Object\"",
      "dispatch nothing threw TypeError ERR_MISSING_ARGS \"The \\\"event\\\" argument must be specified\"",
      "redispatch during dispatch: Error ERR_EVENT_RECURSION The event \"x\" is already being dispatched",
      "wrong this threw TypeError ERR_INVALID_THIS \"Value of \\\"this\\\" must be of type EventTarget\"",
      "target tags: [object EventTarget] | addEventListener,removeEventListener,dispatchEvent",
      "listener added during dispatch runs: 1",
      "signal option: 1",
      "bad signal option threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"options.signal\\\" property must be an instance of AbortSignal. Received type number (5)\"",
      "subclass: 7",
      "controller basics: false,,true | true | AbortError | This operation was aborted | 20 | true | listener:abort:true:true,onabort:true",
      "custom reason: why,why",
      "throwIfAborted when fine: undefined",
      "onabort slot: ,true,",
      "signal constructor threw TypeError ERR_ILLEGAL_CONSTRUCTOR \"Illegal constructor\"",
      "AbortSignal.abort: true,AbortError,42",
      "tags: [object AbortController] | [object AbortSignal] | true",
      "timeout bad delay threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"delay\\\" is out of range. It must be >= 0 && <= 4294967295. Received -1\"",
      "timeout not a number threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"delay\\\" argument must be of type number. Received type string ('5')\"",
      "timeout not an integer threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"delay\\\" is out of range. It must be an integer. Received 1.5\"",
      "any of nothing: false",
      "any already aborted: true,first",
      "any follows: true | b | fired:b",
      "any read without listener: true,late",
      "any of any: true,deep,1",
      "any bad input threw TypeError ERR_INVALID_ARG_TYPE \"signals cannot be converted to sequence.\"",
      "any bad member threw TypeError ERR_INVALID_ARG_TYPE \"signals[0] is not of type AbortSignal.\"",
      "remove with a boolean capture: 1",
      "onabort takes only functions and objects: ,,true,",
      "any wants real signals threw TypeError ERR_INVALID_ARG_TYPE \"signals[0] is not of type AbortSignal.\"",
      "any of an already aborted any: true,gone",
      "any keeps the first reason: a first",
      "aborted getter on wrong this threw TypeError ERR_INVALID_THIS \"Value of \\\"this\\\" must be of type AbortSignal\"",
      "listener error does not stop others: 1,2",
      "timeout signal: true | TimeoutError:The operation was aborted due to timeout:23",
    ]
}

const marks_and_measures_script = "
const show = (label, run) => { try { out.push(label + \": \" + run()); } catch (error) { out.push(label + \" threw \" + error.name + \" \" + error.code + \" \" + JSON.stringify(error.message)); } };
const { performance: perf, PerformanceEntry, PerformanceMark, PerformanceMeasure } = require(\"perf_hooks\");
perf.clearMarks(); perf.clearMeasures();
show(\"same object\", () => perf === performance);
show(\"mark with time\", () => { const m = perf.mark(\"a\", { startTime: 10 }); return [m.name, m.entryType, m.startTime, m.duration, m.detail, m instanceof PerformanceMark, m instanceof PerformanceEntry, Object.prototype.toString.call(m), JSON.stringify(m)].join(\" | \"); });
show(\"mark now\", () => { const m = perf.mark(\"b\"); return [typeof m.startTime, m.startTime >= 0, m.duration].join(); });
show(\"mark coerces name\", () => perf.mark(5, { startTime: 20 }).name === \"5\");
show(\"mark needs a name\", () => perf.mark());
show(\"mark reserved name\", () => perf.mark(\"nodeStart\"));
show(\"mark bad options\", () => perf.mark(\"x\", 5));
show(\"mark bad time\", () => perf.mark(\"x\", { startTime: \"1\" }));
show(\"mark negative time\", () => perf.mark(\"x\", { startTime: -1 }));
show(\"measure between marks\", () => { perf.mark(\"s\", { startTime: 100 }); perf.mark(\"e\", { startTime: 250 }); const m = perf.measure(\"span\", \"s\", \"e\"); return [m.name, m.entryType, m.startTime, m.duration, m.detail, m instanceof PerformanceMeasure, Object.prototype.toString.call(m), JSON.stringify(m)].join(\" | \"); });
show(\"measure with options\", () => { const a = perf.measure(\"o1\", { start: 5, end: 30 }); const b = perf.measure(\"o2\", { start: 5, duration: 7 }); const c = perf.measure(\"o3\", { end: 50, duration: 20 }); const d = perf.measure(\"o4\", { start: \"s\", end: \"e\" }); return [a.startTime, a.duration, b.startTime, b.duration, c.startTime, c.duration, d.startTime, d.duration].join(); });
show(\"measure from zero\", () => { const m = perf.measure(\"z\", undefined, \"e\"); return [m.startTime, m.duration].join(); });
show(\"measure unknown mark\", () => { try { perf.measure(\"bad\", \"nope\"); } catch (e) { return [e.name, e.code, e.message, e instanceof DOMException].join(\" | \"); } });
show(\"measure name type\", () => perf.measure(5));
show(\"measure no name\", () => perf.measure());
show(\"measure options and endMark\", () => perf.measure(\"x\", { start: 1 }, \"e\"));
show(\"measure all three\", () => perf.measure(\"x\", { start: 1, end: 2, duration: 1 }));
show(\"measure negative number\", () => perf.measure(\"x\", { start: -5, end: 2 }));
show(\"entries by type\", () => [perf.getEntriesByType(\"mark\").filter((e) => e.name !== \"b\").map((e) => e.name).join(), perf.getEntriesByType(\"measure\").map((e) => e.name).join(), perf.getEntriesByType(\"nope\").length].join(\" | \"));
show(\"entries by name\", () => [perf.getEntriesByName(\"s\").length, perf.getEntriesByName(\"s\", \"measure\").length, perf.getEntriesByName(\"span\")[0].duration].join());
show(\"entries sorted\", () => perf.getEntries().filter((e) => e.name !== \"b\").map((e) => e.name + \"@\" + e.startTime).join());
show(\"by type needs arg\", () => perf.getEntriesByType());
show(\"by name needs arg\", () => perf.getEntriesByName());
show(\"clear one mark\", () => { perf.clearMarks(\"s\"); let after; try { perf.measure(\"m2\", \"s\"); after = \"still set\"; } catch (e) { after = e.name; } return [perf.getEntriesByName(\"s\").length, after].join(); });
show(\"clear measures\", () => { perf.clearMeasures(\"o1\"); const left = perf.getEntriesByType(\"measure\").length; perf.clearMeasures(); return [left, perf.getEntriesByType(\"measure\").length].join(); });
show(\"clear all marks\", () => { perf.clearMarks(); return perf.getEntriesByType(\"mark\").length; });
show(\"clear reserved\", () => perf.clearMarks(\"nodeStart\"));
show(\"construct entry\", () => new PerformanceEntry());
show(\"construct measure\", () => new PerformanceMeasure());
show(\"construct mark\", () => { const m = new PerformanceMark(\"made\", { startTime: 3 }); return [m.name, m.startTime, perf.getEntriesByName(\"made\").length].join(); });
show(\"entry keys\", () => { const keys = []; for (const key in perf.mark(\"k\", { startTime: 1 })) keys.push(key); return keys.sort().join(); });
show(\"wrong this\", () => perf.mark.call({}, \"x\"));
show(\"entry getter wrong this\", () => Object.getOwnPropertyDescriptor(PerformanceEntry.prototype, \"name\").get.call({}));
"

pub fn performance_marks_and_measures_match_node_test() {
  let rt = run_commonjs(node_runtime(), marks_and_measures_script)
  assert out_lines(rt)
    == [
      "same object: true",
      "mark with time: a | mark | 10 | 0 |  | true | true | [object PerformanceMark] | {\"name\":\"a\",\"entryType\":\"mark\",\"startTime\":10,\"duration\":0,\"detail\":null}",
      "mark now: number,true,0",
      "mark coerces name: true",
      "mark needs a name threw TypeError ERR_MISSING_ARGS \"The \\\"name\\\" argument must be specified\"",
      "mark reserved name threw TypeError ERR_INVALID_ARG_VALUE \"The argument 'name' is invalid. Received 'nodeStart'\"",
      "mark bad options threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"options\\\" argument must be of type object. Received type number (5)\"",
      "mark bad time threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"startTime\\\" argument must be of type number. Received type string ('1')\"",
      "mark negative time threw TypeError ERR_PERFORMANCE_INVALID_TIMESTAMP \"-1 is not a valid timestamp\"",
      "measure between marks: span | measure | 100 | 150 |  | true | [object PerformanceMeasure] | {\"name\":\"span\",\"entryType\":\"measure\",\"startTime\":100,\"duration\":150,\"detail\":null}",
      "measure with options: 5,25,5,7,30,20,100,150",
      "measure from zero: 0,250",
      "measure unknown mark: SyntaxError | 12 | The \"nope\" performance mark has not been set | true",
      "measure name type threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"name\\\" argument must be of type string. Received type number (5)\"",
      "measure no name threw TypeError ERR_MISSING_ARGS \"The \\\"name\\\" argument must be specified\"",
      "measure options and endMark threw TypeError ERR_PERFORMANCE_MEASURE_INVALID_OPTIONS \"endMark must not be specified\"",
      "measure all three threw TypeError ERR_PERFORMANCE_MEASURE_INVALID_OPTIONS \"Must not have options.start, options.end, and options.duration specified\"",
      "measure negative number threw TypeError ERR_PERFORMANCE_INVALID_TIMESTAMP \"-5 is not a valid timestamp\"",
      "entries by type: a,5,s,e | z,o1,o2,o3,span,o4 | 0",
      "entries by name: 1,0,150",
      "entries sorted: z@0,o1@5,o2@5,a@10,5@20,o3@30,s@100,span@100,o4@100,e@250",
      "by type needs arg threw TypeError ERR_MISSING_ARGS \"The \\\"type\\\" argument must be specified\"",
      "by name needs arg threw TypeError ERR_MISSING_ARGS \"The \\\"name\\\" argument must be specified\"",
      "clear one mark: 0,SyntaxError",
      "clear measures: 5,0",
      "clear all marks: 0",
      "clear reserved threw TypeError ERR_INVALID_ARG_VALUE \"The argument 'name' is invalid. Received 'nodeStart'\"",
      "construct entry threw TypeError ERR_ILLEGAL_CONSTRUCTOR \"Illegal constructor\"",
      "construct measure threw TypeError ERR_ILLEGAL_CONSTRUCTOR \"Illegal constructor\"",
      "construct mark: made,3,0",
      "entry keys: detail,duration,entryType,name,startTime",
      "wrong this threw TypeError ERR_INVALID_THIS \"Value of \\\"this\\\" must be of type Performance\"",
      "entry getter wrong this threw TypeError ERR_INVALID_THIS \"Value of \\\"this\\\" must be of type PerformanceEntry\"",
    ]
}

pub fn web_globals_exist_only_when_asked_for_test() {
  assert run(
      start_on(engine.new(), []),
      "[typeof DOMException, typeof QuotaExceededError, typeof Event, typeof EventTarget, typeof AbortController, typeof AbortSignal].join()",
    ).0
    == "'undefined,undefined,undefined,undefined,undefined,undefined'"
}

pub fn a_throwing_listener_does_not_break_the_dispatch_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "
      const target = new EventTarget();
      target.addEventListener('x', () => { throw new Error('from a listener'); });
      target.addEventListener('x', () => out.push('second ran'));
      out.push(target.dispatchEvent(new Event('x')));
      ",
    )
  assert out_lines(rt) == ["second ran", "true"]
}

pub fn a_mark_detail_needs_structured_clone_test() {
  let rt = run_commonjs(node_runtime(), describe <> "
        out.push(performance.mark('plain').detail);
        out.push(describe(() => performance.mark('with detail', { detail: { n: 1 } })));
        globalThis.structuredClone = (value) => ({ ...value, copied: true });
        out.push(JSON.stringify(performance.mark('with detail', { detail: { n: 1 } }).detail));
        ")
  assert out_lines(rt)
    == [
      "",
      "Error [ERR_METHOD_NOT_IMPLEMENTED]: The structuredClone method is not implemented code",
      "{\"n\":1,\"copied\":true}",
    ]
}

const buffer_api_script = "
const buffer = require(\"buffer\");
const show = (v) => {
  if (v instanceof Uint8Array) return \"<\" + v.constructor.name + \" \" + Buffer.from(v.buffer, v.byteOffset, v.byteLength).toString(\"hex\") + \">\";
  if (typeof v === \"string\") return JSON.stringify(v);
  if (typeof v === \"object\" && v !== null) return JSON.stringify(v);
  return String(v);
};
const t = (label, f) => {
  try { out.push(label + \" -> \" + show(f())); }
  catch (e) { out.push(label + \" threw \" + e.name + \" \" + e.code + \" \" + JSON.stringify(e.message)); }
};
t(\"from string\", () => Buffer.from(\"abc\"));
t(\"from empty\", () => Buffer.from(\"\"));
t(\"from array\", () => Buffer.from([1, 2, 256, -1, 3.7, \"9\", \"x\"]));
t(\"from buffer copies\", () => { const a = Buffer.from(\"ab\"); const b = Buffer.from(a); b[0] = 0x7a; return a.toString() + b.toString(); });
t(\"from arraybuffer shares\", () => { const ab = new ArrayBuffer(4); const b = Buffer.from(ab, 1, 2); b[0] = 9; return new Uint8Array(ab); });
t(\"from arraybuffer oob offset\", () => Buffer.from(new ArrayBuffer(4), 5));
t(\"from arraybuffer oob length\", () => Buffer.from(new ArrayBuffer(4), 1, 9));
t(\"from json form\", () => Buffer.from({ type: \"Buffer\", data: [1, 2, 3] }));
t(\"from arraylike\", () => Buffer.from({ length: 2, 0: 7, 1: 8 }));
t(\"from valueOf\", () => Buffer.from(new String(\"hey\")));
t(\"from toPrimitive\", () => Buffer.from({ [Symbol.toPrimitive]() { return \"prim\"; } }));
t(\"from number\", () => Buffer.from(5));
t(\"from null\", () => Buffer.from(null));
t(\"from undefined\", () => Buffer.from(undefined));
t(\"from object\", () => Buffer.from({}));
t(\"from symbol\", () => Buffer.from(Symbol(\"s\")));
t(\"from bad encoding\", () => Buffer.from(\"a\", \"nope\"));
t(\"from odd encoding type\", () => Buffer.from(\"a\", 5));
t(\"from uint16array\", () => Buffer.from(new Uint16Array([1, 258])));
t(\"from big array\", () => Buffer.from(Array.from({ length: 5000 }, (_, i) => i)).subarray(4990).toString(\"hex\"));
t(\"Buffer(3, enc)\", () => Buffer(3, \"utf8\"));
t(\"alloc\", () => Buffer.alloc(3));
t(\"alloc fill\", () => Buffer.alloc(5, \"ab\"));
t(\"alloc fill enc\", () => Buffer.alloc(4, \"YQ==\", \"base64\"));
t(\"alloc fill number\", () => Buffer.alloc(3, 257));
t(\"alloc -1\", () => Buffer.alloc(-1));
t(\"alloc NaN\", () => Buffer.alloc(NaN));
t(\"alloc string\", () => Buffer.alloc(\"3\"));
t(\"alloc huge\", () => Buffer.alloc(2 ** 60));
t(\"alloc 1.5\", () => Buffer.alloc(1.5).length);
t(\"allocUnsafe\", () => Buffer.allocUnsafe(4).length);
t(\"allocUnsafe alignment 3\", () => Buffer.allocUnsafe(4, 3));
t(\"allocUnsafe alignment 16\", () => Buffer.allocUnsafe(4, 16).length);
t(\"allocUnsafeSlow\", () => Buffer.allocUnsafeSlow(4).length);
t(\"of\", () => Buffer.of(1, 2, 300));
t(\"copyBytesFrom\", () => Buffer.copyBytesFrom(new Uint16Array([1, 2, 3]), 1, 1));
t(\"copyBytesFrom bad\", () => Buffer.copyBytesFrom([1]));
t(\"copyBytesFrom neg\", () => Buffer.copyBytesFrom(new Uint8Array(2), -1));
t(\"byteLength\", () => [Buffer.byteLength(\"a€\"), Buffer.byteLength(\"aGk=\", \"base64\"), Buffer.byteLength(\"abc\", \"hex\"), Buffer.byteLength(\"abc\", \"ucs2\"), Buffer.byteLength(new ArrayBuffer(7)), Buffer.byteLength(Buffer.alloc(3)), Buffer.byteLength(\"ab\", \"nope\")].join());
t(\"byteLength bad\", () => Buffer.byteLength(5));
t(\"stringLength\", () => [Buffer.stringLength(Buffer.from(\"a€😀\")), Buffer.stringLength(Buffer.alloc(5), \"hex\"), Buffer.stringLength(Buffer.alloc(5), \"base64\"), Buffer.stringLength(Buffer.alloc(5), \"base64url\"), Buffer.stringLength(Buffer.alloc(5), \"ucs2\"), Buffer.stringLength(Buffer.alloc(5), \"latin1\")].join());
t(\"stringLength bad\", () => Buffer.stringLength(\"x\"));
t(\"isBuffer\", () => [Buffer.isBuffer(Buffer.alloc(1)), Buffer.isBuffer(new Uint8Array(1)), Buffer.isBuffer(null)].join());
t(\"isEncoding\", () => [\"utf8\", \"UTF-8\", \"ucs2\", \"UCS-2\", \"utf16le\", \"UTF-16LE\", \"latin1\", \"binary\", \"BASE64\", \"base64url\", \"hex\", \"ascii\", \"\", \"nope\", 5, null, undefined].map((e) => Buffer.isEncoding(e)).join());
t(\"compare\", () => [Buffer.compare(Buffer.from(\"a\"), Buffer.from(\"b\")), Buffer.compare(Buffer.from(\"b\"), Buffer.from(\"a\")), Buffer.compare(Buffer.from(\"a\"), Buffer.from(\"a\")), Buffer.compare(Buffer.from(\"a\"), new Uint8Array([97, 0]))].join());
t(\"compare bad\", () => Buffer.compare(\"a\", Buffer.alloc(1)));
t(\"compare bad 2\", () => Buffer.compare(Buffer.alloc(1), \"a\"));
t(\"concat\", () => Buffer.concat([Buffer.from(\"ab\"), new Uint8Array([99]), Buffer.from(\"d\")]));
t(\"concat length short\", () => Buffer.concat([Buffer.from(\"abc\"), Buffer.from(\"def\")], 4));
t(\"concat length long\", () => Buffer.concat([Buffer.from(\"ab\")], 5));
t(\"concat empty\", () => Buffer.concat([]));
t(\"concat bad list\", () => Buffer.concat(\"ab\"));
t(\"concat bad item\", () => Buffer.concat([Buffer.alloc(1), \"x\"]));
t(\"concat bad length\", () => Buffer.concat([Buffer.alloc(1)], -1));
t(\"toString args\", () => { const b = Buffer.from(\"hello world\"); return [b.toString(undefined, 6), b.toString(\"utf8\", -3, 5), b.toString(\"utf8\", 3, 99), b.toString(\"utf8\", 5, 2), b.toString(\"hex\", 0, 2), b.toString(\"utf8\", \"1\", \"3\"), b.toString(\"utf8\", 99)].join(\"|\"); });
t(\"toString bad encoding\", () => Buffer.from(\"a\").toString(\"nope\"));
t(\"toString non-string encoding\", () => Buffer.from(\"a\").toString(5));
t(\"toLocaleString\", () => Buffer.from(\"a\").toLocaleString === Buffer.prototype.toString);
t(\"toJSON\", () => Buffer.from(\"ab\").toJSON());
t(\"JSON.stringify\", () => JSON.stringify({ b: Buffer.from([1, 2]) }));
t(\"equals\", () => [Buffer.from(\"a\").equals(Buffer.from(\"a\")), Buffer.from(\"a\").equals(new Uint8Array([97])), Buffer.from(\"a\").equals(Buffer.from(\"b\")), Buffer.alloc(0).equals(Buffer.alloc(0))].join());
t(\"equals bad\", () => Buffer.from(\"a\").equals(\"a\"));
t(\"compare method bad\", () => Buffer.from(\"a\").compare(\"a\"));
t(\"compare range bad\", () => Buffer.from(\"abc\").compare(Buffer.from(\"abc\"), 0, 9));
t(\"compare range neg\", () => Buffer.from(\"abc\").compare(Buffer.from(\"abc\"), -1));
t(\"compare range float\", () => Buffer.from(\"abc\").compare(Buffer.from(\"abc\"), 0.5));
t(\"indexOf kinds\", () => { const b = Buffer.from(\"this is a buffer\"); return [b.indexOf(\"is\"), b.indexOf(\"is\", 3), b.indexOf(Buffer.from(\"a buffer\")), b.indexOf(97), b.indexOf(Buffer.from(\"a buffer example\")), b.indexOf(\"is\", -5), b.lastIndexOf(\"is\"), b.lastIndexOf(\"buffer\", 5), b.lastIndexOf(\"\"), b.indexOf(\"\", 99), b.includes(\"this\"), b.includes(\"that\"), b.indexOf(\"6973\", \"hex\"), b.indexOf(\"aXM=\", \"base64\"), b.indexOf(\"is\", undefined, \"utf8\"), b.indexOf(\"is\", null), b.indexOf(\"is\", \"2\"), b.indexOf(256 + 97), b.indexOf(\"b\", 2 ** 40), b.lastIndexOf(\"b\", -(2 ** 40))].join(); });
t(\"indexOf bad value\", () => Buffer.from(\"a\").indexOf({}));
t(\"indexOf bad encoding\", () => Buffer.from(\"a\").indexOf(\"a\", 0, \"nope\"));
t(\"indexOf on uint8array\", () => Buffer.prototype.indexOf.call(new Uint8Array([1, 2, 3]), 2));
t(\"indexOf on non-view\", () => Buffer.prototype.indexOf.call({}, 2));
t(\"fill kinds\", () => [Buffer.alloc(5).fill(\"a\").toString(), Buffer.alloc(5).fill(\"ab\", 1).toString(\"hex\"), Buffer.alloc(5).fill(\"€\").toString(\"hex\"), Buffer.alloc(6).fill(\"ab\", \"ucs2\").toString(\"hex\"), Buffer.alloc(4).fill(Buffer.from(\"xy\")).toString(), Buffer.alloc(3).fill(true).toString(\"hex\"), Buffer.alloc(3).fill(null).toString(\"hex\"), Buffer.alloc(3).fill(1.9).toString(\"hex\"), Buffer.alloc(3).fill(-1).toString(\"hex\"), Buffer.alloc(4).fill(\"abc\", \"hex\").toString(\"hex\"), Buffer.alloc(3).fill(\"é\", \"latin1\").toString(\"hex\")].join(\"|\"));
t(\"fill bad hex\", () => Buffer.alloc(4).fill(\"zz\", \"hex\"));
t(\"fill bad encoding\", () => Buffer.alloc(4).fill(\"a\", \"nope\"));
t(\"fill bad encoding type\", () => Buffer.alloc(4).fill(\"a\", 0, 4, 5));
t(\"fill offset neg\", () => Buffer.alloc(4).fill(\"a\", -1));
t(\"fill end big\", () => Buffer.alloc(4).fill(\"a\", 0, 9));
t(\"fill offset float\", () => Buffer.alloc(4).fill(\"a\", 1.5));
t(\"fill number big end\", () => Buffer.alloc(4).fill(1, 0, 9));
t(\"write forms\", () => { const b = Buffer.alloc(8, 0x2e); return [b.write(\"ab\"), b.write(\"cd\", 2), b.write(\"efgh\", 4, 2), b.write(\"6869\", \"hex\"), b.write(\"xy\", 6, \"latin1\"), b.write(\"z\", 7, 1, \"ascii\"), b.toString()].join(); });
t(\"write non-string\", () => Buffer.alloc(4).write(5));
t(\"write non-string enc\", () => Buffer.alloc(4).write(5, \"hex\"));
t(\"write offset oob\", () => Buffer.alloc(4).write(\"a\", 5));
t(\"write offset neg\", () => Buffer.alloc(4).write(\"a\", -1));
t(\"write length neg\", () => Buffer.alloc(4).write(\"a\", 0, -1));
t(\"write length big\", () => Buffer.alloc(4).write(\"abcdef\", 1, 99));
t(\"write bad encoding\", () => Buffer.alloc(4).write(\"a\", 0, 1, \"nope\"));
t(\"write offset string number\", () => Buffer.alloc(4).write(\"a\", \"1\"));
t(\"slices\", () => { const b = Buffer.from(\"héllo\"); return [b.utf8Slice(), b.utf8Slice(1), b.utf8Slice(1, 3), b.latin1Slice(0, 3), b.asciiSlice(0, 3), b.hexSlice(1, 3), b.base64Slice(), b.base64urlSlice(0, 2), b.ucs2Slice(0, 4), b.utf8Slice(5, 2), b.utf8Slice(\"1\", \"3\"), b.utf8Slice(1.9, 3.9), b.utf8Slice(NaN, 2)].join(\"|\"); });
t(\"slice neg\", () => Buffer.from(\"hello\").utf8Slice(-1));
t(\"slice end big\", () => Buffer.from(\"hello\").hexSlice(0, 99));
t(\"slice start big\", () => Buffer.from(\"hello\").latin1Slice(9));
t(\"writes\", () => { const b = Buffer.alloc(8, 0x2e); return [b.utf8Write(\"ab\"), b.latin1Write(\"c\", 2), b.asciiWrite(\"d\", 3, 1), b.hexWrite(\"6566\", 4), b.base64Write(\"Zw==\", 6), b.ucs2Write(\"h\", 6), b.base64urlWrite(\"_w\", 0), b.toString(\"latin1\")].join(); });
t(\"utf8Write oob\", () => Buffer.alloc(2).utf8Write(\"a\", 3));
t(\"utf8Write length oob\", () => Buffer.alloc(2).utf8Write(\"a\", 0, 3));
t(\"hexWrite oob\", () => Buffer.alloc(2).hexWrite(\"61\", 3));
t(\"hexWrite neg\", () => Buffer.alloc(2).hexWrite(\"61\", -1));
t(\"hexWrite non-string\", () => Buffer.alloc(2).hexWrite(5));
t(\"utf8Write non-string\", () => Buffer.alloc(2).utf8Write(5));
t(\"subarray\", () => { const b = Buffer.from(\"abcdef\"); const s = b.subarray(1, 4); s[0] = 0x7a; return [s.toString(), b.toString(), b.subarray(-2).toString(), b.subarray(4, 2).length, b.subarray(NaN, \"3\").toString(), b.slice(2).toString(), Buffer.isBuffer(s), b.subarray(9).length].join(); });
t(\"typed array methods keep Buffer\", () => { const b = Buffer.from([3, 1, 2]); return [Buffer.isBuffer(b.map((x) => x * 2)), Buffer.isBuffer(b.filter(() => true)), b.map((x) => x * 2).toString(\"hex\"), Buffer.isBuffer(Uint8Array.prototype.slice.call(b, 1))].join(); });
t(\"copy\", () => { const a = Buffer.from(\"abcdef\"), b = Buffer.alloc(4, 0x2e); return [a.copy(b), b.toString(), a.copy(b, 2, 4), b.toString(), a.copy(a, 2, 0, 4), a.toString(), a.copy(b, 9), a.copy(b, 0, 3, 1), a.copy(new Uint16Array(2), 1.7, \"1\")].join(); });
t(\"copy bad target\", () => Buffer.from(\"a\").copy(\"x\"));
t(\"copy neg target\", () => Buffer.from(\"a\").copy(Buffer.alloc(1), -1));
t(\"copy source oob\", () => Buffer.from(\"a\").copy(Buffer.alloc(1), 0, 5));
t(\"copy neg end\", () => Buffer.from(\"a\").copy(Buffer.alloc(1), 0, 0, -1));
t(\"swap\", () => [Buffer.from([1, 2, 3, 4]).swap16().toString(\"hex\"), Buffer.from([1, 2, 3, 4]).swap32().toString(\"hex\"), Buffer.from([1, 2, 3, 4, 5, 6, 7, 8]).swap64().toString(\"hex\")].join());
t(\"swap16 odd\", () => Buffer.alloc(3).swap16());
t(\"swap32 odd\", () => Buffer.alloc(6).swap32());
t(\"swap64 odd\", () => Buffer.alloc(12).swap64());
t(\"parent and offset\", () => { const ab = new ArrayBuffer(8); const b = Buffer.from(ab, 2, 4); return [b.parent === ab, b.offset, Object.getOwnPropertyDescriptor(Buffer.prototype, \"parent\").enumerable, Buffer.prototype.parent, Buffer.prototype.offset].join(); });
t(\"inspect\", () => [Buffer.from(\"hi\").inspect(), Buffer.alloc(0).inspect(), Buffer.alloc(60, 1).inspect().length, Buffer.from(\"hi\")[Symbol.for(\"nodejs.util.inspect.custom\")] === Buffer.prototype.inspect].join(\"|\"));
t(\"INSPECT_MAX_BYTES\", () => { const old = buffer.INSPECT_MAX_BYTES; buffer.INSPECT_MAX_BYTES = 2; const r = Buffer.from(\"hello\").inspect(); buffer.INSPECT_MAX_BYTES = old; return r; });
t(\"INSPECT_MAX_BYTES bad\", () => { buffer.INSPECT_MAX_BYTES = -1; });
t(\"INSPECT_MAX_BYTES bad type\", () => { buffer.INSPECT_MAX_BYTES = \"x\"; });
t(\"constants\", () => [buffer.kMaxLength, buffer.kStringMaxLength, JSON.stringify(buffer.constants), buffer.constants.MAX_LENGTH === buffer.kMaxLength, Object.getOwnPropertyDescriptor(buffer.constants, \"MAX_LENGTH\").writable, Buffer.poolSize].join());
t(\"class shape\", () => [Buffer.name, Buffer.length, Object.getPrototypeOf(Buffer) === Uint8Array, Object.getPrototypeOf(Buffer.prototype) === Uint8Array.prototype, Buffer.prototype.constructor === Buffer, Buffer.from(\"a\").constructor === Buffer, Buffer[Symbol.species] !== Buffer, new (Buffer[Symbol.species])(2) instanceof Buffer, Object.prototype.toString.call(Buffer.alloc(1)), typeof Buffer.prototype.readUInt8].join());
t(\"btoa\", () => [btoa(\"hello\"), btoa(\"\"), btoa(\"ÿ\\u0000\"), btoa(5), btoa(null), btoa(undefined)].join());
t(\"btoa bad\", () => btoa(\"€\"));
t(\"btoa none\", () => btoa());
t(\"atob\", () => [atob(\"aGVsbG8=\"), atob(\"aGVsbG8\"), atob(\" aGVs\\nbG8= \"), atob(\"\"), atob(\"YQ==\"), atob(\"YQ\"), JSON.stringify(atob(\"/wA=\")), atob(null).length, atob(\"abcd\").length].join(\"|\"));
t(\"atob len 1\", () => atob(\"a\"));
t(\"atob len 5\", () => atob(\"abcde\"));
t(\"atob bad char\", () => atob(\"a$bc\"));
t(\"atob pad middle\", () => atob(\"YQ==YQ==\"));
t(\"atob pad wrong\", () => atob(\"YQ=\"));
t(\"atob three pads\", () => atob(\"Y===\"));
t(\"atob url alphabet\", () => atob(\"_-8=\"));
t(\"atob none\", () => atob());
t(\"atob is global one\", () => atob === buffer.atob && btoa === buffer.btoa && Buffer === buffer.Buffer);
t(\"isUtf8\", () => [buffer.isUtf8(Buffer.from(\"hé\")), buffer.isUtf8(Buffer.from([0xc3])), buffer.isUtf8(new Uint8Array([0xed, 0xa0, 0x80])), buffer.isUtf8(new ArrayBuffer(2)), buffer.isAscii(Buffer.from(\"abc\")), buffer.isAscii(Buffer.from(\"é\")), buffer.isAscii(new Uint16Array([65, 300])), buffer.isLatin1(\"hé\"), buffer.isLatin1(\"€\")].join());
t(\"isUtf8 bad\", () => buffer.isUtf8(\"a\"));
t(\"isAscii bad\", () => buffer.isAscii([1]));
t(\"isLatin1 bad\", () => buffer.isLatin1(5));
t(\"static keys\", () => Object.getOwnPropertyNames(Buffer).sort().join(\" \"));
t(\"proto keys\", () => Object.getOwnPropertyNames(Buffer.prototype).filter((k) => !/^(read|write)[A-Z]/.test(k) || /^write$/.test(k)).sort().join(\" \"));
t(\"require forms\", () => require(\"node:buffer\") === buffer && require(\"buffer\").Buffer === Buffer);
"

// every expected line is what node 26.11.1 prints for the same script
pub fn buffer_matches_node_test() {
  let rt = run_commonjs(node_runtime(), buffer_api_script)
  assert out_lines(rt)
    == [
      "from string -> <Buffer 616263>",
      "from empty -> <Buffer >",
      "from array -> <Buffer 010200ff030900>",
      "from buffer copies -> \"abzb\"",
      "from arraybuffer shares -> <Uint8Array 00090000>",
      "from arraybuffer oob offset threw RangeError ERR_BUFFER_OUT_OF_BOUNDS \"\\\"offset\\\" is outside of buffer bounds\"",
      "from arraybuffer oob length threw RangeError ERR_BUFFER_OUT_OF_BOUNDS \"\\\"length\\\" is outside of buffer bounds\"",
      "from json form -> <Buffer 010203>",
      "from arraylike -> <Buffer 0708>",
      "from valueOf -> <Buffer 686579>",
      "from toPrimitive -> <Buffer 7072696d>",
      "from number threw TypeError ERR_INVALID_ARG_TYPE \"The first argument must be of type string or an instance of Buffer, ArrayBuffer, or Array or an Array-like Object. Received type number (5)\"",
      "from null threw TypeError ERR_INVALID_ARG_TYPE \"The first argument must be of type string or an instance of Buffer, ArrayBuffer, or Array or an Array-like Object. Received null\"",
      "from undefined threw TypeError ERR_INVALID_ARG_TYPE \"The first argument must be of type string or an instance of Buffer, ArrayBuffer, or Array or an Array-like Object. Received undefined\"",
      "from object threw TypeError ERR_INVALID_ARG_TYPE \"The first argument must be of type string or an instance of Buffer, ArrayBuffer, or Array or an Array-like Object. Received an instance of Object\"",
      "from symbol threw TypeError ERR_INVALID_ARG_TYPE \"The first argument must be of type string or an instance of Buffer, ArrayBuffer, or Array or an Array-like Object. Received type symbol (Symbol(s))\"",
      "from bad encoding threw TypeError ERR_UNKNOWN_ENCODING \"Unknown encoding: nope\"",
      "from odd encoding type -> <Buffer 61>",
      "from uint16array -> <Buffer 0102>",
      "from big array -> \"7e7f8081828384858687\"",
      "Buffer(3, enc) threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"string\\\" argument must be of type string. Received type number (3)\"",
      "alloc -> <Buffer 000000>",
      "alloc fill -> <Buffer 6162616261>",
      "alloc fill enc -> <Buffer 61616161>",
      "alloc fill number -> <Buffer 010101>",
      "alloc -1 threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"size\\\" is out of range. It must be >= 0 && <= 9007199254740991. Received -1\"",
      "alloc NaN threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"size\\\" is out of range. It must be >= 0 && <= 9007199254740991. Received NaN\"",
      "alloc string threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"size\\\" argument must be of type number. Received type string ('3')\"",
      "alloc huge threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"size\\\" is out of range. It must be >= 0 && <= 9007199254740991. Received 1_152_921_504_606_847_000\"",
      "alloc 1.5 -> 1",
      "allocUnsafe -> 4",
      "allocUnsafe alignment 3 threw TypeError ERR_INVALID_ARG_VALUE \"The argument 'alignment' must be a power of two. Received 3\"",
      "allocUnsafe alignment 16 -> 4",
      "allocUnsafeSlow -> 4",
      "of -> <Buffer 01022c>",
      "copyBytesFrom -> <Buffer 0200>",
      "copyBytesFrom bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"view\\\" argument must be an instance of TypedArray. Received an instance of Array\"",
      "copyBytesFrom neg threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"offset\\\" is out of range. It must be >= 0 && <= 9007199254740991. Received -1\"",
      "byteLength -> \"4,2,1,6,7,3,2\"",
      "byteLength bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"string\\\" argument must be of type string or an instance of Buffer or ArrayBuffer. Received type number (5)\"",
      "stringLength -> \"4,10,8,7,2,5\"",
      "stringLength bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"input\\\" argument must be an instance of ArrayBuffer, Buffer, or TypedArray. Received type string ('x')\"",
      "isBuffer -> \"true,false,false\"",
      "isEncoding -> \"true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,false,false\"",
      "compare -> \"-1,1,0,-1\"",
      "compare bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"buf1\\\" argument must be an instance of Buffer or Uint8Array. Received type string ('a')\"",
      "compare bad 2 threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"buf2\\\" argument must be an instance of Buffer or Uint8Array. Received type string ('a')\"",
      "concat -> <Buffer 61626364>",
      "concat length short -> <Buffer 61626364>",
      "concat length long -> <Buffer 6162000000>",
      "concat empty -> <Buffer >",
      "concat bad list threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"list\\\" argument must be an instance of Array. Received type string ('ab')\"",
      "concat bad item threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"list[1]\\\" argument must be an instance of Buffer or Uint8Array. Received type string ('x')\"",
      "concat bad length threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"length\\\" is out of range. It must be >= 0 && <= 9007199254740991. Received -1\"",
      "toString args -> \"world|hello|lo world||6865|el|\"",
      "toString bad encoding threw TypeError ERR_UNKNOWN_ENCODING \"Unknown encoding: nope\"",
      "toString non-string encoding threw TypeError ERR_UNKNOWN_ENCODING \"Unknown encoding: 5\"",
      "toLocaleString -> true",
      "toJSON -> {\"type\":\"Buffer\",\"data\":[97,98]}",
      "JSON.stringify -> \"{\\\"b\\\":{\\\"type\\\":\\\"Buffer\\\",\\\"data\\\":[1,2]}}\"",
      "equals -> \"true,true,false,true\"",
      "equals bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"otherBuffer\\\" argument must be an instance of Buffer or Uint8Array. Received type string ('a')\"",
      "compare method bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"target\\\" argument must be an instance of Buffer or Uint8Array. Received type string ('a')\"",
      "compare range bad threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"targetEnd\\\" is out of range. It must be >= 0 && <= 3. Received 9\"",
      "compare range neg threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"targetStart\\\" is out of range. It must be >= 0 && <= 9007199254740991. Received -1\"",
      "compare range float threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"targetStart\\\" is out of range. It must be an integer. Received 0.5\"",
      "indexOf kinds threw TypeError ERR_UNKNOWN_ENCODING \"Unknown encoding: 2\"",
      "indexOf bad value threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"value\\\" argument must be one of type number or string or an instance of Buffer or Uint8Array. Received an instance of Object\"",
      "indexOf bad encoding threw TypeError ERR_UNKNOWN_ENCODING \"Unknown encoding: nope\"",
      "indexOf on uint8array -> 1",
      "indexOf on non-view threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"buffer\\\" argument must be an instance of Buffer, TypedArray, or DataView. Received an instance of Object\"",
      "fill kinds -> \"aaaaa|0061626162|e282ace282|610062006100|xyxy|010101|000000|010101|ffffff|abababab|e9e9e9\"",
      "fill bad hex threw TypeError ERR_INVALID_ARG_VALUE \"The argument 'value' is invalid. Received 'zz'\"",
      "fill bad encoding threw TypeError ERR_UNKNOWN_ENCODING \"Unknown encoding: nope\"",
      "fill bad encoding type threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"encoding\\\" argument must be of type string. Received type number (5)\"",
      "fill offset neg threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"offset\\\" is out of range. It must be >= 0 && <= 9007199254740991. Received -1\"",
      "fill end big threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"end\\\" is out of range. It must be >= 0 && <= 4. Received 9\"",
      "fill offset float threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"offset\\\" is out of range. It must be an integer. Received 1.5\"",
      "fill number big end threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"end\\\" is out of range. It must be >= 0 && <= 4. Received 9\"",
      "write forms -> \"2,2,2,2,2,1,hicdefxz\"",
      "write non-string threw TypeError ERR_INVALID_ARG_TYPE \"argument must be a string\"",
      "write non-string enc threw TypeError ERR_INVALID_ARG_TYPE \"argument must be a string\"",
      "write offset oob threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"offset\\\" is out of range. It must be >= 0 && <= 4. Received 5\"",
      "write offset neg threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"offset\\\" is out of range. It must be >= 0 && <= 4. Received -1\"",
      "write length neg threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"length\\\" is out of range. It must be >= 0 && <= 4. Received -1\"",
      "write length big threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"length\\\" is out of range. It must be >= 0 && <= 4. Received 99\"",
      "write bad encoding threw TypeError ERR_UNKNOWN_ENCODING \"Unknown encoding: nope\"",
      "write offset string number threw TypeError ERR_UNKNOWN_ENCODING \"Unknown encoding: 1\"",
      "slices -> \"héllo|éllo|é|hÃ©|hC)|c3a9|aMOpbGxv|aMM|써沩||é|é|h�\"",
      "slice neg threw RangeError ERR_OUT_OF_RANGE \"Index out of range\"",
      "slice end big threw RangeError ERR_OUT_OF_RANGE \"Index out of range\"",
      "slice start big -> \"\"",
      "writes -> \"2,1,1,2,1,2,1,ÿbcdefh\\u0000\"",
      "utf8Write oob threw RangeError ERR_BUFFER_OUT_OF_BOUNDS \"\\\"offset\\\" is outside of buffer bounds\"",
      "utf8Write length oob threw RangeError ERR_BUFFER_OUT_OF_BOUNDS \"\\\"length\\\" is outside of buffer bounds\"",
      "hexWrite oob threw RangeError ERR_BUFFER_OUT_OF_BOUNDS \"\\\"offset\\\" is outside of buffer bounds\"",
      "hexWrite neg threw RangeError ERR_OUT_OF_RANGE \"Index out of range\"",
      "hexWrite non-string threw TypeError ERR_INVALID_ARG_TYPE \"argument must be a string\"",
      "utf8Write non-string threw TypeError ERR_INVALID_ARG_TYPE \"argument must be a string\"",
      "subarray -> \"zcd,azcdef,ef,0,azc,cdef,true,0\"",
      "typed array methods keep Buffer -> \"true,true,060204,true\"",
      "copy -> \"4,abcd,2,abef,4,ababcd,0,0,3\"",
      "copy bad target threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"target\\\" argument must be an instance of Buffer or Uint8Array. Received type string ('x')\"",
      "copy neg target threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"targetStart\\\" is out of range. It must be >= 0. Received -1\"",
      "copy source oob threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"sourceStart\\\" is out of range. It must be >= 0 && <= 1. Received 5\"",
      "copy neg end threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"sourceEnd\\\" is out of range. It must be >= 0. Received -1\"",
      "swap -> \"02010403,04030201,0807060504030201\"",
      "swap16 odd threw RangeError ERR_INVALID_BUFFER_SIZE \"Buffer size must be a multiple of 16-bits\"",
      "swap32 odd threw RangeError ERR_INVALID_BUFFER_SIZE \"Buffer size must be a multiple of 32-bits\"",
      "swap64 odd threw RangeError ERR_INVALID_BUFFER_SIZE \"Buffer size must be a multiple of 64-bits\"",
      "parent and offset -> \"true,2,true,,\"",
      "inspect -> \"<Buffer 68 69>|<Buffer >|176|true\"",
      "INSPECT_MAX_BYTES -> \"<Buffer 68 65 ... 3 more bytes>\"",
      "INSPECT_MAX_BYTES bad threw RangeError ERR_OUT_OF_RANGE \"The value of \\\"INSPECT_MAX_BYTES\\\" is out of range. It must be >= 0. Received -1\"",
      "INSPECT_MAX_BYTES bad type threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"INSPECT_MAX_BYTES\\\" argument must be of type number. Received type string ('x')\"",
      "constants -> \"9007199254740991,536870888,{\\\"MAX_LENGTH\\\":9007199254740991,\\\"MAX_STRING_LENGTH\\\":536870888},true,false,65536\"",
      "class shape -> \"Buffer,3,true,true,true,true,true,true,[object Uint8Array],function\"",
      "btoa -> \"aGVsbG8=,,/wA=,NQ==,bnVsbA==,dW5kZWZpbmVk\"",
      "btoa bad threw InvalidCharacterError 5 \"Invalid character\"",
      "btoa none threw TypeError ERR_MISSING_ARGS \"The \\\"input\\\" argument must be specified\"",
      "atob -> \"hello|hello|hello||a|a|\\\"ÿ\\\\u0000\\\"|3|3\"",
      "atob len 1 threw InvalidCharacterError 5 \"The string to be decoded is not correctly encoded.\"",
      "atob len 5 threw InvalidCharacterError 5 \"The string to be decoded is not correctly encoded.\"",
      "atob bad char threw InvalidCharacterError 5 \"Invalid character\"",
      "atob pad middle threw InvalidCharacterError 5 \"Invalid character\"",
      "atob pad wrong threw InvalidCharacterError 5 \"Invalid character\"",
      "atob three pads threw InvalidCharacterError 5 \"Invalid character\"",
      "atob url alphabet threw InvalidCharacterError 5 \"Invalid character\"",
      "atob none threw TypeError ERR_MISSING_ARGS \"The \\\"input\\\" argument must be specified\"",
      "atob is global one -> true",
      "isUtf8 -> \"true,false,false,true,true,false,true,true,false\"",
      "isUtf8 bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"input\\\" argument must be an instance of ArrayBuffer, Buffer, or TypedArray. Received type string ('a')\"",
      "isAscii bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"input\\\" argument must be an instance of ArrayBuffer, Buffer, or TypedArray. Received an instance of Array\"",
      "isLatin1 bad threw TypeError ERR_INVALID_ARG_TYPE \"The \\\"input\\\" argument must be of type string. Received type number (5)\"",
      "static keys -> \"alloc allocUnsafe allocUnsafeSlow byteLength compare concat copyBytesFrom from isBuffer isEncoding length name of poolSize prototype stringLength\"",
      "proto keys -> \"asciiSlice asciiWrite base64Slice base64Write base64urlSlice base64urlWrite compare constructor copy equals fill hexSlice hexWrite includes indexOf inspect lastIndexOf latin1Slice latin1Write offset parent slice subarray swap16 swap32 swap64 toJSON toLocaleString toString ucs2Slice ucs2Write utf8Slice utf8Write write\"",
      "require forms -> true",
    ]
}

const buffer_generated_script = "
// generated inputs through every encoding, both ways
let seed = 12345;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const encodings = [\"utf8\", \"hex\", \"base64\", \"base64url\", \"latin1\", \"ascii\", \"ucs2\", \"binary\", \"utf16le\", \"utf-8\"];
const lines = [];
// bytes -> string
for (let i = 0; i < 700; i++) {
  const n = rnd(24);
  const bytes = [];
  for (let k = 0; k < n; k++) bytes.push(rnd(4) === 0 ? 128 + rnd(128) : rnd(128));
  const b = Buffer.from(bytes);
  const enc = encodings[rnd(encodings.length)];
  const s = rnd(3) === 0 ? rnd(n + 1) : 0;
  const e = rnd(3) === 0 ? rnd(n + 1) : n;
  lines.push(\"D \" + enc + \" \" + b.toString(\"hex\") + \" \" + s + \" \" + e + \" \" + JSON.stringify(b.toString(enc, s, e)));
}
// string -> bytes
const alphabets = [
  \"abcdefXYZ 0123456789\",
  \"héllo wörld € 中文 😀 ÿĀ\",
  \"0123456789abcdefABCDEFg \",
  \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/-_= \\n*\",
];
for (let i = 0; i < 900; i++) {
  const alphabet = Array.from(alphabets[rnd(alphabets.length)]);
  const n = rnd(20);
  let s = \"\";
  for (let k = 0; k < n; k++) s += alphabet[rnd(alphabet.length)];
  const enc = encodings[rnd(encodings.length)];
  const b = Buffer.from(s, enc);
  lines.push(\"E \" + enc + \" \" + JSON.stringify(s) + \" \" + b.toString(\"hex\") + \" \" + (Array.from(s).some((c) => c.length > 1) ? \"-\" : Buffer.byteLength(s, enc)));
  // write into a tight buffer at an offset
  const t = Buffer.alloc(8, 0x2e);
  const off = rnd(8), len = rnd(9);
  let w;
  try { w = t.write(s, off, len, enc); } catch (err) { w = err.code; }
  lines.push(\"W \" + enc + \" \" + off + \" \" + len + \" \" + w + \" \" + t.toString(\"hex\"));
}
// searching
for (let i = 0; i < 700; i++) {
  const hay = Buffer.from(Array.from({ length: rnd(14) }, () => 97 + rnd(3)));
  const needleLen = rnd(4);
  const needle = Array.from({ length: needleLen }, () => 97 + rnd(3));
  const offs = [undefined, rnd(16), -rnd(16), 100, -100];
  const off = offs[rnd(offs.length)];
  const kind = rnd(4);
  const val = kind === 0 ? Buffer.from(needle) : kind === 1 ? String.fromCharCode(...needle) : kind === 2 ? needle[0] ?? 97 : new Uint8Array(needle);
  const end = rnd(4) === 0 ? rnd(16) : undefined;
  const show = (v) => (typeof v === \"number\" ? v : typeof v === \"string\" ? JSON.stringify(v) : Buffer.from(v).toString(\"hex\"));
  lines.push(\"S \" + hay.toString() + \" \" + show(val) + \" \" + off + \" \" + end + \" \" + (end === undefined ? hay.indexOf(val, off) + \" \" + hay.lastIndexOf(val, off) + \" \" + hay.includes(val, off) : hay.indexOf(val, off, end) + \" \" + hay.lastIndexOf(val, off, end)));
}
// utf-16 search and compare
for (let i = 0; i < 300; i++) {
  const text = Array.from({ length: rnd(8) }, () => \"ab€\"[rnd(3)]).join(\"\");
  const hay = Buffer.from(text, \"ucs2\");
  const needle = Array.from({ length: 1 + rnd(2) }, () => \"ab€\"[rnd(3)]).join(\"\");
  const off = [undefined, rnd(12), -rnd(12)][rnd(3)];
  lines.push(\"U \" + JSON.stringify(text) + \" \" + JSON.stringify(needle) + \" \" + off + \" \" + hay.indexOf(needle, off, \"ucs2\") + \" \" + hay.lastIndexOf(needle, off, \"utf16le\"));
  const a = Buffer.from(Array.from({ length: rnd(6) }, () => rnd(4)));
  const c = Buffer.from(Array.from({ length: rnd(6) }, () => rnd(4)));
  let ranged;
  const args = [rnd(7), rnd(7), rnd(7), rnd(7)];
  try { ranged = a.compare(c, ...args); } catch (err) { ranged = err.code + \":\" + err.message; }
  lines.push(\"C \" + a.toString(\"hex\") + \" \" + c.toString(\"hex\") + \" \" + Buffer.compare(a, c) + \" \" + a.equals(c) + \" \" + args.join(\",\") + \" \" + ranged);
}
// fill, copy, swap
for (let i = 0; i < 400; i++) {
  const t = Buffer.alloc(rnd(40), 0x2e);
  const values = [\"ab\", \"€\", \"\", \"6162\", \"YWJj\", 7, 300, Buffer.from(\"xyz\"), new Uint8Array([1, 2]), \"q\"];
  const value = values[rnd(values.length)];
  const enc = [\"utf8\", \"hex\", \"base64\", \"latin1\", \"ucs2\", undefined][rnd(6)];
  const a = rnd(3) === 0 ? undefined : rnd(40), b = rnd(3) === 0 ? undefined : rnd(40);
  let r;
  try { r = (typeof value === \"string\" ? t.fill(value, a, b, enc) : t.fill(value, a, b)).toString(\"hex\"); } catch (err) { r = err.code; }
  lines.push(\"F \" + t.length + \" \" + (typeof value === \"string\" ? JSON.stringify(value) : value.length ?? value) + \" \" + a + \" \" + b + \" \" + enc + \" \" + r);
  const src = Buffer.from(Array.from({ length: rnd(12) }, (_, k) => k + 1));
  const dst = Buffer.alloc(rnd(12), 0xee);
  const cargs = [rnd(14), rnd(14), rnd(14)].slice(0, rnd(4));
  let copied;
  try { copied = src.copy(dst, ...cargs); } catch (err) { copied = err.code; }
  lines.push(\"Y \" + src.length + \" \" + cargs.join(\",\") + \" \" + copied + \" \" + dst.toString(\"hex\"));
  const sw = Buffer.from(Array.from({ length: 8 * rnd(9) }, (_, k) => k));
  lines.push(\"X \" + Buffer.from(sw).swap16().toString(\"hex\") + \" \" + Buffer.from(sw).swap32().toString(\"hex\") + \" \" + Buffer.from(sw).swap64().toString(\"hex\"));
}
const groups = new Map();
for (const line of lines) {
  const kind = line.slice(0, line.search(/[ (]/));
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
"

// 5,000 generated cases; each line is a kind of case, how many there were and
// a hash of what they printed, all taken from node 26.11.1
pub fn buffer_generated_cases_match_node_test() {
  let rt = run_commonjs(node_runtime(), buffer_generated_script)
  assert out_lines(rt)
    == [
      "D 700 762278617",
      "E 900 1057510171",
      "W 900 2899727388",
      "S 700 4125716781",
      "U 300 3461003392",
      "C 300 3150946296",
      "F 400 1880786403",
      "Y 400 1402141324",
      "X 400 432202951",
    ]
}

const buffer_numbers_script = "
let seed = 777;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const lines = [];
const call = (f) => { try { const v = f(); return typeof v === \"bigint\" ? v + \"n\" : Object.is(v, -0) ? \"-0\" : String(v); } catch (e) { return e.name + \" \" + e.code + \" \" + e.message; } };
const proto = Buffer.prototype;
const names = Object.getOwnPropertyNames(proto).filter((k) => /^(read|write)[A-Z]/.test(k)).sort();
lines.push(\"names \" + names.join(\" \"));
lines.push(\"aliases \" + names.filter((k) => /Uint/.test(k)).every((k) => proto[k] === proto[k.replace(\"Uint\", \"UInt\")]));
const reads = names.filter((k) => k.startsWith(\"read\"));
const writes = names.filter((k) => k.startsWith(\"write\"));
const base = Array.from({ length: 16 }, () => rnd(256));
const offsets = [undefined, 0, 1, 3, 8, 9, 12, 15, 16, -1, 1.5, \"1\", NaN, null];
for (const name of reads) {
  for (let i = 0; i < 14; i++) {
    const b = Buffer.from(base);
    const off = offsets[rnd(offsets.length)];
    const variable = /^read(U?)Int(LE|BE)$/i.test(name);
    const len = [1, 2, 3, 4, 5, 6, 0, 7, undefined, 2.5][rnd(10)];
    lines.push(name + \"(\" + String(off) + (variable ? \",\" + String(len) : \"\") + \") \" + call(() => (variable ? b[name](off, len) : b[name](off))));
  }
}
const numbers = [0, 1, -1, 127, 128, 255, 256, -128, -129, 32767, 32768, 65535, 65536, -32768, 2 ** 31 - 1, 2 ** 31, -(2 ** 31), 2 ** 32 - 1, 2 ** 32, 2 ** 40, 2 ** 47, 2 ** 48 - 1, -(2 ** 47), 1.5, -0, 0.1, 3.4e38, 1e39, Infinity, -Infinity, NaN, \"5\", null, undefined, true];
const bigs = [0n, 1n, -1n, 2n ** 63n - 1n, 2n ** 63n, -(2n ** 63n), 2n ** 64n - 1n, 2n ** 64n, 123456789012345678n, 5];
for (const name of writes) {
  for (let i = 0; i < 16; i++) {
    const b = Buffer.alloc(12, 0xee);
    const off = offsets[rnd(offsets.length)];
    const big = /Big/.test(name);
    const value = big ? bigs[rnd(bigs.length)] : numbers[rnd(numbers.length)];
    const variable = /^write(U?)Int(LE|BE)$/i.test(name);
    const len = [1, 2, 3, 4, 5, 6, 0, 7, undefined][rnd(9)];
    const r = call(() => (variable ? b[name](value, off, len) : b[name](value, off)));
    lines.push(name + \"(\" + (typeof value === \"bigint\" ? value + \"n\" : Object.is(value, -0) ? \"-0\" : String(value)) + \",\" + String(off) + (variable ? \",\" + String(len) : \"\") + \") \" + r + \" \" + b.toString(\"hex\"));
  }
}
// round trips of floats
for (const v of [0, -0, 1, -1, 0.1, Math.PI, 1e-45, 3.4028234663852886e38, 5e-324, 1.7976931348623157e308, Infinity, -Infinity, NaN]) {
  const b = Buffer.alloc(8);
  b.writeFloatLE(v); const f1 = b.readFloatLE(); const h1 = b.toString(\"hex\", 0, 4);
  b.writeFloatBE(v); const h2 = b.toString(\"hex\", 0, 4);
  b.writeDoubleLE(v); const d1 = b.readDoubleLE(); const h3 = b.toString(\"hex\");
  b.writeDoubleBE(v); const h4 = b.toString(\"hex\");
  lines.push(\"float \" + (Object.is(v, -0) ? \"-0\" : v) + \" \" + (Object.is(f1, -0) ? \"-0\" : f1) + \" \" + h1 + \" \" + h2 + \" \" + (Object.is(d1, -0) ? \"-0\" : d1) + \" \" + h3 + \" \" + h4);
}
lines.push(\"on plain Uint8Array \" + call(() => proto.readUInt16LE.call(new Uint8Array([1, 2]), 0)) + \" \" + call(() => proto.writeInt8.call(new Uint8Array(1), -1, 0)));
lines.push(\"returns next offset \" + [Buffer.alloc(8).writeUInt8(1, 3), Buffer.alloc(8).writeUInt32BE(1, 2), Buffer.alloc(8).writeBigInt64LE(1n), Buffer.alloc(8).writeDoubleBE(1), Buffer.alloc(8).writeIntLE(1, 1, 6)].join());
const groups = new Map();
for (const line of lines) {
  const kind = line.slice(0, line.search(/[ (]/));
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
"

pub fn buffer_number_methods_match_node_test() {
  let rt = run_commonjs(node_runtime(), buffer_numbers_script)
  assert out_lines(rt)
    == [
      "names 1 3589208262",
      "aliases 1 2676032365",
      "readBigInt64BE 14 1026299662",
      "readBigInt64LE 14 2995580848",
      "readBigUInt64BE 14 2661660975",
      "readBigUInt64LE 14 3578371961",
      "readBigUint64BE 14 3325634309",
      "readBigUint64LE 14 3919078691",
      "readDoubleBE 14 1859229908",
      "readDoubleLE 14 3447878452",
      "readFloatBE 14 2203342164",
      "readFloatLE 14 1136496261",
      "readInt16BE 14 2075525357",
      "readInt16LE 14 364116503",
      "readInt32BE 14 273834886",
      "readInt32LE 14 3270718426",
      "readInt8 14 4089921925",
      "readIntBE 14 3220062340",
      "readIntLE 14 392044368",
      "readUInt16BE 14 357289859",
      "readUInt16LE 14 1364100875",
      "readUInt32BE 14 1364923125",
      "readUInt32LE 14 3375769484",
      "readUInt8 14 4121902412",
      "readUIntBE 14 3218339420",
      "readUIntLE 14 3601795281",
      "readUint16BE 14 906719722",
      "readUint16LE 14 1663974370",
      "readUint32BE 14 2063083261",
      "readUint32LE 14 3655896",
      "readUint8 14 1952638483",
      "readUintBE 14 2344493059",
      "readUintLE 14 3495808406",
      "writeBigInt64BE 16 2057460254",
      "writeBigInt64LE 16 4219988261",
      "writeBigUInt64BE 16 355670359",
      "writeBigUInt64LE 16 884066179",
      "writeBigUint64BE 16 1191965515",
      "writeBigUint64LE 16 2502347949",
      "writeDoubleBE 16 1078761128",
      "writeDoubleLE 16 2958939927",
      "writeFloatBE 16 1451430094",
      "writeFloatLE 16 1919113661",
      "writeInt16BE 16 363423253",
      "writeInt16LE 16 1647608670",
      "writeInt32BE 16 1602110576",
      "writeInt32LE 16 388398687",
      "writeInt8 16 4269404071",
      "writeIntBE 16 4047553915",
      "writeIntLE 16 3446259883",
      "writeUInt16BE 16 2804628922",
      "writeUInt16LE 16 943311439",
      "writeUInt32BE 16 3434246836",
      "writeUInt32LE 16 3612413106",
      "writeUInt8 16 2311072293",
      "writeUIntBE 16 415137647",
      "writeUIntLE 16 904749522",
      "writeUint16BE 16 2364314431",
      "writeUint16LE 16 3567116990",
      "writeUint32BE 16 2879658155",
      "writeUint32LE 16 1585777643",
      "writeUint8 16 2426213593",
      "writeUintBE 16 2497832611",
      "writeUintLE 16 1228589692",
      "float 13 236869030",
      "on 1 759922857",
      "returns 1 25121539",
    ]
}

pub fn buffer_module_exports_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "out.push(Object.keys(require('buffer')).sort().join(' '))",
    )
  assert out_lines(rt)
    == [
      "Buffer INSPECT_MAX_BYTES atob btoa constants isAscii isLatin1 isUtf8 kMaxLength kStringMaxLength",
    ]
}

pub fn buffer_globals_exist_only_when_asked_for_test() {
  assert run(
      start_on(engine.new(), []),
      "[typeof Buffer, typeof atob, typeof btoa].join()",
    ).0
    == "'undefined,undefined,undefined'"
}

const path_generated_script = "
const path = require(\"path\");
let seed = 4242;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const lines = [];
const call = (f) => { try { const v = f(); return typeof v === \"string\" ? JSON.stringify(v) : JSON.stringify(v); } catch (e) { return e.name + \" \" + e.code + \" \" + e.message; } };
const posixParts = [\"\", \".\", \"..\", \"a\", \"b.txt\", \"c.tar.gz\", \".hidden\", \"dir\", \"/\", \"//\", \"a/b\", \"/abs\", \"x y\", \"\\u00e9\\u4e2d\", \"a.\", \"..b\", \"...\", \"/a/b/c/\", \"./x\", \"../y\", \"a//b\", \"trailing/\"];
const winParts = [\"\", \".\", \"..\", \"a\", \"b.txt\", \"C:\", \"C:\\\\\", \"c:/\", \"\\\\\\\\server\\\\share\", \"\\\\\\\\?\\\\C:\\\\x\", \"\\\\\", \"/\", \"dir\\\\sub\", \"a/b\", \"D:rel\", \"C:\\\\a\\\\..\\\\b\", \"\\\\\\\\.\\\\COM1\", \"con\", \"x.\", \"..\\\\y\", \"\\\\\\\\\", \"C:\\\\a\\\\b\\\\\", \"file.TXT\", \"\\\\\\\\?\\\\UNC\\\\s\\\\h\"];
const make = (parts, seps) => { const n = 1 + rnd(4); let s = \"\"; for (let i = 0; i < n; i++) s += (i && rnd(3) ? seps[rnd(seps.length)] : \"\") + parts[rnd(parts.length)]; return s; };
for (const [name, flavour, parts, seps] of [[\"posix\", path.posix, posixParts, [\"/\"]], [\"win32\", path.win32, winParts, [\"\\\\\", \"/\"]]]) {
  for (let i = 0; i < 120; i++) {
    const p = make(parts, seps), q = make(parts, seps), r = make(parts, seps);
    lines.push(name + \" normalize \" + JSON.stringify(p) + \" \" + call(() => flavour.normalize(p)));
    lines.push(name + \" dirname \" + call(() => flavour.dirname(p)) + \" basename \" + call(() => flavour.basename(p)) + \" ext \" + call(() => flavour.extname(p)) + \" abs \" + call(() => flavour.isAbsolute(p)));
    lines.push(name + \" basename-ext \" + JSON.stringify(q) + \" \" + call(() => flavour.basename(p, flavour.extname(p))) + \" \" + call(() => flavour.basename(p, q)));
    lines.push(name + \" parse \" + call(() => flavour.parse(p)) + \" format \" + call(() => flavour.format(flavour.parse(p))));
    lines.push(name + \" join \" + JSON.stringify([p, q, r]) + \" \" + call(() => flavour.join(p, q, r)) + \" \" + call(() => flavour.join(p)) + \" \" + call(() => flavour.join()));
    lines.push(name + \" resolve \" + call(() => flavour.resolve(p, q)) + \" \" + call(() => flavour.resolve(\"/base/dir\", p)) + \" \" + call(() => flavour.resolve(p)));
    lines.push(name + \" relative \" + call(() => flavour.relative(p, q)) + \" \" + call(() => flavour.relative(\"/a/b/c\", p)) + \" \" + call(() => flavour.relative(p, p)));
    lines.push(name + \" namespaced \" + call(() => flavour.toNamespacedPath(p)));
  }
  lines.push(name + \" consts \" + JSON.stringify([flavour.sep, flavour.delimiter, flavour.posix === path.posix, flavour.win32 === path.win32, flavour._makeLong === flavour.toNamespacedPath]));
  lines.push(name + \" keys \" + Object.keys(flavour).sort().join(\" \"));
  for (const fn of [\"normalize\", \"dirname\", \"basename\", \"extname\", \"isAbsolute\", \"parse\", \"join\", \"resolve\", \"relative\", \"toNamespacedPath\", \"format\"]) {
    for (const bad of [undefined, null, 5, {}, [], true]) lines.push(name + \" \" + fn + \" bad \" + String(bad) + \" \" + call(() => flavour[fn](bad)) + (fn === \"relative\" ? \" \" + call(() => flavour.relative(\"a\", bad)) : \"\") + (fn === \"basename\" ? \" \" + call(() => flavour.basename(\"a\", bad)) : \"\"));
  }
  lines.push(name + \" format forms \" + [{ dir: \"/d\", base: \"b.x\" }, { root: \"/\", name: \"n\", ext: \".e\" }, { root: \"/\", name: \"n\", ext: \"e\" }, { dir: \"/\", base: \"b\" }, { name: \"n\" }, {}, { root: \"C:\\\\\", dir: \"C:\\\\d\", base: \"f\" }, { dir: \"d\", root: \"d\", base: \"f\" }].map((o) => call(() => flavour.format(o))).join(\" \"));
}
lines.push(\"default is posix \" + (path === path.posix) + \" \" + (require(\"node:path\") === path) + \" \" + (require(\"path/posix\") === path.posix) + \" \" + (require(\"node:path/win32\") === path.win32) + \" \" + path.resolve(\"x\") + \" \" + path.resolve(\"\"));
lines.push(\"matchesGlob \" + typeof path.matchesGlob);
const groups = new Map();
for (const line of lines) {
  const kind = line.split(\" \").slice(0, 2).join(\" \");
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
"

// generated paths through every function of both flavours; each line is a
// kind of case, how many there were and a hash of what they printed, all
// taken from node 26.11.1
pub fn path_generated_cases_match_node_test() {
  let rt = run_commonjs(node_runtime(), path_generated_script)
  assert out_lines(rt)
    == [
      "posix normalize 126 2169166483",
      "posix dirname 126 952955543",
      "posix basename-ext 120 3923222838",
      "posix parse 126 3711254769",
      "posix join 126 2034907829",
      "posix resolve 126 669827026",
      "posix relative 126 35337823",
      "posix namespaced 120 2936134743",
      "posix consts 1 602227949",
      "posix keys 1 630039296",
      "posix basename 6 3128654275",
      "posix extname 6 1339339214",
      "posix isAbsolute 6 68224172",
      "posix toNamespacedPath 6 2890361265",
      "posix format 7 3935465123",
      "win32 normalize 126 3985398056",
      "win32 dirname 126 680378116",
      "win32 basename-ext 120 1264056395",
      "win32 parse 126 1924822768",
      "win32 join 126 3147555912",
      "win32 resolve 126 144449529",
      "win32 relative 126 2102951060",
      "win32 namespaced 120 4109347973",
      "win32 consts 1 2455246637",
      "win32 keys 1 1126941460",
      "win32 basename 6 3222937739",
      "win32 extname 6 3781504598",
      "win32 isAbsolute 6 3323894116",
      "win32 toNamespacedPath 6 2379973345",
      "win32 format 7 249379150",
      "default is 1 3459091138",
      "matchesGlob function 1 3450377924",
    ]
}

pub fn path_matches_glob_is_not_there_yet_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "try { require('path').matchesGlob('a', '*'); } catch (error) { out.push(error.code); }",
    )
  assert out_lines(rt) == ["ERR_METHOD_NOT_IMPLEMENTED"]
}

const net_generated_script = "
const net = require(\"net\");
let seed = 9001;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (list) => list[rnd(list.length)];
const lines = [];
const call = (f) => { try { const v = f(); return v === undefined ? \"undefined\" : JSON.stringify(v); } catch (e) { return e.name + \" \" + e.code + \" \" + e.message.replace(/ Received (SocketAddress )?\\{[\\s\\S]*$/, \"\"); } };
// node keeps address rules in a hash table, so their order means nothing
const ordered = (rules) => rules.filter((r) => r.startsWith(\"Address\")).sort().concat(rules.filter((r) => !r.startsWith(\"Address\")));
const v4 = () => pick([\"1.2.3.4\", \"10.0.0.1\", \"10.0.0.9\", \"10.1.2.3\", \"192.168.1.1\", \"127.0.0.1\", \"0.0.0.0\", \"255.255.255.255\", \"8.8.8.8\", \"172.16.5.4\", \"01.2.3.4\", \"1.2.3\", \"1.2.3.4.5\", \"256.1.1.1\", \"1.2.3.4 \", \"\", \"a.b.c.d\", \"1..2.3\", String(rnd(256)) + \".\" + rnd(256) + \".\" + rnd(256) + \".\" + rnd(256)]);
const v6 = () => pick([\"::\", \"::1\", \"fe80::1\", \"FE80::A:b\", \"::ffff:1.2.3.4\", \"::ffff:10.0.0.5\", \"::1.2.3.4\", \"2001:db8::\", \"2001:db8:0:0:1:0:0:1\", \"1:2:3:4:5:6:7:8\", \"1:2:3:4:5:6:7::\", \"::2:3:4:5:6:7:8\", \"1::8\", \"1:0:0:2:0:0:0:3\", \"0:0:1:0:0:1:0:0\", \"fc00::1\", \"fe80::1%eth0\", \"::ffff:0:1\", \"1:2:3:4:5:6:1.2.3.4\", \"1:2:3:4:5:6:7\", \"1:2:3:4:5:6:7:8:9\", \":::\", \"1::2::3\", \"12345::\", \"g::1\", \"::1.2.3\", \"1:2:3:4:5:6:7:1.2.3.4\", \":1\", \"1:\", \"\", rnd(65536).toString(16) + \"::\" + rnd(65536).toString(16)]);
const any = () => (rnd(2) ? v4() : v6());
for (let i = 0; i < 200; i++) { const a = any(); lines.push(\"isIP \" + JSON.stringify(a) + \" \" + net.isIP(a) + \" \" + net.isIPv4(a) + \" \" + net.isIPv6(a)); }
for (const odd of [undefined, null, 5, {}, [], true, { toString() { return \"::1\"; } }, [\"1.2.3.4\"]]) lines.push(\"isIP odd \" + net.isIP(odd) + \" \" + net.isIPv4(odd) + \" \" + net.isIPv6(odd));
for (let i = 0; i < 200; i++) {
  const family = pick([\"ipv4\", \"ipv6\", \"IPv4\", \"IPV6\", undefined, \"ipv5\", 4]);
  const address = rnd(8) === 0 ? undefined : any();
  const port = pick([undefined, 0, 80, 65535, 65536, -1, \"443\", \"\", \"x\", 1.5, null]);
  const flowlabel = pick([undefined, 0, 7, 4294967295, 4294967296, -1, 1.5, \"1\"]);
  const options = {};
  if (family !== undefined) options.family = family;
  if (address !== undefined) options.address = address;
  if (port !== undefined) options.port = port;
  if (flowlabel !== undefined) options.flowlabel = flowlabel;
  lines.push(\"address \" + JSON.stringify(options) + \" \" + call(() => { const s = new net.SocketAddress(options); return [s.address, s.port, s.family, s.flowlabel, s.toJSON()]; }));
}
for (let round = 0; round < 40; round++) {
  const list = new net.BlockList();
  const log = [];
  const steps = 3 + rnd(10);
  for (let i = 0; i < steps; i++) {
    const fam = pick([\"ipv4\", \"ipv6\", undefined]);
    const addr = () => (fam === \"ipv6\" ? v6() : fam === \"ipv4\" ? v4() : any());
    const args = (list) => (fam === undefined ? list : [...list, fam]);
    const prefix = pick([0, 1, 7, 8, 12, 16, 24, 31, 32, 33, 64, 96, 100, 127, 128, 129, -1, 1.5, \"8\"]);
    switch (rnd(11)) {
      case 0: { const a = addr(); log.push(\"addAddress \" + a + \" \" + call(() => list.addAddress(...args([a])))); break; }
      case 1: { const a = addr(), b = addr(); log.push(\"addRange \" + a + \" \" + b + \" \" + call(() => list.addRange(...args([a, b])))); break; }
      case 2: { const a = addr(); log.push(\"addSubnet \" + a + \"/\" + prefix + \" \" + call(() => list.addSubnet(...args([a, prefix])))); break; }
      case 3: { const a = any() + pick([\"/8\", \"/24\", \"/64\", \"/128\", \"/129\", \"\", \"/x\", \"/08\", \"/ 8\"]); log.push(\"addCIDR \" + a + \" \" + call(() => list.addCIDR(a))); break; }
      case 4: { const a = addr(); log.push(\"removeAddress \" + a + \" \" + call(() => list.removeAddress(...args([a])))); break; }
      case 5: { const a = addr(), b = addr(); log.push(\"removeRange \" + a + \" \" + b + \" \" + call(() => list.removeRange(...args([a, b])))); break; }
      case 6: { const a = addr(); log.push(\"removeSubnet \" + a + \"/\" + prefix + \" \" + call(() => list.removeSubnet(...args([a, prefix])))); break; }
      case 7: { const a = [addr(), addr()]; log.push(\"addAddresses \" + a + \" \" + call(() => list.addAddresses(...args([a])))); break; }
      case 8: { const a = [any() + \"/16\", any() + \"/64\"]; log.push(\"addCIDRs \" + a + \" \" + call(() => list.addCIDRs(a))); break; }
      case 9: { const a = new net.SocketAddress(rnd(2) ? { address: \"10.0.0.5\" } : { address: \"::ffff:10.0.0.5\", family: \"ipv6\" }); log.push(\"addAddress object \" + call(() => list.addAddress(a))); break; }
      default: { const a = \"10.0.0.\" + rnd(12); log.push(\"addAddress dup \" + call(() => list.addAddress(a))); }
    }
  }
  lines.push(\"list \" + round + \" \" + log.join(\" | \"));
  lines.push(\"rules \" + round + \" \" + list.size + \" \" + JSON.stringify(ordered(list.rules)));
  const checks = [];
  for (let i = 0; i < 40; i++) {
    const a = any();
    const fam = pick([\"ipv4\", \"ipv6\", undefined, \"IPv6\", \"banana\"]);
    checks.push(call(() => (fam === undefined ? list.check(a) : list.check(a, fam))));
  }
  checks.push(call(() => list.check(new net.SocketAddress({ address: \"10.0.0.5\" }))), call(() => list.check(new net.SocketAddress({ address: \"::ffff:10.0.0.5\", family: \"ipv6\" }))));
  lines.push(\"check \" + round + \" \" + checks.join(\"\"));
  const copy = new net.BlockList();
  lines.push(\"json \" + round + \" \" + call(() => { copy.fromJSON(JSON.stringify(ordered(list.toJSON()))); return ordered(copy.rules); }));
  list.clear();
  lines.push(\"clear \" + round + \" \" + list.size + JSON.stringify(list.rules));
}
const t = (label, f) => lines.push(label + \" \" + call(f));
t(\"check bad address type\", () => new net.BlockList().check(5));
t(\"check bad family type\", () => new net.BlockList().check(\"1.1.1.1\", 5));
t(\"addAddress bad type\", () => new net.BlockList().addAddress(5));
t(\"addAddresses bad\", () => new net.BlockList().addAddresses(\"1.1.1.1\"));
t(\"addAddresses bad item\", () => new net.BlockList().addAddresses([\"1.1.1.1\", 5]));
t(\"addCIDRs bad item\", () => new net.BlockList().addCIDRs([\"1.1.1.1/8\", 5]));
t(\"addCIDR bad type\", () => new net.BlockList().addCIDR(5));
t(\"fromJSON bad\", () => new net.BlockList().fromJSON(5));
t(\"fromJSON bad item\", () => new net.BlockList().fromJSON([\"a\", 5]));
t(\"fromJSON not array\", () => new net.BlockList().fromJSON(\"{}\"));
t(\"fromJSON not json\", () => { try { new net.BlockList().fromJSON(\"nope\"); } catch (e) { return e.name; } });
t(\"statics\", () => [net.BlockList.PRIVATE_RANGES, Object.isFrozen(net.BlockList.PRIVATE_RANGES), net.BlockList.isBlockList(new net.BlockList()), net.BlockList.isBlockList({}), net.BlockList.isBlockList(null), net.SocketAddress.isSocketAddress(new net.SocketAddress()), net.SocketAddress.isSocketAddress(undefined)]);
t(\"private ranges work\", () => { const b = new net.BlockList(); b.addCIDRs(net.BlockList.PRIVATE_RANGES); return [\"10.1.1.1\", \"172.20.0.1\", \"192.168.0.9\", \"127.0.0.1\", \"169.254.1.1\", \"8.8.8.8\", \"172.32.0.1\"].map((a) => b.check(a)).concat([\"::1\", \"fe80::5\", \"fd00::1\", \"2001:db8::1\", \"::ffff:10.1.1.1\"].map((a) => b.check(a, \"ipv6\"))); });
t(\"shape\", () => [Object.getOwnPropertyNames(net.BlockList.prototype).join(\" \"), Object.getOwnPropertyNames(net.SocketAddress.prototype).join(\" \"), net.BlockList.name, net.BlockList.length, net.SocketAddress.length]);
const groups = new Map();
for (const line of lines) {
  const kind = line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
"

// generated addresses and block lists; each line is a kind of case, how many
// there were and a hash of what they printed, all taken from node 26.11.1.
// Address rules are sorted first: node lists them in hash table order
pub fn net_generated_cases_match_node_test() {
  let rt = run_commonjs(node_runtime(), net_generated_script)
  assert out_lines(rt)
    == [
      "isIP 208 1649224602",
      "address 200 811629502",
      "list 40 3434433788",
      "rules 40 3816565543",
      "check 42 711282156",
      "json 40 4001462177",
      "clear 40 3996444319",
      "addAddress 1 2138075090",
      "addAddresses 2 235182210",
      "addCIDRs 1 2696499320",
      "addCIDR 1 113014980",
      "fromJSON 4 3436466889",
      "statics 1 2762736012",
      "private 1 3877025246",
      "shape 1 676877096",
    ]
}

pub fn net_opens_nothing_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "out.push(Object.keys(require('net')).sort().join(' '))",
    )
  assert out_lines(rt) == ["BlockList SocketAddress isIP isIPv4 isIPv6"]
}
