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
import spark/builtin.{type Builtin, Builtin}
import spark/encoding
import spark/fs
import spark/global
import spark/io.{
  type Io, type ReplySlip, type Want, Access, Done, Failure, Io, Realpath,
  Stderr, Stdout, Text, Write,
}
import spark/os.{BigEndian, Os}
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
      "TypeError [ERR_INVALID_THIS]: Value of \"this\" must be of type TextEncoder code",
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
