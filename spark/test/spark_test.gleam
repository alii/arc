import arc/engine.{type Engine, Returned, Threw}
import arc/host.{AlreadySettled, Resumed}
import arc/host_hooks.{HostHooks}
import arc/module/loader
import arc/rt/types.{KStr, classify}
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import spark.{
  type Runtime, DuplicateBuiltin, Finished, MissingDependency, WakeIn,
}
import spark/async_hooks
import spark/buffer
import spark/builtin.{type Builtin, Builtin}
import spark/crypto
import spark/dom_events
import spark/domexception
import spark/encoding
import spark/events
import spark/fetch
import spark/fs
import spark/global
import spark/io.{
  type Answer, type Failure, type FileInfo, type Io, type ReplySlip, type Want,
  Access, Bytes, Directory, Done, DropBody, Entries, Entry, Failure, Fetch, File,
  FileInfo, Head, Info, Io, Random, ReadBody, ReadFile, Readdir, Readlink,
  Realpath, Stat, Stderr, Stdout, Symlink, Text, Write,
}
import spark/net
import spark/os.{BigEndian, Os, User}
import spark/path
import spark/perf_hooks
import spark/platform.{type Platform, Darwin, Linux}
import spark/process.{Process}
import spark/querystring
import spark/stream
import spark/string_decoder
import spark/structured_clone
import spark/timers
import spark/tls.{Certificates}
import spark/tty
import spark/url
import spark/util
import spark/web_streams
import spark/zlib

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

@external(erlang, "spark_test_ffi", "stash")
fn stash(key: Int, piece: BitArray) -> Nil

@external(erlang, "spark_test_ffi", "unstash")
fn unstash(key: Int) -> Result(BitArray, Nil)

fn builtins() -> List(Builtin(Nil)) {
  [
    global.builtin(),
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
    querystring.internal_builtin(),
    querystring.builtin(),
    url.builtin(),
    events.builtin(),
    string_decoder.builtin(),
    util.builtin(),
    stream.builtin(),
    stream.promises_builtin(),
    stream.consumers_builtin(),
    structured_clone.builtin(),
    zlib.builtin(),
    util.types_builtin(),
    web_streams.builtin(),
    fetch.builtin(),
    crypto.builtin(),
    tty.builtin([1]),
    tls.builtin(
      Certificates(
        bundled: [
          "-----BEGIN CERTIFICATE-----\nbundled one\n-----END CERTIFICATE-----",
        ],
        system: [
          "-----BEGIN CERTIFICATE-----\nsystem one\n-----END CERTIFICATE-----",
        ],
        extra: [
          "-----BEGIN CERTIFICATE-----\nextra one\n-----END CERTIFICATE-----",
        ],
      ),
    ),
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
    user: User(
      uid: 1001,
      gid: 100,
      username: "glenda",
      shell: option.Some("/bin/rc"),
    ),
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

// a made-up disk: /there exists, /secret is refused, /tree is the small
// tree below, nothing else exists; wants through the later door are kept
// until the test answers them
fn fake_io() -> Io {
  Io(now: fake_answer, later: fn(want, slip) { keep(#(want, slip)) })
}

fn fake_answer(want: Want) -> Result(Answer, Failure) {
  case want {
    Access(path: "/there", ..) -> Ok(Done)
    Access(path: "/secret", ..) -> Error(Failure("EACCES"))
    Access(path: "/tree" <> _ as path, ..) -> {
      use real <- result.try(made_real(parts(path), "", True, 0))
      use _ <- result.map(made_at(real))
      Done
    }
    Access(..) -> Error(Failure("ENOENT"))
    Realpath(path: "/link") -> Ok(Text("/real/place"))
    Realpath(path: "/tree" <> _ as path) -> {
      use real <- result.try(made_real(parts(path), "", True, 0))
      use _ <- result.map(made_at(real))
      Text(real)
    }
    Realpath(..) -> Error(Failure("ENOENT"))
    Stat(path:, follow:) -> {
      use real <- result.try(made_real(parts(path), "", follow, 0))
      use made <- result.map(made_at(real))
      Info(made_info(made))
    }
    Readlink(path:) -> {
      use real <- result.try(made_real(parts(path), "", False, 0))
      case made_at(real) {
        Ok(MadeLink(target)) -> Ok(Text(target))
        Ok(_) -> Error(Failure("EINVAL"))
        Error(failure) -> Error(failure)
      }
    }
    Readdir(path:) -> {
      use real <- result.try(made_real(parts(path), "", True, 0))
      case made_at(real) {
        Ok(MadeDir) ->
          Ok(
            Entries(
              list.filter_map(made_tree(), fn(item) {
                case string.split_once(item.0, real <> "/") {
                  Ok(#("", name)) ->
                    case string.contains(name, "/") {
                      True -> Error(Nil)
                      False -> Ok(Entry(name:, kind: made_info(item.1).kind))
                    }
                  _ -> Error(Nil)
                }
              }),
            ),
          )
        Ok(_) -> Error(Failure("ENOTDIR"))
        Error(failure) -> Error(failure)
      }
    }
    ReadFile(path:) -> {
      use real <- result.try(made_real(parts(path), "", True, 0))
      case made_at(real) {
        Ok(MadeFile(bytes)) -> Ok(Bytes(bytes))
        Ok(_) -> Error(Failure("EISDIR"))
        Error(failure) -> Error(failure)
      }
    }
    // a made-up network: one host, a few pages, each body a numbered list of
    // pieces put by when the page is asked for
    Fetch(url: "http://made.up/" <> page, method:, headers:, body:) -> {
      note("fetch " <> method <> " /" <> page)
      case page {
        "hello" -> {
          stash(1, <<"hel":utf8>>)
          stash(1, <<"lo":utf8>>)
          Ok(Head(
            status: 200,
            reason: "OK",
            headers: [
              #("Content-Type", "text/plain"),
              #("Set-Cookie", "a=1"),
              #("Set-Cookie", "b=2"),
            ],
            body: 1,
          ))
        }
        "moved" ->
          Ok(Head(
            status: 302,
            reason: "Found",
            headers: [#("Location", "/hello")],
            body: 2,
          ))
        "echo" ->
          Ok(Head(
            status: 200,
            reason: "OK",
            headers: [
              #("x-method", method),
              #("x-body", text(body)),
              #(
                "x-sent",
                string.join(
                  list.map(headers, fn(header) { header.0 <> "=" <> header.1 }),
                  "; ",
                ),
              ),
            ],
            body: 3,
          ))
        "packed" -> {
          stash(4, <<
            0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x2b,
            0xcd, 0x2b, 0x48, 0x4c, 0xce, 0x4e, 0x4d, 0x51, 0x28, 0x49, 0xad,
          >>)
          stash(4, <<
            0x28, 0xd1, 0x51, 0x28, 0x45, 0xe6, 0x02, 0x00, 0x59, 0xed, 0xdf,
            0xbf, 0x1c, 0x00, 0x00, 0x00,
          >>)
          Ok(Head(
            status: 200,
            reason: "OK",
            headers: [#("Content-Encoding", "gzip")],
            body: 4,
          ))
        }
        "missing" -> {
          stash(5, <<"no such page":utf8>>)
          Ok(Head(status: 404, reason: "Not Found", headers: [], body: 5))
        }
        "endless" -> Ok(Head(status: 200, reason: "OK", headers: [], body: 6))
        _ -> Error(Failure("ECONNRESET"))
      }
    }
    Fetch(..) -> Error(Failure("ECONNREFUSED"))
    ReadBody(body: 6) -> Ok(Bytes(<<"tick\n":utf8>>))
    ReadBody(body:) ->
      case unstash(body) {
        Ok(piece) -> Ok(Bytes(piece))
        Error(Nil) -> Ok(Done)
      }
    DropBody(body:) -> {
      note("drop " <> int.to_string(body))
      Ok(Done)
    }
    // not random at all, so a test sees the same bytes every time
    Random(bytes:) -> {
      let seed = case unstash(random_seed) {
        Ok(<<seed:32>>) -> seed
        _ -> 20_261_011
      }
      let #(made, seed) = scrambled(bytes, seed, <<>>)
      stash(random_seed, <<seed:32>>)
      Ok(Bytes(made))
    }
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
}

// where the made-up random source keeps its place between asks
const random_seed = 999

// bytes that look random and are the same on every run: each is the top of
// the next step of a small generator
fn scrambled(left: Int, seed: Int, acc: BitArray) -> #(BitArray, Int) {
  case left {
    0 -> #(acc, seed)
    _ -> {
      let seed = { seed * 1_103_515_245 + 12_345 } % 2_147_483_648
      scrambled(left - 1, seed, <<acc:bits, { seed / 65_536 % 256 }:8>>)
    }
  }
}

type Made {
  MadeDir
  MadeFile(BitArray)
  MadeLink(String)
}

// the same tree as the real one the expected output of the file tests was
// made from in node
fn made_tree() -> List(#(String, Made)) {
  [
    #("/tree", MadeDir),
    #("/tree/a.txt", MadeFile(<<"hello\n":utf8>>)),
    #("/tree/empty", MadeFile(<<>>)),
    #("/tree/caf\u{e9}.txt", MadeFile(<<"\u{e9}t\u{e9}":utf8>>)),
    #("/tree/sub", MadeDir),
    #("/tree/sub/inner.bin", MadeFile(<<0, 1, 2, 255>>)),
    #("/tree/sub/deep", MadeDir),
    #("/tree/sub/deep/leaf.txt", MadeFile(<<"leaf":utf8>>)),
    #("/tree/link", MadeLink("a.txt")),
    #("/tree/dirlink", MadeLink("sub")),
    #("/tree/broken", MadeLink("nowhere")),
  ]
}

fn made_at(path: String) -> Result(Made, Failure) {
  list.key_find(made_tree(), path) |> result.replace_error(Failure("ENOENT"))
}

fn parts(path: String) -> List(String) {
  string.split(path, "/") |> list.filter(fn(part) { part != "" })
}

// the path with its symlinks followed, one part at a time; the last part
// only when `follow_last`
fn made_real(
  left: List(String),
  done: String,
  follow_last: Bool,
  hops: Int,
) -> Result(String, Failure) {
  case left, hops > 20 {
    _, True -> Error(Failure("ELOOP"))
    [], False -> Ok(done)
    [part, ..rest], False -> {
      let here = done <> "/" <> part
      case made_at(here), rest {
        Ok(MadeLink(_)), [] if !follow_last -> Ok(here)
        Ok(MadeLink(target)), _ ->
          made_real(
            list.append(parts(target), rest),
            done,
            follow_last,
            hops + 1,
          )
        Ok(MadeFile(_)), [_, ..] -> Error(Failure("ENOTDIR"))
        Ok(_), _ -> made_real(rest, here, follow_last, hops)
        Error(failure), _ -> Error(failure)
      }
    }
  }
}

fn made_info(made: Made) -> FileInfo {
  let #(kind, permissions, size) = case made {
    MadeDir -> #(Directory, 0o755, 96)
    MadeFile(bytes) -> #(File, 0o644, bit_array.byte_size(bytes))
    MadeLink(target) -> #(Symlink, 0o755, string.byte_size(target))
  }
  let time = 1_700_000_000_123_456_789
  FileInfo(
    kind:,
    permissions:,
    size:,
    dev: 1,
    ino: 7,
    nlink: 1,
    uid: 501,
    gid: 20,
    rdev: 0,
    blksize: 4096,
    blocks: 8,
    atime: time,
    mtime: time,
    ctime: time,
    birthtime: time,
  )
}

// answers every want that went through the later door from the made-up
// disk, until the script has nothing left to do
fn run_answering(rt: Runtime(Nil)) -> Runtime(Nil) {
  let rt = run_to_end(rt)
  case kept() {
    [] -> rt
    asked ->
      run_answering(
        list.fold(asked, rt, fn(rt, one) {
          spark.answer(rt, one.1, fake_answer(one.0)).1
        }),
      )
  }
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
    optional: [],
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
      "ok /real/place",
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
        const given = globalThis.structuredClone;
        delete globalThis.structuredClone;
        out.push(performance.mark('plain').detail);
        out.push(describe(() => performance.mark('with detail', { detail: { n: 1 } })));
        globalThis.structuredClone = given;
        const detail = { n: 1 };
        const kept = performance.mark('with detail', { detail }).detail;
        out.push(JSON.stringify(kept) + ' ' + (kept !== detail));
        ")
  assert out_lines(rt)
    == [
      "",
      "Error [ERR_METHOD_NOT_IMPLEMENTED]: The structuredClone method is not implemented code",
      "{\"n\":1} true",
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

const url_class_script = "
const parsing = [[\"http://example\\t.\\norg\",\"http://example.org/foo/bar\"],[\"http://user:pass@foo:21/bar;par?b#c\",\"http://example.org/foo/bar\"],[\"https://test:@test\",null],[\"https://:@test\",null],[\"non-special://test:@test/x\",null],[\"non-special://:@test/x\",null],[\"http:foo.com\",\"http://example.org/foo/bar\"],[\"\\t   :foo.com   \\n\",\"http://example.org/foo/bar\"],[\" foo.com  \",\"http://example.org/foo/bar\"],[\"a:\\t foo.com\",\"http://example.org/foo/bar\"],[\"http://f:21/ b ? d # e \",\"http://example.org/foo/bar\"],[\"lolscheme:x x#x x\",null],[\"http://f:/c\",\"http://example.org/foo/bar\"],[\"http://f:0/c\",\"http://example.org/foo/bar\"],[\"http://f:00000000000000/c\",\"http://example.org/foo/bar\"],[\"http://f:00000000000000000000080/c\",\"http://example.org/foo/bar\"],[\"http://f:b/c\",\"http://example.org/foo/bar\"],[\"http://f: /c\",\"http://example.org/foo/bar\"],[\"http://f:\\n/c\",\"http://example.org/foo/bar\"],[\"http://f:fifty-two/c\",\"http://example.org/foo/bar\"],[\"http://f:999999/c\",\"http://example.org/foo/bar\"],[\"non-special://f:999999/c\",\"http://example.org/foo/bar\"],[\"http://f: 21 / b ? d # e \",\"http://example.org/foo/bar\"],[\"\",\"http://example.org/foo/bar\"],[\"  \\t\",\"http://example.org/foo/bar\"],[\":foo.com/\",\"http://example.org/foo/bar\"],[\":foo.com\\\\\",\"http://example.org/foo/bar\"],[\":\",\"http://example.org/foo/bar\"],[\":a\",\"http://example.org/foo/bar\"],[\":/\",\"http://example.org/foo/bar\"],[\":\\\\\",\"http://example.org/foo/bar\"],[\":#\",\"http://example.org/foo/bar\"],[\"#\",\"http://example.org/foo/bar\"],[\"#/\",\"http://example.org/foo/bar\"],[\"#\\\\\",\"http://example.org/foo/bar\"],[\"#;?\",\"http://example.org/foo/bar\"],[\"?\",\"http://example.org/foo/bar\"],[\"/\",\"http://example.org/foo/bar\"],[\":23\",\"http://example.org/foo/bar\"],[\"/:23\",\"http://example.org/foo/bar\"],[\"\\\\x\",\"http://example.org/foo/bar\"],[\"\\\\\\\\x\\\\hello\",\"http://example.org/foo/bar\"],[\"::\",\"http://example.org/foo/bar\"],[\"::23\",\"http://example.org/foo/bar\"],[\"foo://\",\"http://example.org/foo/bar\"],[\"http://a:b@c:29/d\",\"http://example.org/foo/bar\"],[\"http://\\u00e9@\\u00e9\",null],[\"http://\\u00e9@example.com\",null],[\"http::@c:29\",\"http://example.org/foo/bar\"],[\"http://&a:foo(b]c@d:2/\",\"http://example.org/foo/bar\"],[\"http://::@c@d:2\",\"http://example.org/foo/bar\"],[\"http://foo.com:b@d/\",\"http://example.org/foo/bar\"],[\"http://foo.com/\\\\@\",\"http://example.org/foo/bar\"],[\"http:\\\\\\\\foo.com\\\\\",\"http://example.org/foo/bar\"],[\"http:\\\\\\\\a\\\\b:c\\\\d@foo.com\\\\\",\"http://example.org/foo/bar\"],[\"http://a:b@c\\\\\",null],[\"ws://a@b\\\\c\",null],[\"foo:/\",\"http://example.org/foo/bar\"],[\"foo:/bar.com/\",\"http://example.org/foo/bar\"],[\"foo://///////\",\"http://example.org/foo/bar\"],[\"foo://///////bar.com/\",\"http://example.org/foo/bar\"],[\"foo:////://///\",\"http://example.org/foo/bar\"],[\"c:/foo\",\"http://example.org/foo/bar\"],[\"//foo/bar\",\"http://example.org/foo/bar\"],[\"http://foo/path;a??e#f#g\",\"http://example.org/foo/bar\"],[\"http://foo/abcd?efgh?ijkl\",\"http://example.org/foo/bar\"],[\"http://foo/abcd#foo?bar\",\"http://example.org/foo/bar\"],[\"[61:24:74]:98\",\"http://example.org/foo/bar\"],[\"http:[61:27]/:foo\",\"http://example.org/foo/bar\"],[\"http://[1::2]:3:4\",\"http://example.org/foo/bar\"],[\"http://2001::1\",\"http://example.org/foo/bar\"],[\"http://2001::1]\",\"http://example.org/foo/bar\"],[\"http://2001::1]:80\",\"http://example.org/foo/bar\"],[\"http://[2001::1]\",\"http://example.org/foo/bar\"],[\"http://[::127.0.0.1]\",\"http://example.org/foo/bar\"],[\"http://[::127.0.0.1.]\",\"http://example.org/foo/bar\"],[\"http://[0:0:0:0:0:0:13.1.68.3]\",\"http://example.org/foo/bar\"],[\"http://[2001::1]:80\",\"http://example.org/foo/bar\"],[\"http:/example.com/\",\"http://example.org/foo/bar\"],[\"http:/\",\"http://example.com/\"],[\"ftp:/example.com/\",\"http://example.org/foo/bar\"],[\"https:/example.com/\",\"http://example.org/foo/bar\"],[\"madeupscheme:/example.com/\",\"http://example.org/foo/bar\"],[\"file:/example.com/\",\"http://example.org/foo/bar\"],[\"file://example:1/\",null],[\"file://example:test/\",null],[\"file://example%/\",null],[\"file://[example]/\",null],[\"ftps:/example.com/\",\"http://example.org/foo/bar\"],[\"gopher:/example.com/\",\"http://example.org/foo/bar\"],[\"ws:/example.com/\",\"http://example.org/foo/bar\"],[\"wss:/example.com/\",\"http://example.org/foo/bar\"],[\"data:/example.com/\",\"http://example.org/foo/bar\"],[\"javascript:/example.com/\",\"http://example.org/foo/bar\"],[\"mailto:/example.com/\",\"http://example.org/foo/bar\"],[\"http:example.com/\",\"http://example.org/foo/bar\"],[\"ftp:example.com/\",\"http://example.org/foo/bar\"],[\"https:example.com/\",\"http://example.org/foo/bar\"],[\"madeupscheme:example.com/\",\"http://example.org/foo/bar\"],[\"ftps:example.com/\",\"http://example.org/foo/bar\"],[\"gopher:example.com/\",\"http://example.org/foo/bar\"],[\"ws:example.com/\",\"http://example.org/foo/bar\"],[\"wss:example.com/\",\"http://example.org/foo/bar\"],[\"data:example.com/\",\"http://example.org/foo/bar\"],[\"javascript:example.com/\",\"http://example.org/foo/bar\"],[\"mailto:example.com/\",\"http://example.org/foo/bar\"],[\"/a/b/c\",\"http://example.org/foo/bar\"],[\"/a/ /c\",\"http://example.org/foo/bar\"],[\"/a%2fc\",\"http://example.org/foo/bar\"],[\"/a/%2f/c\",\"http://example.org/foo/bar\"],[\"#\\u03b2\",\"http://example.org/foo/bar\"],[\"data:text/html,test#test\",\"http://example.org/foo/bar\"],[\"tel:1234567890\",\"http://example.org/foo/bar\"],[\"ssh://example.com/foo/bar.git\",\"http://example.org/\"],[\"file:c:\\\\foo\\\\bar.html\",\"file:///tmp/mock/path\"],[\"  File:c|////foo\\\\bar.html\",\"file:///tmp/mock/path\"],[\"C|/foo/bar\",\"file:///tmp/mock/path\"],[\"/C|\\\\foo\\\\bar\",\"file:///tmp/mock/path\"],[\"//C|/foo/bar\",\"file:///tmp/mock/path\"],[\"//server/file\",\"file:///tmp/mock/path\"],[\"\\\\\\\\server\\\\file\",\"file:///tmp/mock/path\"],[\"/\\\\server/file\",\"file:///tmp/mock/path\"],[\"file:///foo/bar.txt\",\"file:///tmp/mock/path\"],[\"file:///home/me\",\"file:///tmp/mock/path\"],[\"//\",\"file:///tmp/mock/path\"],[\"///\",\"file:///tmp/mock/path\"],[\"///test\",\"file:///tmp/mock/path\"],[\"file://test\",\"file:///tmp/mock/path\"],[\"file://localhost\",\"file:///tmp/mock/path\"],[\"file://localhost/\",\"file:///tmp/mock/path\"],[\"file://localhost/test\",\"file:///tmp/mock/path\"],[\"test\",\"file:///tmp/mock/path\"],[\"file:test\",\"file:///tmp/mock/path\"],[\"file:///w|m\",null],[\"file:///w||m\",null],[\"file:///w|/m\",null],[\"file:C|/m/\",null],[\"file:C||/m/\",null],[\"http://example.com/././foo\",null],[\"http://example.com/./.foo\",null],[\"http://example.com/foo/.\",null],[\"http://example.com/foo/./\",null],[\"http://example.com/foo/bar/..\",null],[\"http://example.com/foo/bar/../\",null],[\"http://example.com/foo/..bar\",null],[\"http://example.com/foo/bar/../ton\",null],[\"http://example.com/foo/bar/../ton/../../a\",null],[\"http://example.com/foo/../../..\",null],[\"http://example.com/foo/../../../ton\",null],[\"http://example.com/foo/%2e\",null],[\"http://example.com/foo/%2e%2\",null],[\"http://example.com/foo/%2e./%2e%2e/.%2e/%2e.bar\",null],[\"http://example.com////../..\",null],[\"http://example.com/foo/bar//../..\",null],[\"http://example.com/foo/bar//..\",null],[\"http://example.com/foo\",null],[\"http://example.com/%20foo\",null],[\"http://example.com/foo%\",null],[\"http://example.com/foo%2\",null],[\"http://example.com/foo%2zbar\",null],[\"http://example.com/foo%2\\u00c2\\u00a9zbar\",null],[\"http://example.com/foo%41%7a\",null],[\"http://example.com/foo\\t\\u0091%91\",null],[\"http://example.com/foo%00%51\",null],[\"http://example.com/(%28:%3A%29)\",null],[\"http://example.com/%3A%3a%3C%3c\",null],[\"http://example.com/foo\\tbar\",null],[\"http://example.com\\\\\\\\foo\\\\\\\\bar\",null],[\"http://example.com/%7Ffp3%3Eju%3Dduvgw%3Dd\",null],[\"http://example.com/@asdf%40\",null],[\"http://example.com/\\u4f60\\u597d\\u4f60\\u597d\",null],[\"http://example.com/\\u2025/foo\",null],[\"http://example.com/\\ufeff/foo\",null],[\"http://example.com/\\u202e/foo/\\u202d/bar\",null],[\"http://www.google.com/foo?bar=baz#\",null],[\"http://www.google.com/foo?bar=baz# \\u00bb\",null],[\"data:test# \\u00bb\",null],[\"http://www.google.com\",null],[\"http://192.0x00A80001\",null],[\"http://www/foo%2Ehtml\",null],[\"http://www/foo/%2E/html\",null],[\"http://user:pass@/\",null],[\"http://%25DOMAIN:foobar@foodomain.com/\",null],[\"http:\\\\\\\\www.google.com\\\\foo\",null],[\"http://foo:80/\",null],[\"http://foo:81/\",null],[\"httpa://foo:80/\",null],[\"http://foo:-80/\",null],[\"https://foo:443/\",null],[\"https://foo:80/\",null],[\"ftp://foo:21/\",null],[\"ftp://foo:80/\",null],[\"gopher://foo:70/\",null],[\"gopher://foo:443/\",null],[\"ws://foo:80/\",null],[\"ws://foo:81/\",null],[\"ws://foo:443/\",null],[\"ws://foo:815/\",null],[\"wss://foo:80/\",null],[\"wss://foo:81/\",null],[\"wss://foo:443/\",null],[\"wss://foo:815/\",null],[\"http:/example.com/\",null],[\"ftp:/example.com/\",null],[\"https:/example.com/\",null],[\"madeupscheme:/example.com/\",null],[\"file:/example.com/\",null],[\"ftps:/example.com/\",null],[\"gopher:/example.com/\",null],[\"ws:/example.com/\",null],[\"wss:/example.com/\",null],[\"data:/example.com/\",null],[\"javascript:/example.com/\",null],[\"mailto:/example.com/\",null],[\"http:example.com/\",null],[\"ftp:example.com/\",null],[\"https:example.com/\",null],[\"madeupscheme:example.com/\",null],[\"ftps:example.com/\",null],[\"gopher:example.com/\",null],[\"ws:example.com/\",null],[\"wss:example.com/\",null],[\"data:example.com/\",null],[\"javascript:example.com/\",null],[\"mailto:example.com/\",null],[\"https://example.com/aaa/bbb/%2e%2e?query\",null],[\"http:@www.example.com\",null],[\"http:/@www.example.com\",null],[\"http://@www.example.com\",null],[\"http:a:b@www.example.com\",null],[\"http:/a:b@www.example.com\",null],[\"http://a:b@www.example.com\",null],[\"http://@pple.com\",null],[\"http::b@www.example.com\",null],[\"http:/:b@www.example.com\",null],[\"http://:b@www.example.com\",null],[\"http:/:@/www.example.com\",null],[\"http://user@/www.example.com\",null],[\"http:@/www.example.com\",null],[\"http:/@/www.example.com\",null],[\"http://@/www.example.com\",null],[\"https:@/www.example.com\",null],[\"http:a:b@/www.example.com\",null],[\"http:/a:b@/www.example.com\",null],[\"http://a:b@/www.example.com\",null],[\"http::@/www.example.com\",null],[\"http:a:@www.example.com\",null],[\"http:/a:@www.example.com\",null],[\"http://a:@www.example.com\",null],[\"http://www.@pple.com\",null],[\"http:@:www.example.com\",null],[\"http:/@:www.example.com\",null],[\"http://@:www.example.com\",null],[\"http://:@www.example.com\",null],[\"/\",\"http://www.example.com/test\"],[\"/test.txt\",\"http://www.example.com/test\"],[\".\",\"http://www.example.com/test\"],[\"..\",\"http://www.example.com/test\"],[\"test.txt\",\"http://www.example.com/test\"],[\"./test.txt\",\"http://www.example.com/test\"],[\"../test.txt\",\"http://www.example.com/test\"],[\"../aaa/test.txt\",\"http://www.example.com/test\"],[\"../../test.txt\",\"http://www.example.com/test\"],[\"\\u4e2d/test.txt\",\"http://www.example.com/test\"],[\"http://www.example2.com\",\"http://www.example.com/test\"],[\"//www.example2.com\",\"http://www.example.com/test\"],[\"file:...\",\"http://www.example.com/test\"],[\"file:..\",\"http://www.example.com/test\"],[\"file:a\",\"http://www.example.com/test\"],[\"file:.\",null],[\"file:.\",\"http://www.example.com/test\"],[\"http://ExAmPlE.CoM\",\"http://other.com/\"],[\"http://example example.com\",\"http://other.com/\"],[\"http://Goo%20 goo%7C|.com\",\"http://other.com/\"],[\"http://[]\",\"http://other.com/\"],[\"http://[:]\",\"http://other.com/\"],[\"http://GOO\\u00a0\\u3000goo.com\",\"http://other.com/\"],[\"http://GOO\\u200b\\u2060\\ufeffgoo.com\",\"http://other.com/\"],[\"\\u0000\\u001b\\u0004\\u0012 http://example.com/\\u001f \\r \",null],[\"non-special:opaque  \",null],[\"non-special:opaque  ?hi\",null],[\"non-special:opaque  #hi\",null],[\"non-special:opaque  x?hi\",null],[\"non-special:opaque  x#hi\",null],[\"non-special:opaque \\t\\t  \\t#hi\",null],[\"non-special:opaque \\t\\t  #hi\",null],[\"non-special:opaque\\t\\t  \\r #hi\",null],[\"http://www.foo\\u3002bar.com\",\"http://other.com/\"],[\"http://\\ufdd0zyx.com\",\"http://other.com/\"],[\"http://%ef%b7%90zyx.com\",\"http://other.com/\"],[\"https://\\ufffd\",null],[\"https://%EF%BF%BD\",null],[\"https://x/\\ufffd?\\ufffd#\\ufffd\",null],[\"http://a.b.c.xn--pokxncvks\",null],[\"http://10.0.0.xn--pokxncvks\",null],[\"http://a.b.c.XN--pokxncvks\",null],[\"http://a.b.c.Xn--pokxncvks\",null],[\"http://10.0.0.XN--pokxncvks\",null],[\"http://10.0.0.xN--pokxncvks\",null],[\"http://\\uff27\\uff4f.com\",\"http://other.com/\"],[\"http://\\uff05\\uff14\\uff11.com\",\"http://other.com/\"],[\"http://%ef%bc%85%ef%bc%94%ef%bc%91.com\",\"http://other.com/\"],[\"http://\\uff05\\uff10\\uff10.com\",\"http://other.com/\"],[\"http://%ef%bc%85%ef%bc%90%ef%bc%90.com\",\"http://other.com/\"],[\"http://\\u4f60\\u597d\\u4f60\\u597d\",\"http://other.com/\"],[\"https://fa\\u00df.ExAmPlE/\",null],[\"sc://fa\\u00df.ExAmPlE/\",null],[\"http://%zz%66%a.com\",\"http://other.com/\"],[\"http://%25\",\"http://other.com/\"],[\"http://hello%00\",\"http://other.com/\"],[\"http://%30%78%63%30%2e%30%32%35%30.01\",\"http://other.com/\"],[\"http://%30%78%63%30%2e%30%32%35%30.01%2e\",\"http://other.com/\"],[\"http://192.168.0.257\",\"http://other.com/\"],[\"http://%3g%78%63%30%2e%30%32%35%30%2E.01\",\"http://other.com/\"],[\"http://192.168.0.1 hello\",\"http://other.com/\"],[\"https://x x:12\",null],[\"http://\\uff10\\uff38\\uff43\\uff10\\uff0e\\uff10\\uff12\\uff15\\uff10\\uff0e\\uff10\\uff11\",\"http://other.com/\"],[\"http://./\",null],[\"http://../\",null],[\"h://.\",null],[\"http://[www.google.com]/\",null],[\"http://[google.com]\",\"http://other.com/\"],[\"http://[::1.2.3.4x]\",\"http://other.com/\"],[\"http://[::1.2.3.]\",\"http://other.com/\"],[\"http://[::1.2.]\",\"http://other.com/\"],[\"http://[::.1.2]\",\"http://other.com/\"],[\"http://[::1.]\",\"http://other.com/\"],[\"http://[::.1]\",\"http://other.com/\"],[\"http://[::%31]\",\"http://other.com/\"],[\"http://%5B::1]\",\"http://other.com/\"],[\"http://foo:\\ud83d\\udca9@example.com/bar\",\"http://other.com/\"],[\"http://\\ud83d\\ude00x@host/\",null],[\"http://a:\\ud83d\\ude00x@host/\",null],[\"http://\\ud83d\\ude00@host/\",null],[\"https://localhost?q=\\ud83d\\udd25\",null],[\"https://localhost#\\ud83d\\udd25\",null],[\"#\",\"test:test\"],[\"#x\",\"mailto:x@x.com\"],[\"#x\",\"data:,\"],[\"#x\",\"about:blank\"],[\"#x:y\",\"about:blank\"],[\"#\",\"test:test?test\"],[\"https://@test@test@example:800/\",\"http://doesnotmatter/\"],[\"https://@@@example\",\"http://doesnotmatter/\"],[\"http://`{}:`{}@h/`{}?`{}\",\"http://doesnotmatter/\"],[\"http://host/?'\",null],[\"notspecial://host/?'\",null],[\"/some/path\",\"http://user@example.org/smth\"],[\"\",\"http://user:pass@example.org:21/smth\"],[\"/some/path\",\"http://user:pass@example.org:21/smth\"],[\"i\",\"sc:sd\"],[\"i\",\"sc:sd/sd\"],[\"i\",\"sc:/pa/pa\"],[\"i\",\"sc://ho/pa\"],[\"i\",\"sc:///pa/pa\"],[\"../i\",\"sc:sd\"],[\"../i\",\"sc:sd/sd\"],[\"../i\",\"sc:/pa/pa\"],[\"../i\",\"sc://ho/pa\"],[\"../i\",\"sc:///pa/pa\"],[\"/i\",\"sc:sd\"],[\"/i\",\"sc:sd/sd\"],[\"/i\",\"sc:/pa/pa\"],[\"/i\",\"sc://ho/pa\"],[\"/i\",\"sc:///pa/pa\"],[\"?i\",\"sc:sd\"],[\"?i\",\"sc:sd/sd\"],[\"?i\",\"sc:/pa/pa\"],[\"?i\",\"sc://ho/pa\"],[\"?i\",\"sc:///pa/pa\"],[\"#i\",\"sc:sd\"],[\"#i\",\"sc:sd/sd\"],[\"#i\",\"sc:/pa/pa\"],[\"#i\",\"sc://ho/pa\"],[\"#i\",\"sc:///pa/pa\"],[\"about:/../\",null],[\"data:/../\",null],[\"javascript:/../\",null],[\"mailto:/../\",null],[\"sc://\\u00f1.test/\",null],[\"sc://%/\",null],[\"sc://@/\",null],[\"sc://te@s:t@/\",null],[\"sc://:/\",null],[\"sc://:12/\",null],[\"x\",\"sc://\\u00f1\"],[\"sc:\\\\../\",null],[\"sc::a@example.net\",null],[\"wow:%NBD\",null],[\"wow:%1G\",null],[\"wow:\\uffff\",null],[\"sc://a\\u0000b/\",null],[\"sc://a b/\",null],[\"sc://a<b\",null],[\"sc://a>b\",null],[\"sc://a[b/\",null],[\"sc://a\\\\b/\",null],[\"sc://a]b/\",null],[\"sc://a^b\",null],[\"sc://a|b/\",null],[\"foo://ho\\tst/\",null],[\"foo://ho\\nst/\",null],[\"foo://ho\\rst/\",null],[\"http://a\\u0000b/\",null],[\"http://a\\u0001b/\",null],[\"http://a\\u0002b/\",null],[\"http://a\\u0003b/\",null],[\"http://a\\u0004b/\",null],[\"http://a\\u0005b/\",null],[\"http://a\\u0006b/\",null],[\"http://a\\u0007b/\",null],[\"http://a\\bb/\",null],[\"http://a\\u000bb/\",null],[\"http://a\\fb/\",null],[\"http://a\\u000eb/\",null],[\"http://a\\u000fb/\",null],[\"http://a\\u0010b/\",null],[\"http://a\\u0011b/\",null],[\"http://a\\u0012b/\",null],[\"http://a\\u0013b/\",null],[\"http://a\\u0014b/\",null],[\"http://a\\u0015b/\",null],[\"http://a\\u0016b/\",null],[\"http://a\\u0017b/\",null],[\"http://a\\u0018b/\",null],[\"http://a\\u0019b/\",null],[\"http://a\\u001ab/\",null],[\"http://a\\u001bb/\",null],[\"http://a\\u001cb/\",null],[\"http://a\\u001db/\",null],[\"http://a\\u001eb/\",null],[\"http://a\\u001fb/\",null],[\"http://a b/\",null],[\"http://a%b/\",null],[\"http://a<b\",null],[\"http://a>b\",null],[\"http://a[b/\",null],[\"http://a]b/\",null],[\"http://a^b\",null],[\"http://a|b/\",null],[\"http://ab/\",null],[\"http://ho\\tst/\",null],[\"http://ho\\nst/\",null],[\"http://ho\\rst/\",null],[\"http://ho%00st/\",null],[\"http://ho%01st/\",null],[\"http://ho%02st/\",null],[\"http://ho%03st/\",null],[\"http://ho%04st/\",null],[\"http://ho%05st/\",null],[\"http://ho%06st/\",null],[\"http://ho%07st/\",null],[\"http://ho%08st/\",null],[\"http://ho%09st/\",null],[\"http://ho%0Ast/\",null],[\"http://ho%0Bst/\",null],[\"http://ho%0Cst/\",null],[\"http://ho%0Dst/\",null],[\"http://ho%0Est/\",null],[\"http://ho%0Fst/\",null],[\"http://ho%10st/\",null],[\"http://ho%11st/\",null],[\"http://ho%12st/\",null],[\"http://ho%13st/\",null],[\"http://ho%14st/\",null],[\"http://ho%15st/\",null],[\"http://ho%16st/\",null],[\"http://ho%17st/\",null],[\"http://ho%18st/\",null],[\"http://ho%19st/\",null],[\"http://ho%1Ast/\",null],[\"http://ho%1Bst/\",null],[\"http://ho%1Cst/\",null],[\"http://ho%1Dst/\",null],[\"http://ho%1Est/\",null],[\"http://ho%1Fst/\",null],[\"http://ho%20st/\",null],[\"http://ho%23st/\",null],[\"http://ho%25st/\",null],[\"http://ho%2Fst/\",null],[\"http://ho%3Ast/\",null],[\"http://ho%3Cst/\",null],[\"http://ho%3Est/\",null],[\"http://ho%3Fst/\",null],[\"http://ho%40st/\",null],[\"http://ho%5Bst/\",null],[\"http://ho%5Cst/\",null],[\"http://ho%5Dst/\",null],[\"http://ho%7Cst/\",null],[\"http://ho%7Fst/\",null],[\"http://!\\\"$&'()*+,-.;=_`{}~/\",null],[\"sc://\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\b\\u000b\\f\\u000e\\u000f\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001a\\u001b\\u001c\\u001d\\u001e\\u001f!\\\"$%&'()*+,-.;=_`{}~/\",null],[\"ftp://example.com%80/\",null],[\"ftp://example.com%A0/\",null],[\"https://example.com%80/\",null],[\"https://example.com%A0/\",null],[\"ftp://%e2%98%83\",null],[\"https://%e2%98%83\",null],[\"http://127.0.0.1:10100/relative_import.html\",null],[\"http://facebook.com/?foo=%7B%22abc%22\",null],[\"https://localhost:3000/jqueryui@1.2.3\",null],[\"h\\tt\\nt\\rp://h\\to\\ns\\rt:9\\t0\\n0\\r0/p\\ta\\nt\\rh?q\\tu\\ne\\rry#f\\tr\\na\\rg\",null],[\"?a=b&c=d\",\"http://example.org/foo/bar\"],[\"??a=b&c=d\",\"http://example.org/foo/bar\"],[\"http:\",\"http://example.org/foo/bar\"],[\"http:\",\"https://example.org/foo/bar\"],[\"sc:\",\"https://example.org/foo/bar\"],[\"http://foo.bar/baz?qux#foo\\bbar\",null],[\"http://foo.bar/baz?qux#foo\\\"bar\",null],[\"http://foo.bar/baz?qux#foo<bar\",null],[\"http://foo.bar/baz?qux#foo>bar\",null],[\"http://foo.bar/baz?qux#foo`bar\",null],[\"http://1.2.3.4/\",\"http://other.com/\"],[\"http://1.2.3.4./\",\"http://other.com/\"],[\"http://192.168.257\",\"http://other.com/\"],[\"http://192.168.257.\",\"http://other.com/\"],[\"http://192.168.257.com\",\"http://other.com/\"],[\"http://256\",\"http://other.com/\"],[\"http://256.com\",\"http://other.com/\"],[\"http://999999999\",\"http://other.com/\"],[\"http://999999999.\",\"http://other.com/\"],[\"http://999999999.com\",\"http://other.com/\"],[\"http://10000000000\",\"http://other.com/\"],[\"http://10000000000.com\",\"http://other.com/\"],[\"http://4294967295\",\"http://other.com/\"],[\"http://4294967296\",\"http://other.com/\"],[\"http://18446744073709551616\",\"http://other.com/\"],[\"http://18446744075840258049\",\"http://other.com/\"],[\"http://0xffffffff\",\"http://other.com/\"],[\"http://0xffffffff1\",\"http://other.com/\"],[\"http://256.256.256.256\",\"http://other.com/\"],[\"https://0x.0x.0\",null],[\"https://0x.0x.0x.0x\",null],[\"https://00.00.00.00\",null],[\"https://0000000000000000000000000000000000000000177.0.0.1\",null],[\"https://0x100000000/test\",null],[\"https://256.0.0.1/test\",null],[\"file:///C%3A/\",null],[\"file:///C%7C/\",null],[\"file://%43%3A\",null],[\"file://%43%7C\",null],[\"file://%43|\",null],[\"file://C%7C\",null],[\"file://%43%7C/\",null],[\"https://%43%7C/\",null],[\"asdf://%43|/\",null],[\"asdf://%43%7C/\",null],[\"pix/submit.gif\",\"file:///C:/Users/Domenic/Dropbox/GitHub/tmpvar/jsdom/test/level2/html/files/anchor.html\"],[\"..\",\"file:///C:/\"],[\"..\",\"file:///\"],[\"/\",\"file:///C:/a/b\"],[\"/\",\"file://h/C:/a/b\"],[\"/\",\"file://h/a/b\"],[\"//d:\",\"file:///C:/a/b\"],[\"//d:/..\",\"file:///C:/a/b\"],[\"..\",\"file:///ab:/\"],[\"..\",\"file:///1:/\"],[\"\",\"file:///test?test#test\"],[\"file:\",\"file:///test?test#test\"],[\"?x\",\"file:///test?test#test\"],[\"file:?x\",\"file:///test?test#test\"],[\"#x\",\"file:///test?test#test\"],[\"file:#x\",\"file:///test?test#test\"],[\"file:\\\\\\\\//\",null],[\"file:\\\\\\\\\\\\\\\\\",null],[\"file:\\\\\\\\\\\\\\\\?fox\",null],[\"file:\\\\\\\\\\\\\\\\#guppy\",null],[\"file://spider///\",null],[\"file:\\\\\\\\localhost//\",null],[\"file:///localhost//cat\",null],[\"file://\\\\/localhost//cat\",null],[\"file://localhost//a//../..//\",null],[\"/////mouse\",\"file:///elephant\"],[\"\\\\//pig\",\"file://lion/\"],[\"\\\\/localhost//pig\",\"file://lion/\"],[\"//localhost//pig\",\"file://lion/\"],[\"/..//localhost//pig\",\"file://lion/\"],[\"file://\",\"file://ape/\"],[\"/rooibos\",\"file://tea/\"],[\"/?chai\",\"file://tea/\"],[\"C|\",\"file://host/dir/file\"],[\"C|\",\"file://host/D:/dir1/dir2/file\"],[\"C|#\",\"file://host/dir/file\"],[\"C|?\",\"file://host/dir/file\"],[\"C|/\",\"file://host/dir/file\"],[\"C|\\n/\",\"file://host/dir/file\"],[\"C|\\\\\",\"file://host/dir/file\"],[\"C\",\"file://host/dir/file\"],[\"C|a\",\"file://host/dir/file\"],[\"/c:/foo/bar\",\"file:///c:/baz/qux\"],[\"/c|/foo/bar\",\"file:///c:/baz/qux\"],[\"file:\\\\c:\\\\foo\\\\bar\",\"file:///c:/baz/qux\"],[\"/c:/foo/bar\",\"file://host/path\"],[\"..\",\"abc://x/y/z/C:/\"],[\"..\",\"file://x/C:/\"],[\"file://example.net/C:/\",null],[\"file://1.2.3.4/C:/\",null],[\"file://[1::8]/C:/\",null],[\"C|/\",\"file://host/\"],[\"/C:/\",\"file://host/\"],[\"file:C:/\",\"file://host/\"],[\"file:/C:/\",\"file://host/\"],[\"//C:/\",\"file://host/\"],[\"file://C:/\",\"file://host/\"],[\"///C:/\",\"file://host/\"],[\"file:///C:/\",\"file://host/\"],[\"file:/C|/\",null],[\"file://C|/\",null],[\"file:\",null],[\"file:?q=v\",null],[\"file:#frag\",null],[\"file:///Y:\",null],[\"file:///Y:/\",null],[\"file:///./Y\",null],[\"file:///./Y:\",null],[\"\\\\\\\\\\\\.\\\\Y:\",null],[\"file:///y:\",null],[\"file:///y:/\",null],[\"file:///./y\",null],[\"file:///./y:\",null],[\"\\\\\\\\\\\\.\\\\y:\",null],[\"file://localhost//a//../..//foo\",null],[\"file://localhost////foo\",null],[\"file:////foo\",null],[\"file:///one/two\",\"file:///\"],[\"file:////one/two\",\"file:///\"],[\"//one/two\",\"file:///\"],[\"///one/two\",\"file:///\"],[\"////one/two\",\"file:///\"],[\"file:///.//\",\"file:////\"],[\"file:.//p\",null],[\"file:/.//p\",null],[\"https://[0:1:2:3:4:5:192.0.02.1]/\",null],[\"https://[192.0.2.1::]/\",null],[\"http://[1:0::]\",\"http://example.net/\"],[\"http://[0:1:2:3:4:5:6:7:8]\",\"http://example.net/\"],[\"https://[0::0::0]\",null],[\"https://[0:.0]\",null],[\"https://[0:0:]\",null],[\"https://[0:1:2:3:4:5:6:7.0.0.0.1]\",null],[\"https://[0:1.00.0.0.0]\",null],[\"https://[0:1.290.0.0.0]\",null],[\"https://[0:1.23.23]\",null],[\"http://?\",null],[\"http://#\",null],[\"http://f:4294967377/c\",\"http://example.org/\"],[\"http://f:18446744073709551697/c\",\"http://example.org/\"],[\"http://f:340282366920938463463374607431768211537/c\",\"http://example.org/\"],[\"sc://\\u00f1\",null],[\"sc://\\u00f1?x\",null],[\"sc://\\u00f1#x\",null],[\"#x\",\"sc://\\u00f1\"],[\"?x\",\"sc://\\u00f1\"],[\"sc://?\",null],[\"sc://#\",null],[\"///\",\"sc://x/\"],[\"////\",\"sc://x/\"],[\"////x/\",\"sc://x/\"],[\"tftp://foobar.com/someconfig;mode=netascii\",null],[\"telnet://user:pass@foobar.com:23/\",null],[\"ut2004://10.10.10.10:7777/Index.ut2\",null],[\"redis://foo:bar@somehost:6379/0?baz=bam&qux=baz\",null],[\"rsync://foo@host:911/sup\",null],[\"git://github.com/foo/bar.git\",null],[\"irc://myserver.com:6999/channel?passwd\",null],[\"dns://fw.example.org:9999/foo.bar.org?type=TXT\",null],[\"ldap://localhost:389/ou=People,o=JNDITutorial\",null],[\"git+https://github.com/foo/bar\",null],[\"urn:ietf:rfc:2648\",null],[\"tag:joe@example.org,2001:foo/bar\",null],[\"non-spec:/.//\",null],[\"non-spec:/..//\",null],[\"non-spec:/a/..//\",null],[\"non-spec:/.//path\",null],[\"non-spec:/..//path\",null],[\"non-spec:/a/..//path\",null],[\"/.//path\",\"non-spec:/p\"],[\"/..//path\",\"non-spec:/p\"],[\"..//path\",\"non-spec:/p\"],[\"a/..//path\",\"non-spec:/p\"],[\"\",\"non-spec:/..//p\"],[\"path\",\"non-spec:/..//p\"],[\"../path\",\"non-spec:/.//p\"],[\"non-special://%E2%80%A0/\",null],[\"non-special://H%4fSt/path\",null],[\"non-special://[1:2:0:0:5:0:0:0]/\",null],[\"non-special://[1:2:0:0:0:0:0:3]/\",null],[\"non-special://[1:2::3]:80/\",null],[\"non-special://[:80/\",null],[\"blob:https://example.com:443/\",null],[\"blob:http://example.org:88/\",null],[\"blob:d3958f5c-0777-0845-9dcf-2cb28783acaf\",null],[\"blob:\",null],[\"blob:blob:\",null],[\"blob:blob:https://example.org/\",null],[\"blob:about:blank\",null],[\"blob:file://host/path\",null],[\"blob:ftp://host/path\",null],[\"blob:ws://example.org/\",null],[\"blob:wss://example.org/\",null],[\"blob:http%3a//example.org/\",null],[\"http://0x7f.0.0.0x7g\",null],[\"http://0X7F.0.0.0X7G\",null],[\"http://[::127.0.0.0.1]\",null],[\"http://[0:1:0:1:0:1:0:1]\",null],[\"http://[1:0:1:0:1:0:1:0]\",null],[\"http://example.org/test?\\\"\",null],[\"http://example.org/test?#\",null],[\"http://example.org/test?<\",null],[\"http://example.org/test?>\",null],[\"http://example.org/test?\\u2323\",null],[\"http://example.org/test?%23%23\",null],[\"http://example.org/test?%GH\",null],[\"http://example.org/test?a#%EF\",null],[\"http://example.org/test?a#%GH\",null],[\"a\",null],[\"a/\",null],[\"a//\",null],[\"test-a-colon.html\",\"a:\"],[\"test-a-colon-b.html\",\"a:b\"],[\"test-a-colon-slash.html\",\"a:/\"],[\"test-a-colon-slash-slash.html\",\"a://\"],[\"test-a-colon-slash-b.html\",\"a:/b\"],[\"test-a-colon-slash-slash-b.html\",\"a://b\"],[\"http://example.org/test?a#b\\u0000c\",null],[\"non-spec://example.org/test?a#b\\u0000c\",null],[\"non-spec:/test?a#b\\u0000c\",null],[\"10.0.0.7:8080/foo.html\",\"file:///some/dir/bar.html\"],[\"a!@$*=/foo.html\",\"file:///some/dir/bar.html\"],[\"a1234567890-+.:foo/bar\",\"http://example.com/dir/file\"],[\"file://a\\u00adb/p\",null],[\"file://a%C2%ADb/p\",null],[\"file://loC\\ud835\\udc00\\ud835\\udc0b\\ud835\\udc07\\ud835\\udc28\\ud835\\udc2c\\ud835\\udc2d/usr/bin\",null],[\"file://\\u00ad/p\",null],[\"file://%C2%AD/p\",null],[\"file://xn--/p\",null],[\"#link\",\"https://example.org/##link\"],[\"non-special:cannot-be-a-base-url-\\u0000\\u0001\\u001f\\u001e~\\u0080\",null],[\"non-special:cannot-be-a-base-url-!\\\"$%&'()*+,-.;<=>@[\\\\]^_`{|}~@/\",null],[\"https://www.example.com/path{path.html?query'=query#fragment<fragment\",null],[\"https://user:pass[@foo/bar\",\"http://example.org\"],[\"foo:// !\\\"$%&'()*+,-.;<=>@[\\\\]^_`{|}~@host/\",null],[\"wss:// !\\\"$%&'()*+,-.;<=>@[]^_`{|}~@host/\",null],[\"foo://joe: !\\\"$%&'()*+,-.:;<=>@[\\\\]^_`{|}~@host/\",null],[\"wss://joe: !\\\"$%&'()*+,-.:;<=>@[]^_`{|}~@host/\",null],[\"foo://!\\\"$%&'()*+,-.;=_`{}~/\",null],[\"wss://!\\\"$&'()*+,-.;=_`{}~/\",null],[\"foo://host/ !\\\"$%&'()*+,-./:;<=>@[\\\\]^_`{|}~\",null],[\"wss://host/ !\\\"$%&'()*+,-./:;<=>@[\\\\]^_`{|}~\",null],[\"foo://host/dir/? !\\\"$%&'()*+,-./:;<=>?@[\\\\]^_`{|}~\",null],[\"wss://host/dir/? !\\\"$%&'()*+,-./:;<=>?@[\\\\]^_`{|}~\",null],[\"foo://host/dir/# !\\\"#$%&'()*+,-./:;<=>?@[\\\\]^_`{|}~\",null],[\"wss://host/dir/# !\\\"#$%&'()*+,-./:;<=>?@[\\\\]^_`{|}~\",null],[\"abc:rootless\",\"abc://host/path\"],[\"abc:rootless\",\"abc:/path\"],[\"abc:rootless\",\"abc:path\"],[\"abc:/rooted\",\"abc://host/path\"],[\"#\",null],[\"?\",null],[\"http://1.2.3.4.5\",\"http://other.com/\"],[\"http://1.2.3.4.5.\",\"http://other.com/\"],[\"http://0..0x300/\",null],[\"http://0..0x300./\",null],[\"http://256.256.256.256.256\",\"http://other.com/\"],[\"http://256.256.256.256.256.\",\"http://other.com/\"],[\"http://1.2.3.08\",null],[\"http://1.2.3.08.\",null],[\"http://1.2.3.09\",null],[\"http://09.2.3.4\",null],[\"http://09.2.3.4.\",null],[\"http://01.2.3.4.5\",null],[\"http://01.2.3.4.5.\",null],[\"http://0x100.2.3.4\",null],[\"http://0x100.2.3.4.\",null],[\"http://0x1.2.3.4.5\",null],[\"http://0x1.2.3.4.5.\",null],[\"http://foo.1.2.3.4\",null],[\"http://foo.1.2.3.4.\",null],[\"http://foo.2.3.4\",null],[\"http://foo.2.3.4.\",null],[\"http://foo.09\",null],[\"http://foo.09.\",null],[\"http://foo.0x4\",null],[\"http://foo.0x4.\",null],[\"http://foo.09..\",null],[\"http://0999999999999999999/\",null],[\"http://foo.0x\",null],[\"http://foo.0XFfFfFfFfFfFfFfFfFfAcE123\",null],[\"http://\\ud83d\\udca9.123/\",null],[\"https://\\u0000y\",null],[\"https://x/\\u0000y\",null],[\"https://x/?\\u0000y\",null],[\"https://x/?#\\u0000y\",null],[\"https://\\uffffy\",null],[\"https://x/\\uffffy\",null],[\"https://x/?\\uffffy\",null],[\"https://x/?#\\uffffy\",null],[\"non-special:\\u0000y\",null],[\"non-special:x/\\u0000y\",null],[\"non-special:x/?\\u0000y\",null],[\"non-special:x/?#\\u0000y\",null],[\"non-special:\\uffffy\",null],[\"non-special:x/\\uffffy\",null],[\"non-special:x/?\\uffffy\",null],[\"non-special:x/?#\\uffffy\",null],[\"\",null],[\"https://example.com/\\\"quoted\\\"\",null],[\"https://a%C2%ADb/\",null],[\"https://\\u00ad/\",null],[\"https://%C2%AD/\",null],[\"https://xn--/\",null],[\"constructor:example\",null],[\"constructor:/example\",null],[\"constructor://HOST:80\",null],[\"data://example.com:8080/pathname?search#hash\",null],[\"data:///test\",null],[\"data://test/a/../b\",null],[\"data://:443\",null],[\"data://test:test\",null],[\"data://[:1]\",null],[\"javascript://example.com:8080/pathname?search#hash\",null],[\"javascript:///test\",null],[\"javascript://test/a/../b\",null],[\"javascript://:443\",null],[\"javascript://test:test\",null],[\"javascript://[:1]\",null],[\"mailto://example.com:8080/pathname?search#hash\",null],[\"mailto:///test\",null],[\"mailto://test/a/../b\",null],[\"mailto://:443\",null],[\"mailto://test:test\",null],[\"mailto://[:1]\",null],[\"intent://example.com:8080/pathname?search#hash\",null],[\"intent:///test\",null],[\"intent://test/a/../b\",null],[\"intent://:443\",null],[\"intent://test:test\",null],[\"intent://[:1]\",null],[\"urn://example.com:8080/pathname?search#hash\",null],[\"urn:///test\",null],[\"urn://test/a/../b\",null],[\"urn://:443\",null],[\"urn://test:test\",null],[\"urn://[:1]\",null],[\"turn://example.com:8080/pathname?search#hash\",null],[\"turn:///test\",null],[\"turn://test/a/../b\",null],[\"turn://:443\",null],[\"turn://test:test\",null],[\"turn://[:1]\",null],[\"stun://example.com:8080/pathname?search#hash\",null],[\"stun:///test\",null],[\"stun://test/a/../b\",null],[\"stun://:443\",null],[\"stun://test:test\",null],[\"stun://[:1]\",null],[\"w://x:0\",null],[\"west://x:0\",null],[\"android://x:0/a\",null],[\"drivefs://x:0/a\",null],[\"chromeos-steam://x:0/a\",null],[\"steam://x:0/a\",null],[\"materialized-view://x:0/a\",null],[\"android-app://x:0\",null],[\"chrome-distiller://x:0\",null],[\"chrome-extension://x:0\",null],[\"chrome-native://x:0\",null],[\"chrome-resource://x:0\",null],[\"chrome-search://x:0\",null],[\"fuchsia-dir://x:0\",null],[\"isolated-app://x:0\",null],[\"///test\",\"http://example.org/\"],[\"///\\\\//\\\\//test\",\"http://example.org/\"],[\"///example.org/path\",\"http://example.org/\"],[\"///example.org/../path\",\"http://example.org/\"],[\"///example.org/../../\",\"http://example.org/\"],[\"///example.org/../path/../../\",\"http://example.org/\"],[\"///example.org/../path/../../path\",\"http://example.org/\"],[\"/\\\\/\\\\//example.org/../path\",\"http://example.org/\"],[\"///abcdef/../\",\"file:///\"],[\"/\\\\//\\\\/a/../\",\"file:///\"],[\"//a/../\",\"file:///\"],[\"non-special:\\\\\\\\opaque\",null],[\"non-special:\\\\\\\\opaque/path\",null],[\"non-special:\\\\\\\\opaque\\\\path\",null],[\"non-special:\\\\/opaque\",null],[\"non-special:/\\\\path\",null],[\"non-special://host\\\\a\",null],[\"non-special://host/a\\\\b\",null],[\"\\\\a\",\"foo://foo/a\"],[\"\\\\/a\",\"foo://foo/a\"],[\"\\\\\\\\a\",\"foo://foo/a\"],[\"data:text/plain,test#<foo> <bar>\",null],[\"about:blank#<foo> <bar>\",null],[\"data:text/plain,test#\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\",null],[\"about:blank#\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\",null]];
const setters = [[\"protocol\",\"a://example.net\",\"\"],[\"protocol\",\"a://example.net\",\"b\"],[\"protocol\",\"javascript:alert(1)\",\"defuse\"],[\"protocol\",\"a://example.net\",\"B\"],[\"protocol\",\"a://example.net\",\"\\u00e9\"],[\"protocol\",\"a://example.net\",\"0b\"],[\"protocol\",\"a://example.net\",\"+b\"],[\"protocol\",\"a://example.net\",\"bC0+-.\"],[\"protocol\",\"a://example.net\",\"b,c\"],[\"protocol\",\"a://example.net\",\"b\\u00e9\"],[\"protocol\",\"http://test@example.net\",\"file\"],[\"protocol\",\"https://example.net:1234\",\"file\"],[\"protocol\",\"wss://x:x@example.net:1234\",\"file\"],[\"protocol\",\"file://localhost/\",\"http\"],[\"protocol\",\"file:///test\",\"https\"],[\"protocol\",\"file:\",\"wss\"],[\"protocol\",\"http://example.net\",\"b\"],[\"protocol\",\"file://hi/path\",\"s\"],[\"protocol\",\"https://example.net\",\"s\"],[\"protocol\",\"ftp://example.net\",\"test\"],[\"protocol\",\"mailto:me@example.net\",\"http\"],[\"protocol\",\"ssh://me@example.net\",\"http\"],[\"protocol\",\"ssh://me@example.net\",\"https\"],[\"protocol\",\"ssh://me@example.net\",\"file\"],[\"protocol\",\"ssh://example.net\",\"file\"],[\"protocol\",\"nonsense:///test\",\"https\"],[\"protocol\",\"http://example.net\",\"https:foo : bar\"],[\"protocol\",\"data:text/html,<p>Test\",\"view-source+data:foo : bar\"],[\"protocol\",\"http://foo.com:443/\",\"https\"],[\"protocol\",\"http://test/\",\"h\\r\\ntt\\tps\"],[\"protocol\",\"http://test/\",\"https\\r\"],[\"protocol\",\"http://test/\",\"https\\u0000\"],[\"protocol\",\"http://test/\",\"https\\f\"],[\"protocol\",\"http://test/\",\"https\\u000e\"],[\"protocol\",\"http://test/\",\"https \"],[\"username\",\"file:///home/you/index.html\",\"me\"],[\"username\",\"unix:/run/foo.socket\",\"me\"],[\"username\",\"mailto:you@example.net\",\"me\"],[\"username\",\"javascript:alert(1)\",\"wario\"],[\"username\",\"http://example.net\",\"me\"],[\"username\",\"http://:secret@example.net\",\"me\"],[\"username\",\"http://me@example.net\",\"\"],[\"username\",\"http://me:secret@example.net\",\"\"],[\"username\",\"http://example.net\",\"\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\"],[\"username\",\"http://example.net\",\"%c3%89t\\u00e9\"],[\"username\",\"sc:///\",\"x\"],[\"username\",\"javascript://x/\",\"wario\"],[\"username\",\"file://test/\",\"test\"],[\"password\",\"file:///home/me/index.html\",\"secret\"],[\"password\",\"unix:/run/foo.socket\",\"secret\"],[\"password\",\"mailto:me@example.net\",\"secret\"],[\"password\",\"http://example.net\",\"secret\"],[\"password\",\"http://me@example.net\",\"secret\"],[\"password\",\"http://:secret@example.net\",\"\"],[\"password\",\"http://me:secret@example.net\",\"\"],[\"password\",\"http://example.net\",\"\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\"],[\"password\",\"http://example.net\",\"%c3%89t\\u00e9\"],[\"password\",\"sc:///\",\"x\"],[\"password\",\"javascript://x/\",\"bowser\"],[\"password\",\"file://test/\",\"test\"],[\"host\",\"sc://x/\",\"\\u0000\"],[\"host\",\"sc://x/\",\"\\t\"],[\"host\",\"sc://x/\",\"\\n\"],[\"host\",\"sc://x/\",\"\\r\"],[\"host\",\"sc://x/\",\" \"],[\"host\",\"sc://x/\",\"#\"],[\"host\",\"sc://x/\",\"/\"],[\"host\",\"sc://x/\",\"?\"],[\"host\",\"sc://x/\",\"@\"],[\"host\",\"sc://x/\",\"\\u00df\"],[\"host\",\"https://x/\",\"\\u00df\"],[\"host\",\"mailto:me@example.net\",\"example.com\"],[\"host\",\"data:text/plain,Stuff\",\"example.net\"],[\"host\",\"http://example.net\",\"example.com:8080\"],[\"host\",\"http://example.net:8080\",\"example.com\"],[\"host\",\"http://example.net:8080\",\"example.com:\"],[\"host\",\"http://example.net\",\"\"],[\"host\",\"view-source+http://example.net/foo\",\"\"],[\"host\",\"a:/foo\",\"example.net\"],[\"host\",\"http://example.net\",\"0x7F000001:8080\"],[\"host\",\"http://example.net\",\"[::0:01]:2\"],[\"host\",\"http://example.net\",\"[2001:db8::2]:4002\"],[\"host\",\"http://example.net\",\"example.com:80\"],[\"host\",\"https://example.net\",\"example.com:443\"],[\"host\",\"https://example.net\",\"example.com:80\"],[\"host\",\"http://example.net:8080\",\"example.com:80\"],[\"host\",\"http://example.net/path\",\"example.com/stuff\"],[\"host\",\"http://example.net/path\",\"example.com:8080/stuff\"],[\"host\",\"http://example.net/path\",\"example.com?stuff\"],[\"host\",\"http://example.net/path\",\"example.com?stuff:8080\"],[\"host\",\"http://example.net/path\",\"example.com:8080?stuff\"],[\"host\",\"http://example.net/path\",\"example.com#stuff\"],[\"host\",\"http://example.net/path\",\"example.com:8080#stuff\"],[\"host\",\"http://example.net/path\",\"example.com\\\\stuff\"],[\"host\",\"http://example.net/path\",\"example.com:8080\\\\stuff\"],[\"host\",\"view-source+http://example.net/path\",\"example.com\\\\stuff\"],[\"host\",\"view-source+http://example.net/path\",\"example.com:8080stuff2\"],[\"host\",\"http://example.net/path\",\"example.com:8080stuff2\"],[\"host\",\"http://example.net/path\",\"example.com:8080+2\"],[\"host\",\"http://example.net:8080\",\"example.com:invalid\"],[\"host\",\"http://example.net:8080/test\",\"[::1]:invalid\"],[\"host\",\"http://example.net:8080/test\",\"[::1]\"],[\"host\",\"http://example.net/path\",\"example.com:65535\"],[\"host\",\"http://example.net/path\",\"example.com:65536\"],[\"host\",\"http://example.net/\",\"[google.com]\"],[\"host\",\"http://example.net/\",\"[::1.2.3.4x]\"],[\"host\",\"http://example.net/\",\"[::1.2.3.]\"],[\"host\",\"http://example.net/\",\"[::1.2.]\"],[\"host\",\"http://example.net/\",\"[::1.]\"],[\"host\",\"file://y/\",\"x:123\"],[\"host\",\"file://y/\",\"loc%41lhost\"],[\"host\",\"file://hi/x\",\"\"],[\"host\",\"sc://test@test/\",\"\"],[\"host\",\"sc://test:12/\",\"\"],[\"host\",\"http://example.com/\",\"///bad.com\"],[\"host\",\"sc://example.com/\",\"///bad.com\"],[\"host\",\"https://example.com/\",\"a%C2%ADb\"],[\"host\",\"https://example.com/\",\"\\u00ad\"],[\"host\",\"https://example.com/\",\"%C2%AD\"],[\"host\",\"https://example.com/\",\"xn--\"],[\"host\",\"https://test.invalid/\",\"*\"],[\"host\",\"https://test.invalid/\",\"x@x\"],[\"host\",\"https://test.invalid/\",\"foo\\t\\r\\nbar\"],[\"host\",\"https://test.invalid/\",\"><\"],[\"host\",\"https://test.invalid/\",\"test/@aaa\"],[\"host\",\"https://test.invalid/\",\"test/:aaa\"],[\"host\",\"foo://path/to\",\":80\"],[\"hostname\",\"sc://x/\",\"\\u0000\"],[\"hostname\",\"sc://x/\",\"\\t\"],[\"hostname\",\"sc://x/\",\"\\n\"],[\"hostname\",\"sc://x/\",\"\\r\"],[\"hostname\",\"sc://x/\",\" \"],[\"hostname\",\"sc://x/\",\"#\"],[\"hostname\",\"sc://x/\",\"/\"],[\"hostname\",\"sc://x/\",\"?\"],[\"hostname\",\"sc://x/\",\"@\"],[\"hostname\",\"mailto:me@example.net\",\"example.com\"],[\"hostname\",\"data:text/plain,Stuff\",\"example.net\"],[\"hostname\",\"http://example.net:8080\",\"example.com\"],[\"hostname\",\"http://example.net\",\"\"],[\"hostname\",\"view-source+http://example.net/foo\",\"\"],[\"hostname\",\"a:/foo\",\"example.net\"],[\"hostname\",\"http://example.net:8080\",\"0x7F000001\"],[\"hostname\",\"http://example.net\",\"[::0:01]\"],[\"hostname\",\"http://example.net/path\",\"example.com:8080\"],[\"hostname\",\"http://example.net:8080/path\",\"example.com:\"],[\"hostname\",\"http://example.net/path\",\"example.com/stuff\"],[\"hostname\",\"http://example.net/path\",\"example.com?stuff\"],[\"hostname\",\"http://example.net/path\",\"example.com#stuff\"],[\"hostname\",\"http://example.net/path\",\"example.com\\\\stuff\"],[\"hostname\",\"view-source+http://example.net/path\",\"example.com\\\\stuff\"],[\"hostname\",\"http://example.net/\",\"[google.com]\"],[\"hostname\",\"http://example.net/\",\"[::1.2.3.4x]\"],[\"hostname\",\"http://example.net/\",\"[::1.2.3.]\"],[\"hostname\",\"http://example.net/\",\"[::1.2.]\"],[\"hostname\",\"http://example.net/\",\"[::1.]\"],[\"hostname\",\"file://y/\",\"x:123\"],[\"hostname\",\"file://y/\",\"loc%41lhost\"],[\"hostname\",\"file://hi/x\",\"\"],[\"hostname\",\"sc://test@test/\",\"\"],[\"hostname\",\"sc://test:12/\",\"\"],[\"hostname\",\"non-spec:/.//p\",\"h\"],[\"hostname\",\"non-spec:/.//p\",\"\"],[\"hostname\",\"http://example.com/\",\"///bad.com\"],[\"hostname\",\"sc://example.com/\",\"///bad.com\"],[\"hostname\",\"https://example.com/\",\"a%C2%ADb\"],[\"hostname\",\"https://example.com/\",\"\\u00ad\"],[\"hostname\",\"https://example.com/\",\"%C2%AD\"],[\"hostname\",\"https://example.com/\",\"xn--\"],[\"hostname\",\"https://test.invalid/\",\"*\"],[\"hostname\",\"https://test.invalid/\",\"x@x\"],[\"hostname\",\"https://test.invalid/\",\"foo\\t\\r\\nbar\"],[\"hostname\",\"https://test.invalid/\",\"><\"],[\"hostname\",\"https://test.invalid/\",\"test/@aaa\"],[\"hostname\",\"https://test.invalid/\",\"test/:aaa\"],[\"port\",\"http://example.net\",\"8080\"],[\"port\",\"http://example.net:8080\",\"\"],[\"port\",\"http://example.net:8080\",\"80\"],[\"port\",\"https://example.net:4433\",\"443\"],[\"port\",\"https://example.net\",\"80\"],[\"port\",\"http://example.net/path\",\"8080/stuff\"],[\"port\",\"http://example.net/path\",\"8080?stuff\"],[\"port\",\"http://example.net/path\",\"8080#stuff\"],[\"port\",\"http://example.net/path\",\"8080\\\\stuff\"],[\"port\",\"view-source+http://example.net/path\",\"8080stuff2\"],[\"port\",\"http://example.net/path\",\"8080stuff2\"],[\"port\",\"http://example.net/path\",\"8080+2\"],[\"port\",\"http://example.net/path\",\"65535\"],[\"port\",\"http://example.net:8080/path\",\"65536\"],[\"port\",\"http://example.net:8080/path\",\"randomstring\"],[\"port\",\"non-special://example.net:8080/path\",\"65536\"],[\"port\",\"file://test/\",\"12\"],[\"port\",\"file://localhost/\",\"12\"],[\"port\",\"non-base:value\",\"12\"],[\"port\",\"sc:///\",\"12\"],[\"port\",\"sc://x/\",\"12\"],[\"port\",\"javascript://x/\",\"12\"],[\"port\",\"https://domain.com:443\",\"\\t8080\"],[\"port\",\"wpt++://domain.com:443\",\"\\t8080\"],[\"port\",\"https://www.google.com:4343\",\"4wpt\"],[\"port\",\"https://domain.com:3000\",\"\\n\\t80\\n\\t80\\n\\t\"],[\"port\",\"https://domain.com:3000\",\"\\n\\n\\t\\t\"],[\"pathname\",\"mailto:me@example.net\",\"/foo\"],[\"pathname\",\"data:original\",\"new value\"],[\"pathname\",\"sc:original\",\"new value\"],[\"pathname\",\"file:///some/path\",\"\"],[\"pathname\",\"foo://somehost/some/path\",\"\"],[\"pathname\",\"foo:///some/path\",\"\"],[\"pathname\",\"foo:/some/path\",\"\"],[\"pathname\",\"foo:/some/path\",\"test\"],[\"pathname\",\"unix:/run/foo.socket?timeout=10\",\"/var/log/../run/bar.socket\"],[\"pathname\",\"https://example.net#nav\",\"home\"],[\"pathname\",\"https://example.net#nav\",\"../home\"],[\"pathname\",\"http://example.net/home?lang=fr#nav\",\"\\\\a\\\\%2E\\\\b\\\\%2e.\\\\c\"],[\"pathname\",\"view-source+http://example.net/home?lang=fr#nav\",\"\\\\a\\\\%2E\\\\b\\\\%2e.\\\\c\"],[\"pathname\",\"a:/\",\"\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\"],[\"pathname\",\"http://example.net\",\"%2e%2E%c3%89t\\u00e9\"],[\"pathname\",\"http://example.net\",\"?\"],[\"pathname\",\"http://example.net\",\"#\"],[\"pathname\",\"sc://example.net\",\"?\"],[\"pathname\",\"sc://example.net\",\"#\"],[\"pathname\",\"http://example.net\",\"/?\\u00e9\"],[\"pathname\",\"http://example.net\",\"/#\\u00e9\"],[\"pathname\",\"file://monkey/\",\"\\\\\\\\\"],[\"pathname\",\"file:///unicorn\",\"//\\\\/\"],[\"pathname\",\"file:///unicorn\",\"//monkey/..//\"],[\"pathname\",\"non-spec:/\",\"/.//p\"],[\"pathname\",\"non-spec:/\",\"/..//p\"],[\"pathname\",\"non-spec:/\",\"//p\"],[\"pathname\",\"non-spec:/.//\",\"p\"],[\"pathname\",\"data:/nospace\",\"space \"],[\"pathname\",\"sc:/nospace\",\"space \"],[\"pathname\",\"http://example.net\",\" \"],[\"pathname\",\"http://example.net\",\"\\u0000\"],[\"pathname\",\"foo://path/to\",\"/..\"],[\"search\",\"https://example.net#nav\",\"lang=fr\"],[\"search\",\"https://example.net?lang=en-US#nav\",\"lang=fr\"],[\"search\",\"https://example.net?lang=en-US#nav\",\"?lang=fr\"],[\"search\",\"https://example.net?lang=en-US#nav\",\"??lang=fr\"],[\"search\",\"https://example.net?lang=en-US#nav\",\"?\"],[\"search\",\"https://example.net?lang=en-US#nav\",\"\"],[\"search\",\"https://example.net?lang=en-US\",\"\"],[\"search\",\"https://example.net\",\"\"],[\"search\",\"a:/\",\"\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\"],[\"search\",\"http://example.net\",\"%c3%89t\\u00e9\"],[\"search\",\"data:space ?query\",\"\"],[\"search\",\"sc:space ?query\",\"\"],[\"search\",\"data:space  ?query#fragment\",\"\"],[\"search\",\"sc:space  ?query#fragment\",\"\"],[\"search\",\"http://example.net\",\" \"],[\"search\",\"http://example.net\",\"\\u0000\"],[\"hash\",\"https://example.net\",\"main\"],[\"hash\",\"https://example.net#nav\",\"main\"],[\"hash\",\"https://example.net?lang=en-US\",\"##nav\"],[\"hash\",\"https://example.net?lang=en-US#nav\",\"#main\"],[\"hash\",\"https://example.net?lang=en-US#nav\",\"#\"],[\"hash\",\"https://example.net?lang=en-US#nav\",\"\"],[\"hash\",\"http://example.net\",\"#foo bar\"],[\"hash\",\"http://example.net\",\"#foo\\\"bar\"],[\"hash\",\"http://example.net\",\"#foo<bar\"],[\"hash\",\"http://example.net\",\"#foo>bar\"],[\"hash\",\"http://example.net\",\"#foo`bar\"],[\"hash\",\"a:/\",\"\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\"],[\"hash\",\"http://example.net\",\"a\\u0000b\"],[\"hash\",\"non-spec:/\",\"a\\u0000b\"],[\"hash\",\"http://example.net\",\"%c3%89t\\u00e9\"],[\"hash\",\"javascript:alert(1)\",\"castle\"],[\"hash\",\"data:space                                                                                                                                  #fragment\",\"\"],[\"hash\",\"sc:space    #fragment\",\"\"],[\"hash\",\"data:space  ?query#fragment\",\"\"],[\"hash\",\"sc:space  ?query#fragment\",\"\"],[\"hash\",\"http://example.net\",\" \"],[\"hash\",\"http://example.net\",\"\\u0000\"],[\"hash\",\"data:text/plain,test\",\"#<foo> <bar>\"],[\"hash\",\"about:blank\",\"#<foo> <bar>\"],[\"hash\",\"data:text/plain,test\",\"#\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\"],[\"hash\",\"about:blank\",\"#\\u0000\\u0001\\t\\n\\r\\u001f !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\\u0080\\u0081\\u00c9\\u00e9\"],[\"href\",\"file:///var/log/system.log\",\"http://0300.168.0xF0\"]];
const fields = (u) => [u.href, u.origin, u.protocol, u.username, u.password, u.host, u.hostname, u.port, u.pathname, u.search, u.hash, String(u), JSON.stringify(u), u.searchParams.toString()].join(\" | \");
const lines = [];
for (const [input, base] of parsing) {
  let line;
  try { line = fields(base === null ? new URL(input) : new URL(input, base)); }
  catch (e) { line = \"threw \" + e.name + \" \" + e.code + \" \" + e.message + \" \" + JSON.stringify(e.input) + \" \" + JSON.stringify(e.base); }
  lines.push(\"P \" + JSON.stringify(input) + \" \" + JSON.stringify(base) + \" => \" + line + \" | \" + (base === null ? URL.canParse(input) : URL.canParse(input, base)) + \" \" + ((base === null ? URL.parse(input) : URL.parse(input, base)) === null));
}
for (const [setter, href, value] of setters) {
  let line;
  try { const u = new URL(href); u[setter] = value; line = fields(u); }
  catch (e) { line = \"threw \" + e.name + \" \" + e.code + \" \" + e.message; }
  lines.push(\"S \" + setter + \" \" + JSON.stringify(href) + \" <- \" + JSON.stringify(value) + \" => \" + line);
}
const groups = new Map();
for (const line of lines) {
  const kind = line.slice(0, 1) === \"S\" ? line.split(\" \").slice(0, 2).join(\" \") : \"parse\";
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
"

// the url standard's own parsing and setter cases through the URL class:
// every getter, origin, searchParams, canParse and parse. Each line is a
// kind of case, how many, and a hash of what they printed in node 26.11.1
pub fn url_standard_cases_match_node_test() {
  let rt = run_commonjs(node_runtime(), url_class_script)
  assert out_lines(rt)
    == [
      "parse 896 991511301",
      "S protocol 35 487576890",
      "S username 13 880338505",
      "S password 12 1675042284",
      "S host 67 3817123848",
      "S hostname 48 919546457",
      "S port 27 590105943",
      "S pathname 33 2235882693",
      "S search 16 3831221569",
      "S hash 26 692811835",
      "S href 1 1042944760",
    ]
}

const url_names_script = "
const url = require(\"url\");
const names = [\"aa--\", \"a\\u2020--\", \"ab--c\", \"-x\", \"-\\u2020\", \"-x.xn--zca\", \"-x.\\u00df\", \"x-.xn--zca\", \"x-.\\u00df\", \"x..xn--zca\", \"x..\\u00df\", \"xn--a\", \"xn--a.xn--zca\", \"xn--a.\\u00df\", \"xn--ls8h=\", \"xn--te\\u0161la\", \"xn--zca.xn--zca\", \"xn--zca.\\u00df\", \"ab--c.xn--zca\", \"ab--c.\\u00df\", \"\\u200d.example\", \"xn--1ug.example\", \"\\u064aa\", \"xn--a-yoc\", \"\\u0dc1\\u0dca\\u200d\\u0dbb\\u0dd3\", \"\\u0646\\u0627\\u0645\\u0647\\u200c\\u0627\\u06cc\", \"\\ufffd.com\", \"xn--zn7c.com\", \"x01234567890123456789012345678901234567890123456789012345678901x\", \"x01234567890123456789012345678901234567890123456789012345678901\\u2020\", \"x01234567890123456789012345678901234567890123456789012345678901x.xn--zca\", \"x01234567890123456789012345678901234567890123456789012345678901x.\\u00df\", \"01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.0123456789012345678901234567890123456789012345678.x\", \"01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.0123456789012345678901234567890123456789012345678.xn--zca\", \"01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.01234567890123456789012345678901234567890123456789.0123456789012345678901234567890123456789012345678.\\u00df\", \"a\\u00adb\", \"\\u2260\", \"\\u226e\", \"\\u226f\", \"=\\u0338\", \"<\\u0338\", \">\\u0338\", \"=\\u00ad\\u0338\", \"<\\u00ad\\u0338\", \">\\u00ad\\u0338\", \"fa\\u00df.de\", \"\\u03b2\\u03cc\\u03bb\\u03bf\\u03c2.com\", \"\\u0dc1\\u0dca\\u200d\\u0dbb\\u0dd3.com\", \"\\u0646\\u0627\\u0645\\u0647\\u200c\\u0627\\u06cc.com\", \"www.loo\\u0138out.net\", \"\\u15ef\\u15ef\\u15ef.lookout.net\", \"www.lookout.\\u0441\\u043e\\u043c\", \"www\\u2025lookout.net\", \"www.lookout\\u2027net\", \"www.lookout.net\\u2a7480\", \"www\\u00a0.lookout.net\", \"\\u1680lookout.net\", \"\\u001flookout.net\", \"look\\u06ddout.net\", \"look\\u180eout.net\", \"look\\u2060out.net\", \"look\\ufeffout.net\", \"look\\ud83f\\udffeout.net\", \"look\\ufffaout.net\", \"look\\u2ff0out.net\", \"look\\u0341out.net\", \"look\\u202eout.net\", \"look\\u206bout.net\", \"look\\udb40\\udc01out.net\", \"look\\udb40\\udc20out.net\", \"look\\u05beout.net\", \"B\\u00fccher.de\", \"\\u2665.net\", \"\\u0378.net\", \"\\u04c0.com\", \"\\ud87e\\udc68.com\", \"\\u2183.com\", \"look\\u034fout.net\", \"gOoGle.com\", \"\\u09dc.com\", \"\\u1e9e.com\", \"\\u1e9e.foo.com\", \"-foo.bar.com\", \"foo-.bar.com\", \"ab--cd.com\", \"xn--0.com\", \"foo\\u0300.bar.com\", \"fass.de\", \"fa\\u00df.de\", \"Fa\\u00df.de\", \"xn--fa-hia.de\", \"xn--0ca.xn--ssa73l\", \"xn--0ca81i.xn--4db\", \"xn--ab-j1t\", \"a\\u094db\", \"A\\u200dB\", \"A\\u094d\\u200db\", \"xn--pkf\", \"xn--3y9a\", \"xn--12345678901234567890123456789012345678901234567890123456-fxe\", \"b\\u00fccher.de\", \"O\\u0308BB\", \"xn--bb-eka\", \"\\u0392\\u039f\\u0301\\u039b\\u039f\\u03a3.COM\", \"\\u0392\\u03cc\\u03bb\\u03bf\\u03c3.com\", \"xn--nxasmm1c\", \"\\u03b2\\u03cc\\u03bb\\u03bf\\u03c3\", \"\\u0392\\u03cc\\u03bb\\u03bf\\u03c2\", \"www.xn--10cl1a0b.com\", \"\\u0646\\u0627\\u0645\\u0647\\u200c\\u0627\\u06cc\", \"\\u0646\\u0627\\u0645\\u0647\\u200c\\u0627\\u06cc.com\", \"a.b\\uff0ec\\u3002d\\uff61\", \"A.B\\uff0eC\\u3002D\\uff61\", \"u\\u0308.xn--tda\", \"xn--tda.xn--tda\", \"\\u00dc.\\u00fc\", \"A\\u2488COM\", \"xn--a.pt\", \"XN--A-\\u00c4.PT\", \"\\u65e5\\u672c\\u8a9e\\u3002\\uff2a\\uff30\", \"\\u65e5\\u672c\\u8a9e.jp\", \"\\u2615\", \"1.ass\\u200c\\u200db\\u200c\\u200dcssssssssd\\u03c3\\u03c3ssssssssssssssssessssssssssssssssssssxssssssssssssssssssssysssssssssssssss\\u015dssz\", \"1.assbcssssssssd\\u03c3\\u03c3ssssssssssssssssessssssssssssssssssssxssssssssssssssssssssysssssssssssssss\\u015dssz\", \"1.Assbcssssssssd\\u03c3\\u03c3ssssssssssssssssessssssssssssssssssssxssssssssssssssssssssyssssssssssssssss\\u0302ssz\", \"\\u200cX\\u200dN\\u200c-\\u200d-BSS\", \"xn--xn--bss-7z6ccid\", \"x\\u034fn\\u200b-\\u00ad-\\u180cb\\ufe00s\\u2064s\\udb40\\uddefffl\", \"\\u02e3\\u034f\\u2115\\u200b\\ufe63\\u00ad\\uff0d\\u180c\\u212c\\ufe00S\\u2064\\ud835\\udd30\\udb40\\uddefFFL\", \"123456789012345678901234567890123456789012345678901234567890123.123456789012345678901234567890123456789012345678901234567890123.123456789012345678901234567890123456789012345678901234567890123.1234567890123456789012345678901234567890123456789012345678901c\", \"a\\u03081234567890123456789012345678901234567890123456789012345\", \"123456789012345678901234567890123456789012345678901234567890123.1234567890a\\u0308123456789012345678901234567890123456789012345.123456789012345678901234567890123456789012345678901234567890123.123456789012345678901234567890123456789012345678901234567890b\", \"123456789012345678901234567890123456789012345678901234567890123.1234567890a\\u0308123456789012345678901234567890123456789012345.123456789012345678901234567890123456789012345678901234567890123.123456789012345678901234567890123456789012345678901234567890b.\", \"123456789012345678901234567890123456789012345678901234567890123.1234567890a\\u0308123456789012345678901234567890123456789012345.123456789012345678901234567890123456789012345678901234567890123.1234567890123456789012345678901234567890123456789012345678901c\", \"123456789012345678901234567890123456789012345678901234567890123.1234567890a\\u03081234567890123456789012345678901234567890123456.123456789012345678901234567890123456789012345678901234567890123.12345678901234567890123456789012345678901234567890123456789a\", \"123456789012345678901234567890123456789012345678901234567890123.1234567890a\\u03081234567890123456789012345678901234567890123456.123456789012345678901234567890123456789012345678901234567890123.12345678901234567890123456789012345678901234567890123456789a.\", \"123456789012345678901234567890123456789012345678901234567890123.1234567890a\\u03081234567890123456789012345678901234567890123456.123456789012345678901234567890123456789012345678901234567890123.123456789012345678901234567890123456789012345678901234567890b\", \"a.b..-q--\\u00e4-.e\", \"A.b..-Q--A\\u0308-.E\", \"a.-.c\", \"A\\u0308.\\u00ad.C\", \"A\\u0308.-B.\", \"A\\u0308.B-.C\", \"\\u00e4.-.c\", \"\\u00e4.bc--de.f\", \"A\\u0308.bc--De.f\", \"a.b.xn--c-bcb.d\", \"\\u05d09\\u05c7\", \"xn--4db6c0a\", \"\\u0bb9\\u0bcd\\u200d\", \"xn--dmc\", \"\\u0bb9\\u0bcd\\u200c\", \"xn--0ug\", \"\\u0644\\u0670\\u200c\\u06ef\", \"xn--ghb25aga\", \"\\u0644\\u06ef\", \"xn--cmba\", \"a\\u3002\\u3002b\", \"\\u2477.four\", \"xn---\", \"\\uf951\\ud87e\\udc68\\ud87e\\udc74\\ud87e\\udd1f\\ud87e\\udd5f\\ud87e\\uddbf\", \"xn--A-1ga\", \"A\\u00f6\", \"xn--1ch\", \"123456789012345678901234567890123456789012345678901234567890123.1234567890A\\u0308123456789012345678901234567890123456789012345.123456789012345678901234567890123456789012345678901234567890123.1234567890123456789012345678901234567890123456789012345678901c\", \"123456789012345678901234567890123456789012345678901234567890123.1234567890\\u00c41234567890123456789012345678901234567890123456.123456789012345678901234567890123456789012345678901234567890123.12345678901234567890123456789012345678901234567890123456789a.\", \"14.xn--7hb713l3v90n.-\", \"\\u200d=\\u0338\\u1899>\\u0338.\\u1109\\u1169\\u11be-\\u1874\\u10a0\", \"=\\u0338\\u1899>\\u0338.\\u1109\\u1169\\u11be-\\u1874\\u2d00\", \"xn--jbf929a90b0b.xn----6zg521d196p\", \"\\ud84f\\udcd4\\u0303.\\ud805\\udcc2\", \"xn--0ug15083f.xn--9kj2034e\", \"\\u7e71\\ud805\\uddbf.i.\", \"xn--gl0as212a.xn--8-o89h\", \"\\u00df\\u06eb\\u3002\\u200d\", \"ss\\u06eb.\", \"\\udb41\\udc35\\u200c\\u2488\\uff0e\\udb40\\udf87\", \"xn--0ug88o47900b.xn--tv36e\", \"xn--ss-3xd2839nncy1m.xn--bb79d\", \"xn--4pb607jjt73a.xn--1ug236ke314donv1a\", \"\\u3164\\u094d\\u2d00\\u17d0.\\u180b\", \"\\u2763\\u200d\\uff0e\\u09cd\\ud807\\udc3d\\u0612\\ua929\", \"xn--nua.xn--bc6k\", \"\\u850f\\uff61\\ud807\\udc3a\", \"\\ud804\\udee7\\ua9c02\\u3002\\u39c9\\uda09\\udd84\", \"\\u2938\\u03c3\\ud8ab\\udc40\\u3002\\u1160\", \"xn--4xa192qmp03d.xn--psd\", \"\\u200d\\udb7d\\udc56\\udb40\\udc50.\\u05bd\\ud826\\udfb0\\ua85d\\ud800\\udee1\", \"\\u3002\\udbcc\\ude35\\u03c3\\ud8c2\\udc07\\u3002\\ud802\\udf88\", \"2.\\udb40\\ude93\\u2260\\u3002\\u10bf\\u2b23\\u10a8\", \"\\u2489\\udb40\\ude93=\\u0338\\uff61\\u2d1f\\u2b23\\u2d08\", \"\\ud83a\\udd37.\\ud802\\udf90\\ud83a\\udc81\\ud803\\ude60\\u0624\", \"-\\udb40\\ude56\\ua867\\uff0e\\udb40\\ude82\\ud8dc\\udd83\\ud83c\\udd09\", \"\\u200c\\u3002\\u0354\", \"\\ud83a\\udd25\\udb40\\udd6e.\\u1844\\u2d0e\", \"\\ud83a\\udd03.\\u1844\\u10ae\", \"xn--de6h.xn--mnd799a\", \"xn--0fd40533g.xn--1-tws\", \"\\u03a3\\ud9d5\\udf0c8.\\ud83a\\udf64\", \"\\u03c3\\ud9d5\\udf0c\\uff18.\\ud83a\\udf64\", \"\\ub253\\u6cd3\\ud833\\udd7d.\\u09cd\\u200d\", \"\\u1b44\\uff0e\\u1baa-<\\u0338=\\u0338\", \"\\u1bf3\\u10b1\\u115f.\\ud804\\udd34\\u2132\", \"\\u1bf3\\u10b1\\u115f\\uff0e\\ud804\\udd34\\u214e\", \"\\u10a9\\u7315\\udba5\\udeeb<\\u0338\\uff0e\\ufe12\", \"xn--gdh892bbz0d5438s..\", \"xn--hnd212gz32d54x5r.xn--y86c\", \"a\\u030a\\u1103\\u116d\\u11b7-.\\u200c\", \"\\u00e5\\ub444-\\uff0e\\u200c\", \"xn--ct2b0738h.xn--772h.\", \"3,.\\u1cdc1.\\u00df\", \"3,.xn--1-43l.xn--zca\", \"3,.xn--zca344lmif\", \"\\u103a\\u094d\\u1dfd\\uff0e=\\u0338\\u200d\\u31db\", \"\\u1bf3.-\\u900b\\ud98e\\uddad\\udb25\\ude6e\", \"xn--9ob.xn--4xa380e\", \"xn--9ob.xn--3xa995lq2l\", \"xn--57e237h.xn--5sa649la993427a\", \"\\ud838\\udc28\\u3002\\u1b44\\uda45\\udee8\\ud838\\udf87\", \"\\u189b\\udb60\\udd5fSs.\\u1327\", \"xn--0ugx66b.xn--0ugz2871c\", \"\\u00df\\u3002\\ud800\\udef3\\u10ac\\u0fb8\", \"ss.xn--lgd921mvv0m\", \"\\u00df.\\ud800\\udef3\\u2d0c\\u0fb8\", \"ss.xn--lgd10cu829c\", \"\\u1856\\uff61\\u031f\\ud91d\\udee8\\u0b82-\", \"\\u0596\\u10ab.7\\u226f\\u3002\\ufe0a\", \"\\u0596\\u2d0b\\uff0e\\ud835\\udff3>\\u0338\\ufe12\\ufe0a\", \"\\ud83c\\udd07\\u4f10\\ufe12.\\ud831\\ude5a\\ua8c4\", \"\\udb40\\udda0\\uff0e\\ud99d\\udc34\\udaf1\\udfc8\", \"\\udafd\\udcb0\\uff0e-\\ud835\\udffb\\u00df\", \"xn--t960e.xn---5-hia\", \"\\u200d\\ud802\\ude3f.\\ud83e\\udd12\\u10c5\\uda06\\udfb6\", \"xn--1ug9533g.xn--9nd3211w0gz4b\", \"xn--ey1p.xn--ss-eq36b\", \"\\udb40\\udd6f\\ud9df\\udf6d\\u200c\\ud83d\\udf2d\\u3002\\ud805\\uddbf\\u1abb\\u03c2\\u2260\", \"\\udb40\\udd6f\\ud9df\\udf6d\\u200c\\ud83d\\udf2d\\u3002\\ud805\\uddbf\\u1abb\\u03c3=\\u0338\", \"\\udb40\\udd6f\\ud9df\\udf6d\\u200c\\ud83d\\udf2d\\uff61\\ud805\\uddbf\\u1abb\\u03a3\\u2260\", \"4..1.xn--sf51d\", \"\\u10b3\\ud805\\udf2b\\u200d\\uda1e\\udf53.\\u06a7\\ud807\\udc36\", \"xn--rnd8945ky009c.xn--9jb4223l\", \"xn--0s9c.xn--8ug8324p\", \"\\u20e7\\ud97e\\udc4e-\\uda6e\\udcdd.4\\u10a4\\u200c\", \"xn----9snu5320fi76w.xn--4-f0g649i\", \"\\u16ad\\u3002\\ud834\\udf20Ss\\ud81a\\udef1\", \"xn--hwe.xn--zca4946pblnc\", \"\\ud805\\udc44\\u226f\\uff61\\ud805\\udf24\", \"\\u10ab\\u226e\\ud887\\udc86\\u3002\\u200d\\u07a7\\ud800\\udee3\", \"xn--gdhz03bxt42d.xn--lrb506jqr4n\", \"xn--u4e.xn--hdhx0084f\", \"\\ud8fc\\udc47\\u1734.\\ud802\\ude3ae\\u0301\\u2b13\\ud804\\udd34\", \"xn--09e4694e..xn--ye6h\", \"\\u2d23\\uff0e\\u0653\\u18a4\", \"\\udb40\\udd08\\u0813.\\u1109\\u1174\\u11b0\\ud9d0\\uddbb\\u10c4\\ud9ca\\udc50\", \"\\udb40\\udd08\\u0813\\uff0e\\uc2c9\\ud9d0\\uddbb\\u2d24\\ud9ca\\udc50\", \"\\uaa2c\\ud807\\udcab<\\u0338.\\u2902\", \"\\ua866\\u1851\\u200d1.\\u3002\\ud800\\udee3-\", \"\\u2488\\u4c39\\u200d-\\u3002\\uc6c8\", \"1.xn----tgnz80r.xn--kp5b\", \"xn--m9j.xn--rtb154j9l73w\", \"xn--4xa.xn--3lb1944f\", \"\\u0bcd\\udb56\\udec5\\ud9f0\\ude51.\\u2d02\\u2d15\", \"\\u1c32\\ud83c\\udd08\\u2f9b\\u05a6\\uff0e\\u200d\\uda7e\\udd64\\u07fd\", \"xn--xcb756i493fwi5o.xn--1tb334j1197q\", \"\\u1897\\uff61\\u04cf\\ud934\\udd3b\", \"\\ud807\\udc98\\udb40\\udd12\\ud80d\\udc61\\u30028\\u10bc\", \"\\u1bab\\uff61\\ud83c\\udc89\\udb40\\udc70\", \"xn----7i12hu122k9ire.xn--0ug\", \"xn--y86c.xn--s96cu30b\", \"xn--xm38e.-\", \"\\u227c\\u0338\\ud800\\udeee.\\uda98\\ude2e\\u0f18SS>\\u0338\", \"\\u22e0\\ud800\\udeee.\\uda98\\ude2e\\u0f18Ss\\u226f\", \"\\u22e0\\ud800\\udeee\\uff0e\\uda98\\ude2e\\u0f18ss\\u226f\", \"\\u0330.\\udb81\\udf31\\u8680\", \"xn--7,-n1t0654eqo3o.xn--3ed\", \"\\u20eb\\u226e.\\ud836\\ude16\", \".xn--hdh\", \"\\u1849\\ud899\\udce7\\u2b1e\\u189c.-\\u200d\\ud83a\\udcd1\\u202e\", \"xn--of6h.xn--inb\", \"\\u10ba\\ud800\\udef8\\udb40\\udd04\\u30025\\ud7f6\\u103a\", \"\\u2d1a\\ud800\\udef8\\udb40\\udd04\\u3002\\ud835\\udfdd\\ud7f6\\u103a\", \"xn---%-u4oy48b.xn--gff52t\", \"xn--1ch.xn--y7e\", \"\\ud84e\\ude6b\\uff0e\\ud9f1\\udc72\", \"\\u0c4d\\ud836\\ude3e\\u05a9\\ud835\\udfed\\u3002-\\ud805\\udf28\", \"xn--ph26c.xn--281b\", \".xn--6x4u\", \"\\ufff9\\u200c\\u3002\\u66f3\\u897e\\ud800\\udef0>\\u0338\", \"\\u226f1.\\u3002\\u00df\", \">\\u03381.\\u3002ss\", \">\\u0338\\u2488\\u3002SS\", \"\\u226f\\u2488\\u3002Ss\", \"\\u200c\\u3002\\u2260\", \"xn--461dw464a.xn--v8e29loy65a\", \"\\uda12\\udcf3\\u200d\\uda05\\udf71.\\ud81a\\udf34\\u2184\\u2260-\", \"\\u200d\\u252e\\udb40\\uddd0\\uff0e\\u0c00\\u0c4d\\u1734\\u200d\", \"\\udaa5\\udeaa\\u30021,\", \"\\udb40\\udf0f3\\uff61\\u1bf1\\ud835\\udfd2\", \"\\ua87d\\u226f.\\udaaf\\udc80\\uda0b\\udcc4\", \"\\udb43\\udcdb.\\u200d\\u492b\\u2260\\u10be\", \"xn--1t56e.xn--1ug73gzzpwi3a\", \"3.1.xn--110d.j\", \"\\ud6c9\\u226e\\uff61\\u0e34\", \"\\ua846\\u3002\\u2183\\u0fb5\\ub1ae-\", \"xn--fc9a.xn----qmg787k869k\", \"xn--gdh.xn--4tjx101bsg00ds9pyc\", \"xn--8v1d.xn--1ug1386plvx1cd8vya\", \"Ss\\u09c1\\u1ded\\u3002\\u062085\", \"xn--zca266bwrr.xn--85-psd\", \"\\ufe0d\\u0a9b\\u3002\\u5d68\", \"-18.xn--rx9c.xn--382h\", \"\\ufe05\\u3002\\u3002\\ud858\\udc3e\\u1ce0\", \"\\udb41\\udd4f\\uff0e-\\u00df\\u200c\\u2260\", \"\\udb41\\udd4f.-SS\\u200c\\u2260\", \"xn--u836e.xn---ss-gl2a\", \"\\udb41\\udd4f\\uff0e-ss\\u200c\\u2260\", \"\\u1859\\u200c\\uff61>\\u0338\\ud800\\udef2=\\u0338\", \"\\u1859.>\\u0338\\ud800\\udef2=\\u0338\", \"xn--1fb94204l.xn--tnd\", \"\\u2488\\u0dd6\\u7105.\\udb1e\\udc59\\u200d\\ua85f\", \"xn--t1c337io97c.xn--1ugz184c9lw7i\", \"-3.xn--fbf739aq5o\", \"\\u214e\\u17d2\\u200d\\u3002=\\u0338\\u200d\\u200c\", \"\\u2132\\u17d2.=\\u0338\", \"xn--u4e319b.xn--1ch\", \"\\ud9a9\\udd2f\\u0fa8.>\\u0338\", \"xn--l76a726rt2h.xn--4xa\", \"\\u03c3-\\u3002\\u200c1-\", \"\\u03c3-\\u3002\\u200c\\ud835\\udfed-\", \"\\u1734-\\u0ce2\\uff0e\\udb40\\udd29\\u2d04\", \"xn--1ug.xn--1-1p4r.xn--s7uv61m\", \"\\u2ad0\\u3002\\u2d20-\\udacd\\udc22\", \"\\ud805\\udc42\\u25ca.\\u299f\\u2220\", \"\\u1112\\u1164\\u11bc\\udb21\\udd99\\u0e3a\\udb28\\udf5a\\u3002\\u06ba4\", \"\\udae2\\udedd\\uda69\\udef8\\u200d\\u3002\\u4716\", \"xn--8,-g9oy26fzu4d.xn--kmb6733w\", \"\\u135f\\u1848\\u200c.\\u3002-\\ud81b\\udf90-\", \"\\ud836\\ude5c\\u3002-\\u0b4d\\u10ab\", \"9\\u9681\\u2bee.\\u180d\\u200c\", \"xn--mta176jjjm.c\", \"\\udb40\\udd01\\u035f\\u2fb6\\uff61\\u2087\\ufe12\\ub207\\u226e\", \"xn--9ua0567e.xn--7-ngou006d1ttc\", \"\\u200c\\u3002\\u1160\\u0f84\\u0f96\", \".xn--3ed0by082k\", \">\\u0338\\ud9f5\\ude05.\\u200d\\ud800\\udd7c\\uda88\\udddb\", \"\\ua9d0\\u04c0\\u1baa\\u08f6.\\ub235\", \"\\ua9d0\\u04cf\\u1baa\\u08f6\\uff0e\\u1102\\u116f\\u11bc\", \"xn--3g9a.xn--ud1dz07k\", \"xn--3e2d79770c.xn--hdh0088abyy1c\", \"\\ud944\\udd48\\u782a\\u226f\\u1891\\u3002\\u226f\\ud836\\ude5a\\uda0f\\udd14\\u200c\", \"\\u10c5.\\ud804\\udd3343\", \"\\ud91e\\udea8\\udb40\\udd09\\uffa0\\u0fb7.\\ud9a1\\udfb0\\ua953\", \"\\u0618.\\u06f3\\u200c\\ua953\", \"xn--c8e..xn--bbf\", \"\\ud9f0\\uded4\\udb40\\udd8e\\udb40\\udd97\\ud807\\udc95\\u3002<\\u0338\", \"\\u40b9\\udbb9\\udd85\\ud800\\udee6\\uff0e\\u200d\", \"\\ufe12\\uff61\\u10a3>\\u0338\", \"..xn--hdh782b\", \"xn--y86c.xn--bnd622g\", \"\\u7b83\\u2d21-\\udb40\\udc5d\\u3002=\\u0338-\\ud83e\\udd16\", \"xn----11g3013fy8x5m.xn----tfot873s\", \"\\ud83a\\udd3f\", \"\\ufe12\\uff61\\u1b44\\u1849\", \"-\\u1bab\\u3002\\u200d.\\ud90b\\udd88\\ud957\\ude53\", \"\\udb42\\uddae.\\u226f\\ud838\\udc06\", \"xn--skb.xn--g9e\", \"xn--mbm8237g..xn--7-7hf\", \"SS\\u200c\\uaaf6\\u18a5.\\u22b6\\u10c1\\u10b6\", \"SS\\uaaf6\\u18a5.\\u22b6\\u10c1\\u10b6\", \"SS\\u200c\\uaaf6\\u18a5\\uff0e\\u22b6\\u10c1\\u10b6\", \"xn--ss-4epx629f.xn--undv409k\", \"\\u200d\\u3002\\u03c3\\udb40\\udc49\", \"11.\\uda61\\ude19\\uda8f\\udce0\\ud805\\udcc0.-\\udb3a\\udc4a\", \"-.\", \"\\u03c2\\u200d-.\\u10c3\\ud859\\udfd9\", \"xn----zmb048s.xn--rlj2573p\", \"\\u2260\\u3002\\ud83d\\udfb3\\ud835\\udff2\", \"\\u2260.\\ud83d\\udfb36\", \"\\u08e6\\u200d\\uff0e\\u1108\\u1168\\u11c0\", \"\\ud8f6\\ude3d\\uff0e\\ud8ef\\ude15\", \"\\ud802\\udfc0\\ud803\\ude09\\ud83a\\uddcf\\u3002\\ud949\\udea74\\u2d0b\\ud8cb\\ude6b\", \"5\\u3002\\u06d7\", \"xn--0ug26167i.xn--6g3a\", \"xn--y86c71305c.xn----t2c\", \".j\", \"xn--5cb172r175fug38a.xn--mlj\", \"xn--5cb347co96jug15a.xn--2nd\", \"\\u08df\\u10ab\\ud89b\\udff8\\u1100\\u1172\\u11af\\uff0e\\uda40\\udd7c\\ud835\\udfe2\\u1112\\u1171\\u11b9\\u0ae3\", \"xn--i0b436pkl2g2h42a.xn--0-8le8997mulr5f\", \"\\u0784.\\ud83a\\udc5d\\u0601\", \"xn--3-yke.xn--8-ugnv982dbkwm\", \"xn--cld333gn31h0158l.xn--3g0d\", \"xn--4-0bd15808a.\", \"-.xn--xco\", \"\\ud804\\uddc0.\\udb42\\ude31\", \"\\u820c.\\ua8485>\\u0338\\u00df\", \"\\u820c.\\ua8485>\\u0338Ss\", \"\\u2f86\\uff0e\\ua848\\uff15\\u226fSS\", \"\\ud83a\\udd2a.\\u03c2\", \"\\ud83a\\udd08.\\u03c2\", \"\\u200c\\u2d1a\\u3002\\u03c2\", \"\\u10ba.\\u03a3\", \"xn--0ug262c.xn--4xa\", \"xn--ynd.xn--4xa\", \"\\u200d\\u2f95\\u3002\\u200c\\ua953\\u0310\\ua84e\", \"xn--34w.xn--x7e\", \"xn--8di78qvw32y.xn--k80d\", \"xn--45e.xn--et6h\", \"\\u00df\\ud880\\udc3b\\ud8da\\udf17\\uff61\\ud836\\ude68\\ud83d\\udd6e\\u00df\", \"xn--ss-jl59biy67d.xn--ss-4d11aw87d\", \"\\u200d\\u3002\\u200c\", \"\\ua9b7\\udb37\\udd59\\uba79\\u3002\\u249b\\udb42\\ude07\", \"xn--ym9av13acp85w.xn--dth22121k\", \".xn--y86c\", \"\\u1872-3.ss-\\u200c-\", \"\\u1872-\\ud835\\udff9.SS-\\u200c-\", \"\\uda1c\\udda7\\ud835\\udfef\\u3002\\u2488\\u1a76\\ud835\\udfda\\uda41\\ude0c\", \"\\u200d\\u2085\\u2488\\u3002>\\u0338\\ud835\\udff4\\u200d\", \"xn--5-ecp.xn--8-ogo\", \"\\ud8bb\\uddc2\\u0a42\\u2d0a\\ud8c8\\udc9f.\\u226e\", \"xn--5c9a.xn--fmb\", \"xn--5,-i1tz135dnbqa.xn--4sf36u6u4w\", \"xn--b5q.xn--v7e6041kqqd4m251b\", \"\\u1868-\\uff61\\udb43\\udecb\\ud835\\udff7\", \"\\u2d1c\\ud9e3\\udded\\u0f80\\u821b\\u3002\\u2d0f\\u2640\\u200c\\u200c\", \"xn--zed54dz10wo343g.xn--nnd089ea464d\", \"\\ud97b\\udd18\\u10be\\u7640\\uff61\\ud805\\ude3f\\u200d\\u200c\\ubdbc\", \"\\ud97b\\udd18\\u2d1e\\u7640\\u3002\\ud805\\ude3f\\u200d\\u200c\\ubdbc\", \"xn--2nd6803c7q37d.xn--et3bn23n\", \"\\u18435>\\u0338\\u1823.\\u6c01\\ud960\\udff1\\ua06b\", \"xn----scp6252h.xn--zshy411yzpx2d\", \"\\u200c\\u2d01\\u755d\\u200d.<\\u0338\", \"\\u10a1\\u755d.<\\u0338\", \"xn--8md0962c.xn--gdh\", \"xn--t8c059f.xn--iz4a43209d\", \"\\ud8bc\\udc2b.\\u1baa\\u03c2\\u2d06\\u200d\", \"xn--nu4s.xn--4xa153jk8cs1q\", \"\\ud8bc\\udc2b\\uff0e\\u1baa\\u03a3\\u2d06\\u200d\", \"1.\\u200c\\uaaec\\u3002.\\u0acd\", \"\\u00df\\u200d.\\u1bf2\\ud8d3\\udfbc\", \"xn--ss-n1t.xn--0zf22107b\", \"xn--0ugy6glz29a.xn--gdh\", \"xn--my8h.xn--psd\", \"\\udb39\\udf43\\u3002\\uda04\\udd83\\ud8e6\\udc97--\", \"\\u03c2\\u2d25\\u3002\\u075a\", \"\\u03c3\\u2d25.\\u075a\", \"xn--4xa477d.xn--epb\", \"\\u06bc.Y\\u0307\\u1864\", \"\\ud802\\ude57.\\u10a9\\u2d15\", \"xn--1fk.xn--vta284a9o563a\", \"xn--3-ews6985n35s3g.xn--7-cve6271r\", \"xn--eco.1.xn--ms39a\", \"1\\uaaf6\\u00df\\ud807\\udca5\\u3002\\u1dd8\", \"\\uff11\\uaaf6SS\\ud807\\udca5\\uff61\\u1dd8\", \"\\ud88a\\udd31\\u249b\\u2fb3\\uff0e\\ua866\\u2488\", \"\\u10b5\\u3002\\u06f0<\\u0338\\u00df\\u0745\", \"\\u2d15\\u3002\\u06f0<\\u0338ss\\u0745\", \"\\u2d15.\\u06f0\\u226ess\\u0745\", \"\\u10b5.\\u06f0<\\u0338Ss\\u0745\", \"xn--tnd.xn--zca912alh227g\", \"\\u2132\\udb40\\udd7a\\ud937\\udd52\\u3002>\\u0338\\u2f91\", \"xn--73g39298c.xn--hdhz171b\", \"\\u200c.\\u00df\\u2d09-\", \"xn--0ug.xn--ss--bi1b\", \"\\u9f59--\\ud835\\udff0.\\u00df\", \"xn----4-p16k.ss\", \"\\udb42\\udea2-\\u3002\\uda2c\\udc8f\\u226e\\ud805\\udf2b\", \"\\u200c\\udb40\\ude79\\u200d\\u3002\\ud9f3\\udfe7<\\u0338\\u2d09\", \"xn--0ugc90904y.xn--hnd112gpz83n\", \"\\ud836\\ude9e\\u2d10\\u3002\\u110d\\u1168\\u11a8\", \"xn--ond3755u.xn--pi6b\", \"\\ud88a\\udf9a\\uff19\\ua369\\u17d3\\uff0e\\u200d\\u00df\", \"\\ud88a\\udf9a9\\ua369\\u17d3.ss\", \"\\ud88a\\udf9a\\uff19\\ua369\\u17d3\\uff0e\\u200dss\", \"\\u2490\\u226f-\\u3002\\ufe12\\uda65\\udc63-\\ud939\\udee0\", \"xn----ogot9g.xn----n89hl0522az9u2a\", \"xn----3vs.xn--1ug532c\", \"xn--fze.xn----ly8i\", \"\\uabe8-.\\uda60\\udfdc\\u05bdSs\", \"\\uabe8-\\uff0e\\uda60\\udfdc\\u05bdSs\", \"-\\u3002\\uda14\\udf00\\u200d\\u2761\", \"xn--52-dwx47758j.xn--kd3hk431k\", \"\\u03a3.\\u0641\\u0645\\u064a\\ud83d\\udf9b1.\", \"\\u03c3\\uff0e\\ufdc1\\ud83d\\udf9b\\u2488\", \"9.xn--dbf91222q\", \".xn--hva754s.xn--0ug\", \".xn--hva929d.xn--0ug\", \"\\u10b6\\u0366.\", \"xn--hzb.xn--bnd2938u\", \"-\\ud838\\udc24\\udb32\\udc10\\u3002\\ud9e2\\udf16\", \"xn--etlt457ccrq7h.xn--jgb476m\", \"xn--0tb8725k.xn--tu8d.xn--7kj73887a\", \"\\u10c5\\u26ad\\udb41\\uddab\\u22c3\\uff61\\ud804\\udf3c\", \"xn--9nd623g4zc5z060c.xn--ro1d\", \"\\ud8c8\\ude26\\u5e37\\u3002\\u226f\\u843a\\u1dc8-\", \"xn--p9ut19m.xn----mck373i\", \"xn--1ug592ykp6b.xn----k1g451d\", \"xn--9r8a.16.xn--3-nyc0117m\", \"xn--0ug2473c.xn--3-nyc678tu07m\", \"xn--1-5bt6845n.xn--1ug\", \"f\\udb40\\udd5f\\uff61\\ud9fd\\uddc5\\u265a\", \"\\u0b4d\\ud804\\udd34\\u1de9\\u3002\\ud835\\udfee\\u2d18\\ud838\\udc28\\ud8ce\\udd47\", \"xn--1-rgna61159u..xn--4grs325b\", \"\\u200c\\u200c\\uff61\\u2488\\u226f\\ud835\\udff5\", \"xn--0uga.1.xn--9-ogo\", \"xn--w0g.xn----bd0j\", \"\\ud804\\ude35\\u5eca.\\ud802\\udc0d\", \"\\u10be\\ud899\\udec0\\ud82d\\uddfb.\\u1897\\u1105\\u1174\\u11c2\", \"\\u2d1e\\ud899\\udec0\\ud82d\\uddfb\\uff0e\\u1897\\ub9ab\", \"SS\\u103a.B.\", \"ss\\u200d\\u103a\\uff61\\u2488\", \"SS\\u103a.b.\", \"xn--clb2593k.xn--ss-toj6092t\", \"\\u06cc\\ud802\\ude3f\\uff0eSs\\u0f84\\ud804\\udf6c\", \"xn--8-ngo.\", \"xn--8-sgn10i.xn--z3e\", \"\\u1895>\\u0338\\u3002\\ud8d0\\udcaf.\\u2d00\", \"xn--fbf851cq98poxw1a.xn--rkj\", \"xn--vfd.xn----fhd\", \"xn--tbg.xn--11-5o7k.1.xn--k469f\", \"xn--tsh24g49550b.xn--9-o706d\", \"xn--4xa.xn--0f9ars\", \"\\u7f9a\\uff61>\\u0338\", \"\\u7f9a.>\\u0338\", \"\\u2786\\ud99e\\uddd5O\\u0302\\u03031..\\uda06\\udf12\\ud945\\ude2e\\u085b9\", \"xn--6lg26tvvc6v99z.xn--9-6jd87310jtcqs\", \"\\u073c\\u200c-\\u3002\\ud80d\\udc3e\\u00df\", \"xn----s2c071q.xn--ss-066q\", \"xn----b5h1837n2ok9f.xn----mkmw278h\", \"\\ud807\\udc3a.-\\uda05\\udfcf\", \"\\u10ad\\uff0e\\ud8f4\\udde6\\u200c\", \"\\u2d0d\\uff0e\\ud8f4\\udde6\\u200c\", \"3.\\u7b20\", \"xn----ugn.xn--mlj8559d\", \"\\u200d\\u03a3SS\\u0731.\\u0bcd\", \"\\u200d\\u03a3\\u00df\\u0731.\\u0bcd\", \"\\u200d\\u03c3ss\\u0731\\uff0e\\u0bcd\", \"=\\u0338\\uff0e\\u200d\", \"=\\u0338.\", \"\\udb5c\\udef5\\u09cd\\u03c3.\\u03c2\\ud802\\ude3f\", \"xn--4xa502av8297a.xn--3xa8055k\", \"\\udb5c\\udef5\\u09cd\\u03a3\\uff0e\\u03c3\\ud802\\ude3f\", \"\\ud94e\\udd12\\u3002\\u1105\\u1172\\u11b6\", \"\\udbef\\udfe6\\uff61\\u183d\", \"xn--b6s0078f.xn--0ic\", \"\\udaa9\\uded5\\u10a3\\u4805\\uff0e\\ud803\\ude11\", \"xn--bnd074zr557n.xn--yl0d\", \"\\u200d.F\", \"\\u200d\\u3a32\\uff61\\u00df\", \"xn--9bm.ss\", \"xn--1ug914h.xn--zca\", \"\\u200d.\\udbc3\\ude28\", \"\\ud94e\\udf7b\\ud8f2\\udd41\\u3002\\u22606\", \"\\uaaf6\\u188f\\u0e3a2.\\ud800\\udee2\\u0745\\u0f9f\\u3002\", \"\\udad7\\udd27\\u3002\\u2260-\\udb41\\ude44\\u8d70\", \"0.xn--qny\", \"-\\uff61\\u1898\", \"\\ud83c\\udcb4\\u2d0b.<\\u0338\", \"\\u74bc\\ud836\\ude2d\\u3002\\u200c\\udb40\\udddf\", \"xn--0ug.xn--0ug\", \"\\u10b7\\uff0e\\ud804\\udd34\\u05c2\\ua9b7\\ud920\\udce8\", \"\\u2d17\\uff0e\\u05c2\\ud804\\udd34\\ua9b7\\ud920\\udce8\", \"xn--tsh4490bfe8c.xn--8zb\", \"xn--co6h.xn--1-kwssa\", \"xn--l98a.xn--14-jsj57880f.\", \"84\\ud834\\ude3b.\\ud800\\udef5\\u26e7\", \"<\\u03387.\\u8b16\\u00df>\\u0338\", \"<\\u03387.\\u8b16Ss>\\u0338\", \"\\u226e\\ud835\\udfd5\\uff0e\\u8b16SS\\u226f\", \"\\ud975\\udf0e\\u2488\\uff61\\u200c\\ud835\\udfe4\", \"xn--tsh94183d.xn--2-rgn\", \"\\u200d\\u200c\\udb40\\uddaa\\u3002Ss\\ud805\\udcc3\", \"\\u200d\\u200c\\udb40\\uddaa\\uff61ss\\ud805\\udcc3\", \"Ss\\ud805\\udcc3\", \"xn--qekw60dns9k.xn--gd9a\", \"\\u200c1.\\ud852\\udf8d.\\udb49\\udccb\\u1a60\", \"\\ud834\\udd75\\uff61\\ud835\\udfeb\\ud838\\udc08\\u4b3a\\u2488\", \"xn--3f1h.xn--9-ecp936non25a\", \"-\\udb40\\udd710\\uff61\\u17cf\\u1dfd\\u1110\\u1168\\u11aa\\u1109\\u1175\\u11b8\", \"\\u115f\\u10bf\\u10b5\\u10e0\\u3002\\u0b4d\", \"\\u115f\\u10bf\\u10b5\\u1ca0\\uff61\\u0b4d\", \"\\u115f\\u10bf\\u2d15\\u10e0\\uff61\\u0b4d\", \"3\\ua8c4\\ud834\\uddaa\\u3002\\ua8ea-\", \"\\u10a6\\udaae\\udca9\\uff0e\\udb40\\udda1\\ufe09\\ud83a\\udd0d\", \"xn--end82983m.xn--ne6h\", \"xn--mi60a.xn--6-sl4es8023c\", \"xn--6nd5215jr2u0h.xn--skj\", \"\\ud91d\\udc7f\\ua8064\\uda65\\udf86\\u3002\\ud88a\\ude67\\udb41\\udcb9\\u03c3\", \"\\ud8ba\\udcac\\u3002\\u0729\\u3002\\ucbd95\", \"\\u17ca.\\u200d\\ud835\\udfee\\ud804\\udc3f\", \"\\uaaf6\\u3002\\u5b36SS\\u847d\", \"\\u03c2\\ud805\\udc3d\\ud896\\udc88\\ud805\\udf2b\\uff61\\ud83a\\udf29\\u200c\\ud802\\udec4\", \"xn--4xa2260lk3b8z15g.xn--0ug4653g2xzf\", \"\\u2ea2\\ud9df\\ude852\\u3002\\u200d\\ud83d\\udeb7\", \"-\\u1897\\u200c\\ud83c\\udd04.\\ud805\\udf22\", \"xn----pck312bx563c.xn--9h2d\", \"\\u200c\\ud805\\udcc2\\u3002\\u2488-\\udbc2\\ude9b\", \"xn--0ugy057g.xn----dcp29674o\", \"\\u0f94\\ua84b-.-\\ud81a\\udf34\", \"\\ud9bd\\udcb3-\\u2291\\u0338\\u200c.\\u6807-\", \"\\u3002\\u0635\\u0649\\u0e37\\u0644\\u0627\\u3002\\u5c93\\u1bf2\\udb43\\udf83\\u1842\", \"\\u1039-\\ud82a\\udfad\\ud83d\\udfa2\\uff0e\\u00df\", \"xn----9tg11172akr8b.ss\", \"\\u0d4d-\\u200d\\u200c\\uff61\\ud955\\udfa7\\u2085\\u2260\", \"xn----jmf215lda.xn--5-ufo50192e\", \"\\u10c1\\u10b16\\u0318\\u3002\\u00df\\u1b03\", \"xn--6-8cb7433a2ba.xn--ss-2vq\", \"\\u2d21\\u2d116\\u0318.\\u00df\\u1b03\", \"\\ud9ee\\udc50\\uff61>\\u0338\\ud804\\udeea\", \"\\udb40\\udd0f\\ud81a\\udf34\\udb43\\udcbd\\u3002\\u1160\", \"\\udb42\\udf54.\\ud800\\udef12\", \"\\u2dbf.Ss\\u200d\", \"xn--7zv.\", \"\\u0853.SS\", \"\\u1894\\u2260\\udbec\\ude42.\\u200d\\ud800\\udee2\", \"-\\u3002\\u2e90\", \"\\ud800\\udf7a.\\ud928\\uddc3\\u200c\", \"\\u200d\\u226e.\\udb41\\udfea\\ud8a6\\udecf-\", \"\\u200d\\u200d\\u8954\\u3002\\u2d1c5\\ua86e\\ud995\\udf4f\", \"xn--ix9c26l.xn--q0s\", \"xn--1ug1435cfkyaoi04d.c\", \"=\\u0338\\u81a3\\u3002\\u0f83\", \"xn--r97c.xn--1ug\", \"\\ud804\\udd80\\u4074\\ud952\\udde3.\\u2d154\\u200c\\u0348\", \"xn--1mnx647cg3x1b.xn--4-zfb324h32o\", \"1\", \"\\u0644.\\u0130\", \"\\u05d0.\\u0628\", \"\\u05d0.\\u00ad.\\u05d0\", \"z.z\", \"\\u0663.\\ud83d\\ude00.a\", \"\\u0628.a\", \"z.\\u00ad\", \"xn--\", \"xn--.\\u0628\", \"\\u05ea.\\u05ea\", \"\\u0130\", \"\\u05d0.\\u094d\", \"\\u0663\", \"z\\u0300.z\", \"\\u0663.\\u0300.\\u0300\", \"\\u03c2\", \"9\", \"\\ud83d\\ude00.\\u0130\", \"\\u00e9\", \"\\u0130.xn--\", \"z\", \"\\u05d0.9\", \"\\u00ad.\\u0130\", \"\\u05ea.\\u05ea.\\ud83d\\ude00\", \"\\u2168\", \"xn--.\\u0628\", \"\\u0300.xn--.\\u0663\", \"\\u0644\", \"\\u05d0.a.\\u0130\", \"\\u200c.\\u0130\", \"\\u0663.\\u00ad\", \"a\", \"\\u0663.\\u0644\", \"a\", \"9.\\u0300.\\u094d\", \"\\u2168\", \"a.\\u0663\", \"\\u0663.\\ud83d\\ude00\", \"-\", \"xn--.-.z\", \"\\u03c2.\\u200c\", \"\\u05d0\", \"\\u00e9.\\u0663\", \"\\u2168.\\u0663\", \"\\u0130\", \"\\ud83d\\ude00\", \"z.\\u05ea\", \"\\u05ea.\\u03c2.\\u0300\", \"\\u094d\", \"\\u0644\", \"\\u200c\", \"\\u00e9.\\u200c.z\", \"\\u00e9.\\u200c\", \"xn--.\\u094d\", \"\\ud83d\\ude00\", \"\\u0663.\\u00ad.\\u0591\", \"\\u00e9\", \"\\u0300.\\u094d\", \"9\", \"\\u200c.\\u0130\", \"\\u00ad\", \"\\u0300\", \"\\u0663.\\u00e9\", \"\\u0300.9.\\u00ad\", \"\\u0300\", \"\\u200c\", \"\\u05d0.\\u05ea.\\ud83d\\ude00\", \"\\u0644.-\", \"\\ud83d\\ude00\", \"\\u094d.\\u05ea.z\", \"-\", \"\\u0300.a\", \"a.-\", \"\\u03c2\", \"\\u0130.a\", \"\\u0300\", \"xn--.a\", \"\\u05d0.\\u094d.\\u03c2\", \"\\u0300.\\u00e9\", \"\\u0300.\\u0628.\\u0628\", \"9.\\u03c2\", \"\\u0628.\\ud83d\\ude00\", \"\\u0130.\\u00ad\", \"\\u0644.\\u0130\", \"\\u0300.\\u05ea\", \"\\u00ad.\\u00ad\", \"z.-\", \"9.\\u00ad\", \"\\u05d0.\\u03c2\", \"\\u094d.\\u05d0\", \"xn--.9\", \"z.-.\\u2168\", \"\\u00e9\", \"\\u0663.\\u05ea\", \"\\u05ea.z\", \"\\u05ea\", \"\\u0628.\\u200c.z\", \"\\u03c2\", \"z.a.\\ud83d\\ude00\", \"9\", \"xn--.xn--\", \"\\u00ad\", \"\\u0663.\\u0300\", \"\\u05ea.\\u200c.z\", \"xn--.z\", \"9.\\ud83d\\ude00\", \"\\u05d0.xn--.a\", \"\\u0300.z.\\u094d\", \"\\u03c2\", \"\\u0628.\\u0130.xn--\", \"\\u0663\", \"\\u094d\", \"\\u0663.z\", \"z\", \"\\u0644\", \"a.\\u200c.z\", \"\\u2168.\\u03c2\", \"\\u0628\", \"\\u03c2.xn--.\\u0644\", \"z.-.\\u094d\", \"\\u05d0\", \"\\u200c.\\u00e9\", \"\\u0628\", \"z.\\u0300.\\u0644\", \"\\u0300\", \"\\u05ea\", \"\\u00ad.\\u2168.a\", \"\\u200c.-\", \"\\u0130.\\ud83d\\ude00.\\u2168\", \"z\", \"\\u094d.\\u00e9\", \"\\u0644\", \"\\u05d0.\\u0300.\\u05ea\", \"\\u094d\", \"\\u0130\", \"\\u05d0.\\u00ad.\\u0644\", \"\\u0130\", \"\\u0300.\\u0130\", \"9.\\u200c.\\ud83d\\ude00\", \"\\u0300\", \"\\u05ea.\\u0644.xn--\", \"\\u0130.-\", \"\\u0628\", \"\\u0300.\\u0663.\\u200c\", \"-.a\", \"\\u05d0\", \"\\u03c2\", \"\\u0300.\\u0644\", \"\\u0628\", \"\\u00ad.\\u03c2.\\u05d0\", \"\\u05d0.xn--\", \"\\u05ea.\\u05ea\", \"\\u200c\", \"\\u0644\", \"\\u0628\", \"\\u094d.\\u2168.\\u0663\", \"\\u0644\", \"\\u05d0.a\", \"\\u0628.\\u03c2.\\u00e9\", \"\\u00e9.-\", \"\\u00ad.9.\\u2168\", \"-.a.\\u0644\", \"a.xn--\", \"9.\\u0130.\\u00ad\", \"\\u03c2.z.\\u05ea\", \"9\", \"xn--.\\u0663.\\ud83d\\ude00\", \"xn--.9\", \"\\u0628.-.a\", \"\\u200c\", \"a\", \"\\u094d\", \"\\u0300\", \"\\u0628.z\", \"xn--\", \"9\", \"\\u2168\", \"a.-\", \"xn--.\\u05d0.\\u0663\", \"\\ud83d\\ude00.\\u0663\", \"\\u094d.\\u00e9\", \"\\u0644\", \"\\u200c.z.\\ud83d\\ude00\", \"\\u200c.\\u05d0\", \"9.a\", \"\\u0300\\u0663.\\u094d\", \"\\u0663.\\u094d\", \"\\u0663.xn--.z\", \"a\", \"-\", \"\\u0300.\\u05ea.-\", \"\\u00e9.\\u0130.z\", \"\\u0300\", \"\\u00e9.\\u0663\", \"\\u0663.\\u0663\", \"\\u0130.-\", \"a.\\u00e9\", \"\\u03c2.\\u0130.\\u0628\", \"\\u03c2.\\u03c2\", \"9\", \"\\u00ad.a.\\u094d\", \"\\u0300\", \"9\", \"\\u0300\", \"\\u0130.\\ud83d\\ude00\", \"\\u0300\", \"\\u2168.xn--\", \"\\u0644\", \"\\u2168.\\u00e9\", \"\\ud83d\\ude00.\\u0644.-\", \"\\u0300\", \"\\u094d.\\u0628\", \"\\u0663.\\u0644.\\u00ad\", \"\\u200c.\\u0300.-\", \"\\u05d0.\\u0130.\\u0628\", \"\\u03c2.\\u00ad.\\u05d0\", \"\\u0300.a.\\u0663\", \"\\u03c2.-\", \"a\", \"\\u00e9.\\ud83d\\ude00\", \"\\u00ad.\\u200c\", \"9.\\u05ea\", \"xn--\", \"9\", \"\\u03c2.a\", \"\\u05d0.\\ud83d\\ude00.xn--\", \"\\u0644\", \"\\u03c2.z\", \"\\u05d0.\\u0300.\\u0644\", \"z\", \"\\u05d0.\\u0130\", \"a.a\", \"\\u05d0.\\u0130\", \"\\u0130\", \"z.\\u03c2.\\u05d0\", \"xn--\", \"\\u0663\", \"9.\\u05d0\", \"9.xn--.\\u0628\", \"\\u0663\", \"\\u00ad\", \"a\", \"xn--.\\ud83d\\ude00\", \"\\u00e9.z.\\u03c2\", \"\\u0663\\u2168\", \"\\u00ad\", \"\\u00e9.\\u0644\", \"\\u0644\", \"\\u0628.\\u05d0\", \"\\u0130\", \"\\u2168.\\u03c2\", \"xn--.9.9\", \"\\u03c2.a\", \"a.\\u05ea.z\", \"z.\\ud83d\\ude00.a\", \"\\u200c.\\u2168.\\u0663\", \"\\u0663\", \"\\u00e9\", \"\\u200c.\\u0628.\\u0644\", \"\\u200c.a.a\", \"a.z\", \"\\u00e9.9.\\u00e9\", \"\\u0130.\\u0644\", \"\\u2168.\\u0663\", \"\\u00e9.\\ud83d\\ude00.\\u0644\", \"\\u0300.xn--\", \"\\u200c\", \"\\u0628\", \"\\u0628.\\u0663.a\", \"\\u05ea.xn--\", \"xn--\", \"\\u200c.\\u05d0\", \"\\u05d0.-.\\u0663\", \"\\u2168\", \"9.\\u2168.-\", \"\\u0644.\\u00ad\", \"\\u00ad\", \"\\u2168.\\u05ea.a\", \"\\u0644.\\u00ad\", \"\\u0300.\\u2168\", \"\\u0130.\\u00e9\", \"\\ud83d\\ude00.z\", \"\\u2168.-.\\u05d0\", \"a.\\u05ea\", \"\\u03c2\", \"\\u00ad.\\u0130\", \"\\u0644.\\u00ad\", \"\\u0663.\\u0663\", \"\\ud83d\\ude00\", \"xn--.a\", \"\\u200c.\\u00ad\", \"\\u00ad.\\u094d\", \"\\ud83d\\ude00.\\u094d.\\u0644\", \"9.9\", \"\\u0663.\\u200c.-\", \"\\u0663\", \"\\u2168.\\u0663\", \"\\ud83d\\ude00.9.\\u094d\", \"\\u05ea.-\", \"\\u0130.\\u03c2\", \"\\u0628\", \"\\u00ad.a.\\u200c\", \"a.a\", \"\\u0644.\\u05ea\", \"\\u00e9.z\", \"a\", \"xn--\", \"\\u0300.\\u094d\", \"\\u00ad.\\u2168\", \"\\u00e9\", \"\\u2168.xn--\", \"a\", \"\\u094d.\\u05d0\", \"-.9.9\", \"\\u0130.\\u00e9\", \"\\u0644\", \"\\u05d0\", \"z.-\", \"\\u200c\", \"\\u094d.\\u0663.xn--\", \"\\u0663.\\u094d\", \"9.\\u0663.\\u00e9\", \"\\u0300.\\u200c\", \"\\ud83d\\ude00.xn--\", \"\\u094d\", \"xn--.\\u05ea.\\u0300\", \"\\ud83d\\ude00\", \"\\u0628.\\u200c\", \"\\u03c2\", \"\\u0628.\\ud83d\\ude00\", \"\\u00ad.\\u0300.xn--\", \"\\u05ea\", \"\\u0130.\\ud83d\\ude00\", \"\\u05d0.\\u0628\", \"a.a\", \"\\u0628.z.z\", \"a.-\", \"\\u0644.\\u0130\", \"\\u0628.xn--\", \"-.z.\\u0663\", \"\\u05ea\", \"xn--\", \"\\u05d0.z\", \"9\", \"xn--.xn--\", \"\\u00ad\", \"\\u0663.\\u0300\", \"\\u05ea.\\u200c.z\", \"xn--.z\", \"9.\\ud83d\\ude00\", \"\\u05d0.xn--.a\", \"\\u0300.z.\\u094d\", \"\\u03c2\", \"\\u0628.\\u0130.xn--\", \"\\u0663\", \"\\u094d\", \"\\u0663.z\", \"z\", \"\\u0644\", \"a.\\u200c.z\", \"\\u2168.\\u03c2\", \"\\u0628\", \"\\u03c2.xn--.\\u0644\", \"z.-.\\u094d\", \"\\u05d0\", \"\\u200c.\\u00e9\", \"\\u0628\", \"z.\\u0300.\\u0644\", \"\\u0300\", \"\\u05ea\", \"\\u00ad.\\u2168.a\", \"\\u200c.-\", \"\\u0130.\\ud83d\\ude00.\\u2168\", \"z\", \"\\u094d.\\u00e9\", \"\\u0644\", \"\\u05d0.\\u0300.\\u05ea\", \"\\u094d\", \"\\u0130\", \"\\u05d0.\\u00ad.\\u0644\", \"\\u0130\", \"\\u0300.\\u0130\", \"9.\\u200c.\\ud83d\\ude00\", \"\\u0300\", \"\\u05ea.\\u0644.xn--\", \"\\u0130.-\", \"\\u0628\", \"\\u0300.\\u0663.\\u200c\", \"-.a\", \"\\u05d0\", \"\\u03c2\", \"\\u0300.\\u0644\", \"\\u0628\", \"\\u00ad.\\u03c2.\\u05d0\", \"\\u05d0.xn--\", \"\\u05ea.\\u05ea\", \"\\u200c\", \"\\u0644\", \"\\u0628\", \"\\u094d.\\u2168.\\u0663\", \"\\u0644\", \"\\u05d0.a\", \"\\u0628.\\u03c2.\\u00e9\", \"\\u00e9.-\", \"\\u00ad.9.\\u2168\", \"-.a.\\u0644\", \"a.xn--\", \"9.\\u0130.\\u00ad\", \"\\u03c2.z.\\u05ea\", \"9\", \"xn--.\\u0663.\\ud83d\\ude00\", \"xn--.9\", \"\\u0628.-.a\", \"\\u200c\", \"a\", \"\\u094d\", \"\\u0300\", \"\\u0628.z\", \"xn--\", \"9\", \"\\u2168\", \"a.-\", \"xn--.\\u05d0.\\u0663\", \"\\ud83d\\ude00.\\u0663\", \"\\u094d.\\u00e9\", \"\\u0644\", \"\\u200c.z.\\ud83d\\ude00\", \"\\u200c.\\u05d0\", \"9.a\", \"\\u0300\\u0663.\\u094d\", \"\\u0663.\\u094d\", \"\\u0663.xn--.z\", \"a\", \"-\", \"\\u0300.\\u05ea.-\", \"\\u00e9.\\u0130.z\", \"\\u0300\", \"\\u00e9.\\u0663\", \"\\u0663.\\u0663\", \"\\u0130.-\", \"a.\\u00e9\", \"\\u03c2.\\u0130.\\u0628\", \"\\u03c2.\\u03c2\", \"9\", \"\\u00ad.a.\\u094d\", \"\\u0300\", \"9\", \"\\u0300\", \"\\u0130.\\ud83d\\ude00\", \"\\u0300\", \"\\u2168.xn--\", \"\\u0644\", \"\\u2168.\\u00e9\", \"\\ud83d\\ude00.\\u0644.-\", \"\\u0300\", \"\\u094d.\\u0628\", \"\\u0663.\\u0644.\\u00ad\", \"\\u200c.\\u0300.-\", \"\\u05d0.\\u0130.\\u0628\", \"\\u03c2.\\u00ad.\\u05d0\", \"\\u0300.a.\\u0663\", \"\\u03c2.-\", \"a\", \"\\u00e9.\\ud83d\\ude00\", \"\\u00ad.\\u200c\", \"9.\\u05ea\", \"xn--\", \"9\", \"\\u03c2.a\", \"\\u05d0.\\ud83d\\ude00.xn--\", \"\\u0644\", \"\\u03c2.z\", \"\\u05d0.\\u0300.\\u0644\", \"z\", \"\\u05d0.\\u0130\", \"a.a\", \"\\u05d0.\\u0130\", \"\\u0130\", \"z.\\u03c2.\\u05d0\", \"xn--\", \"\\u0663\", \"9.\\u05d0\", \"9.xn--.\\u0628\", \"\\u0663\", \"\\u00ad\", \"a\", \"xn--.\\ud83d\\ude00\", \"\\u00e9.z.\\u03c2\", \"\\u0663\\u2168\", \"\\u00ad\", \"\\u00e9.\\u0644\", \"\\u0644\", \"\\u0628.\\u05d0\", \"\\u0130\", \"\\u2168.\\u03c2\", \"xn--.9.9\", \"\\u03c2.a\", \"a.\\u05ea.z\", \"z.\\ud83d\\ude00.a\", \"xn--\", \"xn--a\", \"xn--.\\u00e9\", \"xn--a.\\u00e9\", \"xn--bcher-kva.\\u00e9\", \"xn--BCHER-KVA.\\u00e9\", \"XN--bcher-kva.\\u00e9\", \"xn--pokxncvks.\\u00e9\", \"xn--n3h.\\u00e9\", \"xn--4dbb.com\", \"xn--4dbb.1com\", \"1.xn--4dbb\", \"\\u05d0.1\", \"a.\\u05d0\", \"1a.\\u05d0\", \"a1.\\u05d0\", \"\\u05d01\", \"\\u05d0a\", \"xn--mgbh0fb.xn--kgbechtv\", \"\\u00e9.xn--a-ecp.ru\", \"xn--zca.\\u00e9\", \"xn--ss-.\\u00e9\"];
const lines = [];
for (const name of names) {
  let host;
  try { host = new URL(\"https://\" + name + \"/x\").hostname; } catch (e) { host = \"FAIL\"; }
  lines.push(JSON.stringify(name) + \" \" + JSON.stringify(url.domainToASCII(name)) + \" \" + JSON.stringify(url.domainToUnicode(name)) + \" \" + host);
}
const groups = new Map();
for (const line of lines) {
  const kind = line.endsWith(\" FAIL\") ? \"refused\" : /^\"[\\x20-\\x7e]*\" /.test(line) ? \"ascii input\" : \"other input\";
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
"

// domain names through domainToASCII, domainToUnicode and a url's hostname:
// the standard's name tests thinned to a fifth, and generated names that mix
// right-to-left letters, digits, marks and joiners. Checked the same way
pub fn international_names_match_node_test() {
  let rt = run_commonjs(node_runtime(), url_names_script)
  assert out_lines(rt)
    == [
      "ascii input 303 3137033169",
      "other input 377 2765661661",
      "refused 467 2015242308",
    ]
}

const url_api_script = "
const url = require(\"url\"), qs = require(\"querystring\");
let seed = 31337;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (list) => list[rnd(list.length)];
const lines = [];
const show = (v) => (v === undefined ? \"undefined\" : typeof v === \"string\" ? JSON.stringify(v) : v instanceof Uint8Array ? \"<\" + Buffer.from(v).toString(\"hex\") + \">\" : JSON.stringify(v));
const call = (f) => { try { return show(f()); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message + (e.input !== undefined ? \" input=\" + JSON.stringify(e.input) : \"\"); } };
const t = (label, f) => lines.push(label + \" \" + call(f));
// ---- URLSearchParams
const words = [\"a\", \"b\", \"c d\", \"e&f\", \"g=h\", \"\\u00e9\", \"\\ud83d\\ude00\", \"\", \"+\", \"%\", \"%41\", \"x y+z\", \"a/b?c#d\", \"~!*'()\", \"\\u4e2d\"];
for (let i = 0; i < 300; i++) {
  const init = pick([
    () => Array.from({ length: rnd(5) }, () => pick(words) + \"=\" + pick(words)).join(\"&\"),
    () => \"?\" + pick(words) + \"=\" + pick(words) + \"&\" + pick(words),
    () => Array.from({ length: rnd(4) }, () => [pick(words), pick(words)]),
    () => Object.fromEntries(Array.from({ length: rnd(4) }, () => [pick(words), pick(words)])),
    () => new Map([[pick(words), pick(words)]]),
    () => \"a=1&a=2&b=3&&=x&c\",
  ])();
  const p = new URLSearchParams(init);
  const ops = [];
  for (let k = 0; k < 4; k++) {
    const key = pick(words), value = pick(words);
    switch (rnd(8)) {
      case 0: p.append(key, value); ops.push(\"append\"); break;
      case 1: p.delete(key); ops.push(\"delete\"); break;
      case 2: p.set(key, value); ops.push(\"set\"); break;
      case 3: ops.push(\"get=\" + show(p.get(key))); break;
      case 4: ops.push(\"getAll=\" + show(p.getAll(key))); break;
      case 5: ops.push(\"has=\" + p.has(key) + p.has(key, value)); break;
      case 6: p.sort(); ops.push(\"sort\"); break;
      default: p.delete(key, value); ops.push(\"delete2\");
    }
  }
  lines.push(\"Q \" + show(typeof init === \"string\" ? init : init instanceof Map ? [...init] : init) + \" \" + ops.join(\",\") + \" => \" + p.toString() + \" | \" + p.size + \" | \" + show([...p]) + \" | \" + show([...p.keys()]) + show([...p.values()]));
}
t(\"params forEach\", () => { const seen = []; new URLSearchParams(\"a=1&b=2\").forEach(function (v, k, o) { seen.push(k + v + (o instanceof URLSearchParams) + (this === undefined)); }); return seen; });
t(\"params iterator\", () => { const it = new URLSearchParams(\"a=1\").entries(); return [Object.prototype.toString.call(it), show(it.next()), show(it.next()), typeof it[Symbol.iterator]]; });
t(\"params tag\", () => [Object.prototype.toString.call(new URLSearchParams()), URLSearchParams.name, URLSearchParams.length, Object.getOwnPropertyNames(URLSearchParams.prototype).join(\" \")]);
t(\"params bad init\", () => new URLSearchParams([[\"a\"]]));
t(\"params bad init 2\", () => new URLSearchParams([5]));
t(\"params not iterable\", () => new URLSearchParams({ [Symbol.iterator]: 5 }));
t(\"params append args\", () => new URLSearchParams().append(\"a\"));
t(\"params wrong this\", () => URLSearchParams.prototype.get.call({}, \"a\"));
t(\"params forEach bad\", () => new URLSearchParams().forEach(5));
t(\"params from params\", () => new URLSearchParams(new URLSearchParams(\"x=1&y=2\")).toString());
t(\"params number init\", () => new URLSearchParams(5).toString());
t(\"params null init\", () => new URLSearchParams(null).toString() + \"|\" + new URLSearchParams(undefined).toString());
// ---- URL and its searchParams together
t(\"url params link\", () => { const u = new URL(\"https://x.test/p?a=1#h\"); u.searchParams.append(\"b\", \"2 3\"); const first = u.href; u.search = \"?z=9\"; const second = [...u.searchParams].join(); u.searchParams.delete(\"z\"); return [first, second, u.href, u.search]; });
t(\"url props\", () => [Object.getOwnPropertyNames(URL.prototype).join(\" \"), URL.name, URL.length, Object.prototype.toString.call(new URL(\"a:b\")), Object.keys(new URL(\"a:b\")).length]);
t(\"url enumerable\", () => { const keys = []; for (const k in new URL(\"a:b\")) keys.push(k); return keys; });
t(\"url no args\", () => new URL());
t(\"url bad\", () => new URL(\"nope\"));
t(\"url bad base\", () => new URL(\"x\", \"also nope\"));
t(\"url coerces\", () => [new URL({ toString() { return \"https://a.test/\"; } }).href, new URL(\"p\", new URL(\"https://b.test/q/r\")).href, new URL(5, \"https://c.test\").href]);
t(\"url href setter bad\", () => { const u = new URL(\"https://a.test\"); u.href = \"nope\"; });
t(\"url canParse\", () => [URL.canParse(\"https://a\"), URL.canParse(\"nope\"), URL.canParse(\"x\", \"https://a\"), URL.canParse(\"x\", \"nope\"), URL.canParse(undefined, \"https://a/\")]);
t(\"url canParse no args\", () => URL.canParse());
t(\"url parse static\", () => [URL.parse(\"https://a/b\").href, URL.parse(\"nope\"), URL.parse(\"c\", \"https://a/b\").href]);
t(\"origins\", () => [\"https://a.test:8443/x\", \"http://u:p@h/\", \"ws://h:80\", \"wss://h:1\", \"ftp://h/\", \"file:///x\", \"blob:https://a.test/uuid\", \"blob:nope\", \"data:text/plain,x\", \"sc://h/\", \"mailto:a@b\", \"blob:file:///x\"].map((s) => new URL(s).origin));
// ---- the url module
t(\"module keys\", () => Object.keys(url).filter((k) => k !== \"URLPattern\").join(\" \"));
t(\"same classes\", () => url.URL === URL && url.URLSearchParams === URLSearchParams && require(\"node:url\") === url);
const paths = [\"/a b/c\", \"/tmp/x.txt\", \"/a%b\", \"/a#b?c\", \"/\\u00e9/\\u4e2d\", \"/a\\\\b\", \"/trail/\", \"rel/x\", \".\", \"/a\\tb\\nc\", \"/[x]^|~\\\"\", \"/\", \"//double\", \"\"];
for (const p of paths) {
  t(\"pathToFileURL \" + show(p), () => url.pathToFileURL(p).href);
  t(\"pathToFileURL win \" + show(p), () => url.pathToFileURL(p, { windows: true }).href.replace(/^file:\\/\\/\\/[A-Za-z]:.*(?=\\/[^/]*$)/, \"file:///<cwd>\"));
}
for (const p of [\"C:\\\\a b\\\\c.txt\", \"C:/x/y\", \"\\\\\\\\server\\\\share\\\\f.txt\", \"\\\\\\\\b\\u00fccher\\\\share\\\\f\", \"\\\\\\\\?\\\\UNC\\\\srv\\\\sh\\\\f\", \"\\\\\\\\server\", \"\\\\\\\\\\\\share\", \"D:\\\\\", \"c:\\\\a#b?c%d\"]) t(\"pathToFileURL win \" + show(p), () => url.pathToFileURL(p, { windows: true }).href);
t(\"pathToFileURL bad\", () => url.pathToFileURL(5));
const fileUrls = [\"file:///a%20b/c\", \"file:///x/%E9\", \"file:///C:/a/b\", \"file://server/share/f\", \"file://localhost/x\", \"file:///a%2Fb\", \"file:///a%5Cb\", \"file:///%E0%A4%A\", \"http://x/y\", \"file:///\", \"file:///a/b/\", \"file:///C:\", \"file:///c%3A/x\", \"file://xn--bcher-kva/s/f\"];
for (const f of fileUrls) {
  t(\"fileURLToPath \" + f, () => url.fileURLToPath(f));
  t(\"fileURLToPath win \" + f, () => url.fileURLToPath(f, { windows: true }));
  t(\"fileURLToPath URL \" + f, () => url.fileURLToPath(new URL(f)));
  t(\"fileURLToPathBuffer \" + f, () => url.fileURLToPathBuffer(f));
}
t(\"fileURLToPath type\", () => url.fileURLToPath(5));
t(\"fileURLToPath null\", () => url.fileURLToPath(null));
t(\"urlToHttpOptions\", () => [url.urlToHttpOptions(new URL(\"https://u%20x:p%40@[::1]:8443/a/b?c=d#e\")), url.urlToHttpOptions(new URL(\"http://h/\"))]);
t(\"urlToHttpOptions bad\", () => url.urlToHttpOptions(5));
const domains = [\"example.com\", \"EXAMPLE.com\", \"b\\u00fccher.de\", \"\\u65e5\\u672c\\u8a9e.jp\", \"xn--bcher-kva.de\", \"a b\", \"\", \"1.2.3.4\", \"0x7f.1\", \"a..b\", \"[::1]\", \"x:y\", \"fa\\u00df.de\", \"\\u2603.net\", \"xn--n3h.net\"];
for (const d of domains) t(\"domain \" + show(d), () => [url.domainToASCII(d), url.domainToUnicode(d)]);
t(\"domainToASCII no args\", () => url.domainToASCII());
// ---- legacy url.parse, format, resolve
const legacy = [\"http://user:pass@host.com:8080/p/a/t/h?query=string#hash\", \"//host/path\", \"/just/path?q=1\", \"mailto:a@b.c\", \"javascript:alert(1)\", \"http://[::1]:3000/x\", \"HTTP://EXAMPLE.com/A B\", \"http://a.com/b c?d e#f g\", \"x://y\", \"file:///etc/passwd\", \"http://h?q\", \"http://h#f\", \"?only=query\", \"#frag\", \"http://h/a;b\", \"http://ex\\u00e4mple.com/\", \"http://a@b@c/\", \"http://x.com/a\\\\b\", \"git+ssh://git@host:repo.git\", \"http://host:port/\", \"\", \" http://sp.ace/ \", \"http://h/%zz\", \"//a:b@c:1/d\", \"s3://bucket/key\", \"a:b:c\", \"http://-/\", \"http://h:/p\"];
for (const u of legacy) {
  t(\"parse \" + show(u), () => url.parse(u));
  t(\"parse qs \" + show(u), () => { const p = url.parse(u, true); return [p.query, p.search, p.path]; });
  t(\"parse slashes \" + show(u), () => { const p = url.parse(u, false, true); return [p.host, p.pathname, p.slashes]; });
  t(\"format \" + show(u), () => url.format(url.parse(u)));
}
for (let i = 0; i < 200; i++) { const a = pick(legacy), b = pick([\"../x\", \"./y\", \"/z\", \"?q\", \"#f\", \"//other/h\", \"http://new/\", \"a/b/../c\", \"\", \"..\", \"g;p\", \"x y\", ...legacy]); t(\"resolve \" + show(a) + \" + \" + show(b), () => [url.resolve(a, b), url.resolveObject(a, b).href]); }
t(\"format object\", () => [url.format({ protocol: \"https\", hostname: \"h\", pathname: \"/p\", query: { a: 1, b: [\"x\", \"y\"] } }), url.format({ host: \"h:1\", search: \"s\", hash: \"f\" }), url.format({ protocol: \"mailto\", auth: \"u\", hostname: \"h\" }), url.format({ slashes: true, pathname: \"x\" }), url.format(\"http://a/b\")]);
t(\"format URL\", () => { const u = new URL(\"https://u:p@b\\u00fccher.de:8/p?q#f\"); return [url.format(u), url.format(u, { fragment: false }), url.format(u, { unicode: true }), url.format(u, { search: false, auth: false })]; });
t(\"format bad\", () => url.format(5));
t(\"parse bad\", () => url.parse(5));
t(\"parse bad host\", () => url.parse(\"http://a b/\"));
t(\"Url class\", () => [typeof url.Url, new url.Url().href, url.parse(url.parse(\"http://a/\")) instanceof url.Url]);
// ---- querystring
const qsInputs = [\"a=1&b=2\", \"a=1&a=2&a=3\", \"a%20b=c+d\", \"a=%E9%zz\", \"=x&y=\", \"a&b&&\", \"\\u00e9=\\ud83d\\ude00\", \"a=1;b=2\", \"a:1,b:2\", \"\", \"a=%41%42\", \"x=\" + \"y\".repeat(40), \"a[]=1&a[]=2\", \"%=%\", \"+=+\"];
for (const s of qsInputs) {
  t(\"qs parse \" + show(s), () => qs.parse(s));
  t(\"qs parse opts \" + show(s), () => [qs.parse(s, \";\", \":\"), qs.parse(s, \"&\", \"=\", { maxKeys: 2 }), qs.parse(s, null, null, { decodeURIComponent: (x) => x.toUpperCase() })]);
  t(\"qs escape \" + show(s), () => [qs.escape(s), qs.unescape(s), qs.unescapeBuffer(s), qs.unescapeBuffer(s, true)]);
}
for (let i = 0; i < 150; i++) {
  const obj = {};
  for (let k = 0; k < rnd(4); k++) obj[pick(words)] = pick([pick(words), [pick(words), pick(words)], rnd(100), true, null, undefined, 1.5, { x: 1 }, 10n, NaN, Infinity, \"\"]);
  t(\"qs stringify \" + JSON.stringify(obj, (k, v) => (typeof v === \"bigint\" ? v + \"n\" : v === undefined ? \"undef\" : v)), () => [qs.stringify(obj), qs.stringify(obj, \";\", \":\"), qs.encode(obj, null, null, { encodeURIComponent: (x) => \"<\" + x + \">\" })]);
}
t(\"qs keys\", () => Object.keys(qs).join(\" \"));
t(\"qs stringify odd\", () => [qs.stringify(\"x\"), qs.stringify(null), qs.stringify(5), qs.stringify([1, 2])]);
t(\"qs escape odd\", () => [qs.escape(5), qs.escape({ toString() { return \"a b\"; } }), qs.escape(null)]);
const groups = new Map();
for (const line of lines) {
  const kind = line.split(\" \")[0] + (line.startsWith(\"qs \") || line.startsWith(\"parse \") || line.startsWith(\"params \") || line.startsWith(\"url \") ? \" \" + line.split(\" \")[1] : \"\");
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line.replace(/ on (darwin|linux)/, \" on <os>\"));
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
"

// URLSearchParams, the url module, the old url.parse family and querystring,
// checked the same way
pub fn url_module_and_querystring_match_node_test() {
  let rt = run_commonjs(node_runtime(), url_api_script)
  assert out_lines(rt)
    == [
      "Q 300 2008793640",
      "params forEach 2 1214470277",
      "params iterator 1 1733063445",
      "params tag 1 1479588791",
      "params bad 2 2199802237",
      "params not 1 799656800",
      "params append 1 4228986052",
      "params wrong 1 2169413622",
      "params from 1 2197321353",
      "params number 1 2464461588",
      "params null 1 3196933358",
      "url params 1 2187457840",
      "url props 1 311779117",
      "url enumerable 1 2597820275",
      "url no 1 3166588325",
      "url bad 2 1144437536",
      "url coerces 1 1241795517",
      "url href 1 1946628969",
      "url canParse 2 4005990670",
      "url parse 1 3910882209",
      "origins 1 3990590039",
      "module 1 2046585460",
      "same 1 1033274121",
      "pathToFileURL 38 38046963",
      "fileURLToPath 44 1266820923",
      "fileURLToPathBuffer 14 2382917223",
      "urlToHttpOptions 2 123282486",
      "domain 15 608329221",
      "domainToASCII 1 2260364061",
      "parse \"http://user:pass@host.com:8080/p/a/t/h?query=string#hash\" 1 419438246",
      "parse qs 28 2008773071",
      "parse slashes 28 3034398335",
      "format 31 4069287580",
      "parse \"//host/path\" 1 538115182",
      "parse \"/just/path?q=1\" 1 123283059",
      "parse \"mailto:a@b.c\" 1 1020762637",
      "parse \"javascript:alert(1)\" 1 631325408",
      "parse \"http://[::1]:3000/x\" 1 3543695593",
      "parse \"HTTP://EXAMPLE.com/A 1 3536659207",
      "parse \"http://a.com/b 1 2926237689",
      "parse \"x://y\" 1 1308794204",
      "parse \"file:///etc/passwd\" 1 2687576824",
      "parse \"http://h?q\" 1 1345987436",
      "parse \"http://h#f\" 1 2247585457",
      "parse \"?only=query\" 1 3760414352",
      "parse \"#frag\" 1 1704240328",
      "parse \"http://h/a;b\" 1 3431738162",
      "parse \"http://exämple.com/\" 1 3232555431",
      "parse \"http://a@b@c/\" 1 119357255",
      "parse \"http://x.com/a\\\\b\" 1 1115819561",
      "parse \"git+ssh://git@host:repo.git\" 1 591738982",
      "parse \"http://host:port/\" 1 1898293486",
      "parse \"\" 1 3704299422",
      "parse \" 1 545909050",
      "parse \"http://h/%zz\" 1 2411906910",
      "parse \"//a:b@c:1/d\" 1 2146804176",
      "parse \"s3://bucket/key\" 1 2332902768",
      "parse \"a:b:c\" 1 2288421166",
      "parse \"http://-/\" 1 1696180026",
      "parse \"http://h:/p\" 1 2257936992",
      "resolve 200 3234791744",
      "parse bad 2 2126090409",
      "Url 1 1996761923",
      "qs parse 30 614735341",
      "qs escape 16 1695966203",
      "qs stringify 151 2070330065",
      "qs keys 1 2575257046",
    ]
}

pub fn url_globals_exist_only_when_asked_for_test() {
  assert run(
      start_on(engine.new(), []),
      "[typeof URL, typeof URLSearchParams].join()",
    ).0
    == "'undefined,undefined'"
}

const events_script = "
const events = require(\"events\");
const { EventEmitter } = events;
let seed = 5150;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const show = (v) => { try { return typeof v === \"function\" ? \"fn:\" + (v.tag ?? v.name) : typeof v === \"symbol\" ? v.toString() : JSON.stringify(v, (k, x) => (typeof x === \"function\" ? \"fn:\" + (x.tag ?? x.name) : typeof x === \"symbol\" ? x.toString() : typeof x === \"bigint\" ? x + \"n\" : x)); } catch (e) { return String(v); } };
const call = (f) => { try { const v = f(); return v === undefined ? \"undefined\" : show(v); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f));
// ---- generated sequences of listener operations
const names = [\"a\", \"b\", Symbol.for(\"s\"), \"error\", \"newListener\", \"removeListener\"];
for (let round = 0; round < 250; round++) {
  const e = new EventEmitter();
  const log = [];
  const fns = Array.from({ length: 4 }, (_, i) => { const f = function (...args) { log.push(\"f\" + i + \"(\" + args.map((a) => (typeof a === \"function\" ? \"fn:\" + a.tag : String(a))).join(\",\") + \")\"); if (i === 3 && args[0] === \"again\") e.removeListener(\"a\", fns[0]); }; f.tag = \"f\" + i; return f; });
  const steps = [];
  for (let k = 0; k < 3 + rnd(9); k++) {
    const name = pick(names), fn = pick(fns);
    const op = rnd(13);
    const r = call(() => {
      switch (op) {
        case 0: e.on(name, fn); return \"on\";
        case 1: e.once(name, fn); return \"once\";
        case 2: e.prependListener(name, fn); return \"prepend\";
        case 3: e.prependOnceListener(name, fn); return \"prependOnce\";
        case 4: e.off(name, fn); return \"off\";
        case 5: e.removeAllListeners(rnd(3) ? name : undefined); return \"removeAll\";
        case 6: return \"emit=\" + e.emit(name, pick([\"again\", 1, undefined]), 2);
        case 7: return \"count=\" + e.listenerCount(name) + \"/\" + e.listenerCount(name, fn);
        case 8: return \"listeners=\" + show(e.listeners(name));
        case 9: return \"raw=\" + show(e.rawListeners(name).map((l) => l.listener ? \"wrap:\" + l.listener.tag : l.tag));
        case 10: return \"names=\" + show(e.eventNames());
        case 11: e.setMaxListeners(rnd(4)); return \"max=\" + e.getMaxListeners();
        default: return \"emit0=\" + e.emit(name);
      }
    });
    steps.push((typeof name === \"symbol\" ? \"sym\" : name) + \":\" + r);
  }
  lines.push(\"G \" + round + \" \" + steps.join(\" \") + \" => \" + log.join(\" \") + \" | \" + show(e.eventNames()) + \" \" + e._eventsCount);
}
// ---- api
t(\"shape\", () => [typeof events, events === EventEmitter, events.EventEmitter === EventEmitter, events.defaultMaxListeners, events.usingDomains, String(events.errorMonitor), String(events.captureRejectionSymbol), events.captureRejections, EventEmitter.name, EventEmitter.length]);
t(\"static keys\", () => Object.getOwnPropertyNames(events).filter((k) => ![\"length\", \"name\", \"prototype\", \"arguments\", \"caller\"].includes(k)).sort());
t(\"proto keys\", () => Object.getOwnPropertyNames(EventEmitter.prototype).sort());
t(\"instance\", () => { const e = new EventEmitter(); return [Object.keys(e), e._eventsCount, e._maxListeners === undefined, Object.getPrototypeOf(e._events)]; });
t(\"init without new\", () => { const o = {}; EventEmitter.call(o); return Object.keys(o); });
t(\"subclass\", () => { class A extends EventEmitter { constructor() { super({ captureRejections: true }); } } const a = new A(); let got; a.on(\"x\", (v) => (got = v)); a.emit(\"x\", 7); return [got, a instanceof EventEmitter]; });
t(\"util.inherits style\", () => { function Old() { EventEmitter.call(this); } Object.setPrototypeOf(Old.prototype, EventEmitter.prototype); const o = new Old(); let n = 0; o.on(\"x\", () => n++); o.emit(\"x\"); return n; });
t(\"this in listener\", () => { const e = new EventEmitter(); let same; e.on(\"x\", function () { same = this === e; }); e.emit(\"x\"); return same; });
t(\"returns this\", () => { const e = new EventEmitter(); const f = () => {}; return [e.on(\"x\", f) === e, e.once(\"x\", f) === e, e.off(\"x\", f) === e, e.removeAllListeners() === e, e.setMaxListeners(1) === e, e.addListener(\"y\", f) === e]; });
t(\"bad listener\", () => new EventEmitter().on(\"x\", 5));
t(\"bad listener once\", () => new EventEmitter().once(\"x\", \"f\"));
t(\"bad listener off\", () => new EventEmitter().off(\"x\", null));
t(\"bad max\", () => new EventEmitter().setMaxListeners(-1));
t(\"bad max type\", () => new EventEmitter().setMaxListeners(\"3\"));
t(\"bad default max\", () => { events.defaultMaxListeners = -1; });
t(\"bad captureRejections\", () => new EventEmitter({ captureRejections: \"yes\" }));
t(\"bad captureRejections static\", () => { events.captureRejections = 1; });
t(\"error with no listener\", () => new EventEmitter().emit(\"error\", new Error(\"boom\")));
t(\"error not an error\", () => new EventEmitter().emit(\"error\", { a: 1 }));
t(\"error string\", () => new EventEmitter().emit(\"error\", \"bad\"));
t(\"error undefined\", () => new EventEmitter().emit(\"error\"));
t(\"error context\", () => { try { new EventEmitter().emit(\"error\", \"bad\"); } catch (e) { return [e.context, Object.keys(e)]; } });
t(\"errorMonitor\", () => { const e = new EventEmitter(); const seen = []; e.on(events.errorMonitor, (err) => seen.push(\"monitor \" + err.message)); e.on(\"error\", (err) => seen.push(\"handler \" + err.message)); e.emit(\"error\", new Error(\"x\")); return seen; });
t(\"errorMonitor alone still throws\", () => { const e = new EventEmitter(); e.on(events.errorMonitor, () => {}); e.emit(\"error\", new Error(\"y\")); });
t(\"newListener order\", () => { const e = new EventEmitter(); const seen = []; e.on(\"newListener\", (name, fn) => seen.push(\"new \" + String(name) + \" \" + e.listenerCount(name))); e.on(\"removeListener\", (name) => seen.push(\"removed \" + name)); const f = () => {}; e.on(\"x\", f); e.once(\"y\", f); e.off(\"x\", f); e.emit(\"y\"); e.removeAllListeners(\"removeListener\"); return seen; });
t(\"once wrapper\", () => { const e = new EventEmitter(); const f = () => {}; e.once(\"x\", f); return [e.listeners(\"x\")[0] === f, e.rawListeners(\"x\")[0] === f, e.rawListeners(\"x\")[0].listener === f]; });
t(\"remove during emit\", () => { const e = new EventEmitter(); const seen = []; const b = () => seen.push(\"b\"); e.on(\"x\", () => { seen.push(\"a\"); e.off(\"x\", b); }); e.on(\"x\", b); e.emit(\"x\"); e.emit(\"x\"); return seen; });
t(\"max listeners\", () => { const e = new EventEmitter(); return [e.getMaxListeners(), events.getMaxListeners(e), (events.setMaxListeners(3, e), e.getMaxListeners()), (e.setMaxListeners(0), e.getMaxListeners()), e.setMaxListeners(Infinity).getMaxListeners()]; });
t(\"leak warning flag\", () => { const e = new EventEmitter(); e.setMaxListeners(1); e.on(\"x\", () => {}); e.on(\"x\", () => {}); return [e._events.x.warned, e.listenerCount(\"x\")]; });
t(\"getEventListeners\", () => { const e = new EventEmitter(); const f = () => {}; e.on(\"x\", f); e.once(\"x\", f); return [events.getEventListeners(e, \"x\").length, events.getEventListeners(e, \"y\"), events.listenerCount(e, \"x\")]; });
t(\"getEventListeners bad\", () => events.getEventListeners({}, \"x\"));
t(\"getMaxListeners bad\", () => events.getMaxListeners({}));
t(\"setMaxListeners static bad target\", () => events.setMaxListeners(1, {}));
t(\"setMaxListeners static default\", () => { const old = events.defaultMaxListeners; events.setMaxListeners(4); const now = [events.defaultMaxListeners, new EventEmitter().getMaxListeners()]; events.setMaxListeners(old); return now; });
t(\"listenerCount static deprecated form\", () => { const e = new EventEmitter(); e.on(\"x\", () => {}); return EventEmitter.listenerCount(e, \"x\"); });
t(\"symbol events\", () => { const e = new EventEmitter(); const s = Symbol(\"k\"); let n = 0; e.on(s, () => n++); e.emit(s); return [n, e.eventNames().length, typeof e.eventNames()[0]]; });
t(\"many args\", () => { const e = new EventEmitter(); let got; e.on(\"x\", (...a) => (got = a)); e.emit(\"x\", 1, 2, 3, 4, 5, 6); return got; });
t(\"emit returns\", () => { const e = new EventEmitter(); return [e.emit(\"x\"), (e.on(\"x\", () => {}), e.emit(\"x\"))]; });
t(\"off unknown\", () => { const e = new EventEmitter(); e.off(\"x\", () => {}); e.removeAllListeners(\"y\"); return e.eventNames(); });
t(\"listener throws\", () => { const e = new EventEmitter(); const seen = []; e.on(\"x\", () => { throw new RangeError(\"inside\"); }); e.on(\"x\", () => seen.push(\"second\")); try { e.emit(\"x\"); } catch (err) { seen.push(err.name); } return seen; });
// ---- event targets and abort signals, when present
t(\"with EventTarget\", () => { const target = new EventTarget(); const f = () => {}; target.addEventListener(\"x\", f); target.addEventListener(\"x\", () => {}, { once: true }); events.setMaxListeners(5, target); return [events.getEventListeners(target, \"x\").length, events.getEventListeners(target, \"x\")[0] === f, events.listenerCount(target, \"x\"), events.getMaxListeners(target), events.getEventListeners(target, \"none\")]; });
t(\"addAbortListener\", () => { const c = new AbortController(); const seen = []; const d = events.addAbortListener(c.signal, (ev) => seen.push(ev.type)); const d2 = events.addAbortListener(c.signal, () => seen.push(\"never\")); d2[Symbol.dispose](); c.abort(); return [seen, typeof d[Symbol.dispose], Object.getPrototypeOf(d)]; });
t(\"addAbortListener stopped\", () => { const c = new AbortController(); const seen = []; c.signal.addEventListener(\"abort\", (ev) => ev.stopImmediatePropagation()); events.addAbortListener(c.signal, () => seen.push(\"still told\")); c.abort(); return seen; });
t(\"addAbortListener bad\", () => events.addAbortListener(undefined, () => {}));
t(\"addAbortListener bad 2\", () => events.addAbortListener({}, () => {}));
t(\"addAbortListener bad 3\", () => events.addAbortListener(new AbortController().signal, 5));
// ---- the async helpers; results arrive later, printed in a fixed order
const later = [];
const settle = (label, promise) => later.push(promise.then((v) => label + \" resolved \" + show(v), (e) => label + \" rejected \" + e.name + \" \" + e.code + \" \" + e.message));
{ const e = new EventEmitter(); settle(\"once\", events.once(e, \"go\")); e.emit(\"go\", 1, \"two\"); }
{ const e = new EventEmitter(); settle(\"once error\", events.once(e, \"go\")); e.emit(\"error\", new Error(\"failed first\")); }
{ const e = new EventEmitter(); settle(\"once for error event\", events.once(e, \"error\")); e.emit(\"error\", new Error(\"wanted\")); }
{ const c = new AbortController(); const e = new EventEmitter(); settle(\"once aborted\", events.once(e, \"go\", { signal: c.signal })); c.abort(); settle(\"once aborted cleans up\", Promise.resolve().then(() => e.listenerCount(\"go\") + \",\" + e.listenerCount(\"error\"))); }
{ const e = new EventEmitter(); settle(\"once already aborted\", events.once(e, \"go\", { signal: AbortSignal.abort() })); }
{ settle(\"once bad signal\", (async () => events.once(new EventEmitter(), \"go\", { signal: 5 }))()); }
{ const target = new EventTarget(); settle(\"once on EventTarget\", events.once(target, \"ping\").then(([ev]) => ev.type)); target.dispatchEvent(new Event(\"ping\")); }
{ settle(\"once bad emitter\", (async () => events.once({}, \"x\"))()); }
{ const e = new EventEmitter(); settle(\"on iterates\", (async () => { const got = []; const it = events.on(e, \"tick\"); queueTicks(e); for await (const args of it) { got.push(args[0]); if (got.length === 3) break; } return [got, e.listenerCount(\"tick\"), e.listenerCount(\"error\")]; })()); }
function queueTicks(e) { Promise.resolve().then(() => { e.emit(\"tick\", 1); e.emit(\"tick\", 2); e.emit(\"tick\", 3); e.emit(\"tick\", 4); }); }
{ const e = new EventEmitter(); settle(\"on error\", (async () => { const it = events.on(e, \"tick\"); Promise.resolve().then(() => e.emit(\"error\", new Error(\"mid stream\"))); for await (const _ of it) { /* none */ } })()); }
{ const c = new AbortController(); const e = new EventEmitter(); settle(\"on aborted\", (async () => { const it = events.on(e, \"tick\", { signal: c.signal }); Promise.resolve().then(() => c.abort()); for await (const _ of it) { /* none */ } })()); }
{ const e = new EventEmitter(); settle(\"on close option\", (async () => { const got = []; const it = events.on(e, \"tick\", { close: [\"end\"] }); Promise.resolve().then(() => { e.emit(\"tick\", \"a\"); e.emit(\"end\"); }); for await (const args of it) got.push(args[0]); return got; })()); }
{ const e = new EventEmitter(); const it = events.on(e, \"tick\"); settle(\"on iterator shape\", (async () => [typeof it.next, typeof it.return, typeof it.throw, it[Symbol.asyncIterator]() === it, show(await it.return()), show(await it.next())])()); }
{ const e = new EventEmitter({ captureRejections: true }); settle(\"captureRejections\", new Promise((resolve) => { e.on(\"error\", (err) => resolve(\"routed \" + err.message)); e.on(\"x\", async () => { throw new Error(\"async boom\"); }); e.emit(\"x\"); })); }
{ const e = new EventEmitter({ captureRejections: true }); settle(\"captureRejections custom\", new Promise((resolve) => { e[events.captureRejectionSymbol] = (err, name, ...args) => resolve([err.message, name, args]); e.on(\"y\", async () => { throw new Error(\"custom\"); }); e.emit(\"y\", 1, 2); })); }
Promise.all(later).then((settled) => {
for (const l of settled) lines.push(l);
const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"G \") ? \"generated\" : line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
});

"

// 250 generated runs of listener operations, the events api, and its async
// helpers with abort signals and event targets. Each line is a kind of case,
// how many, and a hash of what they printed in node 26.11.1
pub fn events_match_node_test() {
  let rt = run_to_end(run_commonjs(node_runtime(), events_script))
  assert out_lines(rt)
    == [
      "generated 250 1053567025",
      "shape 1 682840665",
      "static 1 2073246721",
      "proto 1 4181908381",
      "instance 1 2264329778",
      "init 1 2011871542",
      "subclass 1 2881172964",
      "util.inherits 1 2571703791",
      "this 1 1251994592",
      "returns 1 2767923914",
      "bad 8 1433812664",
      "error 5 3107457037",
      "errorMonitor 2 1561212194",
      "newListener 1 436061221",
      "once 10 1270283493",
      "remove 1 802097553",
      "max 1 1957045456",
      "leak 1 3927728986",
      "getEventListeners 2 1200610165",
      "getMaxListeners 1 2314459384",
      "setMaxListeners 2 1370778731",
      "listenerCount 1 3400937000",
      "symbol 1 2238438402",
      "many 1 4167356386",
      "emit 1 1495309656",
      "off 1 1367172824",
      "listener 1 2928696599",
      "with 1 3671023169",
      "addAbortListener 5 3295607963",
      "on 5 747387987",
      "captureRejections 2 2905010035",
    ]
}

pub fn events_work_without_the_modules_they_can_use_test() {
  let rt =
    run_to_end(run_commonjs(
      start_on(engine.new(), [events.builtin()]),
      "const events = require('events');
      const e = new events.EventEmitter();
      e.setMaxListeners(1);
      e.on('x', () => out.push('one'));
      e.on('x', () => out.push('two'));
      e.emit('x');
      out.push(String(events.getMaxListeners(e)));
      try { events.getEventListeners({}, 'x'); } catch (error) { out.push(error.code); }
      try { events.EventEmitterAsyncResource; } catch (error) { out.push('no async_hooks'); }
      events.once(e, 'later').then((args) => out.push('later ' + args[0]));
      e.emit('later', 5);",
    ))
  assert out_lines(rt)
    == ["one", "two", "1", "ERR_INVALID_ARG_TYPE", "no async_hooks", "later 5"]
}

const string_decoder_script = "
const { StringDecoder } = require(\"string_decoder\");
let seed = 808;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const hexOf = (s) => Buffer.from(s, \"utf8\").toString(\"hex\");
const texts = [\"hello\", \"h\\u00e9llo w\\u00f6rld\", \"\\u20ac\\u20ac\", \"\\u4e2d\\u6587\\u5b57\", \"\\ud83d\\ude00\\ud83d\\ude80\", \"a\\ud83d\\ude00b\", \"\", \"x\"];
const encodings = [\"utf8\", \"utf-8\", \"utf16le\", \"ucs2\", \"base64\", \"base64url\", \"latin1\", \"binary\", \"hex\", \"ascii\", undefined, \"UTF8\"];
for (let i = 0; i < 500; i++) {
  const enc = pick(encodings);
  let bytes;
  if (rnd(3) === 0) bytes = Buffer.from(Array.from({ length: rnd(14) }, () => (rnd(3) ? 128 + rnd(128) : rnd(128))));
  else bytes = Buffer.from(pick(texts) + pick(texts), rnd(2) && (enc === \"utf16le\" || enc === \"ucs2\") ? \"utf16le\" : \"utf8\");
  const d = new StringDecoder(enc);
  const parts = [];
  let at = 0;
  while (at < bytes.length) {
    const n = 1 + rnd(4);
    const chunk = bytes.subarray(at, at + n);
    const piece = rnd(6) === 0 ? new Uint8Array(chunk) : chunk;
    parts.push(hexOf(d.write(piece)) + \"~\" + d.lastNeed + \"/\" + d.lastTotal);
    at += n;
  }
  const tail = rnd(4) === 0 ? d.end(Buffer.from([rnd(256)])) : d.end();
  lines.push(\"D \" + String(enc) + \" \" + bytes.toString(\"hex\") + \" \" + parts.join(\" \") + \" end=\" + hexOf(tail) + \" after=\" + d.lastNeed + \"/\" + d.lastTotal + \" again=\" + hexOf(d.write(Buffer.from(\"ok\"))));
}
const call = (f) => { try { const v = f(); return typeof v === \"string\" ? JSON.stringify(v) : JSON.stringify(v) ?? String(v); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f));
t(\"default encoding\", () => new StringDecoder().encoding);
t(\"normalised\", () => [\"UTF-8\", \"ucs-2\", \"UTF16LE\", \"binary\", \"BASE64\", \"base64url\", \"HEX\", \"ascii\", null, \"\"].map((e) => new StringDecoder(e).encoding));
t(\"bad encoding\", () => new StringDecoder(\"nope\"));
t(\"bad encoding type\", () => new StringDecoder(5));
t(\"write string passes through\", () => new StringDecoder().write(\"already text\"));
t(\"write bad\", () => new StringDecoder().write(5));
t(\"write bad 2\", () => new StringDecoder().write([1, 2]));
t(\"write DataView\", () => new StringDecoder().write(new DataView(new Uint8Array([104, 105]).buffer)));
t(\"write Uint16Array\", () => new StringDecoder(\"hex\").write(new Uint16Array([0x4142])));
t(\"wrong this\", () => StringDecoder.prototype.write.call({}, Buffer.from(\"a\")));
t(\"lastChar\", () => { const d = new StringDecoder(); d.write(Buffer.from([0xe2, 0x82])); return [Buffer.isBuffer(d.lastChar), d.lastChar.length, d.lastChar.toString(\"hex\").slice(0, 4), d.lastNeed, d.lastTotal]; });
t(\"text\", () => { const d = new StringDecoder(); d.write(Buffer.from([0xe2])); return [d.text(Buffer.from(\"xhello\"), 1), d.lastNeed]; });
t(\"end with input\", () => new StringDecoder(\"utf16le\").end(Buffer.from([0x61, 0x00, 0x62])));
t(\"shape\", () => [Object.keys(require(\"string_decoder\")), StringDecoder.name, StringDecoder.length, Object.getOwnPropertyNames(StringDecoder.prototype).sort(), Object.keys(new StringDecoder())]);
t(\"without new\", () => { try { return typeof StringDecoder(\"utf8\"); } catch (e) { return e.name; } });
const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"D \") ? \"decode \" + line.split(\" \")[1] : line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
"

// 500 generated inputs fed in chunks of one to four bytes, in every
// encoding, checking each piece and how much the decoder holds back
pub fn string_decoder_matches_node_test() {
  let rt = run_commonjs(node_runtime(), string_decoder_script)
  assert out_lines(rt)
    == [
      "decode utf-8 1 2277041550",
      "decode hex 161 151346911",
      "decode base64 162 3083645261",
      "decode utf8 176 3024526057",
      "default 1 4120655540",
      "normalised 1 2809180446",
      "bad 2 170096432",
      "write 5 4154319144",
      "wrong 1 1287805370",
      "lastChar 1 4115062536",
      "text 1 1940916560",
      "end 1 1899718399",
      "shape 1 3147696307",
      "without 1 1479083447",
    ]
}

const util_script = "
const util = require(\"util\");
let seed = 2112;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const call = (f) => { try { const v = f(); return typeof v === \"string\" ? v : JSON.stringify(v) ?? String(v); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f).replace(/\\n/g, \"\\\\n\"));
// ---- generated values through inspect with generated options
function leaf() {
  return pick([
    () => rnd(1000) - 500, () => 1.5, () => -0, () => NaN, () => Infinity, () => 123456789, () => 12345678901234567890n, () => \"str\", () => \"it's \\\"q\\\"\", () => \"line\\nbreak\", () => \"\", () => \"x\".repeat(30),
    () => true, () => null, () => undefined, () => Symbol(\"sym\"), () => Symbol.for(\"reg\"), () => Symbol.iterator, () => new Date(86400000 * rnd(20000)), () => new Date(NaN), () => /re+g/gi,
    () => function named() {}, () => () => {}, () => async function af() {}, () => function* gen() {}, () => Math.max, () => new Boolean(false), () => new Number(3), () => new String(\"boxed\"), () => Object(5n), () => Object(Symbol(\"b\")),
    () => new Uint8Array([1, 2, 3]), () => new Float64Array(2), () => new ArrayBuffer(4), () => new DataView(new ArrayBuffer(2)), () => Buffer.from(\"hi\"), () => new Uint16Array(0),
    () => Promise.resolve(7), () => Promise.reject(3).catch(() => {}) && new Promise(() => {}), () => new WeakMap(), () => new WeakSet(), () => new Map().entries(), () => new Set([1]).values(), () => [1, 2][Symbol.iterator](),
    () => Object.create(null), () => Object.create({ inherited: 1 }), () => new (class Point { constructor() { this.x = 1; } })(), () => new (class extends Map {})(), () => new (class Sub extends Array {})(2),
    () => (function () { return arguments; })(1, 2), () => new Proxy({ p: 1 }, {}), () => new Proxy(function f() {}, {}), () => globalThis.undefinedThing, () => [, , 1], () => new Array(5), () => Object.freeze({ fr: 1 }), () => Object.seal([1]),
    () => ({ get g() { return 1; }, set s(v) {}, get gs() { return 2; }, set gs(v) {} }), () => ({ [Symbol(\"k\")]: 1, 2: \"two\", \"needs quote\": 1, valid_id: 2 }), () => new URLSearchParams(\"a=1&b=2\"), () => new URL(\"https://u:p@h.test:8/p?q#f\"),
    () => { const e = new Error(\"flat\"); e.stack = \"Error: flat\\n    at fixed (file.js:1:1)\"; return e; }, () => { const e = new TypeError(\"with props\"); e.stack = \"TypeError: with props\\n    at x (y.js:2:3)\"; e.code = \"E_X\"; e.extra = [1]; return e; },
    () => { const e = new RangeError(\"cause\"); e.stack = \"RangeError: cause\"; e.cause = \"why\"; return e; }, () => new AggregateError([], \"agg\") && Object.assign(new AggregateError([1], \"agg\"), { stack: \"AggregateError: agg\" }),
    () => { const o = { name: \"circ\" }; o.self = o; o.list = [o, { o }]; return o; }, () => new AbortController().signal, () => new EventTarget(), () => new (require(\"events\"))(), () => new TextEncoder(), () => new TextDecoder(),
  ])();
}
function value(depth) {
  if (depth <= 0 || rnd(3) === 0) return leaf();
  switch (rnd(5)) {
    case 0: return Array.from({ length: rnd(8) }, () => value(depth - 1));
    case 1: { const o = {}; for (let i = 0; i < rnd(6); i++) o[pick([\"a\", \"b\", \"long_key_name\", \"k\" + i, \"x y\"])] = value(depth - 1); return o; }
    case 2: return new Map(Array.from({ length: rnd(4) }, () => [value(depth - 1), value(depth - 1)]));
    case 3: return new Set(Array.from({ length: rnd(5) }, () => value(depth - 1)));
    default: return Array.from({ length: 5 + rnd(120) }, (_, i) => (rnd(2) ? i * 7 : \"s\" + i));
  }
}
const optionSets = [undefined, { depth: 0 }, { depth: 4 }, { depth: null }, { compact: false }, { compact: 1 }, { compact: true, breakLength: Infinity }, { breakLength: 40 }, { sorted: true }, { showHidden: true }, { maxArrayLength: 3 }, { maxArrayLength: 0 }, { maxStringLength: 5 }, { numericSeparator: true }, { getters: true }, { getters: \"set\" }, { showProxy: true }, { colors: true }, { customInspect: false }, { depth: 1, compact: 3, breakLength: 60, sorted: true }];
for (let i = 0; i < 300; i++) {
  const v = value(3), o = pick(optionSets);
  lines.push(\"I \" + i + \" \" + JSON.stringify(o) + \" \" + call(() => util.inspect(v, o)).replace(/\\n/g, \"\\\\n\"));
}
for (let i = 0; i < 100; i++) { const a = value(2), b = rnd(3) ? a : value(2); lines.push(\"Q \" + i + \" \" + call(() => [util.isDeepStrictEqual(a, b), util.isDeepStrictEqual(a, a), util.isDeepStrictEqual(a, structuredCloneish(a))])); }
function structuredCloneish(v) { if (Array.isArray(v)) return v.map(structuredCloneish); if (v instanceof Map) return new Map([...v].map(([k, x]) => [k, structuredCloneish(x)])); if (v instanceof Set) return new Set(v); if (v && typeof v === \"object\" && Object.getPrototypeOf(v) === Object.prototype) { const o = {}; for (const k of Reflect.ownKeys(v)) { const d = Object.getOwnPropertyDescriptor(v, k); if (\"value\" in d) o[k] = k === \"self\" ? o : structuredCloneish(d.value); else Object.defineProperty(o, k, d); } return o; } return v; }
// ---- format
const fargs = [[\"%s %s\", \"a\"], [\"%s\", { a: 1 }], [\"%s\", 5n], [\"%s\", Symbol(\"x\")], [\"%s\", -0], [\"%s\", null], [\"%d\", \"42\"], [\"%d\", \"x\"], [\"%d\", 5n], [\"%i\", 3.9], [\"%f\", \"1.5x\"], [\"%j\", { a: [1] }], [\"%j\", 5n], [\"%o\", [1, { a: 2 }]], [\"%O\", { a: { b: { c: { d: 1 } } } }], [\"%c styled\", \"color: red\"], [\"%% %s\", 1], [\"%x\", 1], [\"no subs\", 1, \"two\", { three: 3 }], [5, 6], [{ a: 1 }, \"b\"], [\"%s:%s\", \"only\"], [\"%s\", new Date(0)], [\"%s\", new Error(\"e\").message], [\"%s\", [1, [2, [3]]]], [\"%s\", class K {}.name], [\"a\", Symbol(\"s\")], [\"%s %s %s\", null, undefined, NaN], [\"%d %d\", 1e21, -0]];
for (const a of fargs) t(\"format \" + JSON.stringify(a, (k, v) => (typeof v === \"bigint\" ? v + \"n\" : typeof v === \"symbol\" ? String(v) : typeof v === \"function\" ? \"fn\" : v)), () => util.format(...a));
t(\"formatWithOptions\", () => util.formatWithOptions({ compact: false, depth: 0 }, \"%o and\", { a: { b: 1 } }, { c: { d: 1 } }));
t(\"formatWithOptions bad\", () => util.formatWithOptions(5, \"x\"));
// ---- types
const samples = { date: new Date(), regexp: /x/, map: new Map(), set: new Set(), weakmap: new WeakMap(), weakset: new WeakSet(), promise: Promise.resolve(), proxy: new Proxy({}, {}), error: new Error(\"e\"), typeerror: new TypeError(\"e\"), args: (function () { return arguments; })(), asyncfn: async () => {}, genfn: function* () {}, asyncgen: async function* () {}, fn: () => {}, genobj: (function* () {})(), mapiter: new Map().keys(), setiter: new Set().entries(), arraybuffer: new ArrayBuffer(1), sab: new SharedArrayBuffer(1), dataview: new DataView(new ArrayBuffer(1)), u8: new Uint8Array(1), f64: new Float64Array(1), big64: new BigInt64Array(1), buffer: Buffer.alloc(1), boxedn: new Number(1), boxeds: new String(\"\"), boxedb: new Boolean(true), boxedbig: Object(1n), boxedsym: Object(Symbol()), plain: {}, array: [], nul: null, num: 1, str: \"s\", fake: { [Symbol.toStringTag]: \"Date\" }, domex: new DOMException(\"d\") };
for (const name of Object.keys(util.types).sort()) t(\"types.\" + name, () => Object.entries(samples).filter(([, v]) => util.types[name](v)).map(([k]) => k).join(\",\"));
// ---- the rest of the api
t(\"keys\", () => Object.keys(util).sort());
t(\"promisify\", () => { const f = util.promisify((a, b, cb) => cb(null, a + b)); return [typeof f, f.length, f.name, Object.getPrototypeOf(f) === Function.prototype]; });
t(\"promisify custom\", () => { const f = () => {}; f[util.promisify.custom] = () => \"custom\"; return [util.promisify(f)(), String(util.promisify.custom)]; });
t(\"promisify bad\", () => util.promisify(5));
t(\"promisify custom bad\", () => { const f = () => {}; f[util.promisify.custom] = 5; return util.promisify(f); });
t(\"callbackify bad\", () => util.callbackify(5));
t(\"inherits\", () => { function A() {} function B() {} B.prototype.hi = () => \"hi\"; util.inherits(A, B); return [new A().hi(), A.super_ === B, new A() instanceof B]; });
t(\"inherits bad\", () => util.inherits(5, function () {}));
t(\"inherits bad proto\", () => util.inherits(function () {}, { prototype: undefined }));
t(\"deprecate\", () => { const f = util.deprecate((x) => x * 2, \"old\", \"DEP_X\"); return [f(2), f.name, f.length]; });
t(\"deprecate bad code\", () => util.deprecate(() => {}, \"m\", 5));
t(\"_extend\", () => util._extend({ a: 1 }, { b: 2 }));
t(\"isArray\", () => [util.isArray([]), util.isArray({})]);
t(\"toUSVString\", () => [util.toUSVString(\"ok\"), util.toUSVString(5)]);
t(\"stripVTControlCharacters\", () => [util.stripVTControlCharacters(\"\\u001b[31mred\\u001b[0m plain \\u001b]8;;http://x\\u0007link\\u001b]8;;\\u0007\"), call(() => util.stripVTControlCharacters(5))]);
t(\"styleText\", () => [util.styleText(\"red\", \"x\", { validateStream: false }), util.styleText([\"bold\", \"underline\"], \"y\", { validateStream: false }), call(() => util.styleText(\"red\", 5))].map((s) => JSON.stringify(s)));
t(\"getSystemErrorName\", () => [util.getSystemErrorName(-2), util.getSystemErrorName(-13), call(() => util.getSystemErrorName(2)), call(() => util.getSystemErrorName(\"x\")), util.getSystemErrorMessage(-2), util.getSystemErrorMap().get(-2)]);
t(\"debuglog\", () => { const d = util.debuglog(\"nope\"); d(\"not shown %d\", 1); return [typeof d, d.enabled, util.debug === util.debuglog]; });
t(\"inspect.custom\", () => { class C { [util.inspect.custom](depth, options, inspect) { return \"custom:\" + depth + \":\" + typeof options.stylize + \":\" + (inspect === util.inspect); } } return [util.inspect(new C()), util.inspect({ c: new C() }), util.inspect(new C(), { customInspect: false }), String(util.inspect.custom)]; });
t(\"inspect.custom returns object\", () => util.inspect({ [util.inspect.custom]: () => ({ replaced: true }) }));
t(\"inspect.defaultOptions\", () => { const old = { ...util.inspect.defaultOptions }; util.inspect.defaultOptions = { depth: 0 }; const r = util.inspect({ a: { b: 1 } }); util.inspect.defaultOptions = old; return [r, Object.keys(old).sort()]; });
t(\"inspect.defaultOptions bad\", () => { util.inspect.defaultOptions = 5; });
t(\"inspect bad options\", () => util.inspect({}, 5));
t(\"inspect legacy args\", () => util.inspect({ a: { b: { c: {} } } }, true, 0, false));
t(\"inspect.colors/styles\", () => [Object.keys(util.inspect.colors).length, Object.keys(util.inspect.styles).sort(), util.inspect(\"s\", { colors: true }), util.inspect(5, { colors: true })].map((s) => JSON.stringify(s)));
t(\"inspect buffer\", () => [util.inspect(Buffer.from(\"hello\")), util.inspect(Buffer.alloc(60)), util.inspect(Buffer.alloc(0)), util.inspect({ b: Buffer.from([1]) })]);
t(\"inspect long string\", () => util.inspect(\"a\".repeat(120) + \"\\n\" + \"b\".repeat(20)));
t(\"inspect string quotes\", () => [util.inspect(\"'\"), util.inspect(\"\\\"\"), util.inspect(\"`'\\\"\"), util.inspect(\"a\\tb\\u0001\\u007f\\u00a0\"), util.inspect(\"\\ud83d\\ude00\")]);
t(\"inspect numbers\", () => [0, -0, 1e21, 1e-7, 123456.789, -1234567, 0xffffffffffn].map((n) => util.inspect(n, { numericSeparator: true })));
t(\"inspect class instance kinds\", () => [util.inspect(new (class A { #p = 1; x = 2; })()), util.inspect(Object.create(class B {}.prototype)), util.inspect(Object.setPrototypeOf({ a: 1 }, null)), util.inspect(Object.setPrototypeOf([1], null))]);
t(\"inspect symbols and keys\", () => util.inspect({ [Symbol()]: 1, [Symbol(\"d\")]: 2, \"a-b\": 3, 1e3: 4, constructor: 5, __proto__: null }));
t(\"inspect nested arrays\", () => util.inspect([[[[[1]]]], [[[[2]]]]]));
t(\"inspect big map/set\", () => [util.inspect(new Map(Array.from({ length: 5 }, (_, i) => [i, { i }]))), util.inspect(new Set(Array.from({ length: 120 }, (_, i) => i)))]);
t(\"inspect typed arrays\", () => [util.inspect(new Float32Array([1.5, -0, NaN])), util.inspect(new BigUint64Array([1n])), util.inspect(new Uint8Array(200)), util.inspect(new Int8Array(2), { showHidden: true })]);
t(\"inspect promises\", () => [util.inspect(Promise.resolve({ a: 1 })), util.inspect(new Promise(() => {})), util.inspect((() => { const p = Promise.reject(new RangeError(\"r\")); p.catch(() => {}); p.then = undefined; return Object.assign(Promise.reject(1), {}).catch(() => {}); })())]);
t(\"inspect rejected\", () => { const p = Promise.reject(\"why\"); p.catch(() => {}); return util.inspect(p); });
t(\"inspect proxies\", () => [util.inspect(new Proxy([1, 2], {})), util.inspect(new Proxy({ a: 1 }, { get: () => 5 })), util.inspect(new Proxy(new Map(), {}), { showProxy: true }), (() => { const { proxy, revoke } = Proxy.revocable({}, {}); revoke(); return util.inspect(proxy) + util.inspect(proxy, { showProxy: true }); })()]);
t(\"inspect iterators mid-way\", () => { const it = new Map([[1, \"a\"], [2, \"b\"], [3, \"c\"]]).entries(); it.next(); const s = new Set([\"x\", \"y\"]).values(); s.next(); s.next(); return [util.inspect(it), util.inspect(s), util.inspect(new Map([[1, 2]]).keys()), util.inspect(new Map([[1, 2]]).values()), util.inspect(new Set([1]).entries())]; });
t(\"inspect weak\", () => { const k = { key: 1 }; return [util.inspect(new WeakMap([[k, \"v\"]]), { showHidden: true }), util.inspect(new WeakSet([k]), { showHidden: true })]; });
t(\"inspect functions\", () => [util.inspect(function () {}), util.inspect(Object.assign(() => {}, { prop: 1 })), util.inspect(async () => {}), util.inspect(function* g() {}), util.inspect(async function* ag() {}), util.inspect(Object.setPrototypeOf(function np() {}, null)), util.inspect(Math.sin), util.inspect((() => {}).bind(null))]);
t(\"inspect boxed\", () => [util.inspect(new String(\"s\")), util.inspect(Object.assign(new Number(1), { extra: true })), util.inspect(Object(Symbol(\"b\"))), util.inspect(new Boolean(true)), util.inspect(Object(-0))]);
t(\"inspect dates\", () => [util.inspect(new Date(\"2026-01-02T03:04:05.678Z\")), util.inspect(new Date(\"nope\")), util.inspect(Object.assign(new Date(0), { extra: 1 }))]);
t(\"inspect module-like\", () => util.inspect(Object.defineProperty({}, Symbol.toStringTag, { value: \"Tagged\" })));
t(\"inspect circular\", () => { const a = {}; const b = { a }; a.b = b; a.list = [a, b]; const m = new Map(); m.set(\"self\", m); return [util.inspect(a), util.inspect(m)]; });
t(\"inspect depth default\", () => util.inspect({ a: { b: { c: { d: { e: 1 } } } } }));
t(\"inspect array extras\", () => util.inspect(Object.assign([1, 2], { extra: \"x\", [-1]: \"neg\" })));
t(\"inspect object with many keys\", () => util.inspect(Object.fromEntries(Array.from({ length: 30 }, (_, i) => [\"key\" + i, i]))));
t(\"inspect line widths\", () => util.inspect({ a: \"x\".repeat(30), b: \"y\".repeat(30), c: [1, 2, 3], d: { e: \"z\".repeat(20) } }));
t(\"isDeepStrictEqual kinds\", () => [[new Date(1), new Date(1)], [/a/g, /a/g], [/a/g, /a/i], [new Map([[1, { a: 1 }]]), new Map([[1, { a: 1 }]])], [new Set([{ a: 1 }]), new Set([{ a: 1 }])], [[1, , 3], [1, undefined, 3]], [new Uint8Array([1]), new Uint8Array([1])], [new Uint8Array([1]), new Int8Array([1])], [Buffer.from(\"a\"), Buffer.from(\"a\")], [NaN, NaN], [0, -0], [{ a: undefined }, {}], [Object.create(null), {}], [new Number(1), new Number(1)], [new Number(1), 1], [Symbol.for(\"a\"), Symbol.for(\"a\")], [new Error(\"a\"), new Error(\"a\")], [new Error(\"a\"), new TypeError(\"a\")], [{ [Symbol(\"s\")]: 1 }, { [Symbol(\"s\")]: 1 }], [[1, 2], { 0: 1, 1: 2, length: 2 }], [new Float64Array([NaN]), new Float64Array([NaN])], [new ArrayBuffer(2), new ArrayBuffer(2)], [() => {}, () => {}], [new URL(\"http://a/\"), new URL(\"http://a/\")]].map(([a, b]) => util.isDeepStrictEqual(a, b)));
t(\"isPartialDeepStrictEqual\", () => [util.isPartialDeepStrictEqual({ a: 1, b: 2 }, { a: 1 }), util.isPartialDeepStrictEqual({ a: 1 }, { a: 1, b: 2 }), util.isPartialDeepStrictEqual([1, 2, 3], [1, 3]), util.isPartialDeepStrictEqual(new Set([1, 2]), new Set([2]))]);
const pa = (config) => () => { const r = util.parseArgs(config); return [r.values, r.positionals, r.tokens?.length]; };
t(\"parseArgs basics\", pa({ args: [\"-f\", \"--bar\", \"b\", \"pos\"], options: { foo: { type: \"boolean\", short: \"f\" }, bar: { type: \"string\" } }, allowPositionals: true }));
t(\"parseArgs multiple\", pa({ args: [\"--x=1\", \"--x\", \"2\", \"-abc\", \"--\", \"--raw\"], options: { x: { type: \"string\", multiple: true }, a: { type: \"boolean\", short: \"a\" }, b: { type: \"boolean\", short: \"b\" }, c: { type: \"boolean\", short: \"c\" } }, allowPositionals: true, tokens: true }));
t(\"parseArgs defaults\", pa({ args: [], options: { n: { type: \"string\", default: \"d\" }, v: { type: \"boolean\", default: false } } }));
t(\"parseArgs negative\", pa({ args: [\"--no-color\", \"--verbose\"], options: { color: { type: \"boolean\" }, verbose: { type: \"boolean\" } }, allowNegative: true }));
t(\"parseArgs unknown\", pa({ args: [\"--nope\"], options: {} }));
t(\"parseArgs unexpected positional\", pa({ args: [\"pos\"], options: {} }));
t(\"parseArgs missing value\", pa({ args: [\"--bar\"], options: { bar: { type: \"string\" } } }));
t(\"parseArgs bad type\", pa({ args: [], options: { x: { type: \"number\" } } }));
t(\"parseArgs not strict\", pa({ args: [\"--anything=1\", \"-z\", \"p\"], strict: false }));
t(\"MIMEType\", () => { const m = new util.MIMEType(\"Text/HTML; Charset=UTF-8; q=\\\"a b\\\"\"); m.params.set(\"x\", \"y z\"); return [String(m), m.type, m.subtype, m.essence, [...m.params], m.params.get(\"charset\"), m.params.has(\"q\"), JSON.stringify(m), Object.prototype.toString.call(m)]; });
t(\"MIMEType setters\", () => { const m = new util.MIMEType(\"a/b\"); m.type = \"Image\"; m.subtype = \"PNG\"; m.params.delete(\"none\"); return [String(m), call(() => { m.type = \"\"; }), call(() => { m.subtype = \"a/b\"; })]; });
t(\"MIMEType bad\", () => new util.MIMEType(\"nope\"));
t(\"MIMEType bad 2\", () => new util.MIMEType(\"a/b;;;=x; ok=1; ok=2; bad key=3\").toString());
t(\"MIMEParams\", () => { const p = new util.MIMEParams(); p.set(\"a\", \"1\"); p.set(\"b\", \"needs \\\"quote\\\"\"); return [String(p), [...p.keys()], [...p.values()], call(() => p.set(\"bad key\", \"x\")), call(() => p.set(\"k\", \"\\u0001\"))]; });
t(\"TextEncoder via util\", () => [util.TextEncoder === TextEncoder, util.TextDecoder === TextDecoder]);
t(\"diff\", () => [util.diff(\"abc\", \"abd\"), util.diff([\"a\", \"b\"], [\"a\", \"c\", \"b\"]), call(() => util.diff(1, 2))]);
t(\"errors from real internals\", () => { try { util.inherits(null, null); } catch (e) { return [e.name, e.code, e.message, String(e), Object.keys(e), e instanceof TypeError]; } });
const later = [];
const settle = (label, promise) => later.push(promise.then((v) => label + \" resolved \" + JSON.stringify(v), (e) => label + \" rejected \" + e.name + \" \" + e.code + \" \" + e.message));
settle(\"promisify ok\", util.promisify((a, cb) => cb(null, a * 2))(21));
settle(\"promisify err\", util.promisify((cb) => cb(new RangeError(\"nope\")))());
settle(\"promisify multi\", util.promisify((cb) => cb(null, 1, 2))());
settle(\"promisify this\", util.promisify(function (cb) { cb(null, this.v); }).call({ v: 9 }));
settle(\"callbackify ok\", new Promise((resolve) => util.callbackify(async (x) => x + 1)(1, (err, v) => resolve([err, v]))));
settle(\"callbackify err\", new Promise((resolve) => util.callbackify(async () => { throw new TypeError(\"t\"); })((err) => resolve([err.name, err.message]))));
settle(\"callbackify falsy\", new Promise((resolve) => util.callbackify(() => Promise.reject(null))((err) => resolve([err.name, err.code, err.message, err.reason]))));
settle(\"aborted\", (async () => { try { return typeof util.aborted; } catch (e) { return e.code; } })());
Promise.all(later).then((settled) => {
for (const l of settled) lines.push(l);
const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"I \") ? \"inspect generated\" : line.startsWith(\"Q \") ? \"equality generated\" : line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
});

"

// 300 generated values through util.inspect with generated options, 100
// generated pairs through isDeepStrictEqual, and the rest of util's api:
// format, the type checks, promisify, callbackify, deprecate, inherits,
// parseArgs, MIMEType, styleText. Each line is a kind of case, how many, and
// a hash of what they printed in node 26.11.1
pub fn util_matches_node_test() {
  let rt = run_to_end(run_commonjs(node_runtime(), util_script))
  assert out_lines(rt)
    == [
      "inspect generated 300 3268287443",
      "equality generated 100 4160163590",
      "format 29 3792613809",
      "formatWithOptions 2 2015960298",
      "types.isAnyArrayBuffer 1 214766504",
      "types.isArgumentsObject 1 2535157484",
      "types.isArrayBuffer 1 3330399800",
      "types.isArrayBufferView 1 2166171830",
      "types.isAsyncFunction 1 4183330138",
      "types.isBigInt64Array 1 1786692618",
      "types.isBigIntObject 1 1868185150",
      "types.isBigUint64Array 1 2065710283",
      "types.isBooleanObject 1 167649939",
      "types.isBoxedPrimitive 1 223422513",
      "types.isCryptoKey 1 1988536836",
      "types.isDataView 1 4216004130",
      "types.isDate 1 2822054148",
      "types.isExternal 1 894203657",
      "types.isFloat16Array 1 1433812038",
      "types.isFloat32Array 1 3062104812",
      "types.isFloat64Array 1 1692366621",
      "types.isGeneratorFunction 1 1734199955",
      "types.isGeneratorObject 1 2406055909",
      "types.isInt16Array 1 3667569725",
      "types.isInt32Array 1 1022047855",
      "types.isInt8Array 1 2027317124",
      "types.isKeyObject 1 2629801816",
      "types.isMap 1 3715368968",
      "types.isMapIterator 1 1352726520",
      "types.isModuleNamespaceObject 1 1923184824",
      "types.isNativeError 1 648746506",
      "types.isNumberObject 1 2900715116",
      "types.isPromise 1 734702404",
      "types.isProxy 1 2892445468",
      "types.isRegExp 1 3152985342",
      "types.isSet 1 1600652700",
      "types.isSetIterator 1 2053560772",
      "types.isSharedArrayBuffer 1 4145424114",
      "types.isStringObject 1 192097003",
      "types.isSymbolObject 1 550507250",
      "types.isTypedArray 1 132524420",
      "types.isUint16Array 1 3140921112",
      "types.isUint32Array 1 1947380282",
      "types.isUint8Array 1 3242454048",
      "types.isUint8ClampedArray 1 1426495401",
      "types.isWeakMap 1 3937712720",
      "types.isWeakSet 1 1636978364",
      "keys 1 1263016706",
      "promisify 8 3227000877",
      "callbackify 4 4239679772",
      "inherits 3 2997554435",
      "deprecate 2 326341593",
      "_extend 1 4095957226",
      "isArray 1 2561164555",
      "toUSVString 1 4110052284",
      "stripVTControlCharacters 1 408470721",
      "styleText 1 2410168453",
      "getSystemErrorName 1 3492535761",
      "debuglog 1 1231086015",
      "inspect.custom 2 2658837409",
      "inspect.defaultOptions 2 3188036314",
      "inspect 25 3569434369",
      "inspect.colors/styles 1 3353156063",
      "isDeepStrictEqual 1 3961245404",
      "isPartialDeepStrictEqual 1 3536904300",
      "parseArgs 9 134904136",
      "MIMEType 4 1661135267",
      "MIMEParams 1 3763910895",
      "TextEncoder 1 1892808332",
      "diff 1 3229962060",
      "errors 1 198243991",
      "aborted 1 1600073568",
    ]
}

pub fn error_messages_show_values_as_node_does_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "const show = (f) => { try { f(); } catch (error) { out.push(error.message); } };
      show(() => Buffer.alloc(-1));
      show(() => Buffer.from({}));
      show(() => new URL('/', { a: [1, { b: new Map() }] }));
      show(() => new (require('events'))().emit('error', { why: new Set([1]) }));",
    )
  assert out_lines(rt)
    == [
      "The value of \"size\" is out of range. It must be >= 0 && <= 9007199254740991. Received -1",
      "The first argument must be of type string or an instance of Buffer, ArrayBuffer, or Array or an Array-like Object. Received an instance of Object",
      "Invalid URL",
      "Unhandled error. ({ why: Set(1) { 1 } })",
    ]
}

const stream_script = "
const stream = require(\"stream\");
const process = require(\"process\");
const { Readable, Writable, Duplex, Transform, PassThrough, pipeline, finished } = stream;
let seed = 4242;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const pending = [];
const show = (v) => { if (Buffer.isBuffer(v)) return \"buf:\" + v.toString(\"hex\"); if (v instanceof Error) return v.name + \":\" + v.code + \":\" + v.message; try { return JSON.stringify(v) ?? String(v); } catch { return String(v); } };
const call = (f) => { try { const v = f(); return show(v); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message; } };
// a way to call back later that the scenario picks
const laters = [(f) => f(), (f) => process.nextTick(f), (f) => Promise.resolve().then(f), (f) => setImmediate(f)];
function scenario(label, run) {
  pending.push(() => new Promise((resolve) => {
    const log = [];
    let done = false;
    const guard = setTimeout(() => finish(\"timeout\"), 5000);
    const finish = (why) => { if (done) return; done = true; clearTimeout(guard); lines.push(label + \" \" + log.join(\" \") + \" => \" + why); resolve(); };
    try { run((...parts) => log.push(parts.map((p) => (typeof p === \"string\" ? p : show(p))).join(\":\")), finish); } catch (e) { log.push(\"threw \" + e.name + \" \" + e.code + \" \" + e.message); finish(\"threw\"); }
  }));
}
// lets everything queued so far run: ticks, promise jobs, and several turns of the loop
const settle = (then) => { let turns = 6; const turn = () => (turns-- > 0 ? setImmediate(turn) : then()); turn(); };
// ---- generated pipelines: source -> transforms -> sink, with generated sizes, modes, timing and failures
for (let i = 0; i < 220; i++) {
  const objectMode = rnd(4) === 0;
  const count = rnd(9);
  const items = Array.from({ length: count }, (_, k) => (objectMode ? { k } : \"c\" + k + \"x\".repeat(rnd(6))));
  const rhwm = pick([undefined, 1, 4, 16]), whwm = pick([undefined, 1, 3, 20]);
  const rlater = pick(laters), wlater = pick(laters), tlater = pick(laters);
  const failAt = rnd(5) === 0 ? pick([\"read\", \"write\", \"transform\", \"destroy\"]) : \"none\";
  const stages = rnd(3);
  const useEncoding = !objectMode && rnd(4) === 0;
  scenario(\"P \" + i + \" \" + [objectMode, count, rhwm, whwm, laters.indexOf(rlater), laters.indexOf(wlater), laters.indexOf(tlater), failAt, stages, useEncoding].join(\",\"), (log, finish) => {
    let n = 0;
    const source = new Readable({ objectMode, highWaterMark: rhwm, read() { rlater(() => { if (failAt === \"read\" && n === 2) return this.destroy(new Error(\"read failed\")); log(\"push\", this.push(n < items.length ? items[n++] : null)); }); } });
    if (useEncoding) source.setEncoding(\"utf8\");
    const transforms = Array.from({ length: stages }, (_, s) => new Transform({ objectMode, highWaterMark: pick([undefined, 2]), transform(chunk, enc, cb) { tlater(() => { if (failAt === \"transform\" && s === 0 && String(objectMode ? chunk.k : chunk).includes(\"1\")) return cb(new RangeError(\"transform failed\")); cb(null, objectMode ? { ...chunk, [\"t\" + s]: true } : String(chunk).toUpperCase() + s); }); }, flush(cb) { log(\"flush\" + s); cb(null, objectMode ? { flushed: s } : \"F\" + s); } }));
    const seen = [];
    const sink = new Writable({ objectMode, highWaterMark: whwm, decodeStrings: rnd(2) === 0, write(chunk, enc, cb) { wlater(() => { if (failAt === \"write\" && seen.length === 1) return cb(new TypeError(\"write failed\")); if (failAt === \"destroy\" && seen.length === 1) (count % 2 ? source : sink).destroy(new Error(\"destroyed\")); seen.push(objectMode ? JSON.stringify(chunk) : String(chunk)); cb(); }); }, final(cb) { log(\"final\"); wlater(cb); } });
    for (const [name, s] of [[\"src\", source], [\"sink\", sink]]) for (const ev of [\"close\", \"end\", \"finish\", \"error\", \"drain\", \"pipe\", \"unpipe\"]) s.on(ev, (arg) => log(name + \".\" + ev, arg instanceof Error ? arg.message : arg === undefined || typeof arg === \"object\" ? \"\" : arg));
    pipeline(source, ...transforms, sink, (err) => { log(\"done\", err ? err.name + \":\" + err.code + \":\" + err.message : \"ok\", seen.join(\"|\")); log(\"state\", source.destroyed, sink.destroyed, sink.writableFinished, source.readableEnded, sink.writableLength, source.readableLength); settle(() => finish(\"finished\")); });
  });
}
// ---- generated paused-mode reads
for (let i = 0; i < 80; i++) {
  const text = \"\\u00e9\\u20ac\\ud83d\\ude00abc\".repeat(1 + rnd(3)) + \"end\";
  const bytes = Buffer.from(text);
  const enc = pick([undefined, \"utf8\", \"hex\", \"base64\", \"latin1\", \"utf16le\"]);
  const sizes = Array.from({ length: 8 }, () => pick([undefined, 0, 1, 2, 3, 5, 50]));
  const piece = 1 + rnd(5);
  scenario(\"R \" + i + \" \" + [enc, piece, sizes.join(\"/\")].join(\",\"), (log, finish) => {
    let at = 0;
    const r = new Readable({ highWaterMark: pick([2, 8, 64]), read() { if (at >= bytes.length) return this.push(null); const c = bytes.subarray(at, at + piece); at += piece; this.push(c); } });
    if (enc) r.setEncoding(enc);
    let k = 0;
    let activity = 0, ended = false;
    // a read size that never takes the last bytes leaves the stream waiting for good
    const watch = () => { const mine = ++activity; settle(() => settle(() => { if (mine === activity && !ended) finish(\"stalled\"); })); };
    r.on(\"readable\", () => { let c; let guard = 0; while (guard++ < 50 && (c = r.read(sizes[k++ % sizes.length])) !== null) log(\"read\", typeof c === \"string\" ? Buffer.from(c).toString(\"hex\") : c, r.readableLength, r.readableFlowing); log(\"null\", r.readableLength); watch(); });
    r.on(\"end\", () => { ended = true; log(\"end\", r.readableEnded, r.readable); finish(\"ended\"); });
  });
}
// ---- generated writes with backpressure, cork and end
for (let i = 0; i < 80; i++) {
  const hwm = pick([1, 5, 16, undefined]);
  const later = pick(laters);
  const ops = Array.from({ length: 2 + rnd(8) }, () => pick([\"write\", \"write\", \"write\", \"cork\", \"uncork\", \"end\", \"writev?\", \"destroy\", \"setenc\"]));
  const withWritev = rnd(3) === 0;
  scenario(\"W \" + i + \" \" + [hwm, laters.indexOf(later), withWritev, ops.join(\"/\")].join(\",\"), (log, finish) => {
    const options = { highWaterMark: hwm, write(chunk, enc, cb) { log(\"_write\", chunk, enc); later(cb); }, final(cb) { log(\"_final\"); later(cb); }, destroy(err, cb) { log(\"_destroy\", err ? err.message : null); cb(err); } };
    if (withWritev) options.writev = (chunks, cb) => { log(\"_writev\", chunks.map((c) => c.chunk.toString()).join(\"+\")); later(cb); };
    const w = new Writable(options);
    for (const ev of [\"drain\", \"finish\", \"close\", \"error\"]) w.on(ev, (e) => log(\"on.\" + ev, e ? e.code + \":\" + e.message : \"\"));
    let n = 0;
    for (const op of ops) {
      log(op, call(() => { switch (op) { case \"write\": return w.write(\"d\" + n++, pick([undefined, \"utf8\", \"hex\", \"latin1\"]), (err) => log(\"cb\", err ? err.code : \"ok\")); case \"cork\": return w.cork(); case \"uncork\": return w.uncork(); case \"end\": return w.end(rnd(2) ? \"last\" : undefined) === w; case \"destroy\": return w.destroy(rnd(2) ? new Error(\"bye\") : undefined) === w; case \"setenc\": return w.setDefaultEncoding(pick([\"hex\", \"utf8\", \"nope\"])) === w; default: return [w.writableLength, w.writableNeedDrain, w.writableCorked, w.writableEnded, w.writableFinished, w.writable, w.writableHighWaterMark, w.writableObjectMode]; } }));
    }
    settle(() => settle(() => { log(\"final state\", w.writableLength, w.writableEnded, w.writableFinished, w.destroyed, w.errored ? w.errored.message : null, w.closed); finish(\"observed\"); }));
  });
}
// ---- api, one case each
const t = (label, f) => lines.push(label + \" \" + call(f));
t(\"keys\", () => Object.keys(stream).sort());
t(\"identity\", () => [stream === stream.Stream, typeof stream, stream.promises === require(\"stream/promises\"), Object.keys(require(\"stream/consumers\")).sort(), stream.Readable.prototype instanceof stream.Stream, stream.Duplex.prototype instanceof stream.Readable, new stream.Duplex() instanceof stream.Writable, stream.Transform.prototype instanceof stream.Duplex, new stream.PassThrough() instanceof stream.Transform, stream.Stream.prototype instanceof require(\"events\")]);
t(\"proto keys readable\", () => Object.getOwnPropertyNames(Readable.prototype).sort());
t(\"proto keys writable\", () => Object.getOwnPropertyNames(Writable.prototype).sort());
t(\"proto keys duplex\", () => Object.getOwnPropertyNames(Duplex.prototype).sort());
t(\"static readable\", () => Object.getOwnPropertyNames(Readable).sort());
t(\"defaults\", () => { const r = new Readable(), w = new Writable(), o = new Readable({ objectMode: true }); return [r.readableHighWaterMark, w.writableHighWaterMark, o.readableHighWaterMark, stream.getDefaultHighWaterMark(false), stream.getDefaultHighWaterMark(true), r.readableFlowing, r.readable, w.writable, r.readableEncoding, r.readableObjectMode, r.destroyed, r.closed, r.errored, r.readableAborted, r.readableDidRead]; });
t(\"setDefaultHighWaterMark\", () => { const old = stream.getDefaultHighWaterMark(false); stream.setDefaultHighWaterMark(false, 1024); const now = [stream.getDefaultHighWaterMark(false), new Readable().readableHighWaterMark]; stream.setDefaultHighWaterMark(false, old); return now; });
t(\"setDefaultHighWaterMark bad\", () => stream.setDefaultHighWaterMark(false, -1));
t(\"bad hwm\", () => new Readable({ highWaterMark: -1 }));
t(\"bad hwm type\", () => new Writable({ highWaterMark: \"x\" }));
t(\"read not implemented\", () => { const r = new Readable(); let got; r.on(\"error\", (e) => (got = e.code)); r.read(); return got ?? \"later\"; });
t(\"write not implemented\", () => { const w = new Writable(); let got; w.on(\"error\", (e) => (got = e.code)); w.write(\"x\"); return got ?? \"later\"; });
t(\"write null\", () => new Writable({ write() {} }).write(null));
t(\"write bad type\", () => new Writable({ write() {} }).write(5));
t(\"write bad encoding\", () => new Writable({ write() {} }).write(\"x\", \"nope\"));
t(\"push bad type\", () => { const r = new Readable({ read() {} }); let got; r.on(\"error\", (e) => (got = e.code)); r.push(5); return got ?? \"later\"; });
t(\"unshift after end\", () => { const r = new Readable({ read() {} }); r.push(null); let got; r.on(\"error\", (e) => (got = e.code)); r.read(); r.unshift(\"x\"); return got ?? \"later\"; });
t(\"setEncoding bad\", () => new Readable().setEncoding(\"nope\"));
t(\"pipe to non-writable\", () => { const r = Readable.from([\"a\"]); return typeof r.pipe(new PassThrough()).on; });
t(\"Readable.from kinds\", () => [Readable.from(\"str\").readableObjectMode, Readable.from(Buffer.from(\"b\")).readableObjectMode, Readable.from([1]).readableObjectMode, Readable.from(new Set([1])).readableHighWaterMark]);
t(\"Readable.from bad\", () => Readable.from(5));
t(\"isReadable etc\", () => { const r = new Readable({ read() {} }), w = new Writable({ write() {} }); const before = [stream.isReadable(r), stream.isWritable(w), stream.isErrored(r), stream.isDisturbed(r), stream.isDestroyed(r)]; r.destroy(new Error(\"x\")); r.on(\"error\", () => {}); return [before, stream.isReadable(r), stream.isErrored(r), stream.isDestroyed(r), stream.isReadable({}), stream.isDestroyed({})]; });
t(\"_uint8ArrayToBuffer\", () => { const b = stream._uint8ArrayToBuffer(new Uint8Array([104, 105])); return [Buffer.isBuffer(b), b.toString(), stream._isUint8Array(b), stream._isArrayBufferView(new DataView(new ArrayBuffer(1)))]; });
t(\"pipeline no streams\", () => pipeline());
t(\"pipeline one stream\", () => pipeline(new PassThrough(), () => {}) instanceof PassThrough);
t(\"pipeline bad callback\", () => pipeline(new PassThrough(), new PassThrough()));
t(\"finished bad\", () => finished({}, () => {}));
t(\"finished bad callback\", () => finished(new PassThrough()));
t(\"addAbortSignal bad\", () => stream.addAbortSignal({}, new PassThrough()));
t(\"addAbortSignal bad stream\", () => stream.addAbortSignal(new AbortController().signal, {}));
t(\"duplexPair\", () => { const [a, b] = stream.duplexPair(); return [a instanceof Duplex, b instanceof Duplex, a !== b]; });
t(\"compose bad\", () => stream.compose());
t(\"destroy helper\", () => { const r = new Readable({ read() {} }); r.on(\"error\", () => {}); stream.destroy(r, new Error(\"x\")); return r.destroyed; });
t(\"Symbol.asyncIterator\", () => typeof Readable.prototype[Symbol.asyncIterator]);
t(\"Symbol.asyncDispose\", () => [typeof Readable.prototype[Symbol.asyncDispose], typeof Writable.prototype[Symbol.asyncDispose]]);
t(\"inspect a stream\", () => { const s = require(\"util\").inspect(new PassThrough(), { depth: 0 }); return [s.startsWith(\"PassThrough {\"), s.includes(\"_readableState: [ReadableState]\"), s.includes(\"_writableState: [WritableState]\")]; });
t(\"legacy Stream pipe\", () => { const s = new stream.Stream(); return [typeof s.pipe, s instanceof require(\"events\"), Object.keys(s).sort()]; });
t(\"subclass old style\", () => { function Old(o) { Readable.call(this, o); } require(\"util\").inherits(Old, Readable); Old.prototype._read = function () { this.push(\"x\"); this.push(null); }; return new Old().read().toString(); });
t(\"construct option names\", () => { const calls = []; const d = new Duplex({ read() { calls.push(\"read\"); this.push(null); }, write(c, e, cb) { calls.push(\"write\"); cb(); }, allowHalfOpen: false, readableObjectMode: true, writableHighWaterMark: 7 }); return [d.readableObjectMode, d.writableObjectMode, d.writableHighWaterMark, d.allowHalfOpen]; });
// ---- behaviours that take time
const sc = (label, run) => scenario(\"S \" + label, run);
sc(\"flowing data events\", (log, finish) => { const r = Readable.from([\"a\", \"b\"]); r.on(\"data\", (c) => log(\"data\", c)); r.on(\"end\", () => finish(\"end\")); });
sc(\"pause resume\", (log, finish) => { const r = Readable.from([1, 2, 3]); r.on(\"data\", (c) => { log(\"data\", c, r.isPaused()); r.pause(); log(\"paused\", r.isPaused(), r.readableFlowing); setTimeout(() => r.resume(), 1); }); r.on(\"end\", () => finish(\"end\")); });
sc(\"async iterator break destroys\", (log, finish) => { (async () => { const r = Readable.from([1, 2, 3]); r.on(\"close\", () => log(\"close\")); for await (const c of r) { log(\"got\", c); break; } log(\"destroyed\", r.destroyed); settle(() => finish(\"ok\")); })(); });
sc(\"iterator with destroyOnReturn false\", (log, finish) => { (async () => { const r = Readable.from([1, 2, 3]); for await (const c of r.iterator({ destroyOnReturn: false })) { log(\"got\", c); break; } log(\"destroyed\", r.destroyed); for await (const c of r) log(\"rest\", c); finish(\"ok\"); })(); });
sc(\"operators\", (log, finish) => { (async () => { const src = () => Readable.from([1, 2, 3, 4, 5, 6]); log(\"map\", await src().map((x) => x * 2).toArray()); log(\"filter\", await src().filter((x) => x % 2).toArray()); log(\"take\", await src().take(2).toArray()); log(\"drop\", await src().drop(4).toArray()); log(\"flatMap\", await src().flatMap((x) => [x, x]).take(5).toArray()); log(\"reduce\", await src().reduce((a, b) => a + b, 0)); log(\"some\", await src().some((x) => x > 5)); log(\"every\", await src().every((x) => x > 5)); log(\"find\", await src().find((x) => x > 3)); const seen = []; await src().forEach((x) => seen.push(x)); log(\"forEach\", seen); log(\"async map concurrency\", await src().map(async (x) => x + 1, { concurrency: 2 }).toArray()); log(\"chained\", await src().filter((x) => x > 2).map((x) => \"v\" + x).take(2).toArray()); finish(\"ok\"); })().catch((e) => finish(\"failed \" + e.message)); });
sc(\"operator errors\", (log, finish) => { (async () => { for (const f of [() => Readable.from([1]).map(5), () => Readable.from([1]).take(-1), () => Readable.from([1]).map((x) => x, { concurrency: 0 }), () => Readable.from([1]).reduce(5)]) { try { const r = f(); await (r && r.toArray ? r.toArray() : r); log(\"no error\"); } catch (e) { log(e.name, e.code); } } finish(\"ok\"); })(); });
sc(\"operator abort\", (log, finish) => { (async () => { const c = new AbortController(); const p = Readable.from([1, 2, 3]).map(async (x, { signal }) => { await new Promise((r) => setTimeout(r, 5)); return x; }, { signal: c.signal }).toArray(); c.abort(); try { await p; log(\"no error\"); } catch (e) { log(e.name, e.code); } finish(\"ok\"); })(); });
sc(\"finished on readable\", (log, finish) => { const r = Readable.from([\"a\"]); finished(r, (err) => { log(\"finished\", err); finish(\"ok\"); }); r.resume(); });
sc(\"finished premature close\", (log, finish) => { const r = new Readable({ read() {} }); finished(r, (err) => { log(\"finished\", err); finish(\"ok\"); }); r.destroy(); });
sc(\"finished cleanup fn\", (log, finish) => { const r = new Readable({ read() {} }); const cleanup = finished(r, () => log(\"should not run\")); log(typeof cleanup); cleanup(); r.destroy(); settle(() => finish(\"ok\")); });
sc(\"finished promise with signal\", (log, finish) => { const c = new AbortController(); require(\"stream/promises\").finished(new Readable({ read() {} }), { signal: c.signal }).then(() => log(\"resolved\"), (e) => { log(e.name, e.code); finish(\"ok\"); }); c.abort(); });
sc(\"promises pipeline error\", (log, finish) => { require(\"stream/promises\").pipeline(Readable.from([\"a\"]), new Transform({ transform(c, e, cb) { cb(new Error(\"mid\")); } }), new PassThrough()).then(() => log(\"resolved\"), (e) => { log(e.message); finish(\"ok\"); }); });
sc(\"pipeline with generators\", (log, finish) => { pipeline(async function* () { yield \"a\"; yield \"b\"; }, async function* (source) { for await (const c of source) yield String(c).toUpperCase(); }, async function (source) { let s = \"\"; for await (const c of source) s += c; log(\"collected\", s); return s; }, (err, value) => { log(\"done\", err, value); finish(\"ok\"); }); });
sc(\"pipeline with signal\", (log, finish) => { const c = new AbortController(); pipeline(new Readable({ read() {} }), new PassThrough(), { signal: c.signal }, (err) => { log(err.name, err.code); finish(\"ok\"); }); c.abort(); });
sc(\"addAbortSignal\", (log, finish) => { const c = new AbortController(); const r = stream.addAbortSignal(c.signal, new Readable({ read() {} })); r.on(\"error\", (e) => { log(e.name, e.code); }); r.on(\"close\", () => finish(\"closed\")); c.abort(); });
sc(\"compose\", (log, finish) => { const composed = stream.compose(new Transform({ transform(c, e, cb) { cb(null, String(c) + \"1\"); } }), async function* (source) { for await (const c of source) yield String(c) + \"2\"; }); const got = []; composed.on(\"data\", (c) => got.push(String(c))); composed.on(\"end\", () => { log(got); finish(\"ok\"); }); composed.write(\"a\"); composed.end(\"b\"); });
sc(\"duplexPair talk\", (log, finish) => { const [a, b] = stream.duplexPair(); b.on(\"data\", (c) => { log(\"b got\", String(c)); b.write(\"pong\"); }); a.on(\"data\", (c) => { log(\"a got\", String(c)); finish(\"ok\"); }); a.write(\"ping\"); });
sc(\"Duplex.from\", (log, finish) => { (async () => { log(await Duplex.from([\"x\", \"y\"]).toArray()); log(await Duplex.from(async function* () { yield 1; }).toArray()); log(await Duplex.from(Promise.resolve(\"p\")).toArray()); const d = Duplex.from({ readable: Readable.from([\"r\"]), writable: new Writable({ write(c, e, cb) { log(\"w\", String(c)); cb(); } }) }); d.write(\"in\"); log(await d.toArray()); try { Duplex.from(5); } catch (e) { log(e.code); } finish(\"ok\"); })().catch((e) => finish(\"failed \" + e.message)); });
sc(\"transform errors\", (log, finish) => { const tr = new Transform({ transform(c, e, cb) { cb(); cb(); } }); tr.on(\"error\", (e) => { log(e.code); finish(\"ok\"); }); tr.write(\"x\"); });
sc(\"transform without transform\", (log, finish) => { const tr = new Transform(); tr.on(\"error\", (e) => { log(e.code, e.message); finish(\"ok\"); }); tr.write(\"x\"); });
sc(\"write after end\", (log, finish) => { const w = new Writable({ write(c, e, cb) { cb(); } }); w.on(\"error\", (e) => { log(\"event\", e.code); finish(\"ok\"); }); w.end(); w.write(\"late\", (e) => log(\"cb\", e.code)); });
sc(\"end with callback and chunk\", (log, finish) => { const got = []; const w = new Writable({ write(c, e, cb) { got.push(String(c)); cb(); } }); w.end(\"only\", \"utf8\", () => { log(\"end cb\", got, w.writableFinished); finish(\"ok\"); }); });
sc(\"push after eof\", (log, finish) => { const r = new Readable({ read() {} }); r.on(\"error\", (e) => { log(e.code); finish(\"ok\"); }); r.push(null); r.push(\"late\"); });
sc(\"destroy twice and events\", (log, finish) => { const r = new Readable({ read() {} }); r.on(\"error\", (e) => log(\"error\", e.message)); r.on(\"close\", () => { log(\"close\", r.destroyed, r.errored.message); finish(\"ok\"); }); r.destroy(new Error(\"first\")); r.destroy(new Error(\"second\")); });
sc(\"emitClose false\", (log, finish) => { const w = new Writable({ emitClose: false, write(c, e, cb) { cb(); } }); w.on(\"close\", () => log(\"close\")); w.on(\"finish\", () => { log(\"finish\"); settle(() => finish(\"ok\")); }); w.end(); });
sc(\"autoDestroy false\", (log, finish) => { const r = new Readable({ autoDestroy: false, read() { this.push(null); } }); r.on(\"end\", () => setTimeout(() => { log(\"destroyed\", r.destroyed); finish(\"ok\"); }, 2)); r.resume(); });
sc(\"construct hook\", (log, finish) => { const w = new Writable({ construct(cb) { log(\"construct\"); setTimeout(cb, 2); }, write(c, e, cb) { log(\"write\", String(c)); cb(); } }); w.write(\"early\"); w.end(() => finish(\"ok\")); });
sc(\"construct fails\", (log, finish) => { const r = new Readable({ construct(cb) { cb(new Error(\"no\")); }, read() {} }); r.on(\"error\", (e) => { log(e.message); finish(\"ok\"); }); });
sc(\"objectMode falsy values\", (log, finish) => { const r = new Readable({ objectMode: true, read() {} }); const got = []; r.on(\"data\", (c) => got.push(c)); r.on(\"end\", () => { log(got); finish(\"ok\"); }); r.push(0); r.push(\"\"); r.push(false); r.push(null); });
sc(\"readable event and read sizes\", (log, finish) => { const r = new Readable({ read() {} }); r.push(\"abcdef\"); r.push(null); r.once(\"readable\", () => { log(String(r.read(2)), String(r.read(10)), String(r.read()), r.read()); finish(\"ok\"); }); });
sc(\"unshift\", (log, finish) => { const r = Readable.from([\"bc\"], { objectMode: false }); r.once(\"readable\", () => { const c = r.read(); r.unshift(c); r.unshift(\"a\"); log(String(r.read())); finish(\"ok\"); }); });
sc(\"wrap old stream\", (log, finish) => { const old = new (require(\"events\"))(); old.pause = () => log(\"old.pause\"); old.resume = () => log(\"old.resume\"); const r = new Readable().wrap(old); const got = []; r.on(\"data\", (c) => got.push(String(c))); r.on(\"end\", () => { log(got); finish(\"ok\"); }); old.emit(\"data\", \"a\"); old.emit(\"data\", \"b\"); old.emit(\"end\"); });
sc(\"pipe unpipe events\", (log, finish) => { const r = new Readable({ read() {} }); const w = new PassThrough(); w.on(\"pipe\", (src) => log(\"pipe\", src === r)); w.on(\"unpipe\", (src) => log(\"unpipe\", src === r)); r.pipe(w); r.unpipe(w); r.pipe(w, { end: false }); r.push(\"x\"); r.push(null); r.on(\"end\", () => setTimeout(() => { log(\"writableEnded\", w.writableEnded); finish(\"ok\"); }, 2)); });
sc(\"multiple pipes\", (log, finish) => { const r = Readable.from([\"a\", \"b\"], { objectMode: false }); const got = [[], []]; const mk = (i) => new Writable({ write(c, e, cb) { got[i].push(String(c)); cb(); } }); r.pipe(mk(0)); r.pipe(mk(1)).on(\"finish\", () => { log(got); finish(\"ok\"); }); });
sc(\"consumers\", (log, finish) => { (async () => { const c = require(\"stream/consumers\"); log(await c.text(Readable.from([\"h\\u00e9\", \"llo\"]))); log(await c.json(Readable.from(['{\"a\":', \"1}\"]))); log(await c.buffer(Readable.from([Buffer.from(\"ab\"), Buffer.from(\"c\")]))); log(new Uint8Array(await c.arrayBuffer(Readable.from([Buffer.from(\"xy\")]))).join()); try { await c.json(Readable.from([\"{\"])); } catch (e) { log(e.name); } finish(\"ok\"); })().catch((e) => finish(\"failed \" + e.message)); });
sc(\"events EventEmitter.on over a stream\", (log, finish) => { (async () => { const r = Readable.from([\"a\", \"b\"]); const got = []; for await (const [c] of require(\"events\").on(r, \"data\", { close: [\"end\"] })) got.push(c); log(got); finish(\"ok\"); })(); });
sc(\"highWaterMark backpressure numbers\", (log, finish) => { const w = new Writable({ highWaterMark: 3, write(c, e, cb) { setTimeout(cb, 1); } }); log(w.write(\"a\"), w.write(\"b\"), w.write(\"c\"), w.writableLength, w.writableNeedDrain); w.on(\"drain\", () => { log(\"drain\", w.writableLength); finish(\"ok\"); }); });
sc(\"decodeStrings false\", (log, finish) => { const w = new Writable({ decodeStrings: false, write(c, e, cb) { log(typeof c, e); cb(); } }); w.write(\"s\", \"latin1\"); w.write(Buffer.from(\"b\")); w.end(() => finish(\"ok\")); });
sc(\"defaultEncoding\", (log, finish) => { const w = new Writable({ defaultEncoding: \"hex\", write(c, e, cb) { log(c, e); cb(); } }); w.write(\"6869\"); w.end(() => finish(\"ok\")); });
sc(\"readable encoding option\", (log, finish) => { const r = new Readable({ encoding: \"hex\", read() { this.push(Buffer.from([1, 255])); this.push(null); } }); r.on(\"data\", (c) => log(c, r.readableEncoding)); r.on(\"end\", () => finish(\"ok\")); });
sc(\"signal option\", (log, finish) => { const c = new AbortController(); const r = new Readable({ signal: c.signal, read() {} }); r.on(\"error\", (e) => { log(e.name); finish(\"ok\"); }); c.abort(); });
sc(\"error in data handler order\", (log, finish) => { const r = Readable.from([1, 2]); r.on(\"data\", (c) => log(\"data\", c)); r.on(\"error\", (e) => log(\"error\", e.message)); r.on(\"close\", () => finish(\"close\")); r.destroy(new Error(\"stop\")); });
(async () => {
for (const start of pending) await start();
const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"P \") ? \"pipelines generated\" : line.startsWith(\"R \") ? \"paused reads generated\" : line.startsWith(\"W \") ? \"writes generated\" : line.startsWith(\"S \") ? \"behaviours\" : line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
globalThis.out.push(\"finished\");
})();

"

// 220 generated pipelines (sizes, object mode, encodings, when each callback
// fires, where it fails), 80 generated paused reads, 80 generated runs of
// write, cork, end and destroy, and about 90 api and behaviour cases, run one
// at a time. Each line is a kind of case, how many, and a hash of the events
// and state they logged in node 26.11.1
pub fn streams_match_node_test() {
  let rt = run_to_end(run_commonjs(node_runtime(), stream_script))
  assert out_lines(rt)
    == [
      "keys 1 118087534",
      "identity 1 3419998701",
      "proto 3 1917534173",
      "static 1 3252094429",
      "defaults 1 388984814",
      "setDefaultHighWaterMark 2 2237790337",
      "bad 2 2591978027",
      "read 1 2912733598",
      "write 4 1152775990",
      "push 1 3952561952",
      "unshift 1 2839061847",
      "setEncoding 1 1269605874",
      "pipe 1 2514346320",
      "Readable.from 2 2529752064",
      "isReadable 1 1652705040",
      "_uint8ArrayToBuffer 1 3170151865",
      "pipeline 3 3718402949",
      "finished 2 3836949470",
      "addAbortSignal 2 2762018776",
      "duplexPair 1 1048101853",
      "compose 1 1932482287",
      "destroy 1 1712805417",
      "Symbol.asyncIterator 1 918519703",
      "Symbol.asyncDispose 1 680708370",
      "inspect 1 760238340",
      "legacy 1 1615191197",
      "subclass 1 637345559",
      "construct 1 3561837470",
      "pipelines generated 220 263944527",
      "paused reads generated 80 226554210",
      "writes generated 80 3350482710",
      "behaviours 42 4067627832",
      "finished",
    ]
}

pub fn stream_modules_share_one_stream_test() {
  let rt =
    run_to_end(run_commonjs(
      node_runtime(),
      "const stream = require('stream');
      const promises = require('stream/promises');
      const consumers = require('stream/consumers');
      out.push(String(stream.promises === promises));
      consumers.text(stream.Readable.from(['a', 'b'])).then((text) => out.push(text));
      consumers.blob(stream.Readable.from([])).catch((error) => out.push(error.code));",
    ))
  assert out_lines(rt) == ["true", "ERR_METHOD_NOT_IMPLEMENTED", "ab"]
}

const process_emitter_script = "
const process = require(\"process\");
const EventEmitter = require(\"events\");
const lines = [];
const call = (f) => { try { const v = f(); return typeof v === \"string\" ? v : JSON.stringify(v) ?? String(v); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f));
const proto = Object.getPrototypeOf(process);
t(\"is an emitter\", () => [process instanceof EventEmitter, Object.getPrototypeOf(proto) === EventEmitter.prototype, proto.constructor.name, typeof proto.constructor, Object.getOwnPropertyNames(proto)]);
t(\"tag\", () => [Object.prototype.toString.call(process), Object.getOwnPropertyDescriptor(process, Symbol.toStringTag)]);
t(\"emitter state\", () => [typeof process._events, Object.getPrototypeOf(process._events), typeof process._eventsCount, process._maxListeners === undefined, [\"_events\", \"_eventsCount\", \"_maxListeners\"].map((k) => Object.keys(process).includes(k))]);
t(\"methods come from EventEmitter\", () => [\"on\", \"once\", \"off\", \"emit\", \"addListener\", \"removeListener\", \"removeAllListeners\", \"listeners\", \"rawListeners\", \"listenerCount\", \"eventNames\", \"prependListener\", \"prependOnceListener\", \"setMaxListeners\", \"getMaxListeners\"].map((m) => process[m] === EventEmitter.prototype[m] && !Object.hasOwn(process, m)));
t(\"on, emit, off\", () => { const seen = []; const f = (a, b) => seen.push(\"f\" + a + b); process.on(\"spark-test\", f); process.once(\"spark-test\", (a) => seen.push(\"once\" + a)); const first = process.emit(\"spark-test\", 1, 2); const second = process.emit(\"spark-test\", 3, 4); process.off(\"spark-test\", f); return [first, second, process.emit(\"spark-test\"), seen, process.listenerCount(\"spark-test\")]; });
t(\"returns process\", () => { const f = () => {}; const r = [process.on(\"x-ret\", f) === process, process.off(\"x-ret\", f) === process, process.setMaxListeners(process.getMaxListeners()) === process]; return r; });
t(\"this in a listener\", () => { let same; process.once(\"x-this\", function () { same = this === process; }); process.emit(\"x-this\"); return same; });
t(\"bad listener\", () => process.on(\"x-bad\", 5));
t(\"getMaxListeners\", () => process.getMaxListeners());
t(\"names of the test's own events are gone\", () => process.eventNames().filter((n) => String(n).startsWith(\"x-\") || n === \"spark-test\"));
t(\"events helpers accept it\", () => { const f = () => {}; process.on(\"x-helper\", f); const r = [EventEmitter.getEventListeners(process, \"x-helper\").length, EventEmitter.listenerCount(process, \"x-helper\")]; process.off(\"x-helper\", f); return r; });
t(\"error event with no listener throws\", () => process.emit(\"error\", new Error(\"boom\")));
const later = [];
later.push(EventEmitter.once(process, \"x-later\").then((args) => \"once resolved \" + JSON.stringify(args)));
process.emit(\"x-later\", \"a\", 1);
later.push(new Promise((resolve) => { process.once(\"x-tick\", (v) => resolve(\"emitted from a tick \" + v)); process.nextTick(() => process.emit(\"x-tick\", 7)); }));
Promise.all(later).then((settled) => { for (const l of settled) lines.push(l); for (const l of lines) globalThis.out.push(l); });

"

// process is an EventEmitter the way node's is: by its prototype's
// prototype, under a prototype named process. Each line is what node
// 26.11.1 printed
pub fn process_is_an_event_emitter_test() {
  let rt = run_to_end(run_commonjs(node_runtime(), process_emitter_script))
  assert out_lines(rt)
    == [
      "is an emitter [true,true,\"process\",\"function\",[\"constructor\"]]",
      "tag [\"[object process]\",{\"value\":\"process\",\"writable\":true,\"enumerable\":false,\"configurable\":false}]",
      "emitter state [\"object\",null,\"number\",true,[true,true,true]]",
      "methods come from EventEmitter [true,true,true,true,true,true,true,true,true,true,true,true,true,true,true]",
      "on, emit, off [true,true,false,[\"f12\",\"once1\",\"f34\"],0]",
      "returns process [true,true,true]",
      "this in a listener true",
      "bad listener threw TypeError ERR_INVALID_ARG_TYPE The \"listener\" argument must be of type function. Received type number (5)",
      "getMaxListeners 10",
      "names of the test's own events are gone []",
      "events helpers accept it [1,1]",
      "error event with no listener throws threw Error undefined boom",
      "once resolved [\"a\",1]",
      "emitted from a tick 7",
    ]
}

const structured_clone_script = "
const util = require(\"util\");
let seed = 9001;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const show = (v) => util.inspect(v, { depth: 12, breakLength: Infinity, showHidden: false, sorted: false });
const call = (f) => { try { const v = f(); return typeof v === \"string\" ? v : show(v); } catch (e) { return \"threw \" + (e && e.constructor && e.constructor.name) + \" \" + e.name + \" \" + e.code + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f).replace(/\\n/g, \"\\\\n\"));
// ---- generated values: clone, print, and check it is a deep copy
const shared = () => ({ shared: true });
const fixed = (e) => { e.stack = e.name + \": \" + e.message + \"\\n    at fixed\"; return e; };
function leaf(pool) {
  return pick([
    () => rnd(1000) - 500, () => -0, () => NaN, () => Infinity, () => 12345678901234567890n, () => \"str\", () => \"\", () => true, () => null, () => undefined,
    () => new Date(86400000 * rnd(20000)), () => new Date(NaN), () => /re+g/gi, () => { const r = /a/y; r.lastIndex = 3; return r; },
    () => new Boolean(false), () => new Number(-0), () => new String(\"boxed\"), () => Object(5n),
    () => new Uint8Array([1, 2, 3]), () => new Float64Array([1.5, NaN]), () => new ArrayBuffer(3), () => new DataView(new ArrayBuffer(4), 1, 2), () => Buffer.from(\"hi\"), () => new Uint16Array(new ArrayBuffer(8), 2, 2), () => new BigInt64Array([1n, -1n]),
    () => fixed(new RangeError(\"range\")), () => fixed(Object.assign(new Error(\"with cause\", { cause: { why: 1 } }), { extra: \"dropped\" })), () => { const e = fixed(new TypeError(\"named\")); e.name = \"CustomName\"; return e; }, () => fixed(new Error(\"stack\")),
    () => new (class Point { constructor() { this.x = 1; } get y() { return 2; } })(), () => Object.create(null), () => Object.create({ inherited: 1 }, { own: { value: 2, enumerable: true }, hidden: { value: 3, enumerable: false } }),
    () => ({ [Symbol(\"s\")]: 1, kept: 2 }), () => ({ get g() { return \"from getter\"; } }), () => [, 1, , ], () => Object.assign([1, 2], { extra: \"x\" }), () => new Array(3), () => Object.freeze({ frozen: 1 }),
    () => pool[rnd(pool.length)] ?? (pool.push(shared()), pool[0]),
  ])();
}
function value(depth, pool) {
  if (depth <= 0 || rnd(3) === 0) return leaf(pool);
  switch (rnd(4)) {
    case 0: return Array.from({ length: rnd(6) }, () => value(depth - 1, pool));
    case 1: { const o = {}; for (let i = 0; i < rnd(6); i++) o[pick([\"a\", \"b\", \"k\" + i, \"10\", \"x y\"])] = value(depth - 1, pool); if (rnd(4) === 0) o.self = o; return o; }
    case 2: return new Map(Array.from({ length: rnd(4) }, () => [value(depth - 1, pool), value(depth - 1, pool)]));
    default: return new Set(Array.from({ length: rnd(4) }, () => value(depth - 1, pool)));
  }
}
function sameShape(a, b, seen = new Map()) {
  if (a === null || (typeof a !== \"object\" && typeof a !== \"function\")) return Object.is(a, b) ? \"\" : \"primitive differs\";
  if (a === b) return \"same object, not a copy\";
  if (seen.has(a)) return seen.get(a) === b ? \"\" : \"identity not kept\";
  seen.set(a, b);
  if (Object.prototype.toString.call(a) !== Object.prototype.toString.call(b) && !Buffer.isBuffer(a)) return \"tag differs\";
  if (a instanceof Map) { const ae = [...a], be = [...b]; if (ae.length !== be.length) return \"map size\"; for (let i = 0; i < ae.length; i++) { const r = sameShape(ae[i][0], be[i][0], seen) || sameShape(ae[i][1], be[i][1], seen); if (r) return r; } return \"\"; }
  if (a instanceof Set) { const ae = [...a], be = [...b]; if (ae.length !== be.length) return \"set size\"; for (let i = 0; i < ae.length; i++) { const r = sameShape(ae[i], be[i], seen); if (r) return r; } return \"\"; }
  if (a instanceof Error || ArrayBuffer.isView(a) || a instanceof ArrayBuffer || a instanceof Date || a instanceof RegExp) return \"\";
  for (const k of Object.keys(a)) { const r = sameShape(a[k], b[k], seen); if (r) return k + \": \" + r; }
  return \"\";
}
for (let i = 0; i < 300; i++) {
  const v = value(3, []);
  lines.push(\"G \" + i + \" \" + call(() => { const c = structuredClone(v); return show(c) + \" | \" + (sameShape(v, c) || \"deep copy\") + \" | \" + (c !== null && typeof c === \"object\" ? Object.getPrototypeOf(c)?.constructor?.name : typeof c); }).replace(/\\n/g, \"\\\\n\"));
}
// ---- one case each
t(\"shape\", () => [typeof structuredClone, structuredClone.name, structuredClone.length, Object.getOwnPropertyDescriptor(globalThis, \"structuredClone\").enumerable, Object.getOwnPropertyDescriptor(globalThis, \"structuredClone\").writable]);
t(\"no arguments\", () => structuredClone());
t(\"undefined\", () => [structuredClone(undefined), structuredClone(null)]);
t(\"shared reference kept\", () => { const s = { n: 1 }; const c = structuredClone({ a: s, b: s, list: [s] }); return [c.a === c.b, c.list[0] === c.a, c.a !== s]; });
t(\"cycle\", () => { const a = { name: \"a\" }; a.self = a; const m = new Map(); m.set(\"me\", m); const c = structuredClone({ a, m }); return [c.a.self === c.a, c.m.get(\"me\") === c.m]; });
t(\"class instance loses its class\", () => { class P { constructor() { this.x = 1; } m() {} } const c = structuredClone(new P()); return [Object.getPrototypeOf(c) === Object.prototype, c.x, c.m]; });
t(\"getter is read once, becomes data\", () => { let reads = 0; const c = structuredClone({ get g() { reads++; return 7; } }); return [reads, Object.getOwnPropertyDescriptor(c, \"g\")]; });
t(\"getter throws\", () => structuredClone({ get g() { throw new RangeError(\"from getter\"); } }));
t(\"key order\", () => Object.keys(structuredClone({ b: 1, 2: 1, a: 1, 1: 1 })));
t(\"non-enumerable and symbol keys dropped\", () => { const o = { kept: 1, [Symbol(\"s\")]: 2 }; Object.defineProperty(o, \"hidden\", { value: 3 }); return Reflect.ownKeys(structuredClone(o)); });
t(\"array holes and extras\", () => { const a = [1, , 3]; a.extra = \"x\"; const c = structuredClone(a); return [c.length, 1 in c, c.extra, Array.isArray(c)]; });
t(\"date\", () => { const c = structuredClone(new Date(5)); return [c instanceof Date, c.getTime(), structuredClone(new Date(NaN)).getTime()]; });
t(\"regexp\", () => { const r = /a(b)/giy; r.lastIndex = 2; const c = structuredClone(r); return [c.source, c.flags, c.lastIndex, c instanceof RegExp]; });
t(\"boxed\", () => [new Number(1), new String(\"s\"), new Boolean(true), Object(1n)].map((b) => { const c = structuredClone(b); return [typeof c, Object.prototype.toString.call(c), c.valueOf() === b.valueOf() || typeof c.valueOf()]; }));
t(\"boxed symbol\", () => structuredClone(Object(Symbol(\"s\"))));
t(\"array buffer\", () => { const b = new Uint8Array([1, 2, 3]).buffer; const c = structuredClone(b); new Uint8Array(c)[0] = 9; return [c instanceof ArrayBuffer, c.byteLength, new Uint8Array(b)[0], new Uint8Array(c)[0]]; });
t(\"resizable array buffer\", () => { const b = new ArrayBuffer(2, { maxByteLength: 8 }); const c = structuredClone(b); return [c.resizable, c.byteLength, c.maxByteLength]; });
t(\"shared array buffer\", () => { const s = new SharedArrayBuffer(2); const c = structuredClone(s); new Uint8Array(c)[0] = 5; return [c instanceof SharedArrayBuffer, new Uint8Array(s)[0]]; });
t(\"views share one cloned buffer\", () => { const b = new ArrayBuffer(8); const c = structuredClone({ a: new Uint8Array(b, 1, 3), d: new DataView(b, 2), b }); return [c.a.buffer === c.b, c.d.buffer === c.b, c.a.byteOffset, c.a.length, c.d.byteOffset, c.d.byteLength, c.b !== b]; });
t(\"buffer becomes Uint8Array\", () => { const c = structuredClone(Buffer.from(\"hi\")); return [Buffer.isBuffer(c), c.constructor.name, [...c]]; });
t(\"length-tracking view\", () => { const b = new ArrayBuffer(4, { maxByteLength: 8 }); const c = structuredClone(new Uint8Array(b)); c.buffer.resize(6); return c.length; });
t(\"map and set\", () => { const k = { key: 1 }; const c = structuredClone(new Map([[k, new Set([k])]])); const [ck, cs] = [...c][0]; return [c instanceof Map, cs instanceof Set, cs.has(ck), ck !== k]; });
t(\"map subclass and extras\", () => { class M extends Map {} const m = new M([[1, 2]]); m.extra = 1; const c = structuredClone(m); return [c.constructor.name, c.extra, c.get(1)]; });
for (const E of [Error, EvalError, RangeError, ReferenceError, SyntaxError, TypeError, URIError]) t(\"error \" + E.name, () => { const e = new E(\"m\"); e.stack = \"S\"; e.extra = 1; const c = structuredClone(e); return [c instanceof E, Object.getPrototypeOf(c) === E.prototype, c.name, c.message, c.stack, c.extra, Object.keys(c)]; });
t(\"error name\", () => [\"CustomName\", \"TypeError\", 5, undefined].map((n) => { const e = new RangeError(\"m\"); e.name = n; e.stack = \"S\"; const c = structuredClone(e); return [c.name, Object.getPrototypeOf(c).constructor.name]; }));
t(\"error without message\", () => { const e = new Error(); e.stack = \"S\"; const c = structuredClone(e); return [Object.hasOwn(c, \"message\"), c.message]; });
t(\"error cause\", () => { const e = new Error(\"m\", { cause: { deep: [1] } }); e.stack = \"S\"; const c = structuredClone(e); return [c.cause, c.cause !== e.cause]; });
t(\"aggregate error\", () => { const e = new AggregateError([1], \"agg\"); e.stack = \"S\"; const c = structuredClone(e); return [c.constructor.name, c.message, c.errors]; });
t(\"error subclass\", () => { class My extends TypeError {} const e = new My(\"m\"); e.stack = \"S\"; const c = structuredClone(e); return [c.constructor.name, c.name]; });
t(\"DOMException\", () => { const c = structuredClone(new DOMException(\"gone\", \"AbortError\")); return [c instanceof DOMException, c.name, c.message, c.code]; });
for (const [label, make] of [[\"symbol\", () => Symbol(\"s\")], [\"promise\", () => Promise.resolve()], [\"weakmap\", () => new WeakMap()], [\"weakset\", () => new WeakSet()], [\"weakref\", () => new WeakRef({})], [\"proxy\", () => new Proxy({}, {})], [\"generator\", () => (function* () {})()], [\"map iterator\", () => new Map().keys()], [\"nested symbol\", () => ({ a: [Symbol(\"in\")] })], [\"event target\", () => new EventTarget()], [\"abort signal\", () => new AbortController().signal], [\"url\", () => new URL(\"http://a/\")], [\"text encoder\", () => new TextEncoder()], [\"arguments\", () => (function () { return arguments; })(1, 2)], [\"global\", () => globalThis], [\"math\", () => Math], [\"json\", () => JSON], [\"detached buffer\", () => { const b = new ArrayBuffer(2); b.transfer(); return b; }], [\"view on detached\", () => { const b = new ArrayBuffer(2); const v = new Uint8Array(b); b.transfer(); return v; }]]) t(\"refuses \" + label, () => { try { return [\"cloned\", structuredClone(make())]; } catch (e) { return [e.constructor.name, e.name, e.code, e instanceof DOMException]; } });
for (const [label, make] of [[\"function\", () => function named() {}], [\"arrow\", () => () => 1], [\"class\", () => class K {}], [\"native function\", () => Math.max], [\"bound\", () => (() => 1).bind(null)]]) t(\"refuses \" + label, () => { try { return [\"cloned\", structuredClone(make())]; } catch (e) { return [e.constructor.name, e.name, e.code]; } });
t(\"messages\", () => [Symbol(\"s\"), new WeakMap(), Promise.resolve(), new Proxy({}, {}), new (class Foo { constructor() { return new Proxy(this, {}); } })()].map((v) => { try { structuredClone(v); return \"cloned\"; } catch (e) { return e.message; } }));
t(\"transfer\", () => { const b = new Uint8Array([1, 2, 3]).buffer; const c = structuredClone({ b, v: new Uint8Array(b, 1) }, { transfer: [b] }); return [b.byteLength, b.detached, c.b.byteLength, [...c.v], c.v.buffer === c.b]; });
t(\"transfer not in the value\", () => { const b = new ArrayBuffer(2); const c = structuredClone(1, { transfer: [b] }); return [c, b.detached]; });
t(\"transfer twice\", () => { const b = new ArrayBuffer(2); try { structuredClone(b, { transfer: [b, b] }); } catch (e) { return [e.name, b.detached]; } });
t(\"transfer a detached buffer\", () => { const b = new ArrayBuffer(2); b.transfer(); try { structuredClone(1, { transfer: [b] }); } catch (e) { return e.name; } });
t(\"transfer a shared buffer\", () => { try { structuredClone(1, { transfer: [new SharedArrayBuffer(1)] }); } catch (e) { return e.name; } });
t(\"transfer a plain object\", () => { try { structuredClone(1, { transfer: [{}] }); } catch (e) { return e.name; } });
t(\"transfer not a list\", () => { try { return structuredClone(1, { transfer: 5 }); } catch (e) { return [e.constructor.name, e.name, e.code]; } });
t(\"options not an object\", () => { try { return structuredClone(1, 5); } catch (e) { return [e.constructor.name, e.name, e.code]; } });
t(\"options null and empty\", () => [structuredClone(1, null), structuredClone(2, {}), structuredClone(3, undefined), structuredClone(4, { transfer: undefined })]);
t(\"failed clone transfers nothing\", () => { const b = new ArrayBuffer(2); try { structuredClone({ b, f() {} }, { transfer: [b] }); } catch (e) { return [e.name, b.detached]; } });
t(\"deep nesting\", () => { let v = 1; for (let i = 0; i < 400; i++) v = [v]; let c = structuredClone(v), depth = 0; while (Array.isArray(c)) { c = c[0]; depth++; } return depth; });
t(\"big array\", () => { const c = structuredClone(Array.from({ length: 5000 }, (_, i) => ({ i }))); return [c.length, c[4999].i]; });
const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"G \") ? \"generated\" : line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);

"

// 300 generated values (nested arrays, objects, maps and sets holding dates,
// regexps, buffers, views, errors, shared and circular references) cloned
// and checked to be deep copies, then one case for each kind, each refusal
// and each transfer rule. Each line is a kind of case, how many, and a hash
// of what they printed in node 26.11.1
pub fn structured_clone_matches_node_test() {
  let rt = run_commonjs(node_runtime(), structured_clone_script)
  assert out_lines(rt)
    == [
      "generated 300 3531375872",
      "shape 1 1902906066",
      "no 1 1612814149",
      "undefined 1 3039114596",
      "shared 2 3303679564",
      "cycle 1 201779211",
      "class 1 2523920055",
      "getter 2 471174620",
      "key 1 2299253694",
      "non-enumerable 1 22970972",
      "array 2 328745305",
      "date 1 3772044935",
      "regexp 1 2543213309",
      "boxed 2 3204686228",
      "resizable 1 3945033435",
      "views 1 786687885",
      "buffer 1 3119321695",
      "length-tracking 1 364435268",
      "map 2 1663906942",
      "error 11 3580913138",
      "aggregate 1 3495156515",
      "DOMException 1 2342203299",
      "refuses 24 2773901605",
      "messages 1 2691342779",
      "transfer 7 182286258",
      "options 2 4021613208",
      "failed 1 2645989278",
      "deep 1 1352721305",
      "big 1 2764326404",
    ]
}

const fs_reads_script = "
globalThis.__fsBase = '/tree';
const fs = require(\"fs\");
const fsp = require(\"fs/promises\");
const process = require(\"process\");
const { pathToFileURL } = require(\"url\");
const base = globalThis.__fsBase ?? process.env.FS_BASE;
// the cli's disk answers in whole seconds; the tests' made-up disk and node do not
const coarse = Boolean(process.env.FS_COARSE);
const lines = [];
const norm = (s) => String(s).split(base).join(\"<base>\");
const show = (v) => norm(JSON.stringify(v, (k, x) => (typeof x === \"bigint\" ? x + \"n\" : x)) ?? String(v));
const fail = (e) => (e && e.name ? [\"threw\", e.name, e.code, e.errno, e.syscall, e.path === undefined ? undefined : norm(e.path), norm(e.message)] : [\"threw\", String(e)]);
const call = (f) => { try { return show(f()); } catch (e) { return show(fail(e)); } };
const t = (label, f) => lines.push(label + \" \" + call(f));
const at = (name) => (name === \"\" ? base : base + \"/\" + name);
const kinds = (s) => [\"isFile\", \"isDirectory\", \"isSymbolicLink\", \"isFIFO\", \"isSocket\", \"isBlockDevice\", \"isCharacterDevice\"].filter((m) => s[m]()).join(\"+\");
const time = (ms) => (coarse ? Math.floor(Number(ms) / 1000) : ms);
const statLine = (s) => (s === undefined ? \"undefined\" : [kinds(s), (Number(s.mode) & 0o777).toString(8), s.isDirectory() ? \"-\" : s.size, time(s.mtimeMs), s.mtime instanceof Date, coarse ? \"-\" : s.mtime.getTime(), s.constructor.name]);
const names = [\"\", \"a.txt\", \"empty\", \"caf\\u00e9.txt\", \"sub\", \"sub/inner.bin\", \"sub/deep/leaf.txt\", \"link\", \"dirlink\", \"dirlink/inner.bin\", \"broken\", \"nope\", \"a.txt/under\", \"sub/nope/deeper\"];
for (const name of names) {
  t(\"statSync \" + name, () => statLine(fs.statSync(at(name))));
  t(\"lstatSync \" + name, () => statLine(fs.lstatSync(at(name))));
  t(\"statSync quiet \" + name, () => statLine(fs.statSync(at(name), { throwIfNoEntry: false })));
  t(\"lstatSync quiet \" + name, () => statLine(fs.lstatSync(at(name), { throwIfNoEntry: false })));
  t(\"readlinkSync \" + name, () => fs.readlinkSync(at(name)));
  t(\"readdirSync \" + name, () => fs.readdirSync(at(name)).sort());
  t(\"readFileSync \" + name, () => fs.readFileSync(at(name)).toString(\"hex\"));
  t(\"existsSync \" + name, () => fs.existsSync(at(name)));
}
// ---- stats
t(\"stats keys\", () => Object.keys(fs.statSync(at(\"a.txt\"))));
t(\"stats shape\", () => { const s = fs.statSync(at(\"a.txt\")); return [s instanceof fs.Stats, typeof s.dev, typeof s.ino, typeof s.nlink, typeof s.uid, typeof s.gid, typeof s.blksize, typeof s.blocks, typeof s.atimeMs, typeof s.birthtimeMs, Object.getOwnPropertyNames(Object.getPrototypeOf(s)).sort(), Object.getOwnPropertyNames(Object.getPrototypeOf(Object.getPrototypeOf(s))).sort()]; });
t(\"stats dates are made on first read\", () => { const s = fs.statSync(at(\"a.txt\")); const before = Object.hasOwn(s, \"mtime\"); const d = s.mtime; return [before, Object.hasOwn(s, \"mtime\"), s.mtime === d, Object.keys(s).includes(\"mtime\")]; });
t(\"stats date can be set\", () => { const s = fs.statSync(at(\"a.txt\")); s.atime = 5; return s.atime; });
t(\"stats bigint\", () => { const s = fs.statSync(at(\"a.txt\"), { bigint: true }); return [s.constructor.name, typeof s.size, s.size, typeof s.mode, kinds(s), typeof s.mtimeMs, typeof s.mtimeNs, coarse ? \"-\" : s.mtimeNs, coarse ? \"-\" : s.mtimeMs, s.mtime instanceof Date, Object.keys(s)]; });
t(\"stats instants\", () => { const s = fs.statSync(at(\"a.txt\")); return [typeof s.mtimeInstant, coarse ? \"-\" : String(s.mtimeInstant), coarse ? \"-\" : String(fs.statSync(at(\"a.txt\"), { bigint: true }).mtimeInstant)]; });
t(\"lstat bigint link\", () => { const s = fs.lstatSync(at(\"link\"), { bigint: true }); return [kinds(s), s.size]; });
t(\"stat options null\", () => { try { return statLine(fs.statSync(at(\"a.txt\"), null)); } catch (e) { return e.name; } });
t(\"stat bad path\", () => fs.statSync(5));
t(\"stat null byte\", () => fs.statSync(\"a\\u0000b\"));
t(\"stat URL path\", () => statLine(fs.statSync(pathToFileURL(at(\"a.txt\")))));
t(\"stat Buffer path\", () => statLine(fs.statSync(Buffer.from(at(\"a.txt\")))));
t(\"stat Uint8Array path\", () => statLine(fs.statSync(new TextEncoder().encode(at(\"sub\")))));
t(\"stat no path\", () => fs.statSync());
// ---- readlink
t(\"readlink buffer\", () => { const b = fs.readlinkSync(at(\"link\"), \"buffer\"); return [Buffer.isBuffer(b), b.toString(\"hex\")]; });
t(\"readlink hex\", () => fs.readlinkSync(at(\"dirlink\"), { encoding: \"hex\" }));
t(\"readlink base64\", () => fs.readlinkSync(at(\"broken\"), \"base64\"));
t(\"readlink bad encoding\", () => fs.readlinkSync(at(\"link\"), \"nope\"));
t(\"readlink bad options\", () => fs.readlinkSync(at(\"link\"), 5));
t(\"readlink bad path\", () => fs.readlinkSync({}));
// ---- readdir
const dirent = (d) => [d.name, norm(d.parentPath), kinds(d), d.constructor.name, Object.keys(d)];
const byName = (a, b) => (a[1] + \"/\" + a[0] < b[1] + \"/\" + b[0] ? -1 : 1);
t(\"readdir withFileTypes\", () => fs.readdirSync(at(\"\"), { withFileTypes: true }).map(dirent).sort(byName));
t(\"readdir sub withFileTypes\", () => fs.readdirSync(at(\"sub\"), { withFileTypes: true }).map(dirent).sort(byName));
t(\"readdir through a link\", () => fs.readdirSync(at(\"dirlink\")).sort());
t(\"readdir recursive\", () => fs.readdirSync(at(\"\"), { recursive: true }).sort());
t(\"readdir recursive withFileTypes\", () => fs.readdirSync(at(\"\"), { recursive: true, withFileTypes: true }).map(dirent).sort(byName));
t(\"readdir recursive sub\", () => fs.readdirSync(at(\"sub\"), { recursive: true }).sort());
t(\"readdir buffer\", () => fs.readdirSync(at(\"sub\"), \"buffer\").map((b) => [Buffer.isBuffer(b), b.toString(\"hex\")]).sort());
t(\"readdir hex\", () => fs.readdirSync(at(\"sub\"), { encoding: \"hex\" }).sort());
t(\"readdir buffer withFileTypes\", () => fs.readdirSync(at(\"sub\"), { encoding: \"buffer\", withFileTypes: true }).map((d) => [Buffer.isBuffer(d.name), d.name.toString(), kinds(d)]).sort());
t(\"readdir recursive not boolean\", () => fs.readdirSync(at(\"\"), { recursive: \"yes\" }));
t(\"readdir bad encoding\", () => fs.readdirSync(at(\"\"), { encoding: \"nope\" }));
t(\"readdir dirent is a Dirent\", () => { const d = fs.readdirSync(at(\"sub\"), { withFileTypes: true })[0]; return [d instanceof fs.Dirent, Object.getOwnPropertyNames(fs.Dirent.prototype).sort()]; });
// ---- readFile
t(\"readFile utf8\", () => fs.readFileSync(at(\"a.txt\"), \"utf8\"));
t(\"readFile utf8 option\", () => fs.readFileSync(at(\"caf\\u00e9.txt\"), { encoding: \"utf8\" }));
t(\"readFile latin1\", () => fs.readFileSync(at(\"caf\\u00e9.txt\"), \"latin1\"));
t(\"readFile hex\", () => fs.readFileSync(at(\"sub/inner.bin\"), \"hex\"));
t(\"readFile base64\", () => fs.readFileSync(at(\"sub/inner.bin\"), { encoding: \"base64\" }));
t(\"readFile is a Buffer\", () => { const b = fs.readFileSync(at(\"sub/inner.bin\")); return [Buffer.isBuffer(b), b.length, b[3]]; });
t(\"readFile empty\", () => [fs.readFileSync(at(\"empty\")).length, fs.readFileSync(at(\"empty\"), \"utf8\")]);
t(\"readFile through a link\", () => fs.readFileSync(at(\"link\"), \"utf8\"));
t(\"readFile through a dir link\", () => fs.readFileSync(at(\"dirlink/deep/leaf.txt\"), \"utf8\"));
t(\"readFile URL\", () => fs.readFileSync(pathToFileURL(at(\"a.txt\")), \"utf8\"));
t(\"readFile Buffer path\", () => fs.readFileSync(Buffer.from(at(\"a.txt\")), \"utf8\"));
t(\"readFile flag r\", () => fs.readFileSync(at(\"a.txt\"), { encoding: \"utf8\", flag: \"r\" }));
t(\"readFile bad flag\", () => fs.readFileSync(at(\"a.txt\"), { flag: \"zz\" }));
t(\"readFile bad encoding\", () => fs.readFileSync(at(\"a.txt\"), \"nope\"));
t(\"readFile bad options\", () => fs.readFileSync(at(\"a.txt\"), 5));
t(\"readFile bad path\", () => fs.readFileSync({}));
t(\"readFile null path\", () => fs.readFileSync(null));
t(\"readFile null byte\", () => fs.readFileSync(\"a\\u0000b\"));
// ---- realpath, now with encodings
t(\"realpath\", () => [fs.realpathSync(at(\"dirlink/deep\")), fs.realpathSync(at(\"link\")), fs.realpathSync.native(at(\"dirlink\"))]);
t(\"realpath buffer\", () => { const b = fs.realpathSync(at(\"link\"), \"buffer\"); return [Buffer.isBuffer(b), norm(b.toString())]; });
t(\"realpath bad encoding\", () => fs.realpathSync(at(\"link\"), \"nope\"));
// ---- callbacks need a function
for (const name of [\"stat\", \"lstat\", \"readlink\", \"readdir\", \"readFile\"]) t(name + \" without a callback\", () => fs[name](at(\"a.txt\")));
t(\"shape\", () => [fs.promises === fsp, [\"stat\", \"statSync\", \"lstat\", \"lstatSync\", \"readlink\", \"readlinkSync\", \"readdir\", \"readdirSync\", \"readFile\", \"readFileSync\", \"Stats\", \"Dirent\"].map((k) => typeof fs[k]), [\"stat\", \"lstat\", \"readlink\", \"readdir\", \"readFile\"].map((k) => typeof fsp[k]), fs.constants.S_IFMT, fs.constants.S_IFDIR, fs.constants.UV_DIRENT_DIR, fs.statSync.length, fs.lstat.length, fs.readFile.length, fs.readdirSync.length]);
// ---- the callback and promise forms
const later = [];
const settle = (label, promise) => later.push(promise.then((v) => label + \" \" + show(v), (e) => label + \" \" + show(fail(e))));
const cb = (run) => new Promise((resolve, reject) => run((err, value) => (err ? reject(err) : resolve(value))));
for (const name of [\"a.txt\", \"sub\", \"link\", \"broken\", \"nope\"]) {
  settle(\"stat cb \" + name, cb((done) => fs.stat(at(name), done)).then(statLine));
  settle(\"lstat cb \" + name, cb((done) => fs.lstat(at(name), done)).then(statLine));
  settle(\"stat promise \" + name, fsp.stat(at(name)).then(statLine));
  settle(\"lstat promise \" + name, fsp.lstat(at(name)).then(statLine));
  settle(\"readlink cb \" + name, cb((done) => fs.readlink(at(name), done)));
  settle(\"readlink promise \" + name, fsp.readlink(at(name)));
  settle(\"readdir cb \" + name, cb((done) => fs.readdir(at(name), done)).then((l) => l.sort()));
  settle(\"readdir promise \" + name, fsp.readdir(at(name)).then((l) => l.sort()));
  settle(\"readFile cb \" + name, cb((done) => fs.readFile(at(name), done)).then((b) => b.toString(\"hex\")));
  settle(\"readFile promise \" + name, fsp.readFile(at(name), \"utf8\"));
}
settle(\"stat cb options\", cb((done) => fs.stat(at(\"a.txt\"), { bigint: true }, done)).then((s) => [s.constructor.name, s.size]));
settle(\"stat cb quiet\", cb((done) => fs.stat(at(\"nope\"), { throwIfNoEntry: false }, done)).then((s) => String(s)));
settle(\"stat promise bigint\", fsp.stat(at(\"a.txt\"), { bigint: true }).then((s) => [s.constructor.name, s.size]));
settle(\"stat promise bad options\", (async () => fsp.stat(at(\"a.txt\"), 5))());
settle(\"readlink cb buffer\", cb((done) => fs.readlink(at(\"link\"), \"buffer\", done)).then((b) => b.toString(\"hex\")));
settle(\"readdir cb withFileTypes\", cb((done) => fs.readdir(at(\"sub\"), { withFileTypes: true }, done)).then((l) => l.map(dirent).sort(byName)));
settle(\"readdir cb recursive\", cb((done) => fs.readdir(at(\"\"), { recursive: true }, done)).then((l) => l.sort()));
settle(\"readdir promise recursive withFileTypes\", fsp.readdir(at(\"sub\"), { recursive: true, withFileTypes: true }).then((l) => l.map(dirent).sort(byName)));
settle(\"readFile cb utf8\", cb((done) => fs.readFile(at(\"a.txt\"), \"utf8\", done)));
settle(\"readFile cb options\", cb((done) => fs.readFile(at(\"sub/inner.bin\"), { encoding: \"base64\" }, done)));
settle(\"readFile cb dir\", cb((done) => fs.readFile(at(\"sub\"), done)));
settle(\"readFile promise dir\", fsp.readFile(at(\"sub\")));
settle(\"readFile promise buffer\", fsp.readFile(at(\"sub/inner.bin\")).then((b) => [Buffer.isBuffer(b), b.toString(\"hex\")]));
settle(\"readFile cb aborted\", cb((done) => fs.readFile(at(\"a.txt\"), { signal: AbortSignal.abort() }, done)).catch((e) => [e.name, e.code]));
settle(\"readFile promise aborted\", fsp.readFile(at(\"a.txt\"), { signal: AbortSignal.abort() }).catch((e) => [e.name, e.code]));
settle(\"stat cb aborted\", cb((done) => fs.stat(at(\"a.txt\"), { signal: AbortSignal.abort() }, done)).catch((e) => [e.name, e.code]));
settle(\"readFile cb bad signal\", (async () => cb((done) => fs.readFile(at(\"a.txt\"), { signal: 5 }, done)))());
Promise.all(later).then((settled) => {
for (const l of settled) lines.push(l);
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
globalThis.out.push(\"finished\");
});

"

// stat, lstat, readlink, readdir and readFile in their sync, callback and
// promise forms over the made-up tree: every path in it, missing ones, paths
// through links and through files, each option and each refusal. Each line
// is a kind of case, how many, and a hash of what they printed in node
// 26.11.1 reading a real folder of the same shape
pub fn fs_reads_match_node_test() {
  let rt = run_answering(run_commonjs(node_runtime(), fs_reads_script))
  assert out_lines(rt)
    == [
      "statSync 28 969246112",
      "lstatSync 28 1107365029",
      "readlinkSync 14 2318279608",
      "readdirSync 14 2262184512",
      "readFileSync 14 4202652315",
      "existsSync 14 2071884175",
      "stats 6 4010812161",
      "lstat 12 133708780",
      "stat 23 2665285904",
      "readlink 18 3735817974",
      "readdir 26 3852217585",
      "readFile 37 3387705820",
      "realpath 3 414019862",
      "shape 1 2156372516",
      "finished",
    ]
}

pub fn a_file_read_is_a_want_the_embedder_answers_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "const fs = require('fs');
      fs.readFile('/tree/a.txt', 'utf8', (error, text) => out.push('later: ' + text.trim()));
      fs.promises.readdir('/tree/sub').then((names) => out.push('later: ' + names.sort()));
      out.push('now: ' + fs.readFileSync('/tree/a.txt', 'utf8').trim());",
    )
  assert out_lines(rt) == ["now: hello"]
  let assert [#(first, _), #(second, _)] as asked = kept()
  assert [first, second] == [ReadFile("/tree/a.txt"), Readdir("/tree/sub")]
  let rt =
    list.fold(asked, rt, fn(rt, one) {
      spark.answer(rt, one.1, fake_answer(one.0)).1
    })
  assert out_lines(run_to_end(rt))
    == ["now: hello", "later: hello", "later: deep,inner.bin"]
}

const zlib_script = "
globalThis.__zlibSamples = [[\"deflate text\",\"inflateSync\",\"789c2b4ecc2dc849552849ad28d15128c6c1d1c32933aa6c54d9a8b25165a3ca86993200e6db3457\"],[\"deflate bin\",\"inflateSync\",\"789c6390b78bad99bdebe60f71b3d092c99b2e7ee0d7f3cde95e75f205bb9a6b4af3a2830ffecbd9c654cfda79e3bb986948f1a48d17def3e9fa6477ad3cf19c4dd525b969e181fbff646da2ab66eeb8fe4dd424b868e286f3ef7875bcb33a571c7fc6aae29cd4b860ffbdbf32d6519533b65ffb2a621c543861fdb9b73cda5e991dcb8f3d6551764a6c98bfefee1f69abc88ae9dbae7e11360a2ce85f77f60db7966746fbb2a34f98951c13eae7edbdf35bca32a27cdad62b9f850c03f2fbd69e79cda5e991deb6f4c863264587f8bab97b6eff92b4082f9bbae5f2274103ffbcde35a75f716ab8a7b52e39fc8851c13eae76ceee5b3f25ccc34aa76cbef45140df2fb767f5a9971cea6ea92d8b0f3d6418f5ff90f53f0001995ca6\"],[\"deflate empty\",\"inflateSync\",\"789c030000000001\"],[\"deflate9 text\",\"inflateSync\",\"78da2b4ecc2dc849552849ad28d15128c6c1d1c32933aa6c54d9a8b25165a3ca86993200e6db3457\"],[\"deflate9 bin\",\"inflateSync\",\"78da6390b78bad99bdebe60f71b3d092c99b2e7ee0d7f3cde95e75f205bb9a6b4af3a2830ffecbd9c654cfda79e3bb986948f1a48d17def3e9fa6477ad3cf19c4dd525b969e181fbff646da2ab66eeb8fe4dd424b868e286f3ef7875bcb33a571c7fc6aae29cd4b860ffbdbf32d6519533b65ffb2a621c543861fdb9b73cda5e991dcb8f3d6551764a6c98bfefee1f69abc88ae9dbae7e11360a2ce85f77f60db7966746fbb2a34f98951c13eae7edbdf35bca32a27cdad62b9f850c03f2fbd69e79cda5e991deb6f4c863264587f8bab97b6eff92b4082f9bbae5f2274103ffbcde35a75f716ab8a7b52e39fc8851c13eae76ceee5b3f25ccc34aa76cbef45140df2fb767f5a9971cea6ea92d8b0f3d6418f5ff90f53f0001995ca6\"],[\"deflate9 empty\",\"inflateSync\",\"78da030000000001\"],[\"deflate0 text\",\"inflateSync\",\"7801011806e7f973616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e2073616d706c6520746578742c2073616d706c6520746578742c2073616d706c6520746578742e20e6db3457\"],[\"deflate0 bin\",\"inflateSync\",\"780101bc0243fd001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a5c4e30221405f7e9dbcdbfa1938577695b4d3f211304f6e8daccbea0928476685a4c3e201203f5e7d9cbbdaf91837567594b3d2f1102f4e6d8cabcae90827466584a3c2e1001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a5c4e30221405f7e9dbcdbfa1938577695b4d3f211304f6e8daccbea0928476685a4c3e201203f5e7d9cbbdaf91837567594b3d2f1102f4e6d8cabcae90827466584a3c2e1001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a501995ca6\"],[\"deflate0 empty\",\"inflateSync\",\"7801010000ffff00000001\"],[\"gzip text\",\"gunzipSync\",\"1f8b08000000000000132b4ecc2dc849552849ad28d15128c6c1d1c32933aa6c54d9a8b25165a3ca869932000f26a63918060000\"],[\"gzip bin\",\"gunzipSync\",\"1f8b08000000000000136390b78bad99bdebe60f71b3d092c99b2e7ee0d7f3cde95e75f205bb9a6b4af3a2830ffecbd9c654cfda79e3bb986948f1a48d17def3e9fa6477ad3cf19c4dd525b969e181fbff646da2ab66eeb8fe4dd424b868e286f3ef7875bcb33a571c7fc6aae29cd4b860ffbdbf32d6519533b65ffb2a621c543861fdb9b73cda5e991dcb8f3d6551764a6c98bfefee1f69abc88ae9dbae7e11360a2ce85f77f60db7966746fbb2a34f98951c13eae7edbdf35bca32a27cdad62b9f850c03f2fbd69e79cda5e991deb6f4c863264587f8bab97b6eff92b4082f9bbae5f2274103ffbcde35a75f716ab8a7b52e39fc8851c13eae76ceee5b3f25ccc34aa76cbef45140df2fb767f5a9971cea6ea92d8b0f3d6418f5ff90f53f007f2f6a1cbc020000\"],[\"gzip empty\",\"gunzipSync\",\"1f8b080000000000001303000000000000000000\"],[\"raw text\",\"inflateRawSync\",\"2b4ecc2dc849552849ad28d15128c6c1d1c32933aa6c54d9a8b25165a3ca86993200\"],[\"raw bin\",\"inflateRawSync\",\"6390b78bad99bdebe60f71b3d092c99b2e7ee0d7f3cde95e75f205bb9a6b4af3a2830ffecbd9c654cfda79e3bb986948f1a48d17def3e9fa6477ad3cf19c4dd525b969e181fbff646da2ab66eeb8fe4dd424b868e286f3ef7875bcb33a571c7fc6aae29cd4b860ffbdbf32d6519533b65ffb2a621c543861fdb9b73cda5e991dcb8f3d6551764a6c98bfefee1f69abc88ae9dbae7e11360a2ce85f77f60db7966746fbb2a34f98951c13eae7edbdf35bca32a27cdad62b9f850c03f2fbd69e79cda5e991deb6f4c863264587f8bab97b6eff92b4082f9bbae5f2274103ffbcde35a75f716ab8a7b52e39fc8851c13eae76ceee5b3f25ccc34aa76cbef45140df2fb767f5a9971cea6ea92d8b0f3d6418f5ff90f53f00\"],[\"raw empty\",\"inflateRawSync\",\"0300\"],[\"wbits9 text\",\"inflateSync\",\"18952b4ecc2dc849552849ad28d15128c6c1d1c32933aa6c54d9a8b25165a3ca86993200e6db3457\"],[\"wbits9 bin\",\"inflateSync\",\"189501bc0243fd001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a5c4e30221405f7e9dbcdbfa1938577695b4d3f211304f6e8daccbea0928476685a4c3e201203f5e7d9cbbdaf91837567594b3d2f1102f4e6d8cabcae90827466584a3c2e1001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a5c4e30221405f7e9dbcdbfa1938577695b4d3f211304f6e8daccbea0928476685a4c3e201203f5e7d9cbbdaf91837567594b3d2f1102f4e6d8cabcae90827466584a3c2e1001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a501995ca6\"],[\"wbits9 empty\",\"inflateSync\",\"1895030000000001\"],[\"huffman text\",\"inflateSync\",\"780105c1b10d00400802c05518c0fc4e14749a9848c1f87f779c6dc18a0bc7d916acb8709c6dc18a1f8eb32d5871e138db8215178eb32d58f1c371b6052b2e1c675bb0e2c271b6052b7e38ceb660c585e36c0b565c38ceb660c50fc7d916acb8709c6dc18a0bc7d916acf8e138db8215178eb32d5871e138db82153f1c675bb0e2c271b6052b2e1c675bb0e287e36c0b565c38ceb660c585e36c0b56fc709c6dc18a0bc7d916acb8709c6dc18a1f8eb32d5871e138db8215178eb32d58f1c371b6052b2e1c675bb0e2c271b6052b7e38ceb660c585e36c0b565c38ceb660c50fc7d916acb8709c6dc18a0bc7d916acf8e138db8215178eb32d5871e138db82153f1c675bb0e2c271b6052b2e1c675bb0e287e36c0b565c38ceb660c585e36c0b56fc709c6dc18a0bc7d916acb8709c6dc18a1f8eb32d5871e138db8215178eb32d58f1c371b6052b2e1c675bb0e2c271b6052b7e38ceb660c585e36c0b565c38ceb660c50fc7d916acb8709c6dc18a0bc7d916acf8e138db8215178eb32d5871e138db82153f1c675bb0e2c271b6052b2e1c675bb0e287e36c0b565c38ceb660c585e36c0b56fc709c6dc18a0bc7d916acb8709c6dc18a1f8eb32d5871e138db8215178eb32d58f1c371b6052b2e1c675bb0e2c271b6052b7e38ceb660c585e36c0b565c38ceb660c50fc7d916acb8709c6dc18a0bc7d916acf8e138db8215178eb32d5871e138db82153f1c675bb0e2c271b6052b2e1c675bb0e287e36c0b565c38ceb660c585e36c0b56fc709c6dc18a0bc7d916acb8709c6dc18a1f8eb32d5871e138db8215178eb32d58f1c371b6052b2e1c675bb0e2c271b6052b7e38ceb660c585e36c0b565c38ceb660c50fc7d916acb8709c6dc18a0bc7d916acf8e138db8215178eb32d5871e138db82153f1c675bb0e2c271b6052b2e1c675bb0e287e36c0b565c38ceb660c585e36c0b56fcf001e6db3457\"],[\"huffman bin\",\"inflateSync\",\"780101bc0243fd001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a5c4e30221405f7e9dbcdbfa1938577695b4d3f211304f6e8daccbea0928476685a4c3e201203f5e7d9cbbdaf91837567594b3d2f1102f4e6d8cabcae90827466584a3c2e1001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a5c4e30221405f7e9dbcdbfa1938577695b4d3f211304f6e8daccbea0928476685a4c3e201203f5e7d9cbbdaf91837567594b3d2f1102f4e6d8cabcae90827466584a3c2e1001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a501995ca6\"],[\"huffman empty\",\"inflateSync\",\"7801030000000001\"],[\"zstd text\",\"zstdDecompressSync\",\"28b52ffd6018054d0200d2450a10c025c301c02cc0628afe0f4ae1abc301d2b66ddbb66ddbb66ddbb66ddbb66d93a39821130c0240044fa810f0fb1be0e711bc61f8133ffbffd7fffff5ff7ffdffd7870630a5\"],[\"zstd bin\",\"zstdDecompressSync\",\"28b52ffd60bc015508000410001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a5c4e30221405f7e9dbcdbfa1938577695b4d3f211304f6e8daccbea0928476685a4c3e201203f5e7d9cbbdaf91837567594b3d2f1102f4e6d8cabcae90827466584a3c2e1010000b903ea9a02\"],[\"zstd empty\",\"zstdDecompressSync\",\"28b52ffd2000010000\"],[\"zstd19 text\",\"zstdDecompressSync\",\"28b52ffd601805cd00007873616d706c6520746578742c202e200200ee29f56300ce2f\"],[\"zstd19 bin\",\"zstdDecompressSync\",\"28b52ffd60bc015508000410001f3e5d7c9bbad9f81736557493b2d1f00f2e4d6c8baac9e80726456483a2c1e0ff1e3d5c7b9ab9d8f71635547392b1d0ef0e2d4c6b8aa9c8e70625446382a1c0dffe1d3c5b7a99b8d7f61534537291b0cfee0d2c4b6a89a8c7e60524436281a0bfdefd1c3b5a7998b7d6f51433527190afceed0c2b4a6988a7c6e504234261809fbeddfc1b3a597897b6d5f4133251708faecdec0b2a496887a6c5e4032241607f9ebddcfb1a39587796b5d4f31231506f8eadcceb0a29486786a5c4e30221405f7e9dbcdbfa1938577695b4d3f211304f6e8daccbea0928476685a4c3e201203f5e7d9cbbdaf91837567594b3d2f1102f4e6d8cabcae90827466584a3c2e1010000b903ea9a02\"],[\"zstd19 empty\",\"zstdDecompressSync\",\"28b52ffd2000010000\"],[\"unzip gzip text\",\"unzipSync\",\"1f8b08000000000000132b4ecc2dc849552849ad28d15128c6c1d1c32933aa6c54d9a8b25165a3ca869932000f26a63918060000\"],[\"unzip gzip bin\",\"unzipSync\",\"1f8b08000000000000136390b78bad99bdebe60f71b3d092c99b2e7ee0d7f3cde95e75f205bb9a6b4af3a2830ffecbd9c654cfda79e3bb986948f1a48d17def3e9fa6477ad3cf19c4dd525b969e181fbff646da2ab66eeb8fe4dd424b868e286f3ef7875bcb33a571c7fc6aae29cd4b860ffbdbf32d6519533b65ffb2a621c543861fdb9b73cda5e991dcb8f3d6551764a6c98bfefee1f69abc88ae9dbae7e11360a2ce85f77f60db7966746fbb2a34f98951c13eae7edbdf35bca32a27cdad62b9f850c03f2fbd69e79cda5e991deb6f4c863264587f8bab97b6eff92b4082f9bbae5f2274103ffbcde35a75f716ab8a7b52e39fc8851c13eae76ceee5b3f25ccc34aa76cbef45140df2fb767f5a9971cea6ea92d8b0f3d6418f5ff90f53f007f2f6a1cbc020000\"],[\"unzip gzip empty\",\"unzipSync\",\"1f8b080000000000001303000000000000000000\"]];
const zlib = require(\"zlib\");
const process = require(\"process\");
let seed = 31337;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const show = (v) => (Buffer.isBuffer(v) ? \"buf:\" + v.length + \":\" + (v.length <= 24 ? v.toString(\"hex\") : zlib.crc32(v)) : JSON.stringify(v) ?? String(v));
const call = (f) => { try { return show(f()); } catch (e) { return \"threw \" + e.constructor.name + \" \" + e.code + \" \" + e.errno + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f));
// data that compresses, data that does not, text, and nothing
function data() {
  const n = pick([0, 1, 5, 64, 300, 5000, 40000]);
  const kind = rnd(4);
  if (kind === 0) return Buffer.alloc(n, rnd(256));
  if (kind === 1) return Buffer.from(Array.from({ length: n }, () => rnd(256)));
  if (kind === 2) return Buffer.from(\"the quick brown fox jumps over the lazy dog. \".repeat(Math.ceil(n / 45)).slice(0, n));
  return Buffer.from(Array.from({ length: n }, (_, i) => (i * 7) % 251));
}
const pairs = [[\"deflateSync\", \"inflateSync\"], [\"gzipSync\", \"gunzipSync\"], [\"deflateRawSync\", \"inflateRawSync\"], [\"deflateSync\", \"unzipSync\"], [\"gzipSync\", \"unzipSync\"], [\"zstdCompressSync\", \"zstdDecompressSync\"]];
// ---- what node compressed, this must read; what this compresses must read back the same
for (let i = 0; i < 150; i++) {
  const input = data();
  const [pack, unpack] = pick(pairs);
  const zstd = pack.startsWith(\"zstd\");
  const options = zstd ? pick([undefined, { params: { [zlib.constants.ZSTD_c_compressionLevel]: pick([1, 3, 9, 19]) } }]) : pick([undefined, { level: pick([-1, 0, 1, 6, 9]) }, { memLevel: pick([1, 8, 9]) }, { strategy: pick([0, 1, 2, 3]) }, { windowBits: pick([9, 12, 15]) }, { chunkSize: pick([64, 1024]) }]);
  lines.push(\"R \" + i + \" \" + pack + \" \" + JSON.stringify(options) + \" \" + input.length + \" \" + call(() => { const packed = zlib[pack](input, options); const back = zlib[unpack](packed, zstd || !options?.windowBits ? undefined : options); return [back.equals(input), back.length, packed.length <= input.length + 64]; }));
}
// ---- bytes node made, kept here, so reading them is checked against node's own output
const samples = process.env.ZLIB_SAMPLES ? JSON.parse(process.env.ZLIB_SAMPLES) : null;
if (globalThis.__zlibSamples || samples) {
  for (const [name, unpack, hex, options] of globalThis.__zlibSamples ?? samples) t(\"sample \" + name, () => zlib[unpack](Buffer.from(hex, \"hex\"), options));
}
// ---- broken input
const good = { inflateSync: zlib.deflateSync(\"hello world \".repeat(50)), gunzipSync: zlib.gzipSync(\"hello world \".repeat(50)), inflateRawSync: zlib.deflateRawSync(\"hello world \".repeat(50)), zstdDecompressSync: zlib.zstdCompressSync(\"hello world \".repeat(50)) };
for (const [name, packed] of Object.entries(good)) {
  t(\"cut short \" + name, () => zlib[name](packed.subarray(0, packed.length - 6)));
  t(\"cut to header \" + name, () => zlib[name](packed.subarray(0, 2)));
  t(\"empty \" + name, () => zlib[name](Buffer.alloc(0)));
  t(\"not it \" + name, () => zlib[name](Buffer.from([1, 2, 3, 4, 5, 6, 7, 8])));
  t(\"junk after \" + name, () => zlib[name](Buffer.concat([packed, Buffer.from(\"junk\")])).length);
  t(\"string in \" + name, () => zlib[name](\"not compressed at all\"));
}
t(\"cut short but sync flush\", () => zlib.inflateSync(good.inflateSync.subarray(0, good.inflateSync.length - 6), { finishFlush: zlib.constants.Z_SYNC_FLUSH }).length);
t(\"gunzip a deflate\", () => zlib.gunzipSync(good.inflateSync));
t(\"inflate a gzip\", () => zlib.inflateSync(good.gunzipSync));
t(\"two gzip members\", () => zlib.gunzipSync(Buffer.concat([zlib.gzipSync(\"one\"), zlib.gzipSync(\"two\")])).toString());
// ---- options and arguments
t(\"input kinds\", () => [zlib.inflateSync(zlib.deflateSync(\"text\")).toString(), zlib.inflateSync(zlib.deflateSync(new Uint8Array([1, 2]))).toString(\"hex\"), zlib.inflateSync(zlib.deflateSync(new Uint16Array([513]))).toString(\"hex\"), zlib.inflateSync(zlib.deflateSync(new DataView(new ArrayBuffer(2)))).toString(\"hex\"), zlib.inflateSync(zlib.deflateSync(new ArrayBuffer(3))).length]);
for (const bad of [5, null, undefined, {}, []]) t(\"bad input \" + JSON.stringify(bad), () => zlib.deflateSync(bad));
for (const [name, value] of [[\"level\", 10], [\"level\", -2], [\"level\", \"9\"], [\"windowBits\", 7], [\"windowBits\", 16], [\"memLevel\", 0], [\"memLevel\", 10], [\"strategy\", 5], [\"chunkSize\", 10], [\"chunkSize\", \"x\"], [\"flush\", 99], [\"finishFlush\", -1], [\"maxOutputLength\", 0], [\"dictionary\", \"text\"]]) t(\"bad option \" + name + \"=\" + value, () => zlib.deflateSync(\"x\", { [name]: value }));
t(\"windowBits 8 raw\", () => call(() => zlib.deflateRawSync(\"x\", { windowBits: 8 }).length >= 0));
t(\"gzip windowBits 8\", () => zlib.gzipSync(\"x\", { windowBits: 8 }));
t(\"maxOutputLength\", () => zlib.inflateSync(zlib.deflateSync(Buffer.alloc(5000)), { maxOutputLength: 100 }));
t(\"dictionary\", () => { const dictionary = Buffer.from(\"hello world\"); const packed = zlib.deflateSync(\"hello world hello\", { dictionary }); return [zlib.inflateSync(packed, { dictionary }).toString(), call(() => zlib.inflateSync(packed))]; });
t(\"info option\", () => { const r = zlib.deflateSync(\"x\", { info: true }); return [Object.keys(r), Buffer.isBuffer(r.buffer), r.engine.constructor.name]; });
t(\"zstd bad param\", () => zlib.zstdCompressSync(\"x\", { params: { 99999: 1 } }));
t(\"zstd param type\", () => zlib.zstdCompressSync(\"x\", { params: { [zlib.constants.ZSTD_c_compressionLevel]: \"3\" } }));
t(\"crc32\", () => [zlib.crc32(\"hello\"), zlib.crc32(Buffer.from(\"hello\")), zlib.crc32(\"world\", zlib.crc32(\"hello \")), zlib.crc32(\"hello world\"), zlib.crc32(\"\"), zlib.crc32(new Uint8Array([255]))]);
t(\"crc32 bad\", () => zlib.crc32(5));
t(\"crc32 bad value\", () => zlib.crc32(\"x\", -1));
// ---- shape
t(\"keys\", () => Object.keys(zlib));
t(\"constants\", () => [Object.keys(zlib.constants).length, zlib.constants.Z_BEST_COMPRESSION, zlib.constants.Z_SYNC_FLUSH, zlib.constants.ZSTD_c_compressionLevel, zlib.constants.BROTLI_OPERATION_FLUSH, zlib.constants.Z_MAX_CHUNK, Object.isFrozen(zlib.constants), zlib.codes.Z_DATA_ERROR, zlib.codes[\"-3\"]]);
t(\"classes\", () => [\"Deflate\", \"Inflate\", \"Gzip\", \"Gunzip\", \"DeflateRaw\", \"InflateRaw\", \"Unzip\", \"ZstdCompress\", \"ZstdDecompress\"].map((n) => { const s = n.startsWith(\"Zstd\") ? new zlib[n]() : zlib[\"create\" + n](); return [s instanceof zlib[n], s instanceof require(\"stream\").Transform, s.bytesWritten, typeof s.flush, typeof s.close, typeof s.params, typeof s.reset]; }));
t(\"without new\", () => { try { return zlib.Deflate() instanceof zlib.Deflate; } catch (e) { return e.name; } });
// ---- callbacks and streams, in a fixed order
const later = [];
const settle = (label, promise) => later.push(promise.then((v) => label + \" \" + show(v), (e) => label + \" threw \" + e.constructor.name + \" \" + e.code + \" \" + e.errno + \" \" + e.message));
const cb = (run) => new Promise((resolve, reject) => run((err, value) => (err ? reject(err) : resolve(value))));
for (const [pack, unpack] of [[\"deflate\", \"inflate\"], [\"gzip\", \"gunzip\"], [\"deflateRaw\", \"inflateRaw\"], [\"zstdCompress\", \"zstdDecompress\"]]) {
  settle(\"callback \" + pack, cb((done) => zlib[pack](\"called back \".repeat(30), done)).then((packed) => cb((done) => zlib[unpack](packed, done))).then((b) => b.toString().length));
  settle(\"callback bad \" + unpack, cb((done) => zlib[unpack](Buffer.from([9, 9, 9, 9]), done)));
  settle(\"callback options \" + pack, cb((done) => zlib[pack](\"x\", {}, done)).then((b) => Buffer.isBuffer(b)));
}
settle(\"callback missing\", (async () => zlib.gzip(\"x\"))());
settle(\"promisified\", require(\"util\").promisify(zlib.gzip)(\"promised\").then((b) => zlib.gunzipSync(b).toString()));
function through(streamOf, chunks, flushAt) {
  return new Promise((resolve, reject) => {
    const s = streamOf(); const got = [];
    s.on(\"data\", (c) => got.push(c)); s.on(\"error\", reject); s.on(\"end\", () => resolve(Buffer.concat(got)));
    chunks.forEach((c, i) => { s.write(c); if (i === flushAt) s.flush(); });
    s.end();
  });
}
const big = Buffer.from(\"stream me, \".repeat(3000));
const cut = (b, n) => Array.from({ length: Math.ceil(b.length / n) }, (_, i) => b.subarray(i * n, i * n + n));
for (const [make, unmake] of [[\"createDeflate\", \"createInflate\"], [\"createGzip\", \"createGunzip\"], [\"createDeflateRaw\", \"createInflateRaw\"], [\"createGzip\", \"createUnzip\"], [\"createZstdCompress\", \"createZstdDecompress\"]]) {
  settle(\"stream \" + make, through(() => zlib[make](), cut(big, 1000)).then((packed) => through(() => zlib[unmake](), cut(packed, 17))).then((back) => [back.equals(big), back.length]));
  settle(\"stream small chunks \" + make, through(() => zlib[make]({ chunkSize: 64 }), cut(big.subarray(0, 4000), 100), 3).then((packed) => zlib[unmake.replace(\"create\", \"\").replace(/^./, (c) => c.toLowerCase()) + \"Sync\"](packed).length));
}
settle(\"stream bad data\", through(() => zlib.createGunzip(), [Buffer.from(\"not gzip at all\")]));
settle(\"stream cut short\", through(() => zlib.createInflate(), [good.inflateSync.subarray(0, 10)]));
settle(\"stream bytesWritten\", new Promise((resolve) => { const s = zlib.createGzip(); s.on(\"data\", () => {}); s.on(\"end\", () => resolve(s.bytesWritten)); s.end(\"twelve bytes\"); }));
settle(\"stream pipeline\", new Promise((resolve, reject) => { const { pipeline, Readable, Writable } = require(\"stream\"); const got = []; pipeline(Readable.from([Buffer.from(\"piped \"), Buffer.from(\"through\")]), zlib.createGzip(), zlib.createGunzip(), new Writable({ write(c, e, done) { got.push(c); done(); } }), (err) => (err ? reject(err) : resolve(Buffer.concat(got).toString()))); }));
settle(\"stream sync flush gives output early\", new Promise((resolve) => { const s = zlib.createDeflate(); const seen = []; s.on(\"data\", (c) => seen.push(c.length)); s.write(\"some text to flush\"); s.flush(zlib.constants.Z_SYNC_FLUSH, () => { const early = seen.length > 0; s.end(); s.on(\"end\", () => resolve([early, seen.length >= 1])); }); }));
settle(\"inflate gives output before the end\", new Promise((resolve) => { const packed = zlib.deflateSync(Buffer.from(\"x\".repeat(20000) + \"y\".repeat(20000)), { level: 0 }); const s = zlib.createInflate(); let early = 0; s.on(\"data\", (c) => (early += c.length)); s.write(packed.subarray(0, 25000), () => { const before = early; s.end(packed.subarray(25000)); s.on(\"end\", () => resolve([before > 0, early])); }); }));
Promise.all(later).then((settled) => {
for (const l of settled) lines.push(l);
const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"R \") ? \"round trips generated\" : line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
globalThis.out.push(\"finished\");
});

"

// 150 generated round trips through deflate, gzip, raw deflate and zstd with
// generated options; bytes node 26.11.1 compressed, read back; input cut
// short, wrong, or followed by junk; each option's refusal; the callback
// forms; and the streams, written in pieces with flushes. Each line is a
// kind of case, how many, and a hash of what they printed in node
pub fn zlib_matches_node_test() {
  let rt = run_to_end(run_commonjs(node_runtime(), zlib_script))
  assert out_lines(rt)
    == [
      "round trips generated 150 3922020097",
      "sample 30 841759710",
      "cut 9 727091289",
      "empty 4 1969415492",
      "not 4 4113714169",
      "junk 4 2716881168",
      "string 4 507665161",
      "gunzip 1 304146370",
      "inflate 2 1504577396",
      "two 1 1881970902",
      "input 1 4232513262",
      "bad 19 1218179417",
      "windowBits 1 444615441",
      "gzip 1 3719611897",
      "maxOutputLength 1 120807908",
      "dictionary 1 1740993089",
      "info 1 2044062039",
      "zstd 2 1007315431",
      "crc32 3 3211120290",
      "keys 1 3862148594",
      "constants 1 3905368743",
      "classes 1 1805346879",
      "without 1 4242919027",
      "callback 13 2731326444",
      "promisified 1 2211623251",
      "stream 15 1689510410",
      "finished",
    ]
}

pub fn brotli_says_it_is_not_here_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "const zlib = require('zlib');
      try { zlib.brotliCompressSync('x'); } catch (error) { out.push(error.code); }
      try { zlib.createBrotliDecompress(); } catch (error) { out.push(error.code); }",
    )
  assert out_lines(rt)
    == ["ERR_METHOD_NOT_IMPLEMENTED", "ERR_METHOD_NOT_IMPLEMENTED"]
}

pub fn the_user_comes_from_the_embedder_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "const os = require('os');
      out.push(JSON.stringify(os.userInfo()));
      const bytes = os.userInfo({ encoding: 'buffer' });
      out.push([Buffer.isBuffer(bytes.username), bytes.shell.toString(), bytes.uid].join());
      out.push(typeof os.userInfo(5).uid);",
    )
  assert out_lines(rt)
    == [
      "{\"uid\":1001,\"gid\":100,\"username\":\"glenda\",\"homedir\":\"/usr/glenda\",\"shell\":\"/bin/rc\"}",
      "true,/bin/rc,1001",
      "number",
    ]
}

pub fn a_terminal_is_one_the_embedder_named_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "const tty = require('tty');
      out.push([0, 1, 2, -1, 1.5, '1', undefined].map((fd) => tty.isatty(fd)).join());
      try { new tty.WriteStream(1); } catch (error) { out.push(error.code); }
      out.push(Object.keys(tty).join());",
    )
  assert out_lines(rt)
    == [
      "false,true,false,false,false,false,false",
      "ERR_METHOD_NOT_IMPLEMENTED",
      "isatty,ReadStream,WriteStream",
    ]
}

pub fn certificates_come_from_the_embedder_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "const tls = require('tls');
      const names = (list) => list.map((pem) => pem.split('\\n')[1]).join();
      out.push(names(tls.rootCertificates));
      for (const type of [undefined, 'default', 'bundled', 'system', 'extra']) out.push(String(type) + ': ' + names(tls.getCACertificates(type)));
      out.push([Object.isFrozen(tls.rootCertificates), tls.getCACertificates('bundled') === tls.rootCertificates, tls.getCACertificates() === tls.getCACertificates('default')].join());
      for (const bad of ['nope', 5]) { try { tls.getCACertificates(bad); } catch (error) { out.push(error.code + ' ' + error.message); } }
      try { tls.connect(443, 'example.test'); } catch (error) { out.push(error.code); }
      out.push([tls.DEFAULT_MIN_VERSION, tls.DEFAULT_MAX_VERSION, tls.DEFAULT_ECDH_CURVE, tls.CLIENT_RENEG_LIMIT, tls.CLIENT_RENEG_WINDOW].join());",
    )
  assert out_lines(rt)
    == [
      "bundled one",
      "undefined: bundled one,extra one",
      "default: bundled one,extra one",
      "bundled: bundled one",
      "system: system one",
      "extra: extra one",
      "true,true,true",
      "ERR_INVALID_ARG_VALUE The argument 'type' is invalid. Received 'nope'",
      "ERR_INVALID_ARG_TYPE The \"type\" argument must be of type string. Received type number (5)",
      "ERR_METHOD_NOT_IMPLEMENTED",
      "TLSv1.2,TLSv1.3,auto,3,600",
    ]
}

pub fn util_types_is_the_types_of_util_test() {
  let rt =
    run_commonjs(
      node_runtime(),
      "out.push(String(require('util/types') === require('util').types));
      out.push(String(require('node:util/types').isPromise(Promise.resolve())));",
    )
  assert out_lines(rt) == ["true", "true"]
}

const web_streams_match_node_script = "
const web = require(\"stream/web\");
let seed = 1337;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const show = (v) => { if (v instanceof Uint8Array) return \"u8:\" + Array.from(v).join(\".\"); if (v instanceof Error) return v.name + \":\" + v.code + \":\" + v.message; try { return JSON.stringify(v) ?? String(v); } catch { return String(v); } };
const call = (f) => { try { return show(f()); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f));
const pending = [];
const scenario = (label, run) => pending.push(async () => { const log = []; try { await run((...p) => log.push(p.map((x) => (typeof x === \"string\" ? x : show(x))).join(\":\"))); } catch (e) { log.push(\"rejected \" + e.name + \" \" + e.code + \" \" + e.message); } lines.push(label + \" \" + log.join(\" \")); });
const tick = () => new Promise((r) => setImmediate(r));
// ---- generated: a source that enqueues, closes or errors on a plan; a reader that reads, cancels or releases on a plan
for (let i = 0; i < 60; i++) {
  const hwm = pick([0, 1, 2, 5]);
  const steps = Array.from({ length: 1 + rnd(6) }, () => pick([\"chunk\", \"chunk\", \"chunk\", \"wait\", \"close\", \"error\"]));
  const reads = Array.from({ length: 2 + rnd(6) }, () => pick([\"read\", \"read\", \"read\", \"desired\", \"cancel\", \"release\", \"tee\", \"locked\"]));
  const sized = rnd(3) === 0;
  scenario(\"G \" + i + \" \" + [hwm, sized, steps.join(\"/\"), reads.join(\"/\")].join(\" \"), async (log) => {
    let n = 0, at = 0, controller;
    const stream = new ReadableStream({ start(c) { controller = c; log(\"start\", c.desiredSize); }, async pull(c) { const step = steps[at++] ?? \"close\"; log(\"pull\", step); if (step === \"chunk\") c.enqueue(\"c\" + n++); else if (step === \"wait\") await tick(); else if (step === \"close\") c.close(); else c.error(new RangeError(\"planned\")); }, cancel(reason) { log(\"cancelled\", String(reason)); } }, sized ? new CountQueuingStrategy({ highWaterMark: hwm }) : { highWaterMark: hwm, size: (chunk) => chunk.length });
    let reader = stream.getReader(), other;
    for (const op of reads) {
      try {
        if (op === \"read\") { const r = await Promise.race([reader.read(), tick().then(tick).then(tick).then(() => \"slow\")]); log(\"read\", r === \"slow\" ? \"slow\" : r); }
        else if (op === \"desired\") log(\"desired\", controller.desiredSize);
        else if (op === \"cancel\") log(\"cancel\", await reader.cancel(\"why\"));
        else if (op === \"release\") { reader.releaseLock(); log(\"released\", stream.locked); reader = stream.getReader(); }
        else if (op === \"locked\") log(\"locked\", stream.locked);
        else if (op === \"tee\" && !other) { reader.releaseLock(); const [a, b] = stream.tee(); reader = a.getReader(); other = b.getReader(); log(\"tee\", stream.locked); }
      } catch (e) { log(op, \"threw\", e.name, e.message); }
    }
    if (other) log(\"other\", await Promise.race([other.read(), tick().then(tick).then(() => \"slow\")]).catch((e) => \"rejected \" + e.name));
    await tick();
  });
}
// ---- generated: writes into a sink with a plan, with backpressure
for (let i = 0; i < 40; i++) {
  const hwm = pick([0, 1, 3]);
  const ops = Array.from({ length: 2 + rnd(7) }, () => pick([\"write\", \"write\", \"write\", \"ready\", \"desired\", \"close\", \"abort\", \"release\"]));
  const failAt = rnd(4) === 0 ? rnd(3) : -1;
  const slow = rnd(2) === 0;
  scenario(\"W \" + i + \" \" + [hwm, failAt, slow, ops.join(\"/\")].join(\" \"), async (log) => {
    let seen = 0;
    const stream = new WritableStream({ start() { log(\"start\"); }, async write(chunk) { if (slow) await tick(); if (seen++ === failAt) throw new TypeError(\"sink failed\"); log(\"sink\", chunk); }, close() { log(\"sink closed\"); }, abort(reason) { log(\"sink aborted\", String(reason)); } }, { highWaterMark: hwm });
    let writer = stream.getWriter(), n = 0; const waits = [];
    writer.closed.then(() => log(\"closed\"), (e) => log(\"closed rejected\", e && e.name));
    for (const op of ops) {
      try {
        if (op === \"write\") waits.push(writer.write(\"w\" + n++).then(() => \"ok\", (e) => \"rejected \" + (e && e.name)));
        else if (op === \"ready\") log(\"ready\", await Promise.race([writer.ready.then(() => \"yes\", (e) => \"rejected \" + (e && e.name)), tick().then(() => \"no\")]));
        else if (op === \"desired\") log(\"desired\", writer.desiredSize);
        else if (op === \"close\") waits.push(writer.close().then(() => \"closed ok\", (e) => \"close rejected \" + (e && e.name)));
        else if (op === \"abort\") waits.push(writer.abort(\"stop\").then(() => \"abort ok\", (e) => \"abort rejected \" + (e && e.name)));
        else if (op === \"release\") { writer.releaseLock(); log(\"released\", stream.locked); writer = stream.getWriter(); }
      } catch (e) { log(op, \"threw\", e.name, e.message); }
    }
    log(\"results\", await Promise.all(waits));
    await tick();
  });
}
// ---- one case each
t(\"exports\", () => Object.keys(web));
t(\"globals are the exports\", () => Object.keys(web).every((k) => globalThis[k] === web[k]));
t(\"descriptors\", () => [\"ReadableStream\", \"TextDecoderStream\", \"queueMicrotask\"].map((k) => { const d = Object.getOwnPropertyDescriptor(globalThis, k); return [d.enumerable, d.writable, d.configurable]; }));
t(\"shape\", () => [ReadableStream.name, ReadableStream.length, Object.getOwnPropertyNames(ReadableStream.prototype).sort(), Object.getOwnPropertyNames(ReadableStream).sort(), Object.prototype.toString.call(new ReadableStream()), Object.getOwnPropertyNames(WritableStream.prototype).sort(), Object.getOwnPropertyNames(TransformStream.prototype).sort()]);
t(\"without new\", () => ReadableStream());
t(\"bad source\", () => new ReadableStream(5));
t(\"bad type\", () => new ReadableStream({ type: \"nope\" }));
t(\"bad strategy\", () => new ReadableStream({}, { highWaterMark: -1 }));
t(\"bad size\", () => new ReadableStream({}, { size: 5 }));
t(\"bytes with size\", () => new ReadableStream({ type: \"bytes\" }, { size: () => 1 }));
t(\"locked twice\", () => { const s = new ReadableStream(); s.getReader(); return s.getReader(); });
t(\"byob on a default stream\", () => new ReadableStream().getReader({ mode: \"byob\" }));
t(\"bad reader mode\", () => new ReadableStream().getReader({ mode: \"x\" }));
t(\"reader wrong this\", () => ReadableStreamDefaultReader.prototype.read.call({}).catch((e) => e.code).constructor.name);
t(\"strategies\", () => { const c = new CountQueuingStrategy({ highWaterMark: 4 }), b = new ByteLengthQueuingStrategy({ highWaterMark: 16 }); return [c.highWaterMark, c.size(), c.size.name, b.highWaterMark, b.size(new Uint8Array(3)), b.size.name, call(() => new CountQueuingStrategy()), call(() => new CountQueuingStrategy({}).highWaterMark)]; });
t(\"inspect\", () => require(\"util\").inspect(new ReadableStream()) + \" | \" + require(\"util\").inspect(new WritableStream()) + \" | \" + require(\"util\").inspect(new TransformStream(), { depth: 0 }) + \" | \" + require(\"util\").inspect(new CountQueuingStrategy({ highWaterMark: 1 })));
t(\"structuredClone refuses a stream\", () => { try { structuredClone(new ReadableStream()); return \"cloned\"; } catch (e) { return e.name; } });
t(\"from\", () => typeof ReadableStream.from);
t(\"encoder streams shape\", () => { const e = new TextEncoderStream(), d = new TextDecoderStream(\"utf-8\", { fatal: true, ignoreBOM: true }); return [e.encoding, d.encoding, d.fatal, d.ignoreBOM, typeof e.readable.getReader, typeof d.writable.getWriter, call(() => new TextDecoderStream(\"nope\"))]; });
t(\"compression formats\", () => [\"gzip\", \"deflate\", \"deflate-raw\", \"zstd\", \"nope\"].map((f) => { try { new CompressionStream(f); return f + \":ok\"; } catch (e) { return f + \":\" + e.code; } }));
// ---- behaviours
scenario(\"S from an iterable\", async (log) => { for await (const c of ReadableStream.from([\"a\", \"b\"])) log(c); for await (const c of ReadableStream.from((async function* () { yield 1; yield 2; })())) log(c); });
scenario(\"S async iteration with break cancels\", async (log) => { const s = new ReadableStream({ start(c) { c.enqueue(1); c.enqueue(2); }, cancel(r) { log(\"cancel\", String(r)); } }); for await (const c of s) { log(c); break; } log(\"locked\", s.locked); });
scenario(\"S values preventCancel\", async (log) => { const s = new ReadableStream({ start(c) { c.enqueue(1); c.enqueue(2); c.close(); }, cancel() { log(\"cancel\"); } }); for await (const c of s.values({ preventCancel: true })) { log(c); break; } log(\"locked\", s.locked); log((await s.getReader().read()).value); });
scenario(\"S byob reads\", async (log) => { let n = 0; const s = new ReadableStream({ type: \"bytes\", pull(c) { if (c.byobRequest) { const v = c.byobRequest.view; v[0] = 65 + n++; c.byobRequest.respond(1); } else c.enqueue(new Uint8Array([90])); if (n > 2) c.close(); }, autoAllocateChunkSize: 4 }); const r = s.getReader({ mode: \"byob\" }); for (let i = 0; i < 4; i++) { const { value, done } = await r.read(new Uint8Array(3)); log(done, value ? Array.from(value) : value, value ? value.byteLength : \"-\"); } });
scenario(\"S byte stream default reader\", async (log) => { const s = new ReadableStream({ type: \"bytes\", start(c) { c.enqueue(new Uint8Array([1, 2])); c.enqueue(new Uint16Array([513])); c.close(); } }); for await (const c of s) log(c.constructor.name, Array.from(c)); });
scenario(\"S byte stream bad chunk\", async (log) => { new ReadableStream({ type: \"bytes\", start(c) { try { c.enqueue(\"text\"); } catch (e) { log(e.name, e.code); } try { c.enqueue(new Uint8Array(0)); } catch (e) { log(e.name, e.code); } } }); });
scenario(\"S pipeTo options\", async (log) => { for (const options of [{}, { preventClose: true }, { preventAbort: true }, { preventCancel: true }]) { const sink = []; const ws = new WritableStream({ write(c) { sink.push(c); }, close() { sink.push(\"closed\"); }, abort(r) { sink.push(\"aborted \" + String(r && r.message)); } }); const fail = options.preventAbort !== undefined; const rs = new ReadableStream({ start(c) { c.enqueue(\"a\"); if (fail) c.error(new Error(\"source broke\")); else c.close(); } }); await rs.pipeTo(ws, options).then(() => log(\"done\"), (e) => log(\"rejected\", e.message)); log(JSON.stringify(options), sink); } });
scenario(\"S pipeTo a failing sink cancels the source\", async (log) => { const rs = new ReadableStream({ pull(c) { c.enqueue(\"x\"); }, cancel(r) { log(\"source cancelled\", r && r.message); } }); await rs.pipeTo(new WritableStream({ write() { throw new Error(\"sink broke\"); } })).catch((e) => log(\"rejected\", e.message)); });
scenario(\"S pipeTo with a signal\", async (log) => { const c = new AbortController(); const rs = new ReadableStream({ pull(k) { k.enqueue(\"x\"); return tick(); }, cancel(r) { log(\"cancelled\", r && r.name); } }); const p = rs.pipeTo(new WritableStream({ abort(r) { log(\"aborted\", r && r.name); } }), { signal: c.signal }); await tick(); c.abort(); await p.catch((e) => log(\"rejected\", e.name, e.code)); });
scenario(\"S pipeTo locked\", async (log) => { const rs = new ReadableStream(); rs.getReader(); await rs.pipeTo(new WritableStream()).catch((e) => log(e.name, e.code)); const ws = new WritableStream(); ws.getWriter(); await new ReadableStream().pipeTo(ws).catch((e) => log(e.name, e.code)); });
scenario(\"S pipeThrough chain\", async (log) => { const double = () => new TransformStream({ transform(c, k) { k.enqueue(c); k.enqueue(c); } }); const got = []; for await (const c of ReadableStream.from([\"a\", \"b\"]).pipeThrough(double()).pipeThrough(double())) got.push(c); log(got.join(\"\")); });
scenario(\"S transform flush and errors\", async (log) => { const ts = new TransformStream({ start(c) { c.enqueue(\"first\"); }, transform(c, k) { if (c === \"bad\") throw new RangeError(\"no\"); k.enqueue(c + \"!\"); }, flush(k) { k.enqueue(\"last\"); } }); const w = ts.writable.getWriter(); const r = ts.readable.getReader(); w.write(\"a\"); w.close(); for (;;) { const { value, done } = await r.read(); if (done) break; log(value); } const ts2 = new TransformStream({ transform() { throw new RangeError(\"no\"); } }); const w2 = ts2.writable.getWriter(); w2.write(\"bad\").catch((e) => log(\"write rejected\", e.name)); await ts2.readable.getReader().read().catch((e) => log(\"read rejected\", e.name)); });
scenario(\"S transform backpressure\", async (log) => { const ts = new TransformStream({}, { highWaterMark: 1 }, { highWaterMark: 0 }); const w = ts.writable.getWriter(); log(\"desired\", w.desiredSize); const first = w.write(\"a\"); log(\"after write\", w.desiredSize); const r = ts.readable.getReader(); log((await r.read()).value); await first; log(\"drained\", w.desiredSize); });
scenario(\"S transform terminate\", async (log) => { const ts = new TransformStream({ transform(c, k) { k.enqueue(c); k.terminate(); } }); const w = ts.writable.getWriter(); w.write(\"only\").catch(() => {}); w.write(\"never\").catch((e) => log(\"second rejected\", e.name)); const got = []; for await (const c of ts.readable) got.push(c); log(got); await tick(); });
scenario(\"S tee is independent\", async (log) => { const [a, b] = ReadableStream.from([1, 2, 3]).tee(); const ra = a.getReader(); log((await ra.read()).value); ra.cancel(); const rest = []; for await (const c of b) rest.push(c); log(rest); });
scenario(\"S tee of bytes\", async (log) => { const [a, b] = new ReadableStream({ type: \"bytes\", start(c) { c.enqueue(new Uint8Array([7, 8])); c.close(); } }).tee(); const x = (await a.getReader().read()).value, y = (await b.getReader().read()).value; log(Array.from(x), Array.from(y), x.buffer !== y.buffer); });
scenario(\"S writer after error\", async (log) => { const ws = new WritableStream({ write() { throw new Error(\"boom\"); } }); const w = ws.getWriter(); await w.write(\"a\").catch((e) => log(\"write\", e.message)); await w.write(\"b\").catch((e) => log(\"again\", e.message)); await w.closed.catch((e) => log(\"closed\", e.message)); await w.close().catch((e) => log(\"close\", e.name)); });
scenario(\"S writable signal\", async (log) => { let signal; const ws = new WritableStream({ start(c) { signal = c.signal; }, write() { return new Promise(() => {}); } }); const w = ws.getWriter(); w.write(\"a\").catch(() => {}); await tick(); log(signal.aborted); w.abort(new Error(\"gone\")).catch(() => {}); await tick(); log(signal.aborted, signal.reason && signal.reason.message); });
scenario(\"S text decoder stream\", async (log) => { const parts = [new Uint8Array([0xf0, 0x9f]), new Uint8Array([0x98]), new Uint8Array([0x80, 0x61])]; const got = []; for await (const s of ReadableStream.from(parts).pipeThrough(new TextDecoderStream())) got.push(s); log(got.map((s) => Buffer.from(s).toString(\"hex\"))); const fatal = ReadableStream.from([new Uint8Array([0xff])]).pipeThrough(new TextDecoderStream(\"utf-8\", { fatal: true })); await fatal.getReader().read().catch((e) => log(\"fatal\", e.name, e.code)); });
scenario(\"S text encoder stream\", async (log) => { const got = []; for await (const b of ReadableStream.from([\"a\\u20ac\", \"b\"]).pipeThrough(new TextEncoderStream())) got.push(Array.from(b)); log(got); });
scenario(\"S compression\", async (log) => { for (const format of [\"gzip\", \"deflate\", \"deflate-raw\"]) { const packed = []; for await (const c of ReadableStream.from([new TextEncoder().encode(\"squeeze me \".repeat(30))]).pipeThrough(new CompressionStream(format))) packed.push(Buffer.from(c)); const back = []; for await (const c of ReadableStream.from([Buffer.concat(packed)]).pipeThrough(new DecompressionStream(format))) back.push(Buffer.from(c)); log(format, Buffer.concat(back).toString().length, Buffer.concat(packed).length < 330); } await ReadableStream.from([new Uint8Array([1, 2, 3])]).pipeThrough(new DecompressionStream(\"gzip\")).getReader().read().catch((e) => log(\"bad gzip\", e.name, e.code)); });
scenario(\"S node stream adapters\", async (log) => { const { Readable, Writable, Duplex } = require(\"stream\"); const viaWeb = []; for await (const c of Readable.toWeb(Readable.from([Buffer.from(\"n1\"), Buffer.from(\"n2\")]))) viaWeb.push(Buffer.from(c).toString()); log(viaWeb); const viaNode = []; for await (const c of Readable.fromWeb(ReadableStream.from([\"w1\", \"w2\"]), { objectMode: true })) viaNode.push(c); log(viaNode); const sink = []; const ww = Writable.toWeb(new Writable({ write(c, e, cb) { sink.push(String(c)); cb(); } })); const w = ww.getWriter(); await w.write(\"in\"); await w.close(); log(sink); const fromWeb = Writable.fromWeb(new WritableStream({ write(c) { sink.push(\"web:\" + String(c)); } })); await new Promise((r) => fromWeb.end(\"x\", r)); log(sink); const pair = Duplex.toWeb(new Duplex({ read() { this.push(\"d\"); this.push(null); }, write(c, e, cb) { cb(); } })); log(typeof pair.readable.getReader, typeof pair.writable.getWriter); });
scenario(\"S a reader releases and a new one continues\", async (log) => { const s = ReadableStream.from([1, 2]); const a = s.getReader(); log((await a.read()).value); a.releaseLock(); await a.read().catch((e) => log(e.name, e.code)); log((await s.getReader().read()).value); });
scenario(\"S queueMicrotask\", async (log) => { const order = []; queueMicrotask(() => order.push(\"micro\")); Promise.resolve().then(() => order.push(\"promise\")); order.push(\"sync\"); await tick(); log(order); try { queueMicrotask(5); } catch (e) { log(e.code); } });
(async () => { for (const start of pending) await start(); const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"G \") ? \"reads generated\" : line.startsWith(\"W \") ? \"writes generated\" : line.startsWith(\"S \") ? \"behaviours\" : line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
globalThis.out.push(\"finished\"); })();

"

// 150 generated plans of a source and a reader (enqueue, wait, close, error
// against read, cancel, release, tee), 80 generated plans of writes into a
// sink with backpressure and failures, and about 50 single cases: byte
// streams, pipeTo and its options, transforms, tee, the text and compression
// streams, the node stream adapters. Each line is a kind of case, how many,
// and a hash of what they printed in node 26.11.1
pub fn web_streams_match_node_test() {
  let rt =
    run_to_end(run_commonjs(node_runtime(), web_streams_match_node_script))
  assert out_lines(rt)
    == [
      "exports 1 339852181",
      "globals 1 1207816678",
      "descriptors 1 1205999827",
      "shape 1 2454301418",
      "without 1 2256888937",
      "bad 5 1297428313",
      "bytes 1 2022779013",
      "locked 1 3700155792",
      "byob 1 34615815",
      "reader 1 2862022606",
      "strategies 1 1622576742",
      "inspect 1 2640018962",
      "structuredClone 1 1357471374",
      "from 1 4105487919",
      "encoder 1 2641063473",
      "compression 1 2123825429",
      "reads generated 60 1777956438",
      "writes generated 40 3510005093",
      "behaviours 24 355039172",
      "finished",
    ]
}

const fetch_matches_node_script = "
let seed = 4242;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const show = (v) => { if (v instanceof Uint8Array) return \"u8:\" + Array.from(v).join(\".\"); if (v instanceof ArrayBuffer) return \"ab:\" + Array.from(new Uint8Array(v)).join(\".\"); if (v instanceof Error) return v.name + \":\" + v.message; try { return JSON.stringify(v) ?? String(v); } catch { return String(v); } };
const call = (f) => { try { return show(f()); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f));
const later = [];
const settle = (label, f) => later.push(async () => { try { lines.push(label + \" \" + show(await f())); } catch (e) { lines.push(label + \" rejected \" + e.name + \" \" + e.code + \" \" + (e instanceof SyntaxError ? \"\" : e.message)); } });
// ---- generated: a run of operations on one Headers, then everything it can say
const names = [\"Accept\", \"accept\", \"X-One\", \"x-one\", \"Set-Cookie\", \"set-cookie\", \"Content-Type\", \"b\", \"A\", \"a b\", \"\", \"x:y\", \"caf\\u00e9\", \"x-\\u20ac\"];
const values = [\"1\", \" padded \", \"a, b\", \"\", \"tab\\there\", \"new\\nline\", \"\\u00e9\", \"\\u20ac\", \"nul\\0\", \"\\r\\n trimmed \\t\"];
for (let i = 0; i < 100; i++) {
  const ops = Array.from({ length: 1 + rnd(7) }, () => [pick([\"append\", \"append\", \"set\", \"delete\", \"get\", \"has\", \"getSetCookie\"]), pick(names), pick(values)]);
  const init = pick([undefined, undefined, { \"X-Init\": \"i\" }, [[\"x-init\", \"1\"], [\"X-INIT\", \"2\"]], new Headers({ z: \"9\" })]);
  lines.push(\"H \" + i + \" \" + JSON.stringify(ops) + \" \" + call(() => { const h = new Headers(init); const log = []; for (const [op, name, value] of ops) { try { const r = op === \"append\" || op === \"set\" ? h[op](name, value) : op === \"getSetCookie\" ? h.getSetCookie() : h[op](name); log.push(r === undefined ? \"-\" : r); } catch (e) { log.push(\"threw \" + e.name + \": \" + e.message); } } return [log, [...h], [...h.keys()], [...h.values()]]; }));
}
// ---- Headers, one case each
for (const [label, make] of [[\"number\", () => new Headers(5)], [\"null\", () => new Headers(null)], [\"string\", () => new Headers(\"a\")], [\"pair short\", () => new Headers([[\"a\"]])], [\"pair long\", () => new Headers([[\"a\", \"b\", \"c\"]])], [\"pair not a list\", () => new Headers([5])], [\"bad name\", () => new Headers({ \"a b\": \"1\" })], [\"bad value\", () => new Headers({ a: \"x\\ny\" })], [\"map\", () => [...new Headers(new Map([[\"m\", \"1\"]]))]], [\"symbol key\", () => [...new Headers({ [Symbol(\"s\")]: \"1\", a: \"2\" })]], [\"non-enumerable\", () => [...new Headers(Object.defineProperty({ a: \"1\" }, \"hidden\", { value: \"2\", enumerable: false }))]], [\"numbers\", () => [...new Headers({ n: 5, t: true, u: undefined, z: null })]], [\"wide value\", () => new Headers({ a: \"\\u0100\" })], [\"undefined\", () => [...new Headers(undefined)]], [\"function\", () => [...new Headers(Object.assign(() => {}, { f: \"1\" }))]]]) t(\"headers init \" + label, make);
t(\"headers argument counts\", () => [\"append\", \"delete\", \"get\", \"has\", \"set\", \"forEach\"].map((m) => call(() => new Headers()[m]())).concat(call(() => new Headers().append(\"a\")), call(() => new Headers().set(\"a\"))));
t(\"headers wrong this\", () => [\"append\", \"delete\", \"get\", \"has\", \"set\", \"keys\", \"values\", \"entries\", \"forEach\", \"getSetCookie\"].map((m) => call(() => Headers.prototype[m].call({}, \"a\", \"b\"))));
t(\"headers order and joining\", () => { const h = new Headers([[\"b\", \"2\"], [\"a\", \"1\"], [\"B\", \"3\"], [\"set-cookie\", \"x=1\"], [\"Set-Cookie\", \"y=2\"], [\"c\", \"\"]]); return [[...h], h.get(\"b\"), h.get(\"set-cookie\"), h.getSetCookie(), h.get(\"c\"), h.get(\"nope\"), h.has(\"C\")]; });
t(\"headers forEach\", () => { const seen = []; const h = new Headers({ b: \"2\", a: \"1\" }); const self = {}; h.forEach(function (value, name, parent) { seen.push([value, name, parent === h, this === self]); }, self); return [seen, call(() => h.forEach(5))]; });
t(\"headers iterator\", () => { const it = new Headers({ a: \"1\" }).entries(); return [Object.prototype.toString.call(it), typeof it.next, typeof it[Symbol.iterator], it[Symbol.iterator]() === it, JSON.stringify(it.next()), JSON.stringify(it.next())]; });
t(\"headers shape\", () => [Headers.name, Headers.length, Object.prototype.toString.call(new Headers()), Object.keys(Headers.prototype), Headers.prototype[Symbol.iterator] === Headers.prototype.entries, typeof Headers.prototype.getSetCookie]);
t(\"headers inspect\", () => require(\"util\").inspect(new Headers({ b: \"2\", a: \"1\" })));
// ---- Response
t(\"response defaults\", () => { const r = new Response(); return [r.status, r.statusText, r.ok, r.type, r.url, r.redirected, r.body, r.bodyUsed, [...r.headers]]; });
for (const [label, body] of [[\"string\", \"text \\u20ac\"], [\"empty string\", \"\"], [\"u8\", new Uint8Array([1, 2, 3])], [\"u16\", new Uint16Array([258])], [\"dataview\", new DataView(new Uint8Array([7, 8]).buffer)], [\"arraybuffer\", new Uint8Array([9]).buffer], [\"params\", new URLSearchParams({ a: \"1\", b: \"x y\" })], [\"number\", 5], [\"object\", { a: 1 }], [\"null\", null], [\"undefined\", undefined], [\"boolean\", false]]) {
  t(\"response body \" + label, () => { const r = new Response(body); return [r.headers.get(\"content-type\"), r.body === null ? null : Object.prototype.toString.call(r.body), r.bodyUsed]; });
  settle(\"response text of \" + label, () => new Response(body).text());
  settle(\"response bytes of \" + label, () => new Response(body).bytes());
  settle(\"response arrayBuffer of \" + label, () => new Response(body).arrayBuffer());
}
for (const [label, init] of [[\"status 99\", { status: 99 }], [\"status 600\", { status: 600 }], [\"status 200.9\", { status: 200.9 }], [\"status '201'\", { status: \"201\" }], [\"status NaN\", { status: NaN }], [\"status 204 with body\", { status: 204 }], [\"status 304 with body\", { status: 304 }], [\"statusText ok\", { statusText: \"Fine by me\" }], [\"statusText newline\", { statusText: \"a\\nb\" }], [\"statusText wide\", { statusText: \"\\u20ac\" }], [\"headers\", { headers: { \"X-A\": \"1\" } }], [\"headers bad\", { headers: { \"a b\": \"1\" } }], [\"init number\", 5], [\"init null\", null], [\"content-type kept\", { headers: { \"content-type\": \"x/y\" } }]]) t(\"response init \" + label, () => { const r = new Response(\"body\", init); return [r.status, r.statusText, r.ok, [...r.headers]]; });
t(\"response null body statuses\", () => [101, 103, 204, 205, 304].map((s) => call(() => new Response(null, { status: s }).status)));
t(\"response error\", () => { const r = Response.error(); return [r.type, r.status, r.ok, r.body, r.statusText, call(() => r.headers.set(\"a\", \"b\"))]; });
t(\"response redirect\", () => [301, 302, 303, 307, 308, 200, 300, undefined].map((s) => call(() => { const r = Response.redirect(\"http://example.test/a b?q#h\", s); return [r.status, r.headers.get(\"location\"), r.type, call(() => r.headers.append(\"a\", \"b\"))]; })).concat(call(() => Response.redirect(\"not a url\")), call(() => Response.redirect())));
t(\"response json\", () => [call(() => { const r = Response.json({ a: [1] }); return [r.status, r.headers.get(\"content-type\")]; }), call(() => Response.json({}, { status: 201, headers: { \"content-type\": \"x/y\", extra: \"1\" } }).headers.get(\"content-type\")), call(() => Response.json(undefined)), call(() => Response.json()), call(() => Response.json(Symbol(\"s\")))]);
settle(\"response json text\", () => Response.json({ a: \"\\u20ac\", b: [1, null] }).text());
t(\"response shape\", () => [Response.name, Response.length, Object.prototype.toString.call(new Response()), Object.keys(Response.prototype), Object.keys(Response), call(() => Response()), call(() => Object.getOwnPropertyDescriptor(Response.prototype, \"status\").get.call({}))]);
t(\"response inspect\", () => require(\"util\").inspect(new Response(\"x\", { status: 201, headers: { a: \"1\" } })).replace(/ReadableStream \\{[^}]*\\}/, \"ReadableStream\"));
settle(\"response json parse\", () => new Response('{\"a\":[1,2]}').json());
settle(\"response bad json\", () => new Response(\"{\").json());
settle(\"response empty json\", () => new Response(\"\").json());
settle(\"response bom\", () => new Response(new Uint8Array([0xef, 0xbb, 0xbf, 104, 105])).text());
settle(\"response bad utf8\", () => new Response(new Uint8Array([104, 0xff, 105])).text());
settle(\"response read twice\", async () => { const r = new Response(\"x\"); await r.text(); return [r.bodyUsed, await r.text().catch((e) => e.name + \": \" + e.message), await r.json().catch((e) => e.name + \": \" + e.message), await r.arrayBuffer().catch((e) => e.name + \": \" + e.message)]; });
settle(\"response locked\", async () => { const r = new Response(\"x\"); r.body.getReader(); return [r.bodyUsed, await r.text().catch((e) => e.name + \": \" + e.message), call(() => r.clone())]; });
settle(\"response clone\", async () => { const r = new Response(\"cloned\", { status: 203, statusText: \"S\", headers: { a: \"1\" } }); const c = r.clone(); c.headers.set(\"b\", \"2\"); return [c.status, c.statusText, [...c.headers], [...r.headers], await r.text(), await c.text(), r.bodyUsed, c.bodyUsed, call(() => r.clone())]; });
settle(\"response stream body\", async () => { const r = new Response(new ReadableStream({ start(c) { c.enqueue(new Uint8Array([104])); c.enqueue(new Uint8Array([105])); c.close(); } })); return [r.headers.get(\"content-type\"), await r.text()]; });
settle(\"response stream of strings\", () => new Response(new ReadableStream({ start(c) { c.enqueue(\"text\"); c.close(); } })).text());
settle(\"response used stream\", async () => { const s = new ReadableStream({ start(c) { c.close(); } }); s.getReader(); return call(() => new Response(s)); });
settle(\"response body is the stream\", async () => { const r = new Response(\"abc\"); const parts = []; for await (const c of r.body) parts.push(Array.from(c)); return [parts, r.bodyUsed, r.body.locked]; });
settle(\"response wrong this\", () => Response.prototype.text.call({}));
// ---- Request
t(\"request defaults\", () => { const r = new Request(\"http://example.test/p?q#h\"); return [r.method, r.url, [...r.headers], r.destination, r.referrer, r.referrerPolicy, r.mode, r.credentials, r.cache, r.redirect, r.integrity, r.keepalive, r.isReloadNavigation, r.isHistoryNavigation, r.signal.aborted, r.body, r.bodyUsed, r.duplex]; });
for (const [label, input, init] of [[\"no argument\"], [\"relative\", \"/x\"], [\"credentials\", \"http://u:p@h/\"], [\"url object\", new URL(\"http://example.test/u\")], [\"number\", 5], [\"method lower\", \"http://h/\", { method: \"post\" }], [\"method patch lower\", \"http://h/\", { method: \"patch\" }], [\"method custom\", \"http://h/\", { method: \"Purge\" }], [\"method bad\", \"http://h/\", { method: \"a b\" }], [\"method trace\", \"http://h/\", { method: \"trace\" }], [\"method connect\", \"http://h/\", { method: \"CONNECT\" }], [\"get with body\", \"http://h/\", { body: \"x\" }], [\"head with body\", \"http://h/\", { method: \"HEAD\", body: \"x\" }], [\"post body\", \"http://h/\", { method: \"POST\", body: \"x\" }], [\"post params\", \"http://h/\", { method: \"POST\", body: new URLSearchParams(\"a=1\") }], [\"post bytes\", \"http://h/\", { method: \"POST\", body: new Uint8Array(2) }], [\"stream no duplex\", \"http://h/\", { method: \"POST\", body: new ReadableStream() }], [\"stream half\", \"http://h/\", { method: \"POST\", body: new ReadableStream(), duplex: \"half\" }], [\"mode bad\", \"http://h/\", { mode: \"nope\" }], [\"mode navigate\", \"http://h/\", { mode: \"navigate\" }], [\"mode ok\", \"http://h/\", { mode: \"same-origin\" }], [\"credentials bad\", \"http://h/\", { credentials: \"nope\" }], [\"cache bad\", \"http://h/\", { cache: \"nope\" }], [\"redirect bad\", \"http://h/\", { redirect: \"nope\" }], [\"redirect manual\", \"http://h/\", { redirect: \"manual\" }], [\"signal bad\", \"http://h/\", { signal: 5 }], [\"signal null\", \"http://h/\", { signal: null }], [\"window\", \"http://h/\", { window: 1 }], [\"window null\", \"http://h/\", { window: null }], [\"referrer bad\", \"http://h/\", { referrer: \"::\" }], [\"referrer empty\", \"http://h/\", { referrer: \"\" }], [\"referrer url\", \"http://h/\", { referrer: \"http://r.test/x\" }], [\"policy\", \"http://h/\", { referrerPolicy: \"origin\" }], [\"policy bad\", \"http://h/\", { referrerPolicy: \"nope\" }], [\"integrity keepalive\", \"http://h/\", { integrity: \"sha256-x\", keepalive: true }], [\"headers\", \"http://h/\", { headers: [[\"X-A\", \"1\"], [\"x-a\", \"2\"]] }], [\"init number\", \"http://h/\", 5], [\"data url\", \"data:text/plain,hi\"], [\"file url\", \"file:///tmp/x\"]]) t(\"request \" + label, () => { const r = label === \"no argument\" ? new Request() : new Request(input, init); return [r.method, r.url, [...r.headers], r.mode, r.credentials, r.cache, r.redirect, r.referrer, r.referrerPolicy, r.integrity, r.keepalive, r.body === null ? null : \"stream\"]; });
t(\"request from request\", () => { const a = new Request(\"http://h/a\", { method: \"POST\", headers: { x: \"1\" }, body: \"payload\", redirect: \"manual\", cache: \"no-store\" }); const b = new Request(a); const c = new Request(a.url, { method: \"PUT\", headers: { y: \"2\" }, body: \"z\" }); return [b.method, b.url, [...b.headers], b.redirect, b.cache, a.bodyUsed, call(() => new Request(a)), c.method, [...c.headers]]; });
t(\"request override\", () => { const a = new Request(\"http://h/a\", { method: \"POST\", headers: { x: \"1\" }, body: \"payload\" }); const b = new Request(a, { method: \"PUT\", headers: { y: \"2\" }, body: \"other\" }); return [b.method, [...b.headers], a.bodyUsed]; });
t(\"request signal follows\", () => { const c = new AbortController(); const r = new Request(\"http://h/\", { signal: c.signal }); const before = [r.signal === c.signal, r.signal.aborted]; c.abort(new Error(\"why\")); return before.concat(r.signal.aborted, r.signal.reason.message, new Request(\"http://h/\", { signal: AbortSignal.abort(\"early\") }).signal.reason); });
t(\"request shape\", () => [Request.name, Request.length, Object.prototype.toString.call(new Request(\"http://h/\")), Object.keys(Request.prototype), call(() => Request(\"http://h/\"))]);
settle(\"request body\", async () => { const r = new Request(\"http://h/\", { method: \"POST\", body: \"the body\" }); return [r.headers.get(\"content-type\"), r.bodyUsed, await r.text(), r.bodyUsed, await r.text().catch((e) => e.message)]; });
settle(\"request clone\", async () => { const r = new Request(\"http://h/\", { method: \"POST\", body: \"twice\", headers: { a: \"1\" } }); const c = r.clone(); return [c.method, c.url, [...c.headers], await r.text(), await c.text(), call(() => r.clone())]; });
settle(\"request json\", () => new Request(\"http://h/\", { method: \"PUT\", body: '{\"a\":1}' }).json());
// ---- fetch, without the network
for (const [label, input, init] of [[\"no argument\"], [\"relative\", \"/x\"], [\"ftp\", \"ftp://example.test/\"], [\"about\", \"about:blank\"], [\"bad port\", \"http://127.0.0.1:1/\"], [\"get with body\", \"http://h/\", { method: \"GET\", body: \"x\" }], [\"aborted already\", \"http://127.0.0.1:9/\", { signal: AbortSignal.abort() }], [\"aborted with reason\", \"http://127.0.0.1:9/\", { signal: AbortSignal.abort(new RangeError(\"mine\")) }], [\"bad method\", \"http://h/\", { method: \"a b\" }], [\"data text\", \"data:text/plain,hello%20there\"], [\"data base64\", \"data:text/plain;base64,aGkgdGhlcmU=\"], [\"data no type\", \"data:,plain\"], [\"data json\", \"data:application/json,%7B%22a%22%3A1%7D\"]]) settle(\"fetch \" + label, async () => { const r = await (label === \"no argument\" ? fetch() : fetch(input, init)); return [r.status, r.statusText, r.ok, r.type, r.url.slice(0, 30), r.headers.get(\"content-type\"), await r.text()]; });
settle(\"fetch failure causes\", async () => { const got = []; for (const u of [\"ftp://h/\", \"about:blank\", \"http://127.0.0.1:1/\"]) await fetch(u).catch((e) => got.push([e.name, e.message, e.cause && e.cause.name, e.cause && e.cause.message])); return got; });
t(\"fetch shape\", () => [fetch.name, fetch.length, typeof fetch(\"data:,x\").then, [\"fetch\", \"Headers\", \"Request\", \"Response\"].map((k) => { const d = Object.getOwnPropertyDescriptor(globalThis, k); return [d.enumerable, d.writable, d.configurable]; })]);
(async () => { for (const run of later) await run(); const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"H \") ? \"headers generated\" : line.split(\" \").slice(0, 2).join(\" \");
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
globalThis.out.push(\"finished\"); })();

"

// 100 generated runs of operations on a Headers, then one case each for how
// Headers, Response and Request are made, read, cloned and refused, and for
// what fetch does without a network (bad urls, schemes, ports, data: urls,
// a signal already aborted). Each line is a kind of case, how many, and a
// hash of what they printed in node 26.11.1
pub fn fetch_matches_node_test() {
  let rt = run_to_end(run_commonjs(node_runtime(), fetch_matches_node_script))
  assert out_lines(rt)
    == [
      "headers generated 100 731410379",
      "headers init 15 715682464",
      "headers argument 1 245457837",
      "headers wrong 1 3041442646",
      "headers order 1 1055220960",
      "headers forEach 1 1620045",
      "headers iterator 1 3227185568",
      "headers shape 1 722948104",
      "headers inspect 1 4150275369",
      "response defaults 1 3946628934",
      "response body 13 206542772",
      "response init 15 496581980",
      "response null 1 1906099823",
      "response error 1 3972387116",
      "response redirect 1 3646935498",
      "response json 3 1557017881",
      "response shape 1 405164925",
      "response inspect 1 1741473177",
      "request defaults 1 1849505541",
      "request no 1 2818303001",
      "request relative 1 1590429209",
      "request credentials 2 3548676665",
      "request url 1 3793449636",
      "request number 1 1625617778",
      "request method 6 781969255",
      "request get 1 596986272",
      "request head 1 1684172202",
      "request post 3 513131287",
      "request stream 2 3669092505",
      "request mode 3 3553245103",
      "request cache 1 2462306745",
      "request redirect 2 1569645793",
      "request signal 3 971020308",
      "request window 2 2962849810",
      "request referrer 3 1417987093",
      "request policy 2 246053563",
      "request integrity 1 2196596251",
      "request headers 1 915040732",
      "request init 1 3868682803",
      "request data 1 1910325676",
      "request file 1 226351212",
      "request from 1 2920417464",
      "request override 1 2459487697",
      "request shape 1 2161787438",
      "fetch shape 1 2265728610",
      "response text 12 988344555",
      "response bytes 12 590826521",
      "response arrayBuffer 12 157458773",
      "response bad 2 698163209",
      "response empty 1 552734100",
      "response bom 1 3030648809",
      "response read 1 2785008253",
      "response locked 1 449462756",
      "response clone 1 1854156145",
      "response stream 2 646289661",
      "response used 1 1885718071",
      "response wrong 1 3687331907",
      "request body 1 3904558999",
      "request clone 1 1684122491",
      "request json 1 1264796254",
      "fetch no 1 136108937",
      "fetch relative 1 265445952",
      "fetch ftp 1 3632102552",
      "fetch about 1 1212866631",
      "fetch bad 2 2055975444",
      "fetch get 1 466436593",
      "fetch aborted 2 1046782644",
      "fetch data 4 2852212154",
      "fetch failure 1 2897928826",
      "finished",
    ]
}

// the made-up network in `fake_answer`: nothing here opens a connection
pub fn a_fetch_is_wants_the_embedder_answers_test() {
  let rt =
    run_answering(run_commonjs(
      node_runtime(),
      "(async () => {
        const hello = await fetch('http://made.up/hello');
        out.push([hello.status, hello.statusText, hello.ok, hello.type, hello.url, hello.headers.get('content-type'), hello.headers.getSetCookie().join('+')].join(' '));
        const pieces = [];
        for await (const piece of hello.body) pieces.push(Buffer.from(piece).toString());
        out.push(pieces.join('|'));
        const moved = await fetch('http://made.up/moved');
        out.push([moved.status, moved.redirected, moved.url, await moved.text()].join(' '));
        const kept = await fetch('http://made.up/moved', { redirect: 'manual' });
        out.push([kept.status, kept.headers.get('location')].join(' '));
        const echo = await fetch('http://made.up/echo', { method: 'post', body: 'sent', headers: { 'X-Mine': 'yes' } });
        out.push([echo.headers.get('x-method'), echo.headers.get('x-body'), echo.headers.get('x-sent')].join(' | '));
        out.push(await (await fetch('http://made.up/packed')).text());
        const missing = await fetch('http://made.up/missing');
        out.push([missing.status, missing.ok, await missing.text()].join(' '));
        const endless = await fetch('http://made.up/endless');
        const reader = endless.body.getReader();
        out.push(Buffer.from((await reader.read()).value).toString().trim());
        await reader.cancel();
        for (const url of ['http://made.up/broken', 'http://elsewhere.test/']) {
          await fetch(url).catch((error) => out.push([error.name, error.message, error.cause.code].join(' ')));
        }
      })().catch((error) => out.push('failed ' + error.stack));",
    ))
  assert out_lines(rt)
    == [
      "200 OK true basic http://made.up/hello text/plain a=1+b=2",
      "hel|lo",
      "200 true http://made.up/hello hello",
      "302 /hello",
      "POST | sent | x-mine=yes; content-type=text/plain;charset=UTF-8; accept=*/*; accept-language=*; sec-fetch-mode=cors; user-agent=node; accept-encoding=gzip, deflate",
      "unpacked text, unpacked text",
      "404 false no such page",
      "tick",
      "TypeError fetch failed ECONNRESET",
      "TypeError fetch failed ECONNREFUSED",
    ]
  // what the embedder was asked for, and which bodies it was told to let go
  assert list.filter(notes(), fn(note) {
      string.starts_with(note, "fetch ") || string.starts_with(note, "drop ")
    })
    == [
      "fetch GET /hello", "fetch GET /moved", "drop 2", "fetch GET /hello",
      "fetch GET /moved", "fetch POST /echo", "fetch GET /packed",
      "fetch GET /missing", "fetch GET /endless", "drop 6", "fetch GET /broken",
    ]
}

const crypto_matches_node_script = "
const crypto = require(\"crypto\");
let seed = 2718;
const rnd = (n) => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n; };
const pick = (l) => l[rnd(l.length)];
const lines = [];
const show = (v) => { if (Buffer.isBuffer(v)) return \"buf:\" + v.toString(\"hex\"); if (v instanceof Uint8Array) return \"u8:\" + Array.from(v).join(\".\"); try { return JSON.stringify(v) ?? String(v); } catch { return String(v); } };
const call = (f) => { try { return show(f()); } catch (e) { return \"threw \" + e.name + \" \" + e.code + \" \" + e.message; } };
const t = (label, f) => lines.push(label + \" \" + call(f));
const later = [];
const settle = (label, f) => later.push(async () => { try { lines.push(label + \" \" + show(await f())); } catch (e) { lines.push(label + \" rejected \" + e.name + \" \" + e.code + \" \" + e.message); } });
const algorithms = [\"md5\", \"sha1\", \"sha224\", \"sha256\", \"sha384\", \"sha512\", \"sha512-224\", \"sha512-256\", \"sha3-224\", \"sha3-256\", \"sha3-384\", \"sha3-512\", \"blake2b512\", \"blake2s256\", \"ripemd160\", \"sm3\", \"SHA256\", \"RSA-SHA256\", \"sha-256\", \"sha2-512\", \"rmd160\", \"sha1WithRSAEncryption\"];
const encodings = [undefined, \"hex\", \"base64\", \"base64url\", \"latin1\", \"binary\", \"buffer\", \"ascii\"];
function data() { const n = pick([0, 1, 3, 55, 56, 64, 65, 200, 5000]); const kind = rnd(3); if (kind === 0) return Buffer.alloc(n, rnd(256)); if (kind === 1) return \"text \\u20ac \".repeat(Math.ceil(n / 8)).slice(0, n); return new Uint8Array(Array.from({ length: n }, (_, i) => (i * 13 + 7) % 256)); }
// ---- generated: digests of generated data, in pieces, in every encoding
for (let i = 0; i < 120; i++) {
  const algorithm = pick(algorithms), encoding = pick(encodings), pieces = Array.from({ length: 1 + rnd(3) }, data), input = pick([undefined, undefined, \"utf8\", \"latin1\", \"hex\", \"base64\"]);
  lines.push(\"D \" + i + \" \" + algorithm + \" \" + encoding + \" \" + input + \" \" + pieces.map((p) => p.length).join(\"+\") + \" \" + call(() => { const h = crypto.createHash(algorithm); for (const p of pieces) h.update(p, typeof p === \"string\" ? input : undefined); return h.digest(encoding); }));
}
for (let i = 0; i < 60; i++) {
  const algorithm = pick(algorithms), encoding = pick(encodings), key = pick([\"\", \"key\", \"k\".repeat(200), Buffer.from([0, 1, 255]), new Uint8Array(64).fill(9), \"\\u20ac\"]), pieces = Array.from({ length: 1 + rnd(3) }, data);
  lines.push(\"M \" + i + \" \" + algorithm + \" \" + encoding + \" \" + (typeof key === \"string\" ? key.length : \"bytes\" + key.length) + \" \" + call(() => { const h = crypto.createHmac(algorithm, key); for (const p of pieces) h.update(p); return h.digest(encoding); }));
}
// ---- one case each
t(\"known answers\", () => [crypto.createHash(\"md5\").update(\"\").digest(\"hex\"), crypto.createHash(\"sha1\").update(\"abc\").digest(\"hex\"), crypto.createHash(\"sha256\").update(\"abc\").digest(\"hex\"), crypto.createHmac(\"sha256\", \"key\").update(\"The quick brown fox jumps over the lazy dog\").digest(\"hex\")]);
t(\"shake\", () => [crypto.createHash(\"shake128\", { outputLength: 8 }).update(\"a\").digest(\"hex\"), crypto.createHash(\"shake256\", { outputLength: 40 }).update(\"a\").digest(\"hex\"), crypto.createHash(\"shake128\", { outputLength: 0 }).update(\"a\").digest(\"hex\")]);
for (const [label, f] of [[\"unknown hash\", () => crypto.createHash(\"nope\")], [\"hash not a string\", () => crypto.createHash(5)], [\"unknown hmac\", () => crypto.createHmac(\"nope\", \"k\")], [\"hmac no key\", () => crypto.createHmac(\"sha256\")], [\"hmac key number\", () => crypto.createHmac(\"sha256\", 5)], [\"update number\", () => crypto.createHash(\"md5\").update(5)], [\"update bad encoding\", () => crypto.createHash(\"md5\").update(\"a\", \"nope\")], [\"update after digest\", () => { const h = crypto.createHash(\"md5\"); h.digest(); h.update(\"a\"); }], [\"digest twice\", () => { const h = crypto.createHash(\"md5\"); h.digest(); return h.digest(); }], [\"hmac digest twice\", () => { const h = crypto.createHmac(\"md5\", \"k\"); h.digest(); return [h.digest(\"hex\"), h.digest().length]; }], [\"copy after digest\", () => { const h = crypto.createHash(\"md5\"); h.digest(); return h.copy(); }], [\"digest bad encoding\", () => crypto.createHash(\"md5\").update(\"a\").digest(\"nope\")], [\"outputLength bad\", () => crypto.createHash(\"sha256\", { outputLength: -1 })], [\"without new\", () => crypto.Hash(\"md5\").update(\"a\").digest(\"hex\")], [\"hmac without new\", () => crypto.Hmac(\"md5\", \"k\").update(\"a\").digest(\"hex\")], [\"update returns this\", () => { const h = crypto.createHash(\"md5\"); return h.update(\"a\") === h; }], [\"dataview\", () => crypto.createHash(\"md5\").update(new DataView(new Uint8Array([1, 2]).buffer)).digest(\"hex\")], [\"u16\", () => crypto.createHash(\"md5\").update(new Uint16Array([258, 772])).digest(\"hex\")], [\"arraybuffer refused\", () => crypto.createHash(\"md5\").update(new ArrayBuffer(2))], [\"copy\", () => { const h = crypto.createHash(\"sha256\").update(\"a\"); const c = h.copy(); return [h.update(\"b\").digest(\"hex\"), c.update(\"c\").digest(\"hex\"), crypto.createHash(\"sha256\").update(\"ac\").digest(\"hex\")]; }]]) t(\"hash \" + label, f);
for (const [label, f] of [[\"one shot\", () => [crypto.hash(\"sha1\", \"abc\"), crypto.hash(\"md5\", Buffer.from(\"abc\"), \"base64\"), show(crypto.hash(\"md5\", \"abc\", \"buffer\")), crypto.hash(\"sha256\", \"abc\", { outputEncoding: \"base64url\" })]], [\"one shot unknown\", () => crypto.hash(\"nope\", \"a\")], [\"one shot bad input\", () => crypto.hash(\"md5\", 5)], [\"one shot bad encoding\", () => crypto.hash(\"md5\", \"a\", \"nope\")], [\"getHashes\", () => { const h = crypto.getHashes(); return [Array.isArray(h), h.includes(\"sha256\"), h.includes(\"RSA-SHA256\"), h.includes(\"md5\"), h.every((n) => typeof n === \"string\")]; }]]) t(\"hash \" + label, f);
for (const [label, f] of [[\"equal\", () => crypto.timingSafeEqual(Buffer.from(\"abc\"), Buffer.from(\"abc\"))], [\"not equal\", () => crypto.timingSafeEqual(Buffer.from(\"abc\"), Buffer.from(\"abd\"))], [\"lengths\", () => crypto.timingSafeEqual(Buffer.from(\"a\"), Buffer.from(\"ab\"))], [\"views\", () => crypto.timingSafeEqual(new Uint16Array([1, 2]), new Uint8Array([1, 0, 2, 0]))], [\"arraybuffer\", () => crypto.timingSafeEqual(new Uint8Array([1]).buffer, new Uint8Array([1]))], [\"string\", () => crypto.timingSafeEqual(\"a\", \"a\")], [\"empty\", () => crypto.timingSafeEqual(Buffer.alloc(0), new Uint8Array(0))]]) t(\"timingSafeEqual \" + label, f);
// random: what comes back is different every time, so only its shape is compared
for (const [label, f] of [[\"bytes\", () => { const b = crypto.randomBytes(32); return [Buffer.isBuffer(b), b.length, crypto.randomBytes(0).length]; }], [\"bytes bad\", () => [call(() => crypto.randomBytes(-1)), call(() => crypto.randomBytes(\"5\")), call(() => crypto.randomBytes(2 ** 31)), call(() => crypto.randomBytes(1.5).length), call(() => crypto.randomBytes(4, 5))]], [\"fill\", () => { const b = Buffer.alloc(10); const r = crypto.randomFillSync(b, 2, 4); return [r === b, b[0], b[1], b[6], b[9], crypto.randomFillSync(new Uint32Array(4)).length, crypto.randomFillSync(new ArrayBuffer(8)).byteLength, crypto.randomFillSync(new DataView(new ArrayBuffer(4))).byteLength]; }], [\"fill bad\", () => [call(() => crypto.randomFillSync(\"x\")), call(() => crypto.randomFillSync(Buffer.alloc(4), 9)), call(() => crypto.randomFillSync(Buffer.alloc(4), 2, 9)), call(() => crypto.randomFillSync(Buffer.alloc(4), -1))]], [\"int\", () => { const seen = new Set(); for (let i = 0; i < 300; i++) seen.add(crypto.randomInt(5)); const ranged = Array.from({ length: 100 }, () => crypto.randomInt(10, 13)); return [[...seen].sort().join(), Math.min(...ranged), Math.max(...ranged), Number.isInteger(crypto.randomInt(2 ** 40))]; }], [\"int bad\", () => [call(() => crypto.randomInt()), call(() => crypto.randomInt(5, 5)), call(() => crypto.randomInt(5, 3)), call(() => crypto.randomInt(0, 2 ** 48)), call(() => crypto.randomInt(1.5)), call(() => crypto.randomInt(2 ** 53))]], [\"uuid\", () => { const shape = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/; const many = Array.from({ length: 200 }, () => crypto.randomUUID()); return [many.every((u) => shape.test(u)), new Set(many).size, shape.test(crypto.randomUUID({ disableEntropyCache: true })), call(() => crypto.randomUUID(5)), call(() => crypto.randomUUID({ disableEntropyCache: 1 }))]; }], [\"values\", () => { const a = new Uint8Array(16); return [crypto.getRandomValues(a) === a, crypto.getRandomValues(new Uint32Array(3)).length, crypto.getRandomValues(new BigInt64Array(2)).length, call(() => crypto.getRandomValues(new Float32Array(2))), call(() => crypto.getRandomValues(new Uint8Array(65537))), call(() => crypto.getRandomValues([1])), call(() => crypto.getRandomValues(new DataView(new ArrayBuffer(2))))]; }], [\"not all the same\", () => { const a = crypto.randomBytes(16), b = crypto.randomBytes(16); return [a.equals(b), a.every((x) => x === 0)]; }]]) t(\"random \" + label, f);
t(\"web crypto\", () => { const w = crypto.webcrypto; return [w === globalThis.crypto, Object.prototype.toString.call(w), Object.prototype.toString.call(w.subtle), typeof w.randomUUID(), w.getRandomValues(new Uint8Array(4)).length, call(() => w.getRandomValues.call({}, new Uint8Array(1))), call(() => new w.constructor()), crypto.subtle === w.subtle]; });
t(\"shape\", () => [typeof crypto.createHash, crypto.createHash.length, crypto.createHmac.length, crypto.Hash.name, crypto.Hmac.name, crypto.createHash(\"md5\") instanceof crypto.Hash, crypto.createHash(\"md5\") instanceof require(\"stream\").Transform, crypto.prng === crypto.randomBytes, crypto.pseudoRandomBytes === crypto.randomBytes, crypto.rng === crypto.randomBytes, Object.keys(crypto).includes(\"prng\")]);
for (const name of [\"SHA-1\", \"SHA-256\", \"SHA-384\", \"SHA-512\", \"sha-256\", { name: \"SHA-256\" }]) settle(\"subtle digest \" + JSON.stringify(name), async () => Buffer.from(await crypto.subtle.digest(name, new TextEncoder().encode(\"abc\"))));
settle(\"subtle digest of a buffer\", async () => Buffer.from(await crypto.subtle.digest(\"SHA-1\", new Uint8Array([1, 2, 3]).buffer)));
settle(\"subtle digest unknown\", () => crypto.subtle.digest(\"MD5\", new Uint8Array(1)));
settle(\"random bytes callback\", () => new Promise((resolve, reject) => crypto.randomBytes(8, (err, buf) => (err ? reject(err) : resolve([Buffer.isBuffer(buf), buf.length])))));
settle(\"random fill callback\", () => new Promise((resolve, reject) => { const b = Buffer.alloc(6); crypto.randomFill(b, 1, 2, (err, same) => (err ? reject(err) : resolve([same === b, b[0], b[5]]))); }));
settle(\"random int callback\", () => new Promise((resolve, reject) => crypto.randomInt(3, (err, n) => (err ? reject(err) : resolve(n >= 0 && n < 3)))));
settle(\"hash as a stream\", () => new Promise((resolve) => { const h = crypto.createHash(\"sha256\"); const got = []; h.on(\"data\", (d) => got.push(d.toString(\"hex\"))); h.on(\"end\", () => resolve(got)); h.write(\"ab\"); h.end(\"c\"); }));
settle(\"hmac as a stream\", () => new Promise((resolve) => { const h = crypto.createHmac(\"sha1\", \"k\"); h.setEncoding(\"base64\"); const got = []; h.on(\"data\", (d) => got.push(d)); h.on(\"end\", () => resolve(got)); h.end(\"abc\"); }));
settle(\"hash piped\", () => new Promise((resolve) => { const { Readable } = require(\"stream\"); const h = crypto.createHash(\"md5\"); h.setEncoding(\"hex\"); let text = \"\"; h.on(\"data\", (d) => (text += d)); h.on(\"end\", () => resolve(text)); Readable.from([Buffer.from(\"pi\"), Buffer.from(\"ped\")]).pipe(h); }));
(async () => { for (const run of later) await run(); const groups = new Map();
for (const line of lines) {
  const kind = line.startsWith(\"D \") ? \"digests generated\" : line.startsWith(\"M \") ? \"hmacs generated\" : line.split(\" \")[0];
  const group = groups.get(kind) ?? { count: 0, hash: 2166136261 };
  const bytes = new TextEncoder().encode(line);
  let hash = group.hash;
  for (let i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 16777619) >>> 0;
  groups.set(kind, { count: group.count + 1, hash });
}
for (const [kind, group] of groups) globalThis.out.push(kind + \" \" + group.count + \" \" + group.hash);
globalThis.out.push(\"finished\"); })();

"

// 120 generated digests and 60 generated hmacs over every algorithm, in
// pieces and in each encoding; then one case each for what is refused, the
// one-shot hash, timingSafeEqual, the shapes of what the random calls give
// (the bytes here come from a made-up source, the same on every run), the
// web crypto global, and a hash used as a stream. Each line is a kind of
// case, how many, and a hash of what they printed in node 26.11.1
pub fn crypto_matches_node_test() {
  let rt = run_to_end(run_commonjs(node_runtime(), crypto_matches_node_script))
  assert out_lines(rt)
    == [
      "digests generated 120 552916550",
      "hmacs generated 60 1912751651",
      "known 1 1065135284",
      "shake 1 200561837",
      "hash 27 3637423944",
      "timingSafeEqual 7 931458354",
      "random 12 3208379687",
      "web 1 1058863291",
      "shape 1 3818673648",
      "subtle 8 260331021",
      "hmac 1 4214654828",
      "finished",
    ]
}
