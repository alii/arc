import arc/engine.{Returned, Threw}
import arc/module/file_loader
import arc/module/specifier
import argv
import gleam/erlang/process.{type Subject} as erlang_process
import gleam/io
import gleam/result
import gleam/string
import simplifile
import spark.{type Runtime, Finished, WakeIn}
import spark/async_hooks
import spark/buffer
import spark/builtin.{type Builtin}
import spark/dom_events
import spark/domexception
import spark/encoding
import spark/events
import spark/fetch
import spark/fs
import spark/global
import spark/io.{
  type Answer, type Failure, type Io, type ReplySlip, type Stream, type Want,
  Access, DropBody, Fetch, Io, ReadBody, ReadFile, Readdir, Readlink, Realpath,
  Stat, Stderr, Stdout, Write,
} as _
import spark/net
import spark/os.{type Os, type User, BigEndian, LittleEndian, Os}
import spark/path
import spark/perf_hooks
import spark/platform.{type Platform}
import spark/process.{type Process, Process}
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

type CliError {
  Usage
  UnsupportedPlatform(name: String)
  StartFailed(error: spark.StartError)
  ReadFailed(path: String, error: simplifile.FileError)
  EvalFailed(message: String)
  Uncaught(report: String)
}

fn error_message(err: CliError) -> String {
  case err {
    Usage -> "Usage: spark <file>"
    UnsupportedPlatform(name:) ->
      "spark runs on linux and macos only, not " <> name
    StartFailed(error:) -> spark.start_error_message(error)
    ReadFailed(path:, error:) ->
      "Error reading " <> path <> ": " <> simplifile.describe_error(error)
    EvalFailed(message:) -> message
    Uncaught(report:) -> report
  }
}

// the cli is a full runtime, so it takes every builtin spark ships
fn builtins(
  platform: Platform,
  path: String,
  rest: List(String),
) -> List(Builtin(Nil)) {
  [
    async_hooks.builtin(),
    timers.builtin(),
    fs.builtin(),
    fs.promises_builtin(),
    process.builtin(this_process(platform, path, rest)),
    os.builtin(this_os(platform)),
    global.builtin(),
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
    tty.builtin(terminals()),
    // the cli has no roots of its own to ship, so the machine's stand for both
    tls.builtin(
      Certificates(bundled: certificates(), system: certificates(), extra: []),
    ),
  ]
}

@external(erlang, "spark_cli_ffi", "os_release")
fn os_release() -> String

@external(erlang, "spark_cli_ffi", "os_version")
fn os_version() -> String

@external(erlang, "spark_cli_ffi", "machine")
fn machine() -> String

@external(erlang, "spark_cli_ffi", "hostname")
fn hostname() -> String

@external(erlang, "spark_cli_ffi", "homedir")
fn homedir() -> String

@external(erlang, "spark_cli_ffi", "tmpdir")
fn tmpdir() -> String

@external(erlang, "spark_cli_ffi", "little_endian")
fn little_endian() -> Bool

@external(erlang, "spark_cli_ffi", "user")
fn user() -> User

@external(erlang, "spark_cli_ffi", "certificates")
fn certificates() -> List(String)

@external(erlang, "spark_cli_ffi", "terminals")
fn terminals() -> List(Int)

@external(erlang, "spark_cli_ffi", "parallelism")
fn parallelism() -> Int

fn this_os(platform: Platform) -> Os {
  Os(
    platform:,
    arch: arch(),
    release: os_release(),
    version: os_version(),
    machine: machine(),
    hostname: hostname(),
    homedir: homedir(),
    tmpdir: tmpdir(),
    endianness: case little_endian() {
      True -> LittleEndian
      False -> BigEndian
    },
    parallelism: parallelism(),
    user: user(),
  )
}

@external(erlang, "spark_cli_ffi", "env")
fn env() -> List(#(String, String))

@external(erlang, "spark_cli_ffi", "cwd")
fn cwd() -> String

@external(erlang, "spark_cli_ffi", "platform")
fn platform() -> Result(Platform, String)

@external(erlang, "spark_cli_ffi", "arch")
fn arch() -> String

@external(erlang, "spark_cli_ffi", "pid")
fn pid() -> Int

@external(erlang, "spark_cli_ffi", "is_tty")
fn is_tty(stream: Stream) -> Bool

// spark knows nothing about this machine; the cli reads it and hands it over
fn this_process(
  platform: Platform,
  path: String,
  rest: List(String),
) -> Process {
  Process(
    env: env(),
    argv: ["spark", path, ..rest],
    cwd: cwd(),
    platform:,
    arch: arch(),
    pid: pid(),
    version: "v26.11.1",
    versions: [#("node", "26.11.1"), #("spark", "0.1.0")],
    stdout_is_tty: is_tty(Stdout),
    stderr_is_tty: is_tty(Stderr),
  )
}

type Message {
  Asked
  Answered(slip: ReplySlip, result: Result(Answer, Failure))
}

@external(erlang, "spark_cli_ffi", "access")
fn access(path: String, mode: Int) -> Result(Answer, Failure)

@external(erlang, "spark_cli_ffi", "stat")
fn stat(path: String, follow: Bool) -> Result(Answer, Failure)

@external(erlang, "spark_cli_ffi", "readlink")
fn readlink(path: String) -> Result(Answer, Failure)

@external(erlang, "spark_cli_ffi", "readdir")
fn readdir(path: String) -> Result(Answer, Failure)

@external(erlang, "spark_cli_ffi", "read_file")
fn read_file(path: String) -> Result(Answer, Failure)

@external(erlang, "spark_cli_ffi", "realpath")
fn realpath(path: String) -> Result(Answer, Failure)

@external(erlang, "spark_cli_ffi", "write")
fn write(stream: Stream, data: BitArray) -> Result(Answer, Failure)

// this is where the cli touches the machine, and the only place
fn perform(want: Want) -> Result(Answer, Failure) {
  case want {
    Access(path:, mode:) -> access(path, mode)
    Realpath(path:) -> realpath(path)
    Stat(path:, follow:) -> stat(path, follow)
    Readlink(path:) -> readlink(path)
    Readdir(path:) -> readdir(path)
    ReadFile(path:) -> read_file(path)
    Fetch(method:, url:, headers:, body:) -> fetch(method, url, headers, body)
    ReadBody(body:) -> read_body(body)
    DropBody(body:) -> drop_body(body)
    Write(stream:, data:) -> write(stream, data)
  }
}

@external(erlang, "spark_cli_ffi", "fetch")
fn fetch(
  method: String,
  url: String,
  headers: List(#(String, String)),
  body: BitArray,
) -> Result(Answer, Failure)

@external(erlang, "spark_cli_ffi", "read_body")
fn read_body(body: Int) -> Result(Answer, Failure)

@external(erlang, "spark_cli_ffi", "drop_body")
fn drop_body(body: Int) -> Result(Answer, Failure)

fn real_io(inbox: Subject(Message)) -> Io {
  Io(now: perform, later: fn(want, slip) {
    erlang_process.send(inbox, Asked)
    erlang_process.spawn(fn() {
      erlang_process.send(inbox, Answered(slip, perform(want)))
    })
    Nil
  })
}

// spark never waits; this loop is where the waiting happens
fn run_loop(
  rt: Runtime(Nil),
  inbox: Subject(Message),
  pending: Int,
) -> Runtime(Nil) {
  let #(next, rt) = spark.turn(rt)
  case erlang_process.receive(inbox, 0), next, pending {
    Ok(message), _, _ -> handle(rt, inbox, pending, message)
    Error(Nil), Finished, 0 -> rt
    Error(Nil), Finished, _ ->
      handle(rt, inbox, pending, erlang_process.receive_forever(inbox))
    Error(Nil), WakeIn(ms:), _ ->
      case erlang_process.receive(inbox, ms) {
        Ok(message) -> handle(rt, inbox, pending, message)
        Error(Nil) -> run_loop(rt, inbox, pending)
      }
  }
}

fn handle(
  rt: Runtime(Nil),
  inbox: Subject(Message),
  pending: Int,
  message: Message,
) -> Runtime(Nil) {
  case message {
    Asked -> run_loop(rt, inbox, pending + 1)
    Answered(slip:, result:) -> {
      let #(_outcome, rt) = spark.answer(rt, slip, result)
      run_loop(rt, inbox, pending - 1)
    }
  }
}

// `.cjs` runs as commonjs, anything else as an es module
fn run_file(
  rt: Runtime(Nil),
  inbox: Subject(Message),
  path: String,
) -> Result(Nil, CliError) {
  use source <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(err) { ReadFailed(path:, error: err) }),
  )
  let loop = run_loop(_, inbox, 0)
  case string.ends_with(path, ".cjs") {
    True ->
      case spark.eval_commonjs(rt, path, source) {
        #(Returned(_), rt) -> {
          let _finished = loop(rt)
          Ok(Nil)
        }
        #(Threw(thrown), rt) -> Error(uncaught(rt, thrown))
      }
    False ->
      case
        spark.eval_module(
          rt,
          specifier.normalize(path),
          source,
          file_loader.file_resolve,
          file_loader.file_load,
          loop,
        )
      {
        Ok(#(engine.ModuleReturned(..), rt)) -> {
          let _finished = loop(rt)
          Ok(Nil)
        }
        Ok(#(engine.ModuleThrew(error:), rt)) -> Error(uncaught(rt, error))
        Error(err) -> Error(EvalFailed(engine.eval_error_message(err)))
      }
  }
}

fn uncaught(rt: Runtime(Nil), thrown) -> CliError {
  Uncaught("Uncaught " <> engine.format_error(spark.engine(rt), thrown))
}

fn run(args: List(String)) -> Result(Nil, CliError) {
  case args {
    [path, ..rest] -> {
      use platform <- result.try(
        platform() |> result.map_error(UnsupportedPlatform),
      )
      let inbox = erlang_process.new_subject()
      use rt <- result.try(
        spark.start(
          engine.new(),
          builtins(platform, path, rest),
          real_io(inbox),
        )
        |> result.map_error(StartFailed),
      )
      run_file(rt, inbox, path)
    }
    [] -> Error(Usage)
  }
}

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> a

pub fn main() -> Nil {
  case run(argv.load().arguments) {
    Ok(Nil) -> Nil
    Error(err) -> {
      io.println_error(error_message(err))
      halt(1)
    }
  }
}
