import arc/host.{type Context, HostMethod}
import arc/rt/types.{type JsVal, mk_bool, mk_int, mk_string}
import gleam/bit_array
import gleam/list
import spark/async_hooks
import spark/builtin.{type Builtin, Builtin}
import spark/io.{Stderr, Stdout, Write}
import spark/js
import spark/platform.{type Platform}

/// what scripts see on `process`; spark never reads any of it from the machine
pub type Process {
  Process(
    env: List(#(String, String)),
    argv: List(String),
    cwd: String,
    platform: Platform,
    arch: String,
    pid: Int,
    /// what `process.version` reports, such as "v26.11.1"
    version: String,
    versions: List(#(String, String)),
    stdout_is_tty: Bool,
    stderr_is_tty: Bool,
  )
}

/// node:process, so far: env, argv, cwd, platform, arch, pid, version,
/// versions, uptime (from the engine's clock hook), nextTick, and
/// stdout/stderr with write, which asks through the Write want
pub fn builtin(process: Process) -> Builtin(host) {
  Builtin(
    specifier: "node:process",
    source: js.process,
    natives: fn(io) {
      [
        HostMethod("facts", 0, fn(ctx, _args, _this) {
          let #(facts, ctx) = facts(ctx, process)
          #(Ok(facts), ctx)
        }),
        HostMethod("now", 0, fn(ctx, _args, _this) {
          #(Ok(mk_int(ctx.agent.hooks.monotonic_now())), ctx)
        }),
        HostMethod("write", 2, fn(ctx, args, _this) {
          use fd, ctx <- host.validate_integer(
            ctx,
            host.arg_at(args, 0),
            "fd",
            1,
            2,
          )
          use text, ctx <- host.validate_string(
            ctx,
            host.arg_at(args, 1),
            "chunk",
          )
          let stream = case fd {
            1 -> Stdout
            _ -> Stderr
          }
          io.ask(io, ctx, Write(stream:, data: bit_array.from_string(text)))
        }),
        ..async_hooks.context_natives()
      ]
    },
    dependencies: ["spark:errors", "spark:loop"],
    exports: [
      "env", "argv", "platform", "arch", "pid", "version", "versions", "cwd",
      "uptime", "nextTick", "stdout", "stderr",
    ],
    globals: [],
  )
}

fn facts(ctx: Context(host), process: Process) -> #(JsVal, Context(host)) {
  let #(env, ctx) = host.object(ctx, strings(process.env))
  let #(argv, ctx) = host.array(ctx, list.map(process.argv, mk_string))
  let #(versions, ctx) = host.object(ctx, strings(process.versions))
  host.object(ctx, [
    #("env", env),
    #("argv", argv),
    #("cwd", mk_string(process.cwd)),
    #("platform", mk_string(platform.name(process.platform))),
    #("arch", mk_string(process.arch)),
    #("pid", mk_int(process.pid)),
    #("version", mk_string(process.version)),
    #("versions", versions),
    #("stdoutIsTTY", mk_bool(process.stdout_is_tty)),
    #("stderrIsTTY", mk_bool(process.stderr_is_tty)),
  ])
}

fn strings(pairs: List(#(String, String))) -> List(#(String, JsVal)) {
  list.map(pairs, fn(pair) { #(pair.0, mk_string(pair.1)) })
}
