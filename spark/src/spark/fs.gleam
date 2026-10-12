import arc/host.{HostMethod}
import spark/builtin.{type Builtin, Builtin}
import spark/io.{
  type Io, type Want, Access, ReadFile, Readdir, Readlink, Realpath, Stat,
}
import spark/js

/// node:fs, so far, and only what reads: access and exists, realpath, stat
/// and lstat, readlink, readdir and readFile, each in its sync, callback and
/// promise form. Every one is a want the embedder answers (Access, Realpath,
/// Stat, Readlink, Readdir, ReadFile); nothing here touches a disk
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:fs",
    source: js.fs,
    natives: fn(io) {
      [
        HostMethod("access", 2, fn(ctx, args, _this) {
          use want, ctx <- access_want(ctx, args)
          io.ask(io, ctx, want)
        }),
        HostMethod("accessLater", 2, fn(ctx, args, _this) {
          use want, ctx <- access_want(ctx, args)
          io.ask_later(io, ctx, want)
        }),
        HostMethod("realpath", 1, fn(ctx, args, _this) {
          use path, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "path",
          )
          io.ask(io, ctx, Realpath(path:))
        }),
        HostMethod("realpathLater", 1, fn(ctx, args, _this) {
          use path, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "path",
          )
          io.ask_later(io, ctx, Realpath(path:))
        }),
        HostMethod("stat", 2, fn(ctx, args, _this) {
          use want, ctx <- stat_want(ctx, args)
          io.ask(io, ctx, want)
        }),
        HostMethod("statLater", 2, fn(ctx, args, _this) {
          use want, ctx <- stat_want(ctx, args)
          io.ask_later(io, ctx, want)
        }),
        HostMethod("readlink", 1, fn(ctx, args, _this) {
          use want, ctx <- path_want(ctx, args, Readlink)
          io.ask(io, ctx, want)
        }),
        HostMethod("readlinkLater", 1, fn(ctx, args, _this) {
          use want, ctx <- path_want(ctx, args, Readlink)
          io.ask_later(io, ctx, want)
        }),
        HostMethod("readdir", 1, fn(ctx, args, _this) {
          use want, ctx <- path_want(ctx, args, Readdir)
          io.ask(io, ctx, want)
        }),
        HostMethod("readdirLater", 1, fn(ctx, args, _this) {
          use want, ctx <- path_want(ctx, args, Readdir)
          io.ask_later(io, ctx, want)
        }),
        HostMethod("readFile", 1, fn(ctx, args, _this) {
          use want, ctx <- path_want(ctx, args, ReadFile)
          io.ask(io, ctx, want)
        }),
        HostMethod("readFileLater", 1, fn(ctx, args, _this) {
          use want, ctx <- path_want(ctx, args, ReadFile)
          io.ask_later(io, ctx, want)
        }),
      ]
    },
    dependencies: ["spark:errors", "node:buffer"],
    optional: ["node:path", "node:url"],
    exports: [
      "access", "accessSync", "exists", "existsSync", "realpath", "realpathSync",
      "stat", "statSync", "lstat", "lstatSync", "readlink", "readlinkSync",
      "readdir", "readdirSync", "readFile", "readFileSync", "Stats", "Dirent",
      "constants", "promises", "F_OK", "R_OK", "W_OK", "X_OK",
    ],
    globals: [],
  )
}

/// node:fs/promises; it is node:fs's `promises`, so pass that builtin too
pub fn promises_builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:fs/promises",
    source: js.fs_promises,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:fs"],
    optional: [],
    exports: [
      "access", "realpath", "stat", "lstat", "readlink", "readdir", "readFile",
      "constants",
    ],
    globals: [],
  )
}

fn access_want(ctx, args, next) {
  use path, ctx <- host.validate_string(ctx, host.arg_at(args, 0), "path")
  use mode, ctx <- host.validate_integer(
    ctx,
    host.arg_at(args, 1),
    "mode",
    0,
    7,
  )
  next(Access(path:, mode:), ctx)
}

fn stat_want(ctx, args, next) {
  use path, ctx <- host.validate_string(ctx, host.arg_at(args, 0), "path")
  use follow, ctx <- host.validate_boolean(ctx, host.arg_at(args, 1), "follow")
  next(Stat(path:, follow:), ctx)
}

fn path_want(ctx, args, want: fn(String) -> Want, next) {
  use path, ctx <- host.validate_string(ctx, host.first_arg(args), "path")
  next(want(path), ctx)
}
