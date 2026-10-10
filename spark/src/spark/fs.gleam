import arc/host.{HostMethod}
import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io, Access, Realpath}
import spark/js

/// node:fs, so far: the access family (access, accessSync, exists, existsSync,
/// promises.access) through the Access want, and the realpath family
/// (realpath, realpathSync, their .native forms, promises.realpath) through
/// the Realpath want
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
      ]
    },
    dependencies: ["spark:errors"],
    optional: [],
    exports: [
      "access", "accessSync", "exists", "existsSync", "realpath", "realpathSync",
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
    exports: ["access", "realpath", "constants"],
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
