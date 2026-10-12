import arc/host.{HostMethod}
import arc/rt/types.{mk_int, mk_null, mk_string}
import gleam/option.{type Option, None, Some}
import spark/builtin.{type Builtin, Builtin}
import spark/js
import spark/platform.{type Platform}

/// what scripts see through `os`; spark never asks the machine for any of it
pub type Os {
  Os(
    /// decides `os.platform()`, `os.type()` and the numbers in `os.constants`
    platform: Platform,
    arch: String,
    release: String,
    version: String,
    machine: String,
    hostname: String,
    homedir: String,
    tmpdir: String,
    endianness: Endianness,
    /// what `os.availableParallelism()` reports
    parallelism: Int,
    /// who `os.userInfo()` says is running the script
    user: User,
  )
}

pub type User {
  /// `shell` is None where there is none to name, as on windows
  User(uid: Int, gid: Int, username: String, shell: Option(String))
}

pub type Endianness {
  LittleEndian
  BigEndian
}

/// node:os, so far: arch, platform, type, release, version, machine,
/// hostname, homedir, tmpdir, endianness, availableParallelism, userInfo,
/// constants, EOL, devNull
pub fn builtin(os: Os) -> Builtin(host) {
  Builtin(
    specifier: "node:os",
    source: js.os,
    natives: fn(_io) {
      [
        HostMethod("facts", 0, fn(ctx, _args, _this) {
          let #(facts, ctx) =
            host.object(ctx, [
              #("platform", mk_string(platform.name(os.platform))),
              #("arch", mk_string(os.arch)),
              #("release", mk_string(os.release)),
              #("version", mk_string(os.version)),
              #("machine", mk_string(os.machine)),
              #("hostname", mk_string(os.hostname)),
              #("homedir", mk_string(os.homedir)),
              #("tmpdir", mk_string(os.tmpdir)),
              #(
                "endianness",
                mk_string(case os.endianness {
                  LittleEndian -> "LE"
                  BigEndian -> "BE"
                }),
              ),
              #("parallelism", mk_int(os.parallelism)),
              #("uid", mk_int(os.user.uid)),
              #("gid", mk_int(os.user.gid)),
              #("username", mk_string(os.user.username)),
              #("shell", case os.user.shell {
                Some(shell) -> mk_string(shell)
                None -> mk_null()
              }),
            ])
          #(Ok(facts), ctx)
        }),
      ]
    },
    dependencies: [],
    optional: ["node:buffer"],
    exports: [
      "arch", "availableParallelism", "endianness", "homedir", "hostname",
      "platform", "release", "tmpdir", "type", "userInfo", "version", "machine",
      "constants", "EOL", "devNull",
    ],
    globals: [],
  )
}
