import arc/host
import spark/io.{type Io}

/// one module of the runtime
///
/// the source is the body of `function (exports, require, module, native)`
/// and runs on first use. `native` is an object holding the natives by name
pub type Builtin(host) {
  Builtin(
    /// a `node:x` specifier also answers to plain `x`
    specifier: String,
    source: String,
    /// natives must not do io themselves; they ask through the Io they get
    natives: fn(Io) -> List(host.HostMethod(host)),
    /// the full specifiers of the only modules this one's require can reach.
    /// Each must also be passed to start, because nothing is pulled in for it
    dependencies: List(String),
    /// modules its require can also reach if the embedder passed them.
    /// Requiring one that wasn't passed throws
    optional: List(String),
    /// every name the module exports, for `import { name }`
    exports: List(String),
    /// exports that are also defined on globalThis. If there are any, the
    /// module loads at start
    globals: List(String),
  )
}
