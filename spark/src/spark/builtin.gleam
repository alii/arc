import arc/host
import spark/io.{type Io}

/// one module of the runtime
///
/// the source is the body of `function (exports, require, module, native)`,
/// run on first use; `native` is an object holding the natives by name
pub type Builtin(host) {
  Builtin(
    /// a `node:x` specifier also answers to plain `x`
    specifier: String,
    source: String,
    /// natives must not do io themselves; they ask through the Io they get
    natives: fn(Io) -> List(host.HostMethod(host)),
    /// full specifiers of the only modules this one's require can reach;
    /// each must be passed to start too, nothing is pulled in for it
    dependencies: List(String),
    /// modules its require can also reach when the embedder passed them; a
    /// require of one that was not passed throws
    optional: List(String),
    /// every name the module exports, for `import { name }`
    exports: List(String),
    /// exports also defined on globalThis; any here loads the module at start
    globals: List(String),
  )
}
