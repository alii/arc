import arc/host.{HostMethod}
import arc/rt/types.{mk_string}
import gleam/list
import spark/builtin.{type Builtin, Builtin}
import spark/js

/// the certificate authorities scripts are told of, each a PEM text. spark
/// never asks the machine: `bundled` is what tls.rootCertificates lists (in
/// node, the roots it ships with), `system` what the machine's own store
/// holds, `extra` any added on top
pub type Certificates {
  Certificates(bundled: List(String), system: List(String), extra: List(String))
}

/// node:tls, so far: rootCertificates, getCACertificates and the default
/// version and renegotiation constants. Making a connection is not here
pub fn builtin(certificates: Certificates) -> Builtin(host) {
  Builtin(
    specifier: "node:tls",
    source: js.tls,
    natives: fn(_io) {
      [
        HostMethod("certificates", 0, fn(ctx, _args, _this) {
          let #(bundled, ctx) =
            host.array(ctx, list.map(certificates.bundled, mk_string))
          let #(system, ctx) =
            host.array(ctx, list.map(certificates.system, mk_string))
          let #(extra, ctx) =
            host.array(ctx, list.map(certificates.extra, mk_string))
          let #(all, ctx) =
            host.object(ctx, [
              #("bundled", bundled),
              #("system", system),
              #("extra", extra),
            ])
          #(Ok(all), ctx)
        }),
      ]
    },
    dependencies: ["spark:errors"],
    optional: [],
    exports: [
      "CLIENT_RENEG_LIMIT", "CLIENT_RENEG_WINDOW", "DEFAULT_ECDH_CURVE",
      "DEFAULT_MIN_VERSION", "DEFAULT_MAX_VERSION", "rootCertificates",
      "getCACertificates", "setDefaultCACertificates", "checkServerIdentity",
      "createSecureContext", "SecureContext", "TLSSocket", "Server",
      "createServer", "connect",
    ],
    globals: [],
  )
}
