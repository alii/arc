import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// the parts of net that don't open anything: isIP, isIPv4, isIPv6,
/// SocketAddress and BlockList. There are no sockets, so scripts can't reach
/// the network
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:net",
    source: js.net,
    natives: fn(_io: Io) { [] },
    dependencies: ["spark:errors"],
    optional: [],
    exports: ["BlockList", "SocketAddress", "isIP", "isIPv4", "isIPv6"],
    globals: [],
  )
}
