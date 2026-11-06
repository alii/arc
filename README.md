# arc ⌒

JavaScript on the BEAM

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="./.github/js.png">
  <img alt="js" src="./.github/js-light.png">
</picture>
<br><br>

Arc is a JavaScript engine and runtime written in [Gleam](https://gleam.run). It implements the ECMAScript spec and is tested against test262 on every commit. By default there are no effects, timers, I/O, etc. Such functionality is provided by the embedder, which is very easy to do as Arc has an excellent embedding API.

There's also [Spark](./spark), which is a Node.js compatibility layer for Arc. This _does_ implement the glue code for I/O and timers, such as `fs.readFile` and `setTimeout`. Generally speaking you can think of Arc as logically equivalent to JSC or V8, and Spark as Bun or Node.js.

Arc's API is unstable between commits, but this is true for many engines. We recommend pinning to a commit sha and doing upgrades to Arc on some regular cadence.
<br><br>

By default Arc interprets JavaScript, but it has an AOT mode for compiling JavaScript to Erlang. This has tradeoffs and is not recommended for dynamic code (e.g. code generated/created at runtime). Arc runs on Erlang/OTP, and in the browser on our own fork of [AtomVM](https://www.atomvm.net) compiled to WebAssembly. We forked Atom to improve performance in the browser specifically.
<br><br>

Arc is tested against [test262](https://github.com/tc39/test262) on every commit:

<picture>
  <source media="(prefers-color-scheme: dark)" srcset=".github/test262/conformance-dark.png">
  <img alt="test262 conformance chart" src=".github/test262/conformance.png">
</picture>

<br><br>

Arc uses the Mozilla Public License, Version 2.0
