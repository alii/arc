> [!NOTE]
> arc is still a young project, tread carefully!

# arc ⌒

JavaScript on the BEAM

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="./.github/js.png">
  <img alt="js" src="./.github/js-light.png">
</picture>
<br><br>

Arc is a JavaScript engine written in [Gleam](https://gleam.run). It implements the full language: closures, generators, async/await, classes, proxies, typed arrays, Intl and Temporal. It runs on Erlang/OTP, and in the browser through [AtomVM](https://www.atomvm.net) compiled to WebAssembly.
<br><br>

The engine knows nothing outside ECMAScript. There are no built-in timers, I/O or concurrency model. You embed it in a BEAM program and give it the globals and host functions you want.
<br><br>

Tested against [test262](https://github.com/tc39/test262) on every commit:

<picture>
  <source media="(prefers-color-scheme: dark)" srcset=".github/test262/conformance-dark.png">
  <img alt="test262 conformance chart" src=".github/test262/conformance.png">
</picture>

---

```sh
gleam run -- file.js       # run a script
gleam test                 # unit tests
TEST262_EXEC=1 gleam test  # full test262 suite
TEST262=1 gleam test       # parser-only test262
```
