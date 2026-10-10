import arc/host.{type HostMethod, HostMethod}
import spark/builtin.{type Builtin, Builtin}
import spark/js

pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:async_hooks",
    source: js.async_hooks,
    natives: fn(_io) { context_natives() },
    dependencies: ["spark:errors"],
    optional: [],
    exports: [
      "AsyncLocalStorage", "AsyncResource", "createHook", "executionAsyncId",
      "triggerAsyncId", "executionAsyncResource", "asyncWrapProviders",
    ],
    globals: [],
  )
}

/// getContext and setContext over the engine's async context
pub fn context_natives() -> List(HostMethod(host)) {
  [
    HostMethod("getContext", 0, fn(ctx, _args, _this) {
      #(Ok(host.async_context(ctx)), ctx)
    }),
    HostMethod("setContext", 1, fn(ctx, args, _this) {
      let #(previous, ctx) = host.swap_async_context(ctx, host.first_arg(args))
      #(Ok(previous), ctx)
    }),
  ]
}
