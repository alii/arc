import arc/host
import arc/interp/entry
import arc/module
import arc/module/loader
import arc/rt/builtins as rt_builtins
import arc/rt/types.{type Handle, mk_string}
import gleam/dict
import rt_helpers

fn dance_resolve(raw: String, _ref: String) {
  Ok(raw)
}

fn no_source_loads(_resolved: String) {
  Error(loader.LoadNotFound)
}

fn deferred_namespace_of(
  spec: String,
) -> Result(Handle, module.DeferredNamespaceError) {
  let ctx =
    rt_builtins.new_agent(rt_helpers.quiet_hooks())
    |> entry.link
    |> host.from_agent(host.new_brand())
  let #(greet, ctx) =
    host.function(ctx, "greet", 0, fn(ctx, _a, _t) {
      #(Ok(mk_string("hi")), ctx)
    })
  let hosts =
    dict.from_list([
      #("dance", module.HostModule([#("greet", greet)])),
    ])
  let assert Ok(bundle) =
    module.compile_bundle_with_hosts(
      "entry",
      "import { greet } from 'dance'; export const r = greet();",
      dance_resolve,
      no_source_loads,
      hosts,
    )
  let #(out, _st) =
    host.with_context(ctx.agent, ctx.brand, fn(ctx) {
      let assert #(Ok(linked), st) =
        module.link_for_evaluation(ctx.agent, bundle)
      let #(deferred, st) =
        module.get_or_create_deferred_namespace(st, linked, spec)
      #(deferred, host.Context(..ctx, agent: st))
    })
  out
}

pub fn deferred_namespace_over_host_module_test() {
  let assert Ok(_proxy) = deferred_namespace_of("dance")
}

pub fn deferred_namespace_of_unknown_specifier_test() {
  assert deferred_namespace_of("nope")
    == Error(module.DeferredSpecifierNotInBundle("nope"))
}
