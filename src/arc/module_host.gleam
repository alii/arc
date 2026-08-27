import arc/interp/dynamic_import
import arc/module
import arc/module/graph
import arc/module/load_error
import arc/module/registry
import arc/rt/async as rt_async
import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, Agent, HostFnEntry, SyntaxErr, TypeErr,
  mk_object, mk_undefined,
}
import gleam/dict
import gleam/list
import gleam/option
import gleam/set

pub type ResolveError =
  load_error.ResolveError

pub type LoadError =
  load_error.LoadError

pub type ResolveFn =
  fn(String, String) -> Result(String, ResolveError)

pub type LoadFn =
  fn(String) -> Result(String, LoadError)

/// resolve/load pair that forbids every import
pub fn no_imports() -> #(ResolveFn, LoadFn) {
  #(forbid_resolve, forbid_load)
}

pub fn forbid_resolve(
  _raw_specifier: String,
  _referrer: String,
) -> Result(String, ResolveError) {
  Error(load_error.ResolveForbidden)
}

pub fn forbid_load(_resolved: String) -> Result(String, LoadError) {
  Error(load_error.LoadForbidden)
}

/// installs the §16.2.1.8 dynamic import hook; reinstall after deserialize
pub fn install_import_hook(
  st: Agent,
  referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> Agent {
  let hook =
    HostFnEntry(name: "%DynamicImportHook%", call: fn(st, args, _this, _nt) {
      import_module(args, st, referrer, resolve, load)
    })
  Agent(..st, import_hook: option.Some(hook))
}

fn no_drain(st: Agent) -> Agent {
  st
}

fn type_error(st: Agent, msg: String) -> #(Agent, Result(JsVal, JsVal)) {
  let #(err, st) = st.store.ops.new_error(st, TypeErr, msg)
  #(st, Error(err))
}

fn syntax_error(st: Agent, msg: String) -> #(Agent, Result(JsVal, JsVal)) {
  let #(err, st) = st.store.ops.new_error(st, SyntaxErr, msg)
  #(st, Error(err))
}

// Error(thrown) rejects with thrown; the defer arm settles the promise itself
fn import_module(
  args: List(JsVal),
  st: Agent,
  entry_referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(Agent, Result(JsVal, JsVal)) {
  case dynamic_import.parse_hook_args(args) {
    Error(err) -> type_error(st, dynamic_import.hook_arg_error_message(err))
    Ok(dynamic_import.HookCall(specifier:, referrer:, phase:)) -> {
      let referrer = option.unwrap(referrer, entry_referrer)
      case resolve(specifier, referrer) {
        Error(err) ->
          type_error(
            st,
            load_error.resolve_failure_message(specifier, referrer, err),
          )
        Ok(resolved) ->
          case phase {
            dynamic_import.DeferPhase(resolve_fn:, reject_fn:) ->
              defer_import_module(
                st,
                resolved,
                resolve,
                load,
                resolve_fn,
                reject_fn,
              )
            dynamic_import.EagerPhase ->
              eager_import_module(st, resolved, resolve, load)
          }
      }
    }
  }
}

fn eager_import_module(
  st: Agent,
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(Agent, Result(JsVal, JsVal)) {
  case registry.read_cache_state(st, resolved) {
    // error cache wins: a namespace entry may be stale after a throw
    registry.Failed(error:) -> #(st, Error(error))
    // parked on tla: same in-flight promise (Evaluate step 4)
    registry.Pending(promise:, deferred: _) -> #(st, Ok(mk_object(promise)))
    registry.Started(namespace:, deferred: _) -> #(st, Ok(mk_object(namespace)))
    // linked-only (import.defer) namespaces still need evaluating
    registry.LinkedOnly(..) | registry.Absent(..) ->
      evaluate_module(st, resolved, resolve, load)
  }
}

fn evaluate_module(
  st: Agent,
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(Agent, Result(JsVal, JsVal)) {
  use source <- with_loaded_source(st, resolved, load)
  case module.compile_bundle(resolved, source, resolve, load) {
    Error(err) -> compile_bundle_rejection(st, err)
    Ok(bundle) -> {
      // evaluate without draining: we are inside a promise job
      let #(st, res) = evaluate_bundle_with_registry(st, bundle, no_drain)
      case res {
        Ok(module.EvaluatedBundle(value: _, namespace:)) -> #(
          st,
          Ok(mk_object(namespace)),
        )
        Error(module.EvaluationError(value: thrown)) -> {
          let st = registry.write_module_error(st, resolved, thrown)
          #(st, Error(thrown))
        }
        Error(module.EvaluationPending(promise:)) ->
          pending_module_promise(st, resolved, promise)
        Error(module.NotInBundle(..) as other) ->
          type_error(
            st,
            "Failed to evaluate module '"
              <> resolved
              <> "': "
              <> module.error_message(other, st),
          )
      }
    }
  }
}

// import.defer: link, pre-evaluate async deps, settle via resolve_fn
fn defer_import_module(
  st: Agent,
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
  resolve_fn: JsVal,
  reject_fn: JsVal,
) -> #(Agent, Result(JsVal, JsVal)) {
  case registry.read_cache_state(st, resolved) {
    registry.Failed(error:) -> #(st, Error(error))
    registry.Pending(deferred: option.Some(deferred_ns), ..)
    | registry.Started(deferred: option.Some(deferred_ns), ..)
    | registry.LinkedOnly(deferred: option.Some(deferred_ns), ..)
    | registry.Absent(deferred: option.Some(deferred_ns)) ->
      settle_defer_import(st, resolve_fn, mk_object(deferred_ns))
    registry.Pending(deferred: option.None, ..)
    | registry.Started(deferred: option.None, ..)
    | registry.LinkedOnly(deferred: option.None, ..)
    | registry.Absent(deferred: option.None) -> {
      use source <- with_loaded_source(st, resolved, load)
      case module.compile_bundle(resolved, source, resolve, load) {
        Error(err) -> compile_bundle_rejection(st, err)
        Ok(bundle) ->
          case link_bundle_with_registry(st, bundle) {
            #(st, Error(module.EvaluationError(value: thrown))) -> #(
              st,
              Error(thrown),
            )
            #(st, Error(other)) ->
              type_error(
                st,
                "Failed to link module '"
                  <> resolved
                  <> "': "
                  <> module.error_message(other, st),
              )
            #(st, Ok(linked_bundle)) ->
              case
                module.get_or_create_deferred_namespace(
                  st,
                  linked_bundle,
                  resolved,
                )
              {
                #(st, Ok(ns)) -> {
                  let st = registry.write_deferred_namespace(st, resolved, ns)
                  evaluate_deferred_async_deps(
                    st,
                    resolved,
                    mk_object(ns),
                    linked_bundle,
                    resolve_fn,
                    reject_fn,
                  )
                }
                #(st, Error(module.DeferredSpecifierNotInBundle(specifier:))) ->
                  type_error(st, "Cannot find module '" <> specifier <> "'")
              }
          }
      }
    }
  }
}

fn settle_defer_import(
  st: Agent,
  resolve_fn: JsVal,
  value: JsVal,
) -> #(Agent, Result(JsVal, JsVal)) {
  #(call_import_settle_fn(st, resolve_fn, value), Ok(mk_undefined()))
}

fn with_loaded_source(
  st: Agent,
  resolved: String,
  load: LoadFn,
  then: fn(String) -> #(Agent, Result(JsVal, JsVal)),
) -> #(Agent, Result(JsVal, JsVal)) {
  case load(resolved) {
    Error(err) -> type_error(st, load_error.load_failure_message(resolved, err))
    Ok(source) -> then(source)
  }
}

fn evaluate_deferred_async_deps(
  st: Agent,
  resolved: String,
  ns: JsVal,
  linked_bundle: module.LinkedBundle,
  resolve_fn: JsVal,
  reject_fn: JsVal,
) -> #(Agent, Result(JsVal, JsVal)) {
  case module.evaluate_async_transitive_deps(linked_bundle, st, no_drain) {
    #(st, Ok([])) -> settle_defer_import(st, resolve_fn, ns)
    #(st, Ok(pendings)) -> #(
      chain_deferred_settlement(st, ns, pendings, resolve_fn, reject_fn),
      Ok(mk_undefined()),
    )
    #(st, Error(module.EvaluationError(value: thrown))) -> {
      let st = registry.write_module_error(st, resolved, thrown)
      #(st, Error(thrown))
    }
    #(st, Error(module.NotInBundle(..) as other))
    | #(st, Error(module.EvaluationPending(..) as other)) ->
      type_error(
        st,
        "Failed to evaluate async dependencies of module '"
          <> resolved
          <> "': "
          <> module.error_message(other, st),
      )
  }
}

// PerformPromiseThen directly, never a then lookup
fn chain_deferred_settlement(
  st: Agent,
  ns: JsVal,
  pendings: List(#(String, Handle)),
  resolve_fn: JsVal,
  reject_fn: JsVal,
) -> Agent {
  case pendings {
    [] -> call_import_settle_fn(st, resolve_fn, ns)
    [#(dep_spec, tla_promise), ..rest] -> {
      let #(on_fulfilled, st) = {
        use st, _args <- module.alloc_host_fn(st, "%ContinueDeferredImport%", 0)
        // mark evaluated so a later deferred trigger can run
        let st = registry.write_module_status(st, dep_spec, registry.Evaluated)
        #(
          mk_undefined(),
          chain_deferred_settlement(st, ns, rest, resolve_fn, reject_fn),
        )
      }
      let #(on_rejected, st) = {
        use st, args <- module.alloc_host_fn(
          st,
          "%ContinueDeferredImportRejected%",
          1,
        )
        let reason = first_or_undefined(args)
        // entry stays uncached; a later import.defer relinks
        let st = registry.write_module_error(st, dep_spec, reason)
        #(mk_undefined(), call_import_settle_fn(st, reject_fn, reason))
      }
      let #(_child, st) =
        rt_async.t_promise_then(
          st,
          tla_promise,
          mk_object(on_fulfilled),
          mk_object(on_rejected),
        )
      st
    }
  }
}

fn first_or_undefined(args: List(JsVal)) -> JsVal {
  case args {
    [v, ..] -> v
    [] -> mk_undefined()
  }
}

// §27.2.1.3 resolving functions never throw
fn call_import_settle_fn(st: Agent, settle_fn: JsVal, arg: JsVal) -> Agent {
  case rt_call.t_call(st, settle_fn, mk_undefined(), [arg]) {
    #(rt_call.NormalCompletion(_), st) -> st
    #(rt_call.ThrowCompletion(thrown), st) -> {
      st.hooks.report_uncaught(
        "arc: import.defer settling function threw: "
        <> module.error_message(module.EvaluationError(thrown), st),
      )
      st
    }
  }
}

fn link_bundle_with_registry(
  st: Agent,
  bundle: module.ModuleBundle,
) -> #(Agent, Result(module.LinkedBundle, module.ModuleError)) {
  let specs = dict.keys(bundle.modules)
  let preexisting = registered(st, specs, registry.read_namespace)
  let preexisting_deferred =
    registered(st, specs, registry.read_deferred_namespace)
  case
    module.link_for_evaluation_reusing(
      bundle,
      st,
      preexisting,
      preexisting_deferred,
    )
  {
    #(st, Error(err)) -> #(st, Error(err))
    #(st, Ok(linked_bundle)) -> {
      let st =
        list.fold(module.linked_namespaces(linked_bundle, st), st, fn(st, pair) {
          let #(spec, ns) = pair
          case dict.has_key(preexisting, spec) {
            True -> st
            False -> registry.write_namespace(st, spec, ns)
          }
        })
      let st =
        list.fold(
          module.linked_deferred_namespaces(linked_bundle, st),
          st,
          fn(st, pair) {
            let #(spec, ns) = pair
            case dict.has_key(preexisting_deferred, spec) {
              True -> st
              False -> registry.write_deferred_namespace(st, spec, ns)
            }
          },
        )
      #(st, Ok(linked_bundle))
    }
  }
}

fn registered(
  st: Agent,
  specs: List(String),
  read: fn(Agent, String) -> option.Option(Handle),
) -> dict.Dict(String, Handle) {
  list.fold(specs, dict.new(), fn(acc, spec) {
    case read(st, spec) {
      option.Some(h) -> dict.insert(acc, spec, h)
      option.None -> acc
    }
  })
}

// §16.2.1.5.2 evaluate step 4: entry parked on tla
fn pending_module_promise(
  st: Agent,
  resolved: String,
  tla_promise: Handle,
) -> #(Agent, Result(JsVal, JsVal)) {
  case registry.read_namespace(st, resolved) {
    option.None ->
      type_error(st, "Module '" <> resolved <> "' produced no namespace")
    option.Some(namespace_h) -> {
      let namespace = mk_object(namespace_h)
      let #(#(ns_promise, ns_resolve, ns_reject), st) =
        rt_async.t_new_promise_capability(st)
      let #(on_fulfilled, st) = {
        use st, _args <- module.alloc_host_fn(st, "%FinishDynamicImport%", 0)
        let st =
          st
          |> registry.clear_pending_promise(resolved)
          |> registry.write_module_status(resolved, registry.Evaluated)
        #(namespace, st)
      }
      let #(on_rejected, st) = {
        use st, args <- module.alloc_host_fn(
          st,
          "%FinishDynamicImportRejected%",
          1,
        )
        let reason = first_or_undefined(args)
        let st =
          st
          |> registry.clear_pending_promise(resolved)
          |> registry.write_module_error(resolved, reason)
        rt_store.t_throw(st, reason)
      }
      let st =
        rt_async.t_perform_then(
          st,
          tla_promise,
          mk_object(on_fulfilled),
          mk_object(on_rejected),
          mk_object(ns_resolve),
          mk_object(ns_reject),
        )
      let st = registry.write_pending_promise(st, resolved, ns_promise)
      #(st, Ok(mk_object(ns_promise)))
    }
  }
}

// §16.2.1.8: register before evaluating, roll back bodies that never completed
pub fn evaluate_bundle_with_registry(
  st: Agent,
  bundle: module.ModuleBundle,
  finish: module.Finish,
) -> #(Agent, Result(module.EvaluatedBundle, module.ModuleError)) {
  let specs = dict.keys(bundle.modules)
  let preexisting = registered(st, specs, registry.read_namespace)
  case link_bundle_with_registry(st, bundle) {
    #(st, Error(err)) -> #(st, Error(err))
    #(st, Ok(linked_bundle)) -> {
      // linked-only modules still need their body run
      let already_evaluated =
        list.fold(specs, set.new(), fn(acc, spec) {
          case registry.read_module_status(st, spec) {
            option.Some(registry.Evaluated) -> set.insert(acc, spec)
            option.Some(registry.Evaluating) | option.None -> acc
          }
        })
      let #(st, evaluated, res) =
        module.evaluate_linked_tracking(
          linked_bundle,
          st,
          finish,
          already_evaluated,
        )
      case res {
        Ok(module.EvaluatedBundle(..)) -> #(st, res)
        Error(module.EvaluationError(value:)) -> {
          // host modules are not rolled back; their cells stay initialized
          let st =
            list.fold(module.source_specifiers(bundle), st, fn(st, spec) {
              case
                dict.has_key(preexisting, spec) || set.contains(evaluated, spec)
              {
                True -> st
                False -> registry.clear_module_registrations(st, spec)
              }
            })
          #(st, Error(module.EvaluationError(value:)))
        }
        // mid tla: registrations stay
        Error(module.EvaluationPending(promise: _))
        | Error(module.NotInBundle(..)) -> #(st, res)
      }
    }
  }
}

fn compile_bundle_rejection(
  st: Agent,
  err: module.CompileBundleError,
) -> #(Agent, Result(JsVal, JsVal)) {
  case err {
    module.GraphError(error: graph.ParseFailed(..))
    | module.GraphError(error: graph.SourcePhaseUnsupported(..))
    | module.CompileError(..) ->
      syntax_error(st, module.compile_bundle_error_message(err))
    module.GraphError(error: graph.ResolveFailed(..))
    | module.GraphError(error: graph.LoadFailed(..)) ->
      type_error(st, module.compile_bundle_error_message(err))
  }
}
