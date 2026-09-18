import arc/bytecode/error_kind.{SyntaxError, TypeError}
import arc/module
import arc/module/dynamic_import
import arc/module/graph
import arc/module/loader.{type LoadFn, type ResolveFn}
import arc/module/registry
import arc/rt/async as rt_async
import arc/rt/call as rt_call
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, Agent, HostFnEntry, mk_object,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option
import gleam/set

/// installs the §16.2.1.8 dynamic import hook; reinstall after deserialize
pub fn install_import_hook(
  st: Agent,
  referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> Agent {
  let hook =
    HostFnEntry(name: "%DynamicImportHook%", call: fn(st, args, _this, _nt) {
      import_module(st, args, referrer, resolve, load)
    })
  Agent(..st, import_hook: option.Some(hook))
}

fn type_error(st: Agent, msg: String) -> #(Result(JsVal, JsVal), Agent) {
  let #(err, st) = rt_val.new_error(st, TypeError, msg)
  #(Error(err), st)
}

fn syntax_error(st: Agent, msg: String) -> #(Result(JsVal, JsVal), Agent) {
  let #(err, st) = rt_val.new_error(st, SyntaxError, msg)
  #(Error(err), st)
}

// Error(thrown) rejects with thrown; the defer arm settles the promise itself
fn import_module(
  st: Agent,
  args: List(JsVal),
  entry_referrer: String,
  resolve: ResolveFn,
  load: LoadFn,
) -> #(Result(JsVal, JsVal), Agent) {
  case dynamic_import.parse_hook_args(args) {
    Error(err) -> type_error(st, dynamic_import.hook_arg_error_message(err))
    Ok(dynamic_import.HookCall(specifier:, referrer:, phase:)) -> {
      let referrer = option.unwrap(referrer, entry_referrer)
      case resolve(specifier, referrer) {
        Error(err) ->
          type_error(
            st,
            loader.resolve_failure_message(specifier, referrer, err),
          )
        Ok(resolved) ->
          case phase {
            dynamic_import.DeferPhase(fulfill:, reject:) ->
              defer_import_module(st, resolved, resolve, load, fulfill, reject)
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
) -> #(Result(JsVal, JsVal), Agent) {
  case registry.lookup(st, resolved) {
    // error cache wins: a namespace entry may be stale after a throw
    registry.Failed(error:) -> #(Error(error), st)
    // parked on tla: same in-flight promise (Evaluate step 4)
    registry.Pending(promise:, deferred: _) -> #(Ok(mk_object(promise)), st)
    registry.EvaluationStarted(namespace:, deferred: _) -> #(
      Ok(mk_object(namespace)),
      st,
    )
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
) -> #(Result(JsVal, JsVal), Agent) {
  use source <- with_loaded_source(st, resolved, load)
  case module.compile_bundle(resolved, source, resolve, load) {
    Error(err) -> compile_bundle_rejection(st, err)
    Ok(bundle) -> {
      // evaluate without draining: we are inside a promise job
      let #(res, st) =
        evaluate_bundle_with_registry(st, bundle, rt_async.no_drain)
      case res {
        Ok(module.EvaluatedBundle(value: _, namespace:)) -> #(
          Ok(mk_object(namespace)),
          st,
        )
        Error(module.EvaluationError(value: thrown)) -> {
          let st = registry.write_module_error(st, resolved, thrown)
          #(Error(thrown), st)
        }
        Error(module.EvaluationPending(promise:)) ->
          pending_module_promise(st, resolved, promise)
        Error(module.NotInBundle(..) as other) ->
          type_error(
            st,
            "Failed to evaluate module '"
              <> resolved
              <> "': "
              <> module.error_message(st, other),
          )
      }
    }
  }
}

// import.defer: link, pre-evaluate async deps, settle via fulfill
fn defer_import_module(
  st: Agent,
  resolved: String,
  resolve: ResolveFn,
  load: LoadFn,
  fulfill: JsVal,
  reject: JsVal,
) -> #(Result(JsVal, JsVal), Agent) {
  case registry.lookup(st, resolved) {
    registry.Failed(error:) -> #(Error(error), st)
    registry.Pending(deferred: option.Some(deferred_ns), ..)
    | registry.EvaluationStarted(deferred: option.Some(deferred_ns), ..)
    | registry.LinkedOnly(deferred: option.Some(deferred_ns), ..)
    | registry.Absent(deferred: option.Some(deferred_ns)) ->
      settle_defer_import(st, fulfill, mk_object(deferred_ns))
    registry.Pending(deferred: option.None, ..)
    | registry.EvaluationStarted(deferred: option.None, ..)
    | registry.LinkedOnly(deferred: option.None, ..)
    | registry.Absent(deferred: option.None) -> {
      use source <- with_loaded_source(st, resolved, load)
      case module.compile_bundle(resolved, source, resolve, load) {
        Error(err) -> compile_bundle_rejection(st, err)
        Ok(bundle) ->
          case link_bundle_with_registry(st, bundle) {
            #(Error(module.EvaluationError(value: thrown)), st) -> #(
              Error(thrown),
              st,
            )
            #(Error(other), st) ->
              type_error(
                st,
                "Failed to link module '"
                  <> resolved
                  <> "': "
                  <> module.error_message(st, other),
              )
            #(Ok(linked_bundle), st) ->
              case
                module.get_or_create_deferred_namespace(
                  st,
                  linked_bundle,
                  resolved,
                )
              {
                #(Ok(ns), st) -> {
                  let st = registry.write_deferred_namespace(st, resolved, ns)
                  evaluate_deferred_async_deps(
                    st,
                    resolved,
                    mk_object(ns),
                    linked_bundle,
                    fulfill,
                    reject,
                  )
                }
                #(Error(module.DeferredSpecifierNotInBundle(specifier:)), st) ->
                  type_error(st, "Cannot find module '" <> specifier <> "'")
              }
          }
      }
    }
  }
}

fn settle_defer_import(
  st: Agent,
  fulfill: JsVal,
  value: JsVal,
) -> #(Result(JsVal, JsVal), Agent) {
  #(Ok(mk_undefined()), call_import_settle_fn(st, fulfill, value))
}

fn with_loaded_source(
  st: Agent,
  resolved: String,
  load: LoadFn,
  then: fn(String) -> #(Result(JsVal, JsVal), Agent),
) -> #(Result(JsVal, JsVal), Agent) {
  case load(resolved) {
    Error(err) -> type_error(st, loader.load_failure_message(resolved, err))
    Ok(source) -> then(source)
  }
}

fn evaluate_deferred_async_deps(
  st: Agent,
  resolved: String,
  ns: JsVal,
  linked_bundle: module.LinkedBundle,
  fulfill: JsVal,
  reject: JsVal,
) -> #(Result(JsVal, JsVal), Agent) {
  case
    module.evaluate_async_transitive_deps(st, linked_bundle, rt_async.no_drain)
  {
    #(Ok([]), st) -> settle_defer_import(st, fulfill, ns)
    #(Ok(pendings), st) -> #(
      Ok(mk_undefined()),
      chain_deferred_settlement(st, ns, pendings, fulfill, reject),
    )
    #(Error(module.EvaluationError(value: thrown)), st) -> {
      let st = registry.write_module_error(st, resolved, thrown)
      #(Error(thrown), st)
    }
    #(Error(module.NotInBundle(..) as other), st)
    | #(Error(module.EvaluationPending(..) as other), st) ->
      type_error(
        st,
        "Failed to evaluate async dependencies of module '"
          <> resolved
          <> "': "
          <> module.error_message(st, other),
      )
  }
}

// PerformPromiseThen directly, never a then lookup
fn chain_deferred_settlement(
  st: Agent,
  ns: JsVal,
  pendings: List(#(String, Handle)),
  fulfill: JsVal,
  reject: JsVal,
) -> Agent {
  case pendings {
    [] -> call_import_settle_fn(st, fulfill, ns)
    [#(dep_spec, tla_promise), ..rest] -> {
      let #(on_fulfilled, st) = {
        use st, _args <- rt_call.new_builtin_function(
          st,
          "%ContinueDeferredImport%",
          0,
        )
        // mark evaluated so a later deferred trigger can run
        let st = registry.write_module_status(st, dep_spec, registry.Evaluated)
        #(
          mk_undefined(),
          chain_deferred_settlement(st, ns, rest, fulfill, reject),
        )
      }
      let #(on_rejected, st) = {
        use st, args <- rt_call.new_builtin_function(
          st,
          "%ContinueDeferredImportRejected%",
          1,
        )
        let reason = first_or_undefined(args)
        // entry stays uncached; a later import.defer relinks
        let st = registry.write_module_error(st, dep_spec, reason)
        #(mk_undefined(), call_import_settle_fn(st, reject, reason))
      }
      let #(_child, st) =
        rt_async.promise_then(
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
  case rt_call.try_call(st, settle_fn, mk_undefined(), [arg]) {
    #(rt_call.NormalCompletion(_), st) -> st
    #(rt_call.ThrowCompletion(thrown), st) -> {
      st.hooks.report_uncaught(
        "arc: import.defer settling function threw: "
        <> module.error_message(st, module.EvaluationError(thrown)),
      )
      st
    }
  }
}

fn link_bundle_with_registry(
  st: Agent,
  bundle: module.ModuleBundle,
) -> #(Result(module.LinkedBundle, module.ModuleError), Agent) {
  let specs = dict.keys(bundle.modules)
  let preexisting = read_registered(st, specs, registry.read_namespace)
  let preexisting_deferred =
    read_registered(st, specs, registry.read_deferred_namespace)
  case
    module.link_for_evaluation_reusing(
      st,
      bundle,
      preexisting,
      preexisting_deferred,
    )
  {
    #(Error(err), st) -> #(Error(err), st)
    #(Ok(linked_bundle), st) -> {
      let st =
        list.fold(module.linked_namespaces(st, linked_bundle), st, fn(st, pair) {
          let #(spec, ns) = pair
          case dict.has_key(preexisting, spec) {
            True -> st
            False -> registry.write_namespace(st, spec, ns)
          }
        })
      let st =
        list.fold(
          module.linked_deferred_namespaces(st, linked_bundle),
          st,
          fn(st, pair) {
            let #(spec, ns) = pair
            case dict.has_key(preexisting_deferred, spec) {
              True -> st
              False -> registry.write_deferred_namespace(st, spec, ns)
            }
          },
        )
      #(Ok(linked_bundle), st)
    }
  }
}

fn read_registered(
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
) -> #(Result(JsVal, JsVal), Agent) {
  case registry.read_namespace(st, resolved) {
    option.None ->
      type_error(st, "Module '" <> resolved <> "' produced no namespace")
    option.Some(namespace_h) -> {
      let namespace = mk_object(namespace_h)
      let #(#(ns_promise, ns_resolve, ns_reject), st) =
        rt_async.new_promise_capability(st)
      let #(on_fulfilled, st) = {
        use st, _args <- rt_call.new_builtin_function(
          st,
          "%FinishDynamicImport%",
          0,
        )
        let st =
          st
          |> registry.clear_pending_promise(resolved)
          |> registry.write_module_status(resolved, registry.Evaluated)
        #(namespace, st)
      }
      let #(on_rejected, st) = {
        use st, args <- rt_call.new_builtin_function(
          st,
          "%FinishDynamicImportRejected%",
          1,
        )
        let reason = first_or_undefined(args)
        let st =
          st
          |> registry.clear_pending_promise(resolved)
          |> registry.write_module_error(resolved, reason)
        rt_store.throw(st, reason)
      }
      let st =
        rt_async.perform_then(
          st,
          tla_promise,
          mk_object(on_fulfilled),
          mk_object(on_rejected),
          mk_object(ns_resolve),
          mk_object(ns_reject),
        )
      let st = registry.write_pending_promise(st, resolved, ns_promise)
      #(Ok(mk_object(ns_promise)), st)
    }
  }
}

// §16.2.1.8: register before evaluating, roll back bodies that never completed
pub fn evaluate_bundle_with_registry(
  st: Agent,
  bundle: module.ModuleBundle,
  drain: rt_async.Drain,
) -> #(Result(module.EvaluatedBundle, module.ModuleError), Agent) {
  let specs = dict.keys(bundle.modules)
  let preexisting = read_registered(st, specs, registry.read_namespace)
  case link_bundle_with_registry(st, bundle) {
    #(Error(err), st) -> #(Error(err), st)
    #(Ok(linked_bundle), st) -> {
      // linked-only modules still need their body run
      let already_instantiated =
        list.fold(specs, set.new(), fn(acc, spec) {
          case registry.read_module_status(st, spec) {
            option.Some(registry.Evaluated) -> set.insert(acc, spec)
            option.Some(registry.Evaluating) | option.None -> acc
          }
        })
      let #(evaluated, res, st) =
        module.evaluate_linked_tracking(
          st,
          linked_bundle,
          drain,
          already_instantiated,
        )
      case res {
        Ok(module.EvaluatedBundle(..)) -> #(res, st)
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
          #(Error(module.EvaluationError(value:)), st)
        }
        // mid tla: registrations stay
        Error(module.EvaluationPending(promise: _))
        | Error(module.NotInBundle(..)) -> #(res, st)
      }
    }
  }
}

fn compile_bundle_rejection(
  st: Agent,
  err: module.CompileBundleError,
) -> #(Result(JsVal, JsVal), Agent) {
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
