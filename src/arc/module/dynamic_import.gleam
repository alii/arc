// §13.3.10 import calls, failures reject the promise

import arc/bytecode/error_kind.{SyntaxError, TypeError}
import arc/bytecode/key.{Named}
import arc/module/registry
import arc/rt/async as rt_async
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, HostJob, KHandle, KStr, KUndef, StringKey,
  classify, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{type Option, None, Some}

const defer_phase_marker = "defer"

pub type HookPhase {
  EagerPhase
  DeferPhase(fulfill: JsVal, reject: JsVal)
}

pub type HookCall {
  HookCall(specifier: String, referrer: Option(String), phase: HookPhase)
}

pub type HookArgError {
  MissingSpecifier
  NonStringSpecifier
  MissingResolve
  MissingReject
  BadPhase
}

pub fn hook_arg_error_message(err: HookArgError) -> String {
  case err {
    MissingSpecifier -> "import hook called without a specifier"
    NonStringSpecifier -> "import hook called with a non-string specifier"
    MissingResolve | MissingReject ->
      "import hook called with the defer phase but no promise capability"
    BadPhase -> "import hook called with unexpected arguments"
  }
}

pub fn encode_hook_args(
  specifier: String,
  referrer: Option(String),
  phase: HookPhase,
) -> List(JsVal) {
  case phase {
    EagerPhase ->
      case referrer {
        Some(referrer) -> [mk_string(specifier), mk_string(referrer)]
        None -> [mk_string(specifier)]
      }
    DeferPhase(fulfill:, reject:) -> [
      mk_string(specifier),
      referrer |> option.map(mk_string) |> option.unwrap(mk_undefined()),
      mk_string(defer_phase_marker),
      fulfill,
      reject,
    ]
  }
}

pub fn parse_hook_args(args: List(JsVal)) -> Result(HookCall, HookArgError) {
  case args {
    [] -> Error(MissingSpecifier)
    [first, ..rest] ->
      case classify(first) {
        KStr(specifier) -> parse_hook_tail(specifier, rest)
        _ -> Error(NonStringSpecifier)
      }
  }
}

fn parse_hook_tail(
  specifier: String,
  rest: List(JsVal),
) -> Result(HookCall, HookArgError) {
  let referrer = case list.first(rest) |> option.from_result {
    Some(v) ->
      case classify(v) {
        KStr(referrer) -> Some(referrer)
        _ -> None
      }
    None -> None
  }
  case rest {
    [] | [_] -> Ok(HookCall(specifier:, referrer:, phase: EagerPhase))
    [_, phase, ..capability] ->
      case classify(phase) {
        KStr(marker) if marker == defer_phase_marker ->
          case capability {
            [fulfill, reject] ->
              Ok(HookCall(
                specifier:,
                referrer:,
                phase: DeferPhase(fulfill:, reject:),
              ))
            [] -> Error(MissingResolve)
            [_] -> Error(MissingReject)
            [_, _, ..] -> Error(BadPhase)
          }
        _ -> Error(BadPhase)
      }
  }
}

pub fn import_call(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  use specifier, promise, st <- with_import_request(st, specifier, options)
  let hook_args =
    encode_hook_args(
      string_of(specifier),
      registry.read_active_referrer(st),
      EagerPhase,
    )
  use st <- enqueue_import_job(st, promise)
  call_host_hook(st, hook_args)
}

pub fn defer_import_call(st: Agent, specifier: JsVal) -> #(JsVal, Agent) {
  use specifier, promise, st <- with_import_request(
    st,
    specifier,
    mk_undefined(),
  )
  let #(rt_async.ResolvingFunctions(fulfill_h, reject_h), st) =
    rt_async.alloc_resolving_fns(st, promise)
  let fulfill = mk_object(fulfill_h)
  let reject = mk_object(reject_h)
  let hook_args =
    encode_hook_args(
      string_of(specifier),
      registry.read_active_referrer(st),
      DeferPhase(fulfill:, reject:),
    )
  use st <- enqueue_host_job(st, [fulfill, reject])
  case call_host_hook(st, hook_args) {
    #(Ok(_), st) -> st
    #(Error(reason), st) -> call_settle_fn(st, reject, reason)
  }
}

pub fn source_import_call(st: Agent, specifier: JsVal) -> #(JsVal, Agent) {
  use _specifier, promise, st <- with_import_request(
    st,
    specifier,
    mk_undefined(),
  )
  use st <- enqueue_import_job(st, promise)
  let #(err, st) =
    rt_val.new_error(
      st,
      SyntaxError,
      "Module has no source phase representation",
    )
  #(Error(err), st)
}

// a throwing request rejects the promise and skips k
fn with_import_request(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
  k: fn(JsVal, Handle, Agent) -> Agent,
) -> #(JsVal, Agent) {
  let #(promise, st) = rt_async.new_promise(st)
  let st = case import_request(st, specifier, options) {
    #(ThrowCompletion(reason), st) ->
      rt_async.promise_reject(st, promise, reason)
    #(NormalCompletion(specifier), st) -> k(specifier, promise, st)
  }
  #(mk_object(promise), st)
}

// import_request only completes with a string
fn string_of(v: JsVal) -> String {
  case classify(v) {
    KStr(s) -> s
    _ -> ""
  }
}

fn import_request(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
) -> #(Completion(JsVal), Agent) {
  use st <- rt_call.try_run(st)
  let #(specifier_string, st) = rt_val.to_string(st, specifier)
  let st = validate_options(st, options)
  #(mk_string(specifier_string), st)
}

fn validate_options(st: Agent, options: JsVal) -> Agent {
  case classify(options) {
    KUndef -> st
    KHandle(_) -> {
      let #(attributes, st) =
        rt_obj.get_prop(st, options, StringKey(Named("with")))
      case classify(attributes) {
        KUndef -> st
        KHandle(attributes_h) -> validate_attributes(st, attributes_h)
        _ -> rt_val.throw_type_error(st, "The 'with' option must be an object")
      }
    }
    _ ->
      rt_val.throw_type_error(
        st,
        "The second argument to import() must be an object",
      )
  }
}

fn validate_attributes(st: Agent, attributes: Handle) -> Agent {
  let #(keys, st) = rt_obj.enumerable_own_keys(st, attributes)
  let st =
    list.fold(keys, st, fn(st, pk) {
      let #(v, st) = rt_obj.get_prop(st, mk_object(attributes), StringKey(pk))
      case classify(v) {
        KStr(_) -> st
        _ ->
          rt_val.throw_type_error(st, "Import attribute values must be strings")
      }
    })
  case keys {
    [] -> st
    [pk, ..] ->
      rt_val.throw_type_error(
        st,
        "Import attribute '" <> key.to_text(pk) <> "' is not supported",
      )
  }
}

fn enqueue_import_job(
  st: Agent,
  promise: Handle,
  settle: fn(Agent) -> #(Result(JsVal, JsVal), Agent),
) -> Agent {
  let #(rt_async.ResolvingFunctions(fulfill_h, reject_h), st) =
    rt_async.alloc_resolving_fns(st, promise)
  let fulfill = mk_object(fulfill_h)
  let reject = mk_object(reject_h)
  use st <- enqueue_host_job(st, [fulfill, reject])
  case settle(st) {
    #(Ok(v), st) -> call_settle_fn(st, fulfill, v)
    #(Error(reason), st) -> call_settle_fn(st, reject, reason)
  }
}

// hold capability as roots, imported bodies collect mid-job
fn enqueue_host_job(
  st: Agent,
  capability: List(JsVal),
  run: fn(Agent) -> Agent,
) -> Agent {
  let job = fn(st) {
    let #(held, st) = rt_gc.hold_roots(st, capability)
    let #(outcome, st) =
      rt_call.try_run(st, fn(st) { #(mk_undefined(), run(st)) })
    let st = rt_gc.release_roots(st, held)
    case outcome {
      NormalCompletion(_) -> st
      ThrowCompletion(thrown) -> rt_store.throw(st, thrown)
    }
  }
  rt_async.enqueue_job(st, HostJob(run: job))
}

fn call_settle_fn(st: Agent, settle_fn: JsVal, arg: JsVal) -> Agent {
  let #(_, st) = rt_call.try_call(st, settle_fn, mk_undefined(), [arg])
  st
}

fn call_host_hook(
  st: Agent,
  hook_args: List(JsVal),
) -> #(Result(JsVal, JsVal), Agent) {
  case st.import_hook {
    None -> {
      let #(err, st) =
        rt_val.new_error(
          st,
          TypeError,
          "Dynamic import is not supported in this context",
        )
      #(Error(err), st)
    }
    Some(types.HostFnEntry(call:)) -> {
      let outcome =
        rt_call.try_run(st, fn(st) {
          case call(st, hook_args, mk_undefined(), mk_undefined()) {
            #(Ok(v), st) -> #(v, st)
            #(Error(thrown), st) -> rt_store.throw(st, thrown)
          }
        })
      case outcome {
        #(NormalCompletion(v), st) -> #(Ok(v), st)
        #(ThrowCompletion(thrown), st) -> #(Error(thrown), st)
      }
    }
  }
}
