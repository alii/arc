// §13.3.10 import calls, failures reject the promise

import arc/module/registry
import arc/rt/async as rt_async
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, HostJob, KHandle, KStr, KUndef, StringKey,
  SyntaxErr, TypeErr, classify, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{type Option, None, Some}

const defer_phase_marker = "defer"

pub type HookPhase {
  EagerPhase
  DeferPhase(resolve_fn: JsVal, reject_fn: JsVal)
}

pub type HookCall {
  HookCall(specifier: String, referrer: Option(String), phase: HookPhase)
}

pub type HookArgError {
  MissingSpecifier
  NonStringSpecifier(found: JsVal)
  MissingResolve
  MissingReject
  BadPhase(found: JsVal)
}

pub fn hook_arg_error_message(err: HookArgError) -> String {
  case err {
    MissingSpecifier -> "import hook called without a specifier"
    NonStringSpecifier(_) -> "import hook called with a non-string specifier"
    MissingResolve | MissingReject ->
      "import hook called with the defer phase but no promise capability"
    BadPhase(_) -> "import hook called with unexpected arguments"
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
    DeferPhase(resolve_fn:, reject_fn:) -> [
      mk_string(specifier),
      referrer |> option.map(mk_string) |> option.unwrap(mk_undefined()),
      mk_string(defer_phase_marker),
      resolve_fn,
      reject_fn,
    ]
  }
}

pub fn parse_hook_args(args: List(JsVal)) -> Result(HookCall, HookArgError) {
  case args {
    [] -> Error(MissingSpecifier)
    [first, ..rest] ->
      case classify(first) {
        KStr(specifier) -> parse_hook_tail(specifier, rest)
        _ -> Error(NonStringSpecifier(first))
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
            [resolve_fn, reject_fn] ->
              Ok(HookCall(
                specifier:,
                referrer:,
                phase: DeferPhase(resolve_fn:, reject_fn:),
              ))
            [] -> Error(MissingResolve)
            [_] -> Error(MissingReject)
            [_, _, ..] -> Error(BadPhase(phase))
          }
        _ -> Error(BadPhase(phase))
      }
  }
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(Completion, Agent)

pub fn import_call(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  let #(promise, st) = rt_async.t_new_promise(st)
  let st = case import_request(st, specifier, options) {
    #(ThrowCompletion(reason), st) ->
      rt_async.t_promise_reject(st, promise, reason)
    #(NormalCompletion(specifier_string), st) -> {
      let hook_args =
        encode_hook_args(
          string_of(specifier_string),
          registry.read_active_referrer(st),
          EagerPhase,
        )
      use st <- enqueue_import_job(st, promise)
      call_host_hook(st, hook_args)
    }
  }
  #(mk_object(promise), st)
}

pub fn defer_import_call(st: Agent, specifier: JsVal) -> #(JsVal, Agent) {
  let #(promise, st) = rt_async.t_new_promise(st)
  let st = case import_request(st, specifier, mk_undefined()) {
    #(ThrowCompletion(reason), st) ->
      rt_async.t_promise_reject(st, promise, reason)
    #(NormalCompletion(specifier_string), st) -> {
      let #(#(resolve_h, reject_h), st) =
        rt_async.alloc_resolving_fns(st, promise)
      let resolve_fn = mk_object(resolve_h)
      let reject_fn = mk_object(reject_h)
      let hook_args =
        encode_hook_args(
          string_of(specifier_string),
          registry.read_active_referrer(st),
          DeferPhase(resolve_fn:, reject_fn:),
        )
      use st <- enqueue_host_job(st, [resolve_fn, reject_fn])
      case call_host_hook(st, hook_args) {
        #(st, Ok(_)) -> st
        #(st, Error(reason)) -> call_settle_fn(st, reject_fn, reason)
      }
    }
  }
  #(mk_object(promise), st)
}

pub fn source_import_call(st: Agent, specifier: JsVal) -> #(JsVal, Agent) {
  let #(promise, st) = rt_async.t_new_promise(st)
  let st = case import_request(st, specifier, mk_undefined()) {
    #(ThrowCompletion(reason), st) ->
      rt_async.t_promise_reject(st, promise, reason)
    #(NormalCompletion(_), st) -> {
      use st <- enqueue_import_job(st, promise)
      let #(err, st) =
        st.store.ops.new_error(
          st,
          SyntaxErr,
          "Module has no source phase representation",
        )
      #(st, Error(err))
    }
  }
  #(mk_object(promise), st)
}

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
) -> #(Completion, Agent) {
  use st <- protected(st)
  let #(specifier_string, st) = rt_val.t_to_string(st, specifier)
  let st = validate_options(st, options)
  #(mk_string(specifier_string), st)
}

fn validate_options(st: Agent, options: JsVal) -> Agent {
  case classify(options) {
    KUndef -> st
    KHandle(_) -> {
      let #(attributes, st) = rt_obj.t_get_prop(st, options, StringKey(nk.with))
      case classify(attributes) {
        KUndef -> st
        KHandle(attributes_h) -> validate_attributes(st, attributes_h)
        _ ->
          rt_val.t_throw_type_error(st, "The 'with' option must be an object")
      }
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "The second argument to import() must be an object",
      )
  }
}

fn validate_attributes(st: Agent, attributes: Handle) -> Agent {
  let #(keys, st) = rt_obj.t_enumerable_own_keys(st, attributes)
  let st =
    list.fold(keys, st, fn(st, key) {
      let #(v, st) =
        rt_obj.t_get_prop(st, mk_object(attributes), StringKey(key))
      case classify(v) {
        KStr(_) -> st
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Import attribute values must be strings",
          )
      }
    })
  case keys {
    [] -> st
    [key, ..] ->
      rt_val.t_throw_type_error(
        st,
        "Import attribute '"
          <> rt_store.t_key_text(st, key)
          <> "' is not supported",
      )
  }
}

fn enqueue_import_job(
  st: Agent,
  promise: Handle,
  settle: fn(Agent) -> #(Agent, Result(JsVal, JsVal)),
) -> Agent {
  let #(#(resolve_h, reject_h), st) = rt_async.alloc_resolving_fns(st, promise)
  let resolve_fn = mk_object(resolve_h)
  let reject_fn = mk_object(reject_h)
  use st <- enqueue_host_job(st, [resolve_fn, reject_fn])
  case settle(st) {
    #(st, Ok(v)) -> call_settle_fn(st, resolve_fn, v)
    #(st, Error(reason)) -> call_settle_fn(st, reject_fn, reason)
  }
}

// hold capability as roots, imported bodies collect mid-job
fn enqueue_host_job(
  st: Agent,
  capability: List(JsVal),
  run: fn(Agent) -> Agent,
) -> Agent {
  let job = fn(st) {
    let #(st, held) = rt_gc.t_hold_roots(st, capability)
    let #(outcome, st) = protected(st, fn(st) { #(mk_undefined(), run(st)) })
    let st = rt_gc.t_release_roots(st, held)
    case outcome {
      NormalCompletion(_) -> st
      ThrowCompletion(thrown) -> rt_store.t_throw(st, thrown)
    }
  }
  rt_async.t_enqueue_job(st, HostJob(run: job))
}

fn call_settle_fn(st: Agent, settle_fn: JsVal, arg: JsVal) -> Agent {
  let #(_, st) = rt_call.t_call(st, settle_fn, mk_undefined(), [arg])
  st
}

fn call_host_hook(
  st: Agent,
  hook_args: List(JsVal),
) -> #(Agent, Result(JsVal, JsVal)) {
  case st.import_hook {
    None -> {
      let #(err, st) =
        st.store.ops.new_error(
          st,
          TypeErr,
          "Dynamic import is not supported in this context",
        )
      #(st, Error(err))
    }
    Some(types.HostFnEntry(call:, ..)) -> {
      let outcome =
        protected(st, fn(st) {
          case call(st, hook_args, mk_undefined(), mk_undefined()) {
            #(st, Ok(v)) -> #(v, st)
            #(st, Error(thrown)) -> rt_store.t_throw(st, thrown)
          }
        })
      case outcome {
        #(NormalCompletion(v), st) -> #(st, Ok(v))
        #(ThrowCompletion(thrown), st) -> #(st, Error(thrown))
      }
    }
  }
}
