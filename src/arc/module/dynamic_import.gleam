// §13.3.10 import calls, failures reject the promise

import arc/bytecode/error_kind.{SyntaxError, TypeError}
import arc/bytecode/key.{Named}
import arc/module/loader
import arc/module/registry
import arc/parser/ast.{type ImportAttribute, ImportAttribute}
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
import gleam/result
import gleam/string

const evaluation_phase_marker = "evaluation"

const defer_phase_marker = "defer"

pub type HookPhase {
  EagerPhase
  DeferPhase(fulfill: JsVal, reject: JsVal)
}

// modulerequest record plus referrer and phase
pub type HookCall {
  HookCall(
    specifier: String,
    referrer: Option(String),
    attributes: List(ImportAttribute),
    phase: HookPhase,
  )
}

pub type HookArgError {
  MissingSpecifier
  TooFewArguments
  NonStringSpecifier
  MissingResolve
  MissingReject
  BadPhase
  BadAttributes
}

pub fn hook_arg_error_message(err: HookArgError) -> String {
  case err {
    MissingSpecifier -> "import hook called without a specifier"
    TooFewArguments -> "import hook called with too few arguments"
    NonStringSpecifier -> "import hook called with a non-string specifier"
    MissingResolve | MissingReject ->
      "import hook called with the defer phase but no promise capability"
    BadPhase -> "import hook called with an unknown phase"
    BadAttributes -> "import hook called with malformed import attributes"
  }
}

// [specifier, referrer, phase, fulfill, reject, key, value, ...]
pub fn encode_hook_args(call: HookCall) -> List(JsVal) {
  let HookCall(specifier:, referrer:, attributes:, phase:) = call
  let #(marker, fulfill, reject) = case phase {
    EagerPhase -> #(evaluation_phase_marker, mk_undefined(), mk_undefined())
    DeferPhase(fulfill:, reject:) -> #(defer_phase_marker, fulfill, reject)
  }
  let head = [
    mk_string(specifier),
    referrer |> option.map(mk_string) |> option.unwrap(mk_undefined()),
    mk_string(marker),
    fulfill,
    reject,
  ]
  list.append(
    head,
    list.flat_map(attributes, fn(a) { [mk_string(a.key), mk_string(a.value)] }),
  )
}

pub fn parse_hook_args(args: List(JsVal)) -> Result(HookCall, HookArgError) {
  case args {
    [] -> Error(MissingSpecifier)
    [specifier, referrer, marker, fulfill, reject, ..attributes] -> {
      use specifier <- result.try(
        string_arg(specifier) |> option.to_result(NonStringSpecifier),
      )
      use attributes <- result.try(parse_attribute_args(attributes, []))
      use phase <- result.map(parse_phase(marker, fulfill, reject))
      HookCall(specifier:, referrer: string_arg(referrer), attributes:, phase:)
    }
    [_, ..] -> Error(TooFewArguments)
  }
}

fn string_arg(v: JsVal) -> Option(String) {
  case classify(v) {
    KStr(s) -> Some(s)
    _ -> None
  }
}

fn parse_phase(
  marker: JsVal,
  fulfill: JsVal,
  reject: JsVal,
) -> Result(HookPhase, HookArgError) {
  case string_arg(marker), classify(fulfill), classify(reject) {
    Some(m), _, _ if m == evaluation_phase_marker -> Ok(EagerPhase)
    Some(m), KUndef, _ if m == defer_phase_marker -> Error(MissingResolve)
    Some(m), _, KUndef if m == defer_phase_marker -> Error(MissingReject)
    Some(m), _, _ if m == defer_phase_marker -> Ok(DeferPhase(fulfill:, reject:))
    _, _, _ -> Error(BadPhase)
  }
}

fn parse_attribute_args(
  args: List(JsVal),
  acc: List(ImportAttribute),
) -> Result(List(ImportAttribute), HookArgError) {
  case args {
    [] -> Ok(list.reverse(acc))
    [k, v, ..rest] ->
      case string_arg(k), string_arg(v) {
        Some(key), Some(value) ->
          parse_attribute_args(rest, [ImportAttribute(key:, value:), ..acc])
        _, _ -> Error(BadAttributes)
      }
    [_] -> Error(BadAttributes)
  }
}

pub fn import_call(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  use request, promise, st <- with_import_request(st, specifier, options)
  let hook_args =
    encode_hook_args(HookCall(
      specifier: request.specifier,
      referrer: registry.read_active_referrer(st),
      attributes: request.attributes,
      phase: EagerPhase,
    ))
  use st <- enqueue_import_job(st, promise)
  call_host_hook(st, hook_args)
}

pub fn defer_import_call(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
) -> #(JsVal, Agent) {
  use request, promise, st <- with_import_request(st, specifier, options)
  let #(rt_async.ResolvingFunctions(fulfill_h, reject_h), st) =
    rt_async.alloc_resolving_fns(st, promise)
  let fulfill = mk_object(fulfill_h)
  let reject = mk_object(reject_h)
  let hook_args =
    encode_hook_args(HookCall(
      specifier: request.specifier,
      referrer: registry.read_active_referrer(st),
      attributes: request.attributes,
      phase: DeferPhase(fulfill:, reject:),
    ))
  use st <- enqueue_host_job(st, [fulfill, reject])
  case call_host_hook(st, hook_args) {
    #(Ok(_), st) -> st
    #(Error(reason), st) -> call_settle_fn(st, reject, reason)
  }
}

pub fn source_import_call(st: Agent, specifier: JsVal) -> #(JsVal, Agent) {
  use _request, promise, st <- with_import_request(
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

// modulerequest record
type ImportRequest {
  ImportRequest(specifier: String, attributes: List(ImportAttribute))
}

// a throwing request rejects the promise and skips k
fn with_import_request(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
  k: fn(ImportRequest, Handle, Agent) -> Agent,
) -> #(JsVal, Agent) {
  let #(promise, st) = rt_async.new_promise(st)
  let st = case import_request(st, specifier, options) {
    #(ThrowCompletion(reason), st) ->
      rt_async.promise_reject(st, promise, reason)
    #(NormalCompletion(request), st) -> k(request, promise, st)
  }
  #(mk_object(promise), st)
}

// evaluateimportcall steps 7-11
fn import_request(
  st: Agent,
  specifier: JsVal,
  options: JsVal,
) -> #(Completion(ImportRequest), Agent) {
  use st <- rt_call.try_run(st)
  let #(specifier, st) = rt_val.to_string(st, specifier)
  let #(attributes, st) = import_attributes(st, options)
  #(ImportRequest(specifier:, attributes:), st)
}

// evaluateimportcall step 10
fn import_attributes(
  st: Agent,
  options: JsVal,
) -> #(List(ImportAttribute), Agent) {
  case classify(options) {
    KUndef -> #([], st)
    KHandle(_) -> {
      let #(attributes_obj, st) =
        rt_obj.get_prop(st, options, StringKey(Named("with")))
      case classify(attributes_obj) {
        KUndef -> #([], st)
        KHandle(h) -> {
          let #(entries, st) = rt_obj.enumerable_own_entries(st, h)
          #(validate_attributes(st, entries), st)
        }
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

// evaluateimportcall steps 10.d.iv-10.f
fn validate_attributes(
  st: Agent,
  entries: List(#(String, JsVal)),
) -> List(ImportAttribute) {
  let attributes =
    list.map(entries, fn(entry) {
      case classify(entry.1) {
        KStr(value) -> ImportAttribute(key: entry.0, value:)
        _ ->
          rt_val.throw_type_error(st, "Import attribute values must be strings")
      }
    })
  case loader.unsupported_attribute(attributes) {
    Some(unsupported) ->
      rt_val.throw_type_error(
        st,
        "Import attribute '" <> unsupported <> "' is not supported",
      )
    None -> list.sort(attributes, fn(a, b) { string.compare(a.key, b.key) })
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
