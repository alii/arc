//// Language-level runtime ops that compiled code calls for syntax the core
//// object model does not cover on its own: the iterator protocol behind
//// for-of / spread / array destructuring, CopyDataProperties for object
//// spread and rest, regexp and template literals, sloppy `delete x`.
//// Sits above `arc/rt/builtins/*` so it can reuse their spec routines.

import arc/rt/builtins/iter_protocol
import arc/rt/builtins/object as b_object
import arc/rt/builtins/regexp as b_regexp
import arc/rt/call.{t_call_checked}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type IteratorRecord, type JsVal, type ObjectKey,
  Agent, IteratorRecord, KHandle, KNull, KUndef, Named, StringKey, TypeErr,
  classify, mk_bool, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{Some}

/// Same try/catch as `t_call`; the wire shape is `arc/rt/call.Completion`
/// with the step pair in the normal arm.
type StepOutcome {
  NormalCompletion(#(Bool, JsVal))
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected_step(
  st: Agent,
  body: fn(Agent) -> #(#(Bool, JsVal), Agent),
) -> #(StepOutcome, Agent)

@external(erlang, "arc_rt_store_ffi", "as_object_key")
fn as_object_key(key: k) -> ObjectKey

/// TypeError with `message`, not thrown. The generator state machine's
/// default arm wraps it in a throw completion itself.
pub fn t_new_error(st: Agent, message: String) -> #(JsVal, Agent) {
  st.store.ops.new_error(st, TypeErr, message)
}

// ── §7.4 Iterator Records ───────────────────────────────────────────────────
//
// The record is an ordinary null-proto object with own data properties
// `iterator`, `next` and `done`, so it is an ordinary traced heap value that
// compiled code can keep in a local or a coroutine frame across suspension.
// `done` is the spec's [[Done]]: once set, `t_iter_next` short-circuits and
// `t_iter_close` is a no-op (QuickJS tracks the same bit on the stack slot).

pub type IterHint {
  Sync
  Async
}

const k_iterator = StringKey(Named("iterator"))

const k_next = StringKey(Named("next"))

const k_done = StringKey(Named("done"))

fn alloc_record(st: Agent, rec: IteratorRecord) -> #(JsVal, Agent) {
  let #(h, st) = rt_obj.t_new_object(st, option.None)
  let #(_, st) =
    rt_obj.t_define_own_data(st, h, k_iterator, rec.iterator, True, True, True)
  let #(_, st) =
    rt_obj.t_define_own_data(st, h, k_next, rec.next_method, True, True, True)
  let #(_, st) =
    rt_obj.t_define_own_data(st, h, k_done, mk_bool(False), True, True, True)
  #(mk_object(h), st)
}

fn read_record(st: Agent, rec: JsVal) -> #(Bool, IteratorRecord, Agent) {
  let #(done, st) = rt_obj.t_get_prop(st, rec, k_done)
  let #(iterator, st) = rt_obj.t_get_prop(st, rec, k_iterator)
  let #(next_method, st) = rt_obj.t_get_prop(st, rec, k_next)
  #(rt_val.to_boolean(done), IteratorRecord(iterator:, next_method:), st)
}

fn mark_done(st: Agent, rec: JsVal) -> Agent {
  let #(_, st) = rt_obj.t_set_prop(st, rec, k_done, mk_bool(True))
  st
}

/// §7.4.3 GetIterator(obj, hint). Returns the record object.
pub fn t_get_iterator(
  st: Agent,
  obj: JsVal,
  hint: IterHint,
) -> #(JsVal, Agent) {
  let #(rec, st) = case hint {
    Sync -> iter_protocol.get_iterator_sync(st, obj)
    Async -> iter_protocol.get_iterator_async(st, obj)
  }
  alloc_record(st, rec)
}

/// §7.4.8 IteratorStepValue as a `#(done, value)` pair. An abrupt completion
/// from `next()`/`done`/`value` marks the record done before propagating so
/// a surrounding IteratorClose skips `.return()` (§14.7.5.7 step 6.b).
pub fn t_iter_next(st: Agent, rec: JsVal) -> #(#(Bool, JsVal), Agent) {
  let #(done, record, st) = read_record(st, rec)
  case done {
    True -> #(#(True, mk_undefined()), st)
    False -> {
      let step = fn(st) {
        use result, done, st <- iter_protocol.iterator_step_result(st, record)
        case done {
          True -> #(#(True, mk_undefined()), st)
          False -> {
            let #(v, st) =
              rt_obj.t_get_prop(st, result, StringKey(Named("value")))
            #(#(False, v), st)
          }
        }
      }
      case protected_step(st, step) {
        #(NormalCompletion(#(True, _) as pair), st) -> #(
          pair,
          mark_done(st, rec),
        )
        #(NormalCompletion(pair), st) -> #(pair, st)
        #(ThrowCompletion(thrown), st) ->
          rt_store.t_throw(mark_done(st, rec), thrown)
      }
    }
  }
}

/// §7.4.11 IteratorClose. `abrupt` selects the throw-completion rules
/// (call `.return()`, swallow whatever it does; the caller rethrows the
/// original) over the normal-completion rules (a throwing or non-object
/// `.return()` result propagates). No-op once the record is done.
pub fn t_iter_close(st: Agent, rec: JsVal, abrupt: Bool) -> Agent {
  let #(done, record, st) = read_record(st, rec)
  case done {
    True -> st
    False -> {
      let st = mark_done(st, rec)
      case abrupt {
        True -> {
          let #(_ignored, st) = iter_protocol.call_return(st, record.iterator)
          st
        }
        False -> iter_protocol.iterator_close_normal(st, record.iterator)
      }
    }
  }
}

/// §14.3.3 rest element: drain what is left of the record into a new Array.
/// [[Done]] is set first, so no completion after this closes the iterator.
pub fn t_iter_rest(st: Agent, rec: JsVal) -> #(JsVal, Agent) {
  let #(done, record, st) = read_record(st, rec)
  case done {
    True -> rt_obj.t_new_array(st, [])
    False -> {
      let st = mark_done(st, rec)
      let #(values, st) = iter_protocol.iterator_to_list(st, record)
      rt_obj.t_new_array(st, values)
    }
  }
}

/// Spread element in an array literal or argument list: append every value
/// of `iterable` to the in-order list `acc`.
pub fn t_spread_into_list(
  st: Agent,
  acc: List(JsVal),
  iterable: JsVal,
) -> #(List(JsVal), Agent) {
  let #(record, st) = iter_protocol.get_iterator_sync(st, iterable)
  let #(values, st) = iter_protocol.iterator_to_list(st, record)
  #(list.append(acc, values), st)
}

/// for-await step (§14.7.5.7 step 6.a): `Call(next, iterator)`, not awaited.
/// The compiled state machine awaits the result and reads `done`/`value`.
pub fn t_async_iter_next(st: Agent, rec: JsVal) -> #(JsVal, Agent) {
  let #(_done, record, st) = read_record(st, rec)
  t_call_checked(st, record.next_method, record.iterator, [])
}

// ── §7.3.25 CopyDataProperties ──────────────────────────────────────────────

/// Object literal `{...source}`: copy own enumerable properties of `source`
/// onto `target` with CreateDataProperty. Nullish `source` is a no-op.
pub fn t_copy_data_props(
  st: Agent,
  target: JsVal,
  source: JsVal,
) -> #(JsVal, Agent) {
  let assert KHandle(target_h) = classify(target)
  #(target, copy_data_properties(st, target_h, source, []))
}

/// Object rest `{a, ...rest} = source`: a fresh %Object.prototype% object with
/// every own enumerable property of `source` except `excluded` (wire keys, in
/// source order). RequireObjectCoercible already ran at pattern entry.
pub fn t_object_rest(
  st: Agent,
  source: JsVal,
  excluded: List(k),
) -> #(JsVal, Agent) {
  let #(h, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let excluded = list.map(excluded, as_object_key)
  #(mk_object(h), copy_data_properties(st, h, source, excluded))
}

fn copy_data_properties(
  st: Agent,
  target: Handle,
  source: JsVal,
  excluded: List(ObjectKey),
) -> Agent {
  case classify(source) {
    KNull | KUndef -> st
    _ -> {
      let #(from, st) = rt_val.t_to_object(st, source)
      let #(keys, st) = rt_obj.t_own_keys(st, from)
      use st, key <- list.fold(keys, st)
      use <- bool.guard(list.contains(excluded, key), st)
      let #(prop, st) = rt_obj.t_get_own_property(st, from, key)
      let wanted =
        option.map(prop, types.prop_enumerable) |> option.unwrap(False)
      case wanted {
        False -> st
        True -> {
          let #(v, st) = rt_obj.t_get_prop(st, mk_object(from), key)
          let #(_, st) =
            rt_obj.t_define_own_data(st, target, key, v, True, True, True)
          st
        }
      }
    }
  }
}

// ── literals ────────────────────────────────────────────────────────────────

/// §13.2.7.3 `/pattern/flags`: a fresh RegExp per evaluation.
pub fn t_regexp_new(
  st: Agent,
  pattern: String,
  flags: String,
) -> #(JsVal, Agent) {
  b_regexp.regexp_create_literal(st, pattern, flags)
}

/// §13.2.8.4 GetTemplateObject. `site` is unique per tagged-template source
/// position (the emitter qualifies it with the module name); the frozen
/// template array (with its frozen `raw`) is built once, pinned, and cached
/// on the agent. `cooked` holds `undefined` for quasis with invalid escapes.
pub fn t_get_template_object(
  st: Agent,
  site: String,
  cooked: List(JsVal),
  raw: List(String),
) -> #(JsVal, Agent) {
  case dict.get(st.template_objects, site) {
    Ok(h) -> #(mk_object(h), st)
    Error(Nil) -> {
      let #(raw_v, st) = rt_obj.t_new_array(st, list.map(raw, mk_string))
      let assert KHandle(raw_h) = classify(raw_v)
      let st = b_object.freeze(st, raw_h)
      let #(tpl_v, st) = rt_obj.t_new_array(st, cooked)
      let assert KHandle(tpl_h) = classify(tpl_v)
      let #(_, st) =
        rt_obj.t_define_own_data(
          st,
          tpl_h,
          StringKey(Named("raw")),
          raw_v,
          False,
          False,
          False,
        )
      let st = b_object.freeze(st, tpl_h)
      let st = rt_store.t_pin_root(st, tpl_h)
      let st =
        Agent(
          ..st,
          template_objects: dict.insert(st.template_objects, site, tpl_h),
        )
      #(tpl_v, st)
    }
  }
}

// ── references ──────────────────────────────────────────────────────────────

/// Sloppy `delete x` where `x` resolved to the global object (§13.5.1.2 step
/// 5): `globalThis.[[Delete]](x)`.
pub fn t_global_delete(st: Agent, name: String) -> #(Bool, Agent) {
  rt_obj.t_delete_prop(
    st,
    st.realm.global_object,
    StringKey(types.canonical_key(name)),
  )
}
