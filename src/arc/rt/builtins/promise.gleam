import arc/internal/tree_array
import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined, two_args_or_undefined}
import arc/rt/builtins/iter_protocol.{
  type IteratorRecord, close_and_throw, get_iterator_sync, iterator_step_value,
}
import arc/rt/call.{
  NormalCompletion, ThrowCompletion, is_callable, is_constructor, t_call,
  t_call_checked, t_call_method, t_construct,
} as rt_call
import arc/rt/elements
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type ObjectKey,
  type PromiseKeyedKind, type PromiseNative, ArrayObj, Dense, JInt, KHandle,
  KeyedFulfilled, KeyedRejected, KeyedValue, Named, NoElements, Ordinary,
  PromiseAllKeyedStatic, PromiseAllResolveElement, PromiseAllSettledElement,
  PromiseAllSettledKeyedStatic, PromiseAllSettledStatic, PromiseAllStatic,
  PromiseAnyRejectElement, PromiseAnyStatic, PromiseCapabilityExecutor,
  PromiseCatch, PromiseConstructor, PromiseFinally, PromiseFinallyFn,
  PromiseFinallyThrower, PromiseFinallyValueThunk, PromiseKeyedElement, PromiseN,
  PromiseRaceStatic, PromiseRejectStatic, PromiseResolveStatic, PromiseThen,
  ReturnThis, SBox, SObject, StringKey, SymbolKey, TypeErr, classify, mk_bool,
  mk_number, mk_object, mk_string, mk_undefined,
} as rt_types
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("then", PromiseN(PromiseThen), 2),
      #("catch", PromiseN(PromiseCatch), 1),
      #("finally", PromiseN(PromiseFinally), 1),
    ])
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("resolve", PromiseN(PromiseResolveStatic), 1),
      #("reject", PromiseN(PromiseRejectStatic), 1),
      #("all", PromiseN(PromiseAllStatic), 1),
      #("race", PromiseN(PromiseRaceStatic), 1),
      #("allSettled", PromiseN(PromiseAllSettledStatic), 1),
      #("any", PromiseN(PromiseAnyStatic), 1),
      #("allKeyed", PromiseN(PromiseAllKeyedStatic), 1),
      #("allSettledKeyed", PromiseN(PromiseAllSettledKeyedStatic), 1),
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(_) { PromiseN(PromiseConstructor) },
      "Promise",
      1,
      static_methods,
    )
  let st = common.add_to_string_tag(st, bt.prototype, "Promise")
  let st = common.add_species_accessor(st, fn_proto, bt.constructor, ReturnThis)
  #(bt, st)
}

pub fn dispatch(
  st: Agent,
  n: PromiseNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    PromiseConstructor ->
      throw_type_error(st, "Promise constructor requires 'new'")
    PromiseThen -> then(st, this, args)
    PromiseCatch -> {
      let on_rejected = first_arg_or_undefined(args)
      t_call_method(st, this, StringKey(Named("then")), [
        mk_undefined(),
        on_rejected,
      ])
    }
    PromiseFinally -> finally(st, this, args)
    PromiseResolveStatic -> resolve_static(st, this, args)
    PromiseRejectStatic -> reject_static(st, this, args)
    PromiseAllStatic -> combinator(st, this, args, CombAll)
    PromiseRaceStatic -> combinator(st, this, args, CombRace)
    PromiseAllSettledStatic -> combinator(st, this, args, CombAllSettled)
    PromiseAnyStatic -> combinator(st, this, args, CombAny)
    PromiseAllKeyedStatic -> keyed_combinator(st, this, args, settled: False)
    PromiseAllSettledKeyedStatic ->
      keyed_combinator(st, this, args, settled: True)
    PromiseCapabilityExecutor(resolve_box:, reject_box:) ->
      capability_executor(st, resolve_box, reject_box, args)
    PromiseAllResolveElement(
      index:,
      remaining:,
      values:,
      already_called:,
      resolve:,
    ) ->
      all_element(st, args, index, remaining, values, already_called, resolve)
    PromiseAllSettledElement(
      fulfilled:,
      index:,
      remaining:,
      values:,
      already_called:,
      resolve:,
    ) ->
      all_settled_element(
        st,
        args,
        fulfilled,
        index,
        remaining,
        values,
        already_called,
        resolve,
      )
    PromiseAnyRejectElement(
      index:,
      remaining:,
      errors:,
      already_called:,
      reject:,
    ) ->
      any_reject_element(
        st,
        args,
        index,
        remaining,
        errors,
        already_called,
        reject,
      )
    PromiseKeyedElement(
      kind:,
      index:,
      remaining:,
      keys:,
      values:,
      already_called:,
      resolve:,
    ) ->
      keyed_element(
        st,
        args,
        kind,
        index,
        remaining,
        keys,
        values,
        already_called,
        resolve,
      )
    PromiseFinallyFn(rejecting:, on_finally:, constructor:) ->
      finally_wrapper(st, args, rejecting, on_finally, constructor)
    PromiseFinallyValueThunk(value:) -> #(value, st)
    PromiseFinallyThrower(reason:) -> rt_store.t_throw(st, reason)
  }
}

pub fn dispatch_construct(
  st: Agent,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let executor = first_arg_or_undefined(args)
  case is_callable(st, executor) {
    False -> throw_type_error(st, "Promise resolver is not a function")
    True -> {
      let #(proto, st) =
        rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
          r.promise.prototype
        })
      let #(promise_h, st) = rt_async.t_new_promise_with_proto(st, Some(proto))
      let #(#(resolve_h, reject_h), st) =
        rt_async.alloc_resolving_fns(st, promise_h)
      let resolve = mk_object(resolve_h)
      let reject = mk_object(reject_h)
      let #(outcome, st) =
        t_call(st, executor, mk_undefined(), [resolve, reject])
      let st = case outcome {
        NormalCompletion(_) -> st
        ThrowCompletion(e) -> {
          let #(_, st) = t_call_checked(st, reject, mk_undefined(), [e])
          st
        }
      }
      #(promise_h, st)
    }
  }
}

fn then(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(on_fulfilled, on_rejected) = two_args_or_undefined(args)
  let promise_h = require_promise(st, this, "Promise.prototype.then")
  let #(c, st) = species_constructor(st, this)
  case c == mk_object(st.realm.promise.constructor) {
    True -> {
      let #(child, st) =
        rt_async.t_promise_then(st, promise_h, on_fulfilled, on_rejected)
      #(mk_object(child), st)
    }
    False -> {
      let #(cap, st) = new_capability_from_constructor(st, c)
      let st =
        rt_async.t_perform_then(
          st,
          promise_h,
          on_fulfilled,
          on_rejected,
          cap.resolve,
          cap.reject,
        )
      #(cap.promise, st)
    }
  }
}

fn finally(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let on_finally = first_arg_or_undefined(args)
  case classify(this) {
    KHandle(_) -> Nil
    _ -> throw_type_error(st, "Promise.prototype.finally called on non-object")
  }
  let #(c, st) = species_constructor(st, this)
  let #(then_finally, catch_finally, st) = case is_callable(st, on_finally) {
    False -> #(on_finally, on_finally, st)
    True -> {
      let #(tf, st) =
        alloc_closure(
          st,
          PromiseN(PromiseFinallyFn(
            rejecting: False,
            on_finally:,
            constructor: c,
          )),
        )
      let #(cf, st) =
        alloc_closure(
          st,
          PromiseN(PromiseFinallyFn(
            rejecting: True,
            on_finally:,
            constructor: c,
          )),
        )
      #(tf, cf, st)
    }
  }
  t_call_method(st, this, StringKey(Named("then")), [
    then_finally,
    catch_finally,
  ])
}

fn finally_wrapper(
  st: Agent,
  args: List(JsVal),
  rejecting: Bool,
  on_finally: JsVal,
  constructor: JsVal,
) -> #(JsVal, Agent) {
  let original = first_arg_or_undefined(args)
  let #(result, st) = t_call_checked(st, on_finally, mk_undefined(), [])
  let #(p, st) = promise_resolve(st, constructor, result)
  let #(handler, st) = case rejecting {
    False ->
      alloc_closure_n(st, PromiseN(PromiseFinallyValueThunk(original)), 0)
    True -> alloc_closure_n(st, PromiseN(PromiseFinallyThrower(original)), 0)
  }
  t_call_method(st, p, StringKey(Named("then")), [handler])
}

fn resolve_static(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let val = first_arg_or_undefined(args)
  case classify(this) {
    KHandle(_) -> promise_resolve(st, this, val)
    _ -> throw_type_error(st, "Promise.resolve called on non-object")
  }
}

fn promise_resolve(st: Agent, c: JsVal, x: JsVal) -> #(JsVal, Agent) {
  case rt_async.as_promise(st, x) {
    Some(_) -> {
      let #(ctor, st) =
        rt_obj.t_get_prop(st, x, StringKey(Named("constructor")))
      case ctor == c {
        True -> #(x, st)
        False -> resolve_with_constructor(st, c, x)
      }
    }
    None -> resolve_with_constructor(st, c, x)
  }
}

fn resolve_with_constructor(
  st: Agent,
  c: JsVal,
  val: JsVal,
) -> #(JsVal, Agent) {
  case c == mk_object(st.realm.promise.constructor) {
    True -> {
      let #(h, st) = rt_async.t_new_promise(st)
      #(mk_object(h), rt_async.t_promise_resolve(st, h, val))
    }
    False -> {
      let #(cap, st) = new_capability_from_constructor(st, c)
      let #(_, st) = t_call_checked(st, cap.resolve, mk_undefined(), [val])
      #(cap.promise, st)
    }
  }
}

fn reject_static(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let reason = first_arg_or_undefined(args)
  case this == mk_object(st.realm.promise.constructor) {
    True -> {
      let #(h, st) = rt_async.t_new_promise(st)
      #(mk_object(h), rt_async.t_promise_reject(st, h, reason))
    }
    False -> {
      let #(cap, st) = new_capability_from_constructor(st, this)
      let #(_, st) = t_call_checked(st, cap.reject, mk_undefined(), [reason])
      #(cap.promise, st)
    }
  }
}

type CombKind {
  CombAll
  CombRace
  CombAllSettled
  CombAny
}

fn combinator(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  kind: CombKind,
) -> #(JsVal, Agent) {
  let #(cap, st) = new_capability_from_constructor(st, this)
  let iterable = first_arg_or_undefined(args)
  let #(outcome, st) =
    protected(st, fn(st) {
      let #(promise_resolve, st) = get_promise_resolve(st, this)
      let #(rec, st) = get_iterator_sync(st, iterable)
      // tracks whether the iterator still needs closing
      let #(open_h, st) = alloc_box(st, mk_bool(True))
      let #(loop_outcome, st) =
        protected(st, fn(st) {
          perform_combinator(st, rec, this, cap, promise_resolve, kind, open_h)
        })
      case loop_outcome {
        NormalCompletion(v) -> #(v, st)
        ThrowCompletion(e) ->
          case read_box(st, open_h) == mk_bool(True) {
            True -> {
              let #(e, st) = close_and_throw(st, rec.iterator, e)
              rt_store.t_throw(st, e)
            }
            False -> rt_store.t_throw(st, e)
          }
      }
    })
  let st = case outcome {
    NormalCompletion(_) -> st
    ThrowCompletion(e) -> {
      let #(_, st) = t_call_checked(st, cap.reject, mk_undefined(), [e])
      st
    }
  }
  #(cap.promise, st)
}

fn perform_combinator(
  st: Agent,
  rec: IteratorRecord,
  c: JsVal,
  cap: Capability,
  promise_resolve: JsVal,
  kind: CombKind,
  open_h: Handle,
) -> #(JsVal, Agent) {
  let realm = st.realm
  case kind {
    CombRace ->
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        0,
        fn(st, _i) { #(cap.resolve, cap.reject, st) },
        fn(st) { #(mk_undefined(), st) },
      )
    CombAll -> {
      let #(values_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(remaining_h, st) = alloc_counter(st, 1)
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        0,
        fn(st, i) {
          let st = set_array_element(st, values_h, i, mk_undefined())
          let #(already_called, st) = alloc_box(st, mk_bool(False))
          let #(resolve_fn, st) =
            alloc_closure(
              st,
              PromiseN(PromiseAllResolveElement(
                index: i,
                remaining: remaining_h,
                values: values_h,
                already_called:,
                resolve: cap.resolve,
              )),
            )
          let st = increment_counter(st, remaining_h)
          #(resolve_fn, cap.reject, st)
        },
        fn(st) { final_resolve_values(st, remaining_h, values_h, cap.resolve) },
      )
    }
    CombAllSettled -> {
      let #(values_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(remaining_h, st) = alloc_counter(st, 1)
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        0,
        fn(st, i) {
          let st = set_array_element(st, values_h, i, mk_undefined())
          let #(already_called, st) = alloc_box(st, mk_bool(False))
          let #(resolve_fn, st) =
            alloc_closure(
              st,
              PromiseN(PromiseAllSettledElement(
                fulfilled: True,
                index: i,
                remaining: remaining_h,
                values: values_h,
                already_called:,
                resolve: cap.resolve,
              )),
            )
          let #(reject_fn, st) =
            alloc_closure(
              st,
              PromiseN(PromiseAllSettledElement(
                fulfilled: False,
                index: i,
                remaining: remaining_h,
                values: values_h,
                already_called:,
                resolve: cap.resolve,
              )),
            )
          let st = increment_counter(st, remaining_h)
          #(resolve_fn, reject_fn, st)
        },
        fn(st) { final_resolve_values(st, remaining_h, values_h, cap.resolve) },
      )
    }
    CombAny -> {
      let #(errors_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(remaining_h, st) = alloc_counter(st, 1)
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        0,
        fn(st, i) {
          let st = set_array_element(st, errors_h, i, mk_undefined())
          let #(already_called, st) = alloc_box(st, mk_bool(False))
          let #(reject_fn, st) =
            alloc_closure(
              st,
              PromiseN(PromiseAnyRejectElement(
                index: i,
                remaining: remaining_h,
                errors: errors_h,
                already_called:,
                reject: cap.reject,
              )),
            )
          let st = increment_counter(st, remaining_h)
          #(cap.resolve, reject_fn, st)
        },
        fn(st) { final_reject_aggregate(st, remaining_h, errors_h, cap.reject) },
      )
    }
  }
}

fn combinator_loop(
  st: Agent,
  rec: IteratorRecord,
  c: JsVal,
  promise_resolve: JsVal,
  open_h: Handle,
  index: Int,
  make_handlers: fn(Agent, Int) -> #(JsVal, JsVal, Agent),
  on_done: fn(Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  // §7.4.8 abrupt during step means no close
  let st = rt_store.t_cell_set(st, open_h, SBox(mk_bool(False)))
  let #(step, st) = iterator_step_value(st, rec)
  case step {
    None -> on_done(st)
    Some(v) -> {
      let st = rt_store.t_cell_set(st, open_h, SBox(mk_bool(True)))
      let #(next_promise, st) = t_call_checked(st, promise_resolve, c, [v])
      let #(on_fulfilled, on_rejected, st) = make_handlers(st, index)
      let #(_, st) =
        t_call_method(st, next_promise, StringKey(Named("then")), [
          on_fulfilled,
          on_rejected,
        ])
      combinator_loop(
        st,
        rec,
        c,
        promise_resolve,
        open_h,
        index + 1,
        make_handlers,
        on_done,
      )
    }
  }
}

fn final_resolve_values(
  st: Agent,
  remaining_h: Handle,
  values_h: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  let #(is_zero, st) = decrement_counter(st, remaining_h)
  case is_zero {
    False -> #(mk_undefined(), st)
    True -> t_call_checked(st, resolve, mk_undefined(), [mk_object(values_h)])
  }
}

fn final_reject_aggregate(
  st: Agent,
  remaining_h: Handle,
  errors_h: Handle,
  reject: JsVal,
) -> #(JsVal, Agent) {
  let #(is_zero, st) = decrement_counter(st, remaining_h)
  case is_zero {
    False -> #(mk_undefined(), st)
    True -> {
      let #(err, st) = make_aggregate_error(st, errors_h)
      t_call_checked(st, reject, mk_undefined(), [err])
    }
  }
}

type KeyedLoop {
  KeyedLoop(
    c: JsVal,
    promises: JsVal,
    promises_h: Handle,
    cap: Capability,
    promise_resolve: JsVal,
    settled: Bool,
    keys_h: Handle,
    values_h: Handle,
    remaining_h: Handle,
  )
}

fn keyed_combinator(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  settled settled: Bool,
) -> #(JsVal, Agent) {
  let #(cap, st) = new_capability_from_constructor(st, this)
  let promises = first_arg_or_undefined(args)
  let #(outcome, st) =
    protected(st, fn(st) { perform_all_keyed(st, this, promises, cap, settled) })
  let st = case outcome {
    NormalCompletion(_) -> st
    ThrowCompletion(e) -> {
      let #(_, st) = t_call_checked(st, cap.reject, mk_undefined(), [e])
      st
    }
  }
  #(cap.promise, st)
}

fn perform_all_keyed(
  st: Agent,
  c: JsVal,
  promises: JsVal,
  cap: Capability,
  settled: Bool,
) -> #(JsVal, Agent) {
  let #(promise_resolve, st) = get_promise_resolve(st, c)
  case classify(promises) {
    KHandle(promises_h) -> {
      let #(all_keys, st) = rt_obj.t_own_keys(st, promises_h)
      let realm = st.realm
      let #(keys_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(values_h, st) = alloc_empty_array(st, realm.array.prototype)
      let #(remaining_h, st) = alloc_counter(st, 1)
      let loop =
        KeyedLoop(
          c:,
          promises:,
          promises_h:,
          cap:,
          promise_resolve:,
          settled:,
          keys_h:,
          values_h:,
          remaining_h:,
        )
      keyed_loop(st, loop, all_keys, 0)
    }
    _ ->
      throw_type_error(
        st,
        "Promise keyed combinator argument must be an object",
      )
  }
}

fn keyed_loop(
  st: Agent,
  loop: KeyedLoop,
  all_keys: List(ObjectKey),
  index: Int,
) -> #(JsVal, Agent) {
  case all_keys {
    [] ->
      keyed_final_resolve(
        st,
        loop.remaining_h,
        loop.keys_h,
        loop.values_h,
        loop.cap.resolve,
      )
    [key, ..rest] -> {
      let #(desc, st) = rt_obj.t_get_own_property(st, loop.promises_h, key)
      let enumerable =
        option.map(desc, rt_types.prop_enumerable) |> option.unwrap(False)
      case enumerable {
        False -> keyed_loop(st, loop, rest, index)
        True -> {
          let #(prop_value, st) = rt_obj.t_get_prop(st, loop.promises, key)
          let st =
            set_array_element(
              st,
              loop.keys_h,
              index,
              rt_obj.object_key_value(key),
            )
          let st = set_array_element(st, loop.values_h, index, mk_undefined())
          let #(next_promise, st) =
            t_call_checked(st, loop.promise_resolve, loop.c, [prop_value])
          let #(already_called, st) = alloc_box(st, mk_bool(False))
          let element = fn(st, kind) {
            alloc_closure(
              st,
              PromiseN(PromiseKeyedElement(
                kind:,
                index:,
                remaining: loop.remaining_h,
                keys: loop.keys_h,
                values: loop.values_h,
                already_called:,
                resolve: loop.cap.resolve,
              )),
            )
          }
          let fulfilled_kind = case loop.settled {
            True -> KeyedFulfilled
            False -> KeyedValue
          }
          let #(on_fulfilled, st) = element(st, fulfilled_kind)
          let #(on_rejected, st) = case loop.settled {
            False -> #(loop.cap.reject, st)
            True -> element(st, KeyedRejected)
          }
          let st = increment_counter(st, loop.remaining_h)
          let #(_, st) =
            t_call_method(st, next_promise, StringKey(Named("then")), [
              on_fulfilled,
              on_rejected,
            ])
          keyed_loop(st, loop, rest, index + 1)
        }
      }
    }
  }
}

fn keyed_final_resolve(
  st: Agent,
  remaining_h: Handle,
  keys_h: Handle,
  values_h: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  let #(is_zero, st) = decrement_counter(st, remaining_h)
  case is_zero {
    False -> #(mk_undefined(), st)
    True -> {
      let #(result_h, st) = create_keyed_result(st, keys_h, values_h)
      t_call_checked(st, resolve, mk_undefined(), [mk_object(result_h)])
    }
  }
}

fn create_keyed_result(
  st: Agent,
  keys_h: Handle,
  values_h: Handle,
) -> #(Handle, Agent) {
  let keys = read_array_values(st, keys_h)
  let values = read_array_values(st, values_h)
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: None,
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st =
    list.zip(keys, values)
    |> list.fold(st, fn(st, kv) {
      let #(k, v) = kv
      case key_of_value(k) {
        Some(key) -> {
          let #(_created, st) =
            rt_obj.t_define_own_data(st, h, key, v, True, True, True)
          st
        }
        None -> st
      }
    })
  #(h, st)
}

fn key_of_value(v: JsVal) -> option.Option(ObjectKey) {
  case classify(v) {
    rt_types.KStr(s) -> Some(StringKey(rt_types.canonical_key(s)))
    rt_types.KSym(sym) -> Some(SymbolKey(sym))
    _ -> None
  }
}

fn all_element(
  st: Agent,
  args: List(JsVal),
  index: Int,
  remaining: Handle,
  values: Handle,
  already_called: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  use val, st <- with_element_once(st, args, already_called)
  let st = set_array_element(st, values, index, val)
  final_resolve_values(st, remaining, values, resolve)
}

fn all_settled_element(
  st: Agent,
  args: List(JsVal),
  fulfilled: Bool,
  index: Int,
  remaining: Handle,
  values: Handle,
  already_called: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  use val, st <- with_element_once(st, args, already_called)
  let #(record, st) = settled_record(st, fulfilled, val)
  let st = set_array_element(st, values, index, record)
  final_resolve_values(st, remaining, values, resolve)
}

fn settled_record(st: Agent, fulfilled: Bool, val: JsVal) -> #(JsVal, Agent) {
  let #(status, field) = case fulfilled {
    True -> #("fulfilled", "value")
    False -> #("rejected", "reason")
  }
  let #(obj_h, st) =
    common.alloc_pojo(st, st.realm.object.prototype, [
      #("status", mk_string(status)),
      #(field, val),
    ])
  #(mk_object(obj_h), st)
}

fn keyed_element(
  st: Agent,
  args: List(JsVal),
  kind: PromiseKeyedKind,
  index: Int,
  remaining: Handle,
  keys: Handle,
  values: Handle,
  already_called: Handle,
  resolve: JsVal,
) -> #(JsVal, Agent) {
  use val, st <- with_element_once(st, args, already_called)
  let #(stored, st) = case kind {
    KeyedValue -> #(val, st)
    KeyedFulfilled -> settled_record(st, True, val)
    KeyedRejected -> settled_record(st, False, val)
  }
  let st = set_array_element(st, values, index, stored)
  keyed_final_resolve(st, remaining, keys, values, resolve)
}

fn any_reject_element(
  st: Agent,
  args: List(JsVal),
  index: Int,
  remaining: Handle,
  errors: Handle,
  already_called: Handle,
  reject: JsVal,
) -> #(JsVal, Agent) {
  use reason, st <- with_element_once(st, args, already_called)
  let st = set_array_element(st, errors, index, reason)
  final_reject_aggregate(st, remaining, errors, reject)
}

fn with_element_once(
  st: Agent,
  args: List(JsVal),
  already_called: Handle,
  body: fn(JsVal, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let js_true = mk_bool(True)
  case rt_store.t_cell_get(st, already_called) {
    SBox(v) if v == js_true -> #(mk_undefined(), st)
    _ -> {
      let st = rt_store.t_cell_set(st, already_called, SBox(js_true))
      body(first_arg_or_undefined(args), st)
    }
  }
}

type Capability {
  Capability(promise: JsVal, resolve: JsVal, reject: JsVal)
}

fn new_capability_from_constructor(
  st: Agent,
  c: JsVal,
) -> #(Capability, Agent) {
  let realm = st.realm
  case c == mk_object(realm.promise.constructor) {
    True -> {
      let #(#(p, r, j), st) = rt_async.t_new_promise_capability(st)
      #(
        Capability(
          promise: mk_object(p),
          resolve: mk_object(r),
          reject: mk_object(j),
        ),
        st,
      )
    }
    False -> {
      case is_constructor(st, c) {
        False ->
          throw_type_error(st, "Promise capability requires a constructor")
        True -> {
          let #(resolve_box, st) = alloc_box(st, mk_undefined())
          let #(reject_box, st) = alloc_box(st, mk_undefined())
          let #(executor, st) =
            alloc_closure2(
              st,
              PromiseN(PromiseCapabilityExecutor(resolve_box:, reject_box:)),
            )
          let #(promise_h, st) = t_construct(st, c, [executor], c)
          let resolve = read_box(st, resolve_box)
          let reject = read_box(st, reject_box)
          case is_callable(st, resolve) && is_callable(st, reject) {
            True -> #(
              Capability(promise: mk_object(promise_h), resolve:, reject:),
              st,
            )
            False ->
              throw_type_error(
                st,
                "Promise resolve or reject function is not callable",
              )
          }
        }
      }
    }
  }
}

fn capability_executor(
  st: Agent,
  resolve_box: Handle,
  reject_box: Handle,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let already_set =
    read_box(st, resolve_box) != mk_undefined()
    || read_box(st, reject_box) != mk_undefined()
  case already_set {
    True ->
      throw_type_error(
        st,
        "Promise executor has already been invoked with non-undefined arguments",
      )
    False -> {
      let #(resolve, reject) = two_args_or_undefined(args)
      let st = rt_store.t_cell_set(st, resolve_box, SBox(resolve))
      let st = rt_store.t_cell_set(st, reject_box, SBox(reject))
      #(mk_undefined(), st)
    }
  }
}

fn get_promise_resolve(st: Agent, c: JsVal) -> #(JsVal, Agent) {
  let #(resolve_fn, st) = rt_obj.t_get_prop(st, c, StringKey(Named("resolve")))
  case is_callable(st, resolve_fn) {
    True -> #(resolve_fn, st)
    False -> throw_type_error(st, "Promise resolve is not a function")
  }
}

fn species_constructor(st: Agent, o: JsVal) -> #(JsVal, Agent) {
  let default = mk_object(st.realm.promise.constructor)
  case intrinsic_species(st, o) {
    True -> #(default, st)
    False -> species_constructor_generic(st, o, default)
  }
}

fn intrinsic_species(st: Agent, o: JsVal) -> Bool {
  is_plain_promise(st, o) && common.species_intact(st, st.realm.promise)
}

fn is_plain_promise(st: Agent, o: JsVal) -> Bool {
  case classify(o) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: rt_types.PromiseObj(..), proto: Some(p), props:, ..) ->
          p == st.realm.promise.prototype
          && !dict.has_key(props, Named("constructor"))
        _ -> False
      }
    _ -> False
  }
}

fn species_constructor_generic(
  st: Agent,
  o: JsVal,
  default: JsVal,
) -> #(JsVal, Agent) {
  let #(c, st) = rt_obj.t_get_prop(st, o, StringKey(Named("constructor")))
  case classify(c) {
    rt_types.KUndef -> #(default, st)
    KHandle(_) -> {
      let #(s, st) =
        rt_obj.t_get_prop(st, c, rt_types.SymbolKey(rt_types.symbol_species))
      case classify(s) {
        rt_types.KUndef | rt_types.KNull -> #(default, st)
        _ ->
          case is_constructor(st, s) {
            True -> #(s, st)
            False ->
              throw_type_error(
                st,
                "Promise[Symbol.species] is not a constructor",
              )
          }
      }
    }
    _ -> throw_type_error(st, ".constructor is not an object")
  }
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(rt_call.Completion, Agent)

fn alloc_closure(st: Agent, tag: rt_types.NativeToken) -> #(JsVal, Agent) {
  alloc_closure_n(st, tag, 1)
}

fn alloc_closure2(st: Agent, tag: rt_types.NativeToken) -> #(JsVal, Agent) {
  alloc_closure_n(st, tag, 2)
}

fn alloc_closure_n(
  st: Agent,
  tag: rt_types.NativeToken,
  len: Int,
) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_call.t_native_new(
      st,
      Some(st.realm.function.prototype),
      tag,
      "",
      len,
      False,
    )
  #(mk_object(h), st)
}

fn alloc_box(st: Agent, v: JsVal) -> #(Handle, Agent) {
  rt_store.t_cell_new(st, SBox(v))
}

fn read_box(st: Agent, h: Handle) -> JsVal {
  case rt_store.t_cell_get(st, h) {
    SBox(v) -> v
    _ -> mk_undefined()
  }
}

fn alloc_counter(st: Agent, n: Int) -> #(Handle, Agent) {
  rt_store.t_cell_new(st, SBox(mk_number(JInt(n))))
}

fn adjust_counter(st: Agent, h: Handle, delta: Int) -> #(Int, Agent) {
  case rt_store.t_cell_get(st, h) {
    SBox(v) ->
      case classify(v) {
        rt_types.KNum(JInt(n)) -> {
          let n2 = n + delta
          #(n2, rt_store.t_cell_set(st, h, SBox(mk_number(JInt(n2)))))
        }
        _ -> panic as "promise combinator counter not an int"
      }
    _ -> panic as "promise combinator counter not an SBox"
  }
}

fn increment_counter(st: Agent, h: Handle) -> Agent {
  let #(_, st) = adjust_counter(st, h, 1)
  st
}

fn decrement_counter(st: Agent, h: Handle) -> #(Bool, Agent) {
  let #(n, st) = adjust_counter(st, h, -1)
  #(n <= 0, st)
}

fn alloc_empty_array(st: Agent, array_proto: Handle) -> #(Handle, Agent) {
  common.alloc_array(st, [], array_proto)
}

fn set_array_element(
  st: Agent,
  arr_h: Handle,
  index: Int,
  val: JsVal,
) -> Agent {
  rt_store.t_cell_update(st, arr_h, fn(slot) {
    case slot {
      SObject(kind: ArrayObj(length:), elements:, ..) -> {
        let ta = case elements {
          Dense(t) -> t
          _ -> tree_array.new(rt_types.mk_hole())
        }
        SObject(
          ..slot,
          kind: ArrayObj(int.max(length, index + 1)),
          elements: Dense(tree_array.set(index, val, ta)),
        )
      }
      other -> other
    }
  })
}

fn read_array_values(st: Agent, arr_h: Handle) -> List(JsVal) {
  case rt_store.t_cell_get(st, arr_h) {
    SObject(kind: ArrayObj(length:), elements: els, ..) ->
      collect_elements(els, length - 1, [])
    _ -> []
  }
}

fn collect_elements(
  els: rt_types.JsElements,
  i: Int,
  acc: List(JsVal),
) -> List(JsVal) {
  case i < 0 {
    True -> acc
    False -> collect_elements(els, i - 1, [elements.get(els, i), ..acc])
  }
}

fn make_aggregate_error(st: Agent, errors_h: Handle) -> #(JsVal, Agent) {
  let realm = st.realm
  let #(msg_p, st) =
    common.builtin_property(st, mk_string("All promises were rejected"))
  let #(errs_p, st) = common.builtin_property(st, mk_object(errors_h))
  let #(h, st) =
    common.alloc_error_slot(st, realm.aggregate_error.prototype, [
      #("message", msg_p),
      #("errors", errs_p),
    ])
  #(mk_object(h), st)
}

fn require_promise(st: Agent, this: JsVal, name: String) -> Handle {
  case rt_async.as_promise(st, this) {
    Some(h) -> h
    None -> throw_type_error(st, name <> " called on non-promise")
  }
}

fn throw_type_error(st: Agent, msg: String) -> a {
  let js = st.store
  let #(e, st) = js.ops.new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}
