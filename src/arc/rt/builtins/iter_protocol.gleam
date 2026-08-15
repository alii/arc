//// ES2024 §7.4 — the Iterator Record abstract operations, in ONE place.
////
//// Every "consume an iterable" site in the runtime funnels through the real
//// iterator protocol here instead of reaching into a concrete collection's
//// heap storage. Sits BELOW `rt_builtins/{iterator,object,map,set}` so
//// none of them import each other for §7.4 ops. Faithful port of arc
//// `vm/builtins/iter_protocol.gleam` re-expressed under D7 (throwing ops
//// diverge via `t_throw`; catching sites use `t_apply_protected`).

import arc/rt/call.{
  type Completion, NormalCompletion, ThrowCompletion, is_callable, t_call,
  t_call_checked,
}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsOps, type JsVal, ArrayObj,
  AsyncFromSyncIterator, DataProperty, Dense, Index, IteratorRecord, KHandle,
  KNull, KStr, KUndef, Named, NoElements, SObject, StringKey, SymbolKey, TypeErr,
  classify, mk_object, mk_undefined, symbol_async_iterator, symbol_iterator,
} as rt_types
import arc/rt/val as rt_val
import arc/vm/internal/tree_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

// ── §7.4.1 Iterator Record ──────────────────────────────────────────────────

/// §7.4.1 Iterator Record — `{[[Iterator]], [[NextMethod]], [[Done]]}`. The
/// `[[Done]]` flag is threaded implicitly (D7 throws diverge; a caught throw
/// marks the record done at the catch site). Type lives in `rt_types` so
/// `ObjKind` can carry it (%IteratorHelper% / %WrapForValidIterator%); this
/// alias keeps existing `iter_protocol.IteratorRecord` type refs compiling.
pub type IteratorRecord =
  rt_types.IteratorRecord

// ── local plumbing ──────────────────────────────────────────────────────────

/// Run a threaded thunk under the same `{wasm_exn,0,[St,V]}` try/catch as
/// `t_call` — used for §7.4.11 IteratorClose, whose GetMethod + Call are
/// wrapped in Completion(). Bound to the call-FFI `t_apply_protected/2`.
@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(Completion, Agent)

fn js_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

fn throw_type_error(st: Agent, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}

fn new_type_error(st: Agent, msg: String) -> #(JsVal, Agent) {
  js_ops(st).new_error(st, TypeErr, msg)
}

/// 2core has no `object.inspect`; name a value by its `typeof` in TypeError
/// messages (matches `rt_ops.gleam:636` posture).
fn describe(st: Agent, v: JsVal) -> String {
  let #(ty, _) = rt_val.t_type_of(st, v)
  ty
}

/// Allocate a fresh dense Array from `elems` with proto = `%Array.prototype%`.
/// Local re-expression of arc `state.ok_array` / `common.alloc_array`.
fn alloc_array(st: Agent, elems: List(JsVal)) -> #(Handle, Agent) {
  let len = list.length(elems)
  let elements = case elems {
    [] -> NoElements
    _ -> Dense(tree_array.from_list(elems, rt_types.mk_hole()))
  }
  rt_store.t_cell_new(
    st,
    SObject(
      kind: ArrayObj(length: len),
      proto: Some(st.realm.array.prototype),
      props: dict.new(),
      symbol_props: [],
      elements:,
      extensible: True,
    ),
  )
}

// ═══════════════════════════════════════════════════════════════════════════
// §7.4.2/§7.4.3/§7.4.4 — obtaining an Iterator Record
// ═══════════════════════════════════════════════════════════════════════════

/// §7.4.9 GetIteratorDirect(obj): `obj` must be an Object; its `next` method
/// is read exactly ONCE and cached — monkey-patching `obj.next` mid-iteration
/// has no effect. The ONE place an already-obtained iterator object becomes
/// an `IteratorRecord`. `non_object_msg` names the value in the TypeError.
pub fn get_iterator_direct(
  st: Agent,
  obj: JsVal,
  non_object_msg: String,
) -> #(IteratorRecord, Agent) {
  case rt_val.is_object(obj) {
    True -> {
      let #(next, st) = rt_obj.t_get_prop(st, obj, StringKey(Named("next")))
      #(IteratorRecord(iterator: obj, next_method: next), st)
    }
    False -> throw_type_error(st, non_object_msg)
  }
}

/// §7.4.3 GetIterator(obj, sync): the `@@iterator` method must be callable
/// and its result must be an Object; caches `next` (GetIteratorDirect). The
/// "<typeof obj> is not iterable" message is relied on by for-of, spread,
/// Object.fromEntries — keep them in sync.
pub fn get_iterator_sync(st: Agent, obj: JsVal) -> #(IteratorRecord, Agent) {
  let #(method, st) = rt_obj.t_get_prop(st, obj, SymbolKey(symbol_iterator))
  case is_callable(st, method) {
    False -> throw_type_error(st, describe(st, obj) <> " is not iterable")
    True -> get_iterator_from_method(st, obj, method)
  }
}

/// §7.4.4 GetIteratorFromMethod(obj, method): `iterator = ? Call(method, obj)`
/// — result must be an Object; `next` is read once and cached. Public for
/// consumers that already did GetMethod(obj, @@iterator) — e.g. Array.from.
pub fn get_iterator_from_method(
  st: Agent,
  obj: JsVal,
  method: JsVal,
) -> #(IteratorRecord, Agent) {
  let #(iter, st) = t_call_checked(st, method, obj, [])
  get_iterator_direct(
    st,
    iter,
    "Result of the Symbol.iterator method is not an object",
  )
}

/// §7.4.3 GetIterator(obj, async): try `@@asyncIterator`; if nullish, fall
/// back to `@@iterator` and wrap via CreateAsyncFromSyncIterator (§27.1.6.1).
/// Port of arc `interpreter.gleam:6268-6356` collapsed to a Record return.
pub fn get_iterator_async(st: Agent, obj: JsVal) -> #(IteratorRecord, Agent) {
  let #(method, st) =
    rt_obj.t_get_prop(st, obj, SymbolKey(symbol_async_iterator))
  case classify(method) {
    KUndef | KNull -> {
      // §7.4.3 step 1.b: sync fallback via @@iterator, then wrap.
      let #(sync_method, st) =
        rt_obj.t_get_prop(st, obj, SymbolKey(symbol_iterator))
      case is_callable(st, sync_method) {
        False ->
          throw_type_error(st, describe(st, obj) <> " is not async iterable")
        True -> {
          let #(sync_rec, st) = get_iterator_from_method(st, obj, sync_method)
          create_async_from_sync(st, sync_rec)
        }
      }
    }
    _ ->
      case is_callable(st, method) {
        False ->
          throw_type_error(st, describe(st, obj) <> " is not async iterable")
        True -> get_iterator_from_method(st, obj, method)
      }
  }
}

const k_iterator = StringKey(Named("iterator"))

const k_next = StringKey(Named("next"))

/// §27.1.6.1 CreateAsyncFromSyncIterator: allocate an `%AsyncFromSyncIterator%`
/// wrapper whose [[SyncIteratorRecord]] is `sync`: the iterator AND its
/// already-read `next` (§7.4.4 reads it once), held in an internal null-proto
/// record cell so %AsyncFromSyncIteratorPrototype%.next never re-Gets it.
/// Proto MUST be `%AsyncFromSyncIteratorPrototype%` (owns next/return/throw
/// per §27.1.6.2) — NOT `%AsyncIteratorPrototype%`. The returned Record's
/// `next_method` is that proto's `next`, read from the wrapper.
pub fn create_async_from_sync(
  st: Agent,
  sync: IteratorRecord,
) -> #(IteratorRecord, Agent) {
  let #(sync_rec, st) = rt_obj.t_new_object(st, None)
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      sync_rec,
      k_iterator,
      sync.iterator,
      True,
      True,
      True,
    )
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      sync_rec,
      k_next,
      sync.next_method,
      True,
      True,
      True,
    )
  let #(wrapper_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: AsyncFromSyncIterator(sync_rec:),
        proto: Some(st.realm.async_from_sync_proto),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let wrapper = mk_object(wrapper_h)
  let #(next, st) = rt_obj.t_get_prop(st, wrapper, StringKey(Named("next")))
  #(IteratorRecord(iterator: wrapper, next_method: next), st)
}

/// The [[SyncIteratorRecord]] held in an %AsyncFromSyncIterator%'s record
/// cell (as written by `create_async_from_sync`).
pub fn sync_iterator_record(st: Agent, sync_rec: Handle) -> IteratorRecord {
  case
    rt_obj.t_ordinary_own_property(st, sync_rec, k_iterator),
    rt_obj.t_ordinary_own_property(st, sync_rec, k_next)
  {
    Some(DataProperty(value: iterator, ..)),
      Some(DataProperty(value: next_method, ..))
    -> IteratorRecord(iterator:, next_method:)
    _, _ -> throw_type_error(st, "not an Async-from-Sync Iterator")
  }
}

/// §7.4.13 GetIteratorFlattenable's two call sites, as a type: `Iterator.from`
/// accepts a String receiver and iterates its code points; every other caller
/// (zip inputs, concat inputs, flatMap callback result) rejects primitives.
pub type PrimitiveHandling {
  IterateStrings
  RejectPrimitives
}

/// §7.4.13 GetIteratorFlattenable(obj, primitiveHandling). If `obj` is not an
/// Object: reject-primitives → TypeError; else must be a String. Then
/// GetMethod(obj, @@iterator); undefined → obj IS the iterator; else Call it.
/// Result must be an Object; return GetIteratorDirect. `what` names the value
/// in the two TypeError messages.
pub fn get_iterator_flattenable(
  st: Agent,
  obj: JsVal,
  handling: PrimitiveHandling,
  what: String,
) -> #(IteratorRecord, Agent) {
  let acceptable = case classify(obj), handling {
    KHandle(_), _ -> True
    KStr(_), IterateStrings -> True
    _, _ -> False
  }
  case acceptable {
    False -> throw_type_error(st, what <> " is not an object")
    True -> {
      let #(method, st) = rt_obj.t_get_prop(st, obj, SymbolKey(symbol_iterator))
      // §7.3.10 GetMethod step 3: non-callable @@iterator is a TypeError.
      let #(iter, st) = case classify(method) {
        KUndef | KNull -> #(obj, st)
        _ ->
          case is_callable(st, method) {
            False -> throw_type_error(st, what <> " is not iterable")
            True -> t_call_checked(st, method, obj, [])
          }
      }
      get_iterator_direct(st, iter, what <> " is not iterable")
    }
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// §7.4.5/§7.4.6/§7.4.8 — stepping an Iterator Record
// ═══════════════════════════════════════════════════════════════════════════

/// Shared §7.4.6/§7.4.8 prefix: call the record's cached `next` on its
/// iterator, require the result is an Object, read `.done`; continue with the
/// result object and the done flag. CPS so `.value` is only read when needed.
pub fn iterator_step_result(
  st: Agent,
  rec: IteratorRecord,
  cont: fn(JsVal, Bool, Agent) -> #(a, Agent),
) -> #(a, Agent) {
  let #(result, st) = t_call_checked(st, rec.next_method, rec.iterator, [])
  case rt_val.is_object(result) {
    True -> {
      let #(done, st) = rt_obj.t_get_prop(st, result, StringKey(Named("done")))
      cont(result, rt_val.to_boolean(done), st)
    }
    False -> throw_type_error(st, "Iterator result is not an object")
  }
}

/// §7.4.8 IteratorStepValue: if done → `None`; else read `.value`. The ONE
/// next()/done/value reader shared by every consumer draining an
/// already-obtained record — Iterator.prototype helpers, `iterator_rest`,
/// spread, Object.fromEntries, Object.groupBy. New drain sites reuse THIS.
pub fn iterator_step_value(
  st: Agent,
  rec: IteratorRecord,
) -> #(Option(JsVal), Agent) {
  use result, done, st <- iterator_step_result(st, rec)
  case done {
    True -> #(None, st)
    False -> {
      let #(v, st) = rt_obj.t_get_prop(st, result, StringKey(Named("value")))
      #(Some(v), st)
    }
  }
}

/// §7.4.6 IteratorStep, done-only: call the cached `next()` and read only
/// `done` — a `value` getter must NOT fire when the caller only needs to
/// know whether the iterator finished (e.g. Iterator.zip strict length check).
pub fn iterator_step_done(st: Agent, rec: IteratorRecord) -> #(Bool, Agent) {
  use _result, done, st <- iterator_step_result(st, rec)
  #(done, st)
}

/// §7.4.14 IteratorToList — drain `rec` to exhaustion, collecting every
/// yielded value in order. Abrupt completions from next()/done/value
/// propagate without close (§7.4.8 marks the record done). ONE reversed-
/// accumulator drain loop; `iterator_rest`, Iterator.prototype.toArray and
/// AggregateError's `errors` iteration funnel through it.
pub fn iterator_to_list(
  st: Agent,
  rec: IteratorRecord,
) -> #(List(JsVal), Agent) {
  iterator_to_list_loop(st, rec, [])
}

fn iterator_to_list_loop(
  st: Agent,
  rec: IteratorRecord,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case iterator_step_value(st, rec) {
    #(None, st) -> #(list.reverse(acc), st)
    #(Some(v), st) -> iterator_to_list_loop(st, rec, [v, ..acc])
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// §7.4.11 IteratorClose
// ═══════════════════════════════════════════════════════════════════════════

/// Successful outcome of `call_return`. §7.4.11 IteratorClose and
/// %WrapForValidIteratorPrototype%.return both need to tell "no `return`
/// method" apart from "a `return` method ran and produced this value" — a
/// `JsUndefined` sentinel cannot (a `return` that RAN and returned undefined
/// must be a TypeError under IteratorClose, not a silent success).
pub type ReturnCall {
  /// GetMethod(iterator, "return") was undefined/null: nothing was called.
  NoReturnMethod
  /// The `return` method was called; this is its (unchecked) result.
  Returned(JsVal)
}

/// Shared body of IteratorClose: `Completion(GetMethod(iterator, "return"))`
/// then, if present and callable, `Completion(Call(return, iterator))`. Both
/// steps run under `protected` so their throws are CAUGHT (§7.4.11 steps 3-4
/// wrap them in Completion()). Callers decide what the completion rules make
/// of the outcome.
pub fn call_return(
  st: Agent,
  obj: JsVal,
) -> #(Result(ReturnCall, JsVal), Agent) {
  // Step 3: innerResult = Completion(GetMethod(iterator, "return")).
  let #(get_c, st) =
    protected(st, fn(st) {
      rt_obj.t_get_prop(st, obj, StringKey(Named("return")))
    })
  case get_c {
    ThrowCompletion(e) -> #(Error(e), st)
    NormalCompletion(ret_fn) ->
      case classify(ret_fn) {
        KUndef | KNull -> #(Ok(NoReturnMethod), st)
        _ ->
          // §7.3.10 GetMethod step 3: non-callable `return` is a TypeError.
          case is_callable(st, ret_fn) {
            False -> {
              let #(e, st) =
                new_type_error(st, "iterator.return is not a function")
              #(Error(e), st)
            }
            True ->
              // Step 4.c: innerResult = Completion(Call(return, iterator)).
              case t_call(st, ret_fn, obj, []) {
                #(NormalCompletion(v), st) -> #(Ok(Returned(v)), st)
                #(ThrowCompletion(e), st) -> #(Error(e), st)
              }
          }
      }
  }
}

/// §7.4.11 IteratorClose with throw completion, as a plain value: get
/// `.return`; if callable, call it (swallowing any throw — the ORIGINAL error
/// wins); hand back the throwable so the caller decides the result shape.
pub fn close_and_throw(
  st: Agent,
  obj: JsVal,
  original: JsVal,
) -> #(JsVal, Agent) {
  let #(_ignored, st) = call_return(st, obj)
  #(original, st)
}

/// `close_and_throw` at a dispatch boundary — the throw propagates via
/// `t_throw` (D7).
pub fn close_throw(st: Agent, obj: JsVal, original: JsVal) -> a {
  let #(thrown, st) = close_and_throw(st, obj, original)
  rt_store.t_throw(st, thrown)
}

/// IteratorClose with a freshly-allocated TypeError.
pub fn close_throw_type(st: Agent, obj: JsVal, msg: String) -> a {
  let #(err, st) = new_type_error(st, msg)
  close_throw(st, obj, err)
}

/// §7.4.11 IteratorClose with normal completion: get `.return`; if undefined
/// → ok; else call it; if call throws → propagate; if result not Object →
/// TypeError; else ok. Returns `st'` on success (Nil-carrying `#(_, st)` is
/// noise under D7 — a throw diverges).
pub fn iterator_close_normal(st: Agent, obj: JsVal) -> Agent {
  case call_return(st, obj) {
    #(Ok(NoReturnMethod), st) -> st
    #(Ok(Returned(v)), st) ->
      case rt_val.is_object(v) {
        True -> st
        False -> throw_type_error(st, "Iterator return result is not an object")
      }
    #(Error(thrown), st) -> rt_store.t_throw(st, thrown)
  }
}

/// Run `body` catching a JS throw; on throw, IteratorClose(iter) with the
/// thrown value (original error wins over any close error — §7.4.11); on
/// normal completion, continue with the value. Arc `or_close` re-expressed
/// as a protected thunk under D7.
pub fn or_close(
  st: Agent,
  iter: JsVal,
  body: fn(Agent) -> #(JsVal, Agent),
  cont: fn(JsVal, Agent) -> #(a, Agent),
) -> #(a, Agent) {
  case protected(st, body) {
    #(NormalCompletion(v), st) -> cont(v, st)
    #(ThrowCompletion(thrown), st) -> close_throw(st, iter, thrown)
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// §24.1.1.2 AddEntriesFromIterable — the shared entry-drain loop
// ═══════════════════════════════════════════════════════════════════════════

/// The per-entry sink of AddEntriesFromIterable, at the Gleam level. Map/
/// WeakMap constructors pass a sink that `[[Call]]`s the user-reachable `set`
/// adder; Object.fromEntries passes one that does CreateDataPropertyOrThrow.
/// Both share the ONE §24.1.1.2 loop below (GetIterator, per-entry Object
/// check, Get "0"/"1", IteratorClose on any abrupt completion). Sink returns
/// `st'` on success; a throw diverges (D7) and is caught by `or_close`.
pub type EntrySink =
  fn(Agent, JsVal, JsVal) -> Agent

/// §24.1.1.2 AddEntriesFromIterable(target, iterable, addEntry) with a
/// GLEAM-level `add_entry` sink — full iterator protocol: GetIterator, then
/// per entry Get "0"/"1" and run `add_entry`, closing the iterator on any
/// abrupt completion inside the loop. Returns `target` on normal completion.
pub fn add_entries_with_sink(
  st: Agent,
  target: JsVal,
  iterable: JsVal,
  add_entry: EntrySink,
) -> #(JsVal, Agent) {
  let #(rec, st) = get_iterator_sync(st, iterable)
  add_entries_loop(st, target, rec, add_entry)
}

/// One IteratorStepValue + entry processing per iteration. Abrupt completions
/// from next()/done/value propagate without close (§7.4.8 marks the iterator
/// done); abrupt completions from the entry reads or the sink close the
/// iterator first (§24.1.1.2 step 4).
fn add_entries_loop(
  st: Agent,
  target: JsVal,
  rec: IteratorRecord,
  add_entry: EntrySink,
) -> #(JsVal, Agent) {
  use step, done, st <- iterator_step_result(st, rec)
  case done {
    True -> #(target, st)
    False -> {
      let #(entry, st) = rt_obj.t_get_prop(st, step, StringKey(Named("value")))
      case rt_val.is_object(entry) {
        True -> {
          use k, st <- or_close(st, rec.iterator, fn(st) {
            rt_obj.t_get_prop(st, entry, StringKey(Index(0)))
          })
          use v, st <- or_close(st, rec.iterator, fn(st) {
            rt_obj.t_get_prop(st, entry, StringKey(Index(1)))
          })
          use _, st <- or_close(st, rec.iterator, fn(st) {
            #(mk_undefined(), add_entry(st, k, v))
          })
          add_entries_loop(st, target, rec, add_entry)
        }
        False ->
          close_throw_type(
            st,
            rec.iterator,
            "Iterator value "
              <> describe(st, entry)
              <> " is not an entry object",
          )
      }
    }
  }
}

/// §24.1.1.2 AddEntriesFromIterable(target, iterable, adder) — the Map
/// (§24.1.1.1) / WeakMap (§24.3.1.1) constructors' entry drain. `adder` is
/// the user-reachable `set` method, so it must be `[[Call]]`ed observably.
pub fn add_entries_from_iterable(
  st: Agent,
  target: JsVal,
  iterable: JsVal,
  adder: JsVal,
) -> #(JsVal, Agent) {
  use st, k, v <- add_entries_with_sink(st, target, iterable)
  let #(_, st) = t_call_checked(st, adder, target, [k, v])
  st
}

/// Value-iteration analogue of AddEntriesFromIterable — §24.2.1.1 Set steps
/// 6-8 / §24.4.1.1 WeakSet steps 5-7: for each iterator value `v`, call
/// `adder(target, [v])`, closing the iterator if the adder throws
/// (IfAbruptCloseIterator). Returns `target` on normal completion.
pub fn add_values_from_iterable(
  st: Agent,
  target: JsVal,
  iterable: JsVal,
  adder: JsVal,
) -> #(JsVal, Agent) {
  let #(rec, st) = get_iterator_sync(st, iterable)
  add_values_loop(st, target, rec, adder)
}

fn add_values_loop(
  st: Agent,
  target: JsVal,
  rec: IteratorRecord,
  adder: JsVal,
) -> #(JsVal, Agent) {
  use step, done, st <- iterator_step_result(st, rec)
  case done {
    True -> #(target, st)
    False -> {
      let #(v, st) = rt_obj.t_get_prop(st, step, StringKey(Named("value")))
      use _add_result, st <- or_close(st, rec.iterator, fn(st) {
        t_call_checked(st, adder, target, [v])
      })
      add_values_loop(st, target, rec, adder)
    }
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// Destructuring / yield* support — the two protocol ops the emitted-code
// paths and the generator machinery drive directly.
// ═══════════════════════════════════════════════════════════════════════════

/// §13.15.5.3 / §14.3.3 BindingRestElement: drain an already-obtained
/// iterator object into a fresh Array via repeated `.next()` — does NOT
/// re-GetIterator, so works for bare `{next}` iterators that don't inherit
/// `%IteratorPrototype%`. `.next()` throwing propagates without close.
pub fn iterator_rest(st: Agent, iter: JsVal) -> #(JsVal, Agent) {
  let #(rec, st) =
    get_iterator_direct(
      st,
      iter,
      "Iterator rest element target is not an object",
    )
  let #(values, st) = iterator_to_list(st, rec)
  let #(h, st) = alloc_array(st, values)
  #(mk_object(h), st)
}

/// §7.4.5 IteratorComplete + §7.4.6 IteratorValue: read `{done, value}` from
/// an iterator result object; TypeError if it isn't an object. Both reads can
/// run user getters, so `st` is threaded through. Shared by IteratorNext /
/// yield* paths, sync-generator delegate forwarding, async-generator delegate
/// resume. (§7.4.8 IteratorStep must NOT read `value` when done — that
/// variant is `iterator_step_done`.)
pub fn read_iter_result(st: Agent, res: JsVal) -> #(#(Bool, JsVal), Agent) {
  case rt_val.is_object(res) {
    True -> {
      let #(done, st) = rt_obj.t_get_prop(st, res, StringKey(Named("done")))
      let #(val, st) = rt_obj.t_get_prop(st, res, StringKey(Named("value")))
      #(#(rt_val.to_boolean(done), val), st)
    }
    False -> throw_type_error(st, "Iterator result is not an object")
  }
}

/// Resolve a GetIterator stack slot to the real iterator object. 2core stores
/// `IteratorRecord` at the Gleam level (not on-heap), so no unwrapping is
/// needed — identity. Kept for arc-parity call sites (yield* delegation).
pub fn unwrap_record_value(_st: Agent, v: JsVal) -> JsVal {
  v
}
