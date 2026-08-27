import arc/internal/ordered_entries
import arc/internal/tree_array
import arc/rt/builtins/helpers
import arc/rt/call.{
  type Completion, NormalCompletion, ThrowCompletion, is_callable, t_call,
  t_call_checked,
}
import arc/rt/elements as rt_elements
import arc/rt/js_string
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsElements, type JsOps, type JsSlot, type JsVal,
  type Property, type PropertyKey, ArrayObj, AsyncFromSyncIterator, DataProperty,
  Dense, Index, IteratorRecord, JInt, KHandle, KNull, KStr, KUndef,
  MapIterEntries, MapIterKeys, MapIterValues, MapIterator, MapObj, Named,
  NoElements, SObject, SetIterEntries, SetIterValues, SetIterator, SetObj,
  StringIterator, StringKey, SymbolKey, TypeErr, classify, map_key_to_js,
  mk_number, mk_object, mk_string, mk_undefined, symbol_async_iterator,
  symbol_iterator,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

pub type IteratorRecord =
  rt_types.IteratorRecord

// catches js throws like t_call
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

fn describe(st: Agent, v: JsVal) -> String {
  let #(ty, _) = rt_val.t_type_of(st, v)
  ty
}

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

// §7.4.9, next is read once and cached
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

// §7.4.3 sync
pub fn get_iterator_sync(st: Agent, obj: JsVal) -> #(IteratorRecord, Agent) {
  let #(method, st) = rt_obj.t_get_prop(st, obj, SymbolKey(symbol_iterator))
  case is_callable(st, method) {
    False -> throw_type_error(st, describe(st, obj) <> " is not iterable")
    True -> get_iterator_from_method(st, obj, method)
  }
}

// §7.4.4
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

// §7.4.3 async, falls back to wrapped sync iterator
pub fn get_iterator_async(st: Agent, obj: JsVal) -> #(IteratorRecord, Agent) {
  let #(method, st) =
    rt_obj.t_get_prop(st, obj, SymbolKey(symbol_async_iterator))
  case classify(method) {
    KUndef | KNull -> {
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

// §27.1.6.1, proto must be %AsyncFromSyncIteratorPrototype%
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

pub type PrimitiveHandling {
  IterateStrings
  RejectPrimitives
}

// §7.4.13
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

// §7.4.6 prefix; value is only read by cont
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

// §7.4.8
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

// §7.4.6 done only, must not read value
pub fn iterator_step_done(st: Agent, rec: IteratorRecord) -> #(Bool, Agent) {
  use _result, done, st <- iterator_step_result(st, rec)
  #(done, st)
}

// §7.4.14
pub fn iterator_to_list(
  st: Agent,
  rec: IteratorRecord,
) -> #(List(JsVal), Agent) {
  case array_values_iterator(st, rec) {
    Some(iter_h) -> array_values_to_list(st, rec, iter_h, [])
    None ->
      case intrinsic_next(st, rec) {
        Some(#(next, iter_h)) -> native_to_list(st, rec, next, iter_h, [])
        None -> iterator_to_list_loop(st, rec, [])
      }
  }
}

fn native_to_list(
  st: Agent,
  rec: IteratorRecord,
  next: rt_types.IteratorNative,
  iter_h: Handle,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case native_step(st, next, iter_h) {
    Some(#(Some(v), st)) -> native_to_list(st, rec, next, iter_h, [v, ..acc])
    Some(#(None, st)) -> #(list.reverse(acc), st)
    None ->
      case iterator_step_value(st, rec) {
        #(None, st) -> #(list.reverse(acc), st)
        #(Some(v), st) -> native_to_list(st, rec, next, iter_h, [v, ..acc])
      }
  }
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

fn array_values_iterator(st: Agent, rec: IteratorRecord) -> Option(Handle) {
  case classify(rec.next_method), classify(rec.iterator) {
    KHandle(next_h), KHandle(iter_h) ->
      case rt_store.t_cell_get(st, next_h), rt_store.t_cell_get(st, iter_h) {
        SObject(
          kind: rt_types.KNative(
            tag: rt_types.IteratorN(rt_types.ArrayIteratorNext),
            ..,
          ),
          ..,
        ),
          SObject(
            kind: rt_types.ArrayIterator(kind: rt_types.ArrayIterValues, ..),
            ..,
          )
        -> Some(iter_h)
        _, _ -> None
      }
    _, _ -> None
  }
}

// fast path over plain array elements until a hole or own prop
fn array_values_to_list(
  st: Agent,
  rec: IteratorRecord,
  iter_h: Handle,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  let assert SObject(kind: rt_types.ArrayIterator(target:, index:, kind:), ..) as iter_slot =
    rt_store.t_cell_get(st, iter_h)
  case index < 0 {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(acc, stop) = case rt_store.t_cell_get(st, target) {
        SObject(kind: ArrayObj(length:), elements:, props:, ..) ->
          walk_elements(elements, props, index, length, acc)
        _ -> #(acc, index)
      }
      let st = case stop == index {
        True -> st
        False ->
          rt_store.t_cell_set(
            st,
            iter_h,
            SObject(
              ..iter_slot,
              kind: rt_types.ArrayIterator(target:, index: stop, kind:),
            ),
          )
      }
      case iterator_step_value(st, rec) {
        #(None, st) -> #(list.reverse(acc), st)
        #(Some(v), st) -> array_values_to_list(st, rec, iter_h, [v, ..acc])
      }
    }
  }
}

fn walk_elements(
  elements: JsElements,
  props: Dict(PropertyKey, Property),
  i: Int,
  length: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Int) {
  case i < length {
    False -> #(acc, i)
    True ->
      case dict.has_key(props, Index(i)), rt_elements.get_option(elements, i) {
        False, Some(v) ->
          walk_elements(elements, props, i + 1, length, [v, ..acc])
        _, _ -> #(acc, i)
      }
  }
}

// builtin next steps run no user code, so step in place
pub fn intrinsic_next(
  st: Agent,
  rec: IteratorRecord,
) -> Option(#(rt_types.IteratorNative, Handle)) {
  case classify(rec.next_method), classify(rec.iterator) {
    KHandle(next_h), KHandle(iter_h) ->
      case rt_store.t_cell_get(st, next_h) {
        SObject(kind: rt_types.KNative(tag: rt_types.IteratorN(next), ..), ..) ->
          Some(#(next, iter_h))
        _ -> None
      }
    _, _ -> None
  }
}

// none means caller takes the protocol step; never throws
pub fn native_step(
  st: Agent,
  next: rt_types.IteratorNative,
  iter_h: Handle,
) -> Option(#(Option(JsVal), Agent)) {
  let slot = rt_store.t_cell_get(st, iter_h)
  case next, slot {
    rt_types.ArrayIteratorNext, SObject(kind: rt_types.ArrayIterator(..), ..) ->
      array_iterator_step(st, iter_h, slot)
    rt_types.MapIteratorNext, SObject(kind: MapIterator(..), ..) ->
      Some(map_iterator_step(st, iter_h, slot))
    rt_types.SetIteratorNext, SObject(kind: SetIterator(..), ..) ->
      Some(set_iterator_step(st, iter_h, slot))
    rt_types.StringIteratorNext, SObject(kind: StringIterator(..), ..) ->
      Some(string_iterator_step(st, iter_h, slot))
    _, _ -> None
  }
}

fn array_iterator_step(
  st: Agent,
  iter_h: Handle,
  slot: JsSlot,
) -> Option(#(Option(JsVal), Agent)) {
  case slot {
    SObject(kind: rt_types.ArrayIterator(target:, index:, kind:), ..)
      if index >= 0
    ->
      case rt_store.t_cell_get(st, target) {
        SObject(kind: ArrayObj(length:), ..) if index >= length ->
          Some(#(
            None,
            rt_store.t_cell_set(
              st,
              iter_h,
              SObject(
                ..slot,
                kind: rt_types.ArrayIterator(target:, index: -1, kind:),
              ),
            ),
          ))
        SObject(kind: ArrayObj(_), ..) -> {
          let out = case kind {
            rt_types.ArrayIterKeys -> Some(#(mk_number(JInt(index)), st))
            rt_types.ArrayIterValues ->
              case helpers.own_element(st, mk_object(target), index) {
                helpers.Hit(v) -> Some(#(v, st))
                helpers.Slow -> None
              }
            rt_types.ArrayIterEntries ->
              case helpers.own_element(st, mk_object(target), index) {
                helpers.Hit(v) ->
                  Some(rt_obj.t_new_array(st, [mk_number(JInt(index)), v]))
                helpers.Slow -> None
              }
          }
          use #(v, st) <- option.map(out)
          let st =
            rt_store.t_cell_set(
              st,
              iter_h,
              SObject(
                ..slot,
                kind: rt_types.ArrayIterator(target:, index: index + 1, kind:),
              ),
            )
          #(Some(v), st)
        }
        _ -> None
      }
    _ -> None
  }
}

pub fn map_iterator_step(
  st: Agent,
  iter_h: Handle,
  slot: JsSlot,
) -> #(Option(JsVal), Agent) {
  case slot {
    SObject(kind: MapIterator(target:, index:, kind:), ..) if index >= 0 -> {
      let step = case rt_store.t_cell_get(st, target) {
        SObject(kind: MapObj(entries:), ..) ->
          ordered_entries.next_from(entries, index)
        _ -> None
      }
      case step {
        None -> #(
          None,
          rt_store.t_cell_set(
            st,
            iter_h,
            SObject(..slot, kind: MapIterator(target:, index: -1, kind:)),
          ),
        )
        Some(#(next_cursor, mk, v)) -> {
          let #(out, st) = case kind {
            MapIterKeys -> #(map_key_to_js(mk), st)
            MapIterValues -> #(v, st)
            MapIterEntries -> rt_obj.t_new_array(st, [map_key_to_js(mk), v])
          }
          let st =
            rt_store.t_cell_set(
              st,
              iter_h,
              SObject(
                ..slot,
                kind: MapIterator(target:, index: next_cursor, kind:),
              ),
            )
          #(Some(out), st)
        }
      }
    }
    _ -> #(None, st)
  }
}

pub fn set_iterator_step(
  st: Agent,
  iter_h: Handle,
  slot: JsSlot,
) -> #(Option(JsVal), Agent) {
  case slot {
    SObject(kind: SetIterator(target:, index:, kind:), ..) if index >= 0 -> {
      let step = case rt_store.t_cell_get(st, target) {
        SObject(kind: SetObj(entries:), ..) ->
          ordered_entries.next_from(entries, index)
        _ -> None
      }
      case step {
        None -> #(
          None,
          rt_store.t_cell_set(
            st,
            iter_h,
            SObject(..slot, kind: SetIterator(target:, index: -1, kind:)),
          ),
        )
        Some(#(next_cursor, _mk, v)) -> {
          let #(out, st) = case kind {
            SetIterValues -> #(v, st)
            SetIterEntries -> rt_obj.t_new_array(st, [v, v])
          }
          let st =
            rt_store.t_cell_set(
              st,
              iter_h,
              SObject(
                ..slot,
                kind: SetIterator(target:, index: next_cursor, kind:),
              ),
            )
          #(Some(out), st)
        }
      }
    }
    _ -> #(None, st)
  }
}

pub fn string_iterator_step(
  st: Agent,
  h: Handle,
  slot: JsSlot,
) -> #(Option(JsVal), Agent) {
  case slot {
    SObject(kind: StringIterator(source:, index:), ..) if index >= 0 ->
      case js_string.char_at_offset(source, index) {
        None -> #(
          None,
          rt_store.t_cell_set(
            st,
            h,
            SObject(..slot, kind: StringIterator(source:, index: -1)),
          ),
        )
        Some(#(ch, next)) -> #(
          Some(mk_string(ch)),
          rt_store.t_cell_set(
            st,
            h,
            SObject(..slot, kind: StringIterator(source:, index: next)),
          ),
        )
      }
    _ -> #(None, st)
  }
}

pub type ReturnCall {
  NoReturnMethod
  Returned(JsVal)
}

// §7.4.11 steps 3-4, throws are caught not propagated
pub fn call_return(
  st: Agent,
  obj: JsVal,
) -> #(Result(ReturnCall, JsVal), Agent) {
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
          case is_callable(st, ret_fn) {
            False -> {
              let #(e, st) =
                new_type_error(st, "iterator.return is not a function")
              #(Error(e), st)
            }
            True ->
              case t_call(st, ret_fn, obj, []) {
                #(NormalCompletion(v), st) -> #(Ok(Returned(v)), st)
                #(ThrowCompletion(e), st) -> #(Error(e), st)
              }
          }
      }
  }
}

// original error wins over close errors
pub fn close_and_throw(
  st: Agent,
  obj: JsVal,
  original: JsVal,
) -> #(JsVal, Agent) {
  let #(_ignored, st) = call_return(st, obj)
  #(original, st)
}

pub fn close_throw(st: Agent, obj: JsVal, original: JsVal) -> a {
  let #(thrown, st) = close_and_throw(st, obj, original)
  rt_store.t_throw(st, thrown)
}

pub fn close_throw_type(st: Agent, obj: JsVal, msg: String) -> a {
  let #(err, st) = new_type_error(st, msg)
  close_throw(st, obj, err)
}

// §7.4.11 normal completion
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

// on throw, close iter then rethrow the original
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

pub type EntrySink =
  fn(Agent, JsVal, JsVal) -> Agent

// §24.1.1.2
pub fn add_entries_with_sink(
  st: Agent,
  target: JsVal,
  iterable: JsVal,
  add_entry: EntrySink,
) -> #(JsVal, Agent) {
  let #(rec, st) = get_iterator_sync(st, iterable)
  add_entries_loop(st, target, rec, add_entry)
}

// next/done/value throws skip close; entry reads and sink close first
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

// set/weakset constructor value drain
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

// no getiterator, so bare {next} objects work
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

// §7.4.5 + §7.4.6, always reads value
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

// identity, kept for existing call sites
pub fn unwrap_record_value(_st: Agent, v: JsVal) -> JsVal {
  v
}
