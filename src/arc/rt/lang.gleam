import arc/bytecode/key.{type PropertyKey, Named}
import arc/rt/async as rt_async
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/object as b_object
import arc/rt/call.{NormalCompletion, ThrowCompletion, call} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type IteratorNative, type IteratorRecord, type JsVal,
  type ObjectKey, type Store, Agent, DataProperty, GeneratorN, GeneratorNext,
  GeneratorObj, IteratorN, IteratorRecord, KHandle, KNull, KUndef, NativeFn,
  Ordinary, SObject, Store, StringKey, classify, mk_bool, mk_object, mk_string,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// iterator record is a null-proto object: iterator, next, done
pub type IterHint {
  Sync
  Async
}

const iterator_key = StringKey(Named("iterator"))

const next_key = StringKey(Named("next"))

const done_key = StringKey(Named("done"))

pub fn alloc_record(st: Agent, rec: IteratorRecord) -> #(JsVal, Agent) {
  let store = st.store
  let seq = store.prop_seq
  let props =
    dict.from_list([
      #(
        Named("iterator"),
        types.DataProperty(
          value: rec.iterator,
          writable: True,
          enumerable: True,
          configurable: True,
          seq: seq,
        ),
      ),
      #(
        Named("next"),
        types.DataProperty(
          value: rec.next_method,
          writable: True,
          enumerable: True,
          configurable: True,
          seq: seq + 1,
        ),
      ),
      #(
        Named("done"),
        types.DataProperty(
          value: mk_bool(False),
          writable: True,
          enumerable: True,
          configurable: True,
          seq: seq + 2,
        ),
      ),
    ])
  let st = Agent(..st, store: Store(..store, prop_seq: seq + 3))
  let #(h, st) =
    rt_store.cell_new(
      st,
      types.SObject(
        kind: Ordinary,
        proto: None,
        props: props,
        symbol_props: [],
        elements: types.NoElements,
        extensible: True,
      ),
    )
  #(mk_object(h), st)
}

pub fn record_parts(st: Agent, rec: JsVal) -> Option(IteratorRecord) {
  record_props(st, rec) |> option.then(parts_of)
}

fn record_props(
  st: Agent,
  rec: JsVal,
) -> Option(Dict(PropertyKey, types.Property)) {
  case classify(rec) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: Ordinary, props:, ..) -> Some(props)
        _ -> None
      }
    _ -> None
  }
}

fn parts_of(
  props: Dict(PropertyKey, types.Property),
) -> Option(IteratorRecord) {
  case dict.get(props, Named("iterator")), dict.get(props, Named("next")) {
    Ok(DataProperty(value: iterator, ..)),
      Ok(DataProperty(value: next_method, ..))
    -> Some(IteratorRecord(iterator:, next_method:))
    _, _ -> None
  }
}

type PlainRecord {
  PlainRecord(done: Bool, record: IteratorRecord, native: NativeIter)
  RecordMiss
}

@external(erlang, "arc_rt_lang_ffi", "plain_iter_record")
fn plain_iter_record(st: Agent, rec: JsVal) -> PlainRecord

fn read_record(st: Agent, rec: JsVal) -> #(Bool, IteratorRecord, Agent) {
  case record_fields(st, rec) {
    Some(#(done, record)) -> #(done, record, st)
    None -> {
      let #(done, st) = rt_obj.get_prop(st, rec, done_key)
      let #(iterator, st) = rt_obj.get_prop(st, rec, iterator_key)
      let #(next_method, st) = rt_obj.get_prop(st, rec, next_key)
      #(rt_val.to_boolean(done), IteratorRecord(iterator:, next_method:), st)
    }
  }
}

fn record_fields(st: Agent, rec: JsVal) -> Option(#(Bool, IteratorRecord)) {
  case record_props(st, rec) {
    Some(props) ->
      case dict.get(props, Named("done")), parts_of(props) {
        Ok(DataProperty(value: done, ..)), Some(record) ->
          Some(#(rt_val.to_boolean(done), record))
        _, _ -> None
      }
    None -> None
  }
}

fn mark_done(st: Agent, rec: JsVal) -> Agent {
  let #(_, st) = rt_obj.set_prop(st, rec, done_key, mk_bool(True))
  st
}

type NativeIter {
  NativeNext(next: IteratorNative, iter_h: Handle)
  NativeGenerator(data: Handle)
  NativeMiss
}

fn native_iter(st: Agent, record: IteratorRecord) -> NativeIter {
  case classify(record.next_method), classify(record.iterator) {
    KHandle(next_h), KHandle(iter_h) ->
      case rt_store.cell_get(st, next_h) {
        SObject(kind: NativeFn(token: IteratorN(next), ..), ..) ->
          NativeNext(next, iter_h)
        SObject(kind: NativeFn(token: GeneratorN(GeneratorNext), ..), ..) ->
          case rt_store.cell_get(st, iter_h) {
            SObject(kind: GeneratorObj(data:), ..) -> NativeGenerator(data)
            _ -> NativeMiss
          }
        _ -> NativeMiss
      }
    _, _ -> NativeMiss
  }
}

fn generator_step(
  st: Agent,
  rec: JsVal,
  data: Handle,
) -> #(Option(JsVal), Agent) {
  let step = fn(st) { rt_async.gen_step(st, data, mk_undefined()) }
  case rt_call.try_run(st, step) {
    #(NormalCompletion(#(True, _)), st) -> #(None, st)
    #(NormalCompletion(#(False, v)), st) -> #(Some(v), st)
    #(ThrowCompletion(thrown), st) -> rt_store.throw(mark_done(st, rec), thrown)
  }
}

// §7.4.3 getiterator, returns the record object
pub fn get_iterator(st: Agent, obj: JsVal, hint: IterHint) -> #(JsVal, Agent) {
  let #(rec, st) = case hint {
    Sync -> iter_protocol.get_iterator_sync(st, obj)
    Async -> iter_protocol.get_iterator_async(st, obj)
  }
  alloc_record(st, rec)
}

// §7.4.8 iteratorstepvalue; a throw marks the record done first
pub fn iter_next(st: Agent, rec: JsVal) -> #(#(Bool, JsVal), Agent) {
  let #(done, record, native, st) = case plain_iter_record(st, rec) {
    PlainRecord(done:, record:, native:) -> #(done, record, native, st)
    RecordMiss -> {
      let #(done, record, st) = read_record(st, rec)
      #(done, record, native_iter(st, record), st)
    }
  }
  use <- bool.guard(done, #(#(True, mk_undefined()), st))
  let stepped = case native {
    NativeNext(next, iter_h) -> iter_protocol.native_step(st, next, iter_h)
    NativeGenerator(data) -> Some(generator_step(st, rec, data))
    NativeMiss -> None
  }
  case stepped {
    Some(#(Some(v), st)) -> #(#(False, v), st)
    Some(#(None, st)) -> #(#(True, mk_undefined()), mark_done(st, rec))
    None -> protocol_step(st, rec, record)
  }
}

fn protocol_step(
  st: Agent,
  rec: JsVal,
  record: IteratorRecord,
) -> #(#(Bool, JsVal), Agent) {
  let step = fn(st) {
    use result, done, st <- iter_protocol.iterator_step_result(st, record)
    case done {
      True -> #(#(True, mk_undefined()), st)
      False -> {
        let #(v, st) = rt_obj.get_prop(st, result, StringKey(Named("value")))
        #(#(False, v), st)
      }
    }
  }
  case rt_call.try_run(st, step) {
    #(NormalCompletion(#(True, _) as pair), st) -> #(pair, mark_done(st, rec))
    #(NormalCompletion(pair), st) -> #(pair, st)
    #(ThrowCompletion(thrown), st) -> rt_store.throw(mark_done(st, rec), thrown)
  }
}

// §7.4.11 iteratorclose; abrupt swallows what return() does
pub fn iter_close(st: Agent, rec: JsVal, abrupt abrupt: Bool) -> Agent {
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

// §14.3.3 rest element
pub fn iter_rest(st: Agent, rec: JsVal) -> #(JsVal, Agent) {
  let #(done, record, st) = read_record(st, rec)
  case done {
    True -> rt_obj.new_array(st, [])
    False -> {
      let st = mark_done(st, rec)
      let #(values, st) = iter_protocol.iterator_to_list(st, record)
      rt_obj.new_array(st, values)
    }
  }
}

pub fn spread_into_list(
  st: Agent,
  acc: List(JsVal),
  iterable: JsVal,
) -> #(List(JsVal), Agent) {
  let #(values, st) = case array_spread(st, iterable) {
    Spread(values) -> #(values, st)
    SpreadMiss -> {
      let #(record, st) = iter_protocol.get_iterator_sync(st, iterable)
      iter_protocol.iterator_to_list(st, record)
    }
  }
  #(list.append(acc, values), st)
}

pub type PlainSpread {
  Spread(List(JsVal))
  SpreadMiss
}

// a plain array whose iteration observes nothing, holes excluded
@external(erlang, "arc_rt_lang_ffi", "array_spread")
pub fn array_spread(st: Agent, iterable: JsVal) -> PlainSpread

// §14.7.5.7 step 6.a, not awaited here
pub fn async_iter_next(st: Agent, rec: JsVal) -> #(JsVal, Agent) {
  let #(_done, record, st) = read_record(st, rec)
  call(st, record.next_method, record.iterator, [])
}

// §7.3.25 copydataproperties for {...source}
pub fn copy_data_props(
  st: Agent,
  target: JsVal,
  source: JsVal,
) -> #(JsVal, Agent) {
  case rt_obj.plain_copy_data_props(st, target, source) {
    rt_obj.Copied(st) -> #(target, st)
    rt_obj.CopyMiss -> {
      let assert KHandle(target_h) = classify(target)
      #(target, copy_data_properties(st, target_h, source, []))
    }
  }
}

// object rest pattern, excluded keys skipped
pub fn object_rest(
  st: Agent,
  source: JsVal,
  excluded: List(k),
) -> #(JsVal, Agent) {
  let #(h, st) = rt_obj.new_object(st, Some(st.realm.object.prototype))
  let excluded = list.map(excluded, rt_store.as_object_key)
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
      let #(from, st) = rt_val.to_object(st, source)
      let #(keys, st) = rt_obj.own_keys(st, from)
      use st, key <- list.fold(keys, st)
      use <- bool.guard(list.contains(excluded, key), st)
      let #(prop, st) = rt_obj.get_own_property(st, from, key)
      let wanted =
        option.map(prop, types.prop_enumerable) |> option.unwrap(False)
      case wanted {
        False -> st
        True -> {
          let #(v, st) = rt_obj.get_prop(st, mk_object(from), key)
          let #(_, st) =
            rt_obj.define_own_data(
              st,
              target,
              key,
              v,
              writable: True,
              enumerable: True,
              configurable: True,
            )
          st
        }
      }
    }
  }
}

// §13.2.8.4 gettemplateobject, cached per realm and site
pub fn get_template_object(
  st: Agent,
  site: String,
  cooked: List(JsVal),
  raw: List(String),
) -> #(JsVal, Agent) {
  let site = int.to_string(st.realm.id) <> ":" <> site
  case dict.get(st.template_objects, site) {
    Ok(h) -> #(mk_object(h), st)
    Error(Nil) -> {
      let #(raw_v, st) = rt_obj.new_array(st, list.map(raw, mk_string))
      let assert KHandle(raw_h) = classify(raw_v)
      let st = b_object.freeze(st, raw_h)
      let #(tpl_v, st) = rt_obj.new_array(st, cooked)
      let assert KHandle(tpl_h) = classify(tpl_v)
      let #(_, st) =
        rt_obj.define_own_data(
          st,
          tpl_h,
          StringKey(Named("raw")),
          raw_v,
          writable: False,
          enumerable: False,
          configurable: False,
        )
      let st = b_object.freeze(st, tpl_h)
      let st = rt_store.pin_root(st, tpl_h)
      let st =
        Agent(
          ..st,
          template_objects: dict.insert(st.template_objects, site, tpl_h),
        )
      #(tpl_v, st)
    }
  }
}

// §13.5.1.2 step 5 sloppy delete on the global
pub fn global_delete(st: Agent, name: String) -> #(Bool, Agent) {
  rt_obj.delete_prop(st, st.realm.global_object, StringKey(key.canonical(name)))
}

// for-of over plain arrays and strings keeps this record on the operand stack
pub type ArrayIterStep {
  IterStep(done: Bool, value: JsVal, rec: JsVal)
  // a map entry that still needs its pair array
  IterPair(key: JsVal, value: JsVal, rec: JsVal)
  IterMiss
}

@external(erlang, "arc_rt_lang_ffi", "array_iter_start")
pub fn array_iter_start(st: Agent, iterable: JsVal) -> JsVal

@external(erlang, "arc_rt_lang_ffi", "array_iter_next")
pub fn array_iter_next(store: Store, rec: JsVal) -> ArrayIterStep

@external(erlang, "arc_rt_lang_ffi", "is_array_iter")
pub fn is_array_iter(v: JsVal) -> Bool

@external(erlang, "arc_rt_lang_ffi", "array_iter_parts")
pub fn array_iter_parts(rec: JsVal) -> #(JsVal, Int, JsVal)

@external(erlang, "arc_rt_lang_ffi", "array_iter_proto")
pub fn array_iter_proto(st: Agent, rec: JsVal) -> Handle

@external(erlang, "arc_rt_lang_ffi", "array_iter_record")
pub fn array_iter_record(target: JsVal, index: Int, next_fn: JsVal) -> JsVal

// absent name throws referenceerror; called by name from arc_rt_obj_ffi
pub fn global_get(st: Agent, name: BitArray) -> #(JsVal, Agent) {
  let g = types.mk_object(st.realm.global_object)
  let key = StringKey(binary_key(name))
  let #(has, st) = rt_obj.has_prop(st, g, key)
  case has {
    True -> rt_obj.get_prop(st, g, key)
    False -> {
      let text = bit_array.to_string(name) |> result.unwrap("")
      rt_val.throw_reference_error(st, text <> " is not defined")
    }
  }
}

pub fn global_this(st: Agent) -> JsVal {
  types.mk_object(st.realm.global_object)
}

// sloppy: failed set ignored
pub fn global_set(st: Agent, name: BitArray, v: JsVal) -> Agent {
  let g = st.realm.global_object
  let #(_, st) =
    rt_obj.set_prop(st, types.mk_object(g), StringKey(binary_key(name)), v)
  st
}

// strict: unresolvable throws referenceerror, failed set typeerror
pub fn global_set_strict(st: Agent, name: BitArray, v: JsVal) -> Agent {
  let g = types.mk_object(st.realm.global_object)
  let key = StringKey(binary_key(name))
  let text = bit_array.to_string(name) |> result.unwrap("")
  let #(has, st) = rt_obj.has_prop(st, g, key)
  case has {
    False -> rt_val.throw_reference_error(st, text <> " is not defined")
    True -> {
      let #(ok, st) = rt_obj.set_prop(st, g, key, v)
      case ok {
        True -> st
        False ->
          rt_val.throw_type_error(
            st,
            "Cannot assign to read only property '" <> text <> "'",
          )
      }
    }
  }
}

// unresolvable global yields "undefined" without throwing
pub fn global_typeof(st: Agent, name: BitArray) -> #(String, Agent) {
  let g = st.realm.global_object
  let key = StringKey(binary_key(name))
  let #(has, st) = rt_obj.has_prop(st, types.mk_object(g), key)
  case has {
    False -> #("undefined", st)
    True -> {
      let #(v, st) = rt_obj.get_prop(st, types.mk_object(g), key)
      #(rt_val.type_of(st, v), st)
    }
  }
}

fn binary_key(name: BitArray) -> PropertyKey {
  case bit_array.to_string(name) {
    Ok(s) -> key.canonical(s)
    Error(Nil) -> Named("")
  }
}
