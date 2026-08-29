import arc/bytecode/key.{type PropertyKey, Named, canonical_key}
import arc/rt/async as rt_async
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/object as b_object
import arc/rt/builtins/regexp as b_regexp
import arc/rt/call.{t_call_checked}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type IteratorNative, type IteratorRecord, type JsVal,
  type ObjectKey, Agent, DataProperty, GeneratorN, GeneratorNext, GeneratorObj,
  IteratorN, IteratorRecord, JsStore, KHandle, KNative, KNull, KUndef,
  NoElements, Ordinary, SObject, StringKey, TypeErr, classify, mk_bool,
  mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

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

pub fn t_new_error(st: Agent, message: String) -> #(JsVal, Agent) {
  st.store.ops.new_error(st, TypeErr, message)
}

// iterator record is a null-proto object: iterator, next, done
pub type IterHint {
  Sync
  Async
}

const k_iterator = StringKey(Named("iterator"))

const k_next = StringKey(Named("next"))

const k_done = StringKey(Named("done"))

fn alloc_record(st: Agent, rec: IteratorRecord) -> #(JsVal, Agent) {
  let js = st.store
  let seq = js.prop_seq
  let props =
    dict.from_list([
      #(Named("iterator"), DataProperty(rec.iterator, True, True, True, seq)),
      #(Named("next"), DataProperty(rec.next_method, True, True, True, seq + 1)),
      #(Named("done"), DataProperty(mk_bool(False), True, True, True, seq + 2)),
    ])
  let st = Agent(..st, store: JsStore(..js, prop_seq: seq + 3))
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: None,
        props:,
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(h), st)
}

pub fn t_alloc_record(st: Agent, rec: IteratorRecord) -> #(JsVal, Agent) {
  alloc_record(st, rec)
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
      case rt_store.t_cell_get(st, h) {
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

@external(erlang, "arc_rt_lang_ffi", "iter_fast")
fn iter_fast(
  st: Agent,
  rec: JsVal,
) -> Option(#(Bool, IteratorRecord, NativeIter))

fn read_record(st: Agent, rec: JsVal) -> #(Bool, IteratorRecord, Agent) {
  case record_fields(st, rec) {
    Some(#(done, record)) -> #(done, record, st)
    None -> {
      let #(done, st) = rt_obj.t_get_prop(st, rec, k_done)
      let #(iterator, st) = rt_obj.t_get_prop(st, rec, k_iterator)
      let #(next_method, st) = rt_obj.t_get_prop(st, rec, k_next)
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
  let #(_, st) = rt_obj.t_set_prop(st, rec, k_done, mk_bool(True))
  st
}

type NativeIter {
  NativeNext(next: IteratorNative, iter_h: Handle)
  NativeGenerator(data: Handle)
  NotNative
}

fn native_iter(st: Agent, record: IteratorRecord) -> NativeIter {
  case classify(record.next_method), classify(record.iterator) {
    KHandle(next_h), KHandle(iter_h) ->
      case rt_store.t_cell_get(st, next_h) {
        SObject(kind: KNative(tag: IteratorN(next), ..), ..) ->
          NativeNext(next, iter_h)
        SObject(kind: KNative(tag: GeneratorN(GeneratorNext), ..), ..) ->
          case rt_store.t_cell_get(st, iter_h) {
            SObject(kind: GeneratorObj(data:), ..) -> NativeGenerator(data)
            _ -> NotNative
          }
        _ -> NotNative
      }
    _, _ -> NotNative
  }
}

fn generator_step(
  st: Agent,
  rec: JsVal,
  data: Handle,
) -> #(Option(JsVal), Agent) {
  let step = fn(st) { rt_async.t_gen_step(st, data, mk_undefined()) }
  case protected_step(st, step) {
    #(NormalCompletion(#(True, _)), st) -> #(None, st)
    #(NormalCompletion(#(False, v)), st) -> #(Some(v), st)
    #(ThrowCompletion(thrown), st) ->
      rt_store.t_throw(mark_done(st, rec), thrown)
  }
}

// §7.4.3 getiterator, returns the record object
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

// §7.4.8 iteratorstepvalue; a throw marks the record done first
pub fn t_iter_next(st: Agent, rec: JsVal) -> #(#(Bool, JsVal), Agent) {
  let #(done, record, native, st) = case iter_fast(st, rec) {
    Some(#(done, record, native)) -> #(done, record, native, st)
    None -> {
      let #(done, record, st) = read_record(st, rec)
      #(done, record, native_iter(st, record), st)
    }
  }
  use <- bool.guard(done, #(#(True, mk_undefined()), st))
  let stepped = case native {
    NativeNext(next, iter_h) -> iter_protocol.native_step(st, next, iter_h)
    NativeGenerator(data) -> Some(generator_step(st, rec, data))
    NotNative -> None
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
        let #(v, st) = rt_obj.t_get_prop(st, result, StringKey(Named("value")))
        #(#(False, v), st)
      }
    }
  }
  case protected_step(st, step) {
    #(NormalCompletion(#(True, _) as pair), st) -> #(pair, mark_done(st, rec))
    #(NormalCompletion(pair), st) -> #(pair, st)
    #(ThrowCompletion(thrown), st) ->
      rt_store.t_throw(mark_done(st, rec), thrown)
  }
}

// §7.4.11 iteratorclose; abrupt swallows what return() does
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

// §14.3.3 rest element
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

pub fn t_spread_into_list(
  st: Agent,
  acc: List(JsVal),
  iterable: JsVal,
) -> #(List(JsVal), Agent) {
  let #(record, st) = iter_protocol.get_iterator_sync(st, iterable)
  let #(values, st) = iter_protocol.iterator_to_list(st, record)
  #(list.append(acc, values), st)
}

// §14.7.5.7 step 6.a, not awaited here
pub fn t_async_iter_next(st: Agent, rec: JsVal) -> #(JsVal, Agent) {
  let #(_done, record, st) = read_record(st, rec)
  t_call_checked(st, record.next_method, record.iterator, [])
}

// §7.3.25 copydataproperties for {...source}
pub fn t_copy_data_props(
  st: Agent,
  target: JsVal,
  source: JsVal,
) -> #(JsVal, Agent) {
  let assert KHandle(target_h) = classify(target)
  #(target, copy_data_properties(st, target_h, source, []))
}

// object rest pattern, excluded keys skipped
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

pub fn t_regexp_new(
  st: Agent,
  pattern: String,
  flags: String,
) -> #(JsVal, Agent) {
  b_regexp.regexp_create_literal(st, pattern, flags)
}

// §13.2.8.4 gettemplateobject, cached per realm and site
pub fn t_get_template_object(
  st: Agent,
  site: String,
  cooked: List(JsVal),
  raw: List(String),
) -> #(JsVal, Agent) {
  let site = int.to_string(st.realm.id) <> ":" <> site
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

// §13.5.1.2 step 5 sloppy delete on the global
pub fn t_global_delete(st: Agent, name: String) -> #(Bool, Agent) {
  rt_obj.t_delete_prop(
    st,
    st.realm.global_object,
    StringKey(canonical_key(name)),
  )
}
