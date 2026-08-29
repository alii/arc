import arc/rt/async as rt_async
import arc/rt/buffer as rt_buffer
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{arg_at, first_arg_or_undefined}
import arc/rt/builtins/iter_protocol.{IterateStrings, RejectPrimitives}
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/limits
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type ArrayIterKind, type BuiltinPair, type ConcatItem,
  type GeneratorState, type Handle, type HelperBody, type IteratorHelperKind,
  type IteratorNative, type IteratorRecord, type JsVal, type NativeToken,
  type ObjKind, type ObjectKey, type ZipMember, type ZipMode, ArgumentsObj,
  ArrayIterEntries, ArrayIterKeys, ArrayIterValues, ArrayIterator, ArrayObj,
  AsyncFromSyncClose, AsyncFromSyncIterator, AsyncFromSyncNext,
  AsyncFromSyncReturn, AsyncFromSyncThrow, AsyncFromSyncUnwrap, ClassicHelper,
  ConcatHelper, ConcatItem, GenCompleted, GenExecuting, GenSuspendedStart,
  GenSuspendedYield, HelperDrop, HelperFilter, HelperFlatMap, HelperMap,
  HelperTake, IteratorConstructor, IteratorHelperObj, IteratorN, JFloat, JInt,
  JNan, JNegInf, JPosInf, KHandle, KNull, KStr, KUndef, MapIterator, NoElements,
  Ordinary, RangeErr, ReturnThis, SObject, SetIterator, StringIterator,
  StringKey, SymbolKey, TypeErr, TypedArrayObj, WrapForValidIteratorObj,
  ZipExhausted, ZipHelper, ZipLongest, ZipOpen, ZipShortest, ZipStrict, classify,
  mk_bool, mk_number, mk_object, mk_string, mk_undefined, symbol_async_iterator,
  symbol_iterator, symbol_to_string_tag,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

pub type IteratorProtos {
  IteratorProtos(
    iterator_proto: Handle,
    array_iter_proto: Handle,
    string_iter_proto: Handle,
    map_iter_proto: Handle,
    set_iter_proto: Handle,
    async_iterator_proto: Handle,
    async_from_sync_proto: Handle,
    iterator: BuiltinPair,
    iterator_helper_proto: Handle,
    wrap_for_valid_proto: Handle,
  )
}

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(IteratorProtos, Agent) {
  let #(iter_sym_fn, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      ReturnThis,
      "[Symbol.iterator]",
      0,
    )
  let #(iterator_proto, st) =
    alloc_proto_with_symbol(st, object_proto, symbol_iterator, iter_sym_fn)
  let #(array_iter_proto, st) =
    alloc_iter_proto(
      st,
      fn_proto,
      iterator_proto,
      IteratorN(rt_types.ArrayIteratorNext),
      "Array Iterator",
    )
  let #(string_iter_proto, st) =
    alloc_iter_proto(
      st,
      fn_proto,
      iterator_proto,
      IteratorN(rt_types.StringIteratorNext),
      "String Iterator",
    )
  let #(map_iter_proto, st) =
    alloc_iter_proto(
      st,
      fn_proto,
      iterator_proto,
      IteratorN(rt_types.MapIteratorNext),
      "Map Iterator",
    )
  let #(set_iter_proto, st) =
    alloc_iter_proto(
      st,
      fn_proto,
      iterator_proto,
      IteratorN(rt_types.SetIteratorNext),
      "Set Iterator",
    )
  let #(async_sym_fn, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      ReturnThis,
      "[Symbol.asyncIterator]",
      0,
    )
  let #(async_iterator_proto, st) =
    alloc_proto_with_symbol(
      st,
      object_proto,
      symbol_async_iterator,
      async_sym_fn,
    )
  let #(afs_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("next", IteratorN(AsyncFromSyncNext), 1),
      #("return", IteratorN(AsyncFromSyncReturn), 1),
      #("throw", IteratorN(AsyncFromSyncThrow), 1),
    ])
  let #(async_from_sync_proto, st) =
    common.alloc_proto(
      st,
      Some(async_iterator_proto),
      common.named_props(afs_methods),
    )
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("map", IteratorN(rt_types.IteratorPrototypeMap), 1),
      #("filter", IteratorN(rt_types.IteratorPrototypeFilter), 1),
      #("take", IteratorN(rt_types.IteratorPrototypeTake), 1),
      #("drop", IteratorN(rt_types.IteratorPrototypeDrop), 1),
      #("flatMap", IteratorN(rt_types.IteratorPrototypeFlatMap), 1),
      #("toArray", IteratorN(rt_types.IteratorPrototypeToArray), 0),
      #("forEach", IteratorN(rt_types.IteratorPrototypeForEach), 1),
      #("reduce", IteratorN(rt_types.IteratorPrototypeReduce), 1),
      #("some", IteratorN(rt_types.IteratorPrototypeSome), 1),
      #("every", IteratorN(rt_types.IteratorPrototypeEvery), 1),
      #("find", IteratorN(rt_types.IteratorPrototypeFind), 1),
    ])
  let #(ctor_props, st) =
    common.alloc_methods(st, fn_proto, [
      #("from", IteratorN(rt_types.IteratorFrom), 1),
      #("concat", IteratorN(rt_types.IteratorConcat), 0),
      #("zip", IteratorN(rt_types.IteratorZip), 1),
      #("zipKeyed", IteratorN(rt_types.IteratorZipKeyed), 1),
    ])
  let #(iterator, st) =
    common.init_type_on(
      st,
      iterator_proto,
      fn_proto,
      proto_methods,
      fn(_proto) { IteratorN(IteratorConstructor) },
      "Iterator",
      0,
      ctor_props,
      True,
    )
  let #(ctor_acc, st) =
    common.alloc_get_set_accessor(
      st,
      fn_proto,
      IteratorN(rt_types.IteratorProtoGetConstructor),
      IteratorN(rt_types.IteratorProtoSetConstructor),
      "constructor",
    )
  let #(tag_acc, st) =
    common.alloc_get_set_accessor(
      st,
      fn_proto,
      IteratorN(rt_types.IteratorProtoGetToStringTag),
      IteratorN(rt_types.IteratorProtoSetToStringTag),
      "[Symbol.toStringTag]",
    )
  let st =
    common.add_named_property(st, iterator_proto, "constructor", ctor_acc)
  let st =
    common.add_symbol_property(
      st,
      iterator_proto,
      symbol_to_string_tag,
      tag_acc,
    )
  let #(helper_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("next", IteratorN(rt_types.IteratorHelperNext), 0),
      #("return", IteratorN(rt_types.IteratorHelperReturn), 0),
    ])
  let #(iterator_helper_proto, st) =
    common.init_namespace(st, iterator_proto, "Iterator Helper", helper_methods)
  let #(wrap_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("next", IteratorN(rt_types.WrapForValidIteratorNext), 0),
      #("return", IteratorN(rt_types.WrapForValidIteratorReturn), 0),
    ])
  let #(wrap_for_valid_proto, st) =
    common.alloc_proto(
      st,
      Some(iterator_proto),
      common.named_props(wrap_methods),
    )
  #(
    IteratorProtos(
      iterator_proto:,
      array_iter_proto:,
      string_iter_proto:,
      map_iter_proto:,
      set_iter_proto:,
      async_iterator_proto:,
      async_from_sync_proto:,
      iterator:,
      iterator_helper_proto:,
      wrap_for_valid_proto:,
    ),
    st,
  )
}

fn alloc_proto_with_symbol(
  st: Agent,
  parent: Handle,
  sym: rt_types.SymbolId,
  fn_h: Handle,
) -> #(Handle, Agent) {
  let #(prop, st) = common.builtin_property(st, mk_object(fn_h))
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: Some(parent),
        props: dict.new(),
        symbol_props: [#(sym, prop)],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(h, rt_store.t_pin_root(st, h))
}

fn alloc_iter_proto(
  st: Agent,
  fn_proto: Handle,
  iterator_proto: Handle,
  next: NativeToken,
  tag: String,
) -> #(Handle, Agent) {
  let #(methods, st) = common.alloc_methods(st, fn_proto, [#("next", next, 0)])
  let #(h, st) =
    common.alloc_proto(st, Some(iterator_proto), common.named_props(methods))
  let st = common.add_to_string_tag(st, h, tag)
  #(h, st)
}

type AfsKind {
  AfsNext
  AfsReturn
  AfsThrow
}

pub fn dispatch(
  st: Agent,
  n: IteratorNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    AsyncFromSyncNext -> async_from_sync(st, this, args, AfsNext)
    AsyncFromSyncReturn -> async_from_sync(st, this, args, AfsReturn)
    AsyncFromSyncThrow -> async_from_sync(st, this, args, AfsThrow)
    AsyncFromSyncUnwrap(done:) -> {
      let v = first_arg_or_undefined(args)
      let #(h, st) = rt_async.alloc_iter_result(st, v, done)
      #(mk_object(h), st)
    }
    AsyncFromSyncClose(sync_iter:) -> {
      let err = first_arg_or_undefined(args)
      iter_protocol.close_throw(st, mk_object(sync_iter), err)
    }
    IteratorConstructor ->
      throw_type_error(st, "Abstract class Iterator not directly constructable")
    rt_types.IteratorFrom -> from(st, args)
    rt_types.IteratorZip -> zip(st, args)
    rt_types.IteratorZipKeyed -> zip_keyed(st, args)
    rt_types.IteratorConcat -> concat(st, args)
    rt_types.IteratorPrototypeMap ->
      lazy_helper(st, this, args, HelperMap, "map")
    rt_types.IteratorPrototypeFilter ->
      lazy_helper(st, this, args, HelperFilter, "filter")
    rt_types.IteratorPrototypeFlatMap ->
      lazy_helper(
        st,
        this,
        args,
        fn(func) { HelperFlatMap(func:, inner: None) },
        "flatMap",
      )
    rt_types.IteratorPrototypeTake ->
      take_or_drop(st, this, args, HelperTake, "take")
    rt_types.IteratorPrototypeDrop ->
      take_or_drop(st, this, args, HelperDrop, "drop")
    rt_types.IteratorPrototypeToArray -> to_array(st, this)
    rt_types.IteratorPrototypeForEach -> for_each(st, this, args)
    rt_types.IteratorPrototypeReduce -> reduce(st, this, args)
    rt_types.IteratorPrototypeSome ->
      bool_consumer(st, this, args, True, "some")
    rt_types.IteratorPrototypeEvery ->
      bool_consumer(st, this, args, False, "every")
    rt_types.IteratorPrototypeFind -> find(st, this, args)
    rt_types.IteratorHelperNext -> helper_next(st, this)
    rt_types.IteratorHelperReturn -> helper_return(st, this)
    rt_types.WrapForValidIteratorNext -> wrap_next(st, this)
    rt_types.WrapForValidIteratorReturn -> wrap_return(st, this)
    rt_types.IteratorProtoGetToStringTag -> #(mk_string("Iterator"), st)
    rt_types.IteratorProtoGetConstructor -> #(
      mk_object(st.realm.iterator.constructor),
      st,
    )
    rt_types.IteratorProtoSetToStringTag ->
      ignore_proto_setter(st, this, args, IgnoreSetTag)
    rt_types.IteratorProtoSetConstructor ->
      ignore_proto_setter(st, this, args, IgnoreSetCtor)
    rt_types.ArrayIteratorNext -> array_iterator_next(st, this)
    rt_types.MapIteratorNext -> map_iterator_next(st, this)
    rt_types.SetIteratorNext -> set_iterator_next(st, this)
    rt_types.StringIteratorNext -> string_iterator_next(st, this)
  }
}

// §23.1.5.2.1
fn array_iterator_next(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use st, iter_h, target, index, kind <- require_array_iter(st, this)
  case index < 0 {
    True -> iter_done(st)
    False -> {
      let #(len, st) = array_source_length(st, target)
      case index >= len {
        True ->
          iter_done(set_iter_kind(
            st,
            iter_h,
            ArrayIterator(target:, index: -1, kind:),
          ))
        False -> {
          let #(out, st) = case kind {
            ArrayIterKeys -> #(mk_number(JInt(index)), st)
            _ -> {
              let #(elem, st) = rt_obj.t_get_index(st, mk_object(target), index)
              case kind {
                ArrayIterValues -> #(elem, st)
                ArrayIterEntries -> alloc_pair(st, mk_number(JInt(index)), elem)
                ArrayIterKeys -> #(elem, st)
              }
            }
          }
          let st =
            set_iter_kind(
              st,
              iter_h,
              ArrayIterator(target:, index: index + 1, kind:),
            )
          iter_yield(st, out)
        }
      }
    }
  }
}

fn require_array_iter(
  st: Agent,
  this: JsVal,
  cont: fn(Agent, Handle, Handle, Int, ArrayIterKind) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: ArrayIterator(target:, index:, kind:), ..) ->
          cont(st, h, target, index, kind)
        _ -> iter_incompatible(st, "Array")
      }
    _ -> iter_incompatible(st, "Array")
  }
}

fn array_source_length(st: Agent, target: Handle) -> #(Int, Agent) {
  case rt_store.t_cell_get(st, target) {
    SObject(kind: ArrayObj(length:), ..) -> #(length, st)
    SObject(kind: ArgumentsObj(length:, ..), ..) -> #(length, st)
    SObject(kind: TypedArrayObj(buffer:, elem_kind:, byte_offset:, length:), ..) ->
      case
        rt_buffer.typed_array_iter_length(
          st,
          buffer,
          elem_kind,
          byte_offset,
          length,
        )
      {
        Ok(len) -> #(len, st)
        Error(err) ->
          throw_type_error(st, rt_buffer.view_witness_error_message(err))
      }
    _ -> {
      let #(len_v, st) =
        rt_obj.t_get_prop(st, mk_object(target), StringKey(nk.length))
      let #(len, st) = rt_val.t_to_length(st, len_v)
      case len > limits.max_iteration {
        True -> {
          let #(e, st) = new_range_error(st, iteration_budget_msg)
          rt_store.t_throw(st, e)
        }
        False -> #(len, st)
      }
    }
  }
}

const iteration_budget_msg = "Array-like length exceeds the maximum supported iteration"

// §24.1.5.2.1
fn map_iterator_next(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: MapIterator(..), ..) as slot ->
          yield_step(iter_protocol.map_iterator_step(st, h, slot))
        _ -> iter_incompatible(st, "Map")
      }
    _ -> iter_incompatible(st, "Map")
  }
}

// §24.2.5.2.1
fn set_iterator_next(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: SetIterator(..), ..) as slot ->
          yield_step(iter_protocol.set_iterator_step(st, h, slot))
        _ -> iter_incompatible(st, "Set")
      }
    _ -> iter_incompatible(st, "Set")
  }
}

// §22.1.5.1.1, index is a utf-8 byte offset
fn string_iterator_next(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: StringIterator(..), ..) as slot ->
          yield_step(iter_protocol.string_iterator_step(st, h, slot))
        _ -> iter_incompatible(st, "String")
      }
    _ -> iter_incompatible(st, "String")
  }
}

fn yield_step(step: #(Option(JsVal), Agent)) -> #(JsVal, Agent) {
  case step {
    #(Some(v), st) -> iter_yield(st, v)
    #(None, st) -> iter_done(st)
  }
}

fn iter_done(st: Agent) -> #(JsVal, Agent) {
  let #(h, st) = rt_async.alloc_iter_result(st, mk_undefined(), True)
  #(mk_object(h), st)
}

fn iter_yield(st: Agent, value: JsVal) -> #(JsVal, Agent) {
  let #(h, st) = rt_async.alloc_iter_result(st, value, False)
  #(mk_object(h), st)
}

fn alloc_pair(st: Agent, a: JsVal, b: JsVal) -> #(JsVal, Agent) {
  let #(h, st) = realm_ops.alloc_array(st, [a, b])
  #(mk_object(h), st)
}

// re-reads the cell so getter mutations survive
fn set_iter_kind(st: Agent, iter_h: Handle, kind: ObjKind) -> Agent {
  rt_store.t_cell_update(st, iter_h, fn(slot) {
    case slot {
      SObject(..) as obj -> SObject(..obj, kind:)
      other -> other
    }
  })
}

fn iter_incompatible(st: Agent, tag: String) -> a {
  throw_type_error(st, tag <> " Iterator next called on incompatible receiver")
}

// §27.1.4.2, any sync throw rejects the promise
fn async_from_sync(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  kind: AfsKind,
) -> #(JsVal, Agent) {
  let #(#(promise_h, resolve_h, reject_h), st) =
    rt_async.t_new_promise_capability(st)
  let cap_resolve = mk_object(resolve_h)
  let cap_reject = mk_object(reject_h)
  let #(outcome, st) =
    protected(st, fn(st) {
      do_async_from_sync(st, this, args, kind, cap_resolve, cap_reject)
    })
  let st = case outcome {
    rt_call.NormalCompletion(_) -> st
    rt_call.ThrowCompletion(e) -> rt_async.t_promise_reject(st, promise_h, e)
  }
  #(mk_object(promise_h), st)
}

fn do_async_from_sync(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  kind: AfsKind,
  cap_resolve: JsVal,
  cap_reject: JsVal,
) -> #(JsVal, Agent) {
  let sync =
    iter_protocol.sync_iterator_record(st, require_async_from_sync(st, this))
  let sync_iter = sync.iterator
  let sync_rec = case classify(sync_iter) {
    KHandle(h) -> h
    _ -> throw_type_error(st, "not an Async-from-Sync Iterator")
  }
  let #(method, st) = case kind {
    AfsNext -> #(sync.next_method, st)
    AfsReturn -> rt_obj.t_get_prop(st, sync_iter, StringKey(nk.return))
    AfsThrow -> rt_obj.t_get_prop(st, sync_iter, StringKey(nk.throw))
  }
  case kind, rt_call.is_callable(st, method) {
    AfsReturn, False -> {
      let arg = first_arg_or_undefined(args)
      let #(ir_h, st) = rt_async.alloc_iter_result(st, arg, True)
      let #(_, st) =
        rt_call.t_call_checked(st, cap_resolve, mk_undefined(), [
          mk_object(ir_h),
        ])
      #(mk_undefined(), st)
    }
    AfsThrow, False -> {
      let st = iter_protocol.iterator_close_normal(st, sync_iter)
      throw_type_error(st, "The iterator does not provide a 'throw' method.")
    }
    _, _ -> {
      let #(result_val, st) =
        rt_call.t_call_checked(st, method, sync_iter, args)
      case classify(result_val) {
        KHandle(result_h) -> {
          let close_on_rejection = case kind {
            AfsReturn -> False
            AfsNext | AfsThrow -> True
          }
          afs_continuation(
            st,
            result_h,
            sync_rec,
            close_on_rejection,
            cap_resolve,
            cap_reject,
          )
        }
        _ -> throw_type_error(st, "Iterator result is not an object")
      }
    }
  }
}

// §27.1.4.4 asyncfromsynciteratorcontinuation
fn afs_continuation(
  st: Agent,
  result_h: Handle,
  sync_rec: Handle,
  close_on_rejection: Bool,
  cap_resolve: JsVal,
  cap_reject: JsVal,
) -> #(JsVal, Agent) {
  let result = mk_object(result_h)
  let #(done_v, st) = rt_obj.t_get_prop(st, result, StringKey(nk.done))
  let done = rt_val.to_boolean(done_v)
  let #(inner, st) = rt_obj.t_get_prop(st, result, StringKey(nk.value))
  let #(on_fulfilled, st) =
    alloc_closure(st, IteratorN(AsyncFromSyncUnwrap(done:)))
  let #(on_rejected, st) = case done || !close_on_rejection {
    True -> #(mk_undefined(), st)
    False ->
      alloc_closure(st, IteratorN(AsyncFromSyncClose(sync_iter: sync_rec)))
  }
  let #(inner_p, st) = rt_async.promise_resolve_static(st, inner)
  let st =
    rt_async.t_perform_then(
      st,
      inner_p,
      on_fulfilled,
      on_rejected,
      cap_resolve,
      cap_reject,
    )
  #(mk_undefined(), st)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(rt_call.Completion, Agent)

fn alloc_closure(st: Agent, tag: NativeToken) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_call.t_native_new(
      st,
      Some(st.realm.function.prototype),
      tag,
      "",
      1,
      False,
    )
  #(mk_object(h), st)
}

fn require_async_from_sync(st: Agent, this: JsVal) -> Handle {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: AsyncFromSyncIterator(sync_rec:), ..) -> sync_rec
        _ -> throw_type_error(st, "not an Async-from-Sync Iterator")
      }
    _ -> throw_type_error(st, "not an Async-from-Sync Iterator")
  }
}

fn throw_type_error(st: Agent, msg: String) -> a {
  let js = st.store
  let #(e, st) = js.ops.new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}

fn new_type_error(st: Agent, msg: String) -> #(JsVal, Agent) {
  let js = st.store
  js.ops.new_error(st, TypeErr, msg)
}

fn new_range_error(st: Agent, msg: String) -> #(JsVal, Agent) {
  let js = st.store
  js.ops.new_error(st, RangeErr, msg)
}

// must match rt_call.Completion erlang tags
type ProtOut(a) {
  NormalCompletion(a)
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected_any(
  st: Agent,
  body: fn(Agent) -> #(a, Agent),
) -> #(ProtOut(a), Agent)

pub fn dispatch_construct(
  st: Agent,
  n: IteratorNative,
  _args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case n {
    IteratorConstructor -> {
      let self = mk_object(st.realm.iterator.constructor)
      case rt_val.is_undef(new_target) || same_handle(new_target, self) {
        True ->
          throw_type_error(
            st,
            "Abstract class Iterator not directly constructable",
          )
        False -> {
          let #(proto, st) =
            rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
              r.iterator.prototype
            })
          rt_store.t_cell_new(
            st,
            SObject(
              kind: Ordinary,
              proto: Some(proto),
              props: dict.new(),
              symbol_props: [],
              elements: NoElements,
              extensible: True,
            ),
          )
        }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

fn same_handle(a: JsVal, b: JsVal) -> Bool {
  case classify(a), classify(b) {
    KHandle(ha), KHandle(hb) -> ha.id == hb.id
    _, _ -> False
  }
}

// §27.1.2.1
fn from(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let o = first_arg_or_undefined(args)
  let #(rec, st) =
    iter_protocol.get_iterator_flattenable(
      st,
      o,
      IterateStrings,
      "Iterator.from argument",
    )
  let ctor = st.realm.iterator.constructor
  let #(is_iter, st) = rt_ops.t_ordinary_has_instance(st, ctor, rec.iterator)
  case is_iter != 0 {
    True -> #(rec.iterator, st)
    False -> {
      let #(h, st) =
        realm_ops.alloc_wrapper(
          st,
          WrapForValidIteratorObj(record: rec),
          st.realm.wrap_for_valid_proto,
        )
      #(mk_object(h), st)
    }
  }
}

fn lazy_helper(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  make_kind: fn(JsVal) -> IteratorHelperKind,
  name: String,
) -> #(JsVal, Agent) {
  use rec, func, st <- consumer_with_callback(st, this, args, name)
  alloc_helper(st, make_kind(func), rec)
}

fn take_or_drop(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  make_kind: fn(Int) -> IteratorHelperKind,
  name: String,
) -> #(JsVal, Agent) {
  use _h <- require_object_of(
    st,
    this,
    "Iterator.prototype." <> name <> " called on non-object",
  )
  // §27.1.4.10 tonumber(limit) before getiteratordirect
  let #(remaining, st) = coerce_limit(st, this, args, name)
  let #(rec, st) = get_iterator_direct_for(st, this, name)
  alloc_helper(st, make_kind(remaining), rec)
}

fn coerce_limit(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
) -> #(Int, Agent) {
  let arg = first_arg_or_undefined(args)
  let #(nout, st) = protected_any(st, fn(st) { rt_val.t_to_number(st, arg) })
  let range_error = fn(st, problem) {
    let #(e, st) = new_range_error(st, name <> " limit is " <> problem)
    iter_protocol.close_throw(st, this, e)
  }
  case nout {
    ThrowCompletion(thrown) -> iter_protocol.close_throw(st, this, thrown)
    NormalCompletion(n) ->
      case n {
        JNan -> range_error(st, "NaN")
        JPosInf -> #(limits.max_safe_integer, st)
        JNegInf -> range_error(st, "negative")
        JInt(i) if i > limits.max_safe_integer -> range_error(st, "too large")
        JInt(i) if i < 0 -> range_error(st, "negative")
        JInt(i) -> #(i, st)
        JFloat(f) if f >. 9_007_199_254_740_991.0 -> range_error(st, "too large")
        JFloat(f) ->
          case rt_val.float_to_int(f) {
            i if i < 0 -> range_error(st, "negative")
            i -> #(i, st)
          }
      }
  }
}

fn alloc_helper(
  st: Agent,
  kind: IteratorHelperKind,
  underlying: IteratorRecord,
) -> #(JsVal, Agent) {
  alloc_helper_body(st, ClassicHelper(kind:, underlying:, counter: 0))
}

fn alloc_helper_body(st: Agent, body: HelperBody) -> #(JsVal, Agent) {
  let #(h, st) =
    realm_ops.alloc_wrapper(
      st,
      IteratorHelperObj(gen_state: GenSuspendedStart, body:),
      st.realm.iterator_helper_proto,
    )
  #(mk_object(h), st)
}

const helper_receiver_err = "Iterator Helper method called on incompatible receiver"

const helper_running_err = "Iterator Helper is currently being iterated"

// §27.1.4.1
fn helper_next(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use ref, gen_state, body <- require_helper(st, this)
  use st <- resume(st, ref, gen_state)
  case body {
    ClassicHelper(kind:, underlying:, counter:) ->
      classic_helper_next(st, ref, kind, underlying, counter)
    ZipHelper(members:, mode:, keys:) -> zip_next(st, ref, members, mode, keys)
    ConcatHelper(remaining:, inner:) -> concat_next(st, ref, remaining, inner)
  }
}

fn require_helper(
  st: Agent,
  this: JsVal,
  cont: fn(Handle, GeneratorState, HelperBody) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: IteratorHelperObj(gen_state:, body:), ..) ->
          cont(h, gen_state, body)
        _ -> throw_type_error(st, helper_receiver_err)
      }
    _ -> throw_type_error(st, helper_receiver_err)
  }
}

// §27.5.3.3 generatorresume
fn resume(
  st: Agent,
  ref: Handle,
  gen_state: GeneratorState,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case gen_state {
    GenExecuting -> throw_type_error(st, helper_running_err)
    GenCompleted -> iter_done(st)
    GenSuspendedStart | GenSuspendedYield -> {
      let st = set_gen_state(st, ref, GenExecuting)
      let #(out, st) = protected_any(st, body)
      let st = map_gen_state(st, ref, suspend_if_executing)
      case out {
        NormalCompletion(v) -> #(v, st)
        ThrowCompletion(e) -> rt_store.t_throw(st, e)
      }
    }
  }
}

// §27.5.3.4 generatorresumeabrupt
fn resume_abrupt(
  st: Agent,
  ref: Handle,
  gen_state: GeneratorState,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case gen_state {
    GenExecuting -> throw_type_error(st, helper_running_err)
    GenCompleted -> iter_done(st)
    GenSuspendedStart -> body(set_gen_state(st, ref, GenCompleted))
    GenSuspendedYield -> body(set_gen_state(st, ref, GenExecuting))
  }
}

fn suspend_if_executing(gs: GeneratorState) -> GeneratorState {
  case gs {
    GenExecuting -> GenSuspendedYield
    GenSuspendedStart | GenSuspendedYield | GenCompleted -> gs
  }
}

fn set_gen_state(st: Agent, ref: Handle, gs: GeneratorState) -> Agent {
  map_gen_state(st, ref, fn(_prev) { gs })
}

fn map_gen_state(
  st: Agent,
  ref: Handle,
  update: fn(GeneratorState) -> GeneratorState,
) -> Agent {
  rt_store.t_cell_update(st, ref, fn(slot) {
    case slot {
      SObject(kind: IteratorHelperObj(gen_state:, ..) as k, ..) ->
        SObject(
          ..slot,
          kind: IteratorHelperObj(..k, gen_state: update(gen_state)),
        )
      other -> other
    }
  })
}

fn classic_helper_next(
  st: Agent,
  ref: Handle,
  kind: IteratorHelperKind,
  underlying: IteratorRecord,
  counter: Int,
) -> #(JsVal, Agent) {
  case kind {
    HelperMap(func:) -> step_map(st, ref, underlying, func, counter)
    HelperFilter(func:) -> step_filter(st, ref, underlying, func, counter)
    HelperTake(remaining:) -> step_take(st, ref, underlying, remaining)
    HelperDrop(remaining:) -> step_drop(st, ref, underlying, remaining)
    HelperFlatMap(func:, inner:) ->
      step_flat_map(st, ref, underlying, func, inner, counter)
  }
}

// §27.1.4.2
fn helper_return(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use ref, gen_state, body <- require_helper(st, this)
  use st <- resume_abrupt(st, ref, gen_state)
  case body {
    ClassicHelper(kind:, underlying:, counter: _) ->
      classic_helper_return(st, ref, kind, underlying)
    ZipHelper(members:, mode: _, keys: _) -> zip_return(st, ref, members)
    ConcatHelper(remaining: _, inner:) -> concat_return(st, ref, inner)
  }
}

fn classic_helper_return(
  st: Agent,
  ref: Handle,
  kind: IteratorHelperKind,
  underlying: IteratorRecord,
) -> #(JsVal, Agent) {
  let #(inner_res, st) = case kind {
    HelperFlatMap(inner: Some(inner), func: _) ->
      close_normal_catch(st, inner.iterator)
    HelperFlatMap(inner: None, func: _)
    | HelperMap(func: _)
    | HelperFilter(func: _)
    | HelperTake(remaining: _)
    | HelperDrop(remaining: _) -> #(Ok(Nil), st)
  }
  let #(outer_res, st) = close_normal_catch(st, underlying.iterator)
  let st = mark_done(st, ref)
  case inner_res, outer_res {
    Error(e), _ -> rt_store.t_throw(st, e)
    _, Error(e) -> rt_store.t_throw(st, e)
    Ok(Nil), Ok(Nil) -> iter_done(st)
  }
}

fn step_map(
  st: Agent,
  ref: Handle,
  underlying: IteratorRecord,
  func: JsVal,
  count: Int,
) -> #(JsVal, Agent) {
  use step, st <- after_step(st, ref, underlying)
  case step {
    None -> finish(st, ref)
    Some(v) -> {
      let st = write_counter(st, ref, count + 1)
      let idx = mk_number(rt_val.num_from_int(count))
      case rt_call.t_call(st, func, mk_undefined(), [v, idx]) {
        #(rt_call.NormalCompletion(mapped), st) -> iter_yield(st, mapped)
        #(rt_call.ThrowCompletion(thrown), st) ->
          close_throw_done(st, ref, underlying, thrown)
      }
    }
  }
}

fn step_filter(
  st: Agent,
  ref: Handle,
  underlying: IteratorRecord,
  func: JsVal,
  count: Int,
) -> #(JsVal, Agent) {
  use step, st <- after_step(st, ref, underlying)
  case step {
    None -> finish(st, ref)
    Some(v) -> {
      let st = write_counter(st, ref, count + 1)
      let idx = mk_number(rt_val.num_from_int(count))
      case rt_call.t_call(st, func, mk_undefined(), [v, idx]) {
        #(rt_call.ThrowCompletion(thrown), st) ->
          close_throw_done(st, ref, underlying, thrown)
        #(rt_call.NormalCompletion(selected), st) ->
          case rt_val.to_boolean(selected) {
            True -> iter_yield(st, v)
            False -> step_filter(st, ref, underlying, func, count + 1)
          }
      }
    }
  }
}

fn step_take(
  st: Agent,
  ref: Handle,
  underlying: IteratorRecord,
  remaining: Int,
) -> #(JsVal, Agent) {
  case remaining <= 0 {
    True -> {
      let #(close_res, st) = close_normal_catch(st, underlying.iterator)
      finish_after_close(st, ref, close_res)
    }
    False -> {
      use step, st <- after_step(st, ref, underlying)
      case step {
        None -> finish(st, ref)
        Some(v) -> {
          let st = write_kind(st, ref, HelperTake(remaining - 1))
          iter_yield(st, v)
        }
      }
    }
  }
}

fn step_drop(
  st: Agent,
  ref: Handle,
  underlying: IteratorRecord,
  remaining: Int,
) -> #(JsVal, Agent) {
  use step, st <- after_step(st, ref, underlying)
  case step {
    None -> finish(st, ref)
    Some(v) ->
      case remaining > 0 {
        True -> {
          let st = write_kind(st, ref, HelperDrop(remaining - 1))
          step_drop(st, ref, underlying, remaining - 1)
        }
        False -> iter_yield(st, v)
      }
  }
}

fn step_flat_map(
  st: Agent,
  ref: Handle,
  underlying: IteratorRecord,
  func: JsVal,
  inner: Option(IteratorRecord),
  count: Int,
) -> #(JsVal, Agent) {
  case inner {
    Some(inner_rec) -> {
      let #(step, st) =
        protected_any(st, fn(st) {
          iter_protocol.iterator_step_value(st, inner_rec)
        })
      case step {
        ThrowCompletion(thrown) -> close_throw_done(st, ref, underlying, thrown)
        NormalCompletion(Some(v)) -> iter_yield(st, v)
        NormalCompletion(None) -> {
          let st = write_kind(st, ref, HelperFlatMap(func:, inner: None))
          step_flat_map(st, ref, underlying, func, None, count)
        }
      }
    }
    None -> {
      use step, st <- after_step(st, ref, underlying)
      case step {
        None -> finish(st, ref)
        Some(v) -> {
          let idx = mk_number(rt_val.num_from_int(count))
          let st = write_counter(st, ref, count + 1)
          case rt_call.t_call(st, func, mk_undefined(), [v, idx]) {
            #(rt_call.ThrowCompletion(thrown), st) ->
              close_throw_done(st, ref, underlying, thrown)
            #(rt_call.NormalCompletion(mapped), st) -> {
              let #(open, st) =
                protected_any(st, fn(st) {
                  iter_protocol.get_iterator_flattenable(
                    st,
                    mapped,
                    RejectPrimitives,
                    "flatMap callback result",
                  )
                })
              case open {
                ThrowCompletion(thrown) ->
                  close_throw_done(st, ref, underlying, thrown)
                NormalCompletion(new_inner) -> {
                  let st =
                    write_kind(
                      st,
                      ref,
                      HelperFlatMap(func:, inner: Some(new_inner)),
                    )
                  step_flat_map(
                    st,
                    ref,
                    underlying,
                    func,
                    Some(new_inner),
                    count + 1,
                  )
                }
              }
            }
          }
        }
      }
    }
  }
}

fn wrap_next(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use rec <- require_wrap(st, this)
  rt_call.t_call_checked(st, rec.next_method, rec.iterator, [])
}

fn wrap_return(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use rec <- require_wrap(st, this)
  case iter_protocol.call_return(st, rec.iterator) {
    #(Ok(iter_protocol.NoReturnMethod), st) -> iter_done(st)
    #(Ok(iter_protocol.Returned(result)), st) -> #(result, st)
    #(Error(thrown), st) -> rt_store.t_throw(st, thrown)
  }
}

fn require_wrap(
  st: Agent,
  this: JsVal,
  cont: fn(IteratorRecord) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let err = "WrapForValidIterator method called on incompatible receiver"
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: WrapForValidIteratorObj(record:), ..) -> cont(record)
        _ -> throw_type_error(st, err)
      }
    _ -> throw_type_error(st, err)
  }
}

fn to_array(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(rec, st) = get_iterator_direct_for(st, this, "toArray")
  let #(values, st) = iter_protocol.iterator_to_list(st, rec)
  let #(h, st) = realm_ops.alloc_array(st, values)
  #(mk_object(h), st)
}

fn for_each(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use rec, func, st <- consumer_with_callback(st, this, args, "forEach")
  for_each_loop(st, rec, func, 0)
}

fn for_each_loop(
  st: Agent,
  rec: IteratorRecord,
  func: JsVal,
  counter: Int,
) -> #(JsVal, Agent) {
  case iter_protocol.iterator_step_value(st, rec) {
    #(None, st) -> #(mk_undefined(), st)
    #(Some(v), st) -> {
      let idx = mk_number(rt_val.num_from_int(counter))
      case rt_call.t_call(st, func, mk_undefined(), [v, idx]) {
        #(rt_call.ThrowCompletion(thrown), st) ->
          iter_protocol.close_throw(st, rec.iterator, thrown)
        #(rt_call.NormalCompletion(_result), st) ->
          for_each_loop(st, rec, func, counter + 1)
      }
    }
  }
}

fn reduce(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use rec, func, st <- consumer_with_callback(st, this, args, "reduce")
  case args {
    [_, initial, ..] -> reduce_loop(st, rec, func, initial, 0)
    _ ->
      case iter_protocol.iterator_step_value(st, rec) {
        #(None, st) ->
          throw_type_error(st, "Reduce of empty iterator with no initial value")
        #(Some(seed), st) -> reduce_loop(st, rec, func, seed, 1)
      }
  }
}

fn reduce_loop(
  st: Agent,
  rec: IteratorRecord,
  func: JsVal,
  acc: JsVal,
  counter: Int,
) -> #(JsVal, Agent) {
  case iter_protocol.iterator_step_value(st, rec) {
    #(None, st) -> #(acc, st)
    #(Some(v), st) -> {
      let idx = mk_number(rt_val.num_from_int(counter))
      case rt_call.t_call(st, func, mk_undefined(), [acc, v, idx]) {
        #(rt_call.ThrowCompletion(thrown), st) ->
          iter_protocol.close_throw(st, rec.iterator, thrown)
        #(rt_call.NormalCompletion(new_acc), st) ->
          reduce_loop(st, rec, func, new_acc, counter + 1)
      }
    }
  }
}

// some: match_on true, every: false
fn bool_consumer(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  match_on: Bool,
  name: String,
) -> #(JsVal, Agent) {
  use rec, func, st <- consumer_with_callback(st, this, args, name)
  let #(matched, st) = predicate_loop(st, rec, func, 0, match_on)
  #(mk_bool(option.is_some(matched) == match_on), st)
}

fn predicate_loop(
  st: Agent,
  rec: IteratorRecord,
  func: JsVal,
  counter: Int,
  match_on: Bool,
) -> #(Option(JsVal), Agent) {
  case iter_protocol.iterator_step_value(st, rec) {
    #(None, st) -> #(None, st)
    #(Some(v), st) -> {
      let idx = mk_number(rt_val.num_from_int(counter))
      case rt_call.t_call(st, func, mk_undefined(), [v, idx]) {
        #(rt_call.ThrowCompletion(thrown), st) ->
          iter_protocol.close_throw(st, rec.iterator, thrown)
        #(rt_call.NormalCompletion(result), st) ->
          case rt_val.to_boolean(result) == match_on {
            True -> {
              let st = iter_protocol.iterator_close_normal(st, rec.iterator)
              #(Some(v), st)
            }
            False -> predicate_loop(st, rec, func, counter + 1, match_on)
          }
      }
    }
  }
}

fn find(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use rec, func, st <- consumer_with_callback(st, this, args, "find")
  let #(matched, st) = predicate_loop(st, rec, func, 0, True)
  #(option.unwrap(matched, mk_undefined()), st)
}

type IgnoreSetterKey {
  IgnoreSetCtor
  IgnoreSetTag
}

// §27.1.3.2 setterthatignoresprototypeproperties
fn ignore_proto_setter(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  which: IgnoreSetterKey,
) -> #(JsVal, Agent) {
  let proto = st.realm.iterator.prototype
  case classify(this) {
    KHandle(h) ->
      case h.id == proto.id {
        True ->
          throw_type_error(
            st,
            "Cannot assign to read only property of Iterator.prototype",
          )
        False -> {
          let val = first_arg_or_undefined(args)
          let key = case which {
            IgnoreSetCtor -> StringKey(nk.constructor)
            IgnoreSetTag -> SymbolKey(symbol_to_string_tag)
          }
          let #(ok, st) =
            rt_obj.t_define_own_data(st, h, key, val, True, True, True)
          case ok {
            True -> #(mk_undefined(), st)
            False ->
              throw_type_error(st, "Cannot define property on this receiver")
          }
        }
      }
    _ ->
      throw_type_error(
        st,
        "Cannot set property on non-object Iterator receiver",
      )
  }
}

// §7.4.9 getiteratordirect
fn get_iterator_direct_for(
  st: Agent,
  this: JsVal,
  name: String,
) -> #(IteratorRecord, Agent) {
  iter_protocol.get_iterator_direct(
    st,
    this,
    "Iterator.prototype." <> name <> " called on non-object",
  )
}

// callback checked before reading .next, §27.1.4.5 step 3
fn consumer_with_callback(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
  cont: fn(IteratorRecord, JsVal, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use _h <- require_object_of(
    st,
    this,
    "Iterator.prototype." <> name <> " called on non-object",
  )
  let func = first_arg_or_undefined(args)
  case rt_call.is_callable(st, func) {
    False ->
      iter_protocol.close_throw_type(
        st,
        this,
        name <> " argument is not callable",
      )
    True -> {
      let #(rec, st) = get_iterator_direct_for(st, this, name)
      cont(rec, func, st)
    }
  }
}

fn require_object_of(
  st: Agent,
  this: JsVal,
  msg: String,
  cont: fn(Handle) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) -> cont(h)
    _ -> throw_type_error(st, msg)
  }
}

// next() threw: iterator already broken, no close
fn after_step(
  st: Agent,
  ref: Handle,
  rec: IteratorRecord,
  cont: fn(Option(JsVal), Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(step, st) =
    protected_any(st, fn(st) { iter_protocol.iterator_step_value(st, rec) })
  case step {
    NormalCompletion(v) -> cont(v, st)
    ThrowCompletion(thrown) -> rt_store.t_throw(mark_done(st, ref), thrown)
  }
}

fn finish(st: Agent, ref: Handle) -> #(JsVal, Agent) {
  iter_done(mark_done(st, ref))
}

fn close_throw_done(
  st: Agent,
  ref: Handle,
  underlying: IteratorRecord,
  thrown: JsVal,
) -> a {
  let #(original, st) =
    iter_protocol.close_and_throw(st, underlying.iterator, thrown)
  rt_store.t_throw(mark_done(st, ref), original)
}

fn close_normal_catch(st: Agent, iter: JsVal) -> #(Result(Nil, JsVal), Agent) {
  let #(out, st) =
    protected_any(st, fn(st) {
      #(Nil, iter_protocol.iterator_close_normal(st, iter))
    })
  case out {
    NormalCompletion(Nil) -> #(Ok(Nil), st)
    ThrowCompletion(e) -> #(Error(e), st)
  }
}

fn mark_done(st: Agent, ref: Handle) -> Agent {
  set_gen_state(st, ref, GenCompleted)
}

fn write_counter(st: Agent, ref: Handle, counter: Int) -> Agent {
  use kind, _counter <- update_helper(st, ref)
  #(kind, counter)
}

fn write_kind(st: Agent, ref: Handle, kind: IteratorHelperKind) -> Agent {
  use _kind, counter <- update_helper(st, ref)
  #(kind, counter)
}

fn map_helper_body(
  st: Agent,
  ref: Handle,
  update: fn(HelperBody) -> HelperBody,
) -> Agent {
  rt_store.t_cell_update(st, ref, fn(slot) {
    case slot {
      SObject(kind: IteratorHelperObj(body:, ..) as helper, ..) ->
        SObject(..slot, kind: IteratorHelperObj(..helper, body: update(body)))
      other -> other
    }
  })
}

fn update_helper(
  st: Agent,
  ref: Handle,
  update: fn(IteratorHelperKind, Int) -> #(IteratorHelperKind, Int),
) -> Agent {
  use body <- map_helper_body(st, ref)
  case body {
    ClassicHelper(kind:, underlying:, counter:) -> {
      let #(kind, counter) = update(kind, counter)
      ClassicHelper(kind:, underlying:, counter:)
    }
    ZipHelper(..) | ConcatHelper(..) -> body
  }
}

fn finish_after_close(
  st: Agent,
  ref: Handle,
  close_res: Result(Nil, JsVal),
) -> #(JsVal, Agent) {
  let st = mark_done(st, ref)
  case close_res {
    Error(e) -> rt_store.t_throw(st, e)
    Ok(Nil) -> iter_done(st)
  }
}

type ZipModeOption {
  OptShortest
  OptStrict
  OptLongest(padding: JsVal)
}

fn zip_mode(opt: ZipModeOption) -> ZipMode {
  case opt {
    OptShortest -> ZipShortest
    OptStrict -> ZipStrict
    OptLongest(padding: _) -> ZipLongest
  }
}

fn zip(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let iterables = first_arg_or_undefined(args)
  use _h <- require_object_of(
    st,
    iterables,
    "Iterator.zip iterables argument is not an object",
  )
  let #(mode, st) = zip_options(st, args, "zip")
  let #(input_rec, st) = iter_protocol.get_iterator_sync(st, iterables)
  let #(iters, st) = zip_collect(st, input_rec, [])
  let #(padding, st) = case mode {
    OptLongest(padding: opt) -> zip_padding_iterated(st, opt, iters)
    OptShortest | OptStrict -> #(unread_padding(iters), st)
  }
  alloc_zip(st, iters, zip_mode(mode), padding, None)
}

fn unread_padding(iters: List(IteratorRecord)) -> List(JsVal) {
  list.map(iters, fn(_iter) { mk_undefined() })
}

fn zip_keyed(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let iterables = first_arg_or_undefined(args)
  use iterables_h <- require_object_of(
    st,
    iterables,
    "Iterator.zipKeyed iterables argument is not an object",
  )
  let #(mode, st) = zip_options(st, args, "zipKeyed")
  let #(all_keys, st) = rt_obj.t_own_keys(st, iterables_h)
  let #(#(keys, iters), st) =
    zip_keyed_collect(st, iterables, iterables_h, all_keys, [], [])
  let #(padding, st) = case mode {
    OptLongest(padding: opt) -> zip_keyed_padding(st, opt, keys, iters)
    OptShortest | OptStrict -> #(unread_padding(iters), st)
  }
  alloc_zip(st, iters, zip_mode(mode), padding, Some(keys))
}

fn zip_options(
  st: Agent,
  args: List(JsVal),
  name: String,
) -> #(ZipModeOption, Agent) {
  let options = arg_at(args, 1)
  case classify(options) {
    KUndef -> #(OptShortest, st)
    KHandle(_) -> {
      let #(mode_v, st) = rt_obj.t_get_prop(st, options, StringKey(nk.mode))
      case classify(mode_v) {
        KUndef -> #(OptShortest, st)
        KStr("shortest") -> #(OptShortest, st)
        KStr("strict") -> #(OptStrict, st)
        KStr("longest") -> {
          let #(pad, st) = rt_obj.t_get_prop(st, options, StringKey(nk.padding))
          case classify(pad) {
            KUndef | KHandle(_) -> #(OptLongest(padding: pad), st)
            _ ->
              throw_type_error(
                st,
                "Iterator." <> name <> " padding is not an object",
              )
          }
        }
        _ ->
          throw_type_error(
            st,
            "Iterator."
              <> name
              <> " mode must be \"shortest\", \"longest\", or \"strict\"",
          )
      }
    }
    _ ->
      throw_type_error(st, "Iterator." <> name <> " options is not an object")
  }
}

fn zip_collect(
  st: Agent,
  input_rec: IteratorRecord,
  acc: List(IteratorRecord),
) -> #(List(IteratorRecord), Agent) {
  let #(step, st) =
    protected_any(st, fn(st) {
      iter_protocol.iterator_step_value(st, input_rec)
    })
  case step {
    ThrowCompletion(thrown) -> close_all_throw(st, collected_iters(acc), thrown)
    NormalCompletion(None) -> #(list.reverse(acc), st)
    NormalCompletion(Some(v)) -> {
      use rec, st <- or_close_all(
        st,
        fn() { [input_rec.iterator, ..collected_iters(acc)] },
        fn(st) {
          iter_protocol.get_iterator_flattenable(
            st,
            v,
            RejectPrimitives,
            "Iterator.zip input",
          )
        },
      )
      zip_collect(st, input_rec, [rec, ..acc])
    }
  }
}

fn collected_iters(acc: List(IteratorRecord)) -> List(JsVal) {
  list.reverse(acc) |> list.map(fn(rec) { rec.iterator })
}

fn zip_padding_iterated(
  st: Agent,
  padding_option: JsVal,
  iters: List(IteratorRecord),
) -> #(List(JsVal), Agent) {
  let iter_count = list.length(iters)
  case classify(padding_option) {
    KUndef -> #(list.repeat(mk_undefined(), iter_count), st)
    _ -> {
      let opened = list.map(iters, fn(rec) { rec.iterator })
      use pad_rec, st <- or_close_all(st, fn() { opened }, fn(st) {
        iter_protocol.get_iterator_sync(st, padding_option)
      })
      zip_padding_loop(st, pad_rec, opened, iter_count, [])
    }
  }
}

fn zip_padding_loop(
  st: Agent,
  pad_rec: IteratorRecord,
  opened: List(JsVal),
  remaining: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case remaining <= 0 {
    True -> {
      let #(close_res, st) = close_normal_catch(st, pad_rec.iterator)
      case close_res {
        Error(thrown) -> close_all_throw(st, opened, thrown)
        Ok(Nil) -> #(list.reverse(acc), st)
      }
    }
    False -> {
      let #(step, st) =
        protected_any(st, fn(st) {
          iter_protocol.iterator_step_value(st, pad_rec)
        })
      case step {
        ThrowCompletion(thrown) -> close_all_throw(st, opened, thrown)
        NormalCompletion(None) -> #(
          list.append(list.reverse(acc), list.repeat(mk_undefined(), remaining)),
          st,
        )
        NormalCompletion(Some(v)) ->
          zip_padding_loop(st, pad_rec, opened, remaining - 1, [v, ..acc])
      }
    }
  }
}

fn zip_keyed_collect(
  st: Agent,
  iterables: JsVal,
  iterables_h: Handle,
  keys_left: List(ObjectKey),
  keys_acc: List(ObjectKey),
  iters_acc: List(IteratorRecord),
) -> #(#(List(ObjectKey), List(IteratorRecord)), Agent) {
  case keys_left {
    [] -> #(#(list.reverse(keys_acc), list.reverse(iters_acc)), st)
    [key, ..rest] -> {
      let opened = fn() { collected_iters(iters_acc) }
      use desc, st <- or_close_all(st, opened, fn(st) {
        rt_obj.t_get_own_property(st, iterables_h, key)
      })
      let enumerable = case desc {
        Some(prop) -> rt_types.prop_enumerable(prop)
        None -> False
      }
      case enumerable {
        False ->
          zip_keyed_collect(
            st,
            iterables,
            iterables_h,
            rest,
            keys_acc,
            iters_acc,
          )
        True -> {
          use v, st <- or_close_all(st, opened, fn(st) {
            rt_obj.t_get_prop(st, iterables, key)
          })
          case classify(v) {
            KUndef ->
              zip_keyed_collect(
                st,
                iterables,
                iterables_h,
                rest,
                keys_acc,
                iters_acc,
              )
            _ -> {
              use rec, st <- or_close_all(st, opened, fn(st) {
                iter_protocol.get_iterator_flattenable(
                  st,
                  v,
                  RejectPrimitives,
                  "Iterator.zipKeyed input",
                )
              })
              zip_keyed_collect(
                st,
                iterables,
                iterables_h,
                rest,
                [key, ..keys_acc],
                [rec, ..iters_acc],
              )
            }
          }
        }
      }
    }
  }
}

fn zip_keyed_padding(
  st: Agent,
  padding_option: JsVal,
  keys: List(ObjectKey),
  iters: List(IteratorRecord),
) -> #(List(JsVal), Agent) {
  case classify(padding_option) {
    KUndef -> #(list.repeat(mk_undefined(), list.length(iters)), st)
    _ -> {
      let opened = list.map(iters, fn(rec) { rec.iterator })
      zip_keyed_padding_loop(st, padding_option, opened, keys, [])
    }
  }
}

fn zip_keyed_padding_loop(
  st: Agent,
  padding_option: JsVal,
  opened: List(JsVal),
  keys_left: List(ObjectKey),
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case keys_left {
    [] -> #(list.reverse(acc), st)
    [key, ..rest] -> {
      use v, st <- or_close_all(st, fn() { opened }, fn(st) {
        rt_obj.t_get_prop(st, padding_option, key)
      })
      zip_keyed_padding_loop(st, padding_option, opened, rest, [v, ..acc])
    }
  }
}

fn alloc_zip(
  st: Agent,
  iters: List(IteratorRecord),
  mode: ZipMode,
  padding: List(JsVal),
  keys: Option(List(ObjectKey)),
) -> #(JsVal, Agent) {
  let assert Ok(paired) = list.strict_zip(iters, padding)
    as "Iterator.zip padding must have one entry per iterator"
  let members =
    list.map(paired, fn(pair) {
      let #(record, pad) = pair
      ZipOpen(record:, padding: pad)
    })
  alloc_helper_body(st, ZipHelper(members:, mode:, keys:))
}

fn zip_next(
  st: Agent,
  ref: Handle,
  members: List(ZipMember),
  mode: ZipMode,
  keys: Option(List(ObjectKey)),
) -> #(JsVal, Agent) {
  case members {
    [] -> finish(st, ref)
    _ -> zip_round(st, ref, mode, keys, [], members, [])
  }
}

fn zip_round(
  st: Agent,
  ref: Handle,
  mode: ZipMode,
  keys: Option(List(ObjectKey)),
  prev: List(ZipMember),
  rest: List(ZipMember),
  results: List(JsVal),
) -> #(JsVal, Agent) {
  case rest {
    [] -> zip_emit(st, ref, keys, list.reverse(prev), list.reverse(results))
    [member, ..tail] ->
      case member {
        ZipExhausted(padding:) ->
          zip_round(st, ref, mode, keys, [member, ..prev], tail, [
            padding,
            ..results
          ])
        ZipOpen(record:, padding:) -> {
          let #(step, st) =
            protected_any(st, fn(st) {
              iter_protocol.iterator_step_value(st, record)
            })
          case step {
            ThrowCompletion(thrown) ->
              close_all_throw_done(st, ref, open_others(prev, tail), thrown)
            NormalCompletion(Some(v)) ->
              zip_round(st, ref, mode, keys, [member, ..prev], tail, [
                v,
                ..results
              ])
            NormalCompletion(None) ->
              case mode {
                ZipShortest -> {
                  let #(close_res, st) =
                    close_all_normal(st, open_others(prev, tail))
                  finish_after_close(st, ref, close_res)
                }
                ZipStrict ->
                  case prev {
                    [] -> zip_strict_check(st, ref, tail)
                    _ -> zip_strict_throw(st, ref, open_others(prev, tail))
                  }
                ZipLongest ->
                  case open_others(prev, tail) {
                    [] -> finish(st, ref)
                    _ ->
                      zip_round(
                        st,
                        ref,
                        mode,
                        keys,
                        [ZipExhausted(padding:), ..prev],
                        tail,
                        [padding, ..results],
                      )
                  }
              }
          }
        }
      }
  }
}

fn zip_strict_check(
  st: Agent,
  ref: Handle,
  rest: List(ZipMember),
) -> #(JsVal, Agent) {
  case rest {
    [] -> finish(st, ref)
    [ZipExhausted(padding: _), ..tail] -> zip_strict_check(st, ref, tail)
    [ZipOpen(record:, padding: _), ..tail] -> {
      let #(step, st) =
        protected_any(st, fn(st) {
          iter_protocol.iterator_step_done(st, record)
        })
      case step {
        ThrowCompletion(thrown) ->
          close_all_throw_done(st, ref, open_members(tail), thrown)
        NormalCompletion(True) -> zip_strict_check(st, ref, tail)
        NormalCompletion(False) ->
          zip_strict_throw(st, ref, [record.iterator, ..open_members(tail)])
      }
    }
  }
}

fn zip_strict_throw(
  st: Agent,
  ref: Handle,
  open: List(JsVal),
) -> #(JsVal, Agent) {
  let #(terr, st) =
    new_type_error(
      st,
      "Iterator.zip strict mode: iterators have different lengths",
    )
  close_all_throw_done(st, ref, open, terr)
}

fn zip_emit(
  st: Agent,
  ref: Handle,
  keys: Option(List(ObjectKey)),
  members: List(ZipMember),
  results: List(JsVal),
) -> #(JsVal, Agent) {
  let st = zip_write_members(st, ref, members)
  case keys {
    None -> {
      let #(arr, st) = realm_ops.alloc_array(st, results)
      iter_yield(st, mk_object(arr))
    }
    Some(ks) -> {
      let #(obj, st) = alloc_zip_keyed_result(st, ks, results)
      iter_yield(st, mk_object(obj))
    }
  }
}

fn alloc_zip_keyed_result(
  st: Agent,
  keys: List(ObjectKey),
  results: List(JsVal),
) -> #(Handle, Agent) {
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
    list.zip(keys, results)
    |> list.fold(st, fn(st, pair) {
      let #(key, v) = pair
      let #(_ok, st) = rt_obj.t_define_own_data(st, h, key, v, True, True, True)
      st
    })
  #(h, st)
}

fn zip_return(
  st: Agent,
  ref: Handle,
  members: List(ZipMember),
) -> #(JsVal, Agent) {
  let #(close_res, st) = close_all_normal(st, open_members(members))
  finish_after_close(st, ref, close_res)
}

fn open_others(prev: List(ZipMember), tail: List(ZipMember)) -> List(JsVal) {
  list.append(open_members(list.reverse(prev)), open_members(tail))
}

fn open_members(members: List(ZipMember)) -> List(JsVal) {
  list.filter_map(members, fn(m) {
    case m {
      ZipOpen(record:, ..) -> Ok(record.iterator)
      ZipExhausted(padding: _) -> Error(Nil)
    }
  })
}

fn or_close_all(
  st: Agent,
  iters: fn() -> List(JsVal),
  body: fn(Agent) -> #(a, Agent),
  cont: fn(a, Agent) -> #(b, Agent),
) -> #(b, Agent) {
  case protected_any(st, body) {
    #(NormalCompletion(v), st) -> cont(v, st)
    #(ThrowCompletion(thrown), st) -> close_all_throw(st, iters(), thrown)
  }
}

fn close_all_throw(st: Agent, iters: List(JsVal), original: JsVal) -> a {
  let st =
    list.fold(list.reverse(iters), st, fn(st, it) {
      let #(_superseded, st) = iter_protocol.call_return(st, it)
      st
    })
  rt_store.t_throw(st, original)
}

fn close_all_throw_done(
  st: Agent,
  ref: Handle,
  open: List(JsVal),
  thrown: JsVal,
) -> a {
  let st =
    list.fold(list.reverse(open), st, fn(st, it) {
      let #(_superseded, st) = iter_protocol.call_return(st, it)
      st
    })
  rt_store.t_throw(mark_done(st, ref), thrown)
}

fn close_all_normal(
  st: Agent,
  iters: List(JsVal),
) -> #(Result(Nil, JsVal), Agent) {
  list.fold(list.reverse(iters), #(Ok(Nil), st), fn(acc, it) {
    let #(completion, st) = acc
    case completion {
      Ok(Nil) -> close_normal_catch(st, it)
      Error(e) -> {
        let #(_superseded, st) = iter_protocol.call_return(st, it)
        #(Error(e), st)
      }
    }
  })
}

fn zip_write_members(
  st: Agent,
  ref: Handle,
  members: List(ZipMember),
) -> Agent {
  use body <- map_helper_body(st, ref)
  case body {
    ZipHelper(mode:, keys:, members: _) -> ZipHelper(members:, mode:, keys:)
    ClassicHelper(..) | ConcatHelper(..) -> body
  }
}

fn concat(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  concat_validate(st, args, [])
}

fn concat_validate(
  st: Agent,
  items: List(JsVal),
  acc: List(ConcatItem),
) -> #(JsVal, Agent) {
  case items {
    [] ->
      alloc_helper_body(
        st,
        ConcatHelper(remaining: list.reverse(acc), inner: None),
      )
    [item, ..rest] ->
      case classify(item) {
        KHandle(_) -> {
          let #(method, st) =
            rt_obj.t_get_prop(st, item, SymbolKey(symbol_iterator))
          case classify(method) {
            KUndef | KNull ->
              throw_type_error(st, "Iterator.concat argument is not iterable")
            _ ->
              case rt_call.is_callable(st, method) {
                True ->
                  concat_validate(st, rest, [
                    ConcatItem(open_method: method, iterable: item),
                    ..acc
                  ])
                False ->
                  throw_type_error(
                    st,
                    "Iterator.concat argument [Symbol.iterator] is not callable",
                  )
              }
          }
        }
        _ -> throw_type_error(st, "Iterator.concat argument is not an object")
      }
  }
}

fn concat_next(
  st: Agent,
  ref: Handle,
  remaining: List(ConcatItem),
  inner: Option(IteratorRecord),
) -> #(JsVal, Agent) {
  case inner {
    Some(inner_rec) -> {
      let #(step, st) =
        protected_any(st, fn(st) {
          iter_protocol.iterator_step_value(st, inner_rec)
        })
      case step {
        ThrowCompletion(thrown) ->
          rt_store.t_throw(concat_mark_done(st, ref), thrown)
        NormalCompletion(Some(v)) -> iter_yield(st, v)
        NormalCompletion(None) -> {
          let st = concat_write(st, ref, remaining, None)
          concat_open_next(st, ref, remaining)
        }
      }
    }
    None -> concat_open_next(st, ref, remaining)
  }
}

fn concat_open_next(
  st: Agent,
  ref: Handle,
  remaining: List(ConcatItem),
) -> #(JsVal, Agent) {
  case remaining {
    [] -> iter_done(concat_mark_done(st, ref))
    [ConcatItem(open_method: method, iterable:), ..rest] ->
      case rt_call.t_call(st, method, iterable, []) {
        #(rt_call.ThrowCompletion(thrown), st) ->
          rt_store.t_throw(concat_mark_done(st, ref), thrown)
        #(rt_call.NormalCompletion(iter), st) -> {
          let #(open, st) =
            protected_any(st, fn(st) {
              iter_protocol.get_iterator_direct(
                st,
                iter,
                "Result of the Symbol.iterator method is not an object",
              )
            })
          case open {
            ThrowCompletion(thrown) ->
              rt_store.t_throw(concat_mark_done(st, ref), thrown)
            NormalCompletion(inner) -> {
              let st = concat_write(st, ref, rest, Some(inner))
              concat_next(st, ref, rest, Some(inner))
            }
          }
        }
      }
  }
}

fn concat_return(
  st: Agent,
  ref: Handle,
  inner: Option(IteratorRecord),
) -> #(JsVal, Agent) {
  let #(close_res, st) = case inner {
    Some(inner_rec) -> close_normal_catch(st, inner_rec.iterator)
    None -> #(Ok(Nil), st)
  }
  finish_after_close(concat_mark_done(st, ref), ref, close_res)
}

fn concat_mark_done(st: Agent, ref: Handle) -> Agent {
  let st = mark_done(st, ref)
  use body <- map_helper_body(st, ref)
  case body {
    ConcatHelper(remaining:, inner: _) -> ConcatHelper(remaining:, inner: None)
    ClassicHelper(..) | ZipHelper(..) -> body
  }
}

fn concat_write(
  st: Agent,
  ref: Handle,
  remaining: List(ConcatItem),
  inner: Option(IteratorRecord),
) -> Agent {
  use body <- map_helper_body(st, ref)
  case body {
    ConcatHelper(..) -> ConcatHelper(remaining:, inner:)
    ClassicHelper(..) | ZipHelper(..) -> body
  }
}
