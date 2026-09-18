import arc/bytecode/key.{
  type PropertyKey, Index, Named, max_array_index, max_array_length,
}
import arc/rt/abstract_ops as rt_abstract_ops
import arc/rt/builtins/array_from_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/object as b_object
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/elements
import arc/rt/lang as rt_lang
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type ArrayNative, type BuiltinPair, type Cell, type Handle,
  type JsElements, type JsVal, type Property, ArrayConstructor, ArrayFrom,
  ArrayFromAsync, ArrayFromAsyncCloseReject, ArrayFromAsyncLikeOnMapped,
  ArrayFromAsyncLikeOnValue, ArrayFromAsyncOnMapped, ArrayFromAsyncOnNext,
  ArrayFromAsyncRejectWith, ArrayIsArray, ArrayIterEntries, ArrayIterKeys,
  ArrayIterValues, ArrayIterator, ArrayN, ArrayObj, ArrayOf, ArrayPrototypeAt,
  ArrayPrototypeConcat, ArrayPrototypeCopyWithin, ArrayPrototypeEntries,
  ArrayPrototypeEvery, ArrayPrototypeFill, ArrayPrototypeFilter,
  ArrayPrototypeFind, ArrayPrototypeFindIndex, ArrayPrototypeFindLast,
  ArrayPrototypeFindLastIndex, ArrayPrototypeFlat, ArrayPrototypeFlatMap,
  ArrayPrototypeForEach, ArrayPrototypeIncludes, ArrayPrototypeIndexOf,
  ArrayPrototypeJoin, ArrayPrototypeKeys, ArrayPrototypeLastIndexOf,
  ArrayPrototypeMap, ArrayPrototypePop, ArrayPrototypePush, ArrayPrototypeReduce,
  ArrayPrototypeReduceRight, ArrayPrototypeReverse, ArrayPrototypeShift,
  ArrayPrototypeSlice, ArrayPrototypeSome, ArrayPrototypeSort,
  ArrayPrototypeSplice, ArrayPrototypeToLocaleString, ArrayPrototypeToReversed,
  ArrayPrototypeToSorted, ArrayPrototypeToSpliced, ArrayPrototypeToString,
  ArrayPrototypeUnshift, ArrayPrototypeValues, ArrayPrototypeWith, DataProperty,
  JFloat, JInt, JNan, JNegInf, JPosInf, KHandle, KNull, KNum, KStr, KUndef,
  ObjectPrototypeToString, Ordinary, ParsedDesc, ProxyObj, ReturnThis, SObject,
  StringKey, StringObj, SymbolKey, classify, mk_bool, mk_int, mk_object,
  mk_string, mk_undefined, plain_object, symbol_is_concat_spreadable,
  symbol_iterator, symbol_species, symbol_unscopables,
}
import arc/rt/utf8
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

const iteration_budget_msg = "Invalid array length"

fn check_budget(st: Agent, exhausted exhausted: Bool) -> Nil {
  case exhausted {
    True -> rt_val.t_throw_range_error(st, iteration_budget_msg)
    False -> Nil
  }
}

fn within_budget(st: Agent, length: Int, k: fn() -> a) -> a {
  case length > limits.max_iteration {
    True -> rt_val.t_throw_range_error(st, iteration_budget_msg)
    False -> k()
  }
}

const cannot_convert = "Cannot convert undefined or null to object"

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("join", ArrayN(ArrayPrototypeJoin), 1),
      #("push", ArrayN(ArrayPrototypePush), 1),
      #("pop", ArrayN(ArrayPrototypePop), 0),
      #("shift", ArrayN(ArrayPrototypeShift), 0),
      #("unshift", ArrayN(ArrayPrototypeUnshift), 1),
      #("slice", ArrayN(ArrayPrototypeSlice), 2),
      #("concat", ArrayN(ArrayPrototypeConcat), 1),
      #("reverse", ArrayN(ArrayPrototypeReverse), 0),
      #("fill", ArrayN(ArrayPrototypeFill), 1),
      #("at", ArrayN(ArrayPrototypeAt), 1),
      #("indexOf", ArrayN(ArrayPrototypeIndexOf), 1),
      #("lastIndexOf", ArrayN(ArrayPrototypeLastIndexOf), 1),
      #("includes", ArrayN(ArrayPrototypeIncludes), 1),
      #("forEach", ArrayN(ArrayPrototypeForEach), 1),
      #("map", ArrayN(ArrayPrototypeMap), 1),
      #("filter", ArrayN(ArrayPrototypeFilter), 1),
      #("reduce", ArrayN(ArrayPrototypeReduce), 1),
      #("reduceRight", ArrayN(ArrayPrototypeReduceRight), 1),
      #("every", ArrayN(ArrayPrototypeEvery), 1),
      #("some", ArrayN(ArrayPrototypeSome), 1),
      #("find", ArrayN(ArrayPrototypeFind), 1),
      #("findIndex", ArrayN(ArrayPrototypeFindIndex), 1),
      #("sort", ArrayN(ArrayPrototypeSort), 1),
      #("splice", ArrayN(ArrayPrototypeSplice), 2),
      #("findLast", ArrayN(ArrayPrototypeFindLast), 1),
      #("findLastIndex", ArrayN(ArrayPrototypeFindLastIndex), 1),
      #("flat", ArrayN(ArrayPrototypeFlat), 0),
      #("flatMap", ArrayN(ArrayPrototypeFlatMap), 1),
      #("copyWithin", ArrayN(ArrayPrototypeCopyWithin), 2),
      #("toSpliced", ArrayN(ArrayPrototypeToSpliced), 2),
      #("with", ArrayN(ArrayPrototypeWith), 2),
      #("toSorted", ArrayN(ArrayPrototypeToSorted), 1),
      #("toReversed", ArrayN(ArrayPrototypeToReversed), 0),
      #("toString", ArrayN(ArrayPrototypeToString), 0),
      #("toLocaleString", ArrayN(ArrayPrototypeToLocaleString), 0),
      #("keys", ArrayN(ArrayPrototypeKeys), 0),
      #("values", ArrayN(ArrayPrototypeValues), 0),
      #("entries", ArrayN(ArrayPrototypeEntries), 0),
    ])
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("isArray", ArrayN(ArrayIsArray), 1),
      #("from", ArrayN(ArrayFrom), 1),
      #("fromAsync", ArrayN(ArrayFromAsync), 1),
      #("of", ArrayN(ArrayOf), 0),
    ])
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(_) { ArrayN(ArrayConstructor) },
      "Array",
      1,
      static_methods,
    )
  let st =
    rt_store.t_cell_update(st, bt.prototype, fn(cell) {
      case cell {
        SObject(..) as cell -> SObject(..cell, kind: ArrayObj(0))
        other -> other
      }
    })
  let assert Ok(#(_, DataProperty(value: values_fn, ..))) =
    list.find(proto_methods, fn(entry) { entry.0 == "values" })
  let #(values_prop, st) = rt_store.t_builtin_property(st, values_fn)
  let st =
    common.add_symbol_property(st, bt.prototype, symbol_iterator, values_prop)
  let unscopable_names = [
    "at", "copyWithin", "entries", "fill", "find", "findIndex", "findLast",
    "findLastIndex", "flat", "flatMap", "includes", "keys", "toReversed",
    "toSorted", "toSpliced", "values",
  ]
  let #(unscopable_props, st) =
    list.fold(unscopable_names, #(dict.new(), st), fn(acc, name) {
      let #(props, st) = acc
      let #(seq, st) = rt_store.t_next_prop_seq(st)
      #(
        dict.insert(
          props,
          Named(name),
          types.plain_property(mk_bool(True), seq),
        ),
        st,
      )
    })
  let #(unscopables_h, st) =
    rt_store.t_cell_new(st, plain_object(Ordinary, None, unscopable_props))
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  let st =
    common.add_symbol_property(
      st,
      bt.prototype,
      symbol_unscopables,
      DataProperty(
        value: mk_object(unscopables_h),
        writable: False,
        enumerable: False,
        configurable: True,
        seq:,
      ),
    )
  let st = common.add_species_accessor(st, fn_proto, bt.constructor, ReturnThis)
  #(bt, st)
}

pub fn dispatch(
  st: Agent,
  native: ArrayNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    ArrayConstructor -> construct(st, args)
    ArrayIsArray -> is_array(st, args)
    ArrayFrom -> array_from(st, this, args)
    ArrayFromAsync -> array_from_async.from_async(st, this, args)
    ArrayFromAsyncOnNext(ctx:) -> array_from_async.on_next(st, ctx, args)
    ArrayFromAsyncOnMapped(ctx:) -> array_from_async.on_mapped(st, ctx, args)
    ArrayFromAsyncCloseReject(iter:, reject:) ->
      array_from_async.close_reject(st, iter, reject, args)
    ArrayFromAsyncRejectWith(error:, reject:) ->
      array_from_async.reject_with(st, error, reject)
    ArrayFromAsyncLikeOnValue(ctx:) ->
      array_from_async.like_on_value(st, ctx, args)
    ArrayFromAsyncLikeOnMapped(ctx:) ->
      array_from_async.like_on_mapped(st, ctx, args)
    ArrayOf -> array_of(st, this, args)
    ArrayPrototypeJoin -> array_join(st, this, args)
    ArrayPrototypePush -> array_push(st, this, args)
    ArrayPrototypePop -> array_pop(st, this, args)
    ArrayPrototypeShift -> array_shift(st, this, args)
    ArrayPrototypeUnshift -> array_unshift(st, this, args)
    ArrayPrototypeSlice -> array_slice(st, this, args)
    ArrayPrototypeConcat -> array_concat(st, this, args)
    ArrayPrototypeReverse -> array_reverse(st, this, args)
    ArrayPrototypeFill -> array_fill(st, this, args)
    ArrayPrototypeAt -> array_at(st, this, args)
    ArrayPrototypeIndexOf -> array_index_of(st, this, args)
    ArrayPrototypeLastIndexOf -> array_last_index_of(st, this, args)
    ArrayPrototypeIncludes -> array_includes(st, this, args)
    ArrayPrototypeForEach -> array_for_each(st, this, args)
    ArrayPrototypeMap -> array_map(st, this, args)
    ArrayPrototypeFilter -> array_filter(st, this, args)
    ArrayPrototypeReduce -> array_reduce(st, this, args)
    ArrayPrototypeReduceRight -> array_reduce_right(st, this, args)
    ArrayPrototypeEvery -> array_every(st, this, args)
    ArrayPrototypeSome -> array_some(st, this, args)
    ArrayPrototypeFind -> array_find(st, this, args)
    ArrayPrototypeFindIndex -> array_find_index(st, this, args)
    ArrayPrototypeFindLast -> array_find_last(st, this, args)
    ArrayPrototypeFindLastIndex -> array_find_last_index(st, this, args)
    ArrayPrototypeSort -> array_sort(st, this, args)
    ArrayPrototypeSplice -> array_splice(st, this, args)
    ArrayPrototypeFlat -> array_flat(st, this, args)
    ArrayPrototypeFlatMap -> array_flat_map(st, this, args)
    ArrayPrototypeCopyWithin -> array_copy_within(st, this, args)
    ArrayPrototypeToSpliced -> array_to_spliced(st, this, args)
    ArrayPrototypeWith -> array_with(st, this, args)
    ArrayPrototypeToSorted -> array_to_sorted(st, this, args)
    ArrayPrototypeToReversed -> array_to_reversed(st, this, args)
    ArrayPrototypeToString -> array_to_string(st, this)
    ArrayPrototypeToLocaleString -> array_to_locale_string(st, this, args)
    ArrayPrototypeKeys -> array_keys(st, this)
    ArrayPrototypeValues -> array_values(st, this)
    ArrayPrototypeEntries -> array_entries(st, this)
  }
}

fn construct(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  case args {
    [] -> alloc_array(st, 0, elements.new(), array_proto)
    [only] ->
      case classify(only) {
        KNum(num) ->
          case num {
            JInt(n) ->
              case n >= 0 && n <= max_array_length {
                True -> alloc_array(st, n, elements.new(), array_proto)
                False -> rt_val.t_throw_range_error(st, "Invalid array length")
              }
            JFloat(f) ->
              case array_length_of_float(f) {
                Some(n) -> alloc_array(st, n, elements.new(), array_proto)
                None -> rt_val.t_throw_range_error(st, "Invalid array length")
              }
            JNan | JPosInf | JNegInf ->
              rt_val.t_throw_range_error(st, "Invalid array length")
          }
        _ -> alloc_array(st, 1, elements.from_list([only]), array_proto)
      }
    _ -> {
      let count = list.length(args)
      alloc_array(st, count, elements.from_list(args), array_proto)
    }
  }
}

fn array_length_of_float(f: Float) -> Option(Int) {
  case rt_val.integral_int(f +. 0.0) {
    Some(n) if n >= 0 && n <= max_array_length -> Some(n)
    _ -> None
  }
}

fn is_array(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  #(
    mk_bool(rt_abstract_ops.is_array(st, helpers.first_arg_or_undefined(args))),
    st,
  )
}

fn alloc_array(
  st: Agent,
  length: Int,
  elements: JsElements,
  array_proto: Handle,
) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: ArrayObj(length),
        proto: Some(array_proto),
        props: dict.new(),
        symbol_props: [],
        elements:,
        extensible: True,
      ),
    )
  #(mk_object(h), st)
}

fn alloc_array_list(st: Agent, values: List(JsVal)) -> #(JsVal, Agent) {
  let #(h, st) = realm_ops.alloc_array(st, values)
  #(mk_object(h), st)
}

// reads no properties, must not get length
fn require_object(
  st: Agent,
  this: JsVal,
  cont: fn(JsVal, Handle, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case classify(this) {
    KUndef | KNull -> rt_val.t_throw_type_error(st, cannot_convert)
    _ -> {
      let #(h, st) = rt_val.t_to_object(st, this)
      cont(mk_object(h), h, st)
    }
  }
}

fn require_length(
  st: Agent,
  h: Handle,
  cont: fn(Int, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(length, st) = object_length(st, h)
  cont(length, st)
}

fn require_array(
  st: Agent,
  this: JsVal,
  cont: fn(JsVal, Handle, Int, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use obj, h, st <- require_object(st, this)
  use length, st <- require_length(st, h)
  cont(obj, h, length, st)
}

fn object_length(st: Agent, h: Handle) -> #(Int, Agent) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: ArrayObj(length:), ..) -> #(length, st)
    SObject(kind: StringObj(value: s), ..) -> #(utf8.length(s), st)
    SObject(props:, ..) -> length_of_properties(st, h, props)
    types.SShapedObject(..) as s ->
      case rt_obj.as_sobject(s) {
        SObject(props:, ..) -> length_of_properties(st, h, props)
        _ -> #(0, st)
      }
    _ -> #(0, st)
  }
}

fn length_of_properties(
  st: Agent,
  h: Handle,
  props: Dict(PropertyKey, Property),
) -> #(Int, Agent) {
  case dict.get(props, Named("length")) {
    Ok(DataProperty(value: len_val, ..)) -> rt_val.t_to_length(st, len_val)
    _ -> {
      rt_abstract_ops.length_of_array_like(st, mk_object(h))
    }
  }
}

fn require_callback(
  st: Agent,
  args: List(JsVal),
  cont: fn(ElementFn, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(cb, this_arg) = helpers.two_args_or_undefined(args)
  use call <- require_bound(st, cb, this_arg)
  cont(call, st)
}

fn require_bound(
  st: Agent,
  cb: JsVal,
  this: JsVal,
  cont: fn(ElementFn) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case rt_call.t_try_prepare_call(st, cb, this) {
    Some(call) -> cont(call)
    None -> rt_val.t_throw_type_error(st, not_a_function(st, cb))
  }
}

fn not_a_function(st: Agent, v: JsVal) -> String {
  rt_val.type_of(st, v) <> " is not a function"
}

fn delete_count(
  st: Agent,
  args: List(JsVal),
  length: Int,
  actual_start: Int,
) -> #(#(Int, List(JsVal)), Agent) {
  case args {
    [] -> #(#(0, []), st)
    [_] -> #(#(length - actual_start, []), st)
    [_, dc_val, ..rest] -> {
      let #(dc, st) = rt_val.t_to_integer_or_infinity(st, dc_val)
      #(#(int.clamp(dc, 0, length - actual_start), rest), st)
    }
  }
}

fn guard_safe_length(
  st: Agent,
  n: Int,
  cont: fn() -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case n > limits.max_safe_integer {
    True ->
      rt_val.t_throw_type_error(st, "Array length exceeds maximum safe integer")
    False -> cont()
  }
}

fn generic_set(st: Agent, h: Handle, pk: PropertyKey, val: JsVal) -> Agent {
  let #(ok, st) = rt_obj.t_set_prop(st, mk_object(h), StringKey(pk), val)
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot assign to read only property '"
          <> key.display_string(pk)
          <> "' of object",
      )
  }
}

fn generic_set_index(st: Agent, h: Handle, idx: Int, val: JsVal) -> Agent {
  generic_set(st, h, key.index(idx), val)
}

fn generic_set_length(st: Agent, h: Handle, len: Int) -> Agent {
  generic_set(st, h, Named("length"), mk_int(len))
}

fn generic_delete(st: Agent, h: Handle, pk: PropertyKey) -> Agent {
  let #(ok, st) = rt_obj.t_delete_prop(st, h, StringKey(pk))
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot delete property '" <> key.display_string(pk) <> "' of object",
      )
  }
}

fn generic_delete_index(st: Agent, h: Handle, idx: Int) -> Agent {
  generic_delete(st, h, key.index(idx))
}

fn generic_has_op(st: Agent, h: Handle, idx: Int) -> #(Bool, Agent) {
  rt_obj.t_has_prop(st, mk_object(h), StringKey(key.index(idx)))
}

fn generic_get(st: Agent, h: Handle, idx: Int) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, mk_object(h), StringKey(key.index(idx)))
}

fn get_index_if_present(
  st: Agent,
  this: JsVal,
  idx: Int,
) -> #(Option(JsVal), Agent) {
  case elements.own_element(st, this, idx) {
    elements.Hit(v) -> #(Some(v), st)
    elements.Miss -> probe_index_if_present(st, this, idx)
  }
}

fn probe_index_if_present(
  st: Agent,
  this: JsVal,
  idx: Int,
) -> #(Option(JsVal), Agent) {
  case classify(this), idx >= 0 && idx <= max_array_index {
    KHandle(h), True ->
      case rt_obj.t_get_own_index(st, h, idx) {
        rt_obj.OwnIndexValue(v) -> #(Some(v), st)
        rt_obj.OwnIndexProperty(prop) -> {
          let #(v, st) = rt_obj.t_property_get_value(st, prop, this)
          #(Some(v), st)
        }
        rt_obj.OwnIndexAbsent(Some(proto)) ->
          inherited_index(st, proto, this, idx)
        rt_obj.OwnIndexAbsent(None) -> #(None, st)
        rt_obj.OwnIndexExotic -> generic_index_if_present(st, this, idx)
      }
    _, _ -> generic_index_if_present(st, this, idx)
  }
}

fn inherited_index(
  st: Agent,
  proto: Handle,
  this: JsVal,
  idx: Int,
) -> #(Option(JsVal), Agent) {
  let key = StringKey(key.index(idx))
  let #(has, st) = rt_obj.t_has_prop(st, mk_object(proto), key)
  case has {
    False -> #(None, st)
    True -> {
      let #(v, st) = rt_obj.t_get_prop(st, this, key)
      #(Some(v), st)
    }
  }
}

fn generic_index_if_present(
  st: Agent,
  this: JsVal,
  idx: Int,
) -> #(Option(JsVal), Agent) {
  let #(has, st) = rt_obj.t_has_prop(st, this, StringKey(key.index(idx)))
  case has {
    False -> #(None, st)
    True -> {
      let #(v, st) = rt_abstract_ops.get_index(st, this, idx)
      #(Some(v), st)
    }
  }
}

// some only when reading elements cannot run user code
fn dense_snapshot(
  st: Agent,
  this: JsVal,
) -> Option(#(JsElements, Option(Handle))) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: ArrayObj(_), props:, elements: els, proto:, ..) ->
          case properties_have_index_keys(props) {
            True -> None
            False -> Some(#(els, proto))
          }
        _ -> None
      }
    _ -> None
  }
}

fn properties_have_index_keys(props: Dict(PropertyKey, Property)) -> Bool {
  !dict.is_empty(props) && any_index_key(dict.keys(props))
}

fn any_index_key(keys: List(PropertyKey)) -> Bool {
  case keys {
    [] -> False
    [Index(_), ..] -> True
    [_, ..rest] -> any_index_key(rest)
  }
}

fn with_plain_elements(
  st: Agent,
  h: Handle,
  expected_len: Int,
  from: Int,
  to: Int,
  transform: fn(JsElements, Int) -> #(JsElements, Int, payload),
) -> Option(#(payload, Agent)) {
  case rt_store.t_cell_get(st, h) {
    SObject(
      kind: ArrayObj(length:),
      props:,
      elements: els,
      proto:,
      extensible: True,
      ..,
    ) as cell -> {
      let length_writable = case dict.get(props, Named("length")) {
        Ok(DataProperty(writable:, ..)) -> writable
        _ -> True
      }
      let count = to - from
      let eligible =
        length == expected_len
        && length_writable
        && index_range_plain(st, props, proto, from, count)
      case eligible {
        False -> None
        True -> {
          let #(els, new_length, payload) = transform(els, length)
          let st =
            rt_store.t_cell_set(
              st,
              h,
              SObject(..cell, kind: ArrayObj(new_length), elements: els),
            )
          Some(#(payload, st))
        }
      }
    }
    _ -> None
  }
}

fn push_dense(
  st: Agent,
  h: Handle,
  cell: Cell,
  args: List(JsVal),
) -> Option(#(Int, Agent)) {
  case cell {
    SObject(
      kind: ArrayObj(length:),
      props:,
      elements: els,
      proto:,
      extensible: True,
      ..,
    ) -> {
      let arg_count = list.length(args)
      let length_writable = case dict.get(props, Named("length")) {
        Ok(DataProperty(writable:, ..)) -> writable
        _ -> True
      }
      let eligible =
        length + arg_count <= max_array_length
        && length_writable
        && index_range_plain(st, props, proto, length, arg_count)
      case eligible {
        False -> None
        True -> {
          let new_length = length + arg_count
          let st =
            rt_store.t_cell_set(
              st,
              h,
              SObject(
                ..cell,
                kind: ArrayObj(new_length),
                elements: elements.write_list(els, length, args),
              ),
            )
          Some(#(new_length, st))
        }
      }
    }
    _ -> None
  }
}

@external(erlang, "arc_rt_array_ffi", "index_range_plain")
fn index_range_plain(
  st: Agent,
  props: Dict(PropertyKey, Property),
  proto: Option(Handle),
  start: Int,
  count: Int,
) -> Bool

fn hole_is_inherited(
  st: Agent,
  proto: Option(Handle),
  idx: Int,
) -> #(Bool, Agent) {
  case proto {
    None -> #(False, st)
    Some(proto_h) ->
      case rt_store.t_cell_get(st, proto_h) {
        SObject(kind: ProxyObj(..), ..) -> #(True, st)
        _ ->
          rt_obj.t_has_prop(st, mk_object(proto_h), StringKey(key.index(idx)))
      }
  }
}

fn array_join(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  let sep_val = case args {
    [v, ..] ->
      case classify(v) {
        KUndef -> mk_string(",")
        _ -> v
      }
    [] -> mk_string(",")
  }
  let #(separator, st) = rt_val.t_to_string(st, sep_val)
  use <- within_budget(st, length)
  let #(joined, st) = join_elements(st, this, 0, length, separator, [])
  #(mk_string(joined), st)
}

fn finish_join(
  st: Agent,
  acc: List(String),
  separator: String,
) -> #(String, Agent) {
  case limits.join(list.reverse(acc), separator) {
    Ok(joined) -> #(joined, st)
    Error(Nil) -> rt_val.t_throw_range_error(st, "Invalid string length")
  }
}

fn join_elements(
  st: Agent,
  this: JsVal,
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
) -> #(String, Agent) {
  case dense_snapshot(st, this) {
    Some(#(els, proto)) ->
      join_elements_snapshot(st, this, els, proto, idx, length, separator, acc)
    None -> join_elements_generic(st, this, idx, length, separator, acc)
  }
}

fn join_elements_snapshot(
  st: Agent,
  this: JsVal,
  els: JsElements,
  proto: Option(Handle),
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
) -> #(String, Agent) {
  case idx >= length {
    True -> finish_join(st, acc, separator)
    False ->
      case elements.get_option(els, idx) {
        Some(v) ->
          case classify(v) {
            KUndef | KNull ->
              join_elements_snapshot(
                st,
                this,
                els,
                proto,
                idx + 1,
                length,
                separator,
                ["", ..acc],
              )
            // object tostring may run user code, bail to generic
            KHandle(_) ->
              join_elements_generic(st, this, idx, length, separator, acc)
            _ -> {
              let #(s, st) = rt_val.t_to_string(st, v)
              join_elements_snapshot(
                st,
                this,
                els,
                proto,
                idx + 1,
                length,
                separator,
                [s, ..acc],
              )
            }
          }
        None -> {
          let #(inherited, st) = hole_is_inherited(st, proto, idx)
          case inherited {
            False ->
              join_elements_snapshot(
                st,
                this,
                els,
                proto,
                idx + 1,
                length,
                separator,
                ["", ..acc],
              )
            True -> join_elements_generic(st, this, idx, length, separator, acc)
          }
        }
      }
  }
}

fn join_elements_generic(
  st: Agent,
  this: JsVal,
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
) -> #(String, Agent) {
  case idx >= length {
    True -> finish_join(st, acc, separator)
    False -> {
      let #(v, st) = rt_abstract_ops.get_index(st, this, idx)
      case classify(v) {
        KUndef | KNull ->
          join_elements_generic(st, this, idx + 1, length, separator, [
            "",
            ..acc
          ])
        _ -> {
          let #(s, st) = rt_val.t_to_string(st, v)
          join_elements_generic(st, this, idx + 1, length, separator, [s, ..acc])
        }
      }
    }
  }
}

fn array_push(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  case push(st, this, args) {
    Pushed(new_length, st) -> #(mk_int(new_length), st)
    PushMiss -> push_general(st, this, args)
  }
}

type Push {
  Pushed(Int, Agent)
  PushMiss
}

@external(erlang, "arc_rt_array_ffi", "push")
fn push(st: Agent, this: JsVal, args: List(JsVal)) -> Push

fn push_general(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let fast = case classify(this), args {
    KHandle(h), [_, ..] -> push_dense(st, h, rt_store.t_cell_get(st, h), args)
    _, _ -> None
  }
  case fast {
    Some(#(new_length, st)) -> #(mk_int(new_length), st)
    None -> {
      use _this, h, length, st <- require_array(st, this)
      use <- guard_safe_length(st, length + list.length(args))
      let #(new_length, st) = push_generic(st, h, length, args)
      #(mk_int(new_length), st)
    }
  }
}

fn push_generic(
  st: Agent,
  h: Handle,
  length: Int,
  args: List(JsVal),
) -> #(Int, Agent) {
  case args {
    [] -> {
      let is_real_array = case rt_store.t_cell_get(st, h) {
        SObject(kind: ArrayObj(..), ..) -> True
        _ -> False
      }
      use <- bool.lazy_guard(is_real_array && length > max_array_length, fn() {
        rt_val.t_throw_range_error(st, "Invalid array length")
      })
      let st = generic_set_length(st, h, length)
      #(length, st)
    }
    [val, ..rest] -> {
      let st = generic_set_index(st, h, length, val)
      push_generic(st, h, length + 1, rest)
    }
  }
}

fn array_pop(st: Agent, this: JsVal, _args: List(JsVal)) -> #(JsVal, Agent) {
  case pop(st, this) {
    Popped(val, st) -> #(val, st)
    PopMiss -> pop_general(st, this)
  }
}

type Pop {
  Popped(JsVal, Agent)
  PopMiss
}

@external(erlang, "arc_rt_array_ffi", "pop")
fn pop(st: Agent, this: JsVal) -> Pop

fn pop_general(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use _this, h, length, st <- require_array(st, this)
  case length == 0 {
    True -> #(mk_undefined(), generic_set_length(st, h, 0))
    False -> {
      let new_len = length - 1
      let fast = {
        use els, len <- with_plain_elements(st, h, length, new_len, length)
        #(elements.truncate(els, len - 1), len - 1, elements.get(els, len - 1))
      }
      case fast {
        Some(#(val, st)) -> #(val, st)
        None -> {
          let #(val, st) = generic_get(st, h, new_len)
          let st = generic_delete_index(st, h, new_len)
          #(val, generic_set_length(st, h, new_len))
        }
      }
    }
  }
}

fn array_shift(st: Agent, this: JsVal, _args: List(JsVal)) -> #(JsVal, Agent) {
  use _this, h, length, st <- require_array(st, this)
  case length == 0 {
    True -> #(mk_undefined(), generic_set_length(st, h, 0))
    False -> {
      let fast = {
        use els, len <- with_plain_elements(st, h, length, 0, length)
        let first = elements.get(els, 0)
        let els =
          elements.move_range(els, 1, len, -1) |> elements.truncate(len - 1)
        #(els, len - 1, first)
      }
      case fast {
        Some(#(first, st)) -> #(first, st)
        None -> {
          let #(val, st) = generic_get(st, h, 0)
          let st =
            move_range(st, h, 1, length, Ascending, -1, limits.max_iteration)
          let st = generic_delete_index(st, h, length - 1)
          #(val, generic_set_length(st, h, length - 1))
        }
      }
    }
  }
}

fn move_range(
  st: Agent,
  h: Handle,
  k: Int,
  stop: Int,
  dir: Direction,
  delta: Int,
  fuel: Int,
) -> Agent {
  let done = case dir {
    Ascending -> k >= stop
    Descending -> k < stop
  }
  case done {
    True -> st
    False -> {
      check_budget(st, fuel <= 0)
      let step = step_of(dir)
      let to = k + delta
      let #(has_k, st) = generic_has_op(st, h, k)
      let st = case has_k {
        True -> {
          let #(val, st) = generic_get(st, h, k)
          generic_set_index(st, h, to, val)
        }
        False -> generic_delete_index(st, h, to)
      }
      move_range(st, h, k + step, stop, dir, delta, fuel - 1)
    }
  }
}

fn array_unshift(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use this, h, length, st <- require_array(st, this)
  let arg_count = list.length(args)
  let new_len = length + arg_count
  // step 5 runs even with no args, observable
  use <- bool.lazy_guard(arg_count == 0, fn() {
    case classify(this) {
      KHandle(_) | KStr(_) -> #(
        mk_int(new_len),
        generic_set_length(st, h, new_len),
      )
      _ -> #(mk_int(new_len), st)
    }
  })
  use <- guard_safe_length(st, new_len)
  let fast = {
    use els, len <- with_plain_elements(st, h, length, 0, new_len)
    let els =
      elements.move_range(els, 0, len, arg_count)
      |> elements.write_list(0, args)
    #(els, len + arg_count, Nil)
  }
  case fast {
    Some(#(Nil, st)) -> #(mk_int(new_len), st)
    None -> {
      let st =
        move_range(
          st,
          h,
          length - 1,
          0,
          Descending,
          arg_count,
          limits.max_iteration,
        )
      let st = write_list_at(st, h, 0, args)
      #(mk_int(new_len), generic_set_length(st, h, new_len))
    }
  }
}

fn write_list_at(st: Agent, h: Handle, idx: Int, vals: List(JsVal)) -> Agent {
  case vals {
    [] -> st
    [v, ..rest] ->
      write_list_at(generic_set_index(st, h, idx, v), h, idx + 1, rest)
  }
}

fn array_slice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use this, _h, length, st <- require_array(st, this)
  let #(start, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 0), length, 0)
  let #(end, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 1), length, length)
  let count = int.max(end - start, 0)
  let #(species, st) = array_species_create(st, this, count)
  let #(copied, st) = copy_range(st, this, start, 0, count, elements.new())
  case species {
    None -> alloc_array(st, count, copied, array_proto)
    Some(target) -> {
      let st = write_species_result(st, target, copied, count, Some(count))
      #(mk_object(target), st)
    }
  }
}

fn copy_range_dense(
  st: Agent,
  src: JsVal,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
) -> #(JsElements, Agent) {
  check_budget(st, remaining > limits.max_iteration)
  case remaining <= 0 {
    True -> #(dst, st)
    False -> {
      let #(val, st) = rt_abstract_ops.get_index(st, src, src_idx)
      copy_range_dense(
        st,
        src,
        src_idx + 1,
        dst_idx + 1,
        remaining - 1,
        elements.set(dst, dst_idx, val),
      )
    }
  }
}

fn copy_range(
  st: Agent,
  src: JsVal,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
) -> #(JsElements, Agent) {
  copy_range_fueled(
    st,
    src,
    src_idx,
    dst_idx,
    remaining,
    dst,
    limits.max_iteration,
  )
}

fn copy_range_fueled(
  st: Agent,
  src: JsVal,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
  fuel: Int,
) -> #(JsElements, Agent) {
  check_budget(st, fuel <= 0 && remaining > 0)
  case dense_snapshot(st, src) {
    Some(#(els, proto)) ->
      case elements.range_list(els, src_idx, remaining) {
        Some(vals) -> #(elements.write_list(dst, dst_idx, vals), st)
        None ->
          copy_range_snapshot(
            st,
            src,
            els,
            proto,
            src_idx,
            dst_idx,
            remaining,
            dst,
            fuel,
          )
      }
    None -> copy_range_generic(st, src, src_idx, dst_idx, remaining, dst, fuel)
  }
}

fn copy_range_snapshot(
  st: Agent,
  src: JsVal,
  els: JsElements,
  proto: Option(Handle),
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
  fuel: Int,
) -> #(JsElements, Agent) {
  check_budget(st, fuel <= 0 && remaining > 0)
  case remaining <= 0 {
    True -> #(dst, st)
    False ->
      case elements.get_option(els, src_idx) {
        Some(val) ->
          copy_range_snapshot(
            st,
            src,
            els,
            proto,
            src_idx + 1,
            dst_idx + 1,
            remaining - 1,
            elements.set(dst, dst_idx, val),
            fuel - 1,
          )
        None -> {
          let #(inherited, st) = hole_is_inherited(st, proto, src_idx)
          case inherited {
            False ->
              copy_range_snapshot(
                st,
                src,
                els,
                proto,
                src_idx + 1,
                dst_idx + 1,
                remaining - 1,
                dst,
                fuel - 1,
              )
            True ->
              copy_range_generic(
                st,
                src,
                src_idx,
                dst_idx,
                remaining,
                dst,
                fuel,
              )
          }
        }
      }
  }
}

fn copy_range_generic(
  st: Agent,
  src: JsVal,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
  fuel: Int,
) -> #(JsElements, Agent) {
  check_budget(st, fuel <= 0 && remaining > 0)
  case remaining <= 0 {
    True -> #(dst, st)
    False -> {
      let #(maybe_val, st) = get_index_if_present(st, src, src_idx)
      case maybe_val {
        Some(val) ->
          copy_range_generic(
            st,
            src,
            src_idx + 1,
            dst_idx + 1,
            remaining - 1,
            elements.set(dst, dst_idx, val),
            fuel - 1,
          )
        None ->
          copy_range_generic(
            st,
            src,
            src_idx + 1,
            dst_idx + 1,
            remaining - 1,
            dst,
            fuel - 1,
          )
      }
    }
  }
}

fn array_concat(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use this, _this_h, st <- require_object(st, this)
  let #(species, st) = array_species_create(st, this, 0)
  let all_items = [this, ..args]
  case species {
    None -> {
      let #(#(elems, total), st) =
        concat_items(st, all_items, elements.new(), 0)
      alloc_array(st, total, elems, array_proto)
    }
    Some(target) -> {
      let #(total, st) = concat_items_species(st, all_items, target, 0)
      let st = generic_set_length(st, target, total)
      #(mk_object(target), st)
    }
  }
}

fn concat_items(
  st: Agent,
  items: List(JsVal),
  elems: JsElements,
  pos: Int,
) -> #(#(JsElements, Int), Agent) {
  case items {
    [] -> #(#(elems, pos), st)
    [item, ..rest] -> {
      let #(#(elems, pos), st) = concat_item(st, elems, pos, item)
      concat_items(st, rest, elems, pos)
    }
  }
}

fn concat_item(
  st: Agent,
  elems: JsElements,
  pos: Int,
  item: JsVal,
) -> #(#(JsElements, Int), Agent) {
  let #(spreadable, st) = is_concat_spreadable(st, item)
  case spreadable, classify(item) {
    True, KHandle(h) -> {
      let #(length, st) = object_length(st, h)
      use <- bool.lazy_guard(pos + length > limits.max_safe_integer, fn() {
        rt_val.t_throw_type_error(
          st,
          "Array length exceeds maximum safe integer",
        )
      })
      let #(copied, st) = copy_range(st, item, 0, pos, length, elems)
      #(#(copied, pos + length), st)
    }
    _, _ -> {
      use <- bool.lazy_guard(pos >= limits.max_safe_integer, fn() {
        rt_val.t_throw_type_error(
          st,
          "Array length exceeds maximum safe integer",
        )
      })
      #(#(elements.set(elems, pos, item), pos + 1), st)
    }
  }
}

fn concat_items_species(
  st: Agent,
  items: List(JsVal),
  target: Handle,
  pos: Int,
) -> #(Int, Agent) {
  case items {
    [] -> #(pos, st)
    [item, ..rest] -> {
      let #(spreadable, st) = is_concat_spreadable(st, item)
      case spreadable, classify(item) {
        True, KHandle(h) -> {
          let #(length, st) = object_length(st, h)
          use <- bool.lazy_guard(pos + length > limits.max_safe_integer, fn() {
            rt_val.t_throw_type_error(
              st,
              "Array length exceeds maximum safe integer",
            )
          })
          let st =
            copy_range_to_species(
              st,
              item,
              0,
              target,
              pos,
              length,
              limits.max_iteration,
            )
          concat_items_species(st, rest, target, pos + length)
        }
        _, _ -> {
          use <- bool.lazy_guard(pos >= limits.max_safe_integer, fn() {
            rt_val.t_throw_type_error(
              st,
              "Array length exceeds maximum safe integer",
            )
          })
          let st = write_species_element(st, target, pos, item)
          concat_items_species(st, rest, target, pos + 1)
        }
      }
    }
  }
}

fn is_concat_spreadable(st: Agent, item: JsVal) -> #(Bool, Agent) {
  case classify(item) {
    KHandle(_) -> {
      let #(flag, st) =
        rt_obj.t_get_prop(st, item, SymbolKey(symbol_is_concat_spreadable))
      case classify(flag) {
        KUndef -> #(rt_abstract_ops.is_array(st, item), st)
        _ -> #(rt_val.to_boolean(flag), st)
      }
    }
    _ -> #(False, st)
  }
}

fn array_species_create(
  st: Agent,
  original: JsVal,
  length: Int,
) -> #(Option(Handle), Agent) {
  let #(species, st) = case classify(original) {
    KHandle(h) ->
      case intrinsic_species(st, rt_store.t_cell_get(st, h)) {
        True -> #(None, st)
        False -> species_protocol(st, original, length)
      }
    _ -> #(None, st)
  }
  case species {
    None if length > max_array_length ->
      rt_val.t_throw_range_error(st, "Invalid array length")
    _ -> #(species, st)
  }
}

fn intrinsic_species(st: Agent, cell: Cell) -> Bool {
  let array = st.realm.array
  case cell {
    SObject(kind: ArrayObj(_), proto: Some(p), props:, ..) ->
      p == array.prototype
      && !dict.has_key(props, Named("constructor"))
      && common.species_intact(st, array)
    _ -> False
  }
}

fn species_protocol(
  st: Agent,
  original: JsVal,
  length: Int,
) -> #(Option(Handle), Agent) {
  case classify(original) {
    KHandle(_) -> {
      case rt_abstract_ops.is_array(st, original) {
        False -> #(None, st)
        True -> {
          let #(ctor, st) =
            rt_obj.t_get_prop(st, original, StringKey(Named("constructor")))
          // other realm %Array% gives undefined, species never read
          let ctor = case classify(ctor) {
            KHandle(ctor_h) ->
              case is_foreign_array_ctor(st, ctor_h) {
                True -> mk_undefined()
                False -> ctor
              }
            _ -> ctor
          }
          let #(ctor, st) = case classify(ctor) {
            KHandle(_) -> {
              let #(species, st) =
                rt_obj.t_get_prop(st, ctor, SymbolKey(symbol_species))
              case classify(species) {
                KNull -> #(mk_undefined(), st)
                _ -> #(species, st)
              }
            }
            _ -> #(ctor, st)
          }
          case classify(ctor) {
            KUndef -> #(None, st)
            KHandle(ctor_h) -> {
              let realm_array_ctor = st.realm.array.constructor
              case ctor_h == realm_array_ctor {
                True -> #(None, st)
                False -> species_construct(st, ctor, length)
              }
            }
            _ -> species_construct(st, ctor, length)
          }
        }
      }
    }
    _ -> #(None, st)
  }
}

fn is_foreign_array_ctor(st: Agent, ctor: Handle) -> Bool {
  ctor != st.realm.array.constructor
  && list.any(dict.values(st.realms), fn(r) { r.array.constructor == ctor })
}

fn species_construct(
  st: Agent,
  ctor: JsVal,
  length: Int,
) -> #(Option(Handle), Agent) {
  case rt_call.is_constructor(st, ctor) {
    False ->
      rt_val.t_throw_type_error(st, "Species constructor is not a constructor")
    True -> {
      let #(created, st) = rt_call.t_construct(st, ctor, [mk_int(length)], ctor)
      #(Some(created), st)
    }
  }
}

fn write_species_result(
  st: Agent,
  target: Handle,
  els: JsElements,
  length: Int,
  set_length: Option(Int),
) -> Agent {
  use <- within_budget(st, length)
  let st = write_species_elements(st, target, els, 0, length)
  case set_length {
    None -> st
    Some(n) -> generic_set_length(st, target, n)
  }
}

fn write_species_elements(
  st: Agent,
  target: Handle,
  els: JsElements,
  idx: Int,
  length: Int,
) -> Agent {
  case idx >= length {
    True -> st
    False ->
      case elements.get_option(els, idx) {
        None -> write_species_elements(st, target, els, idx + 1, length)
        Some(val) -> {
          let st = write_species_element(st, target, idx, val)
          write_species_elements(st, target, els, idx + 1, length)
        }
      }
  }
}

fn write_species_element(
  st: Agent,
  target: Handle,
  idx: Int,
  val: JsVal,
) -> Agent {
  let desc =
    ParsedDesc(
      value: Some(val),
      get: None,
      set: None,
      writable: Some(True),
      enumerable: Some(True),
      configurable: Some(True),
    )
  let #(ok, st) =
    rt_obj.t_define_own_prop(st, target, StringKey(key.index(idx)), desc)
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot define property " <> int.to_string(idx) <> " on object",
      )
  }
}

fn copy_range_to_species(
  st: Agent,
  src: JsVal,
  src_idx: Int,
  target: Handle,
  dst_idx: Int,
  remaining: Int,
  fuel: Int,
) -> Agent {
  check_budget(st, fuel <= 0 && remaining > 0)
  case remaining <= 0 {
    True -> st
    False -> {
      let #(maybe_val, st) = get_index_if_present(st, src, src_idx)
      let st = case maybe_val {
        None -> st
        Some(val) -> write_species_element(st, target, dst_idx, val)
      }
      copy_range_to_species(
        st,
        src,
        src_idx + 1,
        target,
        dst_idx + 1,
        remaining - 1,
        fuel - 1,
      )
    }
  }
}

fn array_reverse(
  st: Agent,
  this: JsVal,
  _args: List(JsVal),
) -> #(JsVal, Agent) {
  use this, h, length, st <- require_array(st, this)
  let fast = {
    use els, len <- with_plain_elements(st, h, length, 0, length)
    #(elements.reverse_range(els, len), len, Nil)
  }
  case fast {
    Some(#(Nil, st)) -> #(this, st)
    None -> #(this, reverse_generic(st, h, 0, length - 1, limits.max_iteration))
  }
}

fn reverse_generic(st: Agent, h: Handle, lo: Int, hi: Int, fuel: Int) -> Agent {
  case lo >= hi {
    True -> st
    False -> {
      check_budget(st, fuel <= 0)
      let obj = mk_object(h)
      let #(lo_val, st) = get_index_if_present(st, obj, lo)
      let #(hi_val, st) = get_index_if_present(st, obj, hi)
      let st = case lo_val, hi_val {
        Some(lo_v), Some(hi_v) -> {
          let st = generic_set_index(st, h, lo, hi_v)
          generic_set_index(st, h, hi, lo_v)
        }
        None, Some(hi_v) -> {
          let st = generic_set_index(st, h, lo, hi_v)
          generic_delete_index(st, h, hi)
        }
        Some(lo_v), None -> {
          let st = generic_delete_index(st, h, lo)
          generic_set_index(st, h, hi, lo_v)
        }
        None, None -> st
      }
      reverse_generic(st, h, lo + 1, hi - 1, fuel - 1)
    }
  }
}

fn array_fill(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use this, h, length, st <- require_array(st, this)
  let fill_val = helpers.first_arg_or_undefined(args)
  let #(start, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 1), length, 0)
  let #(end, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 2), length, length)
  use <- within_budget(st, end - start)
  let fast = {
    use els, len <- with_plain_elements(st, h, length, start, end)
    #(elements.fill_range(els, start, end, fill_val), len, Nil)
  }
  case fast {
    Some(#(Nil, st)) -> #(this, st)
    None -> #(this, fill_generic(st, h, start, end, fill_val))
  }
}

fn fill_generic(st: Agent, h: Handle, idx: Int, end: Int, val: JsVal) -> Agent {
  case idx >= end {
    True -> st
    False ->
      fill_generic(generic_set_index(st, h, idx, val), h, idx + 1, end, val)
  }
}

fn array_at(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  let #(raw, st) = rt_val.t_to_integer_or_infinity(st, helpers.arg_at(args, 0))
  let idx = case raw < 0 {
    True -> length + raw
    False -> raw
  }
  case idx < 0 || idx >= length {
    True -> #(mk_undefined(), st)
    False -> rt_abstract_ops.get_index(st, this, idx)
  }
}

fn array_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  forward_search_driver(st, this, args, Strict, SkipHoles, mk_int)
}

fn array_includes(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  forward_search_driver(st, this, args, SameValueZero, VisitHoles, fn(found) {
    mk_bool(found >= 0)
  })
}

fn forward_search_driver(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  eq: EqMode,
  hole_mode: HoleMode,
  wrap: fn(Int) -> JsVal,
) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use <- bool.guard(length == 0, #(wrap(-1), st))
  let search = helpers.first_arg_or_undefined(args)
  let #(from, st) = rt_val.t_to_integer_or_infinity(st, helpers.arg_at(args, 1))
  let start = case from < 0 {
    True -> int.max(length + from, 0)
    False -> from
  }
  let #(found, st) =
    search_forward(
      st,
      this,
      start,
      length,
      search,
      eq,
      hole_mode,
      limits.max_iteration,
    )
  #(wrap(found), st)
}

fn array_last_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use <- bool.guard(length == 0, #(mk_int(-1), st))
  let search = helpers.first_arg_or_undefined(args)
  // checked by arg count, explicit undefined gives 0
  let #(from, st) = case args {
    [_, f, ..] -> rt_val.t_to_integer_or_infinity(st, f)
    _ -> #(length - 1, st)
  }
  let start = case from < 0 {
    True -> length + from
    False -> int.min(from, length - 1)
  }
  let #(found, st) =
    search_backward(st, this, start, search, limits.max_iteration)
  #(mk_int(found), st)
}

type EqMode {
  Strict
  SameValueZero
}

fn eq_apply(eq: EqMode, a: JsVal, b: JsVal) -> Bool {
  case eq {
    Strict -> rt_val.strict_eq(a, b)
    SameValueZero -> rt_val.same_value_zero(a, b)
  }
}

type Scan {
  Match(Int)
  HoleAt(Int)
  Absent
}

@external(erlang, "arc_rt_array_ffi", "scan_forward")
fn scan_forward(
  els: JsElements,
  search: JsVal,
  idx: Int,
  end: Int,
  eq: EqMode,
) -> Scan

@external(erlang, "arc_rt_array_ffi", "scan_backward")
fn scan_backward(els: JsElements, search: JsVal, idx: Int, eq: EqMode) -> Scan

fn search_forward(
  st: Agent,
  this: JsVal,
  idx: Int,
  length: Int,
  search: JsVal,
  eq: EqMode,
  hole_mode: HoleMode,
  fuel: Int,
) -> #(Int, Agent) {
  case dense_snapshot(st, this) {
    Some(#(els, proto)) ->
      search_forward_snapshot(
        st,
        this,
        els,
        proto,
        idx,
        length,
        search,
        eq,
        hole_mode,
        fuel,
      )
    None ->
      search_forward_generic(st, this, idx, length, search, eq, hole_mode, fuel)
  }
}

fn search_forward_snapshot(
  st: Agent,
  this: JsVal,
  els: JsElements,
  proto: Option(Handle),
  idx: Int,
  length: Int,
  search: JsVal,
  eq: EqMode,
  hole_mode: HoleMode,
  fuel: Int,
) -> #(Int, Agent) {
  case scan_forward(els, search, idx, length, eq) {
    Match(i) -> #(i, st)
    Absent -> #(-1, st)
    HoleAt(i) -> {
      let fuel = fuel - { i - idx }
      check_budget(st, fuel <= 0)
      let #(inherited, st) = hole_is_inherited(st, proto, i)
      let matched = case inherited, hole_mode {
        False, VisitHoles -> eq_apply(eq, mk_undefined(), search)
        _, _ -> False
      }
      case inherited, matched {
        True, _ ->
          search_forward_generic(
            st,
            this,
            i,
            length,
            search,
            eq,
            hole_mode,
            fuel,
          )
        False, True -> #(i, st)
        False, False ->
          search_forward_snapshot(
            st,
            this,
            els,
            proto,
            i + 1,
            length,
            search,
            eq,
            hole_mode,
            fuel - 1,
          )
      }
    }
  }
}

fn search_forward_generic(
  st: Agent,
  this: JsVal,
  idx: Int,
  length: Int,
  search: JsVal,
  eq: EqMode,
  hole_mode: HoleMode,
  fuel: Int,
) -> #(Int, Agent) {
  check_budget(st, fuel <= 0 && idx < length)
  case idx >= length {
    True -> #(-1, st)
    False -> {
      let #(maybe_val, st) = case hole_mode {
        SkipHoles -> get_index_if_present(st, this, idx)
        VisitHoles -> {
          let #(v, st) = rt_abstract_ops.get_index(st, this, idx)
          #(Some(v), st)
        }
      }
      let matched = case maybe_val {
        Some(val) -> eq_apply(eq, val, search)
        None -> False
      }
      case matched {
        True -> #(idx, st)
        False ->
          search_forward_generic(
            st,
            this,
            idx + 1,
            length,
            search,
            eq,
            hole_mode,
            fuel - 1,
          )
      }
    }
  }
}

fn search_backward(
  st: Agent,
  this: JsVal,
  idx: Int,
  search: JsVal,
  fuel: Int,
) -> #(Int, Agent) {
  case dense_snapshot(st, this) {
    Some(#(els, proto)) ->
      search_backward_snapshot(st, this, els, proto, idx, search, fuel)
    None -> search_backward_generic(st, this, idx, search, fuel)
  }
}

fn search_backward_snapshot(
  st: Agent,
  this: JsVal,
  els: JsElements,
  proto: Option(Handle),
  idx: Int,
  search: JsVal,
  fuel: Int,
) -> #(Int, Agent) {
  case scan_backward(els, search, idx, Strict) {
    Match(i) -> #(i, st)
    Absent -> #(-1, st)
    HoleAt(i) -> {
      let fuel = fuel - { idx - i }
      check_budget(st, fuel <= 0)
      let #(inherited, st) = hole_is_inherited(st, proto, i)
      case inherited {
        True -> search_backward_generic(st, this, i, search, fuel)
        False ->
          search_backward_snapshot(
            st,
            this,
            els,
            proto,
            i - 1,
            search,
            fuel - 1,
          )
      }
    }
  }
}

fn search_backward_generic(
  st: Agent,
  this: JsVal,
  idx: Int,
  search: JsVal,
  fuel: Int,
) -> #(Int, Agent) {
  check_budget(st, fuel <= 0 && idx >= 0)
  case idx < 0 {
    True -> #(-1, st)
    False -> {
      let #(maybe_val, st) = get_index_if_present(st, this, idx)
      case maybe_val {
        None -> search_backward_generic(st, this, idx - 1, search, fuel - 1)
        Some(val) ->
          case rt_val.strict_eq(val, search) {
            True -> #(idx, st)
            False ->
              search_backward_generic(st, this, idx - 1, search, fuel - 1)
          }
      }
    }
  }
}

// skip = hasproperty gated, visit = plain get
type HoleMode {
  SkipHoles
  VisitHoles
}

type Direction {
  Ascending
  Descending
}

fn bounds(dir: Direction, length: Int) -> #(Int, Int, Int) {
  case dir {
    Ascending -> #(0, length, 1)
    Descending -> #(length - 1, -1, -1)
  }
}

fn step_of(dir: Direction) -> Int {
  case dir {
    Ascending -> 1
    Descending -> -1
  }
}

type FoundAt {
  Found(element: JsVal, index: Int)
  NotFound
}

fn iterate_array(
  st: Agent,
  arr: JsVal,
  length: Int,
  dir: Direction,
  cb: ElementFn,
  hole_mode: HoleMode,
  stop_on: fn(JsVal) -> Bool,
  cont: fn(FoundAt, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(start, end, step) = bounds(dir, length)
  iterate_array_loop(
    st,
    arr,
    start,
    end,
    step,
    limits.max_iteration,
    cb,
    hole_mode,
    stop_on,
    cont,
  )
}

fn iterate_array_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  end: Int,
  step: Int,
  fuel: Int,
  cb: ElementFn,
  hole_mode: HoleMode,
  stop_on: fn(JsVal) -> Bool,
  cont: fn(FoundAt, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case idx == end, fuel {
    True, _ -> cont(NotFound, st)
    False, 0 -> rt_val.t_throw_range_error(st, iteration_budget_msg)
    False, _ -> {
      let #(maybe_elem, st) = case elements.own_element(st, arr, idx) {
        elements.Hit(elem) -> #(Some(elem), st)
        elements.Miss -> {
          let #(maybe_elem, st) = probe_index_if_present(st, arr, idx)
          case hole_mode {
            VisitHoles -> #(Some(option.unwrap(maybe_elem, mk_undefined())), st)
            SkipHoles -> #(maybe_elem, st)
          }
        }
      }
      case maybe_elem {
        None ->
          iterate_array_loop(
            st,
            arr,
            idx + step,
            end,
            step,
            fuel - 1,
            cb,
            hole_mode,
            stop_on,
            cont,
          )
        Some(elem) -> {
          let #(result, st) = cb(st, [elem, mk_int(idx), arr])
          case stop_on(result) {
            True -> cont(Found(elem, idx), st)
            False ->
              iterate_array_loop(
                st,
                arr,
                idx + step,
                end,
                step,
                fuel - 1,
                cb,
                hole_mode,
                stop_on,
                cont,
              )
          }
        }
      }
    }
  }
}

type ElementFn =
  fn(Agent, List(JsVal)) -> #(JsVal, Agent)

fn map_dense(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: ElementFn,
  acc: List(JsVal),
) -> #(JsElements, Agent) {
  case idx >= length {
    True -> #(elements.from_list(list.reverse(acc)), st)
    False ->
      case elements.own_element(st, arr, idx) {
        elements.Hit(elem) ->
          map_dense_step(st, arr, idx, length, cb, acc, elem)
        elements.Miss ->
          case probe_index_if_present(st, arr, idx) {
            #(Some(elem), st) ->
              map_dense_step(st, arr, idx, length, cb, acc, elem)
            #(None, st) ->
              map_sparse(
                st,
                arr,
                idx + 1,
                length,
                cb,
                elements.from_list(list.reverse(acc)),
              )
          }
      }
  }
}

fn map_dense_step(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: ElementFn,
  acc: List(JsVal),
  elem: JsVal,
) -> #(JsElements, Agent) {
  let #(result, st) = cb(st, [elem, mk_int(idx), arr])
  map_dense(st, arr, idx + 1, length, cb, [result, ..acc])
}

fn map_sparse(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: ElementFn,
  acc: JsElements,
) -> #(JsElements, Agent) {
  case idx >= length {
    True -> #(acc, st)
    False -> {
      let #(maybe_elem, st) = get_index_if_present(st, arr, idx)
      case maybe_elem {
        Some(elem) -> {
          let #(result, st) = cb(st, [elem, mk_int(idx), arr])
          map_sparse(
            st,
            arr,
            idx + 1,
            length,
            cb,
            elements.set(acc, idx, result),
          )
        }
        None -> map_sparse(st, arr, idx + 1, length, cb, acc)
      }
    }
  }
}

fn array_filter_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: ElementFn,
  kept: List(JsVal),
) -> #(List(JsVal), Agent) {
  case idx >= length {
    True -> #(kept, st)
    False ->
      case elements.own_element(st, arr, idx) {
        elements.Hit(elem) -> filter_step(st, arr, idx, length, cb, kept, elem)
        elements.Miss ->
          case probe_index_if_present(st, arr, idx) {
            #(Some(elem), st) ->
              filter_step(st, arr, idx, length, cb, kept, elem)
            #(None, st) -> array_filter_loop(st, arr, idx + 1, length, cb, kept)
          }
      }
  }
}

fn filter_step(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: ElementFn,
  kept: List(JsVal),
  elem: JsVal,
) -> #(List(JsVal), Agent) {
  let #(result, st) = cb(st, [elem, mk_int(idx), arr])
  case rt_val.to_boolean(result) {
    True -> array_filter_loop(st, arr, idx + 1, length, cb, [elem, ..kept])
    False -> array_filter_loop(st, arr, idx + 1, length, cb, kept)
  }
}

fn array_for_each(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use call, st <- require_callback(st, args)
  #(mk_undefined(), array_for_each_loop(st, this, 0, length, call))
}

fn array_for_each_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: ElementFn,
) -> Agent {
  case idx >= length, idx == limits.max_iteration {
    True, _ -> st
    False, True -> rt_val.t_throw_range_error(st, iteration_budget_msg)
    False, False -> {
      let st = case elements.own_element(st, arr, idx) {
        elements.Hit(elem) -> cb(st, [elem, mk_int(idx), arr]).1
        elements.Miss ->
          case probe_index_if_present(st, arr, idx) {
            #(Some(elem), st) -> cb(st, [elem, mk_int(idx), arr]).1
            #(None, st) -> st
          }
      }
      array_for_each_loop(st, arr, idx + 1, length, cb)
    }
  }
}

fn array_map(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use call, st <- require_callback(st, args)
  let #(species, st) = array_species_create(st, this, length)
  use <- within_budget(st, length)
  let #(els, st) = map_dense(st, this, 0, length, call, [])
  case species {
    None -> finish_array(st, els, length)
    Some(target) -> {
      let st = write_species_result(st, target, els, length, None)
      #(mk_object(target), st)
    }
  }
}

fn finish_array(
  st: Agent,
  elements: JsElements,
  length: Int,
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  alloc_array(st, length, elements, array_proto)
}

fn array_filter(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use call, st <- require_callback(st, args)
  let #(species, st) = array_species_create(st, this, 0)
  use <- within_budget(st, length)
  let #(kept_rev, st) = array_filter_loop(st, this, 0, length, call, [])
  case species {
    None -> alloc_array_list(st, list.reverse(kept_rev))
    Some(target) -> {
      let vals = list.reverse(kept_rev)
      let st =
        write_species_result(
          st,
          target,
          elements.from_list(vals),
          list.length(vals),
          None,
        )
      #(mk_object(target), st)
    }
  }
}

fn array_every(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  every_some(st, this, args, match_on: False)
}

fn array_some(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  every_some(st, this, args, match_on: True)
}

fn every_some(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  match_on match_on: Bool,
) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use call, st <- require_callback(st, args)
  use found, st <- iterate_array(
    st,
    this,
    length,
    Ascending,
    call,
    SkipHoles,
    fn(r) { rt_val.to_boolean(r) == match_on },
  )
  let stopped_early = case found {
    Found(_, _) -> True
    NotFound -> False
  }
  #(mk_bool(stopped_early == match_on), st)
}

fn find_via_predicate(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  dir: Direction,
  cont: fn(FoundAt, Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use call, st <- require_callback(st, args)
  use found, st <- iterate_array(
    st,
    this,
    length,
    dir,
    call,
    VisitHoles,
    rt_val.to_boolean,
  )
  cont(found, st)
}

fn array_find(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use found, st <- find_via_predicate(st, this, args, Ascending)
  case found {
    Found(elem, _) -> #(elem, st)
    NotFound -> #(mk_undefined(), st)
  }
}

fn array_find_index(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use found, st <- find_via_predicate(st, this, args, Ascending)
  case found {
    Found(_, idx) -> #(mk_int(idx), st)
    NotFound -> #(mk_int(-1), st)
  }
}

fn array_find_last(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use found, st <- find_via_predicate(st, this, args, Descending)
  case found {
    Found(elem, _) -> #(elem, st)
    NotFound -> #(mk_undefined(), st)
  }
}

fn array_find_last_index(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use found, st <- find_via_predicate(st, this, args, Descending)
  case found {
    Found(_, idx) -> #(mk_int(idx), st)
    NotFound -> #(mk_int(-1), st)
  }
}

fn array_reduce(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  reduce_directed(st, this, args, Ascending)
}

fn array_reduce_right(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  reduce_directed(st, this, args, Descending)
}

fn reduce_directed(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  dir: Direction,
) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  let cb = helpers.first_arg_or_undefined(args)
  use call <- require_bound(st, cb, mk_undefined())
  let #(start, end, step) = bounds(dir, length)
  let #(has_init, init) = case args {
    [_, v, ..] -> #(True, v)
    _ -> #(False, mk_undefined())
  }
  case has_init {
    True ->
      reduce_directed_loop(
        st,
        this,
        start,
        end,
        call,
        init,
        dir,
        limits.max_iteration,
      )
    False -> {
      let #(found, st) =
        find_present(st, this, start, end, dir, limits.max_iteration)
      case found {
        None ->
          rt_val.t_throw_type_error(
            st,
            "Reduce of empty array with no initial value",
          )
        Some(#(first_idx, first_val)) ->
          reduce_directed_loop(
            st,
            this,
            first_idx + step,
            end,
            call,
            first_val,
            dir,
            limits.max_iteration,
          )
      }
    }
  }
}

fn find_present(
  st: Agent,
  this: JsVal,
  idx: Int,
  end: Int,
  dir: Direction,
  fuel: Int,
) -> #(Option(#(Int, JsVal)), Agent) {
  case idx == end {
    True -> #(None, st)
    False -> {
      check_budget(st, fuel <= 0)
      let #(maybe_val, st) = get_index_if_present(st, this, idx)
      case maybe_val {
        Some(val) -> #(Some(#(idx, val)), st)
        None -> find_present(st, this, idx + step_of(dir), end, dir, fuel - 1)
      }
    }
  }
}

fn reduce_directed_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  end: Int,
  cb: ElementFn,
  acc: JsVal,
  dir: Direction,
  fuel: Int,
) -> #(JsVal, Agent) {
  case idx == end, fuel {
    True, _ -> #(acc, st)
    False, 0 -> rt_val.t_throw_range_error(st, iteration_budget_msg)
    False, _ -> {
      let step = step_of(dir)
      case elements.own_element(st, arr, idx) {
        elements.Hit(elem) -> {
          let #(result, st) = cb(st, [acc, elem, mk_int(idx), arr])
          reduce_directed_loop(
            st,
            arr,
            idx + step,
            end,
            cb,
            result,
            dir,
            fuel - 1,
          )
        }
        elements.Miss ->
          case probe_index_if_present(st, arr, idx) {
            #(Some(elem), st) -> {
              let #(result, st) = cb(st, [acc, elem, mk_int(idx), arr])
              reduce_directed_loop(
                st,
                arr,
                idx + step,
                end,
                cb,
                result,
                dir,
                fuel - 1,
              )
            }
            #(None, st) ->
              reduce_directed_loop(
                st,
                arr,
                idx + step,
                end,
                cb,
                acc,
                dir,
                fuel - 1,
              )
          }
      }
    }
  }
}

fn array_sort(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use comparefn, st <- with_comparefn(st, args)
  use this, h, length, st <- require_array(st, this)
  use <- within_budget(st, length)
  case comparefn {
    None -> sort_default(st, h, length, this)
    Some(cmp) -> sort_with_comparefn(st, h, length, cmp, this)
  }
}

fn with_comparefn(
  st: Agent,
  args: List(JsVal),
  cont: fn(Option(JsVal), Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let comparefn = helpers.first_arg_or_undefined(args)
  case classify(comparefn) {
    KUndef -> cont(None, st)
    _ -> {
      use comparefn <- helpers.require_callable(st, comparefn, fn() {
        not_a_function(st, comparefn)
      })
      cont(Some(comparefn), st)
    }
  }
}

fn sort_default(
  st: Agent,
  h: Handle,
  length: Int,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(#(defined, undefs), st) =
    collect_sort_elements(st, this, length, 0, [], 0, SkipHoles)
  let #(pairs, st) = stringify_elements(st, defined, [])
  let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
  let sorted_values = list.map(sorted, fn(pair) { pair.1 })
  let all_values =
    list.append(sorted_values, list.repeat(mk_undefined(), undefs))
  #(this, write_sort_result(st, h, all_values, length, 0))
}

fn sort_with_comparefn(
  st: Agent,
  h: Handle,
  length: Int,
  comparefn: JsVal,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(#(defined, undefs), st) =
    collect_sort_elements(st, this, length, 0, [], 0, SkipHoles)
  let #(sorted, st) = merge_sort(st, defined, comparefn)
  let all_values = list.append(sorted, list.repeat(mk_undefined(), undefs))
  #(this, write_sort_result(st, h, all_values, length, 0))
}

fn collect_sort_elements(
  st: Agent,
  this: JsVal,
  length: Int,
  idx: Int,
  acc: List(JsVal),
  undefs: Int,
  hole_mode: HoleMode,
) -> #(#(List(JsVal), Int), Agent) {
  case dense_snapshot(st, this) {
    Some(#(els, proto)) ->
      collect_sort_elements_snapshot(
        st,
        this,
        els,
        proto,
        length,
        idx,
        acc,
        undefs,
        hole_mode,
      )
    None ->
      collect_sort_elements_generic(
        st,
        this,
        length,
        idx,
        acc,
        undefs,
        hole_mode,
      )
  }
}

fn collect_sort_elements_snapshot(
  st: Agent,
  this: JsVal,
  els: JsElements,
  proto: Option(Handle),
  length: Int,
  idx: Int,
  acc: List(JsVal),
  undefs: Int,
  hole_mode: HoleMode,
) -> #(#(List(JsVal), Int), Agent) {
  case idx >= length {
    True -> #(#(list.reverse(acc), undefs), st)
    False ->
      case elements.get_option(els, idx) {
        Some(v) ->
          case classify(v) {
            KUndef ->
              collect_sort_elements_snapshot(
                st,
                this,
                els,
                proto,
                length,
                idx + 1,
                acc,
                undefs + 1,
                hole_mode,
              )
            _ ->
              collect_sort_elements_snapshot(
                st,
                this,
                els,
                proto,
                length,
                idx + 1,
                [v, ..acc],
                undefs,
                hole_mode,
              )
          }
        None -> {
          let #(inherited, st) = hole_is_inherited(st, proto, idx)
          case inherited {
            False ->
              collect_sort_elements_snapshot(
                st,
                this,
                els,
                proto,
                length,
                idx + 1,
                acc,
                case hole_mode {
                  VisitHoles -> undefs + 1
                  SkipHoles -> undefs
                },
                hole_mode,
              )
            True ->
              collect_sort_elements_generic(
                st,
                this,
                length,
                idx,
                acc,
                undefs,
                hole_mode,
              )
          }
        }
      }
  }
}

fn collect_sort_elements_generic(
  st: Agent,
  this: JsVal,
  length: Int,
  idx: Int,
  acc: List(JsVal),
  undefs: Int,
  hole_mode: HoleMode,
) -> #(#(List(JsVal), Int), Agent) {
  case idx >= length {
    True -> #(#(list.reverse(acc), undefs), st)
    False -> {
      let #(maybe_val, st) = get_index_if_present(st, this, idx)
      case maybe_val {
        None ->
          collect_sort_elements_generic(
            st,
            this,
            length,
            idx + 1,
            acc,
            case hole_mode {
              VisitHoles -> undefs + 1
              SkipHoles -> undefs
            },
            hole_mode,
          )
        Some(val) ->
          case classify(val) {
            KUndef ->
              collect_sort_elements_generic(
                st,
                this,
                length,
                idx + 1,
                acc,
                undefs + 1,
                hole_mode,
              )
            _ ->
              collect_sort_elements_generic(
                st,
                this,
                length,
                idx + 1,
                [val, ..acc],
                undefs,
                hole_mode,
              )
          }
      }
    }
  }
}

fn stringify_elements(
  st: Agent,
  values: List(JsVal),
  acc: List(#(String, JsVal)),
) -> #(List(#(String, JsVal)), Agent) {
  case values {
    [] -> #(list.reverse(acc), st)
    [val, ..rest] -> {
      let #(s, st) = rt_val.t_to_string(st, val)
      stringify_elements(st, rest, [#(s, val), ..acc])
    }
  }
}

@external(erlang, "lists", "reverse")
fn reverse_onto(items: List(a), tail: List(a)) -> List(a)

fn merge_sort(
  st: Agent,
  items: List(JsVal),
  comparefn: JsVal,
) -> #(List(JsVal), Agent) {
  case items {
    [] | [_] -> #(items, st)
    _ ->
      merge_all(
        st,
        list.map(items, fn(x) { [x] }),
        rt_call.t_prepare_call(st, comparefn, mk_undefined()),
      )
  }
}

fn merge_all(
  st: Agent,
  runs: List(List(JsVal)),
  comparefn: ElementFn,
) -> #(List(JsVal), Agent) {
  case runs {
    [] -> #([], st)
    [done] -> #(done, st)
    _ -> {
      let #(next, st) = merge_pairs(st, runs, comparefn, [])
      merge_all(st, next, comparefn)
    }
  }
}

fn merge_pairs(
  st: Agent,
  runs: List(List(JsVal)),
  comparefn: ElementFn,
  acc: List(List(JsVal)),
) -> #(List(List(JsVal)), Agent) {
  case runs {
    [] -> #(list.reverse(acc), st)
    [a] -> #(list.reverse([a, ..acc]), st)
    [a, b, ..rest] -> {
      let #(ab, st) = merge_two(st, a, b, comparefn, [])
      merge_pairs(st, rest, comparefn, [ab, ..acc])
    }
  }
}

fn merge_two(
  st: Agent,
  left: List(JsVal),
  right: List(JsVal),
  comparefn: ElementFn,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case left, right {
    [], _ -> #(reverse_onto(acc, right), st)
    _, [] -> #(reverse_onto(acc, left), st)
    [l, ..ls], [r, ..rs] -> {
      let #(res, st) = comparefn(st, [l, r])
      let #(num, st) = rt_val.t_to_number(st, res)
      let cmp = case num {
        JInt(n) -> int.to_float(n)
        JFloat(f) -> f
        JPosInf -> 1.0
        JNegInf -> -1.0
        JNan -> 0.0
      }
      case cmp <=. 0.0 {
        True -> merge_two(st, ls, right, comparefn, [l, ..acc])
        False -> merge_two(st, left, rs, comparefn, [r, ..acc])
      }
    }
  }
}

fn write_sort_result(
  st: Agent,
  h: Handle,
  values: List(JsVal),
  length: Int,
  idx: Int,
) -> Agent {
  let fast = case idx == 0 {
    True -> {
      use _els, len <- with_plain_elements(st, h, length, 0, length)
      #(elements.from_list(values), len, Nil)
    }
    False -> None
  }
  case fast {
    Some(#(Nil, st)) -> st
    None ->
      case values {
        [val, ..rest] -> {
          let st = generic_set_index(st, h, idx, val)
          write_sort_result(st, h, rest, length, idx + 1)
        }
        [] -> delete_trailing(st, h, idx, length)
      }
  }
}

fn delete_trailing(st: Agent, h: Handle, idx: Int, length: Int) -> Agent {
  case idx >= length {
    True -> st
    False ->
      delete_trailing(generic_delete_index(st, h, idx), h, idx + 1, length)
  }
}

fn array_to_sorted(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use comparefn, st <- with_comparefn(st, args)
  use this, _h, length, st <- require_array(st, this)
  use <- within_budget(st, length)
  case comparefn {
    None -> to_sorted_with(st, length, this, sort_values_default)
    Some(cmp) ->
      to_sorted_with(st, length, this, fn(st, defined) {
        merge_sort(st, defined, cmp)
      })
  }
}

fn to_sorted_with(
  st: Agent,
  length: Int,
  this: JsVal,
  sort: fn(Agent, List(JsVal)) -> #(List(JsVal), Agent),
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  let #(#(defined, undefs), st) =
    collect_sort_elements(st, this, length, 0, [], 0, VisitHoles)
  let #(sorted, st) = sort(st, defined)
  let all_values = list.append(sorted, list.repeat(mk_undefined(), undefs))
  alloc_array(st, length, elements.from_list(all_values), array_proto)
}

fn sort_values_default(
  st: Agent,
  defined: List(JsVal),
) -> #(List(JsVal), Agent) {
  let #(pairs, st) = stringify_elements(st, defined, [])
  let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
  #(list.map(sorted, fn(pair) { pair.1 }), st)
}

fn array_splice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use this, h, length, st <- require_array(st, this)
  let #(actual_start, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 0), length, 0)
  let #(#(actual_delete_count, items), st) =
    delete_count(st, args, length, actual_start)
  let item_count = list.length(items)
  let new_length = length - actual_delete_count + item_count
  use <- guard_safe_length(st, new_length)
  let #(species, st) = array_species_create(st, this, actual_delete_count)
  let #(removed_arr, st) = case species {
    None -> {
      let #(removed_elements, st) =
        copy_range(
          st,
          this,
          actual_start,
          0,
          actual_delete_count,
          elements.new(),
        )
      alloc_array(st, actual_delete_count, removed_elements, array_proto)
    }
    Some(target) -> {
      let st =
        copy_range_to_species(
          st,
          this,
          actual_start,
          target,
          0,
          actual_delete_count,
          limits.max_iteration,
        )
      let st = generic_set_length(st, target, actual_delete_count)
      #(mk_object(target), st)
    }
  }
  let shift = item_count - actual_delete_count
  let fast = {
    use els, len <- with_plain_elements(
      st,
      h,
      length,
      actual_start,
      int.max(length, new_length),
    )
    let move_from = actual_start + actual_delete_count
    let els = case shift == 0 {
      True -> els
      False -> elements.move_range(els, move_from, len, shift)
    }
    let els =
      elements.write_list(els, actual_start, items)
      |> elements.truncate(new_length)
    #(els, new_length, Nil)
  }
  case fast {
    Some(#(Nil, st)) -> #(removed_arr, st)
    None -> {
      let st =
        splice_shift(st, h, actual_start, actual_delete_count, length, shift)
      let st = write_list_at(st, h, actual_start, items)
      #(removed_arr, generic_set_length(st, h, new_length))
    }
  }
}

fn splice_shift(
  st: Agent,
  h: Handle,
  start: Int,
  delete_count: Int,
  length: Int,
  shift: Int,
) -> Agent {
  let from_start = start + delete_count
  case shift > 0 {
    True ->
      move_range(
        st,
        h,
        length - 1,
        from_start,
        Descending,
        shift,
        limits.max_iteration,
      )
    False ->
      case shift < 0 {
        True -> {
          let st =
            move_range(
              st,
              h,
              from_start,
              length,
              Ascending,
              shift,
              limits.max_iteration,
            )
          delete_trailing(st, h, length + shift, length)
        }
        False -> st
      }
  }
}

fn array_flat(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  let #(depth, st) = case classify(helpers.first_arg_or_undefined(args)) {
    KUndef -> #(1, st)
    _ -> {
      let #(raw, st) =
        rt_val.t_to_integer_or_infinity(st, helpers.arg_at(args, 0))
      #(int.max(raw, 0), st)
    }
  }
  let #(species, st) = array_species_create(st, this, 0)
  let #(kept_rev, st) = flatten_into(st, this, length, depth, [])
  finish_species_list(st, kept_rev, species)
}

fn finish_species_list(
  st: Agent,
  kept_rev: List(JsVal),
  species: Option(Handle),
) -> #(JsVal, Agent) {
  let kept = list.reverse(kept_rev)
  case species {
    None -> alloc_array_list(st, kept)
    Some(target) -> {
      let count = list.length(kept)
      let st =
        write_species_result(st, target, elements.from_list(kept), count, None)
      #(mk_object(target), st)
    }
  }
}

// returns elements reversed
fn flatten_into(
  st: Agent,
  src: JsVal,
  length: Int,
  depth: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  use <- within_budget(st, length)
  flatten_into_loop(st, src, 0, length, depth, acc)
}

fn flatten_into_loop(
  st: Agent,
  src: JsVal,
  idx: Int,
  length: Int,
  depth: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case idx >= length {
    True -> #(acc, st)
    False -> {
      let #(maybe_elem, st) = get_index_if_present(st, src, idx)
      case maybe_elem {
        None -> flatten_into_loop(st, src, idx + 1, length, depth, acc)
        Some(elem) ->
          case depth > 0 {
            True -> {
              case classify(elem), rt_abstract_ops.is_array(st, elem) {
                KHandle(sub_h), True -> {
                  let #(sub_len, st) = object_length(st, sub_h)
                  let #(new_acc, st) =
                    flatten_into(st, elem, sub_len, depth - 1, acc)
                  flatten_into_loop(st, src, idx + 1, length, depth, new_acc)
                }
                _, _ ->
                  flatten_into_loop(st, src, idx + 1, length, depth, [
                    elem,
                    ..acc
                  ])
              }
            }
            False ->
              flatten_into_loop(st, src, idx + 1, length, depth, [elem, ..acc])
          }
      }
    }
  }
}

fn array_flat_map(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use call, st <- require_callback(st, args)
  use <- within_budget(st, length)
  let #(species, st) = array_species_create(st, this, 0)
  let #(kept_rev, st) = array_flat_map_loop(st, this, 0, length, call, [])
  finish_species_list(st, kept_rev, species)
}

fn array_flat_map_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: ElementFn,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case idx >= length {
    True -> #(acc, st)
    False -> {
      let #(maybe_elem, st) = get_index_if_present(st, arr, idx)
      case maybe_elem {
        None -> array_flat_map_loop(st, arr, idx + 1, length, cb, acc)
        Some(elem) -> {
          let #(mapped, st) = cb(st, [elem, mk_int(idx), arr])
          case classify(mapped), rt_abstract_ops.is_array(st, mapped) {
            KHandle(sub_h), True -> {
              let #(sub_len, st) = object_length(st, sub_h)
              let #(new_acc, st) = flatten_into(st, mapped, sub_len, 0, acc)
              array_flat_map_loop(st, arr, idx + 1, length, cb, new_acc)
            }
            _, _ ->
              array_flat_map_loop(st, arr, idx + 1, length, cb, [mapped, ..acc])
          }
        }
      }
    }
  }
}

fn array_copy_within(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use this, h, length, st <- require_array(st, this)
  let #(target, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 0), length, 0)
  let #(from, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 1), length, 0)
  let #(final, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 2), length, length)
  let count = int.min(final - from, length - target)
  use <- within_budget(st, count)
  case count <= 0 {
    True -> #(this, st)
    False -> {
      let fast = {
        use els, len <- with_plain_elements(st, h, length, 0, length)
        #(elements.copy_within(els, from, target, count), len, Nil)
      }
      case fast {
        Some(#(Nil, st)) -> #(this, st)
        None ->
          case from < target && target < from + count {
            True -> #(
              this,
              copy_within_step(
                st,
                h,
                from + count - 1,
                target + count - 1,
                Descending,
                count,
              ),
            )
            False -> #(
              this,
              copy_within_step(st, h, from, target, Ascending, count),
            )
          }
      }
    }
  }
}

fn copy_within_step(
  st: Agent,
  h: Handle,
  from: Int,
  to: Int,
  dir: Direction,
  remaining: Int,
) -> Agent {
  case remaining <= 0 {
    True -> st
    False -> {
      let step = step_of(dir)
      let #(has_from, st) = generic_has_op(st, h, from)
      let st = case has_from {
        True -> {
          let #(val, st) = generic_get(st, h, from)
          generic_set_index(st, h, to, val)
        }
        False -> generic_delete_index(st, h, to)
      }
      copy_within_step(st, h, from + step, to + step, dir, remaining - 1)
    }
  }
}

type FromTarget {
  FreshArray(acc: List(JsVal))
  Constructed(target: Handle)
}

fn from_target(
  st: Agent,
  ctor: JsVal,
  ctor_args: List(JsVal),
) -> #(FromTarget, Agent) {
  case classify(ctor) {
    KHandle(h) if h != st.realm.array.constructor ->
      case rt_call.is_constructor(st, ctor) {
        True -> {
          let #(a, st) = rt_call.t_construct(st, ctor, ctor_args, ctor)
          #(Constructed(a), st)
        }
        False -> #(FreshArray([]), st)
      }
    _ -> #(FreshArray([]), st)
  }
}

fn from_put(
  st: Agent,
  t: FromTarget,
  idx: Int,
  v: JsVal,
) -> #(FromTarget, Agent) {
  case t {
    FreshArray(acc) -> #(FreshArray([v, ..acc]), st)
    Constructed(target) -> #(t, write_species_element(st, target, idx, v))
  }
}

fn from_finish(st: Agent, t: FromTarget, len: Int) -> #(JsVal, Agent) {
  case t {
    FreshArray(acc) -> {
      let array_proto = st.realm.array.prototype
      alloc_array(st, len, elements.from_list(list.reverse(acc)), array_proto)
    }
    Constructed(target) -> {
      let st = generic_set_length(st, target, len)
      #(mk_object(target), st)
    }
  }
}

fn array_from(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(items_val, map_fn, this_arg) = helpers.three_args_or_undefined(args)
  case classify(map_fn) {
    KUndef -> array_from_array_like(st, this, items_val, None, this_arg)
    _ -> {
      use mf <- helpers.require_callable(st, map_fn, fn() {
        not_a_function(st, map_fn)
      })
      array_from_array_like(st, this, items_val, Some(mf), this_arg)
    }
  }
}

fn array_from_array_like(
  st: Agent,
  ctor: JsVal,
  items: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
) -> #(JsVal, Agent) {
  case classify(items) {
    KNull | KUndef -> {
      rt_val.t_throw_type_error(
        st,
        "Cannot create array from " <> rt_val.type_of(st, items),
      )
    }
    _ -> {
      // a plain array under the intrinsic constructor is a copy
      let plain = case classify(ctor), map_fn {
        KHandle(h), None if h == st.realm.array.constructor ->
          rt_lang.array_spread(st, items)
        _, _ -> rt_lang.SpreadMiss
      }
      use <- lazy_guard_spread(plain, fn(values) {
        alloc_array_list(st, values)
      })
      let #(iter_method, st) =
        rt_obj.t_get_prop(st, items, SymbolKey(symbol_iterator))
      case classify(iter_method) {
        KUndef | KNull -> {
          let #(length, st) = rt_abstract_ops.length_of_array_like(st, items)
          use <- within_budget(st, length)
          let #(target, st) = from_target(st, ctor, [mk_int(length)])
          array_from_loop(st, items, 0, length, map_fn, this_arg, target)
        }
        _ -> {
          use m <- helpers.require_callable(st, iter_method, fn() {
            not_a_function(st, iter_method)
          })
          let #(target, st) = from_target(st, ctor, [])
          array_from_iterator(st, items, m, map_fn, this_arg, target)
        }
      }
    }
  }
}

fn array_from_iterator(
  st: Agent,
  items: JsVal,
  iter_method: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  target: FromTarget,
) -> #(JsVal, Agent) {
  let #(rec, st) =
    iter_protocol.get_iterator_from_method(st, items, iter_method)
  array_from_iterator_loop(st, rec, map_fn, this_arg, 0, target)
}

fn array_from_iterator_loop(
  st: Agent,
  rec: iter_protocol.IteratorRecord,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  k: Int,
  target: FromTarget,
) -> #(JsVal, Agent) {
  let #(step, st) = iter_protocol.iterator_step_value(st, rec)
  case step {
    None -> from_finish(st, target, k)
    Some(item) -> {
      let #(mapped, st) = case map_fn {
        Some(mf) -> {
          use mapped, st <- iter_protocol.or_close(st, rec.iterator, fn(st) {
            rt_call.t_call(st, mf, this_arg, [item, mk_int(k)])
          })
          #(mapped, st)
        }
        None -> #(item, st)
      }
      let #(target, st) = case target {
        FreshArray(_) -> from_put(st, target, k, mapped)
        Constructed(t) -> {
          use _undef, st <- iter_protocol.or_close(st, rec.iterator, fn(st) {
            #(mk_undefined(), write_species_element(st, t, k, mapped))
          })
          #(target, st)
        }
      }
      array_from_iterator_loop(st, rec, map_fn, this_arg, k + 1, target)
    }
  }
}

fn array_from_loop(
  st: Agent,
  items: JsVal,
  idx: Int,
  length: Int,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  target: FromTarget,
) -> #(JsVal, Agent) {
  case idx >= length {
    True -> from_finish(st, target, length)
    False -> {
      let #(elem, st) = rt_abstract_ops.get_index(st, items, idx)
      let #(mapped, st) = case map_fn {
        None -> #(elem, st)
        Some(mf) -> rt_call.t_call(st, mf, this_arg, [elem, mk_int(idx)])
      }
      let #(target, st) = from_put(st, target, idx, mapped)
      array_from_loop(st, items, idx + 1, length, map_fn, this_arg, target)
    }
  }
}

fn array_of(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let len = list.length(args)
  let #(target, st) = from_target(st, this, [mk_int(len)])
  let #(target, st) =
    list.index_fold(args, #(target, st), fn(acc, item, k) {
      let #(target, st) = acc
      from_put(st, target, k, item)
    })
  from_finish(st, target, len)
}

fn array_to_spliced(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use this, _h, length, st <- require_array(st, this)
  let #(actual_start, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 0), length, 0)
  let #(#(actual_skip_count, items), st) =
    delete_count(st, args, length, actual_start)
  let item_count = list.length(items)
  let new_len = length + item_count - actual_skip_count
  use <- guard_safe_length(st, new_len)
  let #(new_elements, st) =
    copy_range_dense(st, this, 0, 0, actual_start, elements.new())
  let new_elements = elements.write_list(new_elements, actual_start, items)
  let src_from = actual_start + actual_skip_count
  let dst_from = actual_start + item_count
  let remaining = length - src_from
  let #(new_elements, st) =
    copy_range_dense(st, this, src_from, dst_from, remaining, new_elements)
  alloc_array(st, new_len, new_elements, array_proto)
}

fn array_with(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use this, _h, length, st <- require_array(st, this)
  let #(raw, st) = rt_val.t_to_integer_or_infinity(st, helpers.arg_at(args, 0))
  let actual_index = case raw < 0 {
    True -> length + raw
    False -> raw
  }
  use <- bool.lazy_guard(length > max_array_length, fn() {
    rt_val.t_throw_range_error(st, "Invalid array length")
  })
  case actual_index < 0 || actual_index >= length {
    True -> rt_val.t_throw_range_error(st, "Invalid index")
    False -> {
      let replacement = case args {
        [_, r, ..] -> r
        _ -> mk_undefined()
      }
      let #(new_elements, st) =
        copy_range_dense(st, this, 0, 0, actual_index, elements.new())
      let new_elements = elements.set(new_elements, actual_index, replacement)
      let #(new_elements, st) =
        copy_range_dense(
          st,
          this,
          actual_index + 1,
          actual_index + 1,
          length - actual_index - 1,
          new_elements,
        )
      alloc_array(st, length, new_elements, array_proto)
    }
  }
}

fn array_to_reversed(
  st: Agent,
  this: JsVal,
  _args: List(JsVal),
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use this, _h, length, st <- require_array(st, this)
  use <- within_budget(st, length)
  let #(reversed, st) = collect_elements_descending(st, this, length - 1, [])
  alloc_array(st, length, elements.from_list(reversed), array_proto)
}

fn collect_elements_descending(
  st: Agent,
  this: JsVal,
  idx: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case idx < 0 {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(val, st) = rt_abstract_ops.get_index(st, this, idx)
      collect_elements_descending(st, this, idx - 1, [val, ..acc])
    }
  }
}

fn array_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use array, h, st <- require_object(st, this)
  let #(func, st) =
    rt_obj.t_get_prop(st, mk_object(h), StringKey(Named("join")))
  case rt_val.is_callable(st, func) {
    True -> rt_call.t_call(st, func, array, [])
    False -> b_object.dispatch(st, ObjectPrototypeToString, array, [])
  }
}

fn array_to_locale_string(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use this, _h, length, st <- require_array(st, this)
  use <- within_budget(st, length)
  array_to_locale_string_loop(
    st,
    this,
    0,
    length,
    helpers.first_arg_or_undefined(args),
    helpers.arg_at(args, 1),
    [],
  )
}

fn array_to_locale_string_loop(
  st: Agent,
  this: JsVal,
  idx: Int,
  length: Int,
  locales_v: JsVal,
  options_v: JsVal,
  acc: List(String),
) -> #(JsVal, Agent) {
  case idx >= length {
    True ->
      case limits.join(list.reverse(acc), ",") {
        Ok(result) -> #(mk_string(result), st)
        Error(Nil) -> rt_val.t_throw_range_error(st, "Invalid string length")
      }
    False -> {
      let #(elem, st) = rt_abstract_ops.get_index(st, this, idx)
      case classify(elem) {
        KUndef | KNull ->
          array_to_locale_string_loop(
            st,
            this,
            idx + 1,
            length,
            locales_v,
            options_v,
            ["", ..acc],
          )
        _ -> {
          let #(method, st) =
            rt_obj.t_get_prop(st, elem, StringKey(Named("toLocaleString")))
          use method <- helpers.require_callable(st, method, fn() {
            not_a_function(st, method)
          })
          let #(locale_val, st) =
            rt_call.t_call(st, method, elem, [locales_v, options_v])
          let #(s, st) = rt_val.t_to_string(st, locale_val)
          array_to_locale_string_loop(
            st,
            this,
            idx + 1,
            length,
            locales_v,
            options_v,
            [s, ..acc],
          )
        }
      }
    }
  }
}

// toobject only, must not read length
fn create_array_iterator(
  st: Agent,
  this: JsVal,
  kind: types.ArrayIterKind,
) -> #(JsVal, Agent) {
  use _this, h, st <- require_object(st, this)
  let #(iter_h, st) =
    realm_ops.alloc_object(
      st,
      ArrayIterator(target: h, index: 0, kind:),
      st.realm.array_iter_proto,
    )
  #(mk_object(iter_h), st)
}

fn array_keys(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  create_array_iterator(st, this, ArrayIterKeys)
}

fn array_values(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  create_array_iterator(st, this, ArrayIterValues)
}

fn array_entries(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  create_array_iterator(st, this, ArrayIterEntries)
}

// every element of a plain hole-free array whose iteration observes nothing
fn lazy_guard_spread(
  plain: rt_lang.PlainSpread,
  then: fn(List(JsVal)) -> a,
  otherwise: fn() -> a,
) -> a {
  case plain {
    rt_lang.Spread(values) -> then(values)
    rt_lang.SpreadMiss -> otherwise()
  }
}
