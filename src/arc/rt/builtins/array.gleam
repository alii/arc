//// `rt_builtins/array` — Array constructor + Array.prototype (SPEC §7.M6).
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).** Errors RAISE via
//// `rt_val.t_throw_type_error/t_throw_range_error` (D7 — never `Result`).
////
//// `init` builds Array.prototype + Array (constructor) via `common.init_type`;
//// `dispatch` routes every `ArrayNative` token to its impl. Shared iteration
//// / element helpers (SkipHoles/VisitHoles, `iterate_array`, `fold_array`,
//// `move_range`, JsElements ops) are local and follow a fast-path bulk-mutate
//// pattern: one heap read, one JsElements transform, one heap write.

import arc/rt/builtins/array_from_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/object as object_builtin
import arc/rt/call as rt_call
import arc/rt/elements
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type ArrayNative, type BuiltinPair, type Handle, type JsElements,
  type JsVal, type Property, type PropertyKey, ArrayConstructor, ArrayFrom,
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
  Index, JFloat, JInt, JNan, JNegInf, JPosInf, KHandle, KNull, KNum, KStr,
  KUndef, Named, NoElements, ObjectPrototypeToString, Ordinary, ParsedDesc,
  ProxyObj, ReturnThis, SObject, StringKey, StringObj, SymbolKey, classify,
  index_key, key_display_string, max_array_length, mk_bool, mk_number, mk_object,
  mk_string, mk_undefined, symbol_is_concat_spreadable, symbol_iterator,
  symbol_species, symbol_unscopables,
} as rt_types
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// V8's message for the iteration-budget RangeError and for a rejected
/// array-length Set. Shared with the string-size cap.
const iteration_budget_msg = "Invalid array length"

/// V8's standard ToObject failure message.
const cannot_convert = "Cannot convert undefined or null to object"

/// A JS integer as `JsVal` — port of arc `value.from_int`.
fn from_int(n: Int) -> JsVal {
  mk_number(JInt(n))
}

// ────────────────────────────── init / dispatch ─────────────────────────────

/// Set up Array.prototype and the Array constructor (ES2024 §23.1).
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
  // §23.1.3 "The Array prototype object … is an Array exotic object" with a
  // "length" property whose initial value is +0.
  let st =
    rt_store.t_cell_update(st, bt.prototype, fn(slot) {
      case slot {
        SObject(..) as slot -> SObject(..slot, kind: ArrayObj(0))
        other -> other
      }
    })
  // §23.1.3.40 Array.prototype[@@iterator] — the SAME function object as
  // Array.prototype.values, not a fresh one.
  let assert Ok(#(_, DataProperty(value: values_fn, ..))) =
    list.find(proto_methods, fn(entry) { entry.0 == "values" })
  let #(values_prop, st) = common.builtin_property(st, values_fn)
  let st =
    common.add_symbol_property(st, bt.prototype, symbol_iterator, values_prop)
  // §23.1.3.41 Array.prototype[@@unscopables]: a null-prototype object whose
  // true-valued properties hide the listed methods from `with` scoping. Each
  // entry {W:T,E:T,C:T}; the @@unscopables property itself {W:F,E:F,C:T}.
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
          DataProperty(
            value: mk_bool(True),
            writable: True,
            enumerable: True,
            configurable: True,
            seq:,
          ),
        ),
        st,
      )
    })
  let #(unscopables_ref, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: None,
        props: unscopable_props,
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  let st =
    common.add_symbol_property(
      st,
      bt.prototype,
      symbol_unscopables,
      DataProperty(
        value: mk_object(unscopables_ref),
        writable: False,
        enumerable: False,
        configurable: True,
        seq:,
      ),
    )
  // §23.1.2.5 Array[@@species] — a getter returning `this`.
  let st = common.add_species_accessor(st, fn_proto, bt.constructor, ReturnThis)
  #(bt, st)
}

/// Route an `ArrayNative` token to its implementation.
pub fn dispatch(
  st: Agent,
  native: ArrayNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    ArrayConstructor -> construct(st, args)
    ArrayIsArray -> is_array(st, args)
    ArrayFrom -> array_from(st, args)
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
    ArrayOf -> array_of(st, args)
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

// ──────────────────────── Array constructor / Array.isArray ──────────────────

/// §23.1.1.1 Array ( ...values ). Called as function or constructor (identical).
/// NewTarget / subclassing not yet supported — proto is always
/// %Array.prototype%.
fn construct(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  case args {
    // numberOfArgs = 0 → ArrayCreate(0, proto).
    [] -> alloc_array(st, 0, elements.new(), array_proto)
    // numberOfArgs = 1 and the arg IS a Number → length path (§23.1.1.1 step 5).
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
        // Single non-Number arg → items path.
        _ -> alloc_array(st, 1, elements.from_list([only]), array_proto)
      }
    // numberOfArgs ≥ 2 → items path.
    _ -> {
      let count = list.length(args)
      alloc_array(st, count, elements.from_list(args), array_proto)
    }
  }
}

/// §23.1.1.1 step 5.b: `Some(intLen)` iff `f` is integral in [0, 2^32-1].
/// SameValueZero(0, -0) is true so `Array(-0)` is length-0 (`+. 0.0` normalizes).
fn array_length_of_float(f: Float) -> Option(Int) {
  case rt_val.integral_int(f +. 0.0) {
    Some(n) if n >= 0 && n <= max_array_length -> Some(n)
    _ -> None
  }
}

/// §23.1.2.1 Array.isArray(arg): Return ? IsArray(arg).
fn is_array(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(b, st) = try_is_array(st, helpers.first_arg_or_undefined(args))
  #(mk_bool(b), st)
}

/// §7.2.2 IsArray — pierces Proxy exotic objects to their [[ProxyTarget]]
/// (step 3) and throws TypeError on a revoked proxy (step 3.a).
fn try_is_array(st: Agent, v: JsVal) -> #(Bool, Agent) {
  case classify(v) {
    KHandle(h) -> #(is_array_handle(st, h), st)
    _ -> #(False, st)
  }
}

fn is_array_handle(st: Agent, h: Handle) -> Bool {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: ArrayObj(_), ..) -> True
    SObject(kind: ProxyObj(target:, revoked:, ..), ..) ->
      case revoked {
        True ->
          rt_val.t_throw_type_error(
            st,
            "Cannot perform 'IsArray' on a proxy that has been revoked",
          )
        False -> is_array_handle(st, target)
      }
    _ -> False
  }
}

/// ArrayCreate + element population as one alloc.
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

/// Allocate a plain dense array from a value list.
fn alloc_array_list(st: Agent, values: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  alloc_array(st, list.length(values), elements.from_list(values), array_proto)
}

// ──────────────────── shared prologue (ToObject / LengthOfArrayLike) ─────────

/// ToObject (§7.1.18) — hands `cont` the normalized `this` (spec's O) and its
/// handle. A real wrapper IS allocated for primitives so observable uses of it
/// (fill/copyWithin's return, callback third argument) see the wrapper object.
/// Reads NO properties (§23.1.5.1 CreateArrayIterator must not fire
/// Get("length")).
fn to_object_ref(
  st: Agent,
  this: JsVal,
  cont: fn(Agent, JsVal, Handle) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case classify(this) {
    KUndef | KNull -> rt_val.t_throw_type_error(st, cannot_convert)
    _ -> {
      let #(h, st) = rt_val.t_to_object(st, this)
      cont(st, mk_object(h), h)
    }
  }
}

/// LengthOfArrayLike (§7.3.18) — `? ToLength(? Get(O, "length"))` clamped to
/// [0, 2^53-1]. Getter/proxy trap and ToLength's valueOf may throw (D7).
fn require_length(
  st: Agent,
  ref: Handle,
  cont: fn(Agent, Int) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(length, st) = object_length(st, ref)
  cont(st, length)
}

/// ToObject then LengthOfArrayLike — the "intentionally generic" §23.1.3
/// prologue. Captures `length` once, passes `(st, O, ref, length)` to `cont`.
fn require_array(
  st: Agent,
  this: JsVal,
  cont: fn(Agent, JsVal, Handle, Int) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use st, obj, ref <- to_object_ref(st, this)
  use st, length <- require_length(st, ref)
  cont(st, obj, ref, length)
}

/// §7.3.18 LengthOfArrayLike with slot fast paths. Array/String kinds answer
/// from [[ArrayLength]]/[[StringData]] (non-configurable, unobservable
/// shortcut); everything else takes the full [[Get]]("length") + ToLength.
fn object_length(st: Agent, ref: Handle) -> #(Int, Agent) {
  case rt_store.t_cell_get(st, ref) {
    SObject(kind: ArrayObj(length:), ..) -> #(length, st)
    SObject(kind: StringObj(value: s), ..) -> #(string.length(s), st)
    SObject(props:, ..) -> length_of_properties(st, ref, props)
    rt_types.SShapedObject(..) as s ->
      case rt_obj.as_sobject(st, s) {
        SObject(props:, ..) -> length_of_properties(st, ref, props)
        _ -> #(0, st)
      }
    _ -> #(0, st)
  }
}

fn length_of_properties(
  st: Agent,
  ref: Handle,
  props: Dict(PropertyKey, Property),
) -> #(Int, Agent) {
  case dict.get(props, Named("length")) {
    // Own data property fast path — ToLength may still call valueOf on an
    // object-valued length.
    Ok(DataProperty(value: len_val, ..)) -> rt_val.t_to_length(st, len_val)
    // Accessor or missing: full [[Get]] then ToLength.
    _ -> {
      let #(len_val, st) =
        rt_obj.t_get_prop(st, mk_object(ref), StringKey(Named("length")))
      rt_val.t_to_length(st, len_val)
    }
  }
}

/// IsCallable check + argument extraction shared by the callback-driven
/// Array.prototype methods (§7.2.3 IsCallable → TypeError).
fn require_callback(
  st: Agent,
  args: List(JsVal),
  cont: fn(Agent, JsVal, JsVal) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(cb, this_arg) = helpers.two_args_or_undefined(args)
  use cb <- helpers.require_callable(st, cb, fn() { not_a_function(st, cb) })
  cont(st, cb, this_arg)
}

fn not_a_function(st: Agent, v: JsVal) -> String {
  let #(ty, _) = rt_val.t_type_of(st, v)
  ty <> " is not a function"
}

/// §23.1.3.x common relative-index step: `ToIntegerOrInfinity(val)`, then
/// `-∞→0`, `<0→max(len+rel,0)`, `≥0→min(rel,len)`. `undefined` → `default`.
fn relative_index(
  st: Agent,
  val: JsVal,
  len: Int,
  default: Int,
) -> #(Int, Agent) {
  case classify(val) {
    KUndef -> #(default, st)
    _ -> {
      let #(raw, st) = rt_val.t_to_integer_or_infinity(st, val)
      let k = case raw < 0 {
        True -> int.max(len + raw, 0)
        False -> int.min(raw, len)
      }
      #(k, st)
    }
  }
}

/// Shared steps 7-10 of splice / step 7 of toSpliced: `actualDeleteCount` +
/// trailing items, determined by argument COUNT.
fn try_delete_count(
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

/// §23.1.3.22 step 4 / §23.1.3.33 step 4a / §23.1.3.31 step 11: throw
/// TypeError when `n > 2^53 - 1`. CPS so it composes with `use <-`.
fn guard_safe_length(
  st: Agent,
  n: Int,
  cont: fn() -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case n > rt_val.max_safe_integer {
    True ->
      rt_val.t_throw_type_error(st, "Array length exceeds maximum safe integer")
    False -> cont()
  }
}

// ────────────────── generic Set / Delete / Get / Has by key ─────────────────

/// §7.3.4 Set(O, P, V, true) — every mutating Array.prototype method uses
/// Throw=true.
fn generic_set(st: Agent, ref: Handle, key: PropertyKey, val: JsVal) -> Agent {
  let #(ok, st) = rt_obj.t_set_prop(st, mk_object(ref), StringKey(key), val)
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot assign to read only property '"
          <> key_display_string(key)
          <> "' of object",
      )
  }
}

/// Set(O, ! ToString(𝔽(idx)), V, true).
fn generic_set_index(st: Agent, ref: Handle, idx: Int, val: JsVal) -> Agent {
  generic_set(st, ref, index_key(idx), val)
}

/// Set(O, "length", 𝔽(len), true).
fn generic_set_length(st: Agent, ref: Handle, len: Int) -> Agent {
  generic_set(st, ref, Named("length"), from_int(len))
}

/// §7.3.9 DeletePropertyOrThrow(O, P).
fn generic_delete(st: Agent, ref: Handle, key: PropertyKey) -> Agent {
  let #(ok, st) = rt_obj.t_delete_prop(st, ref, StringKey(key))
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot delete property '" <> key_display_string(key) <> "' of object",
      )
  }
}

fn generic_delete_index(st: Agent, ref: Handle, idx: Int) -> Agent {
  generic_delete(st, ref, index_key(idx))
}

/// §7.3.11 HasProperty(O, ! ToString(𝔽(idx))). Trap-aware via `t_has_prop`.
fn generic_has_op(st: Agent, ref: Handle, idx: Int) -> #(Bool, Agent) {
  rt_obj.t_has_prop(st, mk_object(ref), StringKey(index_key(idx)))
}

/// §7.3.2 Get(O, ! ToString(𝔽(idx))).
fn generic_get(st: Agent, ref: Handle, idx: Int) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, mk_object(ref), StringKey(index_key(idx)))
}

/// §7.3.2 Get on any array-like receiver (including primitive strings) by
/// integer index. Goes through `t_get_prop`, which auto-boxes primitives.
fn get_index(st: Agent, this: JsVal, idx: Int) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, this, StringKey(index_key(idx)))
}

/// Fused HasProperty + Get by integer index. `Some(val)` when present (own or
/// inherited); `None` on a hole. arc's own-index fast path is subsumed by
/// `t_has_prop`/`t_get_prop`'s own-first walk — spec-equivalent, one extra
/// heap read on the hit path vs arc.
fn get_index_if_present(
  st: Agent,
  this: JsVal,
  idx: Int,
) -> #(Option(JsVal), Agent) {
  let #(has, st) = rt_obj.t_has_prop(st, this, StringKey(index_key(idx)))
  case has {
    False -> #(None, st)
    True -> {
      let #(v, st) = get_index(st, this, idx)
      #(Some(v), st)
    }
  }
}

// ───────────── snapshot / bulk-mutate fast-path helpers ─────────────────────

/// Snapshot a plain Array's element storage for bulk iteration. `Some` only
/// when `this` is an ArrayObj whose props dict has NO Index keys — reading it
/// then can't invoke user code.
fn dense_snapshot(
  st: Agent,
  this: JsVal,
) -> Option(#(JsElements, Option(Handle))) {
  case classify(this) {
    KHandle(ref) ->
      case rt_store.t_cell_get(st, ref) {
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

/// True when `props` carries any Index-keyed entry (accessor / attribute
/// override) — forces the generic per-element path.
fn properties_have_index_keys(props: Dict(PropertyKey, Property)) -> Bool {
  !dict.is_empty(props)
  && list.any(dict.keys(props), fn(key) {
    case key {
      Index(_) -> True
      _ -> False
    }
  })
}

/// True when any prototype-chain object carries an Index property (dict or
/// elements) — makes holes / beyond-length writes observable.
fn proto_chain_has_index_keys(st: Agent, proto: Option(Handle)) -> Bool {
  case proto {
    None -> False
    Some(proto_ref) ->
      case rt_store.t_cell_get(st, proto_ref) {
        SObject(kind: ProxyObj(..), ..) -> True
        SObject(kind: StringObj(value: s), ..) if s != "" -> True
        SObject(props:, elements: els, proto:, ..) ->
          !elements.is_empty(els)
          || properties_have_index_keys(props)
          || proto_chain_has_index_keys(st, proto)
        // Shaped: no elements, Named-only keys → recurse.
        rt_types.SShapedObject(proto:, ..) ->
          proto_chain_has_index_keys(st, proto)
        _ -> False
      }
  }
}

/// Bulk in-place mutator fast path — one heap read, one `transform` on the
/// #(elements, length), one heap write. `None` when ineligible (see arc doc).
fn try_elements_fast_path(
  st: Agent,
  ref: Handle,
  expected_len: Int,
  transform: fn(JsElements, Int) -> #(JsElements, Int, payload),
) -> Option(#(payload, Agent)) {
  case rt_store.t_cell_get(st, ref) {
    SObject(
      kind: ArrayObj(length:),
      props:,
      elements: els,
      proto:,
      extensible: True,
      ..,
    ) as slot -> {
      let length_writable = case dict.get(props, Named("length")) {
        Ok(DataProperty(writable:, ..)) -> writable
        _ -> True
      }
      let eligible =
        length == expected_len
        && length_writable
        && !properties_have_index_keys(props)
        && !proto_chain_has_index_keys(st, proto)
      case eligible {
        False -> None
        True -> {
          let #(els, new_length, payload) = transform(els, length)
          let st =
            rt_store.t_cell_set(
              st,
              ref,
              SObject(..slot, kind: ArrayObj(new_length), elements: els),
            )
          Some(#(payload, st))
        }
      }
    }
    _ -> None
  }
}

/// Push-specific fast path — checks only the target index range instead of
/// scanning every proto-chain dict key.
fn try_push_fast_path(
  st: Agent,
  ref: Handle,
  expected_len: Int,
  args: List(JsVal),
) -> Option(#(Int, Agent)) {
  case rt_store.t_cell_get(st, ref) {
    SObject(
      kind: ArrayObj(length:),
      props:,
      elements: els,
      proto:,
      extensible: True,
      ..,
    ) as slot -> {
      let arg_count = list.length(args)
      let length_writable = case dict.get(props, Named("length")) {
        Ok(DataProperty(writable:, ..)) -> writable
        _ -> True
      }
      let eligible =
        length == expected_len
        && length_writable
        && !dict_has_index_in_range(props, length, arg_count)
        && !proto_chain_has_index_in_range(st, proto, length, arg_count)
      case eligible {
        False -> None
        True -> {
          let new_length = length + arg_count
          let st =
            rt_store.t_cell_set(
              st,
              ref,
              SObject(
                ..slot,
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

fn dict_has_index_in_range(
  props: Dict(PropertyKey, Property),
  start: Int,
  count: Int,
) -> Bool {
  !dict.is_empty(props) && dict_index_in_range_loop(props, start, start + count)
}

fn dict_index_in_range_loop(
  props: Dict(PropertyKey, Property),
  idx: Int,
  end: Int,
) -> Bool {
  case idx >= end {
    True -> False
    False ->
      case dict.get(props, Index(idx)) {
        Ok(_) -> True
        Error(Nil) -> dict_index_in_range_loop(props, idx + 1, end)
      }
  }
}

fn proto_chain_has_index_in_range(
  st: Agent,
  proto: Option(Handle),
  start: Int,
  count: Int,
) -> Bool {
  case proto {
    None -> False
    Some(proto_ref) ->
      case rt_store.t_cell_get(st, proto_ref) {
        SObject(kind: ProxyObj(..), ..) -> True
        SObject(kind: StringObj(value: s), ..) if s != "" -> True
        SObject(props:, elements: proto_els, proto:, ..) ->
          elements_has_in_range(proto_els, start, count)
          || dict_has_index_in_range(props, start, count)
          || proto_chain_has_index_in_range(st, proto, start, count)
        // Shaped: no elements, Named-only keys → recurse.
        rt_types.SShapedObject(proto:, ..) ->
          proto_chain_has_index_in_range(st, proto, start, count)
        _ -> False
      }
  }
}

fn elements_has_in_range(els: JsElements, start: Int, count: Int) -> Bool {
  !elements.is_empty(els) && elements_in_range_loop(els, start, start + count)
}

fn elements_in_range_loop(els: JsElements, idx: Int, end: Int) -> Bool {
  case idx >= end {
    True -> False
    False ->
      case elements.has(els, idx) {
        True -> True
        False -> elements_in_range_loop(els, idx + 1, end)
      }
  }
}

/// True when a hole at `idx` is shadowed by an inherited property — reading it
/// would invoke [[Get]] on the prototype chain (possibly user code). A proxy
/// on the chain answers True (its traps are user code) — arc's
/// `has_property |> or_when_proxy(True)`.
fn hole_is_inherited(
  st: Agent,
  proto: Option(Handle),
  idx: Int,
) -> #(Bool, Agent) {
  case proto {
    None -> #(False, st)
    Some(proto_ref) ->
      case rt_store.t_cell_get(st, proto_ref) {
        SObject(kind: ProxyObj(..), ..) -> #(True, st)
        _ ->
          rt_obj.t_has_prop(st, mk_object(proto_ref), StringKey(index_key(idx)))
      }
  }
}

// ──────────────────────────── Array.prototype.join ───────────────────────────

/// §23.1.3.18 Array.prototype.join(separator).
fn array_join(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  let sep_val = case args {
    [v, ..] ->
      case classify(v) {
        KUndef -> mk_string(",")
        _ -> v
      }
    [] -> mk_string(",")
  }
  let #(separator, st) = rt_val.t_to_string(st, sep_val)
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  let #(joined, st) = join_elements(st, this, 0, length, separator, [])
  #(mk_string(joined), st)
}

/// Bounded terminal step for join: refuse to materialize a result over
/// `limits.max_string_bytes` (V8's "Invalid string length" RangeError).
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

/// §23.1.3.18 step 6: fast path when a snapshot is safe, else generic
/// per-element Get.
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
            // undefined / null → "".
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
            // Object ToString may run user code — bail to generic from here.
            KHandle(_) ->
              join_elements_generic(st, this, idx, length, separator, acc)
            // Primitive — ToString runs no user code.
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
      let #(v, st) = get_index(st, this, idx)
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

// ───────────────────────── Array.prototype.push / pop ───────────────────────

/// §23.1.3.22 Array.prototype.push(...items).
fn array_push(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, _this, ref, length <- require_array(st, this)
  use <- guard_safe_length(st, length + list.length(args))
  let fast = case args {
    [] -> None
    _ ->
      case length + list.length(args) > max_array_length {
        True -> None
        False -> try_push_fast_path(st, ref, length, args)
      }
  }
  case fast {
    Some(#(new_length, st)) -> #(from_int(new_length), st)
    None -> {
      let #(new_length, st) = push_generic(st, ref, length, args)
      #(from_int(new_length), st)
    }
  }
}

/// §23.1.3.22 steps 5-7.
fn push_generic(
  st: Agent,
  ref: Handle,
  length: Int,
  args: List(JsVal),
) -> #(Int, Agent) {
  case args {
    [] -> {
      // §10.4.2.4: real Array + len ≥ 2^32 → RangeError on Set("length").
      let is_real_array = case rt_store.t_cell_get(st, ref) {
        SObject(kind: ArrayObj(..), ..) -> True
        _ -> False
      }
      use <- bool.lazy_guard(is_real_array && length > max_array_length, fn() {
        rt_val.t_throw_range_error(st, "Invalid array length")
      })
      let st = generic_set_length(st, ref, length)
      #(length, st)
    }
    [val, ..rest] -> {
      let st = generic_set_index(st, ref, length, val)
      push_generic(st, ref, length + 1, rest)
    }
  }
}

/// §23.1.3.21 Array.prototype.pop().
fn array_pop(st: Agent, this: JsVal, _args: List(JsVal)) -> #(JsVal, Agent) {
  use st, _this, ref, length <- require_array(st, this)
  case length == 0 {
    True -> #(mk_undefined(), generic_set_length(st, ref, 0))
    False -> {
      let new_len = length - 1
      let fast = {
        use els, len <- try_elements_fast_path(st, ref, length)
        #(elements.truncate(els, len - 1), len - 1, elements.get(els, len - 1))
      }
      case fast {
        Some(#(val, st)) -> #(val, st)
        None -> {
          let #(val, st) = generic_get(st, ref, new_len)
          let st = generic_delete_index(st, ref, new_len)
          #(val, generic_set_length(st, ref, new_len))
        }
      }
    }
  }
}

// ─────────────────────── Array.prototype.shift / unshift ────────────────────

/// §23.1.3.25 Array.prototype.shift().
fn array_shift(st: Agent, this: JsVal, _args: List(JsVal)) -> #(JsVal, Agent) {
  use st, _this, ref, length <- require_array(st, this)
  case length == 0 {
    True -> #(mk_undefined(), generic_set_length(st, ref, 0))
    False -> {
      let fast = {
        use els, len <- try_elements_fast_path(st, ref, length)
        let first = elements.get(els, 0)
        let els =
          elements.move_range(els, 1, len, -1) |> elements.truncate(len - 1)
        #(els, len - 1, first)
      }
      case fast {
        Some(#(first, st)) -> #(first, st)
        None -> {
          let #(val, st) = generic_get(st, ref, 0)
          let st =
            move_range(st, ref, 1, length, Ascending, -1, limits.max_iteration)
          let st = generic_delete_index(st, ref, length - 1)
          #(val, generic_set_length(st, ref, length - 1))
        }
      }
    }
  }
}

/// Shared spec-faithful element-move loop for shift / unshift / splice.
fn move_range(
  st: Agent,
  ref: Handle,
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
      use <- bool.lazy_guard(fuel <= 0, fn() {
        rt_val.t_throw_range_error(st, iteration_budget_msg)
      })
      let step = step_of(dir)
      let to = k + delta
      let #(has_k, st) = generic_has_op(st, ref, k)
      let st = case has_k {
        True -> {
          let #(val, st) = generic_get(st, ref, k)
          generic_set_index(st, ref, to, val)
        }
        False -> generic_delete_index(st, ref, to)
      }
      move_range(st, ref, k + step, stop, dir, delta, fuel - 1)
    }
  }
}

/// §23.1.3.33 Array.prototype.unshift(...items).
fn array_unshift(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, this, ref, length <- require_array(st, this)
  let arg_count = list.length(args)
  let new_len = length + arg_count
  // Step 5 runs even when argCount = 0; observable on objects/strings.
  use <- bool.lazy_guard(arg_count == 0, fn() {
    case classify(this) {
      KHandle(_) | KStr(_) -> #(
        from_int(new_len),
        generic_set_length(st, ref, new_len),
      )
      _ -> #(from_int(new_len), st)
    }
  })
  use <- guard_safe_length(st, new_len)
  let fast = {
    use els, len <- try_elements_fast_path(st, ref, length)
    let els =
      elements.move_range(els, 0, len, arg_count)
      |> elements.write_list(0, args)
    #(els, len + arg_count, Nil)
  }
  case fast {
    Some(#(Nil, st)) -> #(from_int(new_len), st)
    None -> {
      let st =
        move_range(
          st,
          ref,
          length - 1,
          0,
          Descending,
          arg_count,
          limits.max_iteration,
        )
      let st = write_list_at(st, ref, 0, args)
      #(from_int(new_len), generic_set_length(st, ref, new_len))
    }
  }
}

/// §23.1.3.33 steps 4d-4e / splice item-write loop.
fn write_list_at(st: Agent, ref: Handle, idx: Int, vals: List(JsVal)) -> Agent {
  case vals {
    [] -> st
    [v, ..rest] ->
      write_list_at(generic_set_index(st, ref, idx, v), ref, idx + 1, rest)
  }
}

// ──────────────────────── Array.prototype.slice / concat ─────────────────────

/// §23.1.3.25 Array.prototype.slice(start, end).
fn array_slice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use st, this, _ref, length <- require_array(st, this)
  let #(start, st) = relative_index(st, helpers.arg_at(args, 0), length, 0)
  let #(end, st) = relative_index(st, helpers.arg_at(args, 1), length, length)
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

/// Holes-read-as-undefined copy loop (toSpliced / with — §23.1.3.35 step 15.b.ii).
fn copy_range_dense(
  st: Agent,
  src: JsVal,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
) -> #(JsElements, Agent) {
  use <- bool.lazy_guard(remaining > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  case remaining <= 0 {
    True -> #(dst, st)
    False -> {
      let #(val, st) = get_index(st, src, src_idx)
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

/// §23.1.3.25 step 14 / §23.1.3.1 step 5.c.iii — HasProperty-gated copy loop
/// (holes preserved). Snapshot fast path when safe.
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
  use <- bool.lazy_guard(fuel <= 0 && remaining > 0, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  case dense_snapshot(st, src) {
    Some(#(els, proto)) ->
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
  use <- bool.lazy_guard(fuel <= 0 && remaining > 0, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
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
  use <- bool.lazy_guard(fuel <= 0 && remaining > 0, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
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

/// §23.1.3.1 Array.prototype.concat(...items).
fn array_concat(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use st, this, _this_ref <- to_object_ref(st, this)
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

/// One §23.1.3.1 step-5 iteration for the plain-Array target.
fn concat_item(
  st: Agent,
  elems: JsElements,
  pos: Int,
  item: JsVal,
) -> #(#(JsElements, Int), Agent) {
  let #(spreadable, st) = is_concat_spreadable(st, item)
  case spreadable, classify(item) {
    True, KHandle(ref) -> {
      let #(length, st) = object_length(st, ref)
      use <- bool.lazy_guard(pos + length > rt_val.max_safe_integer, fn() {
        rt_val.t_throw_type_error(
          st,
          "Array length exceeds maximum safe integer",
        )
      })
      let #(copied, st) = copy_range(st, item, 0, pos, length, elems)
      #(#(copied, pos + length), st)
    }
    _, _ -> {
      use <- bool.lazy_guard(pos >= rt_val.max_safe_integer, fn() {
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
        True, KHandle(ref) -> {
          let #(length, st) = object_length(st, ref)
          use <- bool.lazy_guard(pos + length > rt_val.max_safe_integer, fn() {
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
          use <- bool.lazy_guard(pos >= rt_val.max_safe_integer, fn() {
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

/// §7.2.18 IsConcatSpreadable.
fn is_concat_spreadable(st: Agent, item: JsVal) -> #(Bool, Agent) {
  case classify(item) {
    KHandle(_) -> {
      let #(flag, st) =
        rt_obj.t_get_prop(st, item, SymbolKey(symbol_is_concat_spreadable))
      case classify(flag) {
        KUndef -> try_is_array(st, item)
        _ -> #(rt_val.to_boolean(flag), st)
      }
    }
    _ -> #(False, st)
  }
}

// ─────────────────────── ArraySpeciesCreate + species writes ────────────────

/// §9.4.2.3 ArraySpeciesCreate. `None` → caller allocates a plain Array;
/// `Some(target)` → a custom species constructor was invoked.
fn array_species_create(
  st: Agent,
  original: JsVal,
  length: Int,
) -> #(Option(Handle), Agent) {
  case classify(original) {
    KHandle(_) -> {
      let #(is_arr, st) = try_is_array(st, original)
      case is_arr {
        False -> #(None, st)
        True -> {
          // Step 3: C = ? Get(originalArray, "constructor").
          let #(ctor, st) =
            rt_obj.t_get_prop(st, original, StringKey(Named("constructor")))
          // Step 4: C is the %Array% of ANOTHER realm → C = undefined, so
          // @@species is never read (create-proto-from-ctor-realm-array.js).
          let ctor = case classify(ctor) {
            KHandle(ctor_ref) ->
              case is_foreign_array_ctor(st, ctor_ref) {
                True -> mk_undefined()
                False -> ctor
              }
            _ -> ctor
          }
          // Step 5: C = ? Get(C, @@species); null → undefined.
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
            KHandle(ctor_ref) -> {
              let realm_array_ctor = st.realm.array.constructor
              case ctor_ref == realm_array_ctor {
                // Intrinsic Array constructor → identical to a plain array.
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

/// Step 4 of ArraySpeciesCreate: `ctor` is SameValue with the intrinsic
/// %Array% of a realm other than the running one (GetFunctionRealm of an
/// intrinsic constructor is the realm that owns it).
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
      let #(created, st) =
        rt_call.t_construct(st, ctor, [from_int(length)], ctor)
      #(Some(created), st)
    }
  }
}

/// CreateDataPropertyOrThrow(A, ! ToString(𝔽(k)), v) for each present index in
/// [0, length), then optionally Set(A, "length", 𝔽(n), true).
fn write_species_result(
  st: Agent,
  target: Handle,
  els: JsElements,
  length: Int,
  set_length: Option(Int),
) -> Agent {
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
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

/// §7.3.6 CreateDataPropertyOrThrow(A, ! ToString(𝔽(idx)), val) via
/// `t_define_own_prop` with {value, W:T, E:T, C:T}. Proxy targets go through
/// the same [[DefineOwnProperty]] internal method (`t_define_own_prop` is
/// trap-aware once M6 wires ProxyObj dispatch).
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
    rt_obj.t_define_own_prop(st, target, StringKey(index_key(idx)), desc)
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot define property " <> int.to_string(idx) <> " on object",
      )
  }
}

/// Interleaved HasProperty → Get → CreateDataPropertyOrThrow copy loop for
/// species targets (splice / concat). An abrupt completion from the target's
/// [[DefineOwnProperty]] terminates huge-length loops at the failing index.
fn copy_range_to_species(
  st: Agent,
  src: JsVal,
  src_idx: Int,
  target: Handle,
  dst_idx: Int,
  remaining: Int,
  fuel: Int,
) -> Agent {
  use <- bool.lazy_guard(fuel <= 0 && remaining > 0, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
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

// ─────────────────────── Array.prototype.reverse / fill / at ────────────────

/// §23.1.3.24 Array.prototype.reverse().
fn array_reverse(
  st: Agent,
  this: JsVal,
  _args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, this, ref, length <- require_array(st, this)
  let fast = {
    use els, len <- try_elements_fast_path(st, ref, length)
    #(elements.reverse_range(els, len), len, Nil)
  }
  case fast {
    Some(#(Nil, st)) -> #(this, st)
    None -> #(
      this,
      reverse_generic(st, ref, 0, length - 1, limits.max_iteration),
    )
  }
}

fn reverse_generic(
  st: Agent,
  ref: Handle,
  lo: Int,
  hi: Int,
  fuel: Int,
) -> Agent {
  case lo >= hi {
    True -> st
    False -> {
      use <- bool.lazy_guard(fuel <= 0, fn() {
        rt_val.t_throw_range_error(st, iteration_budget_msg)
      })
      let #(has_lo, st) = generic_has_op(st, ref, lo)
      let #(lo_val, st) = get_index_if(st, ref, lo, has_lo)
      let #(has_hi, st) = generic_has_op(st, ref, hi)
      let #(hi_val, st) = get_index_if(st, ref, hi, has_hi)
      let st = case lo_val, hi_val {
        Some(lo_v), Some(hi_v) -> {
          let st = generic_set_index(st, ref, lo, hi_v)
          generic_set_index(st, ref, hi, lo_v)
        }
        None, Some(hi_v) -> {
          let st = generic_set_index(st, ref, lo, hi_v)
          generic_delete_index(st, ref, hi)
        }
        Some(lo_v), None -> {
          let st = generic_delete_index(st, ref, lo)
          generic_set_index(st, ref, hi, lo_v)
        }
        None, None -> st
      }
      reverse_generic(st, ref, lo + 1, hi - 1, fuel - 1)
    }
  }
}

fn get_index_if(
  st: Agent,
  ref: Handle,
  idx: Int,
  present: Bool,
) -> #(Option(JsVal), Agent) {
  case present {
    True -> {
      let #(v, st) = generic_get(st, ref, idx)
      #(Some(v), st)
    }
    False -> #(None, st)
  }
}

/// §23.1.3.7 Array.prototype.fill(value[, start[, end]]).
fn array_fill(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, this, ref, length <- require_array(st, this)
  let fill_val = helpers.first_arg_or_undefined(args)
  let #(start, st) = relative_index(st, helpers.arg_at(args, 1), length, 0)
  let #(end, st) = relative_index(st, helpers.arg_at(args, 2), length, length)
  use <- bool.lazy_guard(end - start > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  let fast = {
    use els, len <- try_elements_fast_path(st, ref, length)
    #(elements.fill_range(els, start, end, fill_val), len, Nil)
  }
  case fast {
    Some(#(Nil, st)) -> #(this, st)
    None -> #(this, fill_generic(st, ref, start, end, fill_val))
  }
}

fn fill_generic(
  st: Agent,
  ref: Handle,
  idx: Int,
  end: Int,
  val: JsVal,
) -> Agent {
  case idx >= end {
    True -> st
    False ->
      fill_generic(generic_set_index(st, ref, idx, val), ref, idx + 1, end, val)
  }
}

/// §23.1.3.1 Array.prototype.at(index).
fn array_at(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  let #(raw, st) = rt_val.t_to_integer_or_infinity(st, helpers.arg_at(args, 0))
  let idx = case raw < 0 {
    True -> length + raw
    False -> raw
  }
  case idx < 0 || idx >= length {
    True -> #(mk_undefined(), st)
    False -> get_index(st, this, idx)
  }
}

// ────────────────────── indexOf / lastIndexOf / includes ────────────────────

/// §23.1.3.16 Array.prototype.indexOf(searchElement[, fromIndex]).
fn array_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  forward_search_driver(
    st,
    this,
    args,
    rt_val.strict_equal,
    SkipHoles,
    from_int,
  )
}

/// §23.1.3.15 Array.prototype.includes(searchElement[, fromIndex]).
fn array_includes(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  forward_search_driver(
    st,
    this,
    args,
    rt_val.same_value_zero,
    VisitHoles,
    fn(found) { mk_bool(found >= 0) },
  )
}

/// Shared prologue + dispatch for indexOf / includes.
fn forward_search_driver(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  eq: fn(JsVal, JsVal) -> Bool,
  hole_mode: HoleMode,
  wrap: fn(Int) -> JsVal,
) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
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

/// §23.1.3.19 Array.prototype.lastIndexOf(searchElement[, fromIndex]).
fn array_last_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  use <- bool.guard(length == 0, #(from_int(-1), st))
  let search = helpers.first_arg_or_undefined(args)
  // Step 4-5: fromIndex present → ToIntegerOrInfinity; absent → len - 1.
  // Checked by arg COUNT (explicitly passing undefined yields 0).
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
  #(from_int(found), st)
}

fn search_forward(
  st: Agent,
  this: JsVal,
  idx: Int,
  length: Int,
  search: JsVal,
  eq: fn(JsVal, JsVal) -> Bool,
  hole_mode: HoleMode,
  fuel: Int,
) -> #(Int, Agent) {
  use <- bool.lazy_guard(fuel <= 0 && idx < length, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  case idx >= length {
    True -> #(-1, st)
    False -> {
      let #(maybe_val, st) = case hole_mode {
        SkipHoles -> get_index_if_present(st, this, idx)
        VisitHoles -> {
          let #(v, st) = get_index(st, this, idx)
          #(Some(v), st)
        }
      }
      let matched = case maybe_val {
        Some(val) -> eq(val, search)
        None -> False
      }
      case matched {
        True -> #(idx, st)
        False ->
          search_forward(
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
  use <- bool.lazy_guard(fuel <= 0 && idx >= 0, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  case idx < 0 {
    True -> #(-1, st)
    False -> {
      let #(maybe_val, st) = get_index_if_present(st, this, idx)
      case maybe_val {
        None -> search_backward(st, this, idx - 1, search, fuel - 1)
        Some(val) ->
          case rt_val.strict_equal(val, search) {
            True -> #(idx, st)
            False -> search_backward(st, this, idx - 1, search, fuel - 1)
          }
      }
    }
  }
}

// ─────────────────── iteration methods (forEach/map/filter/…) ───────────────

/// How a scan treats holes: SkipHoles = HasProperty-gated (indexOf, forEach,
/// every, some, sort); VisitHoles = plain Get (includes, find*, toSorted).
type HoleMode {
  SkipHoles
  VisitHoles
}

/// Bidirectional loop direction. `bounds` builds the (start,end,step) triple.
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

/// Outcome of a predicate-driven scan.
type FoundAt {
  Found(element: JsVal, index: Int)
  NotFound
}

/// Shared driver for forEach / every / some / find* — the "call cb per element,
/// stop when `stop_on(result)`" pattern.
fn iterate_array(
  st: Agent,
  arr: JsVal,
  length: Int,
  dir: Direction,
  cb: JsVal,
  this_arg: JsVal,
  hole_mode: HoleMode,
  stop_on: fn(JsVal) -> Bool,
  cont: fn(Agent, FoundAt) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let #(start, end, step) = bounds(dir, length)
  iterate_loop(
    st,
    arr,
    start,
    end,
    step,
    limits.max_iteration,
    cb,
    this_arg,
    hole_mode,
    stop_on,
    cont,
  )
}

fn iterate_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  end: Int,
  step: Int,
  fuel: Int,
  cb: JsVal,
  this_arg: JsVal,
  hole_mode: HoleMode,
  stop_on: fn(JsVal) -> Bool,
  cont: fn(Agent, FoundAt) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use <- bool.lazy_guard(fuel <= 0 && idx != end, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  case idx == end {
    True -> cont(st, NotFound)
    False -> {
      let #(maybe_elem, st) = get_index_if_present(st, arr, idx)
      let maybe_elem = case hole_mode {
        VisitHoles -> Some(option.unwrap(maybe_elem, mk_undefined()))
        SkipHoles -> maybe_elem
      }
      case maybe_elem {
        None ->
          iterate_loop(
            st,
            arr,
            idx + step,
            end,
            step,
            fuel - 1,
            cb,
            this_arg,
            hole_mode,
            stop_on,
            cont,
          )
        Some(elem) -> {
          let #(result, st) =
            rt_call.t_call_checked(st, cb, this_arg, [
              elem,
              from_int(idx),
              arr,
            ])
          case stop_on(result) {
            True -> cont(st, Found(elem, idx))
            False ->
              iterate_loop(
                st,
                arr,
                idx + step,
                end,
                step,
                fuel - 1,
                cb,
                this_arg,
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

/// Shared driver for map / filter / flatMap — cb per element with an
/// accumulator; SkipHoles.
fn fold_array(
  st: Agent,
  arr: JsVal,
  length: Int,
  cb: JsVal,
  this_arg: JsVal,
  initial: acc,
  combine: fn(Agent, acc, JsVal, JsVal, Int) -> acc,
) -> #(acc, Agent) {
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  fold_loop(st, arr, 0, length, cb, this_arg, initial, combine)
}

fn fold_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: JsVal,
  this_arg: JsVal,
  acc: acc,
  combine: fn(Agent, acc, JsVal, JsVal, Int) -> acc,
) -> #(acc, Agent) {
  case idx >= length {
    True -> #(acc, st)
    False -> {
      let #(maybe_elem, st) = get_index_if_present(st, arr, idx)
      case maybe_elem {
        None -> fold_loop(st, arr, idx + 1, length, cb, this_arg, acc, combine)
        Some(elem) -> {
          let #(result, st) =
            rt_call.t_call_checked(st, cb, this_arg, [
              elem,
              from_int(idx),
              arr,
            ])
          let acc = combine(st, acc, result, elem, idx)
          fold_loop(st, arr, idx + 1, length, cb, this_arg, acc, combine)
        }
      }
    }
  }
}

/// §23.1.3.13 Array.prototype.forEach(callbackfn[, thisArg]).
fn array_for_each(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  use st, cb, this_arg <- require_callback(st, args)
  use st, _found <- iterate_array(
    st,
    this,
    length,
    Ascending,
    cb,
    this_arg,
    SkipHoles,
    fn(_) { False },
  )
  #(mk_undefined(), st)
}

/// §23.1.3.19 Array.prototype.map(callbackfn[, thisArg]).
fn array_map(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  use st, cb, this_arg <- require_callback(st, args)
  let #(species, st) = array_species_create(st, this, length)
  let #(els, st) =
    fold_array(
      st,
      this,
      length,
      cb,
      this_arg,
      elements.new(),
      fn(_st, acc, result, _elem, idx) { elements.set(acc, idx, result) },
    )
  case species {
    None -> finish_array(st, els, length)
    Some(target) -> {
      let st = write_species_result(st, target, els, length, None)
      #(mk_object(target), st)
    }
  }
}

/// Plain-array "Return A" — build a fresh Array from collected elements.
fn finish_array(
  st: Agent,
  elements: JsElements,
  length: Int,
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  alloc_array(st, length, elements, array_proto)
}

/// §23.1.3.8 Array.prototype.filter(callbackfn[, thisArg]).
fn array_filter(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  use st, cb, this_arg <- require_callback(st, args)
  let #(species, st) = array_species_create(st, this, 0)
  let #(kept_rev, st) =
    fold_array(
      st,
      this,
      length,
      cb,
      this_arg,
      [],
      fn(_st, acc, result, elem, _idx) {
        case rt_val.to_boolean(result) {
          True -> [elem, ..acc]
          False -> acc
        }
      },
    )
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

/// §23.1.3.5 Array.prototype.every(callbackfn[, thisArg]).
fn array_every(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  every_some(st, this, args, match_on: False)
}

/// §23.1.3.27 Array.prototype.some(callbackfn[, thisArg]).
fn array_some(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  every_some(st, this, args, match_on: True)
}

fn every_some(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  match_on match_on: Bool,
) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  use st, cb, this_arg <- require_callback(st, args)
  use st, found <- iterate_array(
    st,
    this,
    length,
    Ascending,
    cb,
    this_arg,
    SkipHoles,
    fn(r) { rt_val.to_boolean(r) == match_on },
  )
  let stopped_early = case found {
    Found(_, _) -> True
    NotFound -> False
  }
  #(mk_bool(stopped_early == match_on), st)
}

/// §23.1.3.9.1 FindViaPredicate driver.
fn find_via_predicate(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  dir: Direction,
  cont: fn(Agent, FoundAt) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  use st, cb, this_arg <- require_callback(st, args)
  use st, found <- iterate_array(
    st,
    this,
    length,
    dir,
    cb,
    this_arg,
    VisitHoles,
    rt_val.to_boolean,
  )
  cont(st, found)
}

/// §23.1.3.9 Array.prototype.find.
fn array_find(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, found <- find_via_predicate(st, this, args, Ascending)
  case found {
    Found(elem, _) -> #(elem, st)
    NotFound -> #(mk_undefined(), st)
  }
}

/// §23.1.3.10 Array.prototype.findIndex.
fn array_find_index(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, found <- find_via_predicate(st, this, args, Ascending)
  case found {
    Found(_, idx) -> #(from_int(idx), st)
    NotFound -> #(from_int(-1), st)
  }
}

/// §23.1.3.11 Array.prototype.findLast.
fn array_find_last(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, found <- find_via_predicate(st, this, args, Descending)
  case found {
    Found(elem, _) -> #(elem, st)
    NotFound -> #(mk_undefined(), st)
  }
}

/// §23.1.3.12 Array.prototype.findLastIndex.
fn array_find_last_index(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, found <- find_via_predicate(st, this, args, Descending)
  case found {
    Found(_, idx) -> #(from_int(idx), st)
    NotFound -> #(from_int(-1), st)
  }
}

// ─────────────────────────── reduce / reduceRight ───────────────────────────

/// §23.1.3.23 Array.prototype.reduce(callbackfn[, initialValue]).
fn array_reduce(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  reduce_impl(st, this, args, Ascending)
}

/// §23.1.3.24 Array.prototype.reduceRight(callbackfn[, initialValue]).
fn array_reduce_right(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  reduce_impl(st, this, args, Descending)
}

fn reduce_impl(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  dir: Direction,
) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  let cb = helpers.first_arg_or_undefined(args)
  use cb <- helpers.require_callable(st, cb, fn() { not_a_function(st, cb) })
  let #(start, end, step) = bounds(dir, length)
  let #(has_init, init) = case args {
    [_, v, ..] -> #(True, v)
    _ -> #(False, mk_undefined())
  }
  case has_init {
    True ->
      reduce_loop(st, this, start, end, cb, init, dir, limits.max_iteration)
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
          reduce_loop(
            st,
            this,
            first_idx + step,
            end,
            cb,
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
      use <- bool.lazy_guard(fuel <= 0, fn() {
        rt_val.t_throw_range_error(st, iteration_budget_msg)
      })
      let #(maybe_val, st) = get_index_if_present(st, this, idx)
      case maybe_val {
        Some(val) -> #(Some(#(idx, val)), st)
        None -> find_present(st, this, idx + step_of(dir), end, dir, fuel - 1)
      }
    }
  }
}

fn reduce_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  end: Int,
  cb: JsVal,
  acc: JsVal,
  dir: Direction,
  fuel: Int,
) -> #(JsVal, Agent) {
  let step = step_of(dir)
  case idx == end {
    True -> #(acc, st)
    False -> {
      use <- bool.lazy_guard(fuel <= 0, fn() {
        rt_val.t_throw_range_error(st, iteration_budget_msg)
      })
      let #(maybe_elem, st) = get_index_if_present(st, arr, idx)
      case maybe_elem {
        None -> reduce_loop(st, arr, idx + step, end, cb, acc, dir, fuel - 1)
        Some(elem) -> {
          let #(result, st) =
            rt_call.t_call_checked(st, cb, mk_undefined(), [
              acc,
              elem,
              from_int(idx),
              arr,
            ])
          reduce_loop(st, arr, idx + step, end, cb, result, dir, fuel - 1)
        }
      }
    }
  }
}

// ────────────────────────── sort / toSorted ─────────────────────────────────

/// §23.1.3.30 Array.prototype.sort(comparefn).
fn array_sort(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, comparefn <- with_comparefn(st, args)
  use st, this, ref, length <- require_array(st, this)
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  case comparefn {
    None -> sort_default(st, ref, length, this)
    Some(cmp) -> sort_with_comparefn(st, ref, length, cmp, this)
  }
}

/// Shared step-1 comparefn validation for sort / toSorted.
fn with_comparefn(
  st: Agent,
  args: List(JsVal),
  cont: fn(Agent, Option(JsVal)) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let comparefn = helpers.first_arg_or_undefined(args)
  case classify(comparefn) {
    KUndef -> cont(st, None)
    _ -> {
      use comparefn <- helpers.require_callable(st, comparefn, fn() {
        not_a_function(st, comparefn)
      })
      cont(st, Some(comparefn))
    }
  }
}

fn sort_default(
  st: Agent,
  ref: Handle,
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
  #(this, write_sort_result(st, ref, all_values, length, 0))
}

fn sort_with_comparefn(
  st: Agent,
  ref: Handle,
  length: Int,
  comparefn: JsVal,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(#(defined, undefs), st) =
    collect_sort_elements(st, this, length, 0, [], 0, SkipHoles)
  let #(sorted, st) = merge_sort(st, defined, comparefn)
  let all_values = list.append(sorted, list.repeat(mk_undefined(), undefs))
  #(this, write_sort_result(st, ref, all_values, length, 0))
}

/// Collect defined elements + undefined-count. `hole_mode`: sort() SkipHoles;
/// toSorted() VisitHoles (holes → trailing undefineds).
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

/// Stable bottom-up merge sort with a state-threaded effectful comparator.
fn merge_sort(
  st: Agent,
  items: List(JsVal),
  comparefn: JsVal,
) -> #(List(JsVal), Agent) {
  case items {
    [] | [_] -> #(items, st)
    _ -> merge_all(st, list.map(items, fn(x) { [x] }), comparefn)
  }
}

fn merge_all(
  st: Agent,
  runs: List(List(JsVal)),
  comparefn: JsVal,
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
  comparefn: JsVal,
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
  comparefn: JsVal,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case left, right {
    [], _ -> #(list.append(list.reverse(acc), right), st)
    _, [] -> #(list.append(list.reverse(acc), left), st)
    [l, ..ls], [r, ..rs] -> {
      let #(res, st) =
        rt_call.t_call_checked(st, comparefn, mk_undefined(), [l, r])
      // §23.1.3.30.2 step 6: v = ? ToNumber(v); step 7: NaN → +0.
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

/// §23.1.3.30 steps 7-8: write sorted values back, delete trailing holes.
fn write_sort_result(
  st: Agent,
  ref: Handle,
  values: List(JsVal),
  length: Int,
  idx: Int,
) -> Agent {
  let fast = case idx == 0 {
    True -> {
      use _els, len <- try_elements_fast_path(st, ref, length)
      #(elements.from_list(values), len, Nil)
    }
    False -> None
  }
  case fast {
    Some(#(Nil, st)) -> st
    None ->
      case values {
        [val, ..rest] -> {
          let st = generic_set_index(st, ref, idx, val)
          write_sort_result(st, ref, rest, length, idx + 1)
        }
        [] -> delete_trailing(st, ref, idx, length)
      }
  }
}

fn delete_trailing(st: Agent, ref: Handle, idx: Int, length: Int) -> Agent {
  case idx >= length {
    True -> st
    False ->
      delete_trailing(generic_delete_index(st, ref, idx), ref, idx + 1, length)
  }
}

/// §23.1.3.34 Array.prototype.toSorted([comparefn]).
fn array_to_sorted(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, comparefn <- with_comparefn(st, args)
  use st, this, _ref, length <- require_array(st, this)
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  case comparefn {
    None -> to_sorted_impl(st, length, this, sort_values_default)
    Some(cmp) ->
      to_sorted_impl(st, length, this, fn(st, defined) {
        merge_sort(st, defined, cmp)
      })
  }
}

fn to_sorted_impl(
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

// ─────────────────────────────── splice ──────────────────────────────────────

/// §23.1.3.31 Array.prototype.splice(start, deleteCount, ...items).
fn array_splice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use st, this, ref, length <- require_array(st, this)
  let #(actual_start, st) =
    relative_index(st, helpers.arg_at(args, 0), length, 0)
  let #(#(actual_delete_count, items), st) =
    try_delete_count(st, args, length, actual_start)
  let item_count = list.length(items)
  let new_length = length - actual_delete_count + item_count
  use <- guard_safe_length(st, new_length)
  let #(species, st) = array_species_create(st, this, actual_delete_count)
  // Steps 12-13: build removed array A.
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
    use els, len <- try_elements_fast_path(st, ref, length)
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
        splice_shift(st, ref, actual_start, actual_delete_count, length, shift)
      let st = write_list_at(st, ref, actual_start, items)
      #(removed_arr, generic_set_length(st, ref, new_length))
    }
  }
}

fn splice_shift(
  st: Agent,
  ref: Handle,
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
        ref,
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
              ref,
              from_start,
              length,
              Ascending,
              shift,
              limits.max_iteration,
            )
          delete_trailing(st, ref, length + shift, length)
        }
        False -> st
      }
  }
}

// ────────────────────────── flat / flatMap ──────────────────────────────────

/// §23.1.3.13 Array.prototype.flat([depth]).
fn array_flat(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
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

/// §23.1.3.13.1 FlattenIntoArray. Returns elements REVERSED (caller reverses).
fn flatten_into(
  st: Agent,
  src: JsVal,
  length: Int,
  depth: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
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
              let #(should_flatten, st) = try_is_array(st, elem)
              case classify(elem), should_flatten {
                KHandle(sub_ref), True -> {
                  let #(sub_len, st) = object_length(st, sub_ref)
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

/// §23.1.3.14 Array.prototype.flatMap(mapperFunction[, thisArg]).
fn array_flat_map(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  use st, cb, this_arg <- require_callback(st, args)
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  let #(species, st) = array_species_create(st, this, 0)
  let #(kept_rev, st) = flat_map_loop(st, this, 0, length, cb, this_arg, [])
  finish_species_list(st, kept_rev, species)
}

fn flat_map_loop(
  st: Agent,
  arr: JsVal,
  idx: Int,
  length: Int,
  cb: JsVal,
  this_arg: JsVal,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case idx >= length {
    True -> #(acc, st)
    False -> {
      let #(maybe_elem, st) = get_index_if_present(st, arr, idx)
      case maybe_elem {
        None -> flat_map_loop(st, arr, idx + 1, length, cb, this_arg, acc)
        Some(elem) -> {
          let #(mapped, st) =
            rt_call.t_call_checked(st, cb, this_arg, [
              elem,
              from_int(idx),
              arr,
            ])
          let #(should_flatten, st) = try_is_array(st, mapped)
          case classify(mapped), should_flatten {
            KHandle(sub_ref), True -> {
              let #(sub_len, st) = object_length(st, sub_ref)
              let #(new_acc, st) = flatten_into(st, mapped, sub_len, 0, acc)
              flat_map_loop(st, arr, idx + 1, length, cb, this_arg, new_acc)
            }
            _, _ ->
              flat_map_loop(st, arr, idx + 1, length, cb, this_arg, [
                mapped,
                ..acc
              ])
          }
        }
      }
    }
  }
}

// ─────────────────────────── copyWithin ─────────────────────────────────────

/// §23.1.3.4 Array.prototype.copyWithin(target, start[, end]).
fn array_copy_within(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, this, ref, length <- require_array(st, this)
  let #(target, st) = relative_index(st, helpers.arg_at(args, 0), length, 0)
  let #(from, st) = relative_index(st, helpers.arg_at(args, 1), length, 0)
  let #(final, st) = relative_index(st, helpers.arg_at(args, 2), length, length)
  let count = int.min(final - from, length - target)
  use <- bool.lazy_guard(count > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  case count <= 0 {
    True -> #(this, st)
    False -> {
      let fast = {
        use els, len <- try_elements_fast_path(st, ref, length)
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
                ref,
                from + count - 1,
                target + count - 1,
                Descending,
                count,
              ),
            )
            False -> #(
              this,
              copy_within_step(st, ref, from, target, Ascending, count),
            )
          }
      }
    }
  }
}

fn copy_within_step(
  st: Agent,
  ref: Handle,
  from: Int,
  to: Int,
  dir: Direction,
  remaining: Int,
) -> Agent {
  case remaining <= 0 {
    True -> st
    False -> {
      let step = step_of(dir)
      let #(has_from, st) = generic_has_op(st, ref, from)
      let st = case has_from {
        True -> {
          let #(val, st) = generic_get(st, ref, from)
          generic_set_index(st, ref, to, val)
        }
        False -> generic_delete_index(st, ref, to)
      }
      copy_within_step(st, ref, from + step, to + step, dir, remaining - 1)
    }
  }
}

// ───────────────────────── Array.from / Array.of ────────────────────────────

/// §23.1.2.1 Array.from(items[, mapFn[, thisArg]]).
fn array_from(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(items_val, map_fn, this_arg) = helpers.three_args_or_undefined(args)
  case classify(map_fn) {
    KUndef -> array_from_array_like(st, items_val, None, this_arg)
    _ -> {
      use mf <- helpers.require_callable(st, map_fn, fn() {
        not_a_function(st, map_fn)
      })
      array_from_array_like(st, items_val, Some(mf), this_arg)
    }
  }
}

fn array_from_array_like(
  st: Agent,
  items: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
) -> #(JsVal, Agent) {
  case classify(items) {
    KNull | KUndef -> {
      let #(ty, _) = rt_val.t_type_of(st, items)
      rt_val.t_throw_type_error(st, "Cannot create array from " <> ty)
    }
    _ -> {
      // §23.1.2.1 step 4: usingIterator = ? GetMethod(items, @@iterator) —
      // GetV goes through the primitive's prototype for strings/etc.
      let #(iter_method, st) =
        rt_obj.t_get_prop(st, items, SymbolKey(symbol_iterator))
      case classify(iter_method) {
        KUndef | KNull -> {
          let #(len_val, st) =
            rt_obj.t_get_prop(st, items, StringKey(Named("length")))
          let #(length, st) = rt_val.t_to_length(st, len_val)
          use <- bool.lazy_guard(length > limits.max_iteration, fn() {
            rt_val.t_throw_range_error(st, iteration_budget_msg)
          })
          array_from_loop(st, items, 0, length, map_fn, this_arg, [])
        }
        _ -> {
          use m <- helpers.require_callable(st, iter_method, fn() {
            not_a_function(st, iter_method)
          })
          array_from_iterator(st, items, m, map_fn, this_arg)
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
) -> #(JsVal, Agent) {
  let #(rec, st) =
    iter_protocol.get_iterator_from_method(st, items, iter_method)
  array_from_iterator_loop(st, rec, map_fn, this_arg, 0, [])
}

fn array_from_iterator_loop(
  st: Agent,
  rec: iter_protocol.IteratorRecord,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  k: Int,
  acc: List(JsVal),
) -> #(JsVal, Agent) {
  let #(step, st) = iter_protocol.iterator_step_value(st, rec)
  case step {
    None -> alloc_array_list(st, list.reverse(acc))
    Some(item) -> {
      let #(mapped, st) = case map_fn {
        // §23.1.2.1 step 5.e.vi-vii: IfAbruptCloseIterator on mapFn.
        Some(mf) -> {
          use mapped, st <- iter_protocol.or_close(st, rec.iterator, fn(st) {
            rt_call.t_call_checked(st, mf, this_arg, [item, from_int(k)])
          })
          #(mapped, st)
        }
        None -> #(item, st)
      }
      array_from_iterator_loop(st, rec, map_fn, this_arg, k + 1, [mapped, ..acc])
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
  acc: List(JsVal),
) -> #(JsVal, Agent) {
  case idx >= length {
    True -> {
      let array_proto = st.realm.array.prototype
      alloc_array(
        st,
        length,
        elements.from_list(list.reverse(acc)),
        array_proto,
      )
    }
    False -> {
      let #(elem, st) = get_index(st, items, idx)
      case map_fn {
        None ->
          array_from_loop(st, items, idx + 1, length, map_fn, this_arg, [
            elem,
            ..acc
          ])
        Some(mf) -> {
          let #(mapped, st) =
            rt_call.t_call_checked(st, mf, this_arg, [elem, from_int(idx)])
          array_from_loop(st, items, idx + 1, length, map_fn, this_arg, [
            mapped,
            ..acc
          ])
        }
      }
    }
  }
}

/// §23.1.2.3 Array.of(...items).
fn array_of(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  alloc_array_list(st, args)
}

// ───────────────── change-array-by-copy: toSpliced / with / toReversed ──────

/// §23.1.3.35 Array.prototype.toSpliced(start, skipCount, ...items).
fn array_to_spliced(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use st, this, _ref, length <- require_array(st, this)
  let #(actual_start, st) =
    relative_index(st, helpers.arg_at(args, 0), length, 0)
  let #(#(actual_skip_count, items), st) =
    try_delete_count(st, args, length, actual_start)
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

/// §23.1.3.39 Array.prototype.with(index, value).
fn array_with(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use st, this, _ref, length <- require_array(st, this)
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

/// §23.1.3.33 Array.prototype.toReversed().
fn array_to_reversed(
  st: Agent,
  this: JsVal,
  _args: List(JsVal),
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  use st, this, _ref, length <- require_array(st, this)
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
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
      let #(val, st) = get_index(st, this, idx)
      collect_elements_descending(st, this, idx - 1, [val, ..acc])
    }
  }
}

// ─────────────────── toString / toLocaleString / iterators ──────────────────

/// §23.1.3.36 Array.prototype.toString().
fn array_to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use st, array, ref <- to_object_ref(st, this)
  let #(func, st) =
    rt_obj.t_get_prop(st, mk_object(ref), StringKey(Named("join")))
  let #(callable, st) = rt_val.t_is_callable(st, func)
  case callable {
    True -> rt_call.t_call_checked(st, func, array, [])
    // Step 3: non-callable join → the %Object.prototype.toString% INTRINSIC.
    False -> object_builtin.dispatch(st, ObjectPrototypeToString, array, [])
  }
}

/// §23.1.3.30 Array.prototype.toLocaleString().
fn array_to_locale_string(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, this, _ref, length <- require_array(st, this)
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    rt_val.t_throw_range_error(st, iteration_budget_msg)
  })
  to_locale_string_loop(
    st,
    this,
    0,
    length,
    helpers.first_arg_or_undefined(args),
    helpers.arg_at(args, 1),
    [],
  )
}

fn to_locale_string_loop(
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
      let #(elem, st) = get_index(st, this, idx)
      case classify(elem) {
        KUndef | KNull ->
          to_locale_string_loop(
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
            rt_call.t_call_checked(st, method, elem, [locales_v, options_v])
          let #(s, st) = rt_val.t_to_string(st, locale_val)
          to_locale_string_loop(
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

/// §23.1.5.1 CreateArrayIterator(array, kind). ToObject only — must NOT read
/// `length` (the iterator re-reads it lazily each step).
fn create_array_iterator(
  st: Agent,
  this: JsVal,
  kind: rt_types.ArrayIterKind,
) -> #(JsVal, Agent) {
  use st, _this, ref <- to_object_ref(st, this)
  let iter_proto = st.realm.array_iter_proto
  let #(iter_ref, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: ArrayIterator(target: ref, index: 0, kind:),
        proto: Some(iter_proto),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(iter_ref), st)
}

/// §23.1.3.16 Array.prototype.keys().
fn array_keys(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  create_array_iterator(st, this, ArrayIterKeys)
}

/// §23.1.3.37 Array.prototype.values().
fn array_values(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  create_array_iterator(st, this, ArrayIterValues)
}

/// §23.1.3.4 Array.prototype.entries().
fn array_entries(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  create_array_iterator(st, this, ArrayIterEntries)
}
