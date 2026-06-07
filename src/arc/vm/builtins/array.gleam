import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/builtins/object as object_builtin
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type ArrayNativeFn, type JsElements, type JsValue, type Property, type Ref,
  AccessorProperty, ArrayConstructor, ArrayFrom, ArrayIsArray, ArrayNative,
  ArrayObject, ArrayOf, ArrayPrototypeAt, ArrayPrototypeConcat,
  ArrayPrototypeCopyWithin, ArrayPrototypeEvery, ArrayPrototypeFill,
  ArrayPrototypeFilter, ArrayPrototypeFind, ArrayPrototypeFindIndex,
  ArrayPrototypeFindLast, ArrayPrototypeFindLastIndex, ArrayPrototypeFlat,
  ArrayPrototypeFlatMap, ArrayPrototypeForEach, ArrayPrototypeIncludes,
  ArrayPrototypeIndexOf, ArrayPrototypeJoin, ArrayPrototypeLastIndexOf,
  ArrayPrototypeMap, ArrayPrototypePop, ArrayPrototypePush, ArrayPrototypeReduce,
  ArrayPrototypeReduceRight, ArrayPrototypeReverse, ArrayPrototypeShift,
  ArrayPrototypeSlice, ArrayPrototypeSome, ArrayPrototypeSort,
  ArrayPrototypeSplice, ArrayPrototypeToReversed, ArrayPrototypeToSorted,
  ArrayPrototypeToSpliced, ArrayPrototypeUnshift, ArrayPrototypeWith,
  DataProperty, Dispatch, Finite, Index, JsBool, JsNull, JsNumber, JsObject,
  JsString, JsUndefined, Named, ObjectSlot,
}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Set up Array.prototype and Array constructor.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("join", ArrayNative(ArrayPrototypeJoin), 1),
      #("push", ArrayNative(ArrayPrototypePush), 1),
      #("pop", ArrayNative(ArrayPrototypePop), 0),
      #("shift", ArrayNative(ArrayPrototypeShift), 0),
      #("unshift", ArrayNative(ArrayPrototypeUnshift), 1),
      #("slice", ArrayNative(ArrayPrototypeSlice), 2),
      #("concat", ArrayNative(ArrayPrototypeConcat), 1),
      #("reverse", ArrayNative(ArrayPrototypeReverse), 0),
      #("fill", ArrayNative(ArrayPrototypeFill), 1),
      #("at", ArrayNative(ArrayPrototypeAt), 1),
      #("indexOf", ArrayNative(ArrayPrototypeIndexOf), 1),
      #("lastIndexOf", ArrayNative(ArrayPrototypeLastIndexOf), 1),
      #("includes", ArrayNative(ArrayPrototypeIncludes), 1),
      #("forEach", ArrayNative(ArrayPrototypeForEach), 1),
      #("map", ArrayNative(ArrayPrototypeMap), 1),
      #("filter", ArrayNative(ArrayPrototypeFilter), 1),
      #("reduce", ArrayNative(ArrayPrototypeReduce), 1),
      #("reduceRight", ArrayNative(ArrayPrototypeReduceRight), 1),
      #("every", ArrayNative(ArrayPrototypeEvery), 1),
      #("some", ArrayNative(ArrayPrototypeSome), 1),
      #("find", ArrayNative(ArrayPrototypeFind), 1),
      #("findIndex", ArrayNative(ArrayPrototypeFindIndex), 1),
      #("sort", ArrayNative(ArrayPrototypeSort), 1),
      #("splice", ArrayNative(ArrayPrototypeSplice), 2),
      #("findLast", ArrayNative(ArrayPrototypeFindLast), 1),
      #("findLastIndex", ArrayNative(ArrayPrototypeFindLastIndex), 1),
      #("flat", ArrayNative(ArrayPrototypeFlat), 0),
      #("flatMap", ArrayNative(ArrayPrototypeFlatMap), 1),
      #("copyWithin", ArrayNative(ArrayPrototypeCopyWithin), 2),
      #("toSpliced", ArrayNative(ArrayPrototypeToSpliced), 2),
      #("with", ArrayNative(ArrayPrototypeWith), 2),
      #("toSorted", ArrayNative(ArrayPrototypeToSorted), 1),
      #("toReversed", ArrayNative(ArrayPrototypeToReversed), 0),
      #("toString", ArrayNative(value.ArrayPrototypeToString), 0),
      #("toLocaleString", ArrayNative(value.ArrayPrototypeToLocaleString), 0),
      #("keys", ArrayNative(value.ArrayPrototypeKeys), 0),
      #("values", ArrayNative(value.ArrayPrototypeValues), 0),
      #("entries", ArrayNative(value.ArrayPrototypeEntries), 0),
    ])
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("isArray", ArrayNative(ArrayIsArray), 1),
      #("from", ArrayNative(ArrayFrom), 1),
      #("of", ArrayNative(ArrayOf), 0),
    ])
  // §23.1.2.1 Array.fromAsync — a Call-level native (returns a promise, needs
  // VM re-entry), unlike the Dispatch-level statics above.
  let #(h, async_static_methods) =
    common.alloc_call_methods(h, function_proto, [
      #("fromAsync", value.ArrayFromAsync, 1),
    ])
  let static_methods = list.append(static_methods, async_static_methods)
  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(_) { Dispatch(ArrayNative(ArrayConstructor)) },
      "Array",
      1,
      static_methods,
    )

  // §23.1.3 "The Array prototype object … is an Array exotic object" with
  // a "length" property whose initial value is +0 (test262: prototype/length.js,
  // prototype/exotic-array.js).
  let h =
    heap.update(h, bt.prototype, fn(slot) {
      case slot {
        ObjectSlot(..) as slot -> ObjectSlot(..slot, kind: ArrayObject(0))
        other -> other
      }
    })

  // §23.1.3.40 Array.prototype [ @@iterator ] ( )
  // "The initial value of the @@iterator property is %Array.prototype.values%"
  // — the SAME function object as Array.prototype.values, not a fresh one
  // (test262: built-ins/Array/prototype/Symbol.iterator.js).
  let assert Ok(#(_, DataProperty(value: values_fn, ..))) =
    list.find(proto_methods, fn(entry) { entry.0 == "values" })
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_iterator,
      value.builtin_property(values_fn),
    )

  // §23.1.3.41 Array.prototype [ @@unscopables ]: a null-prototype object
  // whose true-valued properties hide the listed methods from `with`
  // statement scoping. Property attrs: each entry {W:T, E:T, C:T}; the
  // @@unscopables property itself {W:F, E:F, C:T}.
  let unscopable_names = [
    "at", "copyWithin", "entries", "fill", "find", "findIndex", "findLast",
    "findLastIndex", "flat", "flatMap", "includes", "keys", "toReversed",
    "toSorted", "toSpliced", "values",
  ]
  let unscopable_props =
    list.fold(unscopable_names, dict.new(), fn(props, name) {
      dict.insert(
        props,
        Named(name),
        DataProperty(
          value: JsBool(True),
          writable: True,
          enumerable: True,
          configurable: True,
        ),
      )
    })
  let #(h, unscopables_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: value.OrdinaryObject,
        properties: unscopable_props,
        elements: elements.new(),
        prototype: None,
        symbol_properties: [],
        extensible: True,
      ),
    )
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_unscopables,
      value.data(JsObject(unscopables_ref)) |> value.configurable(),
    )

  #(h, bt)
}

/// Dispatch an ArrayNativeFn to the corresponding implementation.
pub fn dispatch(
  native: ArrayNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    ArrayConstructor -> construct(args, state)
    ArrayIsArray -> is_array(args, state)
    ArrayPrototypeJoin -> array_join(this, args, state)
    ArrayPrototypePush -> array_push(this, args, state)
    ArrayPrototypePop -> array_pop(this, args, state)
    ArrayPrototypeShift -> array_shift(this, args, state)
    ArrayPrototypeUnshift -> array_unshift(this, args, state)
    ArrayPrototypeSlice -> array_slice(this, args, state)
    ArrayPrototypeConcat -> array_concat(this, args, state)
    ArrayPrototypeReverse -> array_reverse(this, args, state)
    ArrayPrototypeFill -> array_fill(this, args, state)
    ArrayPrototypeAt -> array_at(this, args, state)
    ArrayPrototypeIndexOf -> array_index_of(this, args, state)
    ArrayPrototypeLastIndexOf -> array_last_index_of(this, args, state)
    ArrayPrototypeIncludes -> array_includes(this, args, state)
    ArrayPrototypeForEach -> array_for_each(this, args, state)
    ArrayPrototypeMap -> array_map(this, args, state)
    ArrayPrototypeFilter -> array_filter(this, args, state)
    ArrayPrototypeReduce -> array_reduce(this, args, state)
    ArrayPrototypeReduceRight -> array_reduce_right(this, args, state)
    ArrayPrototypeEvery -> array_every(this, args, state)
    ArrayPrototypeSome -> array_some(this, args, state)
    ArrayPrototypeFind -> array_find(this, args, state)
    ArrayPrototypeFindIndex -> array_find_index(this, args, state)
    ArrayPrototypeSort -> array_sort(this, args, state)
    ArrayPrototypeSplice -> array_splice(this, args, state)
    ArrayPrototypeFindLast -> array_find_last(this, args, state)
    ArrayPrototypeFindLastIndex -> array_find_last_index(this, args, state)
    ArrayPrototypeFlat -> array_flat(this, args, state)
    ArrayPrototypeFlatMap -> array_flat_map(this, args, state)
    ArrayPrototypeCopyWithin -> array_copy_within(this, args, state)
    ArrayPrototypeToSpliced -> array_to_spliced(this, args, state)
    ArrayPrototypeWith -> array_with(this, args, state)
    ArrayPrototypeToSorted -> array_to_sorted(this, args, state)
    ArrayPrototypeToReversed -> array_to_reversed(this, args, state)
    ArrayFrom -> array_from(args, state)
    ArrayOf -> array_of(args, state)
    value.ArrayPrototypeToString -> array_to_string(this, state)
    value.ArrayPrototypeToLocaleString ->
      array_to_locale_string(this, args, state)
    value.ArrayPrototypeKeys -> array_keys(this, state)
    value.ArrayPrototypeValues -> array_values(this, state)
    value.ArrayPrototypeEntries -> array_entries(this, state)
  }
}

/// Array() / new Array() — construct a new array.
/// Wrapper that threads State around native_array_constructor.
/// §23.1.1 The Array Constructor: "is the initial value of the Array property
/// of the global object." Called as both function and constructor (identical).
fn construct(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  let heap = state.heap
  let #(heap, result) =
    native_array_constructor(args, heap, array_proto, state.builtins)
  #(State(..state, heap:), result)
}

/// Array.isArray(value) — check if a value is an array.
/// §23.1.2.1 Array.isArray ( arg ): Return ? IsArray(arg).
///
/// IsArray (§7.2.2) pierces Proxy exotic objects to their [[ProxyTarget]]
/// (step 3) and throws TypeError on a revoked proxy (step 3.a).
fn is_array(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let arg = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }
  case object.is_array(state.heap, arg) {
    Ok(b) -> #(state, Ok(JsBool(b)))
    Error(Nil) ->
      state.type_error(
        state,
        "Cannot perform 'IsArray' on a proxy that has been revoked",
      )
  }
}

/// Array ( ...values ) — §23.1.1.1
/// A single unified algorithm that branches on numberOfArgs:
///   step 4: numberOfArgs = 0 → ArrayCreate(0, proto)
///   step 5: numberOfArgs = 1 → single-arg (len) path
///   step 6: numberOfArgs ≥ 2 → ...items path
///
/// NewTarget / subclassing is not supported — proto is always the passed-in
/// array_proto (equivalent to %Array.prototype%).
fn native_array_constructor(
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,
  builtins: common.Builtins,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    // §23.1.1.1 Array() — numberOfArgs = 0
    // 1. Let numberOfArgs be the number of elements in values.
    // 2. Assert: numberOfArgs = 0.
    // 3. (NewTarget handling — skipped, no subclassing)
    // 4. Return ! ArrayCreate(0, proto).
    [] -> alloc_array(heap, 0, elements.new(), array_proto)

    // §23.1.1.2 Array(len) — numberOfArgs = 1
    // 5. If len is not a Number, then
    //    a. (non-numeric path — falls through to the _ branch below)
    // 6. Else (len is a Number),
    //    a. Let intLen be ! ToUint32(len).
    //    b. If intLen ≠ len, throw a RangeError.
    //    (we check integer + non-negative via float_to_int + round-trip,
    //     which is equivalent: ToUint32 would truncate non-integers and wrap
    //     negatives, making them !== the original value)
    // 7. Perform ! Set(array, "length", intLen, true).
    // 8. Return array.
    [JsNumber(value.Finite(n))] -> {
      let len = value.float_to_int(n)
      case len >= 0 && int.to_float(len) == n {
        True -> alloc_array(heap, len, elements.new(), array_proto)
        // intLen ≠ len → RangeError (spec step 6b)
        False -> {
          let #(heap, err) =
            common.make_range_error(heap, builtins, "Invalid array length")
          #(heap, Error(err))
        }
      }
    }

    // §23.1.1.3 Array(...items) — numberOfArgs >= 2
    // (also handles single non-Number arg — non-Number single args fall
    //  through here and are treated as items, producing the same result)
    _ -> {
      let count = list.length(args)
      alloc_array(heap, count, elements.from_list(args), array_proto)
    }
  }
}

/// Allocate an array object (combines ArrayCreate + element population).
fn alloc_array(
  heap: Heap,
  length: Int,
  elements: JsElements,
  array_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_array_from_elements(heap, elements, length, array_proto)
  #(heap, Ok(JsObject(ref)))
}

/// Array.prototype.join ( separator )
/// ES2024 §23.1.3.18
fn array_join(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: Let O be ? ToObject(this value).
  //            Let len be ? LengthOfArrayLike(O).
  // (handled by require_array — converts this to object, reads .length)
  use this, _ref, length, state <- require_array(this, state)
  // Steps 3-4: If separator is undefined, let sep be ",".
  //            Else, let sep be ? ToString(separator).
  let sep_val = case args {
    [JsUndefined, ..] | [] -> JsString(",")
    [v, ..] -> v
  }
  use separator, state <- coerce.try_to_string(state, sep_val)
  // Pragmatic bound: O(length) string materialization with no early exit —
  // cap at max_iteration (see iteration_budget_msg).
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  // Steps 5-8: Build result string R by iterating k from 0 to len-1,
  //            joining elements with sep. Return R.
  case join_elements(state, this, 0, length, separator, []) {
    #(state, Ok(result)) -> #(state, Ok(JsString(result)))
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

/// join_elements — implements step 6 of Array.prototype.join (§23.1.3.18).
/// Iterates k from 0 to len-1, building the result string R.
///
/// Plain arrays with no index overrides take join_elements_snapshot (one
/// heap read for the whole loop); everything else takes the generic
/// per-element path via get_index (object.get_value_of — walks prototype
/// chain and invokes getters).
fn join_elements(
  state: State,
  this: JsValue,
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
) -> #(State, Result(String, JsValue)) {
  case dense_snapshot(state, this) {
    Some(#(els, proto)) ->
      join_elements_snapshot(
        state,
        this,
        els,
        proto,
        idx,
        length,
        separator,
        acc,
      )
    None -> join_elements_generic(state, this, idx, length, separator, acc)
  }
}

/// Fast path for join_elements: read elements from a one-time snapshot of
/// the array slot. Falls back to the generic path from the current index the
/// moment user code could run (object ToString, inherited hole property) —
/// after which the snapshot might be stale.
fn join_elements_snapshot(
  state: State,
  this: JsValue,
  els: JsElements,
  proto: Option(Ref),
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
) -> #(State, Result(String, JsValue)) {
  case idx >= length {
    // Step 8: Return R.
    True -> #(state, Ok(acc |> list.reverse |> string.join(separator)))
    False ->
      case elements.get_option(els, idx) {
        // Step 7c: If element is undefined or null, let next be "".
        Some(JsUndefined) | Some(JsNull) ->
          join_elements_snapshot(
            state,
            this,
            els,
            proto,
            idx + 1,
            length,
            separator,
            ["", ..acc],
          )
        // ToString of an object may invoke valueOf/toString (user code that
        // can mutate the array) — bail to the generic path from here.
        Some(JsObject(_)) ->
          join_elements_generic(state, this, idx, length, separator, acc)
        // Step 7c (cont.): primitive — ToString runs no user code.
        Some(val) -> {
          use str, state <- coerce.try_to_string(state, val)
          join_elements_snapshot(
            state,
            this,
            els,
            proto,
            idx + 1,
            length,
            separator,
            [str, ..acc],
          )
        }
        // Hole: Get walks the prototype chain. If nothing is inherited the
        // result is undefined → "". An inherited property may be a getter —
        // bail to the generic path which performs the real Get.
        None ->
          case hole_is_inherited(state, proto, idx) {
            False ->
              join_elements_snapshot(
                state,
                this,
                els,
                proto,
                idx + 1,
                length,
                separator,
                ["", ..acc],
              )
            True ->
              join_elements_generic(state, this, idx, length, separator, acc)
          }
      }
  }
}

/// Generic per-element path for join_elements: re-reads the heap each
/// iteration via get_index (handles getters, proxies-on-proto, string
/// primitives, and arrays mutated mid-join by user code).
fn join_elements_generic(
  state: State,
  this: JsValue,
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
) -> #(State, Result(String, JsValue)) {
  case idx >= length {
    // Step 8: Return R.
    True -> #(state, Ok(acc |> list.reverse |> string.join(separator)))
    False -> {
      // Step 7b: Let element be ? Get(O, ! ToString(𝔽(k))).
      use val, state <- state.try_op(get_index(state, this, idx))
      case val {
        // Step 7c: If element is undefined or null, let next be "".
        JsUndefined | JsNull ->
          join_elements_generic(state, this, idx + 1, length, separator, [
            "",
            ..acc
          ])
        // Step 7c (cont.): Otherwise, let next be ? ToString(element).
        _ -> {
          use str, state <- coerce.try_to_string(state, val)
          // Step 7d: Set R to string-concatenation of R and next.
          // Step 7e: Set k to k + 1.
          join_elements_generic(state, this, idx + 1, length, separator, [
            str,
            ..acc
          ])
        }
      }
    }
  }
}

/// Array.prototype.push ( ...items )
/// ES2024 §23.1.3.22
fn array_push(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: Let O be ? ToObject(this value).
  //            Let len be ? LengthOfArrayLike(O).
  // (require_array handles ToObject + length extraction as a fast path.)
  use _this, ref, length, state <- require_array(this, state)
  // Step 3: Let argCount be the number of elements in items.
  // Step 4: If len + argCount > 2^53 - 1, throw a TypeError.
  use <- state.guard_safe_length(state, length + list.length(args))
  // Fast path: append all items with one heap read + one heap write.
  // Even a single-element push wins here: the generic path performs TWO full
  // OrdinarySet operations (element write + length write), each with its own
  // heap reads and receiver walk. Eligibility is O(argCount) map lookups —
  // see try_push_fast_path — not a scan over every proto-chain property dict.
  let fast = case args {
    [] -> None
    // Indices ≥ 2^32-1 are NOT array indices (§6.1.7) — they live in the
    // dict as named properties and must not land in element storage, and
    // the final length Set throws RangeError (§10.4.2.4). Generic path only.
    _ ->
      case length + list.length(args) > 4_294_967_295 {
        True -> None
        False -> try_push_fast_path(state, ref, length, args)
      }
  }
  case fast {
    Some(#(new_length, state)) -> #(state, Ok(value.from_int(new_length)))
    None -> {
      // Steps 5-7 delegated to push_generic.
      use new_length, state <- state.try_op(push_generic(
        state,
        ref,
        length,
        args,
      ))
      // Step 7: Return 𝔽(len).
      #(state, Ok(value.from_int(new_length)))
    }
  }
}

/// ES2024 §23.1.3.22 steps 5-7 (loop + length update + return).
fn push_generic(
  state: State,
  ref: Ref,
  length: Int,
  args: List(JsValue),
) -> Result(#(Int, State), #(JsValue, State)) {
  case args {
    [] -> {
      // §10.4.2.4 ArraySetLength step 3: a real Array's length is a uint32 —
      // ToUint32(len) ≠ len for len ≥ 2^32, so Set(O, "length", 𝔽(len))
      // throws RangeError. The element Sets already landed (indices ≥ 2^32-1
      // are ordinary named properties that don't bump [[ArrayLength]]).
      let is_real_array = case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: ArrayObject(..), ..)) -> True
        _ -> False
      }
      let exceeds_uint32 = is_real_array && length > 4_294_967_295
      use <- bool.lazy_guard(exceeds_uint32, fn() {
        range_error_op(state, "Invalid array length")
      })
      // Step 6: Perform ? Set(O, "length", 𝔽(len), true).
      use state <- result.try(generic_set_length(state, ref, length))
      // Step 7: Return 𝔽(len).
      Ok(#(length, state))
    }
    [val, ..rest] -> {
      // Step 5a: Perform ? Set(O, ! ToString(𝔽(len)), E, true).
      use state <- result.try(generic_set_index(state, ref, length, val))
      // Step 5b: Set len to len + 1.
      push_generic(state, ref, length + 1, rest)
    }
  }
}

// ============================================================================
// Shared helpers for Array.prototype methods
// ============================================================================

/// V8's standard ToObject failure message.
const cannot_convert = "Cannot convert undefined or null to object"

/// Combined ToObject (ES2024 §7.1.18) + LengthOfArrayLike (§7.3.18).
///
/// Captures `length` once, passes `(ref, length, state)` to `cont`. Per-index
/// reads during iteration go through `get_index`/`get_index_if_present`
/// (HasProperty+Get via object.get_value_of) — no eager snapshot.
///
/// Per spec (ES2024 §23.1.3), Array.prototype methods are "intentionally
/// generic" — they work on any object with a `.length` property and indexed
/// elements. This function fuses the two abstract operations:
///
///   ToObject (§7.1.18):
///     - Undefined / Null → throw TypeError
///     - String → create a String exotic object (§10.4.3)
///     - Boolean / Number / Symbol / BigInt → create wrapper object
///     - Object → return argument unchanged
///
///   LengthOfArrayLike (§7.3.18):
///     1. Assert: Type(obj) is Object.
///     2. Return ? ToLength(? Get(obj, "length")).
///       where ToLength (§7.1.17) clamps to [0, 2^53 - 1].
///
/// For String primitives/wrappers, ref is the wrapper ref or heap.sentinel_ref;
/// iteration helpers use `get_index(state, this, idx)` which delegates to
/// `object.get_value_of` — handles both objects and string primitives.
fn require_array(
  this: JsValue,
  state: State,
  cont: fn(JsValue, Ref, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    // §7.1.18: Object → return argument unchanged.
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        // Real array — length from [[ArrayLength]].
        Some(ObjectSlot(kind: ArrayObject(length:), ..)) ->
          cont(this, ref, length, state)
        // NOTE: Arguments objects deliberately take the generic path below —
        // their "length" is an ordinary writable/configurable data property
        // in the dict (§10.4.4.6), so overrides via defineProperty/assignment
        // must be honored (the slot kind's length only drives element-key
        // enumeration).
        // §7.1.18 String row / §10.4.3: String exotic object.
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) ->
          cont(this, ref, object.string_length(s), state)
        // Generic object: LengthOfArrayLike (§7.3.18) — Get(obj, "length"),
        // then ToLength (§7.1.17). The Get may invoke a getter (user code)
        // and the ToLength may invoke valueOf — both can throw, and their
        // side effects must be kept (state is threaded through).
        Some(ObjectSlot(properties:, ..)) ->
          case length_of_properties(state, ref, properties) {
            Ok(#(length, state)) -> cont(this, ref, length, state)
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        // Non-object heap slot under a ref shouldn't happen, but fall through.
        _ -> cont(this, ref, 0, state)
      }
    // §7.1.18: Undefined / Null → throw a TypeError exception.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // §7.1.18 String row: ToObject creates a String exotic object (§10.4.3).
    // A real wrapper is allocated and handed back as the normalized `this`
    // (the spec's O) so observable uses — the callback's third argument
    // (`obj instanceof String`), concat's E — see the wrapper. Index reads go
    // through StringGetOwnProperty (§10.4.3.5) on the wrapper slot; mutators
    // throw TypeError via [[Set]] on the non-writable length/indices, which
    // matches the spec's `Set(O, ..., true)` on a String exotic object.
    JsString(s) -> {
      let #(h, wrapper_ref) =
        common.alloc_wrapper(
          state.heap,
          value.StringObject(s),
          state.builtins.string.prototype,
        )
      cont(
        JsObject(wrapper_ref),
        wrapper_ref,
        object.string_length(s),
        State(..state, heap: h),
      )
    }
    // §7.1.18 Boolean/Number/Symbol/BigInt rows: ToObject creates a wrapper
    // object whose prototype chain (e.g. Boolean.prototype) may carry a
    // "length" property — Get(O, "length") must consult it. get_value_of
    // delegates primitives to their prototype without allocating a wrapper.
    // A real wrapper IS allocated and handed back as the normalized `this`
    // (the spec's O), so observable uses of O — the callback's third argument
    // (`obj instanceof Boolean`), concat's E, fill/copyWithin's return value —
    // see the wrapper object, and mutators succeed unobservably (e.g.
    // `pop.call(true)` Sets length on the wrapper and returns undefined)
    // instead of throwing on a sentinel.
    _ ->
      case object.get_value_of(state, this, Named("length")) {
        Ok(#(len_val, state)) ->
          case to_length_value(state, len_val) {
            Ok(#(length, state)) ->
              // §7.1.18 ToObject — common.to_object covers every primitive
              // row (Boolean/Number/Symbol/BigInt) so `instanceof` on the
              // normalized O sees the right wrapper kind and prototype
              // (e.g. `[].sort.call(Symbol()) instanceof Symbol`).
              case common.to_object(state.heap, state.builtins, this) {
                Some(#(h, wrapper_ref)) ->
                  cont(
                    JsObject(wrapper_ref),
                    wrapper_ref,
                    length,
                    State(..state, heap: h),
                  )
                None -> state.type_error(state, cannot_convert)
              }
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
  }
}

/// Canonical PropertyKey for an integer index, matching value.canonical_key:
/// array indices (§6.1.7) are integers in [0, 2^32-2], stored as Index(n);
/// anything outside that range is stored under its ToString form as Named.
/// Array.prototype methods are generic over array-likes whose length can
/// reach 2^53-1, so per-element keys derived from such lengths MUST go
/// through this — a raw Index(idx) for idx >= 2^32-1 can never match how the
/// property was stored.
fn index_key(idx: Int) -> value.PropertyKey {
  case 0 <= idx && idx <= 4_294_967_294 {
    True -> Index(idx)
    False -> Named(int.to_string(idx))
  }
}

/// Get (ES2024 §7.3.2) on an array-like by integer index.
///
/// §7.3.2 Get ( O, P ): Return ? O.[[Get]](P, O).
///
/// Uses `object.get_value_of` (not `get_value`) so that `this` may be a
/// primitive string — get_value_of synthesizes string index properties inline
/// per StringGetOwnProperty (§10.4.3.5). For objects, delegates to [[Get]]
/// which walks the prototype chain and invokes getter accessors.
fn get_index(
  state: State,
  this: JsValue,
  idx: Int,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  object.get_value_of(state, this, index_key(idx))
}

/// Fused HasProperty + Get on an array-like by integer index.
///
/// Replaces the per-element `HasProperty then get_index()` pattern used by
/// the SkipHoles iteration loops (forEach/map/filter/reduce/indexOf/...).
/// The old pattern did two independent `heap.read + own_property_of_slot`
/// lookups per element; this does ONE for the common case where the element
/// is an own property (dense arrays — the overwhelming majority).
///
/// Returns:
///   Ok(#(Some(val), state)) — index is present (own or inherited)
///   Ok(#(None, state))      — index is absent (hole; SkipHoles would skip)
///   Error(#(exc, state))    — accessor getter threw
///
/// Semantics match `HasProperty() ? get_index() : skip` exactly: own property
/// is checked first via §10.1.5 [[GetOwnProperty]], and only on a miss do we
/// fall through to the prototype-chain HasProperty/Get (§7.3.11 / §7.3.2).
fn get_index_if_present(
  state: State,
  this: JsValue,
  idx: Int,
) -> Result(#(Option(JsValue), State), #(JsValue, State)) {
  let key = index_key(idx)
  let is_proxy = case this {
    JsObject(ref) -> option.is_some(object.as_proxy(state.heap, ref))
    _ -> False
  }
  case this, key {
    // Proxy: HasProperty/Get MUST run the "has"/"get" traps (observable —
    // they record calls, can throw, and the own-index fast path can't see
    // through the proxy at all).
    JsObject(ref), _ if is_proxy -> {
      use #(has, state) <- result.try(object.has_property_stateful(
        state,
        ref,
        object.PkString(key),
      ))
      case has {
        False -> Ok(#(None, state))
        True -> {
          use #(val, state) <- result.map(object.get_value(
            state,
            ref,
            key,
            this,
          ))
          #(Some(val), state)
        }
      }
    }
    // Index beyond the array-index cap (>= 2^32-1) canonicalizes to a Named
    // string key — get_own_index only consults Index keys, so take the
    // generic HasProperty + Get path (own properties included).
    JsObject(ref), Named(_) -> inherited_index(state, ref, key, this)
    JsObject(ref), _ ->
      // Fast path: ONE heap read for own [[GetOwnProperty]], with no
      // synthesized descriptor boxing on the dense-elements hit.
      case object.get_own_index(state.heap, ref, idx) {
        // §10.1.8.1 step 3: data value. (Hot path: dense arrays.)
        object.OwnIndexValue(val) -> Ok(#(Some(val), state))
        object.OwnIndexProperty(DataProperty(value: val, ..)) ->
          Ok(#(Some(val), state))
        // §10.1.8.1 steps 5-7: accessor → Call(getter, Receiver) or undefined.
        object.OwnIndexProperty(AccessorProperty(get: Some(getter), ..)) -> {
          use #(val, state) <- result.map(state.call(state, getter, this, []))
          #(Some(val), state)
        }
        object.OwnIndexProperty(AccessorProperty(get: None, ..)) ->
          Ok(#(Some(JsUndefined), state))
        // Own property absent — consult prototype chain (cold path: holes).
        // §7.3.11 step 4 / §10.1.8.1 step 2: parent.[[HasProperty]]/[[Get]].
        // The prototype was carried out of the slot get_own_index already
        // read, so the hole path costs one heap read, not two.
        object.OwnIndexAbsent(prototype: Some(proto)) ->
          case object.has_property(state.heap, proto, key) {
            False -> Ok(#(None, state))
            True -> {
              use #(val, state) <- result.map(object.get_value(
                state,
                proto,
                key,
                this,
              ))
              #(Some(val), state)
            }
          }
        object.OwnIndexAbsent(prototype: None) -> Ok(#(None, state))
      }
    // String primitive: in-range index is an own data property per §10.4.3.5;
    // out-of-range falls through to String.prototype (§7.3.11 walks the
    // wrapper's prototype chain).
    JsString(s), _ ->
      case idx >= 0, object.string_char_at(s, idx) {
        True, Some(ch) -> Ok(#(Some(JsString(ch)), state))
        _, _ ->
          inherited_index(state, state.builtins.string.prototype, key, this)
      }
    // Boolean/Number/Symbol primitives: ToObject's wrapper has no own index
    // properties, but its prototype chain may (e.g. `Boolean.prototype[0] = 1`)
    // — HasProperty/Get consult it (§7.3.11 / §7.3.2).
    value.JsBool(_), _ ->
      inherited_index(state, state.builtins.boolean.prototype, key, this)
    JsNumber(_), _ ->
      inherited_index(state, state.builtins.number.prototype, key, this)
    value.JsSymbol(_), _ ->
      inherited_index(state, state.builtins.object.prototype, key, this)
    // null/undefined (already rejected by require_array) and other values:
    // no indexed properties.
    _, _ -> Ok(#(None, state))
  }
}

/// HasProperty + Get against a start object with an explicit receiver. Used
/// for a primitive's wrapper prototype chain (the primitive as receiver, so
/// getters see the right `this`) and for Named-canonicalized huge indices on
/// the object itself.
fn inherited_index(
  state: State,
  proto: Ref,
  key: value.PropertyKey,
  this: JsValue,
) -> Result(#(Option(JsValue), State), #(JsValue, State)) {
  case object.has_property(state.heap, proto, key) {
    False -> Ok(#(None, state))
    True -> {
      use #(val, state) <- result.map(object.get_value(state, proto, key, this))
      #(Some(val), state)
    }
  }
}

/// Snapshot the element storage of a plain Array for bulk iteration.
///
/// Returns Some(#(elements, prototype)) only when `this` is an ArrayObject
/// whose properties dict contains NO integer-indexed entries — i.e. every
/// in-range index is either a plain data element or a hole, so reading it
/// cannot invoke user code (no index accessors, no attribute overrides).
///
/// On that path nothing can mutate the array between loop iterations, so the
/// non-callback iteration loops (copy_range, collect_sort_elements,
/// join_elements) can read this snapshot directly instead of re-reading the
/// same unchanged heap slot per element via get_index_if_present — one
/// heap.read total instead of one per element. Callback-based iterators
/// (forEach/map/filter/reduce) must NOT use this: their callbacks can mutate
/// the array, so they keep re-reading the live heap.
fn dense_snapshot(
  state: State,
  this: JsValue,
) -> Option(#(JsElements, Option(Ref))) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: ArrayObject(_),
          properties:,
          elements: els,
          prototype:,
          ..,
        )) ->
          case properties_have_index_keys(properties) {
            // An index accessor/override could run user code or carry
            // non-default attributes — use the generic per-element path.
            True -> None
            False -> Some(#(els, prototype))
          }
        _ -> None
      }
    _ -> None
  }
}

/// True when the properties dict carries any integer-indexed entry — i.e. a
/// per-index accessor or attribute override that the elements store can't
/// represent. Such entries force the spec-faithful generic per-element path.
fn properties_have_index_keys(
  properties: dict.Dict(value.PropertyKey, Property),
) -> Bool {
  // Plain arrays have an empty properties dict — skip building the key list.
  !dict.is_empty(properties)
  && list.any(dict.keys(properties), fn(key) {
    case key {
      Index(_) -> True
      Named(_) -> False
    }
  })
}

/// True when any object on the prototype chain starting at `proto` carries an
/// integer-indexed property (in its properties dict or elements store). When
/// False, HasProperty/Get/Set on a missing own index can never observe the
/// prototype chain — no inherited values, no proto getters/setters — so the
/// in-place mutator fast path stays spec-equivalent even across holes and
/// writes to previously-absent indices. The chain is short (usually
/// Array.prototype → Object.prototype) so this is O(proto dict size), paid
/// once per mutator call instead of per element.
fn proto_chain_has_index_keys(heap: Heap, proto: Option(Ref)) -> Bool {
  case proto {
    None -> False
    Some(proto_ref) ->
      case heap.read(heap, proto_ref) {
        // String exotic objects (§10.4.3.5) expose own index properties
        // virtually from [[StringData]] — stored in neither `elements` nor
        // the properties dict — so a non-empty boxed String anywhere on the
        // chain makes holes observable through it.
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) if s != "" ->
          True
        Some(ObjectSlot(properties:, elements: els, prototype:, ..)) ->
          !elements.is_empty(els)
          || properties_have_index_keys(properties)
          || proto_chain_has_index_keys(heap, prototype)
        // Non-object slot can't carry index properties; stop walking.
        _ -> False
      }
  }
}

/// Dense bulk-write fast path for the in-place Array.prototype mutators
/// (pop/push/shift/unshift/reverse/fill/copyWithin/splice/sort write-back).
///
/// The generic spec algorithms do HasProperty + Get + Set/Delete PER MOVED
/// ELEMENT — each a separate heap-dict read-modify-write plus a full
/// ObjectSlot rebuild, so `while (q.length) q.shift()` is O(n²) heap traffic.
/// When every per-index step is guaranteed to be a pure own-element data
/// access, the whole mutation collapses to ONE heap.read, one JsElements
/// transformation, ONE heap.write.
///
/// Eligible iff (checked here, once per call):
///   - the slot is a real ArrayObject whose recorded length still equals
///     `expected_len` (sort's comparefn may have resized the array between
///     snapshot and write-back — fall back if so),
///   - extensible (writes to absent indices must not be rejected),
///   - its properties dict has no Index keys (no per-index accessors or
///     attribute overrides → every present index is plain writable data),
///   - "length" has no non-writable dict override (a frozen length must
///     reject mutators through the generic Set(O, "length", ..., true) path —
///     the fast path writes ArrayObject(new_length) unconditionally),
///   - no prototype-chain object has index-keyed properties (holes and
///     beyond-length writes can't observe proto getters/setters).
///
/// `transform` receives the current #(elements, length) and returns the new
/// #(elements, new_length, payload). Returns None when ineligible — caller
/// falls back to the spec-faithful generic loop.
fn try_elements_fast_path(
  state: State,
  ref: Ref,
  expected_len: Int,
  transform: fn(JsElements, Int) -> #(JsElements, Int, payload),
) -> Option(#(payload, State)) {
  case heap.read(state.heap, ref) {
    Some(
      ObjectSlot(
        kind: ArrayObject(length:),
        properties:,
        elements: els,
        prototype:,
        extensible: True,
        ..,
      ) as slot,
    ) -> {
      // Object.defineProperty(a, "length", {writable: false}) stores a
      // Named("length") dict override; mutators must then fail through the
      // generic path (Set throws TypeError) instead of writing the length.
      let length_writable = case dict.get(properties, Named("length")) {
        Ok(DataProperty(writable:, ..)) -> writable
        _ -> True
      }
      let eligible =
        length == expected_len
        && length_writable
        && !properties_have_index_keys(properties)
        && !proto_chain_has_index_keys(state.heap, prototype)
      case eligible {
        False -> None
        True -> {
          let #(els, new_length, payload) = transform(els, length)
          let heap =
            heap.write(
              state.heap,
              ref,
              ObjectSlot(..slot, kind: ArrayObject(new_length), elements: els),
            )
          Some(#(payload, State(..state, heap:)))
        }
      }
    }
    _ -> None
  }
}

/// Dense append fast path for Array.prototype.push.
///
/// Unlike try_elements_fast_path — which must rule out index keys ANYWHERE
/// (pop/shift/splice read, move, and delete arbitrary indices, so any hole
/// can observe the prototype chain) — push only CREATES the previously-absent
/// own indices [len, len + argCount). Set on such an index can only observe:
///   - a receiver dict override at that exact index (accessor / attributes),
///   - a prototype-chain own property at that exact index (setter / getter),
///   - a non-writable or dict-overridden "length",
///   - a non-extensible receiver.
/// Checking exactly those is O(argCount) map lookups instead of a scan over
/// every prototype-chain property dict, so it wins even for a single-element
/// push (the generic path does two full OrdinarySet operations: element write
/// + length write).
fn try_push_fast_path(
  state: State,
  ref: Ref,
  expected_len: Int,
  args: List(JsValue),
) -> Option(#(Int, State)) {
  case heap.read(state.heap, ref) {
    Some(
      ObjectSlot(
        kind: ArrayObject(length:),
        properties:,
        elements: els,
        prototype:,
        extensible: True,
        ..,
      ) as slot,
    ) -> {
      let arg_count = list.length(args)
      // Object.defineProperty(a, "length", {writable: false}) stores a
      // Named("length") dict override; push must then fail through the
      // generic Set(O, "length", ...) path (TypeError) instead of writing
      // ArrayObject(new_length) unconditionally.
      let length_writable = case dict.get(properties, Named("length")) {
        Ok(DataProperty(writable:, ..)) -> writable
        _ -> True
      }
      let eligible =
        length == expected_len
        && length_writable
        && !dict_has_index_in_range(properties, length, arg_count)
        && !proto_chain_has_index_in_range(
          state.heap,
          prototype,
          length,
          arg_count,
        )
      case eligible {
        False -> None
        True -> {
          let new_length = length + arg_count
          let heap =
            heap.write(
              state.heap,
              ref,
              ObjectSlot(
                ..slot,
                kind: ArrayObject(new_length),
                elements: elements.write_list(els, length, args),
              ),
            )
          Some(#(new_length, State(..state, heap:)))
        }
      }
    }
    _ -> None
  }
}

/// True when the properties dict carries an Index key in
/// [start, start + count). Per-index dict.get instead of scanning the whole
/// key set — Array.prototype/Object.prototype hold dozens of Named keys.
fn dict_has_index_in_range(
  properties: dict.Dict(value.PropertyKey, Property),
  start: Int,
  count: Int,
) -> Bool {
  // Plain arrays/objects have an empty properties dict — skip the lookups.
  !dict.is_empty(properties)
  && dict_index_in_range_loop(properties, start, start + count)
}

fn dict_index_in_range_loop(
  properties: dict.Dict(value.PropertyKey, Property),
  idx: Int,
  end: Int,
) -> Bool {
  case idx >= end {
    True -> False
    False ->
      case dict.get(properties, Index(idx)) {
        Ok(_) -> True
        Error(Nil) -> dict_index_in_range_loop(properties, idx + 1, end)
      }
  }
}

/// Range-restricted variant of proto_chain_has_index_keys: True when any
/// object on the chain carries an own index property in
/// [start, start + count) — the only indices a push can observe. Mirrors the
/// clause structure of proto_chain_has_index_keys exactly (non-empty boxed
/// String → conservative True; non-object slot stops the walk).
fn proto_chain_has_index_in_range(
  heap: Heap,
  proto: Option(Ref),
  start: Int,
  count: Int,
) -> Bool {
  case proto {
    None -> False
    Some(proto_ref) ->
      case heap.read(heap, proto_ref) {
        // String exotic objects (§10.4.3.5) expose own index properties
        // virtually from [[StringData]] — conservatively bail like
        // proto_chain_has_index_keys does.
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) if s != "" ->
          True
        Some(ObjectSlot(properties:, elements: proto_els, prototype:, ..)) ->
          elements_has_in_range(proto_els, start, count)
          || dict_has_index_in_range(properties, start, count)
          || proto_chain_has_index_in_range(heap, prototype, start, count)
        // Non-object slot can't carry index properties; stop walking.
        _ -> False
      }
  }
}

/// True when the elements store holds a value at any index in
/// [start, start + count).
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

/// True when a hole at `idx` is shadowed by an inherited property — reading
/// it would invoke [[Get]] on the prototype chain (possibly a getter, i.e.
/// user code that can mutate the array), so snapshot loops must fall back to
/// the generic per-element path from that index. has_property is pure (no
/// user code), so a False answer keeps the snapshot valid.
fn hole_is_inherited(state: State, proto: Option(Ref), idx: Int) -> Bool {
  case proto {
    None -> False
    Some(proto_ref) ->
      object.has_property(state.heap, proto_ref, index_key(idx))
  }
}

/// LengthOfArrayLike (ES2024 §7.3.18) — pure approximation.
///
/// §7.3.18 LengthOfArrayLike ( obj ):
///   1. Return ℝ(? ToLength(? Get(obj, "length"))).
///
/// Uses object.get_value to support accessor-valued "length" and prototype
/// chain lookups.
///
/// ToLength (§7.1.17):
///   1. Let len be ? ToIntegerOrInfinity(argument).
///   2. If len ≤ 0, return +0𝔽.
///   3. Return 𝔽(min(len, 2^53 - 1)).
/// LengthOfArrayLike (§7.3.18) for an arbitrary object ref — dispatches on
/// the slot kind the same way require_array does, without the ToObject /
/// TypeError prologue. Used by concat's spread path, where the spread target
/// is an argument rather than the receiver.
fn object_length(
  state: State,
  ref: value.Ref,
) -> Result(#(Int, State), #(JsValue, State)) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: ArrayObject(length:), ..)) -> Ok(#(length, state))
    // Arguments objects take the generic path: "length" is an ordinary
    // writable/configurable dict property (§10.4.4.6) and overrides must be
    // honored (e.g. concat spreading after defineProperty length).
    Some(ObjectSlot(kind: value.StringObject(value: s), ..)) ->
      Ok(#(object.string_length(s), state))
    Some(ObjectSlot(properties:, ..)) ->
      length_of_properties(state, ref, properties)
    _ -> Ok(#(0, state))
  }
}

fn length_of_properties(
  state: State,
  ref: value.Ref,
  properties: dict.Dict(value.PropertyKey, Property),
) -> Result(#(Int, State), #(JsValue, State)) {
  // Fast path: own data property — no user code can run on the Get itself,
  // but ToLength may still call valueOf on an object-valued length.
  case dict.get(properties, Named("length")) {
    Ok(DataProperty(value: len_val, ..)) -> to_length_value(state, len_val)
    // Accessor or missing: full [[Get]] (getters + prototype chain). Getter
    // exceptions propagate; getter side effects are kept via the threaded
    // state (§7.3.18 LengthOfArrayLike step order matters for test262).
    _ -> {
      use #(len_val, state) <- result.try(object.get_value(
        state,
        ref,
        Named("length"),
        JsObject(ref),
      ))
      to_length_value(state, len_val)
    }
  }
}

/// ES2024 §7.1.17 ToLength(argument) with full ToNumber coercion —
/// may invoke valueOf/toString on objects (user code), so it threads state
/// and can throw.
///
///   1. Let len be ? ToIntegerOrInfinity(argument).
///   2. If len ≤ 0, return +0𝔽.
///   3. Return 𝔽(min(len, 2^53 - 1)).
fn to_length_value(
  state: State,
  val: JsValue,
) -> Result(#(Int, State), #(JsValue, State)) {
  case val {
    // String lengths go through StringToNumber's full grammar (hex/octal/
    // binary prefixes) — helpers.to_number_int implements it.
    JsString(_) -> Ok(#(to_length(val), state))
    _ -> {
      use #(num, state) <- result.map(coerce.js_to_number(state, val))
      let len = case num {
        value.Finite(f) ->
          int.max(0, int.min(value.float_to_int(f), limits.max_safe_integer))
        // §7.1.5 step 3: +∞ → §7.1.17 step 3 clamps to 2^53 - 1.
        value.Infinity -> limits.max_safe_integer
        // NaN / -∞ / anything non-positive → +0.
        _ -> 0
      }
      #(len, state)
    }
  }
}

/// Allocate a RangeError in the op-result shape `Result(a, #(JsValue, State))`
/// used by the generic per-index loops.
fn range_error_op(state: State, msg: String) -> Result(a, #(JsValue, State)) {
  let #(state, res) = state.range_error(state, msg)
  case res {
    Error(err) -> Error(#(err, state))
    // state.range_error always returns Error; this arm is unreachable but
    // keeps the case exhaustive without an assert.
    Ok(val) -> Error(#(val, state))
  }
}

/// Pragmatic iteration bound for the generic per-index loops. ToLength allows
/// lengths up to 2^53-1; spec-conformant programs over huge array-likes
/// terminate early (element found / falsy callback / start near the end) long
/// before this budget. Anything that would actually perform >max_iteration
/// generic heap operations gets a RangeError instead of hanging the BEAM.
/// QuickJS lets such loops spin; we don't, since BEAM recursion is heavier
/// than a C for-loop.
const iteration_budget_msg = "Invalid array length"

/// ES2024 §7.1.17 ToLength(argument)
fn to_length(val: JsValue) -> Int {
  case helpers.to_number_int(val) {
    Some(n) if n > 0 -> int.min(n, limits.max_safe_integer)
    // helpers.to_number_int collapses NaN and ±Infinity to None. §7.1.4.1.1
    // StringNumericLiteral parses "Infinity"/"+Infinity" to +∞, which §7.1.17
    // step 3 clamps to 2^53-1 — distinguish it from the NaN/-∞ → 0 bucket.
    None ->
      case val {
        JsNumber(value.Infinity) -> limits.max_safe_integer
        JsString(s) ->
          case string.trim(s) {
            "Infinity" | "+Infinity" -> limits.max_safe_integer
            _ -> 0
          }
        _ -> 0
      }
    _ -> 0
  }
}

/// IsCallable check + argument extraction for Array.prototype callback methods.
///
/// Most Array.prototype iteration methods (forEach, map, filter, every, some,
/// find, findIndex, reduce, etc.) share a common preamble:
///
///   1. Let callbackfn be args[0].
///   2. If IsCallable(callbackfn) is false, throw a TypeError.  (§7.2.3)
///   3. Let thisArg be args[1] (or undefined if absent).
///
/// IsCallable (ES2024 §7.2.3):
///   1. If argument is not an Object, return false.
///   2. If argument has a [[Call]] internal method, return true.
///   3. Return false.
///
/// The TypeError message follows V8/Node convention: "<type> is not a function".
fn require_callback(
  args: List(JsValue),
  state: State,
  cont: fn(JsValue, JsValue, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  // Extract callbackfn (args[0]) and thisArg (args[1], default undefined).
  let #(cb, this_arg) = case args {
    [c, t, ..] -> #(c, t)
    [c] -> #(c, JsUndefined)
    [] -> #(JsUndefined, JsUndefined)
  }
  // §7.2.3 IsCallable: check [[Call]] internal method.
  case helpers.is_callable(state.heap, cb) {
    True -> cont(cb, this_arg, state)
    // Step 2: If IsCallable(callbackfn) is false, throw a TypeError exception.
    False ->
      state.type_error(
        state,
        common.typeof_value(cb, state.heap) <> " is not a function",
      )
  }
}

/// Relative index resolution used by Array.prototype.{slice,fill,copyWithin,
/// splice,at,indexOf,lastIndexOf,flat,flatMap,etc.}.
///
/// Implements the common "relative index" clamping pattern found throughout
/// §23.1.3.*. Many array methods contain steps like:
///
///   Let relativeStart be ? ToIntegerOrInfinity(start).    (§7.1.5)
///   If relativeStart = -∞, let k = 0.
///   Else if relativeStart < 0, let k = max(len + relativeStart, 0).
///   Else, let k = min(relativeStart, len).
///
/// ToIntegerOrInfinity (§7.1.5):
///   1. Let number be ? ToNumber(argument).
///   2. If number is NaN, +0, or -0, return 0.
///   3. If number is +∞, return +∞. If -∞, return -∞.
///   4. Return truncate(number).
///
/// Parameters:
///   arg     — the raw JS argument (e.g. args[1] for slice's start)
///   len     — the array length (for relative-to-end computation)
///   default — value to use when arg is undefined (spec says different defaults
///             for different methods: 0 for start, len for end, etc.)
fn try_resolve_index(
  state: State,
  arg: JsValue,
  len: Int,
  default: Int,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case arg {
    // Undefined args use the caller-specified default (e.g. slice(start)
    // with no end → end defaults to len, not 0). ToNumber(undefined) has no
    // observable steps, so skipping the coercion is spec-equivalent.
    JsUndefined -> cont(default, state)
    _ -> {
      // §7.1.5 ToIntegerOrInfinity(arg) — observable: ToPrimitive may run
      // user valueOf/toString/@@toPrimitive or throw (Symbol/BigInt).
      use raw, state <- try_integer_or_infinity(state, arg)
      // Relative index clamping (common pattern across §23.1.3.*):
      //   If relativeIndex < 0, let k = max(len + relativeIndex, 0).
      //   Else, let k = min(relativeIndex, len).
      let k = case raw < 0 {
        True -> int.max(len + raw, 0)
        False -> int.min(raw, len)
      }
      cont(k, state)
    }
  }
}

/// CPS ToIntegerOrInfinity (ES2024 §7.1.5): full ToNumber — including
/// ToPrimitive (valueOf/toString/@@toPrimitive) on objects, which can run
/// user code or throw, and TypeError on Symbol/BigInt — then truncate, with
/// ±∞ saturated to ±(2^53 - 1) so downstream clamps behave like the spec's
/// explicit ±∞ branches (array lengths never exceed 2^53 - 1).
fn try_integer_or_infinity(
  state: State,
  arg: JsValue,
  cont: fn(Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  use num, state <- coerce.try_to_number(state, arg)
  let raw = case num {
    // §7.1.5 step 5: truncate(number)
    Finite(f) -> value.float_to_int(f)
    // §7.1.5 step 2: NaN → +0
    value.NaN -> 0
    // §7.1.5 steps 3-4: ±∞, saturated for Int arithmetic
    value.Infinity -> limits.max_safe_integer
    value.NegInfinity -> -limits.max_safe_integer
  }
  cont(raw, state)
}

/// args[n], or undefined when absent — for optional trailing arguments.
fn arg_or_undefined(args: List(JsValue), n: Int) -> JsValue {
  case list.drop(args, n) {
    [v, ..] -> v
    [] -> JsUndefined
  }
}

/// Shared steps 7-10 of splice (§23.1.3.29) / step 7 of toSpliced
/// (§23.1.3.35): actualDeleteCount/actualSkipCount + trailing items,
/// determined by argument COUNT:
///   - 0 args: deleteCount = 0, items = []
///   - 1 arg (start only): deleteCount = len - actualStart
///   - 2+ args: clamp(ToIntegerOrInfinity(deleteCount), 0, len - actualStart),
///     items = remaining args. The coercion is observable (can run user code
///     or throw on Symbol/BigInt).
fn try_delete_count(
  state: State,
  args: List(JsValue),
  length: Int,
  actual_start: Int,
  cont: fn(#(Int, List(JsValue)), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [] -> cont(#(0, []), state)
    [_] -> cont(#(length - actual_start, []), state)
    [_, dc_val, ..rest] -> {
      use dc, state <- try_integer_or_infinity(state, dc_val)
      cont(#(int.clamp(dc, 0, length - actual_start), rest), state)
    }
  }
}

/// Set (ES2024 §7.3.4) — with Throw = true.
///
/// §7.3.4 Set ( O, P, V, Throw ):
///   1. Let success be ? O.[[Set]](P, V, O).
///   2. If success is false and Throw is true, throw a TypeError exception.
///   3. Return unused.
///
/// We always pass Throw=true because Array.prototype methods operate in
/// strict-mode-equivalent semantics (the spec says "Perform ? Set(..., true)"
/// for every mutating array method).
///
/// object.set_value implements the [[Set]] internal method (§10.1.9) which
/// walks the prototype chain, invokes setters, and returns a Bool indicating
/// success. The receiver is JsObject(ref) — i.e. the object itself.
///
/// Takes a canonical PropertyKey directly so per-element loops avoid the
/// int → string → canonical_key → Index(int) roundtrip.
fn generic_set(
  state: State,
  ref: Ref,
  key: value.PropertyKey,
  val: JsValue,
) -> Result(State, #(JsValue, State)) {
  // §7.3.4 step 1: Let success be ? O.[[Set]](P, V, O).
  {
    use #(state, success) <- result.try(object.set_value(
      state,
      ref,
      key,
      val,
      JsObject(ref),
    ))
    case success {
      // success = true → return normally.
      True -> Ok(state)
      // §7.3.4 step 2: success = false and Throw = true → TypeError.
      False ->
        coerce.thrown_type_error(
          state,
          "Cannot assign to read only property '"
            <> value.key_to_string(key)
            <> "' of object",
        )
    }
  }
}

/// Convenience: Set(O, ! ToString(𝔽(index)), V, true).
///
/// Array.prototype methods address elements by numeric index via
/// ToString(𝔽(k)), e.g. §23.1.3.22 Array.prototype.push step 5:
///   "Perform ? Set(O, ! ToString(𝔽(len)), E, true)."
///
/// Passes Index(idx) directly — no string allocation on the hot path.
fn generic_set_index(
  state: State,
  ref: Ref,
  idx: Int,
  val: JsValue,
) -> Result(State, #(JsValue, State)) {
  generic_set(state, ref, index_key(idx), val)
}

/// Convenience: Set(O, "length", 𝔽(len), true).
///
/// Nearly every mutating Array.prototype method ends with a "length" update,
/// e.g. §23.1.3.22 Array.prototype.push step 7:
///   "Perform ? Set(O, "length", 𝔽(len), true)."
///
/// The length is set as a Number (not integer) per spec — 𝔽(len).
fn generic_set_length(
  state: State,
  ref: Ref,
  len: Int,
) -> Result(State, #(JsValue, State)) {
  generic_set(state, ref, Named("length"), value.from_int(len))
}

/// DeletePropertyOrThrow (ES2024 §7.3.9).
///
/// §7.3.9 DeletePropertyOrThrow ( O, P ):
///   1. Let success be ? O.[[Delete]](P).
///   2. If success is false, throw a TypeError exception.
///   3. Return unused.
///
/// Used by Array.prototype methods that remove elements, e.g.
/// §23.1.3.21 Array.prototype.pop step 5:
///   "Perform ? DeletePropertyOrThrow(O, ! ToString(𝔽(newLen)))."
///
/// object.delete_property implements [[Delete]] (§10.1.10):
///   1. Let desc be ? O.[[GetOwnProperty]](P).
///   2. If desc is undefined, return true.
///   3. If desc.[[Configurable]] is true, remove P and return true.
///   4. Return false.
fn generic_delete(
  state: State,
  ref: Ref,
  key: value.PropertyKey,
) -> Result(State, #(JsValue, State)) {
  // §7.3.9 step 1: Let success be ? O.[[Delete]](P) — trap-aware so a Proxy
  // "deleteProperty" trap runs (it can record the call and throw).
  use #(state, ok) <- result.try(object.delete_property_stateful(
    state,
    ref,
    object.PkString(key),
  ))
  case ok {
    // success = true → return normally.
    True -> Ok(state)
    // §7.3.9 step 2: success = false → throw TypeError.
    False ->
      coerce.thrown_type_error(
        state,
        "Cannot delete property '" <> value.key_to_string(key) <> "' of object",
      )
  }
}

/// Convenience: DeletePropertyOrThrow(O, ! ToString(𝔽(index))).
fn generic_delete_index(
  state: State,
  ref: Ref,
  idx: Int,
) -> Result(State, #(JsValue, State)) {
  generic_delete(state, ref, index_key(idx))
}

/// HasProperty (§7.3.11) by integer index, trap-aware: routes Proxy "has"
/// traps (which run user code and can throw) via has_property_stateful.
/// The mutating generic loops (reverse/shift/unshift/splice) use this so a
/// proxied array-like observes its [[HasProperty]] calls in spec order.
fn generic_has_op(
  state: State,
  ref: Ref,
  idx: Int,
) -> Result(#(Bool, State), #(JsValue, State)) {
  object.has_property_stateful(state, ref, object.PkString(index_key(idx)))
}

/// Get (ES2024 §7.3.2).
///
/// §7.3.2 Get ( O, P ):
///   1. Return ? O.[[Get]](P, O).
///
/// The receiver argument is O itself (the object being read from).
/// object.get_value implements [[Get]] (§10.1.8 OrdinaryGet) which walks the
/// prototype chain and invokes getter accessors.
///
/// Used by iteration methods to read elements, e.g.
/// §23.1.3.13 Array.prototype.forEach step 6.c.ii:
///   "Let kValue be ? Get(O, Pk)."
fn generic_get(
  state: State,
  ref: Ref,
  idx: Int,
) -> Result(#(JsValue, State), #(JsValue, State)) {
  // §7.3.2 step 1: O.[[Get]](! ToString(𝔽(idx)), O)
  object.get_value(state, ref, index_key(idx), JsObject(ref))
}

// ============================================================================
// Non-callback methods (no VM re-entry needed)
// ============================================================================

/// Array.prototype.pop() — remove and return the last element.
/// Generic: Get(O, len-1), DeletePropertyOrThrow(O, len-1), Set(O, "length", len-1, true).
/// Array.prototype.pop ( )
/// ES2024 §23.1.3.21
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. If len = 0, then
///    a. Perform ? Set(O, "length", +0𝔽, true).
///    b. Return undefined.
/// 4. Else,
///    a. Assert: len > 0.
///    b. Let newLen be 𝔽(len - 1).
///    c. Let index be ! ToString(newLen).
///    d. Let element be ? Get(O, index).
///    e. Perform ? DeletePropertyOrThrow(O, index).
///    f. Perform ? Set(O, "length", newLen, true).
///    g. Return element.
///
fn array_pop(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _this, ref, length, state <- require_array(this, state)
  case length == 0 {
    // Step 3: len = 0
    // Step 3a: Set(O, "length", +0𝔽, true)
    // Step 3b: Return undefined
    True -> wrap(generic_set_length(state, ref, 0), JsUndefined)
    // Step 4: len > 0
    False -> {
      // Step 4b: newLen = len - 1
      let new_len = length - 1
      // Fast path: Get + Delete + length update fused into one heap
      // read-modify-write (truncate covers steps 4e-4f).
      let fast = {
        use els, len <- try_elements_fast_path(state, ref, length)
        #(elements.truncate(els, len - 1), len - 1, elements.get(els, len - 1))
      }
      case fast {
        Some(#(val, state)) -> #(state, Ok(val))
        None -> {
          // Step 4d: element = Get(O, ToString(newLen))
          use val, state <- state.try_op(generic_get(state, ref, new_len))
          // Step 4e: DeletePropertyOrThrow(O, index)
          use state <- state.try_state(generic_delete_index(state, ref, new_len))
          // Step 4f: Set(O, "length", newLen, true)
          // Step 4g: Return element
          wrap(generic_set_length(state, ref, new_len), val)
        }
      }
    }
  }
}

/// Array.prototype.shift ( )
/// ES2024 §23.1.3.25
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. If len = 0, then
///    a. Perform ? Set(O, "length", +0𝔽, true).
///    b. Return undefined.
/// 4. Let first be ? Get(O, "0").
/// 5. Let k be 1.
/// 6. Repeat, while k < len,
///    a. Let from be ! ToString(𝔽(k)).
///    b. Let to be ! ToString(𝔽(k - 1)).
///    c. Let fromPresent be ? HasProperty(O, from).
///    d. If fromPresent is true, then
///       i. Let fromVal be ? Get(O, from).
///       ii. Perform ? Set(O, to, fromVal, true).
///    e. Else,
///       i. Perform ? DeletePropertyOrThrow(O, to).
///    f. Set k to k + 1.
/// 7. Perform ? DeletePropertyOrThrow(O, ! ToString(𝔽(len - 1))).
/// 8. Perform ? Set(O, "length", 𝔽(len - 1), true).
/// 9. Return first.
///
fn array_shift(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _this, ref, length, state <- require_array(this, state)
  case length == 0 {
    // Step 3: len = 0
    // Step 3a: Set(O, "length", +0𝔽, true)
    // Step 3b: Return undefined
    True -> wrap(generic_set_length(state, ref, 0), JsUndefined)
    False -> {
      // Fast path: the whole Get + shift-down loop + trailing delete +
      // length update is one heap read, one elements transform, one heap
      // write — instead of 3-4 heap ops per element.
      let fast = {
        use els, len <- try_elements_fast_path(state, ref, length)
        let first = elements.get(els, 0)
        let els =
          elements.move_down(els, 1, len, 1) |> elements.truncate(len - 1)
        #(els, len - 1, first)
      }
      case fast {
        Some(#(first, state)) -> #(state, Ok(first))
        None -> {
          // Step 4: first = Get(O, "0")
          use val, state <- state.try_op(generic_get(state, ref, 0))
          // Steps 5-6: shift indices [1..len) down by 1
          use state <- state.try_state(move_range(
            state,
            ref,
            1,
            length,
            1,
            -1,
            limits.max_iteration,
          ))
          // Step 7: DeletePropertyOrThrow(O, ToString(len - 1))
          use state <- state.try_state(generic_delete_index(
            state,
            ref,
            length - 1,
          ))
          // Step 8: Set(O, "length", len - 1, true)
          // Step 9: Return first
          wrap(generic_set_length(state, ref, length - 1), val)
        }
      }
    }
  }
}

/// Generic slow-path element-move loop shared by shift (§23.1.3.25 steps
/// 5-6), unshift (§23.1.3.33 steps 4b-4c), and splice (§23.1.3.31).
///
/// Moves each element from index k to k + delta, stepping k by `step` (+1 or
/// -1) until the bound is passed: ascending stops when k >= stop, descending
/// stops when k < stop. Each iteration follows the spec pattern:
///   - If HasProperty(O, from): Set(O, to, Get(O, from), true)
///   - Else: DeletePropertyOrThrow(O, to)
fn move_range(
  state: State,
  ref: Ref,
  k: Int,
  stop: Int,
  step: Int,
  delta: Int,
  fuel: Int,
) -> Result(State, #(JsValue, State)) {
  let done = case step > 0 {
    True -> k >= stop
    False -> k < stop
  }
  case done {
    True -> Ok(state)
    False -> {
      // Pragmatic per-step budget (see iteration_budget_msg): a getter or
      // proxy trap on a huge array-like throws within its first few steps,
      // so only loops doing real unbounded work hit the RangeError.
      use <- bool.lazy_guard(fuel <= 0, fn() {
        range_error_op(state, iteration_budget_msg)
      })
      let to = k + delta
      use #(has_k, state) <- result.try(generic_has_op(state, ref, k))
      case has_k {
        True -> {
          use #(val, state) <- result.try(generic_get(state, ref, k))
          use state <- result.try(generic_set_index(state, ref, to, val))
          move_range(state, ref, k + step, stop, step, delta, fuel - 1)
        }
        False -> {
          use state <- result.try(generic_delete_index(state, ref, to))
          move_range(state, ref, k + step, stop, step, delta, fuel - 1)
        }
      }
    }
  }
}

/// Array.prototype.unshift ( ...items )
/// ES2024 §23.1.3.33
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. Let argCount be the number of elements in items.
/// 4. If argCount > 0, then
///    a. If len + argCount > 2^53 - 1, throw a TypeError exception.
///    b. Let k be len.
///    c. Repeat, while k > 0,
///       i. Let from be ! ToString(𝔽(k - 1)).
///       ii. Let to be ! ToString(𝔽(k + argCount - 1)).
///       iii. Let fromPresent be ? HasProperty(O, from).
///       iv. If fromPresent is true, then
///           1. Let fromValue be ? Get(O, from).
///           2. Perform ? Set(O, to, fromValue, true).
///       v. Else,
///           1. Perform ? DeletePropertyOrThrow(O, to).
///       vi. Set k to k - 1.
///    d. Let j be +0𝔽.
///    e. For each element E of items, do
///       i. Perform ? Set(O, ! ToString(j), E, true).
///       ii. Set j to j + 1𝔽.
/// 5. Perform ? Set(O, "length", 𝔽(len + argCount), true).
/// 6. Return 𝔽(len + argCount).
///
fn array_unshift(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use this, ref, length, state <- require_array(this, state)
  let arg_count = list.length(args)
  let new_len = length + arg_count
  // §23.1.3.33 step 5 runs even when argCount = 0: Set(O, "length", 𝔽(len))
  // is observable (length clamped by ToLength on array-likes; TypeError on a
  // non-writable length such as a String exotic's). For other primitives the
  // Set lands on a throwaway wrapper and succeeds unobservably — skip it.
  use <- bool.lazy_guard(arg_count == 0, fn() {
    case this {
      JsObject(_) | JsString(_) ->
        wrap(generic_set_length(state, ref, new_len), value.from_int(new_len))
      _ -> #(state, Ok(value.from_int(new_len)))
    }
  })
  // §23.1.3.33 step 4a: If len + argCount > 2^53 - 1, throw TypeError
  use <- state.guard_safe_length(state, new_len)
  // Fast path: shift-up loop + item writes + length update fused into one
  // heap read-modify-write.
  let fast = {
    use els, len <- try_elements_fast_path(state, ref, length)
    let els =
      elements.move_up(els, 0, len, arg_count) |> elements.write_list(0, args)
    #(els, len + arg_count, Nil)
  }
  case fast {
    Some(#(Nil, state)) -> #(state, Ok(value.from_int(new_len)))
    None -> {
      // Steps 4b-4c: shift indices [0..len) up by argCount, right-to-left
      use state <- state.try_state(move_range(
        state,
        ref,
        length - 1,
        0,
        -1,
        arg_count,
        limits.max_iteration,
      ))
      use state <- state.try_state(write_list_at(state, ref, 0, args))
      wrap(generic_set_length(state, ref, new_len), value.from_int(new_len))
    }
  }
}

/// Implements the item-writing loop from Array.prototype.unshift (§23.1.3.33 steps 4d-4e).
///
/// Corresponds to the spec's:
///   4d. Let j be +0𝔽.
///   4e. For each element E of items, do
///       i. Perform ? Set(O, ! ToString(j), E, true).
///       ii. Set j to j + 1𝔽.
///
/// Also used by Array.prototype.splice for inserting new elements at the
/// splice point (§23.1.3.30 steps 12-13, analogous pattern).
fn write_list_at(
  state: State,
  ref: Ref,
  idx: Int,
  vals: List(JsValue),
) -> Result(State, #(JsValue, State)) {
  case vals {
    // All items written
    [] -> Ok(state)
    [v, ..rest] -> {
      // Step 4e.i: Set(O, ToString(j), E, true)
      use state <- result.try(generic_set_index(state, ref, idx, v))
      // Step 4e.ii: j = j + 1
      write_list_at(state, ref, idx + 1, rest)
    }
  }
}

/// Utility: convert a generic op Result (from generic_set, generic_delete, etc.)
/// into the #(State, Result(JsValue, JsValue)) return format used by builtins.
/// On success, returns the given `val` as the Ok result. On error, propagates
/// the thrown value. Not a spec operation — purely internal plumbing.
fn wrap(
  r: Result(State, #(JsValue, State)),
  val: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  case r {
    Ok(state) -> #(state, Ok(val))
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Array.prototype.slice (ES2024 §23.1.3.25)
///
/// §23.1.3.25 Array.prototype.slice ( start, end ):
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. Let relativeStart be ? ToIntegerOrInfinity(start).
///   4. If relativeStart = -∞, let k = 0.
///   5. Else if relativeStart < 0, let k = max(len + relativeStart, 0).
///   6. Else, let k = min(relativeStart, len).
///   7. If end is undefined, let relativeEnd = len; else let relativeEnd = ? ToIntegerOrInfinity(end).
///   8. If relativeEnd = -∞, let final = 0.
///   9. Else if relativeEnd < 0, let final = max(len + relativeEnd, 0).
///  10. Else, let final = min(relativeEnd, len).
///  11. Let count = max(final - k, 0).
///  12. Let A be ? ArraySpeciesCreate(O, count).
///  13. Let n = 0.
///  14. Repeat, while k < final,
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kPresent be ? HasProperty(O, Pk).
///      c. If kPresent is true, then
///         i. Let kValue be ? Get(O, Pk).
///         ii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), kValue).
///      d. Set k to k + 1.
///      e. Set n to n + 1.
///  15. Perform ? Set(A, "length", 𝔽(n), true).
///  16. Return A.
///
/// Simplifications:
///   - require_array collapses steps 1-2 (ToObject + LengthOfArrayLike).
///   - Steps 3-10 are handled by resolve_index (see §7.1.22 / clamp logic).
///   - Step 12: we skip ArraySpeciesCreate and always create a plain Array.
///     This means @@species is not respected (a known simplification).
///   - Steps 14b-14c: copy_range uses get_index_if_present for HasProperty+Get
///     on the source, preserving holes (sparse indices are not copied).
///   - Step 15: length is set via ArrayObject(count) in the slot constructor
///     rather than a separate Set("length") call.
fn array_slice(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O).
  use this, _ref, length, state <- require_array(this, state)
  // Steps 3-6: relativeStart → k (clamped). Default 0 if no arg.
  use start, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 0),
    length,
    0,
  )
  // Steps 7-10: relativeEnd → final (clamped). Default len if no end arg.
  use end, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 1),
    length,
    length,
  )
  // Step 11: count = max(final - k, 0).
  let count = int.max(end - start, 0)
  // Step 12: A = ArraySpeciesCreate(O, count) — before the copy loop.
  use species, state <- state.try_op(array_species_create(state, this, count))
  // Steps 13-14: copy elements [k..final) into the result.
  // Holes (kPresent = false) are preserved by copy_range skipping them.
  use copied, state <- state.try_op(copy_range(
    state,
    this,
    start,
    0,
    count,
    elements.new(),
  ))
  case species {
    None -> {
      let #(heap, ref) =
        common.alloc_array_from_elements(state.heap, copied, count, array_proto)
      // Step 16: Return A.
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    Some(target) ->
      // Step 15: Perform ? Set(A, "length", 𝔽(n), true).
      case write_species_result(state, target, copied, count, Some(count)) {
        Ok(state) -> #(state, Ok(JsObject(target)))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
  }
}

/// Internal helper implementing the element-copying loop shared by
/// Array.prototype.slice (§23.1.3.25 step 14) and Array.prototype.concat
/// (§23.1.3.1 step 5.c.iii).
///
/// Corresponds to the spec's "Repeat, while k < final" loop:
///   a. Let Pk be ! ToString(𝔽(k)).
///   b. Let kPresent be ? HasProperty(O, Pk).
///   c. If kPresent is true, then
///      i. Let kValue be ? Get(O, Pk).
///      ii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), kValue).
///   d. Set k to k + 1. / e. Set n to n + 1.
///
/// When kPresent is false (a hole), we skip writing to dst — this preserves
/// sparse array structure in the result, matching spec behavior.
///
/// Plain arrays with no index overrides take copy_range_snapshot (one heap
/// read for the whole loop); everything else takes the generic per-element
/// path.
/// Copy a range via unconditional Get — holes read as undefined (through
/// the prototype chain) and every destination index gets a value, so the
/// result is DENSE. Used by the change-array-by-copy methods (§23.1.3.35
/// toSpliced step 15.b.ii / §23.1.3.39 with step 9.c: fromValue =
/// ? Get(O, Pk) with no HasProperty gate — holes are NOT preserved).
fn copy_range_dense(
  state: State,
  src: JsValue,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
) -> Result(#(JsElements, State), #(JsValue, State)) {
  use <- bool.lazy_guard(remaining > limits.max_iteration, fn() {
    range_error_op(state, iteration_budget_msg)
  })
  case remaining <= 0 {
    True -> Ok(#(dst, state))
    False -> {
      use #(val, state) <- result.try(get_index(state, src, src_idx))
      copy_range_dense(
        state,
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
  state: State,
  src: JsValue,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
) -> Result(#(JsElements, State), #(JsValue, State)) {
  // Pragmatic step budget (see iteration_budget_msg): per-step fuel rather
  // than an up-front bound so an abrupt completion from a poisoned getter at
  // a low index surfaces before the budget RangeError, matching the spec's
  // evaluation order on huge spreadable array-likes.
  copy_range_fueled(
    state,
    src,
    src_idx,
    dst_idx,
    remaining,
    dst,
    limits.max_iteration,
  )
}

fn copy_range_fueled(
  state: State,
  src: JsValue,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
  fuel: Int,
) -> Result(#(JsElements, State), #(JsValue, State)) {
  use <- bool.lazy_guard(fuel <= 0 && remaining > 0, fn() {
    range_error_op(state, iteration_budget_msg)
  })
  case dense_snapshot(state, src) {
    Some(#(els, proto)) ->
      copy_range_snapshot(
        state,
        src,
        els,
        proto,
        src_idx,
        dst_idx,
        remaining,
        dst,
        fuel,
      )
    None ->
      copy_range_generic(state, src, src_idx, dst_idx, remaining, dst, fuel)
  }
}

/// Fast path for copy_range: read elements from a one-time snapshot of the
/// array slot. Nothing on this path runs user code, so the snapshot stays
/// valid for the whole loop. A hole shadowed by an inherited prototype
/// property may hide a getter — bail to the generic path from that index.
fn copy_range_snapshot(
  state: State,
  src: JsValue,
  els: JsElements,
  proto: Option(Ref),
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
  fuel: Int,
) -> Result(#(JsElements, State), #(JsValue, State)) {
  use <- bool.lazy_guard(fuel <= 0 && remaining > 0, fn() {
    range_error_op(state, iteration_budget_msg)
  })
  case remaining <= 0 {
    True -> Ok(#(dst, state))
    False ->
      case elements.get_option(els, src_idx) {
        // Step 14c: kPresent is true — copy the element.
        Some(val) ->
          copy_range_snapshot(
            state,
            src,
            els,
            proto,
            src_idx + 1,
            dst_idx + 1,
            remaining - 1,
            elements.set(dst, dst_idx, val),
            fuel - 1,
          )
        None ->
          case hole_is_inherited(state, proto, src_idx) {
            // kPresent is false (hole): skip — do not set dst[dst_idx].
            False ->
              copy_range_snapshot(
                state,
                src,
                els,
                proto,
                src_idx + 1,
                dst_idx + 1,
                remaining - 1,
                dst,
                fuel - 1,
              )
            // Inherited property — Get may invoke a getter (user code).
            True ->
              copy_range_generic(
                state,
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

/// Generic per-element path for copy_range: re-reads the heap each iteration
/// via get_index_if_present (handles index accessors, inherited properties,
/// string primitives, and arrays mutated mid-copy by getters).
fn copy_range_generic(
  state: State,
  src: JsValue,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
  fuel: Int,
) -> Result(#(JsElements, State), #(JsValue, State)) {
  use <- bool.lazy_guard(fuel <= 0 && remaining > 0, fn() {
    range_error_op(state, iteration_budget_msg)
  })
  case remaining <= 0 {
    True -> Ok(#(dst, state))
    False -> {
      // Step 14b-c: kPresent = HasProperty(O, Pk); if true, kValue = Get(O, Pk)
      // — fused into one heap lookup.
      use #(maybe_val, state) <- result.try(get_index_if_present(
        state,
        src,
        src_idx,
      ))
      case maybe_val {
        // Step 14c: kPresent is true — copy the element.
        Some(val) ->
          copy_range_generic(
            state,
            src,
            src_idx + 1,
            dst_idx + 1,
            remaining - 1,
            elements.set(dst, dst_idx, val),
            fuel - 1,
          )
        // kPresent is false (hole): skip — do not set dst[dst_idx].
        None ->
          copy_range_generic(
            state,
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

/// Array.prototype.concat (ES2024 §23.1.3.1)
///
/// §23.1.3.1 Array.prototype.concat ( ...items ):
///   1. Let O be ? ToObject(this value).
///   2. Let A be ? ArraySpeciesCreate(O, 0).
///   3. Let n = 0.
///   4. Prepend O to items.
///   5. For each element E of items, do
///      a. Let spreadable be ? IsConcatSpreadable(E).
///      b. If spreadable is true, then
///         i. Let len be ? LengthOfArrayLike(E).
///         ii. If n + len > 2^53 - 1, throw a TypeError exception.
///         iii. Let k = 0.
///         iv. Repeat, while k < len,
///             1. Let Pk be ! ToString(𝔽(k)).
///             2. Let exists be ? HasProperty(E, Pk).
///             3. If exists is true, then
///                a. Let subElement be ? Get(E, Pk).
///                b. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), subElement).
///             4. Set n to n + 1.
///             5. Set k to k + 1.
///      c. Else,
///         i. NOTE: E is added as a single item rather than spread.
///         ii. If n >= 2^53 - 1, throw a TypeError exception.
///         iii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), E).
///         iv. Set n to n + 1.
///   6. Perform ? Set(A, "length", 𝔽(n), true).
///   7. Return A.
///
/// Simplifications:
///   - Step 2: ArraySpeciesCreate is skipped; we always create a plain Array.
///     @@species is not respected (known simplification).
///   - Step 5a: IsConcatSpreadable (§7.2.18) checks @@isConcatSpreadable then
///     falls back to IsArray. We simplify: only ArrayObject kinds are spread.
///     This means @@isConcatSpreadable on non-arrays is not honored, and
///     arrays with @@isConcatSpreadable=false are still spread.
///   - Step 5b.ii: The 2^53-1 length overflow check is not implemented.
///   - Steps 5b.iv.2-3: Hole handling done by copy_range (HasProperty check).
///   - Step 6: length is baked into ArrayObject(length) at construction time
///     rather than a separate Set("length") call.
fn array_concat(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Step 1: Let O be ? ToObject(this value) — primitives get a real wrapper
  // so the non-spreadable O lands in the result as its wrapper object
  // (concat/call-with-boolean: result[0] instanceof Boolean).
  case common.to_object(state.heap, state.builtins, this) {
    None -> state.type_error(state, cannot_convert)
    Some(#(heap, this_ref)) -> {
      let state = State(..state, heap:)
      let this = JsObject(this_ref)
      // Step 2: A = ArraySpeciesCreate(O, 0) — before the item loop.
      use species, state <- state.try_op(array_species_create(state, this, 0))
      // Step 4: Prepend O to items.
      let all_items = [this, ..args]
      case species {
        // Steps 3, 5-7: collect into a plain elements store, then create
        // result array A with final length n, return A.
        None -> {
          use #(elems, total), state <- state.try_op(concat_items(
            state,
            all_items,
            elements.new(),
            0,
          ))
          let #(heap, ref) =
            common.alloc_array_from_elements(
              state.heap,
              elems,
              total,
              array_proto,
            )
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
        Some(target) -> {
          // Steps 3, 5-6 (species path): CreateDataPropertyOrThrow
          // interleaved with the per-element reads (step 5.b.iv), then
          // Set(A, "length", 𝔽(n), true).
          use total, state <- state.try_op(concat_items_species(
            state,
            all_items,
            target,
            0,
          ))
          case generic_set_length(state, target, total) {
            Ok(state) -> #(state, Ok(JsObject(target)))
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        }
      }
    }
  }
}

/// Fold concat_item over a list of items, threading state.
fn concat_items(
  state: State,
  items: List(JsValue),
  elems: JsElements,
  pos: Int,
) -> Result(#(#(JsElements, Int), State), #(JsValue, State)) {
  case items {
    [] -> Ok(#(#(elems, pos), state))
    [item, ..rest] -> {
      use #(#(elems, pos), state) <- result.try(concat_item(
        state,
        elems,
        pos,
        item,
      ))
      concat_items(state, rest, elems, pos)
    }
  }
}

/// Concat a single item E into the accumulating elements.
///
/// Implements the per-item logic of §23.1.3.1 step 5:
///   a. Let spreadable be ? IsConcatSpreadable(E).
///   b. If spreadable is true, spread E's elements via copy_range.
///   c. Else, append E as a single element.
///
/// ArraySpeciesCreate (ES2024 §9.4.2.3).
///
///   2. Let isArray be ? IsArray(originalArray); if false → ArrayCreate(length).
///   3. Let C be ? Get(originalArray, "constructor").
///   4. (cross-realm Array check — single realm, skipped)
///   5. If C is an Object: set C to ? Get(C, @@species); if C is null → undefined.
///   6. If C is undefined → ArrayCreate(length).
///   7. If IsConstructor(C) is false, throw a TypeError exception.
///   8. Return ? Construct(C, « 𝔽(length) »).
///
/// Returns None when the caller should allocate a plain Array (steps 2/6, or
/// C is the intrinsic Array constructor — observably identical and far
/// cheaper), Some(target_ref) when a custom species constructor was invoked.
///
/// Deviation: with a custom target, the callback-driven callers (slice, map,
/// filter, flat/flatMap) collect results first and write them after iteration
/// (write_species_result) instead of interleaving CreateDataPropertyOrThrow
/// with callback calls. splice and concat interleave per spec
/// (copy_range_to_species) so abrupt completions from the target's
/// [[DefineOwnProperty]] terminate huge-length copy loops early.
fn array_species_create(
  state: State,
  original: JsValue,
  length: Int,
) -> Result(#(Option(Ref), State), #(JsValue, State)) {
  case original {
    JsObject(_) ->
      // Step 1: isArray = ? IsArray(originalArray) — pierces proxies
      // (IsArray of a proxy is IsArray of its target, §7.2.2), so an
      // array-backed proxy DOES take the constructor/@@species path
      // (test262: slice/map/filter/concat create-proxy.js).
      case object.is_array(state.heap, original) {
        Error(Nil) ->
          coerce.thrown_type_error(
            state,
            "Cannot perform 'IsArray' on a proxy that has been revoked",
          )
        // Step 2: not an array → ArrayCreate(length).
        Ok(False) -> Ok(#(None, state))
        Ok(True) -> {
          // Step 3: C = Get(originalArray, "constructor") — may hit a getter.
          use #(ctor, state) <- result.try(object.get_value_of(
            state,
            original,
            Named("constructor"),
          ))
          // Step 4a: C is the Array constructor of ANOTHER realm → set C to
          // undefined, skipping the @@species read entirely (test262:
          // create-proto-from-ctor-realm-array.js expects no species access).
          let ctor = case ctor {
            JsObject(ctor_ref) ->
              case
                ctor_ref != state.builtins.array.constructor
                && list.any(dict.values(state.ctx.realms), fn(b) {
                  b.array.constructor == ctor_ref
                })
              {
                True -> JsUndefined
                False -> ctor
              }
            _ -> ctor
          }
          // Step 5: object C → C = Get(C, @@species); null → undefined.
          use #(ctor, state) <- result.try(case ctor {
            JsObject(_) -> {
              use #(species, state) <- result.map(object.get_symbol_value_of(
                state,
                ctor,
                value.symbol_species,
              ))
              case species {
                JsNull -> #(JsUndefined, state)
                _ -> #(species, state)
              }
            }
            _ -> Ok(#(ctor, state))
          })
          case ctor {
            // Step 6: undefined → ArrayCreate(length).
            JsUndefined -> Ok(#(None, state))
            // Intrinsic Array constructor → Construct(%Array%, «len») yields
            // a plain array — take the cheap path.
            JsObject(ctor_ref)
              if ctor_ref == state.builtins.array.constructor
            -> Ok(#(None, state))
            _ ->
              case object.is_constructor(state.heap, ctor) {
                // Step 7: not a constructor → TypeError.
                False ->
                  coerce.thrown_type_error(
                    state,
                    "Species constructor is not a constructor",
                  )
                // Step 8: Construct(C, « 𝔽(length) »).
                True -> {
                  use #(created, state) <- result.try(
                    state.construct(state, ctor, [value.from_int(length)]),
                  )
                  case created {
                    JsObject(created_ref) -> Ok(#(Some(created_ref), state))
                    // Constructors return objects; a non-object means the
                    // construct machinery already deviated — fall back.
                    _ -> Ok(#(None, state))
                  }
                }
              }
          }
        }
      }
    _ -> Ok(#(None, state))
  }
}

/// Write collected results into a custom species target:
/// CreateDataPropertyOrThrow(A, ! ToString(𝔽(k)), v) for each present index
/// in [0, length), then optionally Set(A, "length", 𝔽(n), true).
fn write_species_result(
  state: State,
  target: Ref,
  els: JsElements,
  length: Int,
  set_length: Option(Int),
) -> Result(State, #(JsValue, State)) {
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    range_error_op(state, iteration_budget_msg)
  })
  use state <- result.try(write_species_elements(state, target, els, 0, length))
  case set_length {
    None -> Ok(state)
    Some(n) -> generic_set_length(state, target, n)
  }
}

fn write_species_elements(
  state: State,
  target: Ref,
  els: JsElements,
  idx: Int,
  length: Int,
) -> Result(State, #(JsValue, State)) {
  case idx >= length {
    True -> Ok(state)
    False ->
      case elements.get_option(els, idx) {
        // Hole — CreateDataPropertyOrThrow is skipped for absent indices.
        None -> write_species_elements(state, target, els, idx + 1, length)
        Some(val) -> {
          use state <- result.try(write_species_element(state, target, idx, val))
          write_species_elements(state, target, els, idx + 1, length)
        }
      }
  }
}

/// CreateDataPropertyOrThrow(A, ! ToString(𝔽(idx)), val) on a species target
/// (§7.3.6).
fn write_species_element(
  state: State,
  target: Ref,
  idx: Int,
  val: JsValue,
) -> Result(State, #(JsValue, State)) {
  // Proxy target: CreateDataPropertyOrThrow goes through the real
  // [[DefineOwnProperty]] so the "defineProperty" trap fires per
  // element (splice/property-traps-order-with-species).
  use <- bool.lazy_guard(
    option.is_some(object.as_proxy(state.heap, target)),
    fn() {
      let #(heap, desc_ref) =
        common.alloc_pojo(state.heap, state.builtins.object.prototype, [
          #("value", value.data_property(val)),
          #("writable", value.data_property(JsBool(True))),
          #("enumerable", value.data_property(JsBool(True))),
          #("configurable", value.data_property(JsBool(True))),
        ])
      let state = State(..state, heap:)
      use #(state, ok) <- result.try(object_builtin.define_property_bool(
        state,
        target,
        JsString(int.to_string(idx)),
        desc_ref,
      ))
      case ok {
        False ->
          coerce.thrown_type_error(
            state,
            "Cannot define property " <> int.to_string(idx) <> " on proxy",
          )
        True -> Ok(state)
      }
    },
  )
  // CreateDataPropertyOrThrow (§7.3.6): [[DefineOwnProperty]] with
  // {value, W:T, E:T, C:T}. An existing CONFIGURABLE property is
  // redefined regardless of its [[Writable]] — the result has plain
  // default attributes, i.e. exactly an element-store entry, so drop
  // any dict override first. A non-configurable existing property
  // (or non-extensible target) rejects → TypeError.
  let #(state, redefinable) =
    drop_configurable_index_override(state, target, idx)
  use <- bool.lazy_guard(!redefinable, fn() {
    coerce.thrown_type_error(
      state,
      "Cannot define property " <> int.to_string(idx) <> " on object",
    )
  })
  let #(h, ok) = object.set_property(state.heap, target, index_key(idx), val)
  let state = State(..state, heap: h)
  case ok {
    False ->
      coerce.thrown_type_error(
        state,
        "Cannot define property " <> int.to_string(idx) <> " on object",
      )
    True -> Ok(state)
  }
}

/// Species-target copy loop: HasProperty → Get → CreateDataPropertyOrThrow
/// interleaved per index (§23.1.3.31 steps 12-13, §23.1.3.1 step 5.b.iv).
/// Unlike copy_range-then-write_species_result, an abrupt completion from the
/// target's [[DefineOwnProperty]] (e.g. a throwing proxy "defineProperty"
/// trap) terminates huge-length loops at the failing index instead of
/// draining the whole source range first — required by test262's
/// create-species-length-exceeding-integer-limit tests, where the read loop
/// alone would burn the full iteration budget.
fn copy_range_to_species(
  state: State,
  src: JsValue,
  src_idx: Int,
  target: Ref,
  dst_idx: Int,
  remaining: Int,
  fuel: Int,
) -> Result(State, #(JsValue, State)) {
  use <- bool.lazy_guard(fuel <= 0 && remaining > 0, fn() {
    range_error_op(state, iteration_budget_msg)
  })
  case remaining <= 0 {
    True -> Ok(state)
    False -> {
      // kPresent = HasProperty(O, Pk); if true, kValue = Get(O, Pk).
      use #(maybe_val, state) <- result.try(get_index_if_present(
        state,
        src,
        src_idx,
      ))
      use state <- result.try(case maybe_val {
        // Hole — CreateDataPropertyOrThrow is skipped for absent indices.
        None -> Ok(state)
        Some(val) -> write_species_element(state, target, dst_idx, val)
      })
      copy_range_to_species(
        state,
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

/// CreateDataProperty redefinition prep: if `target` carries a dict override
/// for Index(idx) (created by Object.defineProperty with non-default
/// attributes), remove it when it is configurable — the subsequent
/// set_property then writes a plain default-attribute element, which is what
/// [[DefineOwnProperty]] with {W:T, E:T, C:T} produces. Returns
/// #(state, False) when the override is non-configurable (redefinition must
/// reject per §10.1.6.3 ValidateAndApplyPropertyDescriptor step 4).
fn drop_configurable_index_override(
  state: State,
  target: Ref,
  idx: Int,
) -> #(State, Bool) {
  case heap.read(state.heap, target) {
    Some(ObjectSlot(properties:, ..) as slot) ->
      case dict.get(properties, index_key(idx)) {
        Error(Nil) -> #(state, True)
        Ok(prop) ->
          case value.prop_configurable(prop) {
            False -> #(state, False)
            True -> {
              let h =
                heap.write(
                  state.heap,
                  target,
                  ObjectSlot(
                    ..slot,
                    properties: dict.delete(properties, index_key(idx)),
                  ),
                )
              #(State(..state, heap: h), True)
            }
          }
      }
    _ -> #(state, True)
  }
}

/// IsConcatSpreadable (§7.2.18):
///   1. If E is not an Object, return false.
///   2. Let spreadable be ? Get(E, @@isConcatSpreadable).
///   3. If spreadable is not undefined, return ToBoolean(spreadable).
///   4. Return ? IsArray(E).
fn is_concat_spreadable(
  state: State,
  item: JsValue,
) -> Result(#(Bool, State), #(JsValue, State)) {
  case item {
    JsObject(_) -> {
      // Step 2: Get(E, @@isConcatSpreadable) — may invoke a getter.
      use #(flag, state) <- result.try(object.get_symbol_value_of(
        state,
        item,
        value.symbol_is_concat_spreadable,
      ))
      case flag {
        // Step 4: Return ? IsArray(E) — pierces proxies to their target
        // (§7.2.2) and throws TypeError on a revoked proxy.
        JsUndefined ->
          case object.is_array(state.heap, item) {
            Ok(spreadable) -> Ok(#(spreadable, state))
            Error(Nil) ->
              coerce.thrown_type_error(
                state,
                "Cannot perform 'IsArray' on a proxy that has been revoked",
              )
          }
        // Step 3: spreadable is not undefined → ToBoolean(spreadable).
        _ -> Ok(#(value.is_truthy(flag), state))
      }
    }
    // Step 1: E is not an Object → false.
    _ -> Ok(#(False, state))
  }
}

fn concat_item(
  state: State,
  elems: JsElements,
  pos: Int,
  item: JsValue,
) -> Result(#(#(JsElements, Int), State), #(JsValue, State)) {
  // Step 5a: Let spreadable be ? IsConcatSpreadable(E).
  use #(spreadable, state) <- result.try(is_concat_spreadable(state, item))
  case spreadable, item {
    // Step 5b: spreadable — spread E's elements.
    // Step 5b.i: len = LengthOfArrayLike(E) — works on any object
    // (arrays, arguments, array-likes with @@isConcatSpreadable = true).
    True, JsObject(ref) -> {
      use #(length, state) <- result.try(object_length(state, ref))
      // Step 5b.iii: If n + len > 2^53 - 1, throw a TypeError exception.
      use <- bool.lazy_guard(pos + length > limits.max_safe_integer, fn() {
        coerce.thrown_type_error(
          state,
          "Array length exceeds maximum safe integer",
        )
      })
      // Step 5b.iv: copy elements [0..len) into result at position n.
      use #(copied, state) <- result.map(copy_range(
        state,
        item,
        0,
        pos,
        length,
        elems,
      ))
      #(#(copied, pos + length), state)
    }
    // Step 5c: not spreadable — append E as a single element.
    _, _ -> Ok(#(#(elements.set(elems, pos, item), pos + 1), state))
  }
}

/// concat_items for a custom species target (§23.1.3.1 step 5): spreadable
/// items are copied via the interleaved HasProperty → Get →
/// CreateDataPropertyOrThrow loop; non-spreadable items are defined directly
/// (step 5.c.iii). Returns the final n.
fn concat_items_species(
  state: State,
  items: List(JsValue),
  target: Ref,
  pos: Int,
) -> Result(#(Int, State), #(JsValue, State)) {
  case items {
    [] -> Ok(#(pos, state))
    [item, ..rest] -> {
      // Step 5a: Let spreadable be ? IsConcatSpreadable(E).
      use #(spreadable, state) <- result.try(is_concat_spreadable(state, item))
      case spreadable, item {
        True, JsObject(ref) -> {
          // Step 5b.i: len = LengthOfArrayLike(E).
          use #(length, state) <- result.try(object_length(state, ref))
          // Step 5b.ii: If n + len > 2^53 - 1, throw a TypeError exception.
          use <- bool.lazy_guard(pos + length > limits.max_safe_integer, fn() {
            coerce.thrown_type_error(
              state,
              "Array length exceeds maximum safe integer",
            )
          })
          // Step 5b.iv: per-k HasProperty → Get → CreateDataPropertyOrThrow.
          use state <- result.try(copy_range_to_species(
            state,
            item,
            0,
            target,
            pos,
            length,
            limits.max_iteration,
          ))
          concat_items_species(state, rest, target, pos + length)
        }
        // Step 5c: CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), E).
        _, _ -> {
          use state <- result.try(write_species_element(
            state,
            target,
            pos,
            item,
          ))
          concat_items_species(state, rest, target, pos + 1)
        }
      }
    }
  }
}

/// Array.prototype.reverse ( )
/// ES2024 §23.1.3.24
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. Let middle be floor(len / 2).
/// 4. Let lower be 0.
/// 5. Repeat, while lower ≠ middle,
///    a. Let upper be len - lower - 1.
///    b. Let upperP be ! ToString(𝔽(upper)).
///    c. Let lowerP be ! ToString(𝔽(lower)).
///    d. Let lowerExists be ? HasProperty(O, lowerP).
///    e. If lowerExists is true, let lowerValue be ? Get(O, lowerP).
///    f. Let upperExists be ? HasProperty(O, upperP).
///    g. If upperExists is true, let upperValue be ? Get(O, upperP).
///    h. If lowerExists is true and upperExists is true, then
///       i. Perform ? Set(O, lowerP, upperValue, true).
///       ii. Perform ? Set(O, upperP, lowerValue, true).
///    i. Else if lowerExists is false and upperExists is true, then
///       i. Perform ? Set(O, lowerP, upperValue, true).
///       ii. Perform ? DeletePropertyOrThrow(O, upperP).
///    j. Else if lowerExists is true and upperExists is false, then
///       i. Perform ? DeletePropertyOrThrow(O, lowerP).
///       ii. Perform ? Set(O, upperP, lowerValue, true).
///    k. Else, (neither exists) no action.
///    l. Set lower to lower + 1.
/// 6. Return O.
///
fn array_reverse(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, ref, length, state <- require_array(this, state)
  // Fast path: in-place element reversal, one heap read + one heap write.
  let fast = {
    use els, len <- try_elements_fast_path(state, ref, length)
    #(elements.reverse_range(els, len), len, Nil)
  }
  case fast {
    Some(#(Nil, state)) -> #(state, Ok(this))
    // Steps 3-5: middle = floor(len/2), lower = 0, loop while lower != middle
    // Step 6: Return O
    None ->
      wrap(
        reverse_generic(state, ref, 0, length - 1, limits.max_iteration),
        this,
      )
  }
}

/// Implements §23.1.3.24 step 5's loop body. lo = lower, hi = upper (len-lower-1).
/// The spec iterates lower from 0 to middle; here we converge lo/hi toward
/// each other which is equivalent.
///
/// `fuel` is the pragmatic per-step budget (see iteration_budget_msg) — a
/// proxy/getter on a huge array-like throws within its first few accesses,
/// so the budget only fires on loops doing real unbounded work.
fn reverse_generic(
  state: State,
  ref: Ref,
  lo: Int,
  hi: Int,
  fuel: Int,
) -> Result(State, #(JsValue, State)) {
  // Step 5: Repeat, while lower != middle (lo < hi is equivalent)
  case lo >= hi {
    True -> Ok(state)
    False -> {
      use <- bool.lazy_guard(fuel <= 0, fn() {
        range_error_op(state, iteration_budget_msg)
      })
      // Step 5d: Let lowerExists be ? HasProperty(O, lowerP)
      use #(has_lo, state) <- result.try(generic_has_op(state, ref, lo))
      // Step 5e: If lowerExists, lowerValue = ? Get(O, lowerP) — BEFORE the
      // upper HasProperty: the lower getter may delete the upper element
      // (get_if_present_with_delete in test262).
      use #(lo_val, state) <- result.try(get_index_if(state, ref, lo, has_lo))
      // Step 5f: Let upperExists be ? HasProperty(O, upperP)
      use #(has_hi, state) <- result.try(generic_has_op(state, ref, hi))
      // Step 5g: If upperExists, upperValue = ? Get(O, upperP)
      use #(hi_val, state) <- result.try(get_index_if(state, ref, hi, has_hi))
      use state <- result.try(case lo_val, hi_val {
        // Step 5h: lowerExists AND upperExists — swap
        Some(lo_val), Some(hi_val) -> {
          // Step 5h.i: Set(O, lowerP, upperValue, true)
          use state <- result.try(generic_set_index(state, ref, lo, hi_val))
          // Step 5h.ii: Set(O, upperP, lowerValue, true)
          generic_set_index(state, ref, hi, lo_val)
        }
        // Step 5i: NOT lowerExists AND upperExists — move upper to lower, delete upper
        None, Some(hi_val) -> {
          // Step 5i.i: Set(O, lowerP, upperValue, true)
          use state <- result.try(generic_set_index(state, ref, lo, hi_val))
          // Step 5i.ii: DeletePropertyOrThrow(O, upperP)
          generic_delete_index(state, ref, hi)
        }
        // Step 5j: lowerExists AND NOT upperExists — delete lower, move lower to upper
        Some(lo_val), None -> {
          // Step 5j.i: DeletePropertyOrThrow(O, lowerP)
          use state <- result.try(generic_delete_index(state, ref, lo))
          // Step 5j.ii: Set(O, upperP, lowerValue, true)
          generic_set_index(state, ref, hi, lo_val)
        }
        // Step 5k: Neither exists — no action
        None, None -> Ok(state)
      })
      // Step 5l: lower = lower + 1
      reverse_generic(state, ref, lo + 1, hi - 1, fuel - 1)
    }
  }
}

/// Conditional Get(O, ! ToString(𝔽(idx))) — Some(value) when `present`
/// (the preceding HasProperty result), None for a hole. Keeps the spec's
/// "HasProperty then Get" pairs in their observable order.
fn get_index_if(
  state: State,
  ref: Ref,
  idx: Int,
  present: Bool,
) -> Result(#(Option(JsValue), State), #(JsValue, State)) {
  case present {
    True -> {
      use #(val, state) <- result.map(generic_get(state, ref, idx))
      #(Some(val), state)
    }
    False -> Ok(#(None, state))
  }
}

/// Array.prototype.fill ( value [ , start [ , end ] ] )
/// ES2024 §23.1.3.7
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. Let relativeStart be ? ToIntegerOrInfinity(start).
/// 4. If relativeStart = -∞, let k be 0.
/// 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
/// 6. Else, let k be min(relativeStart, len).
/// 7. If end is undefined, let relativeEnd be len.
/// 8. Else, let relativeEnd be ? ToIntegerOrInfinity(end).
/// 9. If relativeEnd = -∞, let final be 0.
/// 10. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
/// 11. Else, let final be min(relativeEnd, len).
/// 12. Repeat, while k < final,
///     a. Let Pk be ! ToString(𝔽(k)).
///     b. Perform ? Set(O, Pk, value, true).
///     c. Set k to k + 1.
/// 13. Return O.
///
fn array_fill(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, ref, length, state <- require_array(this, state)
  // Step 12 uses value; if not provided, defaults to undefined
  let fill_val = helpers.first_arg_or_undefined(args)
  // Steps 3-6: relativeStart → k (clamped index)
  // try_resolve_index handles ToIntegerOrInfinity + clamping; default 0 when
  // absent. The coercion can run user code or throw.
  use start, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 1),
    length,
    0,
  )
  // Steps 7-11: relativeEnd → final (clamped index); default len when absent
  // (step 7: if end is undefined, relativeEnd = len)
  use end, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 2),
    length,
    length,
  )
  // Pragmatic bound: O(end-start) writes with no early exit — fail fast on
  // ranges that would hang (see iteration_budget_msg).
  use <- bool.lazy_guard(end - start > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  // Fast path: bulk fill [start, end), one heap read + one heap write.
  let fast = {
    use els, len <- try_elements_fast_path(state, ref, length)
    #(elements.fill_range(els, start, end, fill_val), len, Nil)
  }
  case fast {
    Some(#(Nil, state)) -> #(state, Ok(this))
    // Steps 12-13: fill loop, then return O
    None -> wrap(fill_generic(state, ref, start, end, fill_val), this)
  }
}

/// Implements §23.1.3.7 step 11: Repeat, while k < final.
fn fill_generic(
  state: State,
  ref: Ref,
  idx: Int,
  end: Int,
  val: JsValue,
) -> Result(State, #(JsValue, State)) {
  // Step 11: Repeat, while k < final
  case idx >= end {
    True -> Ok(state)
    False -> {
      // Step 12a-b: Pk = ToString(k), Set(O, Pk, value, true)
      use state <- result.try(generic_set_index(state, ref, idx, val))
      // Step 12c: k = k + 1
      fill_generic(state, ref, idx + 1, end, val)
    }
  }
}

/// Array.prototype.at ( index )
/// ES2024 §23.1.3.1
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. Let relativeIndex be ? ToIntegerOrInfinity(index).
/// 4. If relativeIndex >= 0, then let k be relativeIndex.
/// 5. Else, let k be len + relativeIndex.
/// 6. If k < 0 or k >= len, return undefined.
/// 7. Return ? Get(O, ! ToString(𝔽(k))).
///
fn array_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, _ref, length, state <- require_array(this, state)
  // Step 3: relativeIndex = ToIntegerOrInfinity(index) — observable
  // coercion: valueOf/toString run, Symbol/BigInt throw TypeError.
  use raw, state <- try_integer_or_infinity(state, arg_or_undefined(args, 0))
  // Steps 4-5: resolve negative index
  let idx = case raw < 0 {
    True -> length + raw
    False -> raw
  }
  // Steps 6-7: bounds check, then Get
  case idx < 0 || idx >= length {
    True -> #(state, Ok(JsUndefined))
    False -> {
      use val, state <- state.try_op(get_index(state, this, idx))
      #(state, Ok(val))
    }
  }
}

// ============================================================================
// Search methods (indexOf / lastIndexOf / includes)
// ============================================================================

/// Array.prototype.indexOf ( searchElement [ , fromIndex ] )
/// ES2024 §23.1.3.16
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. If len = 0, return -1𝔽.
/// 4. Let n be ? ToIntegerOrInfinity(fromIndex).
///    (If fromIndex is not present, n = 0.)
/// 5. If n = +∞, return -1𝔽.
/// 6. Else if n = -∞, set n to 0.
/// 7. If n >= 0, then let k be n.
/// 8. Else, let k be max(len + n, 0).
/// 9. Repeat, while k < len,
///    a. Let kPresent be ? HasProperty(O, ! ToString(𝔽(k))).
///    b. If kPresent is true, then
///       i. Let elementK be ? Get(O, ! ToString(𝔽(k))).
///       ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
///    c. Set k to k + 1.
/// 10. Return -1𝔽.
///
/// Steps 1-2 combined via require_array. Step 9 loop delegated to
/// search_forward with skip_holes=True (step 9a HasProperty check).
fn array_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  forward_search_driver(
    this,
    args,
    state,
    value.strict_equal,
    True,
    value.from_int,
  )
}

/// Shared prologue + dispatch for indexOf (§23.1.3.16) and includes
/// (§23.1.3.15) — their steps 3-8 are identical; they differ only in the
/// equality predicate, hole handling, and result wrapping. A miss is
/// search_forward returning -1, so wrap(-1) also covers the len=0 and
/// n=+∞ early exits (𝔽(-1) for indexOf, false for includes).
fn forward_search_driver(
  this: JsValue,
  args: List(JsValue),
  state: State,
  eq: fn(JsValue, JsValue) -> Bool,
  skip_holes: Bool,
  wrap: fn(Int) -> JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, _ref, length, state <- require_array(this, state)
  // Step 3: If len = 0, return miss
  use <- bool.guard(length == 0, #(state, Ok(wrap(-1))))
  let search = helpers.first_arg_or_undefined(args)
  // Step 4: n = ToIntegerOrInfinity(fromIndex) — observable coercion (runs
  // after the len=0 early exit per spec order). ±∞ comes back saturated to
  // ±(2^53 - 1), so:
  //   Step 5 (n = +∞ → miss): start >= length, search loop exits with -1.
  //   Step 6 (n = -∞ → 0): max(length + n, 0) = 0.
  use from, state <- try_integer_or_infinity(state, arg_or_undefined(args, 1))
  // Steps 7-8: resolve start index (negative → max(len + n, 0))
  let start = case from < 0 {
    True -> int.max(length + from, 0)
    False -> from
  }
  // Steps 9-10: forward search
  use found, state <- state.try_op(search_forward(
    state,
    this,
    start,
    length,
    search,
    eq,
    skip_holes,
    limits.max_iteration,
  ))
  #(state, Ok(wrap(found)))
}

/// Array.prototype.lastIndexOf ( searchElement [ , fromIndex ] )
/// ES2024 §23.1.3.19
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. If len = 0, return -1𝔽.
/// 4. If fromIndex is present, let n be ? ToIntegerOrInfinity(fromIndex).
/// 5. Else, let n be len - 1.
/// 6. If n = -∞, return -1𝔽.
/// 7. If n >= 0, then let k be min(n, len - 1).
/// 8. Else, let k be len + n.
/// 9. Repeat, while k >= 0,
///    a. Let kPresent be ? HasProperty(O, ! ToString(𝔽(k))).
///    b. If kPresent is true, then
///       i. Let elementK be ? Get(O, ! ToString(𝔽(k))).
///       ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
///    c. Set k to k - 1.
/// 10. Return -1𝔽.
///
/// Steps 1-2 combined via require_array. Step 4 checks arg COUNT (not value):
/// explicitly passing undefined yields ToIntegerOrInfinity(undefined) = 0,
/// while omitting defaults to len-1 per step 5. Step 9 loop delegated to
/// search_backward.
fn array_last_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, _ref, length, state <- require_array(this, state)
  // Step 3: If len = 0, return -1
  use <- bool.guard(length == 0, #(state, Ok(value.from_int(-1))))
  let search = helpers.first_arg_or_undefined(args)
  // Steps 4-5: fromIndex present → ToIntegerOrInfinity (observable coercion);
  // absent → len - 1. Checked by arg COUNT, not value (explicitly passing
  // undefined yields ToIntegerOrInfinity(undefined) = 0, see MEMORY.md
  // lastIndexOf gotcha). ±∞ comes back saturated to ±(2^53 - 1), so:
  //   Step 6 (n = -∞ → -1): start = len + n < 0, search_backward returns -1.
  //   Step 7 (n = +∞): min(n, len - 1) = len - 1.
  let proceed = fn(from: Int, state: State) {
    // Steps 7-8: resolve start index
    let start = case from < 0 {
      // Step 8: k = len + n
      True -> length + from
      // Step 7: k = min(n, len - 1)
      False -> int.min(from, length - 1)
    }
    // Steps 9-10: backward search with IsStrictlyEqual, skipping holes
    use found, state <- state.try_op(search_backward(
      state,
      this,
      start,
      search,
      limits.max_iteration,
    ))
    #(state, Ok(value.from_int(found)))
  }
  case args {
    [_, f, ..] -> {
      use from, state <- try_integer_or_infinity(state, f)
      proceed(from, state)
    }
    _ -> proceed(length - 1, state)
  }
}

/// Array.prototype.includes ( searchElement [ , fromIndex ] )
/// ES2024 §23.1.3.15
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. If len = 0, return false.
/// 4. Let n be ? ToIntegerOrInfinity(fromIndex).
///    (If fromIndex is not present, n = 0.)
/// 5. If n = +∞, return false.
/// 6. Else if n = -∞, set n to 0.
/// 7. If n >= 0, then let k be n.
/// 8. Else, let k be max(len + n, 0).
/// 9. Repeat, while k < len,
///    a. Let elementK be ? Get(O, ! ToString(𝔽(k))).
///    b. If SameValueZero(searchElement, elementK) is true, return true.
///    c. Set k to k + 1.
/// 10. Return false.
///
/// Steps 1-2 combined via require_array. Step 9a does NOT have a HasProperty
/// check (unlike indexOf) — holes are visited and treated as undefined per the
/// spec. Delegated to search_forward with skip_holes=False and SameValueZero.
fn array_includes(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  forward_search_driver(
    this,
    args,
    state,
    value.same_value_zero,
    False,
    fn(found) { JsBool(found >= 0) },
  )
}

/// Shared forward search loop for indexOf (§23.1.3.16 step 9) and
/// includes (§23.1.3.15 step 9).
///
/// indexOf step 9:                        includes step 9:
///   a. kPresent = HasProperty(O, k)        a. elementK = Get(O, k)
///   b. If kPresent, then                   b. If SameValueZero(search, elementK), return true
///      i. elementK = Get(O, k)             c. k = k + 1
///      ii. If IsStrictlyEqual(...), return k
///   c. k = k + 1
///
/// skip_holes=True → indexOf semantics (step 9a HasProperty check).
/// skip_holes=False → includes semantics (no HasProperty, holes read as undefined).
/// eq → IsStrictlyEqual for indexOf, SameValueZero for includes.
fn search_forward(
  state: State,
  this: JsValue,
  idx: Int,
  length: Int,
  search: JsValue,
  eq: fn(JsValue, JsValue) -> Bool,
  skip_holes: Bool,
  fuel: Int,
) -> Result(#(Int, State), #(JsValue, State)) {
  // Pragmatic step budget — see iteration_budget_msg.
  use <- bool.lazy_guard(fuel <= 0 && idx < length, fn() {
    range_error_op(state, iteration_budget_msg)
  })
  // Loop condition: k < len (both specs)
  case idx >= length {
    // Step 10: return -1 (indexOf) / return false (includes; caller converts)
    True -> Ok(#(-1, state))
    False ->
      case skip_holes {
        True -> {
          // indexOf step 9a-9b.i: kPresent = HasProperty(O, k); if true,
          // elementK = Get(O, k) — fused into one heap lookup.
          use #(maybe_val, state) <- result.try(get_index_if_present(
            state,
            this,
            idx,
          ))
          case maybe_val {
            None ->
              // indexOf step 9c: k = k + 1 (hole skipped)
              search_forward(
                state,
                this,
                idx + 1,
                length,
                search,
                eq,
                skip_holes,
                fuel - 1,
              )
            Some(val) ->
              // indexOf step 9b.ii: comparison
              case eq(val, search) {
                True -> Ok(#(idx, state))
                False ->
                  // Step 9c: k = k + 1
                  search_forward(
                    state,
                    this,
                    idx + 1,
                    length,
                    search,
                    eq,
                    skip_holes,
                    fuel - 1,
                  )
              }
          }
        }
        False -> {
          // includes step 9a-b: Get + compare (holes read as undefined)
          use #(val, state) <- result.try(get_index(state, this, idx))
          case eq(val, search) {
            True -> Ok(#(idx, state))
            False ->
              // Step 9c: k = k + 1
              search_forward(
                state,
                this,
                idx + 1,
                length,
                search,
                eq,
                skip_holes,
                fuel - 1,
              )
          }
        }
      }
  }
}

/// Backward search loop for lastIndexOf (§23.1.3.19 step 9).
///
/// 9. Repeat, while k >= 0,
///    a. Let kPresent be ? HasProperty(O, ! ToString(𝔽(k))).
///    b. If kPresent is true, then
///       i. Let elementK be ? Get(O, ! ToString(𝔽(k))).
///       ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
///    c. Set k to k - 1.
/// 10. Return -1𝔽.
///
/// Always skips holes (step 9a HasProperty check). Always uses
/// IsStrictlyEqual (only called from lastIndexOf).
fn search_backward(
  state: State,
  this: JsValue,
  idx: Int,
  search: JsValue,
  fuel: Int,
) -> Result(#(Int, State), #(JsValue, State)) {
  // Pragmatic step budget — see iteration_budget_msg.
  use <- bool.lazy_guard(fuel <= 0 && idx >= 0, fn() {
    range_error_op(state, iteration_budget_msg)
  })
  // Loop condition: k >= 0
  case idx < 0 {
    // Step 10: return -1
    True -> Ok(#(-1, state))
    False -> {
      // Step 9a-9b.i: kPresent = HasProperty(O, k); if true,
      // elementK = Get(O, k) — fused into one heap lookup.
      use #(maybe_val, state) <- result.try(get_index_if_present(
        state,
        this,
        idx,
      ))
      case maybe_val {
        None ->
          // Step 9c: k = k - 1 (hole skipped)
          search_backward(state, this, idx - 1, search, fuel - 1)
        Some(val) ->
          // Step 9b.ii: IsStrictlyEqual check
          case value.strict_equal(val, search) {
            True -> Ok(#(idx, state))
            False ->
              // Step 9c: k = k - 1
              search_backward(state, this, idx - 1, search, fuel - 1)
          }
      }
    }
  }
}

// ============================================================================
// Iteration methods (forEach / map / filter / every / some / find / findIndex)
// ============================================================================

/// Iteration mode — whether to skip holes (HasProperty check before Get).
/// find/findIndex do NOT skip holes; all other iteration methods do.
type HoleMode {
  SkipHoles
  VisitHoles
}

/// Array.prototype.forEach (ES2024 §23.1.3.13)
///
/// §23.1.3.13 Array.prototype.forEach ( callbackfn [ , thisArg ] ):
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. If IsCallable(callbackfn) is false, throw a TypeError exception.
///   4. Let k be 0.
///   5. Repeat, while k < len,
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kPresent be ? HasProperty(O, Pk).
///      c. If kPresent is true, then
///         i. Let kValue be ? Get(O, Pk).
///         ii. Perform ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
///      d. Set k to k + 1.
///   6. Return undefined.
///
/// Simplifications:
///   - Steps 1-2 are collapsed by require_array (ToObject + LengthOfArrayLike),
///     which extracts internal elements directly rather than going through Get.
///   - Step 3 is handled by require_callback (IsCallable check + TypeError).
///   - Steps 4-5 are handled by iterate_array with SkipHoles mode, which
///     implements the HasProperty check (step 5b) via get_index_if_present and
///     calls the callback with (kValue, k, O) arguments (step 5c.ii).
///   - Step 6: return undefined.
fn array_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: O = ToObject(this), len = LengthOfArrayLike(O)
  use this, _ref, length, state <- require_array(this, state)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-5: k = 0; Repeat while k < len (iterate_array handles the loop,
  // HasProperty check via SkipHoles, and Call(callbackfn, thisArg, «kValue, k, O»))
  use _final_elem, _final_idx, state <- iterate_array(
    state,
    this,
    length,
    cb,
    this_arg,
    SkipHoles,
    // No early-exit predicate (forEach always runs to completion)
    fn(_) { False },
  )
  // Step 6: Return undefined
  #(state, Ok(JsUndefined))
}

/// Array.prototype.map (ES2024 §23.1.3.19)
///
/// §23.1.3.19 Array.prototype.map ( callbackfn [ , thisArg ] ):
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. If IsCallable(callbackfn) is false, throw a TypeError exception.
///   4. Let A be ? ArraySpeciesCreate(O, len).
///   5. Let k be 0.
///   6. Repeat, while k < len,
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kPresent be ? HasProperty(O, Pk).
///      c. If kPresent is true, then
///         i. Let kValue be ? Get(O, Pk).
///         ii. Let mappedValue be ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
///         iii. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
///      d. Set k to k + 1.
///   7. Return A.
///
/// Simplifications:
///   - Steps 1-2: require_array collapses ToObject + LengthOfArrayLike.
///   - Step 3: require_callback handles IsCallable check + TypeError.
///   - Step 4: we skip ArraySpeciesCreate and always create a plain Array
///     (@@species is not respected — a known simplification).
///   - Steps 5-6: map_loop implements the iteration. Holes are preserved
///     in the result (callback is not called for absent elements, matching
///     step 6b's HasProperty check).
///   - Step 6c.iii: CreateDataPropertyOrThrow is done via elements.set
///     on the accumulator elements (equivalent for dense arrays).
///   - Step 7: finish_array allocates the result array with the collected
///     elements and the original length.
fn array_map(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: O = ToObject(this), len = LengthOfArrayLike(O)
  use this, _ref, length, state <- require_array(this, state)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // Step 4: A = ArraySpeciesCreate(O, len) — before the iteration loop, so a
  // custom species constructor runs (exactly once) before any callback.
  use species, state <- state.try_op(array_species_create(state, this, length))
  // Steps 5-6: iterate, collecting mappedValue at each present index.
  // Step 6c.iii: CreateDataPropertyOrThrow(A, Pk, mappedValue) — done via
  // elements.set on the accumulator (preserves holes: absent source indices
  // are never written to the output).
  let folded =
    fold_array(
      state,
      this,
      length,
      cb,
      this_arg,
      elements.new(),
      fn(_state, acc, result, _elem, idx) { elements.set(acc, idx, result) },
    )
  case species {
    // Step 7: Return A (plain array path).
    None -> finish_array(folded, length)
    Some(target) -> {
      let #(state, outcome) = folded
      case outcome {
        Error(thrown) -> #(state, Error(thrown))
        Ok(els) ->
          case write_species_result(state, target, els, length, None) {
            Ok(state) -> #(state, Ok(JsObject(target)))
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
      }
    }
  }
}

/// Allocates a result array from collected elements — corresponds to
/// ArraySpeciesCreate (step 4) + the final "Return A" (step 7) in both
/// Array.prototype.map (§23.1.3.19) and similar methods.
///
/// Simplification: always creates a plain Array (ignores @@species).
/// The length is set directly via ArrayObject(length) in the slot
/// constructor rather than a separate Set("length") call.
fn finish_array(
  result: #(State, Result(JsElements, JsValue)),
  length: Int,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, outcome) = result
  let array_proto = state.builtins.array.prototype
  case outcome {
    Error(thrown) -> #(state, Error(thrown))
    Ok(elements) -> {
      // Allocate result array A with collected elements and original length
      let #(heap, ref) =
        common.alloc_array_from_elements(
          state.heap,
          elements,
          length,
          array_proto,
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// Allocates a dense result array from a reversed list of collected values —
/// corresponds to ArraySpeciesCreate (step 4) + the final "Return A" in
/// Array.prototype.filter (§23.1.3.8) and Array.prototype.flatMap (§23.1.3.14).
///
/// The input list is in reverse order (built by prepending during iteration).
/// The result is a contiguous array with length = list length (no holes).
///
/// Simplification: always creates a plain Array (ignores @@species).
fn finish_list(
  result: #(State, Result(List(JsValue), JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, outcome) = result
  case outcome {
    Error(thrown) -> #(state, Error(thrown))
    Ok(kept) -> state.ok_array(state, list.reverse(kept))
  }
}

/// Array.prototype.filter (ES2024 §23.1.3.8)
///
/// §23.1.3.8 Array.prototype.filter ( callbackfn [ , thisArg ] ):
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. If IsCallable(callbackfn) is false, throw a TypeError exception.
///   4. Let A be ? ArraySpeciesCreate(O, 0).
///   5. Let k be 0.
///   6. Let to be 0.
///   7. Repeat, while k < len,
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kPresent be ? HasProperty(O, Pk).
///      c. If kPresent is true, then
///         i. Let kValue be ? Get(O, Pk).
///         ii. Let selected be ! ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
///         iii. If selected is true, then
///              1. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(to)), kValue).
///              2. Set to to to + 1.
///      d. Set k to k + 1.
///   8. Return A.
///
/// Simplifications:
///   - Steps 1-2: require_array collapses ToObject + LengthOfArrayLike.
///   - Step 3: require_callback handles IsCallable check + TypeError.
///   - Step 4: we skip ArraySpeciesCreate and always create a plain Array
///     (@@species is not respected — a known simplification).
///   - Steps 5-7: filter_loop implements the iteration, collecting kept
///     values into a reversed list (the "to" index is implicit via list length).
///   - Step 7c.ii: ToBoolean is done via value.is_truthy.
///   - Step 8: result array is allocated via common.alloc_array from the
///     reversed kept list — contiguous, no holes.
fn array_filter(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: O = ToObject(this), len = LengthOfArrayLike(O)
  use this, _ref, length, state <- require_array(this, state)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // Step 4: A = ArraySpeciesCreate(O, 0) — before the iteration loop.
  use species, state <- state.try_op(array_species_create(state, this, 0))
  // Steps 5-7: iterate, keeping kValue when selected is true.
  // Step 7c.ii: selected = ToBoolean(result) — done via value.is_truthy.
  // Step 7c.iii: If selected, prepend kValue (the "to" index is implicit via
  // list prepend; reversed on return so final index equals insertion order).
  let folded =
    fold_array(
      state,
      this,
      length,
      cb,
      this_arg,
      [],
      fn(_state, acc, result, elem, _idx) {
        case value.is_truthy(result) {
          True -> [elem, ..acc]
          False -> acc
        }
      },
    )
  case species {
    // Step 8: Return A (plain array path).
    None -> finish_list(folded)
    Some(target) -> {
      let #(state, outcome) = folded
      case outcome {
        Error(thrown) -> #(state, Error(thrown))
        Ok(kept) -> {
          let vals = list.reverse(kept)
          let els = build_elements_from_list(vals, 0, elements.new())
          case
            write_species_result(state, target, els, list.length(vals), None)
          {
            Ok(state) -> #(state, Ok(JsObject(target)))
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        }
      }
    }
  }
}

/// Array.prototype.every (ES2024 §23.1.3.5)
///
/// §23.1.3.5 Array.prototype.every ( callbackfn [ , thisArg ] ):
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. If IsCallable(callbackfn) is false, throw a TypeError exception.
///   4. Let k be 0.
///   5. Repeat, while k < len,
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kPresent be ? HasProperty(O, Pk).
///      c. If kPresent is true, then
///         i. Let kValue be ? Get(O, Pk).
///         ii. Let testResult be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
///         iii. If testResult is false, return false.
///      d. Set k to k + 1.
///   6. Return true.
fn array_every(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  every_some(this, args, state, match_on: False)
}

/// Shared driver for every (match_on: False) / some (match_on: True).
/// Steps 1-2: ToObject + LengthOfArrayLike (via require_array).
/// Step 3: IsCallable check (via require_callback).
/// Steps 4-5: k = 0, repeat while k < len — iterate_array handles the loop,
/// SkipHoles implements step 5b (HasProperty), and the stop predicate
/// implements step 5c.iii (stop when ToBoolean(testResult) == match_on).
/// Step 6: stopped early (idx < length) means a match_on result was found.
fn every_some(
  this: JsValue,
  args: List(JsValue),
  state: State,
  match_on match_on: Bool,
) -> #(State, Result(JsValue, JsValue)) {
  use this, _ref, length, state <- require_array(this, state)
  use cb, this_arg, state <- require_callback(args, state)
  use _elem, idx, state <- iterate_array(
    state,
    this,
    length,
    cb,
    this_arg,
    SkipHoles,
    fn(r) { value.is_truthy(r) == match_on },
  )
  #(state, Ok(JsBool({ idx < length } == match_on)))
}

/// Array.prototype.some (ES2024 §23.1.3.27)
///
/// §23.1.3.27 Array.prototype.some ( callbackfn [ , thisArg ] ):
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. If IsCallable(callbackfn) is false, throw a TypeError exception.
///   4. Let k be 0.
///   5. Repeat, while k < len,
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kPresent be ? HasProperty(O, Pk).
///      c. If kPresent is true, then
///         i. Let kValue be ? Get(O, Pk).
///         ii. Let testResult be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
///         iii. If testResult is true, return true.
///      d. Set k to k + 1.
///   6. Return false.
fn array_some(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  every_some(this, args, state, match_on: True)
}

/// Array.prototype.find (ES2024 §23.1.3.9)
///
/// §23.1.3.9 Array.prototype.find ( predicate [ , thisArg ] ):
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
///   4. Return findRec.[[Value]].
///
/// Delegates to FindViaPredicate (§23.1.3.9.1):
///   1. If IsCallable(predicate) is false, throw a TypeError exception.
///   2-3. Build ascending index list [0..len-1].
///   4. For each integer k of indices, do
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kValue be ? Get(O, Pk).
///      c. Let testResult be ToBoolean(? Call(predicate, thisArg, « kValue, 𝔽(k), O »)).
///      d. If testResult is true, return Record { [[Index]]: 𝔽(k), [[Value]]: kValue }.
///   5. Return Record { [[Index]]: -1𝔽, [[Value]]: undefined }.
///
/// Note: FindViaPredicate step 4b uses Get (not HasProperty + Get). This means
/// holes are visited as undefined, not skipped — hence VisitHoles mode.
///
/// FindViaPredicate (§23.1.3.9.1) shared driver for find / findIndex /
/// findLast / findLastIndex. ToObject + LengthOfArrayLike + IsCallable, then
/// iterate with VisitHoles + is_truthy. cont gets (elem, idx, length, state);
/// not-found ⇒ idx==length (forward) or idx==-1 (reverse).
fn find_via_predicate(
  this: JsValue,
  args: List(JsValue),
  state: State,
  reverse: Bool,
  cont: fn(JsValue, Int, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  use this, _ref, length, state <- require_array(this, state)
  use cb, this_arg, state <- require_callback(args, state)
  let iter = case reverse {
    True -> iterate_array_rev
    False -> iterate_array
  }
  use elem, idx, state <- iter(
    state,
    this,
    length,
    cb,
    this_arg,
    VisitHoles,
    value.is_truthy,
  )
  cont(elem, idx, length, state)
}

fn array_find(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use elem, idx, length, state <- find_via_predicate(this, args, state, False)
  case idx < length {
    True -> #(state, Ok(elem))
    False -> #(state, Ok(JsUndefined))
  }
}

/// Array.prototype.findIndex (ES2024 §23.1.3.10)
///
/// §23.1.3.10 Array.prototype.findIndex ( predicate [ , thisArg ] ):
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
///   4. Return findRec.[[Index]].
///
/// Uses the same FindViaPredicate (§23.1.3.9.1) as Array.prototype.find — see
/// that function's doc comment for the full algorithm. The only difference is
/// step 4: find returns [[Value]], findIndex returns [[Index]].
fn array_find_index(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _elem, idx, length, state <- find_via_predicate(this, args, state, False)
  case idx < length {
    True -> #(state, Ok(value.from_int(idx)))
    False -> #(state, Ok(value.from_int(-1)))
  }
}

/// Array.prototype.sort ( comparefn )
/// ES2024 §23.1.3.30
///
/// 1. If comparefn is not undefined and IsCallable(comparefn) is false,
///    throw a TypeError exception.
/// 2. Let obj be ? ToObject(this value).
/// 3. Let len be ? LengthOfArrayLike(obj).
/// 4. Let items be SortIndexedProperties(obj, len, SortCompare, skip-holes).
/// 5. Let itemCount be the number of elements in items.
/// 6. Let j be 0.
/// 7. Repeat, while j < itemCount,
///    a. Perform ? Set(obj, ! ToString(𝔽(j)), items[j], true).
///    b. Set j to j + 1.
/// 8. Repeat, while j < len,
///    a. Perform ? DeletePropertyOrThrow(obj, ! ToString(𝔽(j))).
///    b. Set j to j + 1.
/// 9. Return obj.
fn array_sort(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use comparefn, state <- with_comparefn(args, state)
  use this, ref, length, state <- require_array(this, state)
  // Pragmatic bound: SortIndexedProperties reads all len indices with no
  // early exit (see iteration_budget_msg).
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  case comparefn {
    None -> sort_default(state, ref, length, this)
    Some(cmp) -> sort_with_comparefn(state, ref, length, cmp, this)
  }
}

/// Shared comparefn validation for sort and toSorted (step 1 of both):
/// undefined → None, callable → Some(comparefn), anything else → TypeError.
fn with_comparefn(
  args: List(JsValue),
  state: State,
  cont: fn(Option(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let comparefn = helpers.first_arg_or_undefined(args)
  case comparefn {
    JsUndefined -> cont(None, state)
    _ ->
      case helpers.is_callable(state.heap, comparefn) {
        True -> cont(Some(comparefn), state)
        False ->
          state.type_error(
            state,
            common.typeof_value(comparefn, state.heap) <> " is not a function",
          )
      }
  }
}

/// Default sort: convert each element to string, sort lexicographically,
/// then write back in-place. Undefined values sort to the end per spec.
fn sort_default(
  state: State,
  ref: Ref,
  length: Int,
  this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  // Collect defined (non-hole) elements. Undefined values go to end.
  use #(defined, undefs), state <- state.try_op(collect_sort_elements(
    state,
    this,
    length,
    0,
    [],
    0,
    False,
  ))
  // Convert each defined element to string for comparison via ToString.
  use pairs, state <- state.try_op(stringify_elements(state, defined, []))
  // Sort by string key lexicographically (stable sort).
  let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
  // Write sorted values back, then undefineds, then delete trailing holes.
  let sorted_values = list.map(sorted, fn(pair) { pair.1 })
  let all_values = list.append(sorted_values, list.repeat(JsUndefined, undefs))
  wrap(write_sort_result(state, ref, all_values, length, 0), this)
}

/// Collect defined elements from the array for sorting.
/// Returns #(defined_values, undefined_count).
/// Holes are skipped entirely (not counted). Undefineds are counted separately.
///
/// Plain arrays with no index overrides take collect_sort_elements_snapshot
/// (one heap read for the whole loop); everything else takes the generic
/// per-element path.
/// `holes_as_undefined` selects the spec's hole treatment: sort() uses
/// SKIP-HOLES (§23.1.3.30 — holes are neither sorted nor counted, they stay
/// holes at the tail), toSorted() uses READ-THROUGH-HOLES (§23.1.3.34 —
/// holes read as undefined and the result array is dense).
fn collect_sort_elements(
  state: State,
  this: JsValue,
  length: Int,
  idx: Int,
  acc: List(JsValue),
  undefs: Int,
  holes_as_undefined: Bool,
) -> Result(#(#(List(JsValue), Int), State), #(JsValue, State)) {
  case dense_snapshot(state, this) {
    Some(#(els, proto)) ->
      collect_sort_elements_snapshot(
        state,
        this,
        els,
        proto,
        length,
        idx,
        acc,
        undefs,
        holes_as_undefined,
      )
    None ->
      collect_sort_elements_generic(
        state,
        this,
        length,
        idx,
        acc,
        undefs,
        holes_as_undefined,
      )
  }
}

/// Fast path for collect_sort_elements: read elements from a one-time
/// snapshot of the array slot. Nothing on this path runs user code, so the
/// snapshot stays valid for the whole loop. A hole shadowed by an inherited
/// prototype property may hide a getter — bail to the generic path from
/// that index.
fn collect_sort_elements_snapshot(
  state: State,
  this: JsValue,
  els: JsElements,
  proto: Option(Ref),
  length: Int,
  idx: Int,
  acc: List(JsValue),
  undefs: Int,
  holes_as_undefined: Bool,
) -> Result(#(#(List(JsValue), Int), State), #(JsValue, State)) {
  case idx >= length {
    True -> Ok(#(#(list.reverse(acc), undefs), state))
    False ->
      case elements.get_option(els, idx) {
        // Undefined — count but don't include in sort.
        Some(JsUndefined) ->
          collect_sort_elements_snapshot(
            state,
            this,
            els,
            proto,
            length,
            idx + 1,
            acc,
            undefs + 1,
            holes_as_undefined,
          )
        Some(val) ->
          collect_sort_elements_snapshot(
            state,
            this,
            els,
            proto,
            length,
            idx + 1,
            [val, ..acc],
            undefs,
            holes_as_undefined,
          )
        None ->
          case hole_is_inherited(state, proto, idx) {
            // Hole — SKIP-HOLES drops it; READ-THROUGH-HOLES counts it as
            // one more trailing undefined.
            False ->
              collect_sort_elements_snapshot(
                state,
                this,
                els,
                proto,
                length,
                idx + 1,
                acc,
                case holes_as_undefined {
                  True -> undefs + 1
                  False -> undefs
                },
                holes_as_undefined,
              )
            // Inherited property — Get may invoke a getter (user code).
            True ->
              collect_sort_elements_generic(
                state,
                this,
                length,
                idx,
                acc,
                undefs,
                holes_as_undefined,
              )
          }
      }
  }
}

/// Generic per-element path for collect_sort_elements: re-reads the heap
/// each iteration via get_index_if_present (handles index accessors,
/// inherited properties, and arrays mutated mid-collect by getters).
fn collect_sort_elements_generic(
  state: State,
  this: JsValue,
  length: Int,
  idx: Int,
  acc: List(JsValue),
  undefs: Int,
  holes_as_undefined: Bool,
) -> Result(#(#(List(JsValue), Int), State), #(JsValue, State)) {
  case idx >= length {
    True -> Ok(#(#(list.reverse(acc), undefs), state))
    False -> {
      use #(maybe_val, state) <- result.try(get_index_if_present(
        state,
        this,
        idx,
      ))
      case maybe_val {
        None ->
          // Hole — SKIP-HOLES drops it; READ-THROUGH-HOLES counts it as one
          // more trailing undefined.
          collect_sort_elements_generic(
            state,
            this,
            length,
            idx + 1,
            acc,
            case holes_as_undefined {
              True -> undefs + 1
              False -> undefs
            },
            holes_as_undefined,
          )
        Some(val) ->
          case val {
            JsUndefined ->
              // Undefined — count but don't include in sort.
              collect_sort_elements_generic(
                state,
                this,
                length,
                idx + 1,
                acc,
                undefs + 1,
                holes_as_undefined,
              )
            _ ->
              collect_sort_elements_generic(
                state,
                this,
                length,
                idx + 1,
                [val, ..acc],
                undefs,
                holes_as_undefined,
              )
          }
      }
    }
  }
}

/// Convert each value to its string representation for default sort comparison.
/// Returns #(list_of_#(string_key, original_value), state).
fn stringify_elements(
  state: State,
  values: List(JsValue),
  acc: List(#(String, JsValue)),
) -> Result(#(List(#(String, JsValue)), State), #(JsValue, State)) {
  case values {
    [] -> Ok(#(list.reverse(acc), state))
    [val, ..rest] -> {
      use #(str, state) <- result.try(coerce.js_to_string(state, val))
      stringify_elements(state, rest, [#(str, val), ..acc])
    }
  }
}

/// Sort with a user-provided comparefn. Each comparison re-enters the VM to
/// call the JS function. Undefined values sort to the end per spec; holes are
/// removed.
fn sort_with_comparefn(
  state: State,
  ref: Ref,
  length: Int,
  comparefn: JsValue,
  this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  // Collect defined (non-hole) elements. Undefined values go to end.
  use #(defined, undefs), state <- state.try_op(collect_sort_elements(
    state,
    this,
    length,
    0,
    [],
    0,
    False,
  ))
  use sorted, state <- state.try_op(merge_sort(state, defined, comparefn))
  let all_values = list.append(sorted, list.repeat(JsUndefined, undefs))
  wrap(write_sort_result(state, ref, all_values, length, 0), this)
}

/// Stable bottom-up merge sort that threads State through an effectful
/// comparator. O(n log n) compares, O(n log n) cons cells.
fn merge_sort(
  state: State,
  items: List(JsValue),
  comparefn: JsValue,
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case items {
    [] | [_] -> Ok(#(items, state))
    _ -> merge_all(state, list.map(items, list.wrap), comparefn)
  }
}

/// Repeatedly merge adjacent pairs of runs until one run remains.
fn merge_all(
  state: State,
  runs: List(List(JsValue)),
  comparefn: JsValue,
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case runs {
    [] -> Ok(#([], state))
    [done] -> Ok(#(done, state))
    _ -> {
      use #(next, state) <- result.try(merge_pairs(state, runs, comparefn, []))
      merge_all(state, next, comparefn)
    }
  }
}

/// Merge runs pairwise, preserving left-to-right order for stability.
fn merge_pairs(
  state: State,
  runs: List(List(JsValue)),
  comparefn: JsValue,
  acc: List(List(JsValue)),
) -> Result(#(List(List(JsValue)), State), #(JsValue, State)) {
  case runs {
    [] -> Ok(#(list.reverse(acc), state))
    [a] -> Ok(#(list.reverse([a, ..acc]), state))
    [a, b, ..rest] -> {
      use #(ab, state) <- result.try(merge_two(state, a, b, comparefn, []))
      merge_pairs(state, rest, comparefn, [ab, ..acc])
    }
  }
}

/// Merge two sorted runs. Ties go to `left` (stable: left holds earlier elems).
fn merge_two(
  state: State,
  left: List(JsValue),
  right: List(JsValue),
  comparefn: JsValue,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case left, right {
    [], _ -> Ok(#(list.append(list.reverse(acc), right), state))
    _, [] -> Ok(#(list.append(list.reverse(acc), left), state))
    [l, ..ls], [r, ..rs] -> {
      use #(res, state) <- result.try(
        state.call(state, comparefn, JsUndefined, [l, r]),
      )
      let cmp = case res {
        JsNumber(Finite(n)) -> n
        _ -> 0.0
      }
      case cmp <=. 0.0 {
        True -> merge_two(state, ls, right, comparefn, [l, ..acc])
        False -> merge_two(state, left, rs, comparefn, [r, ..acc])
      }
    }
  }
}

/// Write sorted values back to the array in-place, then delete trailing holes.
/// Steps 7-8 of the spec: set indices 0..itemCount-1, delete itemCount..len-1.
///
/// Fast path: the write-back is a wholesale elements replacement — sorted
/// values dense at [0, itemCount), holes at [itemCount, len) — done in one
/// heap read-modify-write. Eligibility is re-checked here (not reused from
/// collection time) because comparefn is user code that may have added index
/// overrides, swapped the prototype, or resized the array; the expected_len
/// check inside try_elements_fast_path catches resizes.
fn write_sort_result(
  state: State,
  ref: Ref,
  values: List(JsValue),
  length: Int,
  idx: Int,
) -> Result(State, #(JsValue, State)) {
  let fast = case idx == 0 {
    True -> {
      use _els, len <- try_elements_fast_path(state, ref, length)
      #(elements.from_list(values), len, Nil)
    }
    False -> None
  }
  case fast {
    Some(#(Nil, state)) -> Ok(state)
    None ->
      case values {
        [val, ..rest] -> {
          // Step 7a: Set(obj, ToString(j), items[j], true).
          use state <- result.try(generic_set_index(state, ref, idx, val))
          write_sort_result(state, ref, rest, length, idx + 1)
        }
        [] ->
          // Step 8: Delete remaining indices (holes at the end).
          delete_trailing(state, ref, idx, length)
      }
  }
}

/// Delete trailing indices from idx to length-1.
/// Step 8: Repeat, while j < len, DeletePropertyOrThrow(obj, ToString(j)).
fn delete_trailing(
  state: State,
  ref: Ref,
  idx: Int,
  length: Int,
) -> Result(State, #(JsValue, State)) {
  case idx >= length {
    True -> Ok(state)
    False -> {
      use state <- result.try(generic_delete_index(state, ref, idx))
      delete_trailing(state, ref, idx + 1, length)
    }
  }
}

/// Shared iteration driver used by every, some, find, findIndex, and forEach.
///
/// Generalizes two spec patterns:
///
/// Pattern A — "HasProperty + Get" loop (every §23.1.3.5, some §23.1.3.27,
/// forEach §23.1.3.13):
///   5. Repeat, while k < len,
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kPresent be ? HasProperty(O, Pk).
///      c. If kPresent is true, then
///         i. Let kValue be ? Get(O, Pk).
///         ii. Let testResult be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
///         iii. If testResult is <false|true>, return <false|true>.
///      d. Set k to k + 1.
///
/// Pattern B — "Get only" loop via FindViaPredicate (find §23.1.3.9,
/// findIndex §23.1.3.10):
///   4. For each integer k of indices, do
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kValue be ? Get(O, Pk).
///      c. Let testResult be ToBoolean(? Call(predicate, thisArg, « kValue, 𝔽(k), O »)).
///      d. If testResult is true, return Record { ... }.
///
/// The key difference: Pattern A checks HasProperty first (skips holes),
/// Pattern B always calls Get (holes become undefined). This is controlled
/// by the `hole_mode` parameter:
///   - SkipHoles → Pattern A (HasProperty check via get_index_if_present)
///   - VisitHoles → Pattern B (always visit, holes read as undefined)
///
/// The `stop_on` predicate controls the early-exit condition:
///   - every: stop_on = !is_truthy (step 5c.iii: testResult is false → return)
///   - some/find/findIndex: stop_on = is_truthy (step 5c.iii/4d: testResult is true → return)
///   - forEach: stop_on = fn(_) { False } (never stops early)
///
/// Returns via `cont(element, idx, state)`:
///   - If stopped early: cont(kValue, k, state) — the element and index that triggered stop.
///   - If loop completed: cont(JsUndefined, length, state) — sentinel idx=length signals completion.
fn iterate_array(
  state: State,
  arr: JsValue,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  hole_mode: HoleMode,
  stop_on: fn(JsValue) -> Bool,
  cont: fn(JsValue, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  // Step 4 (Pattern A/B): k = 0, begin loop ascending.
  iterate_loop(
    state,
    arr,
    0,
    length,
    1,
    limits.max_iteration,
    cb,
    this_arg,
    hole_mode,
    stop_on,
    cont,
  )
}

/// Like iterate_array but iterates [len-1..0] descending — for findLast /
/// findLastIndex, which delegate to FindViaPredicate (§23.1.3.9.1) with
/// direction = descending.
///
/// Returns via `cont(element, idx, state)`:
///   - If stopped early: cont(kValue, k, state) — the element and index that triggered stop.
///   - If loop completed: cont(JsUndefined, -1, state) — sentinel idx=-1 signals completion.
fn iterate_array_rev(
  state: State,
  arr: JsValue,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  hole_mode: HoleMode,
  stop_on: fn(JsValue) -> Bool,
  cont: fn(JsValue, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  // k = len - 1, begin loop descending (end = -1, step = -1).
  iterate_loop(
    state,
    arr,
    length - 1,
    -1,
    -1,
    limits.max_iteration,
    cb,
    this_arg,
    hole_mode,
    stop_on,
    cont,
  )
}

/// Inner loop of iterate_array / iterate_array_rev. Each recursive call
/// corresponds to one iteration of the spec's "Repeat, while k < len"
/// (Pattern A) or "For each integer k of indices" (Pattern B).
///
/// Bidirectional: `step` is +1 (ascending) or -1 (descending), and `end` is
/// the exclusive terminal index (`length` for ascending, `-1` for descending).
///
/// `fuel` is the pragmatic step budget (see iteration_budget_msg) — methods
/// with early exits (every/some/find) on huge array-likes stop long before
/// it runs out; a loop that actually exceeds it throws RangeError.
fn iterate_loop(
  state: State,
  arr: JsValue,
  idx: Int,
  end: Int,
  step: Int,
  fuel: Int,
  cb: JsValue,
  this_arg: JsValue,
  hole_mode: HoleMode,
  stop_on: fn(JsValue) -> Bool,
  cont: fn(JsValue, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  use <- bool.lazy_guard(fuel <= 0 && idx != end, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  // Loop termination: k == end → completed without early exit.
  // Pattern A step 6 / Pattern B step 5.
  case idx == end {
    True -> cont(JsUndefined, end, state)
    False -> {
      // Pattern A step 5b-5c.i: kPresent = HasProperty(O, Pk); if true,
      //   kValue = Get(O, Pk) — fused into one heap lookup.
      // Pattern B step 4b: kValue = Get(O, Pk) — holes yield undefined.
      use maybe_elem, state <- state.try_op(get_index_if_present(
        state,
        arr,
        idx,
      ))
      let maybe_elem = case hole_mode {
        // Pattern B: visit holes as undefined (Get returns undefined for absent).
        VisitHoles -> Some(option.unwrap(maybe_elem, JsUndefined))
        SkipHoles -> maybe_elem
      }
      case maybe_elem {
        // Pattern A step 5b-c: kPresent is false → skip, advance k.
        None ->
          iterate_loop(
            state,
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
          // Pattern A step 5c.ii / Pattern B step 4c:
          // testResult = ToBoolean(Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
          use result, state <- state.try_call(state, cb, this_arg, [
            elem,
            value.from_int(idx),
            arr,
          ])
          // Pattern A step 5c.iii / Pattern B step 4d:
          // If testResult matches stop condition, return early.
          case stop_on(result) {
            True -> cont(elem, idx, state)
            // Step 5d / continue to next k.
            False ->
              iterate_loop(
                state,
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

/// Generic accumulator-driven iteration for map / filter / flatMap.
///
/// All three methods share the skeleton of §23.1.3.19 steps 5-6 (map) /
/// §23.1.3.8 steps 5-7 (filter) / §23.1.3.14 via FlattenIntoArray (flatMap):
///
///   5. Let k be 0.
///   6. Repeat, while k < len,
///      a. Let Pk be ! ToString(𝔽(k)).
///      b. Let kPresent be ? HasProperty(O, Pk).
///      c. If kPresent is true, then
///         i. Let kValue be ? Get(O, Pk).
///         ii. Let result be ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
///         iii. <method-specific: store result into accumulator>
///      d. Set k to k + 1.
///
/// The ONLY per-method variation is step 6c.iii (how the callback result
/// combines with the accumulator), parameterized here by `combine`:
///   - map:     set acc[k] = result (preserving holes via sparse elements)
///   - filter:  if ToBoolean(result) then prepend kValue to acc
///   - flatMap: flatten result one level into acc
///
/// Holes are always skipped (step 6b HasProperty check) — all three methods
/// follow Pattern A (SkipHoles).
fn fold_array(
  state: State,
  arr: JsValue,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  initial: acc,
  combine: fn(State, acc, JsValue, JsValue, Int) -> acc,
) -> #(State, Result(acc, JsValue)) {
  // Pragmatic bound: this loop has no early exit, so the step count equals
  // len — fail fast on lengths that would hang (see iteration_budget_msg).
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    let #(err, state) = case state.range_error(state, iteration_budget_msg) {
      #(state, Error(err)) -> #(err, state)
      #(state, Ok(val)) -> #(val, state)
    }
    #(state, Error(err))
  })
  fold_loop(state, arr, 0, length, cb, this_arg, initial, combine)
}

/// Inner loop of fold_array. Each recursive call corresponds to one
/// iteration of the spec's "Repeat, while k < len" with a HasProperty
/// check (SkipHoles pattern).
fn fold_loop(
  state: State,
  arr: JsValue,
  idx: Int,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  acc: acc,
  combine: fn(State, acc, JsValue, JsValue, Int) -> acc,
) -> #(State, Result(acc, JsValue)) {
  // Step 6 loop condition: k < len
  case idx >= length {
    True -> #(state, Ok(acc))
    False -> {
      // Step 6b-6c.i: kPresent = HasProperty(O, Pk); if true, kValue = Get(O, Pk)
      // — fused into one heap lookup.
      use maybe_elem, state <- state.try_op(get_index_if_present(
        state,
        arr,
        idx,
      ))
      case maybe_elem {
        // kPresent is false → skip (step 6d: k = k + 1)
        None ->
          fold_loop(state, arr, idx + 1, length, cb, this_arg, acc, combine)
        Some(elem) -> {
          // Step 6c.ii: result = Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)
          use result, state <- state.try_call(state, cb, this_arg, [
            elem,
            value.from_int(idx),
            arr,
          ])
          // Step 6c.iii: method-specific accumulator update
          let acc = combine(state, acc, result, elem, idx)
          // Step 6d: k = k + 1
          fold_loop(state, arr, idx + 1, length, cb, this_arg, acc, combine)
        }
      }
    }
  }
}

// ============================================================================
// Reduce methods
// ============================================================================

/// Array.prototype.reduce ( callbackfn [ , initialValue ] )
/// ES2024 §23.1.3.23
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
/// 4. If len = 0 and initialValue is not present, throw a TypeError exception.
/// 5. Let k be 0.
/// 6. Let accumulator be undefined.
/// 7. If initialValue is present, then
///    a. Set accumulator to initialValue.
/// 8. Else,
///    a. Let kPresent be false.
///    b. Repeat, while kPresent is false and k < len,
///       i. Let Pk be ! ToString(𝔽(k)).
///       ii. Set kPresent to ? HasProperty(O, Pk).
///       iii. If kPresent is true, then
///            1. Set accumulator to ? Get(O, Pk).
///       iv. Set k to k + 1.
///    c. If kPresent is false, throw a TypeError exception.
/// 9. Repeat, while k < len,
///    a. Let Pk be ! ToString(𝔽(k)).
///    b. Let kPresent be ? HasProperty(O, Pk).
///    c. If kPresent is true, then
///       i. Let kValue be ? Get(O, Pk).
///       ii. Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
///    d. Set k to k + 1.
/// 10. Return accumulator.
///
fn array_reduce(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  reduce_impl(this, args, state, 1)
}

/// Array.prototype.reduceRight ( callbackfn [ , initialValue ] )
/// ES2024 §23.1.3.24
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
/// 4. If len = 0 and initialValue is not present, throw a TypeError exception.
/// 5. Let k be len - 1.
/// 6. Let accumulator be undefined.
/// 7. If initialValue is present, then
///    a. Set accumulator to initialValue.
/// 8. Else,
///    a. Let kPresent be false.
///    b. Repeat, while kPresent is false and k ≥ 0,
///       i. Let Pk be ! ToString(𝔽(k)).
///       ii. Set kPresent to ? HasProperty(O, Pk).
///       iii. If kPresent is true, then
///            1. Set accumulator to ? Get(O, Pk).
///       iv. Set k to k - 1.
///    c. If kPresent is false, throw a TypeError exception.
/// 9. Repeat, while k ≥ 0,
///    a. Let Pk be ! ToString(𝔽(k)).
///    b. Let kPresent be ? HasProperty(O, Pk).
///    c. If kPresent is true, then
///       i. Let kValue be ? Get(O, Pk).
///       ii. Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
///    d. Set k to k - 1.
/// 10. Return accumulator.
///
fn array_reduce_right(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  reduce_impl(this, args, state, -1)
}

/// Shared implementation of reduce/reduceRight. `step` controls direction:
/// 1 = reduce (forward, start=0, end=len), -1 = reduceRight (backward,
/// start=len-1, end=-1).
fn reduce_impl(
  this: JsValue,
  args: List(JsValue),
  state: State,
  step: Int,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: Let O be ? ToObject(this value). Let len be ? LengthOfArrayLike(O).
  use this, _ref, length, state <- require_array(this, state)
  // Step 3 setup: extract callbackfn argument
  let cb = helpers.first_arg_or_undefined(args)
  // Step 3: If IsCallable(callbackfn) is false, throw a TypeError exception.
  use <- bool.guard(
    !helpers.is_callable(state.heap, cb),
    state.type_error(
      state,
      common.typeof_value(cb, state.heap) <> " is not a function",
    ),
  )
  // Step 5: k = 0 (reduce) or len - 1 (reduceRight); loop bound is exclusive.
  let #(start, end) = case step {
    1 -> #(0, length)
    _ -> #(length - 1, -1)
  }
  // Step 7: If initialValue is present, set accumulator to initialValue.
  // Presence checked by arg count (args.length >= 2).
  let #(has_init, init) = case args {
    [_, v, ..] -> #(True, v)
    _ -> #(False, JsUndefined)
  }
  case has_init {
    // Steps 5, 7a, 9: accumulator=initialValue, enter main loop
    True ->
      reduce_loop(state, this, start, end, cb, init, step, limits.max_iteration)
    False -> {
      // Steps 8a-8c: Find first present element (in iteration order) as
      // initial accumulator. find_present implements step 8b's loop.
      use found, state <- state.try_op(find_present(
        state,
        this,
        start,
        end,
        step,
        limits.max_iteration,
      ))
      case found {
        // Step 4/8c: If len=0 or no present element found, throw TypeError.
        None ->
          state.type_error(state, "Reduce of empty array with no initial value")
        // Step 8b.iii: Set accumulator to Get(O, Pk), then k = k ± 1.
        // Step 9: Enter main loop starting at first_idx + step.
        Some(#(first_idx, first_val)) ->
          reduce_loop(
            state,
            this,
            first_idx + step,
            end,
            cb,
            first_val,
            step,
            limits.max_iteration,
          )
      }
    }
  }
}

/// Implements §23.1.3.23 step 8b / §23.1.3.24 step 8b:
/// When no initialValue is provided, scan for the first present (non-hole) element
/// to use as the initial accumulator.
///
/// For reduce (step=1): step 8b says "Repeat, while kPresent is false and k < len"
///   — scans forward from index 0, checking HasProperty at each k.
/// For reduceRight (step=-1): step 8b says "Repeat, while kPresent is false and k ≥ 0"
///   — scans backward from index len-1.
///
/// Step 8b.ii-iii.1: kPresent = HasProperty(O, Pk); if true, accumulator =
/// Get(O, Pk) — fused into one lookup via get_index_if_present.
/// Step 8b.iv: Set k to k ± 1 — implemented by idx + step.
///
/// Returns Some(#(index, value)) on the first present element found (step 8b.iii),
/// or None if no present element exists (triggering step 8c TypeError).
fn find_present(
  state: State,
  this: JsValue,
  idx: Int,
  end: Int,
  step: Int,
  fuel: Int,
) -> Result(#(Option(#(Int, JsValue)), State), #(JsValue, State)) {
  // Loop termination: k < len (forward) or k ≥ 0 (backward)
  case idx == end {
    // Step 8c: kPresent is false after exhausting all indices
    True -> Ok(#(None, state))
    False -> {
      // Pragmatic per-step budget — see iteration_budget_msg.
      use <- bool.lazy_guard(fuel <= 0, fn() {
        range_error_op(state, iteration_budget_msg)
      })
      // Step 8b.ii-iii: kPresent = HasProperty(O, Pk); if true,
      // accumulator = Get(O, Pk) — fused into one heap lookup.
      use #(maybe_val, state) <- result.try(get_index_if_present(
        state,
        this,
        idx,
      ))
      case maybe_val {
        // Step 8b.iii: kPresent is true
        Some(val) -> Ok(#(Some(#(idx, val)), state))
        // Step 8b.iv: Set k to k + 1 (or k - 1 for reduceRight)
        None -> find_present(state, this, idx + step, end, step, fuel - 1)
      }
    }
  }
}

/// Implements §23.1.3.23 step 9 / §23.1.3.24 step 9:
/// The main iteration loop for both reduce and reduceRight.
///
/// For reduce (step=1, end=len): step 9 says "Repeat, while k < len"
/// For reduceRight (step=-1, end=-1): step 9 says "Repeat, while k ≥ 0"
///
/// This is a unified bidirectional implementation — step controls direction,
/// end is the exclusive termination bound. Both directions share the same
/// algorithm structure (steps 9a-9d are identical between the two specs).
///
/// Per-index reads via get_index (which uses object.get_value_of — walks
/// prototype chain and invokes getters).
fn reduce_loop(
  state: State,
  arr: JsValue,
  idx: Int,
  end: Int,
  cb: JsValue,
  acc: JsValue,
  step: Int,
  fuel: Int,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 9 loop condition: k < len (forward) or k ≥ 0 (backward)
  case idx == end {
    // Step 10: Return accumulator.
    True -> #(state, Ok(acc))
    False -> {
      // Pragmatic per-step budget — see iteration_budget_msg.
      use <- bool.lazy_guard(fuel <= 0, fn() {
        state.range_error(state, iteration_budget_msg)
      })
      // Step 9b-9c.i: kPresent = HasProperty(O, Pk); if true, kValue = Get(O, Pk)
      // — fused into one heap lookup.
      use maybe_elem, state <- state.try_op(get_index_if_present(
        state,
        arr,
        idx,
      ))
      case maybe_elem {
        // kPresent is false — skip this index (hole).
        // Step 9d: Set k to k + 1 (or k - 1 for reduceRight).
        None ->
          reduce_loop(state, arr, idx + step, end, cb, acc, step, fuel - 1)
        // Step 9c: If kPresent is true, then
        Some(elem) -> {
          // Step 9c.ii: Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
          // Note: thisArg is always undefined for reduce/reduceRight (no thisArg parameter).
          use result, state <- state.try_call(state, cb, JsUndefined, [
            acc,
            elem,
            value.from_int(idx),
            arr,
          ])
          // Step 9d: Set k to k + 1 (or k - 1 for reduceRight).
          reduce_loop(state, arr, idx + step, end, cb, result, step, fuel - 1)
        }
      }
    }
  }
}

// ============================================================================
// Array.prototype.splice (ES2024 §23.1.3.31)
// ============================================================================

/// Array.prototype.splice ( start, deleteCount, ...items )
/// ES2024 §23.1.3.31
///
/// Removes elements from an array and, if necessary, inserts new elements in
/// their place, returning the deleted elements.
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. Let relativeStart be ? ToIntegerOrInfinity(start).
/// 4. If relativeStart = -∞, let actualStart = 0.
/// 5. Else if relativeStart < 0, let actualStart = max(len + relativeStart, 0).
/// 6. Else, let actualStart = min(relativeStart, len).
/// 7-10. Compute actualDeleteCount depending on argument count.
/// 11-18. Build removed array, shift elements, insert items, set length.
fn array_splice(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, ref, length, state <- require_array(this, state)
  // Steps 3-6: actualStart
  use actual_start, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 0),
    length,
    0,
  )
  // Steps 7-10: actualDeleteCount (observable ToIntegerOrInfinity)
  use #(actual_delete_count, items), state <- try_delete_count(
    state,
    args,
    length,
    actual_start,
  )
  // Step 11 prologue: A = ArraySpeciesCreate(O, actualDeleteCount) — the
  // species constructor (if any) runs before the removal copy loop.
  use species, state <- state.try_op(array_species_create(
    state,
    this,
    actual_delete_count,
  ))
  // Steps 12-13: Build the removed array A from
  // [actualStart..actualStart+actualDeleteCount)
  use removed_arr, state <- state.try_op(case species {
    None -> {
      use #(removed_elements, state) <- result.try(copy_range(
        state,
        this,
        actual_start,
        0,
        actual_delete_count,
        elements.new(),
      ))
      let #(heap, removed_ref) =
        common.alloc_array_from_elements(
          state.heap,
          removed_elements,
          actual_delete_count,
          array_proto,
        )
      Ok(#(JsObject(removed_ref), State(..state, heap:)))
    }
    Some(target) -> {
      // Steps 12-13 (species path): HasProperty → Get →
      // CreateDataPropertyOrThrow interleaved per k, so an abrupt completion
      // from the target's [[DefineOwnProperty]] ends huge-deleteCount loops
      // early; then Set(A, "length", 𝔽(actualDeleteCount), true).
      use state <- result.try(copy_range_to_species(
        state,
        this,
        actual_start,
        target,
        0,
        actual_delete_count,
        limits.max_iteration,
      ))
      use state <- result.map(generic_set_length(
        state,
        target,
        actual_delete_count,
      ))
      #(JsObject(target), state)
    }
  })
  // Steps 12-17: Shift elements and insert items.
  let item_count = list.length(items)
  let new_length = length - actual_delete_count + item_count
  // §23.1.3.31 step 11: If len + insertCount - actualDeleteCount > 2^53 - 1, throw TypeError
  use <- state.guard_safe_length(state, new_length)
  let shift = item_count - actual_delete_count
  // Fast path: shift + insert + truncate + length update fused into one heap
  // read-modify-write. Eligibility is checked here, after copy_range — on
  // this path copy_range ran no user code, so length is still accurate.
  let fast = {
    use els, len <- try_elements_fast_path(state, ref, length)
    let move_from = actual_start + actual_delete_count
    let els = case shift > 0, shift < 0 {
      True, _ -> elements.move_up(els, move_from, len, shift)
      _, True -> elements.move_down(els, move_from, len, -shift)
      _, _ -> els
    }
    let els =
      elements.write_list(els, actual_start, items)
      |> elements.truncate(new_length)
    #(els, new_length, Nil)
  }
  case fast {
    Some(#(Nil, state)) -> #(state, Ok(removed_arr))
    None -> {
      // If shift > 0: move elements right. If shift < 0: move left.
      // If shift == 0: no shifting needed.
      use state <- state.try_state(splice_shift(
        state,
        ref,
        actual_start,
        actual_delete_count,
        length,
        shift,
      ))
      // Step 15: Insert items at actualStart.
      use state <- state.try_state(splice_insert(
        state,
        ref,
        actual_start,
        items,
      ))
      // Step 17: Set length.
      wrap(generic_set_length(state, ref, new_length), removed_arr)
    }
  }
}

/// Shift elements for splice: move elements at [start+deleteCount..len) to
/// [start+itemCount..len+shift). Handles both rightward (shift>0) and
/// leftward (shift<0) moves, and deletes trailing elements when shrinking.
fn splice_shift(
  state: State,
  ref: Ref,
  start: Int,
  delete_count: Int,
  length: Int,
  shift: Int,
) -> Result(State, #(JsValue, State)) {
  let from_start = start + delete_count
  case shift > 0 {
    // Moving right: iterate from end to avoid overwriting
    True ->
      move_range(
        state,
        ref,
        length - 1,
        from_start,
        -1,
        shift,
        limits.max_iteration,
      )
    False ->
      case shift < 0 {
        // Moving left: iterate from start
        True -> {
          use state <- result.try(move_range(
            state,
            ref,
            from_start,
            length,
            1,
            shift,
            limits.max_iteration,
          ))
          // Delete trailing elements that are now beyond the new length
          delete_trailing(state, ref, length + shift, length)
        }
        // No shift needed
        False -> Ok(state)
      }
  }
}

/// Insert items at the given start index.
fn splice_insert(
  state: State,
  ref: Ref,
  start: Int,
  items: List(JsValue),
) -> Result(State, #(JsValue, State)) {
  case items {
    [] -> Ok(state)
    [item, ..rest] -> {
      use state <- result.try(generic_set_index(state, ref, start, item))
      splice_insert(state, ref, start + 1, rest)
    }
  }
}

// ============================================================================
// Array.prototype.findLast / findLastIndex (ES2024 §23.1.3.10.1 / §23.1.3.10.2)
// ============================================================================

/// Array.prototype.findLast ( predicate [ , thisArg ] )
/// ES2024 §23.1.3.11
///
/// Like find() but searches from end to start.
/// Uses FindViaPredicate with direction = descending.
fn array_find_last(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use elem, idx, _len, state <- find_via_predicate(this, args, state, True)
  case idx >= 0 {
    True -> #(state, Ok(elem))
    False -> #(state, Ok(JsUndefined))
  }
}

/// Array.prototype.findLastIndex ( predicate [ , thisArg ] )
/// ES2024 §23.1.3.12
///
/// Like findIndex() but searches from end to start.
fn array_find_last_index(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _elem, idx, _len, state <- find_via_predicate(this, args, state, True)
  // idx is already -1 when not found (descending sentinel).
  #(state, Ok(value.from_int(idx)))
}

// ============================================================================
// Array.prototype.flat / flatMap (ES2024 §23.1.3.13 / §23.1.3.14)
// ============================================================================

/// Array.prototype.flat ( [ depth ] )
/// ES2024 §23.1.3.13
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let sourceLen be ? LengthOfArrayLike(O).
/// 3. Let depthNum be 1 (default).
/// 4. If depth is not undefined, set depthNum to ? ToIntegerOrInfinity(depth).
/// 5. If depthNum < 0, set depthNum to 0.
/// 6. Let A be ? ArraySpeciesCreate(O, 0).
/// 7. Perform ? FlattenIntoArray(A, O, sourceLen, 0, depthNum).
/// 8. Return A.
fn array_flat(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + LengthOfArrayLike
  use this, _ref, length, state <- require_array(this, state)
  // Steps 3-5: depth (default 1; observable ToIntegerOrInfinity otherwise)
  let proceed = fn(depth: Int, state: State) {
    // Step 6: A = ArraySpeciesCreate(O, 0) — the species constructor (if
    // any) runs before the flatten loop.
    use species, state <- state.try_op(array_species_create(state, this, 0))
    // Step 7: FlattenIntoArray(A, O, sourceLen, 0, depthNum)
    let flattened = flatten_into(state, this, length, depth, [])
    finish_species_list(flattened, species)
  }
  case helpers.first_arg_or_undefined(args) {
    JsUndefined -> proceed(1, state)
    d -> {
      use raw, state <- try_integer_or_infinity(state, d)
      proceed(int.max(raw, 0), state)
    }
  }
}

/// Step 8 "Return A" shared by flat/flatMap: reverse the accumulated list,
/// then either allocate a plain array (no custom species) or write into the
/// species-constructed target via CreateDataPropertyOrThrow (which throws on
/// a non-extensible / non-configurable-property target). Neither method sets
/// "length" on the species target (FlattenIntoArray has no final Set).
fn finish_species_list(
  result: #(State, Result(List(JsValue), JsValue)),
  species: Option(Ref),
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, outcome) = result
  case outcome {
    Error(thrown) -> #(state, Error(thrown))
    Ok(kept) -> {
      let kept = list.reverse(kept)
      case species {
        None -> state.ok_array(state, kept)
        Some(target) -> {
          let count = list.length(kept)
          case
            write_species_result(
              state,
              target,
              elements.from_list(kept),
              count,
              None,
            )
          {
            Ok(state) -> #(state, Ok(JsObject(target)))
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        }
      }
    }
  }
}

/// FlattenIntoArray (ES2024 §23.1.3.13.1)
///
/// Recursively flattens array elements up to the given depth.
/// Returns elements in reverse order (caller must reverse).
fn flatten_into(
  state: State,
  src: JsValue,
  length: Int,
  depth: Int,
  acc: List(JsValue),
) -> #(State, Result(List(JsValue), JsValue)) {
  // Pragmatic bound: visits all length indices with no early exit
  // (see iteration_budget_msg).
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    let #(state, res) = state.range_error(state, iteration_budget_msg)
    case res {
      Error(err) -> #(state, Error(err))
      Ok(val) -> #(state, Error(val))
    }
  })
  flatten_into_loop(state, src, 0, length, depth, acc)
}

fn flatten_into_loop(
  state: State,
  src: JsValue,
  idx: Int,
  length: Int,
  depth: Int,
  acc: List(JsValue),
) -> #(State, Result(List(JsValue), JsValue)) {
  case idx >= length {
    True -> #(state, Ok(acc))
    False -> {
      use maybe_elem, state <- state.try_op(get_index_if_present(
        state,
        src,
        idx,
      ))
      case maybe_elem {
        None ->
          // Hole: skip
          flatten_into_loop(state, src, idx + 1, length, depth, acc)
        Some(elem) ->
          // If depth > 0 and element is an array, recursively flatten
          case depth > 0 {
            True ->
              // Step 3.c.ii: shouldFlatten = ? IsArray(element) — pierces
              // proxies to their target (§7.2.2; revoked → TypeError).
              case elem, object.is_array(state.heap, elem) {
                JsObject(_), Error(Nil) ->
                  case
                    coerce.thrown_type_error(
                      state,
                      "Cannot perform 'IsArray' on a proxy that has been revoked",
                    )
                  {
                    Error(#(thrown, state)) -> #(state, Error(thrown))
                    // thrown_type_error always returns Error; keeps the case
                    // exhaustive without an assert.
                    Ok(_) -> #(state, Ok(acc))
                  }
                JsObject(sub_ref), Ok(True) -> {
                  // Step 3.c.iii.1: elementLen = ? LengthOfArrayLike(element)
                  // — an observable Get(element, "length") on proxies and
                  // array-likes with length accessors.
                  use sub_len, state <- state.try_op(object_length(
                    state,
                    sub_ref,
                  ))
                  // Recurse with depth - 1
                  case flatten_into(state, elem, sub_len, depth - 1, acc) {
                    #(state, Ok(new_acc)) ->
                      flatten_into_loop(
                        state,
                        src,
                        idx + 1,
                        length,
                        depth,
                        new_acc,
                      )
                    #(state, Error(thrown)) -> #(state, Error(thrown))
                  }
                }
                _, _ ->
                  // Not an array, just append
                  flatten_into_loop(state, src, idx + 1, length, depth, [
                    elem,
                    ..acc
                  ])
              }
            False ->
              // Depth is 0, just append
              flatten_into_loop(state, src, idx + 1, length, depth, [
                elem,
                ..acc
              ])
          }
      }
    }
  }
}

/// Array.prototype.flatMap ( mapperFunction [ , thisArg ] )
/// ES2024 §23.1.3.14
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let sourceLen be ? LengthOfArrayLike(O).
/// 3. If IsCallable(mapperFunction) is false, throw a TypeError exception.
/// 4. Let A be ? ArraySpeciesCreate(O, 0).
/// 5. Perform ? FlattenIntoArray(A, O, sourceLen, 0, 1, mapperFunction, thisArg).
/// 6. Return A.
fn array_flat_map(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + LengthOfArrayLike
  use this, _ref, length, state <- require_array(this, state)
  // Step 3: IsCallable check
  use cb, this_arg, state <- require_callback(args, state)
  // Pragmatic bound: visits all sourceLen indices with no early exit
  // (see iteration_budget_msg).
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  // Step 4: A = ArraySpeciesCreate(O, 0) — the species constructor (if any)
  // runs before the mapping loop.
  use species, state <- state.try_op(array_species_create(state, this, 0))
  // Step 5: FlattenIntoArray with mapperFunction — call mapper on each present
  // element, then flatten the result one level deep via flatten_into at depth 0
  // (which just collects present elements). If the mapped value is not an array,
  // prepend directly.
  let flattened = flat_map_loop(state, this, 0, length, cb, this_arg, [])
  // Step 6: Return A.
  finish_species_list(flattened, species)
}

/// Per-element loop for flatMap: call mapper, then flatten result one level.
fn flat_map_loop(
  state: State,
  arr: JsValue,
  idx: Int,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  acc: List(JsValue),
) -> #(State, Result(List(JsValue), JsValue)) {
  case idx >= length {
    True -> #(state, Ok(acc))
    False -> {
      use maybe_elem, state <- state.try_op(get_index_if_present(
        state,
        arr,
        idx,
      ))
      case maybe_elem {
        None -> flat_map_loop(state, arr, idx + 1, length, cb, this_arg, acc)
        Some(elem) -> {
          use mapped, state <- state.try_call(state, cb, this_arg, [
            elem,
            value.from_int(idx),
            arr,
          ])
          // Step 3.c.ii (FlattenIntoArray with mapper): shouldFlatten =
          // ? IsArray(mapped) — pierces proxies (§7.2.2; revoked → TypeError).
          case mapped, object.is_array(state.heap, mapped) {
            JsObject(_), Error(Nil) ->
              case
                coerce.thrown_type_error(
                  state,
                  "Cannot perform 'IsArray' on a proxy that has been revoked",
                )
              {
                Error(#(thrown, state)) -> #(state, Error(thrown))
                // thrown_type_error always returns Error; keeps the case
                // exhaustive without an assert.
                Ok(_) -> #(state, Ok(acc))
              }
            JsObject(sub_ref), Ok(True) -> {
              // elementLen = ? LengthOfArrayLike(mapped) — observable Get
              // of "length" on proxies / length accessors.
              use sub_len, state <- state.try_op(object_length(state, sub_ref))
              // Flatten one level: collect sub-array's present elements.
              case flatten_into(state, mapped, sub_len, 0, acc) {
                #(state, Ok(new_acc)) ->
                  flat_map_loop(
                    state,
                    arr,
                    idx + 1,
                    length,
                    cb,
                    this_arg,
                    new_acc,
                  )
                #(state, Error(thrown)) -> #(state, Error(thrown))
              }
            }
            _, _ ->
              flat_map_loop(state, arr, idx + 1, length, cb, this_arg, [
                mapped,
                ..acc
              ])
          }
        }
      }
    }
  }
}

// ============================================================================
// Array.prototype.copyWithin (ES2024 §23.1.3.4)
// ============================================================================

/// Array.prototype.copyWithin ( target, start [ , end ] )
/// ES2024 §23.1.3.4
///
/// 1. Let O be ? ToObject(this value).
/// 2. Let len be ? LengthOfArrayLike(O).
/// 3. Let relativeTarget be ? ToIntegerOrInfinity(target).
/// 4-5. Compute to from relativeTarget.
/// 6. Let relativeStart be ? ToIntegerOrInfinity(start).
/// 7-8. Compute from from relativeStart.
/// 9-11. Compute final from end argument.
/// 12. Let count be min(final - from, len - to).
/// 13-15. Handle copy direction to avoid overlap issues.
/// 16. Return O.
fn array_copy_within(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, ref, length, state <- require_array(this, state)
  // Steps 3-5: target (observable ToIntegerOrInfinity, in argument order)
  use target, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 0),
    length,
    0,
  )
  // Steps 6-8: start (from)
  use from, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 1),
    length,
    0,
  )
  // Steps 9-11: end (final)
  use final, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 2),
    length,
    length,
  )
  // Step 12: count = min(final - from, len - to)
  let count = int.min(final - from, length - target)
  // Pragmatic bound: count moves with no early exit (see iteration_budget_msg).
  use <- bool.lazy_guard(count > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  case count <= 0 {
    True -> #(state, Ok(this))
    False -> {
      // Fast path: direction-aware bulk copy in one heap read-modify-write
      // (elements.copy_within picks the safe iteration direction itself).
      let fast = {
        use els, len <- try_elements_fast_path(state, ref, length)
        #(elements.copy_within(els, from, target, count), len, Nil)
      }
      case fast {
        Some(#(Nil, state)) -> #(state, Ok(this))
        None ->
          // Steps 13-15: Direction-aware copy
          case from < target && target < from + count {
            // Overlapping, copy backwards
            True ->
              wrap(
                copy_within_step(
                  state,
                  ref,
                  from + count - 1,
                  target + count - 1,
                  -1,
                  count,
                ),
                this,
              )
            // No overlap issue, copy forwards
            False ->
              wrap(copy_within_step(state, ref, from, target, 1, count), this)
          }
      }
    }
  }
}

/// Copy `remaining` elements one at a time, advancing `from`/`to` by `step`
/// (+1 forward; -1 backward for overlapping regions).
fn copy_within_step(
  state: State,
  ref: Ref,
  from: Int,
  to: Int,
  step: Int,
  remaining: Int,
) -> Result(State, #(JsValue, State)) {
  case remaining <= 0 {
    True -> Ok(state)
    False -> {
      // §23.1.3.4 step 17c: fromPresent = ? HasProperty(O, fromKey) —
      // trap-aware (a proxy "has" trap can throw; return-abrupt-from-has-*).
      use #(has_from, state) <- result.try(generic_has_op(state, ref, from))
      use state <- result.try(case has_from {
        True -> {
          use #(val, state) <- result.try(generic_get(state, ref, from))
          generic_set_index(state, ref, to, val)
        }
        False -> generic_delete_index(state, ref, to)
      })
      copy_within_step(state, ref, from + step, to + step, step, remaining - 1)
    }
  }
}

// ============================================================================
// Array.from (ES2024 §23.1.2.1) — static method
// ============================================================================

/// Array.from ( items [ , mapFn [ , thisArg ] ] )
/// ES2024 §23.1.2.1
///
/// Creates a new Array from an array-like or iterable object.
///
/// Simplified: handles arrays and array-like objects (objects with .length).
/// Iterator protocol support is not yet implemented.
fn array_from(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  let #(items_val, map_fn, this_arg) = case args {
    [i, m, t, ..] -> #(i, Some(m), t)
    [i, m] -> #(i, Some(m), JsUndefined)
    [i] -> #(i, None, JsUndefined)
    [] -> #(JsUndefined, None, JsUndefined)
  }
  // Validate mapFn if provided
  case map_fn {
    Some(mf) ->
      case mf {
        JsUndefined ->
          array_from_array_like(
            items_val,
            None,
            JsUndefined,
            array_proto,
            state,
          )
        _ ->
          case helpers.is_callable(state.heap, mf) {
            True ->
              array_from_array_like(
                items_val,
                Some(mf),
                this_arg,
                array_proto,
                state,
              )
            False ->
              state.type_error(
                state,
                common.typeof_value(mf, state.heap) <> " is not a function",
              )
          }
      }
    None ->
      array_from_array_like(items_val, None, JsUndefined, array_proto, state)
  }
}

/// Array.from implementation for array-like objects (non-iterator path).
///
/// Per-index reads via get_index (object.get_value_of) — handles arrays,
/// array-likes, and string primitives uniformly.
fn array_from_array_like(
  items: JsValue,
  map_fn: Option(JsValue),
  this_arg: JsValue,
  array_proto: Ref,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case items {
    JsNull | JsUndefined ->
      state.type_error(
        state,
        "Cannot create array from " <> common.typeof_value(items, state.heap),
      )
    _ -> {
      // §23.1.2.1 step 4: usingIterator = ? GetMethod(items, @@iterator).
      // Iterables (generators, Maps, Sets, custom iterators, proxies of
      // arrays) take the iterator path; everything else is array-like.
      use iter_method, state <- state.try_op(case items {
        JsObject(_) ->
          object.get_symbol_value_of(state, items, value.symbol_iterator)
        _ -> Ok(#(JsUndefined, state))
      })
      case iter_method {
        // GetMethod step 3: undefined or null → no iterator; array-like path.
        JsUndefined | JsNull -> {
          // Step 8: len = ? LengthOfArrayLike(arrayLike) — a throwing
          // "length" getter propagates, the getter's heap side effects are
          // kept via the threaded state, and ToLength's ToNumber coercion
          // (valueOf on an object-valued length) is observable.
          use len_val, state <- state.try_op(object.get_value_of(
            state,
            items,
            Named("length"),
          ))
          use length, state <- state.try_op(to_length_value(state, len_val))
          array_from_loop(
            state,
            items,
            0,
            length,
            map_fn,
            this_arg,
            array_proto,
            [],
          )
        }
        m ->
          case helpers.is_callable(state.heap, m) {
            True ->
              array_from_iterator(
                state,
                items,
                iter_method,
                map_fn,
                this_arg,
                array_proto,
              )
            // GetMethod step 4 (§7.3.10): present but not callable → TypeError.
            False ->
              state.type_error(
                state,
                common.typeof_value(m, state.heap) <> " is not a function",
              )
          }
      }
    }
  }
}

/// §23.1.2.1 steps 5.e-5.g: drain the iterator, applying mapFn per element.
fn array_from_iterator(
  state: State,
  items: JsValue,
  iter_method: JsValue,
  map_fn: Option(JsValue),
  this_arg: JsValue,
  array_proto: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  // GetIteratorFromMethod: iterator = ? Call(method, items).
  use iter, state <- state.try_call(state, iter_method, items, [])
  case iter {
    JsObject(_) -> {
      // GetIterator step: nextMethod fetched ONCE (§7.4.3).
      use next_method, state <- state.try_op(object.get_value_of(
        state,
        iter,
        Named("next"),
      ))
      array_from_iterator_loop(
        state,
        iter,
        next_method,
        map_fn,
        this_arg,
        array_proto,
        0,
        [],
      )
    }
    _ ->
      state.type_error(
        state,
        "Result of the Symbol.iterator method is not an object",
      )
  }
}

fn array_from_iterator_loop(
  state: State,
  iter: JsValue,
  next_method: JsValue,
  map_fn: Option(JsValue),
  this_arg: JsValue,
  array_proto: Ref,
  k: Int,
  acc: List(JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  use res, state <- state.try_call(state, next_method, iter, [])
  case res {
    JsObject(_) -> {
      use done_val, state <- state.try_op(object.get_value_of(
        state,
        res,
        Named("done"),
      ))
      case value.is_truthy(done_val) {
        True -> {
          let #(heap, ref) =
            common.alloc_array(state.heap, list.reverse(acc), array_proto)
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
        False -> {
          use item, state <- state.try_op(object.get_value_of(
            state,
            res,
            Named("value"),
          ))
          use mapped, state <- state.try_op(case map_fn {
            Some(mf) ->
              case state.call(state, mf, this_arg, [item, value.from_int(k)]) {
                Ok(#(v, state)) -> Ok(#(v, state))
                Error(e) -> Error(e)
              }
            None -> Ok(#(item, state))
          })
          array_from_iterator_loop(
            state,
            iter,
            next_method,
            map_fn,
            this_arg,
            array_proto,
            k + 1,
            [mapped, ..acc],
          )
        }
      }
    }
    _ -> state.type_error(state, "Iterator result is not an object")
  }
}

/// Array.from loop: iterate 0..length, optionally apply mapFn to each element.
/// Per spec §23.1.2.1 step 12: holes are read as undefined (Get without HasProperty).
fn array_from_loop(
  state: State,
  items: JsValue,
  idx: Int,
  length: Int,
  map_fn: Option(JsValue),
  this_arg: JsValue,
  array_proto: Ref,
  acc: List(JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  case idx >= length {
    True -> {
      let vals = list.reverse(acc)
      let #(heap, ref) =
        common.alloc_array_from_elements(
          state.heap,
          elements.from_list(vals),
          length,
          array_proto,
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    False -> {
      // Step 12.b: Let kValue be ? Get(items, Pk). Holes → undefined.
      use elem, state <- state.try_op(get_index(state, items, idx))
      case map_fn {
        None ->
          array_from_loop(
            state,
            items,
            idx + 1,
            length,
            map_fn,
            this_arg,
            array_proto,
            [elem, ..acc],
          )
        Some(mf) -> {
          // Step 12.c: mappedValue = Call(mapfn, thisArg, «kValue, k»)
          use mapped, state <- state.try_call(state, mf, this_arg, [
            elem,
            value.from_int(idx),
          ])
          array_from_loop(
            state,
            items,
            idx + 1,
            length,
            map_fn,
            this_arg,
            array_proto,
            [mapped, ..acc],
          )
        }
      }
    }
  }
}

// ============================================================================
// Array.of (ES2024 §23.1.2.3) — static method
// ============================================================================

/// Array.of ( ...items )
/// ES2024 §23.1.2.3
///
/// Creates a new Array instance from a variable number of arguments,
/// regardless of number or type of the arguments.
/// Unlike Array(), Array.of(7) creates [7] not an array with length 7.
fn array_of(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  let #(heap, ref) = common.alloc_array(state.heap, args, array_proto)
  #(State(..state, heap:), Ok(JsObject(ref)))
}

// ============================================================================
// Array.prototype.toSpliced (ES2024 §23.1.3.35)
// ============================================================================

/// Array.prototype.toSpliced ( start, skipCount, ...items )
/// ES2024 §23.1.3.35
///
/// Like splice() but returns a new array instead of modifying in place.
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. Let relativeStart be ? ToIntegerOrInfinity(start).
///   4. If relativeStart = -Infinity, let actualStart be 0.
///   5. Else if relativeStart < 0, let actualStart be max(len + relativeStart, 0).
///   6. Else, let actualStart be min(relativeStart, len).
///   7. Let insertCount and actualSkipCount be determined by number of args:
///      - 0 args: insertCount=0, actualSkipCount=0
///      - 1 arg (start only): insertCount=0, actualSkipCount=len-actualStart
///      - 2+ args: insertCount=argCount-2, actualSkipCount=min(max(ToIntegerOrInfinity(skipCount),0), len-actualStart)
///   8. Let newLen be len + insertCount - actualSkipCount.
///   9. If newLen > 2^53 - 1, throw a TypeError.
///  10. Let A be ? ArrayCreate(newLen).
///  11-14. Copy elements [0, actualStart), then items, then [actualStart+actualSkipCount, len).
///  15. Return A.
fn array_to_spliced(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, _ref, length, state <- require_array(this, state)
  // Steps 3-6: actualStart
  use actual_start, state <- try_resolve_index(
    state,
    arg_or_undefined(args, 0),
    length,
    0,
  )
  // Step 7: insertCount and actualSkipCount (observable ToIntegerOrInfinity)
  use #(actual_skip_count, items), state <- try_delete_count(
    state,
    args,
    length,
    actual_start,
  )
  // Step 8: newLen = len + insertCount - actualSkipCount
  let item_count = list.length(items)
  let new_len = length + item_count - actual_skip_count
  // Step 9: If newLen > 2^53 - 1, throw a TypeError.
  use <- state.guard_safe_length(state, new_len)
  // Steps 10-14: Build the new array
  // Copy [0, actualStart) from source via Get (holes -> undefined).
  use new_elements, state <- state.try_op(copy_range_dense(
    state,
    this,
    0,
    0,
    actual_start,
    elements.new(),
  ))
  // Insert items at actualStart
  let new_elements = insert_items(new_elements, actual_start, items)
  // Copy [actualStart + actualSkipCount, length) from source
  let src_from = actual_start + actual_skip_count
  let dst_from = actual_start + item_count
  let remaining = length - src_from
  use new_elements, state <- state.try_op(copy_range_dense(
    state,
    this,
    src_from,
    dst_from,
    remaining,
    new_elements,
  ))
  // Step 15: Return A
  let #(heap, ref) =
    common.alloc_array_from_elements(
      state.heap,
      new_elements,
      new_len,
      array_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Insert a list of items into elements starting at the given index.
fn insert_items(
  elements: JsElements,
  start: Int,
  items: List(JsValue),
) -> JsElements {
  case items {
    [] -> elements
    [item, ..rest] ->
      insert_items(elements.set(elements, start, item), start + 1, rest)
  }
}

// ============================================================================
// Array.prototype.with (ES2024 §23.1.3.39)
// ============================================================================

/// Array.prototype.with ( index, value )
/// ES2024 §23.1.3.39
///
/// Returns a new array with the element at the given index replaced.
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. Let relativeIndex be ? ToIntegerOrInfinity(index).
///   4. If relativeIndex >= 0, let actualIndex be relativeIndex.
///   5. Else, let actualIndex be len + relativeIndex.
///   6. If actualIndex >= len or actualIndex < 0, throw a RangeError.
///   7. Let A be ? ArrayCreate(len).
///   8-11. Copy all elements, replacing actualIndex with value.
///  12. Return A.
fn array_with(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use this, _ref, length, state <- require_array(this, state)
  // Step 3: relativeIndex = ToIntegerOrInfinity(index) — observable coercion
  use raw, state <- try_integer_or_infinity(state, arg_or_undefined(args, 0))
  // Steps 4-5: resolve relative index (without clamping — out of bounds throws)
  let actual_index = case raw < 0 {
    True -> length + raw
    False -> raw
  }
  // Step 6: bounds check — throw RangeError if out of bounds
  // Step 7: ArrayCreate(len) throws RangeError when len > 2^32 - 1, before
  // any element Get (test262: with/length-exceeding-array-length-limit).
  use <- bool.lazy_guard(length > 4_294_967_295, fn() {
    state.range_error(state, "Invalid array length")
  })
  case actual_index < 0 || actual_index >= length {
    True -> state.range_error(state, "Invalid index")
    False -> {
      // Get the replacement value
      let replacement = case args {
        [_, r, ..] -> r
        _ -> JsUndefined
      }
      // Steps 7-11: Copy elements via Get (holes -> undefined, result dense).
      // Step 5b: k = actualIndex takes `value` WITHOUT a Get(O, Pk) — the
      // replaced position must not be read (with/no-get-replaced-index).
      use new_elements, state <- state.try_op(copy_range_dense(
        state,
        this,
        0,
        0,
        actual_index,
        elements.new(),
      ))
      let new_elements = elements.set(new_elements, actual_index, replacement)
      use new_elements, state <- state.try_op(copy_range_dense(
        state,
        this,
        actual_index + 1,
        actual_index + 1,
        length - actual_index - 1,
        new_elements,
      ))
      // Step 12: Return A
      let #(heap, ref) =
        common.alloc_array_from_elements(
          state.heap,
          new_elements,
          length,
          array_proto,
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

// ============================================================================
// Array.prototype.toSorted (ES2024 §23.1.3.34)
// ============================================================================

/// Array.prototype.toSorted ( [ comparefn ] )
/// ES2024 §23.1.3.34
///
/// Returns a NEW sorted array without mutating the original.
///   1. If comparefn is not undefined and IsCallable(comparefn) is false, throw TypeError.
///   2. Let O be ? ToObject(this value).
///   3. Let len be ? LengthOfArrayLike(O).
///   4. Let A be ? ArrayCreate(len).
///   5. Sort a copy of the elements using SortCompare.
///   6. Return A.
fn array_to_sorted(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use comparefn, state <- with_comparefn(args, state)
  use this, _ref, length, state <- require_array(this, state)
  // Pragmatic bound: visits all length indices with no early exit
  // (see iteration_budget_msg).
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  case comparefn {
    None -> to_sorted_impl(state, length, this, sort_values_default)
    Some(cmp) -> {
      use state, defined <- to_sorted_impl(state, length, this)
      merge_sort(state, defined, cmp)
    }
  }
}

/// Shared toSorted body: collect elements, sort the defined values with the
/// given sort step, then build a new array from the result.
fn to_sorted_impl(
  state: State,
  length: Int,
  this: JsValue,
  sort: fn(State, List(JsValue)) ->
    Result(#(List(JsValue), State), #(JsValue, State)),
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // READ-THROUGH-HOLES (§23.1.3.34): holes become trailing undefineds and
  // the result is dense.
  use #(defined, undefs), state <- state.try_op(collect_sort_elements(
    state,
    this,
    length,
    0,
    [],
    0,
    True,
  ))
  use sorted, state <- state.try_op(sort(state, defined))
  let all_values = list.append(sorted, list.repeat(JsUndefined, undefs))
  let new_elements = build_elements_from_list(all_values, 0, elements.new())
  let #(heap, ref) =
    common.alloc_array_from_elements(
      state.heap,
      new_elements,
      length,
      array_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Default sort step for toSorted: stringify, sort lexicographically.
fn sort_values_default(
  state: State,
  defined: List(JsValue),
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  use #(pairs, state) <- result.map(stringify_elements(state, defined, []))
  let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
  #(list.map(sorted, fn(pair) { pair.1 }), state)
}

/// Build a JsElements from a list, writing each value at consecutive indices.
fn build_elements_from_list(
  values: List(JsValue),
  idx: Int,
  acc: JsElements,
) -> JsElements {
  case values {
    [] -> acc
    [val, ..rest] ->
      build_elements_from_list(rest, idx + 1, elements.set(acc, idx, val))
  }
}

// ============================================================================
// Array.prototype.toReversed (ES2024 §23.1.3.33)
// ============================================================================

/// Array.prototype.toReversed ()
/// ES2024 §23.1.3.33
///
/// Returns a NEW reversed array without mutating the original.
///   1. Let O be ? ToObject(this value).
///   2. Let len be ? LengthOfArrayLike(O).
///   3. Let A be ? ArrayCreate(len).
///   4. Let k be 0.
///   5. Repeat, while k < len,
///      a. Let from be ! ToString(𝔽(len - k - 1)).
///      b. Let Pk be ! ToString(𝔽(k)).
///      c. Let fromValue be ? Get(O, from).
///      d. Perform ? CreateDataPropertyOrThrow(A, Pk, fromValue).
///      e. Set k to k + 1.
///   6. Return A.
fn array_to_reversed(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  use this, _ref, length, state <- require_array(this, state)
  // Pragmatic bound: visits all length indices with no early exit
  // (see iteration_budget_msg).
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  // Step 5: k ascends 0..len, reading from = len-k-1 — i.e. the source is
  // read in DESCENDING index order (observable via getters that mutate the
  // array mid-iteration). Holes become undefined (step 5c Get).
  use reversed, state <- state.try_op(
    collect_elements_descending(state, this, length - 1, []),
  )
  let new_elements = build_elements_from_list(reversed, 0, elements.new())
  let #(heap, ref) =
    common.alloc_array_from_elements(
      state.heap,
      new_elements,
      length,
      array_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Collect elements reading indices DESCENDING from `idx` to 0 (toReversed's
/// observable Get order, §23.1.3.33 step 5). Returns the values in read
/// order: [Get(len-1), Get(len-2), ..., Get(0)].
fn collect_elements_descending(
  state: State,
  this: JsValue,
  idx: Int,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case idx < 0 {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      use #(val, state) <- result.try(get_index(state, this, idx))
      collect_elements_descending(state, this, idx - 1, [val, ..acc])
    }
  }
}

/// ES2024 §23.1.3.36 Array.prototype.toString ( )
///
/// 1. Let array be ? ToObject(this value).
/// 2. Let func be ? Get(array, "join").
/// 3. If IsCallable(func) is false, set func to %Object.prototype.toString%.
/// 4. Return ? Call(func, array).
fn array_to_string(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: ToObject(this value) — undefined/null throw.
  case common.to_object(state.heap, state.builtins, this) {
    None -> state.type_error(state, cannot_convert)
    Some(#(heap, ref)) -> {
      let state = State(..state, heap:)
      let array = JsObject(ref)
      // Step 2: func = ? Get(array, "join") — observable (proxy get traps,
      // getters).
      use func, state <- state.try_op(object.get_value(
        state,
        ref,
        Named("join"),
        array,
      ))
      case helpers.is_callable(state.heap, func) {
        // Step 4: Return ? Call(func, array).
        True -> {
          use val, state <- state.try_call(state, func, array, [])
          #(state, Ok(val))
        }
        // Step 3: non-callable join → the %Object.prototype.toString%
        // INTRINSIC (not a lookup — it applies even after
        // `delete Object.prototype.toString`).
        False ->
          object_builtin.dispatch(
            value.ObjectPrototypeToString,
            [],
            array,
            state,
          )
      }
    }
  }
}

/// ES2024 §23.1.3.30 Array.prototype.toLocaleString ( )
/// Calls toLocaleString() on each element and joins with ",".
fn array_to_locale_string(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use this, _ref, length, state <- require_array(this, state)
  // Pragmatic bound: visits all length indices with no early exit
  // (see iteration_budget_msg).
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  to_locale_string_loop(
    state,
    this,
    0,
    length,
    helpers.first_arg_or_undefined(args),
    helpers.list_at(args, 1) |> option.unwrap(JsUndefined),
    [],
  )
}

fn to_locale_string_loop(
  state: State,
  this: JsValue,
  idx: Int,
  length: Int,
  locales_v: JsValue,
  options_v: JsValue,
  acc: List(String),
) -> #(State, Result(JsValue, JsValue)) {
  case idx >= length {
    True -> {
      let result = list.reverse(acc) |> string.join(",")
      #(state, Ok(JsString(result)))
    }
    False -> {
      use elem, state <- state.try_op(get_index(state, this, idx))
      case elem {
        // §23.1.3.32 step 4c: undefined/null elements contribute "".
        JsUndefined | JsNull ->
          to_locale_string_loop(
            state,
            this,
            idx + 1,
            length,
            locales_v,
            options_v,
            ["", ..acc],
          )
        _ -> {
          // §23.1.3.32 step 4c.i: R = ? ToString(? Invoke(element,
          // "toLocaleString")) — the element's own method, NOT plain ToString
          // (test262: invoke-element-tolocalestring.js).
          use method, state <- state.try_op(object.get_value_of(
            state,
            elem,
            Named("toLocaleString"),
          ))
          use <- bool.lazy_guard(!helpers.is_callable(state.heap, method), fn() {
            state.type_error(
              state,
              common.typeof_value(method, state.heap) <> " is not a function",
            )
          })
          // ECMA-402 §18.4.1: Invoke(element, "toLocaleString",
          // « locales, options ») — both are always passed.
          use locale_val, state <- state.try_call(state, method, elem, [
            locales_v,
            options_v,
          ])
          use s, state <- coerce.try_to_string(state, locale_val)
          to_locale_string_loop(
            state,
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

/// ES2024 §23.1.3.16 Array.prototype.keys ( )
/// CreateArrayIterator(O, key) — LAZY: each .next() re-reads the live
/// source, so mid-iteration mutation (push, length change, a resizable
/// buffer resize for typed-array receivers) is observed per spec.
fn array_keys(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _this, ref, length, state <- require_array(this, state)
  // Pragmatic bound: consuming the iterator visits all length indices
  // (see iteration_budget_msg); fail at creation like the other guards.
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  let #(heap, iter_ref) =
    common.alloc_wrapper(
      state.heap,
      value.ArrayIteratorObject(
        source: ref,
        index: 0,
        iter_kind: value.ArrayIterKeys,
      ),
      state.builtins.array_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(iter_ref)))
}

/// ES2024 §23.1.3.37 Array.prototype.values ( )
/// Returns a new Array Iterator object (§23.1.5.1 CreateArrayIterator).
fn array_values(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _this, ref, length, state <- require_array(this, state)
  // Pragmatic bound: consuming the iterator visits all length indices
  // (see iteration_budget_msg); fail at creation like the other guards.
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  let #(heap, iter_ref) =
    common.alloc_wrapper(
      state.heap,
      value.ArrayIteratorObject(
        source: ref,
        index: 0,
        iter_kind: value.ArrayIterValues,
      ),
      state.builtins.array_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(iter_ref)))
}

/// ES2024 §23.1.3.4 Array.prototype.entries ( )
/// CreateArrayIterator(O, key+value) — LAZY, like array_keys: each .next()
/// re-reads the live source and allocates a fresh [index, value] pair.
fn array_entries(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _this, ref, length, state <- require_array(this, state)
  // Pragmatic bound: consuming the iterator visits all length indices
  // (see iteration_budget_msg); fail at creation like the other guards.
  use <- bool.lazy_guard(length > limits.max_iteration, fn() {
    state.range_error(state, iteration_budget_msg)
  })
  let #(heap, iter_ref) =
    common.alloc_wrapper(
      state.heap,
      value.ArrayIteratorObject(
        source: ref,
        index: 0,
        iter_kind: value.ArrayIterEntries,
      ),
      state.builtins.array_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(iter_ref)))
}
