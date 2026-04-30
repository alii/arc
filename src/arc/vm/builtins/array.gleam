import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type ArrayNativeFn, type JsElements, type JsValue, type Property, type Ref,
  ArrayConstructor, ArrayFrom, ArrayIsArray, ArrayNative, ArrayObject, ArrayOf,
  ArrayPrototypeAt, ArrayPrototypeConcat, ArrayPrototypeCopyWithin,
  ArrayPrototypeEvery, ArrayPrototypeFill, ArrayPrototypeFilter,
  ArrayPrototypeFind, ArrayPrototypeFindIndex, ArrayPrototypeFindLast,
  ArrayPrototypeFindLastIndex, ArrayPrototypeFlat, ArrayPrototypeFlatMap,
  ArrayPrototypeForEach, ArrayPrototypeIncludes, ArrayPrototypeIndexOf,
  ArrayPrototypeJoin, ArrayPrototypeLastIndexOf, ArrayPrototypeMap,
  ArrayPrototypePop, ArrayPrototypePush, ArrayPrototypeReduce,
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

  // §23.1.3.37 Array.prototype [ @@iterator ] ( )
  // "The initial value of the @@iterator property is %Array.prototype.values%"
  let #(h, values_fn_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      ArrayNative(value.ArrayPrototypeValues),
      "values",
      0,
    )
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_iterator,
      value.builtin_property(JsObject(values_fn_ref)),
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
    value.ArrayPrototypeToLocaleString -> array_to_locale_string(this, state)
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
/// Wrapper that threads State around native_is_array.
/// §23.1.2.1 Array.isArray ( arg )
fn is_array(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  #(State(..state, heap:), Ok(native_is_array(args, heap)))
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
    heap.alloc(
      heap,
      ObjectSlot(
        kind: ArrayObject(length),
        properties: dict.new(),
        elements:,
        prototype: Some(array_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(heap, Ok(JsObject(ref)))
}

/// Array.isArray ( arg ) — §23.1.2.1
/// Delegates to the abstract operation IsArray (§7.2.2):
///   1. If arg is not an Object, return false.
///   2. If arg is an Array exotic object, return true.
///   3. If arg is a Proxy exotic object, follow [[ProxyHandler]].
///   4. Return false.
///
/// TODO(Deviation): Step 3 (Proxy exotic object) — needs Proxy implementation.
fn native_is_array(args: List(JsValue), heap: Heap) -> JsValue {
  case args {
    // Step 1: If arg is not an Object, return false.
    // (non-object JsValues fall through to the _ branch → false)
    [JsObject(ref), ..] ->
      case heap.read(heap, ref) {
        // Step 2: If arg is an Array exotic object, return true.
        Some(ObjectSlot(kind: ArrayObject(_), ..)) -> JsBool(True)
        // Step 4: Return false.
        _ -> JsBool(False)
      }
    // Step 1: arg is not an Object → false.
    _ -> JsBool(False)
  }
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
  use _ref, length, state <- require_array(this, state)
  // Steps 3-4: If separator is undefined, let sep be ",".
  //            Else, let sep be ? ToString(separator).
  let sep_val = case args {
    [JsUndefined, ..] | [] -> JsString(",")
    [v, ..] -> v
  }
  use separator, state <- coerce.try_to_string(state, sep_val)
  // Guard: O(length) string materialization — cap at max_iteration.
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
/// Per-index reads via get_index (which uses object.get_value_of — walks
/// prototype chain and invokes getters).
fn join_elements(
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
          join_elements(state, this, idx + 1, length, separator, ["", ..acc])
        // Step 7c (cont.): Otherwise, let next be ? ToString(element).
        _ -> {
          use str, state <- coerce.try_to_string(state, val)
          // Step 7d: Set R to string-concatenation of R and next.
          // Step 7e: Set k to k + 1.
          join_elements(state, this, idx + 1, length, separator, [str, ..acc])
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
  use ref, length, state <- require_array(this, state)
  // Step 3: Let argCount be the number of elements in items.
  // Step 4: If len + argCount > 2^53 - 1, throw a TypeError.
  // NOTE: Step 4 (overflow check) is not implemented — we don't guard
  // against len + argCount exceeding MAX_SAFE_INTEGER.
  // Steps 5-7 delegated to push_generic.
  use new_length, state <- state.try_op(push_generic(state, ref, length, args))
  // Step 7: Return 𝔽(len).
  #(state, Ok(value.from_int(new_length)))
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
/// reads during iteration go through `get_index`/`has_index` (HasProperty+Get
/// via object.get_value_of) — no eager snapshot.
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
  cont: fn(Ref, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  // Bound length at the single choke point so every array method is guarded
  // against pathological sparse arrays. `Array(2**31).forEach(f)` would
  // otherwise recurse 2**31 times through iterate_loop. QuickJS doesn't guard
  // this (lets it spin), but we do since BEAM recursion is heavier than a C
  // for-loop. Trade-off: push/pop on 10M+ sparse arrays throw RangeError
  // instead of working — acceptable for now, revisit with sparse iteration.
  let cont = fn(ref, length, state) {
    case length > limits.max_iteration {
      True -> state.range_error(state, "Invalid array length")
      False -> cont(ref, length, state)
    }
  }
  case this {
    // §7.1.18: Object → return argument unchanged.
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        // Real array — length from [[ArrayLength]].
        Some(ObjectSlot(kind: ArrayObject(length:), ..)) ->
          cont(ref, length, state)
        // Arguments exotic object (§10.4.4): length stored in kind.
        Some(ObjectSlot(kind: value.ArgumentsObject(length:), ..)) ->
          cont(ref, length, state)
        // §7.1.18 String row / §10.4.3: String exotic object.
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) ->
          cont(ref, string.length(s), state)
        // Generic object: LengthOfArrayLike (§7.3.18) — Get(obj, "length"),
        // then ToLength (§7.1.17).
        Some(ObjectSlot(properties:, ..)) ->
          cont(ref, to_length_from_properties(state, ref, properties), state)
        // Non-object heap slot under a ref shouldn't happen, but fall through.
        _ -> cont(ref, 0, state)
      }
    // §7.1.18: Undefined / Null → throw a TypeError exception.
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    // §7.1.18 String row: ToObject creates a String exotic object (§10.4.3).
    // We don't actually allocate the wrapper — iteration uses get_value_of
    // which handles string primitives via StringGetOwnProperty semantics.
    // Mutating methods will fail via generic_set on heap.sentinel_ref (no-op), which
    // is correct since string indices are non-writable (§10.4.3.5 step 11).
    JsString(s) -> cont(heap.sentinel_ref, string.length(s), state)
    // §7.1.18 Boolean/Number/Symbol/BigInt rows: ToObject creates a wrapper
    // object with no own `.length` property → LengthOfArrayLike returns 0.
    _ -> cont(heap.sentinel_ref, 0, state)
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
  object.get_value_of(state, this, Index(idx))
}

/// HasProperty (ES2024 §7.3.11) on an array-like by integer index.
///
/// For objects: O.[[HasProperty]](! ToString(𝔽(idx))) — walks prototype chain.
/// For string primitives: true iff 0 ≤ idx < length (per §10.4.3.5 — string
/// indices are always-present own data properties within range).
/// For other primitives: false (wrapper object has no indexed own properties).
fn has_index(heap: Heap, this: JsValue, idx: Int) -> Bool {
  case this {
    JsObject(ref) -> object.has_property(heap, ref, Index(idx))
    JsString(s) -> idx >= 0 && idx < string.length(s)
    _ -> False
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
fn to_length_from_properties(
  state: State,
  ref: value.Ref,
  properties: dict.Dict(value.PropertyKey, Property),
) -> Int {
  // Fast path: own data property
  case dict.get(properties, Named("length")) {
    Ok(DataProperty(value: len_val, ..)) ->
      to_length(coerce.unwrap_primitive_wrapper(state.heap, len_val))
    // Accessor or missing: use full [[Get]] which handles getters + prototype chain
    _ ->
      case object.get_value(state, ref, Named("length"), JsObject(ref)) {
        Ok(#(len_val, _state)) ->
          to_length(coerce.unwrap_primitive_wrapper(state.heap, len_val))
        Error(_) -> 0
      }
  }
}

/// ES2024 §7.1.17 ToLength(argument)
fn to_length(val: JsValue) -> Int {
  case helpers.to_number_int(val) {
    Some(n) if n > 0 -> int.min(n, limits.max_safe_integer)
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
fn resolve_index(arg: JsValue, len: Int, default: Int) -> Int {
  // §7.1.5 ToIntegerOrInfinity(arg)
  let raw = case helpers.to_number_int(arg) {
    Some(n) -> n
    // §7.1.5 step 2: NaN → 0. But undefined args use the caller-specified default
    // (e.g. slice(start) with no end → end defaults to len, not 0).
    None ->
      case arg {
        JsUndefined -> default
        _ -> 0
      }
  }
  // Relative index clamping (common pattern across §23.1.3.*):
  //   If relativeIndex < 0, let k = max(len + relativeIndex, 0).
  //   Else, let k = min(relativeIndex, len).
  case raw < 0 {
    True -> int.max(len + raw, 0)
    False -> int.min(raw, len)
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
fn generic_set(
  state: State,
  ref: Ref,
  key: String,
  val: JsValue,
) -> Result(State, #(JsValue, State)) {
  // §7.3.4 step 1: Let success be ? O.[[Set]](P, V, O).
  {
    use #(state, success) <- result.try(object.set_value(
      state,
      ref,
      value.canonical_key(key),
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
          "Cannot assign to read only property '" <> key <> "' of object",
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
/// We stringify the index and delegate to generic_set (§7.3.4).
fn generic_set_index(
  state: State,
  ref: Ref,
  idx: Int,
  val: JsValue,
) -> Result(State, #(JsValue, State)) {
  generic_set(state, ref, int.to_string(idx), val)
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
  generic_set(state, ref, "length", value.from_int(len))
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
  key: String,
) -> Result(State, #(JsValue, State)) {
  // §7.3.9 step 1: Let success be ? O.[[Delete]](P).
  let #(h, ok) =
    object.delete_property(state.heap, ref, value.canonical_key(key))
  let state = State(..state, heap: h)
  case ok {
    // success = true → return normally.
    True -> Ok(state)
    // §7.3.9 step 2: success = false → throw TypeError.
    False ->
      coerce.thrown_type_error(
        state,
        "Cannot delete property '" <> key <> "' of object",
      )
  }
}

/// HasProperty (ES2024 §7.3.11).
///
/// §7.3.11 HasProperty ( O, P ):
///   1. Return ? O.[[HasProperty]](P).
///
/// [[HasProperty]] (§10.1.7 OrdinaryHasProperty):
///   1. Let hasOwn be ? O.[[GetOwnProperty]](P).
///   2. If hasOwn is not undefined, return true.
///   3. Let parent be ? O.[[GetPrototypeOf]]().
///   4. If parent is not null, return ? parent.[[HasProperty]](P).
///   5. Return false.
///
/// Used by iteration methods to distinguish holes from present-but-undefined
/// elements, e.g. §23.1.3.13 Array.prototype.forEach step 6.c:
///   "Let kPresent be ? HasProperty(O, Pk)."
fn generic_has(heap: Heap, ref: Ref, idx: Int) -> Bool {
  // §7.3.11 step 1: O.[[HasProperty]](! ToString(𝔽(idx)))
  object.has_property(heap, ref, Index(idx))
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
  object.get_value(state, ref, Index(idx), JsObject(ref))
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
  use ref, length, state <- require_array(this, state)
  case length == 0 {
    // Step 3: len = 0
    // Step 3a: Set(O, "length", +0𝔽, true)
    // Step 3b: Return undefined
    True -> wrap(generic_set_length(state, ref, 0), JsUndefined)
    // Step 4: len > 0
    False -> {
      // Step 4b: newLen = len - 1
      let new_len = length - 1
      // Step 4d: element = Get(O, ToString(newLen))
      use val, state <- state.try_op(generic_get(state, ref, new_len))
      // Step 4e: DeletePropertyOrThrow(O, index)
      use state <- state.try_state(generic_delete(
        state,
        ref,
        int.to_string(new_len),
      ))
      // Step 4f: Set(O, "length", newLen, true)
      // Step 4g: Return element
      wrap(generic_set_length(state, ref, new_len), val)
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
  use ref, length, state <- require_array(this, state)
  case length == 0 {
    // Step 3: len = 0
    // Step 3a: Set(O, "length", +0𝔽, true)
    // Step 3b: Return undefined
    True -> wrap(generic_set_length(state, ref, 0), JsUndefined)
    False -> {
      // Step 4: first = Get(O, "0")
      use val, state <- state.try_op(generic_get(state, ref, 0))
      // Steps 5-6: shift indices [1..len) down by 1 (see shift_left_generic)
      use state <- state.try_state(shift_left_generic(state, ref, 1, length))
      // Step 7: DeletePropertyOrThrow(O, ToString(len - 1))
      use state <- state.try_state(generic_delete(
        state,
        ref,
        int.to_string(length - 1),
      ))
      // Step 8: Set(O, "length", len - 1, true)
      // Step 9: Return first
      wrap(generic_set_length(state, ref, length - 1), val)
    }
  }
}

/// Implements the shift-down loop from Array.prototype.shift (§23.1.3.25 steps 5-6).
///
/// Corresponds to the spec's:
///   5. Let k be 1.
///   6. Repeat, while k < len,
///      a. Let from be ! ToString(𝔽(k)).
///      b. Let to be ! ToString(𝔽(k - 1)).
///      c. Let fromPresent be ? HasProperty(O, from).
///      d. If fromPresent is true, then
///         i. Let fromVal be ? Get(O, from).
///         ii. Perform ? Set(O, to, fromVal, true).
///      e. Else,
///         i. Perform ? DeletePropertyOrThrow(O, to).
///      f. Set k to k + 1.
///
/// Parameters: k is the current "from" index, len is the array length.
/// Iterates left-to-right [k..len), moving each element down by 1.
fn shift_left_generic(
  state: State,
  ref: Ref,
  k: Int,
  len: Int,
) -> Result(State, #(JsValue, State)) {
  // Step 6 loop condition: k < len
  case k >= len {
    True -> Ok(state)
    False -> {
      // Step 6b: to = ToString(k - 1)
      let to = int.to_string(k - 1)
      // Step 6c: fromPresent = HasProperty(O, from)
      case generic_has(state.heap, ref, k) {
        True -> {
          // Step 6d.i: fromVal = Get(O, from)
          use #(val, state) <- result.try(generic_get(state, ref, k))
          // Step 6d.ii: Set(O, to, fromVal, true)
          use state <- result.try(generic_set(state, ref, to, val))
          // Step 6f: k = k + 1
          shift_left_generic(state, ref, k + 1, len)
        }
        False -> {
          // Step 6e.i: DeletePropertyOrThrow(O, to)
          use state <- result.try(generic_delete(state, ref, to))
          // Step 6f: k = k + 1
          shift_left_generic(state, ref, k + 1, len)
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
  use ref, length, state <- require_array(this, state)
  let arg_count = list.length(args)
  use <- bool.guard(arg_count == 0, #(state, Ok(value.from_int(length))))
  let new_len = length + arg_count
  // §23.1.3.33 step 4a: If len + argCount > 2^53 - 1, throw TypeError
  use <- state.guard_safe_length(state, new_len)
  use state <- state.try_state(shift_right_generic(
    state,
    ref,
    length - 1,
    arg_count,
  ))
  use state <- state.try_state(write_list_at(state, ref, 0, args))
  wrap(generic_set_length(state, ref, new_len), value.from_int(new_len))
}

/// Implements the shift-up loop from Array.prototype.unshift (§23.1.3.33 steps 4b-4c).
///
/// Corresponds to the spec's:
///   4b. Let k be len.
///   4c. Repeat, while k > 0,
///       i. Let from be ! ToString(𝔽(k - 1)).
///       ii. Let to be ! ToString(𝔽(k + argCount - 1)).
///       iii. Let fromPresent be ? HasProperty(O, from).
///       iv. If fromPresent is true, then
///           1. Let fromValue be ? Get(O, from).
///           2. Perform ? Set(O, to, fromValue, true).
///       v. Else,
///           1. Perform ? DeletePropertyOrThrow(O, to).
///       vi. Set k to k - 1.
///
/// Parameters: k is (len - 1) on first call (the spec uses k starting at len
/// and references k-1 as "from" — we pre-subtract, so our k directly equals
/// the "from" index). delta is argCount.
/// Iterates right-to-left [k..0], moving each element up by delta.
fn shift_right_generic(
  state: State,
  ref: Ref,
  k: Int,
  delta: Int,
) -> Result(State, #(JsValue, State)) {
  // Step 4c loop condition: k > 0 (our k = spec's k-1, so we check k < 0)
  case k < 0 {
    True -> Ok(state)
    False -> {
      // Step 4c.ii: to = ToString(k + argCount)
      // (spec says k + argCount - 1, but our k = spec's k - 1, so k + delta)
      let to = int.to_string(k + delta)
      // Step 4c.iii: fromPresent = HasProperty(O, from)
      case generic_has(state.heap, ref, k) {
        True -> {
          // Step 4c.iv.1: fromValue = Get(O, from)
          use #(val, state) <- result.try(generic_get(state, ref, k))
          // Step 4c.iv.2: Set(O, to, fromValue, true)
          use state <- result.try(generic_set(state, ref, to, val))
          // Step 4c.vi: k = k - 1
          shift_right_generic(state, ref, k - 1, delta)
        }
        False -> {
          // Step 4c.v.1: DeletePropertyOrThrow(O, to)
          use state <- result.try(generic_delete(state, ref, to))
          // Step 4c.vi: k = k - 1
          shift_right_generic(state, ref, k - 1, delta)
        }
      }
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
///   - Steps 14b-14c: copy_range uses has_index for HasProperty on the
///     source, preserving holes (sparse indices are not copied).
///   - Step 15: length is set via ArrayObject(count) in the slot constructor
///     rather than a separate Set("length") call.
fn array_slice(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O).
  use _ref, length, state <- require_array(this, state)
  // Steps 3-6: relativeStart → k (clamped). Default 0 if no arg.
  let start = case args {
    [s, ..] -> resolve_index(s, length, 0)
    [] -> 0
  }
  // Steps 7-10: relativeEnd → final (clamped). Default len if no end arg.
  let end = case args {
    [_, e, ..] -> resolve_index(e, length, length)
    _ -> length
  }
  // Step 11: count = max(final - k, 0).
  let count = int.max(end - start, 0)
  // Steps 12-14: Create result array A, copy elements [k..final) into it.
  // Holes (kPresent = false) are preserved by copy_range skipping them.
  use copied, state <- state.try_op(copy_range(
    state,
    this,
    start,
    0,
    count,
    elements.new(),
  ))
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(count),
        properties: dict.new(),
        elements: copied,
        prototype: Some(array_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  // Step 16: Return A.
  #(State(..state, heap:), Ok(JsObject(ref)))
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
fn copy_range(
  state: State,
  src: JsValue,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
) -> Result(#(JsElements, State), #(JsValue, State)) {
  case remaining <= 0 {
    True -> Ok(#(dst, state))
    False ->
      // Step 14b: kPresent = HasProperty(O, Pk).
      case has_index(state.heap, src, src_idx) {
        // Step 14c: kPresent is true — copy the element.
        True -> {
          use #(val, state) <- result.try(get_index(state, src, src_idx))
          copy_range(
            state,
            src,
            src_idx + 1,
            dst_idx + 1,
            remaining - 1,
            elements.set(dst, dst_idx, val),
          )
        }
        // kPresent is false (hole): skip — do not set dst[dst_idx].
        False ->
          copy_range(state, src, src_idx + 1, dst_idx + 1, remaining - 1, dst)
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
  // Step 1: Let O be ? ToObject(this value).
  case this {
    JsNull | JsUndefined -> state.type_error(state, cannot_convert)
    _ -> {
      // Step 4: Prepend O to items.
      let all_items = [this, ..args]
      // Steps 3, 5: n = 0, then iterate each element E of items.
      use #(elems, total), state <- state.try_op(concat_items(
        state,
        all_items,
        elements.new(),
        0,
      ))
      // Steps 2, 6-7: Create result array A with final length n, return A.
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(total),
            properties: dict.new(),
            elements: elems,
            prototype: Some(array_proto),
            symbol_properties: [],
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
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
/// IsConcatSpreadable (§7.2.18) full algorithm:
///   1. If E is not an Object, return false.
///   2. Let spreadable be ? Get(E, @@isConcatSpreadable).
///   3. If spreadable is not undefined, return ToBoolean(spreadable).
///   4. Return ? IsArray(E).
///
/// Simplification: we skip step 2 (@@isConcatSpreadable symbol lookup) and
/// go straight to step 4 — only ArrayObject kinds are treated as spreadable.
/// This means objects with @@isConcatSpreadable=true won't be spread, and
/// arrays with @@isConcatSpreadable=false will still be spread.
fn concat_item(
  state: State,
  elems: JsElements,
  pos: Int,
  item: JsValue,
) -> Result(#(#(JsElements, Int), State), #(JsValue, State)) {
  case item {
    JsObject(ref) ->
      case heap.read_array(state.heap, ref) {
        // Step 5b: spreadable = true (IsArray(E) = true) — spread elements.
        // Step 5b.i: len = LengthOfArrayLike(E).
        // Step 5b.iv: copy elements [0..len) into result at position n.
        Some(#(length, _src)) -> {
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
        // Step 5c: spreadable = false (non-array object) — append as single item.
        None -> Ok(#(#(elements.set(elems, pos, item), pos + 1), state))
      }
    // Step 5c: E is not an object (primitive) — append as single item.
    _ -> Ok(#(#(elements.set(elems, pos, item), pos + 1), state))
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
  use ref, length, state <- require_array(this, state)
  // Steps 3-5: middle = floor(len/2), lower = 0, loop while lower != middle
  // Step 6: Return O
  wrap(reverse_generic(state, ref, 0, length - 1), this)
}

/// Implements §23.1.3.24 step 5's loop body. lo = lower, hi = upper (len-lower-1).
/// The spec iterates lower from 0 to middle; here we converge lo/hi toward
/// each other which is equivalent.
fn reverse_generic(
  state: State,
  ref: Ref,
  lo: Int,
  hi: Int,
) -> Result(State, #(JsValue, State)) {
  // Step 5: Repeat, while lower != middle (lo < hi is equivalent)
  case lo >= hi {
    True -> Ok(state)
    False -> {
      // Step 5d: Let lowerExists be ? HasProperty(O, lowerP)
      let has_lo = generic_has(state.heap, ref, lo)
      // Step 5f: Let upperExists be ? HasProperty(O, upperP)
      let has_hi = generic_has(state.heap, ref, hi)
      // Steps 5b-c: lowerP = ToString(lower), upperP = ToString(upper)
      let lo_key = int.to_string(lo)
      let hi_key = int.to_string(hi)
      case has_lo, has_hi {
        // Step 5h: lowerExists AND upperExists — swap
        True, True -> {
          // Step 5e: lowerValue = Get(O, lowerP)
          use #(lo_val, state) <- result.try(generic_get(state, ref, lo))
          // Step 5g: upperValue = Get(O, upperP)
          use #(hi_val, state) <- result.try(generic_get(state, ref, hi))
          // Step 5h.i: Set(O, lowerP, upperValue, true)
          use state <- result.try(generic_set(state, ref, lo_key, hi_val))
          // Step 5h.ii: Set(O, upperP, lowerValue, true)
          use state <- result.try(generic_set(state, ref, hi_key, lo_val))
          // Step 5l: lower = lower + 1
          reverse_generic(state, ref, lo + 1, hi - 1)
        }
        // Step 5i: NOT lowerExists AND upperExists — move upper to lower, delete upper
        False, True -> {
          // Step 5g: upperValue = Get(O, upperP)
          use #(hi_val, state) <- result.try(generic_get(state, ref, hi))
          // Step 5i.i: Set(O, lowerP, upperValue, true)
          use state <- result.try(generic_set(state, ref, lo_key, hi_val))
          // Step 5i.ii: DeletePropertyOrThrow(O, upperP)
          use state <- result.try(generic_delete(state, ref, hi_key))
          reverse_generic(state, ref, lo + 1, hi - 1)
        }
        // Step 5j: lowerExists AND NOT upperExists — delete lower, move lower to upper
        True, False -> {
          // Step 5e: lowerValue = Get(O, lowerP)
          use #(lo_val, state) <- result.try(generic_get(state, ref, lo))
          // Step 5j.i: DeletePropertyOrThrow(O, lowerP)
          use state <- result.try(generic_delete(state, ref, lo_key))
          // Step 5j.ii: Set(O, upperP, lowerValue, true)
          use state <- result.try(generic_set(state, ref, hi_key, lo_val))
          reverse_generic(state, ref, lo + 1, hi - 1)
        }
        // Step 5k: Neither exists — no action
        False, False -> reverse_generic(state, ref, lo + 1, hi - 1)
      }
    }
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
  use ref, length, state <- require_array(this, state)
  // Step 12 uses value; if not provided, defaults to undefined
  let fill_val = helpers.first_arg_or_undefined(args)
  // Steps 3-6: relativeStart → k (clamped index)
  // resolve_index handles ToIntegerOrInfinity + clamping; default 0 when absent
  let start = case args {
    [_, s, ..] -> resolve_index(s, length, 0)
    _ -> 0
  }
  // Steps 7-11: relativeEnd → final (clamped index)
  // resolve_index handles ToIntegerOrInfinity + clamping; default len when absent
  // (step 7: if end is undefined, relativeEnd = len)
  let end = case args {
    [_, _, e, ..] -> resolve_index(e, length, length)
    _ -> length
  }
  // Guard: O(end-start) writes — cap at max_iteration.
  // Steps 12-13: fill loop, then return O
  wrap(fill_generic(state, ref, start, end, fill_val), this)
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
  use _ref, length, state <- require_array(this, state)
  // Step 3: relativeIndex = ToIntegerOrInfinity(index)
  let raw = case args {
    [v, ..] -> helpers.to_number_int(v) |> option.unwrap(0)
    [] -> 0
  }
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
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _ref, length, state <- require_array(this, state)
  // Step 3: If len = 0, return -1
  use <- bool.guard(length == 0, #(state, Ok(value.from_int(-1))))
  let search = helpers.first_arg_or_undefined(args)
  // Steps 4-6: n = ToIntegerOrInfinity(fromIndex), handle ±Infinity
  case args {
    // Step 5: If n = +∞, return -1 (no index >= +∞)
    [_, JsNumber(value.Infinity), ..] -> #(state, Ok(value.from_int(-1)))
    _ -> {
      let from = case args {
        // Step 6: If n = -∞, set n to 0
        [_, JsNumber(value.NegInfinity), ..] -> 0
        [_, f, ..] -> helpers.to_number_int(f) |> option.unwrap(0)
        _ -> 0
      }
      // Steps 7-8: resolve start index (negative → max(len + n, 0))
      let start = case from < 0 {
        True -> int.max(length + from, 0)
        False -> from
      }
      // Steps 9-10: forward search with IsStrictlyEqual, skipping holes
      use found, state <- state.try_op(search_forward(
        state,
        this,
        start,
        length,
        search,
        value.strict_equal,
        True,
      ))
      #(state, Ok(value.from_int(found)))
    }
  }
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
  use _ref, length, state <- require_array(this, state)
  // Step 3: If len = 0, return -1
  use <- bool.guard(length == 0, #(state, Ok(value.from_int(-1))))
  let search = helpers.first_arg_or_undefined(args)
  // Steps 4-6: fromIndex present → ToIntegerOrInfinity; absent → len - 1
  // Checked by arg COUNT, not value (see MEMORY.md lastIndexOf gotcha).
  case args {
    // Step 6: If n = -∞, return -1
    [_, JsNumber(value.NegInfinity), ..] -> #(state, Ok(value.from_int(-1)))
    _ -> {
      let from = case args {
        // +∞ → clamp to len - 1 via step 7
        [_, JsNumber(value.Infinity), ..] -> length - 1
        [_, f, ..] -> helpers.to_number_int(f) |> option.unwrap(0)
        _ -> length - 1
      }
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
      ))
      #(state, Ok(value.from_int(found)))
    }
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
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _ref, length, state <- require_array(this, state)
  // Step 3: If len = 0, return false
  use <- bool.guard(length == 0, #(state, Ok(JsBool(False))))
  let search = helpers.first_arg_or_undefined(args)
  // Steps 4-6: n = ToIntegerOrInfinity(fromIndex), handle ±Infinity
  case args {
    // Step 5: If n = +∞, return false
    [_, JsNumber(value.Infinity), ..] -> #(state, Ok(JsBool(False)))
    _ -> {
      let from = case args {
        // Step 6: If n = -∞, set n to 0
        [_, JsNumber(value.NegInfinity), ..] -> 0
        [_, f, ..] -> helpers.to_number_int(f) |> option.unwrap(0)
        _ -> 0
      }
      // Steps 7-8: resolve start index (negative → max(len + n, 0))
      let start = case from < 0 {
        True -> int.max(length + from, 0)
        False -> from
      }
      // Steps 9-10: forward search with SameValueZero, visiting holes
      use found, state <- state.try_op(search_forward(
        state,
        this,
        start,
        length,
        search,
        value.same_value_zero,
        False,
      ))
      #(state, Ok(JsBool(found >= 0)))
    }
  }
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
) -> Result(#(Int, State), #(JsValue, State)) {
  // Loop condition: k < len (both specs)
  case idx >= length {
    // Step 10: return -1 (indexOf) / return false (includes; caller converts)
    True -> Ok(#(-1, state))
    False ->
      // indexOf step 9a: kPresent = HasProperty(O, k) — skip if absent
      case skip_holes && !has_index(state.heap, this, idx) {
        True ->
          // indexOf step 9c: k = k + 1 (hole skipped)
          search_forward(state, this, idx + 1, length, search, eq, skip_holes)
        False -> {
          // indexOf step 9b.i-ii / includes step 9a-b: Get + compare
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
) -> Result(#(Int, State), #(JsValue, State)) {
  // Loop condition: k >= 0
  case idx < 0 {
    // Step 10: return -1
    True -> Ok(#(-1, state))
    False ->
      // Step 9a: kPresent = HasProperty(O, k) — skip holes
      case has_index(state.heap, this, idx) {
        False ->
          // Step 9c: k = k - 1 (hole skipped)
          search_backward(state, this, idx - 1, search)
        True -> {
          // Step 9b.i-ii: elementK = Get(O, k), IsStrictlyEqual check
          use #(val, state) <- result.try(get_index(state, this, idx))
          case value.strict_equal(val, search) {
            True -> Ok(#(idx, state))
            False ->
              // Step 9c: k = k - 1
              search_backward(state, this, idx - 1, search)
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
///     implements the HasProperty check (step 5b) via has_index and
///     calls the callback with (kValue, k, O) arguments (step 5c.ii).
///   - Step 6: return undefined.
fn array_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: O = ToObject(this), len = LengthOfArrayLike(O)
  use _ref, length, state <- require_array(this, state)
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
  use _ref, length, state <- require_array(this, state)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 5-6: iterate, collecting mappedValue at each present index.
  // Step 6c.iii: CreateDataPropertyOrThrow(A, Pk, mappedValue) — done via
  // elements.set on the accumulator (preserves holes: absent source indices
  // are never written to the output).
  fold_array(
    state,
    this,
    length,
    cb,
    this_arg,
    elements.new(),
    fn(_state, acc, result, _elem, idx) { elements.set(acc, idx, result) },
  )
  // Steps 4+7: ArraySpeciesCreate(O, len), return A
  |> finish_array(length)
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
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(length),
            properties: dict.new(),
            elements:,
            prototype: Some(array_proto),
            symbol_properties: [],
            extensible: True,
          ),
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
  use _ref, length, state <- require_array(this, state)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 5-7: iterate, keeping kValue when selected is true.
  // Step 7c.ii: selected = ToBoolean(result) — done via value.is_truthy.
  // Step 7c.iii: If selected, prepend kValue (the "to" index is implicit via
  // list prepend; reversed on return so final index equals insertion order).
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
  // Steps 4+8: ArraySpeciesCreate(O, 0), return A (allocate from kept values)
  |> finish_list
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
  // Steps 1-2: ToObject + LengthOfArrayLike (via require_array).
  use _ref, length, state <- require_array(this, state)
  // Step 3: IsCallable check (via require_callback).
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-5: k = 0, repeat while k < len.
  // iterate_array handles the loop. SkipHoles implements step 5b (HasProperty).
  // stop_on = !is_truthy implements step 5c.iii (stop when testResult is false).
  use _elem, idx, state <- iterate_array(
    state,
    this,
    length,
    cb,
    this_arg,
    SkipHoles,
    fn(r) { !value.is_truthy(r) },
  )
  // Step 5c.iii / Step 6: If stopped early (idx < length), a falsy was found → false.
  // If loop completed (idx >= length), all passed → true.
  #(state, Ok(JsBool(idx >= length)))
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
  // Steps 1-2: ToObject + LengthOfArrayLike (via require_array).
  use _ref, length, state <- require_array(this, state)
  // Step 3: IsCallable check (via require_callback).
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-5: k = 0, repeat while k < len.
  // SkipHoles implements step 5b (HasProperty check).
  // stop_on = is_truthy implements step 5c.iii (stop when testResult is true).
  use _elem, idx, state <- iterate_array(
    state,
    this,
    length,
    cb,
    this_arg,
    SkipHoles,
    value.is_truthy,
  )
  // Step 5c.iii / Step 6: If stopped early (idx < length), a truthy was found → true.
  // If loop completed (idx >= length), none passed → false.
  #(state, Ok(JsBool(idx < length)))
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
  use _ref, length, state <- require_array(this, state)
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
  // Step 1: If comparefn is not undefined and not callable, throw TypeError.
  let comparefn = helpers.first_arg_or_undefined(args)
  case comparefn {
    JsUndefined -> {
      // No comparefn — sort by string conversion (default sort).
      use ref, length, state <- require_array(this, state)
      sort_default(state, ref, length, this)
    }
    _ ->
      case helpers.is_callable(state.heap, comparefn) {
        True -> {
          // comparefn is callable — sort using it for comparisons.
          use ref, length, state <- require_array(this, state)
          sort_with_comparefn(state, ref, length, comparefn, this)
        }
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
fn collect_sort_elements(
  state: State,
  this: JsValue,
  length: Int,
  idx: Int,
  acc: List(JsValue),
  undefs: Int,
) -> Result(#(#(List(JsValue), Int), State), #(JsValue, State)) {
  case idx >= length {
    True -> Ok(#(#(list.reverse(acc), undefs), state))
    False ->
      case has_index(state.heap, this, idx) {
        False ->
          // Hole — skip entirely.
          collect_sort_elements(state, this, length, idx + 1, acc, undefs)
        True -> {
          use #(val, state) <- result.try(get_index(state, this, idx))
          case val {
            JsUndefined ->
              // Undefined — count but don't include in sort.
              collect_sort_elements(
                state,
                this,
                length,
                idx + 1,
                acc,
                undefs + 1,
              )
            _ ->
              collect_sort_elements(
                state,
                this,
                length,
                idx + 1,
                [val, ..acc],
                undefs,
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
fn write_sort_result(
  state: State,
  ref: Ref,
  values: List(JsValue),
  length: Int,
  idx: Int,
) -> Result(State, #(JsValue, State)) {
  case values {
    [val, ..rest] -> {
      // Step 7a: Set(obj, ToString(j), items[j], true).
      use state <- result.try(generic_set(state, ref, int.to_string(idx), val))
      write_sort_result(state, ref, rest, length, idx + 1)
    }
    [] ->
      // Step 8: Delete remaining indices (holes at the end).
      delete_trailing(state, ref, idx, length)
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
      use state <- result.try(generic_delete(state, ref, int.to_string(idx)))
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
///   - SkipHoles → Pattern A (HasProperty check via has_index)
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
  iterate_loop(state, arr, 0, length, 1, cb, this_arg, hole_mode, stop_on, cont)
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
fn iterate_loop(
  state: State,
  arr: JsValue,
  idx: Int,
  end: Int,
  step: Int,
  cb: JsValue,
  this_arg: JsValue,
  hole_mode: HoleMode,
  stop_on: fn(JsValue) -> Bool,
  cont: fn(JsValue, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  // Loop termination: k == end → completed without early exit.
  // Pattern A step 6 / Pattern B step 5.
  case idx == end {
    True -> cont(JsUndefined, end, state)
    False -> {
      // Pattern A step 5b: kPresent = HasProperty(O, Pk).
      // Pattern B: no HasProperty check, always visits.
      let should_visit = case hole_mode {
        VisitHoles -> True
        SkipHoles -> has_index(state.heap, arr, idx)
      }
      case should_visit {
        // Pattern A step 5b-c: kPresent is false → skip, advance k.
        False ->
          iterate_loop(
            state,
            arr,
            idx + step,
            end,
            step,
            cb,
            this_arg,
            hole_mode,
            stop_on,
            cont,
          )
        True -> {
          // Pattern A step 5c.i / Pattern B step 4b: kValue = Get(O, Pk).
          // For holes with VisitHoles, get_index returns JsUndefined.
          use elem, state <- state.try_op(get_index(state, arr, idx))
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
    False ->
      // Step 6b: kPresent = HasProperty(O, Pk)
      case has_index(state.heap, arr, idx) {
        // kPresent is false → skip (step 6d: k = k + 1)
        False ->
          fold_loop(state, arr, idx + 1, length, cb, this_arg, acc, combine)
        True -> {
          // Step 6c.i: kValue = Get(O, Pk)
          use elem, state <- state.try_op(get_index(state, arr, idx))
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
  // Steps 1-2: Let O be ? ToObject(this value). Let len be ? LengthOfArrayLike(O).
  use _ref, length, state <- require_array(this, state)
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
  // Step 7: If initialValue is present, set accumulator to initialValue.
  // Presence checked by arg count (args.length >= 2).
  let #(has_init, init) = case args {
    [_, v, ..] -> #(True, v)
    _ -> #(False, JsUndefined)
  }
  case has_init {
    // Steps 5, 7a, 9: k=0, accumulator=initialValue, enter main loop
    True -> reduce_loop(state, this, 0, length, cb, init, 1)
    False -> {
      // Steps 8a-8c: Find first present element as initial accumulator.
      // find_present implements step 8b's loop: scan forward for HasProperty=true.
      use found, state <- state.try_op(find_present(state, this, 0, length, 1))
      case found {
        // Step 4/8c: If len=0 or no present element found, throw TypeError.
        None ->
          state.type_error(state, "Reduce of empty array with no initial value")
        // Step 8b.iii: Set accumulator to Get(O, Pk), then k = k + 1.
        // Step 9: Enter main loop starting at first_idx + 1.
        Some(#(first_idx, first_val)) ->
          reduce_loop(state, this, first_idx + 1, length, cb, first_val, 1)
      }
    }
  }
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
  // Steps 1-2: Let O be ? ToObject(this value). Let len be ? LengthOfArrayLike(O).
  use _ref, length, state <- require_array(this, state)
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
  // Step 7: If initialValue is present, set accumulator to initialValue.
  let #(has_init, init) = case args {
    [_, v, ..] -> #(True, v)
    _ -> #(False, JsUndefined)
  }
  case has_init {
    // Steps 5, 7a, 9: k=len-1, accumulator=initialValue, enter main loop (backward)
    True -> reduce_loop(state, this, length - 1, -1, cb, init, -1)
    False -> {
      // Steps 8a-8c: Find last present element as initial accumulator.
      // find_present with step=-1 scans backward (step 8b: while k ≥ 0).
      use found, state <- state.try_op(find_present(
        state,
        this,
        length - 1,
        -1,
        -1,
      ))
      case found {
        // Step 4/8c: If len=0 or no present element found, throw TypeError.
        None ->
          state.type_error(state, "Reduce of empty array with no initial value")
        // Step 8b.iii: Set accumulator to Get(O, Pk), then k = k - 1.
        // Step 9: Enter main loop starting at first_idx - 1 (backward).
        Some(#(first_idx, first_val)) ->
          reduce_loop(state, this, first_idx - 1, -1, cb, first_val, -1)
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
/// Step 8b.ii: Set kPresent to ? HasProperty(O, Pk) — implemented via has_index.
/// Step 8b.iii.1: Set accumulator to ? Get(O, Pk) — implemented via get_index.
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
) -> Result(#(Option(#(Int, JsValue)), State), #(JsValue, State)) {
  // Loop termination: k < len (forward) or k ≥ 0 (backward)
  case idx == end {
    // Step 8c: kPresent is false after exhausting all indices
    True -> Ok(#(None, state))
    False ->
      // Step 8b.ii: Set kPresent to ? HasProperty(O, Pk)
      case has_index(state.heap, this, idx) {
        // Step 8b.iii: kPresent is true — set accumulator to Get(O, Pk)
        True -> {
          use #(val, state) <- result.map(get_index(state, this, idx))
          #(Some(#(idx, val)), state)
        }
        // Step 8b.iv: Set k to k + 1 (or k - 1 for reduceRight)
        False -> find_present(state, this, idx + step, end, step)
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
) -> #(State, Result(JsValue, JsValue)) {
  // Step 9 loop condition: k < len (forward) or k ≥ 0 (backward)
  case idx == end {
    // Step 10: Return accumulator.
    True -> #(state, Ok(acc))
    False ->
      // Step 9b: Let kPresent be ? HasProperty(O, Pk).
      case has_index(state.heap, arr, idx) {
        // kPresent is false — skip this index (hole).
        // Step 9d: Set k to k + 1 (or k - 1 for reduceRight).
        False -> reduce_loop(state, arr, idx + step, end, cb, acc, step)
        // Step 9c: If kPresent is true, then
        True -> {
          // Step 9c.i: Let kValue be ? Get(O, Pk).
          use elem, state <- state.try_op(get_index(state, arr, idx))
          // Step 9c.ii: Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
          // Note: thisArg is always undefined for reduce/reduceRight (no thisArg parameter).
          use result, state <- state.try_call(state, cb, JsUndefined, [
            acc,
            elem,
            value.from_int(idx),
            arr,
          ])
          // Step 9d: Set k to k + 1 (or k - 1 for reduceRight).
          reduce_loop(state, arr, idx + step, end, cb, result, step)
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
  use ref, length, state <- require_array(this, state)
  // Steps 3-6: actualStart
  let actual_start = case args {
    [s, ..] -> resolve_index(s, length, 0)
    [] -> 0
  }
  // Steps 7-10: actualDeleteCount
  let #(actual_delete_count, items) = case args {
    // No args: spec says actualDeleteCount = 0, items = empty
    [] -> #(0, [])
    // Only start: deleteCount = len - actualStart (delete everything from start)
    [_] -> #(length - actual_start, [])
    // start + deleteCount [+ items]
    [_, dc_val, ..rest] -> {
      let dc = case helpers.to_number_int(dc_val) {
        Some(n) -> int.max(n, 0)
        None -> 0
      }
      #(int.min(dc, length - actual_start), rest)
    }
  }
  // Step 11: Build the removed array A from [actualStart..actualStart+actualDeleteCount)
  use removed_elements, state <- state.try_op(copy_range(
    state,
    this,
    actual_start,
    0,
    actual_delete_count,
    elements.new(),
  ))
  let #(heap, removed_ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(actual_delete_count),
        properties: dict.new(),
        elements: removed_elements,
        prototype: Some(array_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  let state = State(..state, heap:)
  let removed_arr = JsObject(removed_ref)
  // Steps 12-17: Shift elements and insert items.
  let item_count = list.length(items)
  let new_length = length - actual_delete_count + item_count
  // §23.1.3.31 step 11: If len + insertCount - actualDeleteCount > 2^53 - 1, throw TypeError
  use <- state.guard_safe_length(state, new_length)
  let shift = item_count - actual_delete_count
  // If shift > 0: we need to move elements right. If shift < 0: move left.
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
  use state <- state.try_state(splice_insert(state, ref, actual_start, items))
  // Step 17: Set length.
  wrap(generic_set_length(state, ref, new_length), removed_arr)
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
    True -> splice_shift_right(state, ref, length - 1, from_start, shift)
    False ->
      case shift < 0 {
        // Moving left: iterate from start
        True -> {
          use state <- result.try(shift_left_from(
            state,
            ref,
            from_start,
            length,
            shift,
          ))
          // Delete trailing elements that are now beyond the new length
          delete_trailing(state, ref, length + shift, length)
        }
        // No shift needed
        False -> Ok(state)
      }
  }
}

/// Shift elements right by `shift` positions, iterating from end to start.
/// Moves elements at [from_start..k+1) rightward by `shift`.
fn splice_shift_right(
  state: State,
  ref: Ref,
  k: Int,
  from_start: Int,
  shift: Int,
) -> Result(State, #(JsValue, State)) {
  case k < from_start {
    True -> Ok(state)
    False -> {
      let to = k + shift
      case generic_has(state.heap, ref, k) {
        True -> {
          use #(val, state) <- result.try(generic_get(state, ref, k))
          use state <- result.try(generic_set_index(state, ref, to, val))
          splice_shift_right(state, ref, k - 1, from_start, shift)
        }
        False -> {
          use state <- result.try(generic_delete(state, ref, int.to_string(to)))
          splice_shift_right(state, ref, k - 1, from_start, shift)
        }
      }
    }
  }
}

/// Shift elements left: move elements at [from..len) leftward by |shift|.
fn shift_left_from(
  state: State,
  ref: Ref,
  from: Int,
  length: Int,
  shift: Int,
) -> Result(State, #(JsValue, State)) {
  case from >= length {
    True -> Ok(state)
    False -> {
      let to = from + shift
      case generic_has(state.heap, ref, from) {
        True -> {
          use #(val, state) <- result.try(generic_get(state, ref, from))
          use state <- result.try(generic_set_index(state, ref, to, val))
          shift_left_from(state, ref, from + 1, length, shift)
        }
        False -> {
          use state <- result.try(generic_delete(state, ref, int.to_string(to)))
          shift_left_from(state, ref, from + 1, length, shift)
        }
      }
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
  use _ref, length, state <- require_array(this, state)
  // Steps 3-5: depth (default 1)
  let depth = case args {
    [d, ..] ->
      case d {
        JsUndefined -> 1
        _ ->
          helpers.to_number_int(d)
          |> option.map(int.max(_, 0))
          |> option.unwrap(0)
      }
    [] -> 1
  }
  // Steps 6-8: FlattenIntoArray, ArraySpeciesCreate, return A
  flatten_into(state, this, length, depth, [])
  |> finish_list
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
    False ->
      case has_index(state.heap, src, idx) {
        False ->
          // Hole: skip
          flatten_into_loop(state, src, idx + 1, length, depth, acc)
        True -> {
          use elem, state <- state.try_op(get_index(state, src, idx))
          // If depth > 0 and element is an array, recursively flatten
          case depth > 0 {
            True ->
              case is_array_value(elem, state.heap) {
                Some(sub_len) -> {
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
                None ->
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

/// Check if a JsValue is an array, returning its length if so.
fn is_array_value(val: JsValue, h: Heap) -> Option(Int) {
  case val {
    JsObject(ref) ->
      case heap.read_array(h, ref) {
        Some(#(length, _elems)) -> Some(length)
        None -> None
      }
    _ -> None
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
  use _ref, length, state <- require_array(this, state)
  // Step 3: IsCallable check
  use cb, this_arg, state <- require_callback(args, state)
  // Step 5: FlattenIntoArray with mapperFunction — call mapper on each present
  // element, then flatten the result one level deep via flatten_into at depth 0
  // (which just collects present elements). If the mapped value is not an array,
  // prepend directly.
  flat_map_loop(state, this, 0, length, cb, this_arg, [])
  // Steps 4+6: ArraySpeciesCreate(O, 0), return A
  |> finish_list
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
    False ->
      case has_index(state.heap, arr, idx) {
        False -> flat_map_loop(state, arr, idx + 1, length, cb, this_arg, acc)
        True -> {
          use elem, state <- state.try_op(get_index(state, arr, idx))
          use mapped, state <- state.try_call(state, cb, this_arg, [
            elem,
            value.from_int(idx),
            arr,
          ])
          case is_array_value(mapped, state.heap) {
            Some(sub_len) ->
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
            None ->
              flat_map_loop(state, arr, idx + 1, length, cb, this_arg, [
                mapped,
                ..acc
              ])
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
  use ref, length, state <- require_array(this, state)
  // Steps 3-5: target
  let target = case args {
    [t, ..] -> resolve_index(t, length, 0)
    [] -> 0
  }
  // Steps 6-8: start (from)
  let from = case args {
    [_, s, ..] -> resolve_index(s, length, 0)
    _ -> 0
  }
  // Steps 9-11: end (final)
  let final = case args {
    [_, _, e, ..] -> resolve_index(e, length, length)
    _ -> length
  }
  // Step 12: count = min(final - from, len - to)
  let count = int.min(final - from, length - target)
  case count <= 0 {
    True -> #(state, Ok(this))
    False ->
      // Steps 13-15: Direction-aware copy
      case from < target && target < from + count {
        // Overlapping, copy backwards
        True ->
          wrap(
            copy_within_backward(
              state,
              ref,
              from + count - 1,
              target + count - 1,
              count,
            ),
            this,
          )
        // No overlap issue, copy forwards
        False ->
          wrap(copy_within_forward(state, ref, from, target, count), this)
      }
  }
}

/// Copy elements forward (from..from+count) to (target..target+count).
fn copy_within_forward(
  state: State,
  ref: Ref,
  from: Int,
  to: Int,
  remaining: Int,
) -> Result(State, #(JsValue, State)) {
  case remaining <= 0 {
    True -> Ok(state)
    False ->
      case generic_has(state.heap, ref, from) {
        True -> {
          use #(val, state) <- result.try(generic_get(state, ref, from))
          use state <- result.try(generic_set_index(state, ref, to, val))
          copy_within_forward(state, ref, from + 1, to + 1, remaining - 1)
        }
        False -> {
          use state <- result.try(generic_delete(state, ref, int.to_string(to)))
          copy_within_forward(state, ref, from + 1, to + 1, remaining - 1)
        }
      }
  }
}

/// Copy elements backward for overlapping regions.
fn copy_within_backward(
  state: State,
  ref: Ref,
  from: Int,
  to: Int,
  remaining: Int,
) -> Result(State, #(JsValue, State)) {
  case remaining <= 0 {
    True -> Ok(state)
    False ->
      case generic_has(state.heap, ref, from) {
        True -> {
          use #(val, state) <- result.try(generic_get(state, ref, from))
          use state <- result.try(generic_set_index(state, ref, to, val))
          copy_within_backward(state, ref, from - 1, to - 1, remaining - 1)
        }
        False -> {
          use state <- result.try(generic_delete(state, ref, int.to_string(to)))
          copy_within_backward(state, ref, from - 1, to - 1, remaining - 1)
        }
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
      // LengthOfArrayLike: read "length" via get_value_of, then ToLength.
      let length = case object.get_value_of(state, items, Named("length")) {
        Ok(#(len_val, _state)) -> to_length(len_val)
        Error(_) -> 0
      }
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
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(length),
            properties: dict.new(),
            elements: elements.from_list(vals),
            prototype: Some(array_proto),
            symbol_properties: [],
            extensible: True,
          ),
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
  let count = list.length(args)
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(count),
        properties: dict.new(),
        elements: elements.from_list(args),
        prototype: Some(array_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
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
  use _ref, length, state <- require_array(this, state)
  // Steps 3-6: actualStart
  let actual_start = case args {
    [s, ..] -> resolve_index(s, length, 0)
    [] -> 0
  }
  // Step 7: insertCount and actualSkipCount
  let #(actual_skip_count, items) = case args {
    // No args: skipCount=0, items=[]
    [] -> #(0, [])
    // Only start: skipCount = len - actualStart
    [_] -> #(length - actual_start, [])
    // start + skipCount [+ items]
    [_, dc_val, ..rest] -> {
      let dc = case helpers.to_number_int(dc_val) {
        Some(n) -> int.max(n, 0)
        None -> 0
      }
      #(int.min(dc, length - actual_start), rest)
    }
  }
  // Step 8: newLen = len + insertCount - actualSkipCount
  let item_count = list.length(items)
  let new_len = length + item_count - actual_skip_count
  // Steps 10-14: Build the new array
  // Copy [0, actualStart) from source
  use new_elements, state <- state.try_op(copy_range(
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
  use new_elements, state <- state.try_op(copy_range(
    state,
    this,
    src_from,
    dst_from,
    remaining,
    new_elements,
  ))
  // Step 15: Return A
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(new_len),
        properties: dict.new(),
        elements: new_elements,
        prototype: Some(array_proto),
        symbol_properties: [],
        extensible: True,
      ),
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
  use _ref, length, state <- require_array(this, state)
  // Step 3: relativeIndex = ToIntegerOrInfinity(index)
  let raw = case args {
    [v, ..] -> helpers.to_number_int(v) |> option.unwrap(0)
    [] -> 0
  }
  // Steps 4-5: resolve relative index (without clamping — out of bounds throws)
  let actual_index = case raw < 0 {
    True -> length + raw
    False -> raw
  }
  // Step 6: bounds check — throw RangeError if out of bounds
  case actual_index < 0 || actual_index >= length {
    True -> state.range_error(state, "Invalid index")
    False -> {
      // Get the replacement value
      let replacement = case args {
        [_, r, ..] -> r
        _ -> JsUndefined
      }
      // Steps 7-11: Copy all elements, replacing actualIndex with value
      use new_elements, state <- state.try_op(copy_range(
        state,
        this,
        0,
        0,
        length,
        elements.new(),
      ))
      let new_elements = elements.set(new_elements, actual_index, replacement)
      // Step 12: Return A
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(length),
            properties: dict.new(),
            elements: new_elements,
            prototype: Some(array_proto),
            symbol_properties: [],
            extensible: True,
          ),
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
  let comparefn = helpers.first_arg_or_undefined(args)
  case comparefn {
    JsUndefined -> {
      use _ref, length, state <- require_array(this, state)
      to_sorted_default(state, length, this)
    }
    _ ->
      case helpers.is_callable(state.heap, comparefn) {
        True -> {
          use _ref, length, state <- require_array(this, state)
          to_sorted_with_comparefn(state, length, this, comparefn)
        }
        False ->
          state.type_error(
            state,
            common.typeof_value(comparefn, state.heap) <> " is not a function",
          )
      }
  }
}

/// Default sort for toSorted: collect elements, stringify, sort lexicographically,
/// then build a new array from the result.
fn to_sorted_default(
  state: State,
  length: Int,
  this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  use #(defined, undefs), state <- state.try_op(collect_sort_elements(
    state,
    this,
    length,
    0,
    [],
    0,
  ))
  use pairs, state <- state.try_op(stringify_elements(state, defined, []))
  let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
  let sorted_values = list.map(sorted, fn(pair) { pair.1 })
  let all_values = list.append(sorted_values, list.repeat(JsUndefined, undefs))
  let new_elements = build_elements_from_list(all_values, 0, elements.new())
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(length),
        properties: dict.new(),
        elements: new_elements,
        prototype: Some(array_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Comparefn sort for toSorted: collect elements, merge-sort using comparefn,
/// then build a new array from the result.
fn to_sorted_with_comparefn(
  state: State,
  length: Int,
  this: JsValue,
  comparefn: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  use #(defined, undefs), state <- state.try_op(collect_sort_elements(
    state,
    this,
    length,
    0,
    [],
    0,
  ))
  use sorted, state <- state.try_op(merge_sort(state, defined, comparefn))
  let all_values = list.append(sorted, list.repeat(JsUndefined, undefs))
  let new_elements = build_elements_from_list(all_values, 0, elements.new())
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(length),
        properties: dict.new(),
        elements: new_elements,
        prototype: Some(array_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
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
  use _ref, length, state <- require_array(this, state)
  // Collect all elements; holes become undefined (spec step 5c: Get returns undefined for holes).
  use all_values, state <- state.try_op(
    collect_all_elements(state, this, length, 0, []),
  )
  // Reverse the collected list.
  let reversed = list.reverse(all_values)
  let new_elements = build_elements_from_list(reversed, 0, elements.new())
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(length),
        properties: dict.new(),
        elements: new_elements,
        prototype: Some(array_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Collect all elements at indices [0, length), reading holes as JsUndefined.
/// Per §23.1.3.33 step 5c: Get(O, from) — holes are treated as undefined.
fn collect_all_elements(
  state: State,
  this: JsValue,
  length: Int,
  idx: Int,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case idx >= length {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      use #(val, state) <- result.try(get_index(state, this, idx))
      collect_all_elements(state, this, length, idx + 1, [val, ..acc])
    }
  }
}

/// ES2024 §23.1.3.31 Array.prototype.toString ( )
/// Calls this.join() — equivalent to Array.prototype.join().
fn array_to_string(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // §23.1.3.31: "Let func be ? Get(array, "join")." then call it.
  // Simplified: delegate directly to array_join (which uses require_array).
  array_join(this, [], state)
}

/// ES2024 §23.1.3.30 Array.prototype.toLocaleString ( )
/// Calls toLocaleString() on each element and joins with ",".
fn array_to_locale_string(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _ref, length, state <- require_array(this, state)
  to_locale_string_loop(state, this, 0, length, [])
}

fn to_locale_string_loop(
  state: State,
  this: JsValue,
  idx: Int,
  length: Int,
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
        JsUndefined | JsNull ->
          to_locale_string_loop(state, this, idx + 1, length, ["", ..acc])
        _ -> {
          use s, state <- coerce.try_to_string(state, elem)
          to_locale_string_loop(state, this, idx + 1, length, [s, ..acc])
        }
      }
    }
  }
}

/// ES2024 §23.1.3.16 Array.prototype.keys ( )
/// Returns an array of indices (simplified — no iterator protocol).
fn array_keys(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _ref, length, state <- require_array(this, state)
  let keys = build_index_list(0, length, [])
  state.ok_array(state, keys)
}

fn build_entry_pairs(
  state: State,
  this: JsValue,
  idx: Int,
  length: Int,
  array_proto: Ref,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case idx >= length {
    True -> Ok(#(list.reverse(acc), state))
    False -> {
      use #(val, state) <- result.try(get_index(state, this, idx))
      let #(heap, pair_ref) =
        common.alloc_array(state.heap, [value.from_int(idx), val], array_proto)
      build_entry_pairs(
        State(..state, heap:),
        this,
        idx + 1,
        length,
        array_proto,
        [JsObject(pair_ref), ..acc],
      )
    }
  }
}

fn build_index_list(
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> build_index_list(idx + 1, length, [value.from_int(idx), ..acc])
  }
}

/// ES2024 §23.1.3.37 Array.prototype.values ( )
/// Returns a new Array Iterator object (§23.1.5.1 CreateArrayIterator).
fn array_values(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, _length, state <- require_array(this, state)
  let #(heap, iter_ref) =
    common.alloc_wrapper(
      state.heap,
      value.ArrayIteratorObject(source: ref, index: 0),
      state.builtins.array_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(iter_ref)))
}

/// ES2024 §23.1.3.4 Array.prototype.entries ( )
/// Returns an array of [index, value] pairs (simplified — no iterator protocol).
fn array_entries(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let proto = state.builtins.array.prototype
  use _ref, length, state <- require_array(this, state)
  use pairs, state <- state.try_op(
    build_entry_pairs(state, this, 0, length, proto, []),
  )
  state.ok_array(state, pairs)
}
