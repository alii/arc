import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/object
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
  DataProperty, Finite, JsBool, JsNull, JsNumber, JsObject, JsString,
  JsUndefined, ObjectSlot,
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
    ])
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("isArray", ArrayNative(ArrayIsArray), 1),
      #("from", ArrayNative(ArrayFrom), 1),
      #("of", ArrayNative(ArrayOf), 0),
    ])
  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { ArrayNative(ArrayConstructor) },
    "Array",
    1,
    static_methods,
  )
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
  }
}

/// Array() / new Array() — construct a new array.
/// Wrapper that threads State around native_array_constructor.
/// §23.1.1 The Array Constructor: "is the initial value of the Array property
/// of the global object." Called as both function and constructor (identical).
pub fn construct(
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
pub fn is_array(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let heap = state.heap
  #(State(..state, heap:), Ok(native_is_array(args, heap)))
}

/// Array ( ...values ) — §23.1.1 The Array Constructor
/// Dispatches on numberOfArgs to one of three sub-algorithms:
///   §23.1.1.1 Array() — 0 args
///   §23.1.1.2 Array(len) — 1 arg
///   §23.1.1.3 Array(...items) — 2+ args
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
    [] -> alloc_array(heap, 0, js_elements.new(), array_proto)

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
        True -> alloc_array(heap, len, js_elements.new(), array_proto)
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
      alloc_array(heap, count, js_elements.from_list(args), array_proto)
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
        symbol_properties: dict.new(),
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
pub fn array_join(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: Let O be ? ToObject(this value).
  //            Let len be ? LengthOfArrayLike(O).
  // (handled by require_array — converts this to object, reads .length)
  use _ref, length, elements, state <- require_array(this, state)
  // Steps 3-4: If separator is undefined, let sep be ",".
  //            Else, let sep be ? ToString(separator).
  let sep_val = case args {
    [JsUndefined, ..] | [] -> JsString(",")
    [v, ..] -> v
  }
  use separator, state <- frame.try_to_string(state, sep_val)
  // Steps 5-8: Build result string R by iterating k from 0 to len-1,
  //            joining elements with sep. Return R.
  case join_elements(elements, 0, length, separator, [], state) {
    #(state, Ok(result)) -> #(state, Ok(JsString(result)))
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

/// join_elements — implements step 7 of Array.prototype.join (§23.1.3.18).
/// Iterates k from 0 to len-1, building the result string R.
///
/// TODO(Deviation): reads elements directly from JsElements instead of calling
/// [[Get]](O, ToString(k)). This skips prototype chain lookups for missing
/// indices — holes return undefined directly rather than walking the prototype.
/// For generic array-likes passed via .call/.apply, the spec's Get would
/// invoke getters on the prototype chain.
fn join_elements(
  elements: JsElements,
  idx: Int,
  length: Int,
  separator: String,
  acc: List(String),
  state: State,
) -> #(State, Result(String, JsValue)) {
  case idx >= length {
    // Step 8: Return R.
    True -> #(state, Ok(acc |> list.reverse |> string.join(separator)))
    False -> {
      // Step 7b: Let element be ? Get(O, ! ToString(𝔽(k))).
      let val = js_elements.get(elements, idx)
      case val {
        // Step 7c: If element is undefined or null, let next be "".
        JsUndefined | JsNull ->
          join_elements(
            elements,
            idx + 1,
            length,
            separator,
            ["", ..acc],
            state,
          )
        // Step 7c (cont.): Otherwise, let next be ? ToString(element).
        _ ->
          case frame.to_string(state, val) {
            // Step 7d: Set R to string-concatenation of R and next.
            // Step 7e: Set k to k + 1.
            Ok(#(str, state)) ->
              join_elements(
                elements,
                idx + 1,
                length,
                separator,
                [str, ..acc],
                state,
              )
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
      }
    }
  }
}

/// Array.prototype.push ( ...items )
/// ES2024 §23.1.3.22
pub fn array_push(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: Let O be ? ToObject(this value).
  //            Let len be ? LengthOfArrayLike(O).
  // (require_array handles ToObject + length extraction as a fast path.)
  use ref, length, _elements, state <- require_array(this, state)
  // Step 3: Let argCount be the number of elements in items.
  // Step 4: If len + argCount > 2^53 - 1, throw a TypeError.
  // NOTE: Step 4 (overflow check) is not implemented — we don't guard
  // against len + argCount exceeding MAX_SAFE_INTEGER.
  // Steps 5-7 delegated to push_generic.
  case push_generic(state, ref, length, args) {
    // Step 7: Return 𝔽(len).
    Ok(#(new_length, state)) -> #(state, Ok(js_int(new_length)))
    Error(#(thrown, state)) -> #(state, Error(thrown))
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

/// Convert an Int to JsNumber.
fn js_int(n: Int) -> JsValue {
  JsNumber(Finite(int.to_float(n)))
}

/// Cap for LengthOfArrayLike on non-array objects. Spec allows up to 2^53-1
/// but we synthesize elements eagerly here, so unbounded lengths would OOM.
/// Real arrays don't go through this path (their length is already trusted).
const max_array_like_length = 4_294_967_295

/// Sentinel ref passed to `cont` for primitive `this` values. heap.read returns
/// Error for any id not in the heap, and heap.write is a no-op for unallocated
/// refs, so mutating methods safely no-op on primitives.
const invalid_ref = value.Ref(-1)

/// Combined ToObject (ES2024 §7.1.18) + LengthOfArrayLike (§7.3.18) fast path.
///
/// Read `this` as an array-like. Returns #(ref, length, elements) on success.
/// Throws TypeError on null/undefined (ToObject step for those types).
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
/// We handle the following cases:
///   - True arrays (ArrayObject) — fast path, pass through directly
///   - arguments (ArgumentsObject) — length in kind, indexed values in elements
///   - String wrappers and primitive strings — synthesize from code units
///     (StringGetOwnProperty §10.4.3.5)
///   - Plain objects — read `.length` from properties dict, gather indexed
///     values from both `elements` and stringified-int keys in `properties`
///   - Other primitives (number/bool) — ToObject wrapper has no `.length` → 0
///
/// For non-array objects, indexed values may live in the `properties` dict
/// as string keys ("0", "1", ...) rather than `elements` — put_elem_value
/// stores them that way for OrdinaryObject. We merge both sources into a
/// fresh SparseElements so `js_elements.has`/`get` give correct hole semantics.
fn require_array(
  this: JsValue,
  state: State,
  cont: fn(Ref, Int, JsElements, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    // §7.1.18: Object → return argument unchanged.
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        // Fast path: real array — length from [[ArrayLength]], elements
        // already in the right shape. No need for LengthOfArrayLike.
        Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) ->
          cont(ref, length, elements, state)
        // Arguments exotic object (§10.4.4): length stored in kind,
        // indexed values already in elements.
        Some(ObjectSlot(kind: value.ArgumentsObject(length:), elements:, ..)) ->
          cont(ref, length, elements, state)
        // §7.1.18 String row / §10.4.3: String exotic object.
        // Synthesize elements via StringGetOwnProperty (§10.4.3.5).
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) -> {
          let #(length, elements) = string_to_elements(s)
          cont(ref, length, elements, state)
        }
        // Generic object: LengthOfArrayLike (§7.3.18) — Get(obj, "length"),
        // then ToLength (§7.1.17), then gather indexed properties.
        Some(ObjectSlot(properties:, elements:, ..)) -> {
          let length = read_length_property(properties)
          let elements = gather_indexed(properties, elements, length)
          cont(ref, length, elements, state)
        }
        // Non-object heap slot under a ref shouldn't happen, but fall through.
        _ -> cont(ref, 0, js_elements.new(), state)
      }
    // §7.1.18: Undefined / Null → throw a TypeError exception.
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
    // §7.1.18 String row: ToObject creates a String exotic object (§10.4.3).
    // We don't actually allocate the wrapper — just synthesize elements inline
    // via StringGetOwnProperty (§10.4.3.5) semantics.
    // Mutating methods will fail via generic_set on invalid_ref (no-op), which
    // is correct since string indices are non-writable (§10.4.3.5 step 11).
    JsString(s) -> {
      let #(length, elements) = string_to_elements(s)
      cont(invalid_ref, length, elements, state)
    }
    // §7.1.18 Boolean/Number/Symbol/BigInt rows: ToObject creates a wrapper
    // object with no own `.length` property → LengthOfArrayLike returns 0.
    _ -> cont(invalid_ref, 0, js_elements.new(), state)
  }
}

/// StringGetOwnProperty (ES2024 §10.4.3.5) — bulk version.
///
/// Converts a string's [[StringData]] into (length, JsElements) where each
/// index holds a single UTF-16 code unit as a JsString, matching JS semantics
/// where `"𝄞".length === 2` (surrogate pair = two indices).
///
/// Per §10.4.3.5:
///   Step 7: Let str be S.[[StringData]].
///   Step 8: Let len be the length of str (in UTF-16 code units).
///   Step 9-10: If ℝ(index) < 0 or ℝ(index) ≥ len, return undefined.
///   Step 11: Let resultStr be the substring of str from index to index + 1.
///   Step 12: Return PropertyDescriptor {
///     [[Value]]: resultStr, [[Writable]]: false,
///     [[Enumerable]]: true, [[Configurable]]: false }.
///
/// We pre-compute ALL indices rather than lazily resolving per-access.
/// Gleam strings are UTF-8, so we must split supplementary-plane codepoints
/// (U+10000..U+10FFFF) into UTF-16 surrogate pairs to get correct indices.
fn string_to_elements(s: String) -> #(Int, JsElements) {
  let code_units =
    string.to_utf_codepoints(s)
    |> list.flat_map(fn(cp) {
      let n = string.utf_codepoint_to_int(cp)
      case n > 0xFFFF {
        True -> {
          // Supplementary plane — split into UTF-16 surrogate pair.
          // High surrogate: 0xD800 + (cp - 0x10000) >> 10
          // Low surrogate:  0xDC00 + (cp - 0x10000) & 0x3FF
          let adjusted = n - 0x10000
          let hi = 0xD800 + int.bitwise_shift_right(adjusted, 10)
          let lo = 0xDC00 + int.bitwise_and(adjusted, 0x3FF)
          [surrogate_to_string(hi), surrogate_to_string(lo)]
        }
        // BMP codepoint — single code unit, maps 1:1 to a string index.
        False -> [string.from_utf_codepoints([cp])]
      }
    })
  // Each element corresponds to one UTF-16 code unit index position.
  // §10.4.3.5 step 8: len = number of code units in [[StringData]].
  #(
    list.length(code_units),
    js_elements.from_list(list.map(code_units, JsString)),
  )
}

/// Convert a surrogate code unit (0xD800-0xDFFF range) to a one-char string.
///
/// Per §10.4.3.5 step 11, each UTF-16 code unit — including isolated
/// surrogates — must be individually addressable as a single-character string.
/// Erlang/BEAM strings are UTF-8, which cannot represent isolated surrogates
/// (they are not valid Unicode scalar values). We fall back to U+FFFD
/// (REPLACEMENT CHARACTER) when the codepoint is rejected by the runtime.
///
/// TODO(Deviation): BEAM UTF-8 strings cannot represent isolated surrogates. A fully
/// conformant implementation would use raw binary (e.g. WTF-8) to preserve
/// surrogate identity. The U+FFFD fallback gives correct .length and index
/// semantics but means `"𝄞"[0] === "\uFFFD"` instead of `"\uD834"`.
fn surrogate_to_string(code: Int) -> String {
  case string.utf_codepoint(code) {
    Ok(cp) -> string.from_utf_codepoints([cp])
    Error(_) -> "\u{FFFD}"
  }
}

/// LengthOfArrayLike (ES2024 §7.3.18) — pure approximation.
///
/// §7.3.18 LengthOfArrayLike ( obj ):
///   1. Assert: Type(obj) is Object.
///   2. Return ? ToLength(? Get(obj, "length")).
///
/// ToLength (§7.1.17):
///   1. Let len be ? ToIntegerOrInfinity(argument).
///   2. If len ≤ 0, return +0𝔽.
///   3. Return 𝔽(min(len, 2^53 - 1)).
///
/// This is a pure (no-State) approximation: it reads "length" from the
/// properties dict directly rather than going through the full [[Get]]
/// path, and uses helpers.to_number_int as a simplified ToIntegerOrInfinity.
///
/// TODO(Deviation): Does NOT do full ToPrimitive/ToNumber on object-valued length
/// — that would require VM re-entry (State threading). Returns 0 for objects.
/// TODO(Deviation): Accessor-valued "length" (get/set) would need State to invoke
/// the getter. Returns 0 for now.
/// Note: We cap at max_array_like_length (2^32 - 1) instead of 2^53 - 1
/// because we eagerly materialize elements and unbounded lengths would OOM.
fn read_length_property(properties: dict.Dict(String, Property)) -> Int {
  // §7.3.18 step 2: Get(obj, "length")
  case dict.get(properties, "length") {
    Ok(DataProperty(value: len_val, ..)) ->
      // §7.1.17 step 1: ToIntegerOrInfinity(len_val)
      case helpers.to_number_int(len_val) {
        // §7.1.17 step 2: If len ≤ 0, return +0.
        // §7.1.17 step 3: Return min(len, 2^53-1). We use max_array_like_length.
        Some(n) if n > 0 -> int.min(n, max_array_like_length)
        _ -> 0
      }
    // Accessor length: would need State to call getter. Return 0 for now.
    Ok(value.AccessorProperty(..)) -> 0
    // No "length" property → ToLength(undefined) → ToIntegerOrInfinity(NaN) → 0.
    Error(_) -> 0
  }
}

/// Build a JsElements snapshot for a generic array-like object.
///
/// This implements the "indexed property" reading that Array.prototype methods
/// do via repeated Get(O, ! ToString(𝔽(k))) calls (e.g. §23.1.3.13
/// Array.prototype.forEach step 6c: Let kValue be ? Get(O, Pk)).
///
/// In the spec, each method individually calls HasProperty + Get for each
/// index. We pre-compute all present indices into a SparseElements map so
/// that `js_elements.has()` correctly reports holes — forEach/map/etc. skip
/// indices not present, matching the spec's HasProperty check (§7.3.1).
///
/// Plain objects store numeric keys as strings in the `properties` dict
/// (see put_elem_value for OrdinaryObject — it stringifies the key and calls
/// object.set_property). We scan the properties dict for keys that parse as
/// non-negative ints in [0, length) and build a SparseElements from them.
///
/// The `elements` field is technically also merged in, but for OrdinaryObject
/// / FunctionObject / wrappers it's always empty (only ArrayObject and
/// ArgumentsObject route numeric writes there, and those have their own
/// cases above). We include it defensively at trivial cost.
///
/// TODO(Deviation): Accessor-valued indexed properties are skipped (can't call
/// getter without State threading). The spec's Get would invoke [[Get]]
/// which calls the getter.
///
/// Cost is O(|elements| + |properties|), independent of `length`.
fn gather_indexed(
  properties: dict.Dict(String, Property),
  elements: JsElements,
  length: Int,
) -> JsElements {
  // Seed from any existing elements entries (almost always empty here).
  let seed = case elements {
    value.DenseElements(_) ->
      collect_elements(
        elements,
        0,
        js_elements.stored_count(elements),
        length,
        dict.new(),
      )
    value.SparseElements(data) ->
      dict.filter(data, fn(k, _) { k >= 0 && k < length })
  }
  // Merge in numeric-string keys from properties. Properties win on collision
  // (they're where put_elem_value actually stores indices for OrdinaryObject).
  // This corresponds to the spec's Get(O, ! ToString(𝔽(k))) resolving through
  // the ordinary [[Get]] to an own data property.
  let merged =
    dict.fold(properties, seed, fn(acc, key, prop) {
      case int.parse(key) {
        Ok(idx) if idx >= 0 && idx < length ->
          case prop {
            DataProperty(value: v, ..) -> dict.insert(acc, idx, v)
            // Accessor: can't call getter in this pure context, skip.
            value.AccessorProperty(..) -> acc
          }
        _ -> acc
      }
    })
  value.SparseElements(merged)
}

/// Tail-recursive helper: copy in-range present entries from `elements`
/// into an accumulator dict. Mirrors the spec's HasProperty (§7.3.1) +
/// Get (§7.3.2) loop — only indices where HasProperty returns true are
/// included. Absent indices (holes in DenseElements) are omitted, so
/// downstream iteration correctly skips them per the spec's "If kPresent
/// is true, then" pattern (e.g. §23.1.3.13 forEach step 6c).
fn collect_elements(
  elements: JsElements,
  idx: Int,
  stored: Int,
  length: Int,
  acc: dict.Dict(Int, JsValue),
) -> dict.Dict(Int, JsValue) {
  case idx >= stored || idx >= length {
    True -> acc
    False -> {
      // §7.3.1 HasProperty(O, P): check if index is present (not a hole).
      let acc = case js_elements.has(elements, idx) {
        // §7.3.2 Get(O, P): retrieve the value at this index.
        True -> dict.insert(acc, idx, js_elements.get(elements, idx))
        False -> acc
      }
      collect_elements(elements, idx + 1, stored, length, acc)
    }
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
      frame.type_error(
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
      key,
      val,
      JsObject(ref),
    ))
    case success {
      // success = true → return normally.
      True -> Ok(state)
      // §7.3.4 step 2: success = false and Throw = true → TypeError.
      False -> {
        let #(heap, err) =
          common.make_type_error(
            state.heap,
            state.builtins,
            "Cannot assign to read only property '" <> key <> "' of object",
          )
        Error(#(err, State(..state, heap:)))
      }
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
  generic_set(state, ref, "length", JsNumber(Finite(int.to_float(len))))
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
  let #(h, ok) = object.delete_property(state.heap, ref, key)
  let state = State(..state, heap: h)
  case ok {
    // success = true → return normally.
    True -> Ok(state)
    // §7.3.9 step 2: success = false → throw TypeError.
    False -> {
      let #(heap, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "Cannot delete property '" <> key <> "' of object",
        )
      Error(#(err, State(..state, heap:)))
    }
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
  object.has_property(heap, ref, int.to_string(idx))
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
  object.get_value(state, ref, int.to_string(idx), JsObject(ref))
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
pub fn array_pop(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use ref, length, _elements, state <- require_array(this, state)
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
      case generic_get(state, ref, new_len) {
        Ok(#(val, state)) -> {
          // Step 4e: DeletePropertyOrThrow(O, index)
          use state <- try_wrap(generic_delete(
            state,
            ref,
            int.to_string(new_len),
          ))
          // Step 4f: Set(O, "length", newLen, true)
          // Step 4g: Return element
          wrap(generic_set_length(state, ref, new_len), val)
        }
        Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn array_shift(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use ref, length, _elements, state <- require_array(this, state)
  case length == 0 {
    // Step 3: len = 0
    // Step 3a: Set(O, "length", +0𝔽, true)
    // Step 3b: Return undefined
    True -> wrap(generic_set_length(state, ref, 0), JsUndefined)
    False ->
      // Step 4: first = Get(O, "0")
      case generic_get(state, ref, 0) {
        Ok(#(val, state)) -> {
          // Steps 5-6: shift indices [1..len) down by 1 (see shift_left_generic)
          use state <- try_wrap(shift_left_generic(state, ref, 1, length))
          // Step 7: DeletePropertyOrThrow(O, ToString(len - 1))
          use state <- try_wrap(generic_delete(
            state,
            ref,
            int.to_string(length - 1),
          ))
          // Step 8: Set(O, "length", len - 1, true)
          // Step 9: Return first
          wrap(generic_set_length(state, ref, length - 1), val)
        }
        Error(#(thrown, state)) -> #(state, Error(thrown))
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
/// TODO(Deviation): Step 4a overflow check (len + argCount > 2^53-1 → TypeError) is not
/// implemented — Gleam ints are arbitrary-precision so no overflow occurs.
pub fn array_unshift(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use ref, length, _elements, state <- require_array(this, state)
  // Step 3: argCount = number of elements in items
  let arg_count = list.length(args)
  // Short-circuit when argCount = 0 (spec would still Set length, we skip it)
  use <- bool.guard(arg_count == 0, #(state, Ok(js_int(length))))
  // Step 5-6 value: len + argCount
  let new_len = length + arg_count
  // Steps 4b-4c: shift existing elements right by argCount (right-to-left)
  case shift_right_generic(state, ref, length - 1, arg_count) {
    Ok(state) ->
      // Steps 4d-4e: write items at indices [0..argCount)
      case write_list_at(state, ref, 0, args) {
        Ok(state) ->
          // Step 5: Set(O, "length", len + argCount, true)
          // Step 6: Return len + argCount
          wrap(generic_set_length(state, ref, new_len), js_int(new_len))
        Error(#(thrown, state)) -> #(state, Error(thrown))
      }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
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

/// Utility: chain a generic op Result, continuing with `cont` on success.
/// Like `wrap` but instead of returning a fixed value, it passes the updated
/// State to a continuation that produces the final builtin return tuple.
/// Not a spec operation — purely internal CPS plumbing for sequencing
/// multiple fallible operations (e.g. Set then Set then return).
fn try_wrap(
  r: Result(State, #(JsValue, State)),
  cont: fn(State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case r {
    Ok(state) -> cont(state)
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
///   - require_array collapses steps 1-2 (ToObject + LengthOfArrayLike) and
///     gives us the internal elements directly instead of going through Get.
///   - Steps 3-10 are handled by resolve_index (see §7.1.22 / clamp logic).
///   - Step 12: we skip ArraySpeciesCreate and always create a plain Array.
///     This means @@species is not respected (a known simplification).
///   - Steps 14b-14c: copy_range uses js_elements.has for HasProperty on the
///     source elements, preserving holes (sparse indices are not copied).
///   - Step 15: length is set via ArrayObject(count) in the slot constructor
///     rather than a separate Set("length") call.
pub fn array_slice(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O).
  use _ref, length, elements, state <- require_array(this, state)
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
  let copied = copy_range(elements, start, 0, count, js_elements.new())
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(count),
        properties: dict.new(),
        elements: copied,
        prototype: Some(array_proto),
        symbol_properties: dict.new(),
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
  src: JsElements,
  src_idx: Int,
  dst_idx: Int,
  remaining: Int,
  dst: JsElements,
) -> JsElements {
  case remaining <= 0 {
    True -> dst
    False ->
      // Step 14b: kPresent = HasProperty(O, Pk).
      case js_elements.has(src, src_idx) {
        // Step 14c: kPresent is true — copy the element.
        True ->
          copy_range(
            src,
            src_idx + 1,
            dst_idx + 1,
            remaining - 1,
            js_elements.set(dst, dst_idx, js_elements.get(src, src_idx)),
          )
        // kPresent is false (hole): skip — do not set dst[dst_idx].
        False -> copy_range(src, src_idx + 1, dst_idx + 1, remaining - 1, dst)
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
pub fn array_concat(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Step 1: Let O be ? ToObject(this value).
  case this {
    JsNull | JsUndefined -> frame.type_error(state, cannot_convert)
    _ -> {
      // Step 4: Prepend O to items.
      let all_items = [this, ..args]
      // Steps 3, 5: n = 0, then iterate each element E of items.
      let #(heap, elements, length) =
        list.fold(all_items, #(state.heap, js_elements.new(), 0), fn(acc, item) {
          let #(h, elems, pos) = acc
          // Step 5a-c: IsConcatSpreadable check + spread or append.
          concat_item(h, elems, pos, item)
        })
      // Steps 2, 6-7: Create result array A with final length n, return A.
      let #(heap, ref) =
        heap.alloc(
          heap,
          ObjectSlot(
            kind: ArrayObject(length),
            properties: dict.new(),
            elements:,
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
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
  h: Heap,
  elems: JsElements,
  pos: Int,
  item: JsValue,
) -> #(Heap, JsElements, Int) {
  case item {
    JsObject(ref) ->
      case heap.read(h, ref) {
        // Step 5b: spreadable = true (IsArray(E) = true) — spread elements.
        // Step 5b.i: len = LengthOfArrayLike(E).
        // Step 5b.iv: copy elements [0..len) into result at position n.
        Some(ObjectSlot(kind: ArrayObject(length:), elements: src, ..)) -> {
          let copied = copy_range(src, 0, pos, length, elems)
          #(h, copied, pos + length)
        }
        // Step 5c: spreadable = false (non-array object) — append as single item.
        _ -> #(h, js_elements.set(elems, pos, item), pos + 1)
      }
    // Step 5c: E is not an object (primitive) — append as single item.
    _ -> #(h, js_elements.set(elems, pos, item), pos + 1)
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
pub fn array_reverse(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use ref, length, _elements, state <- require_array(this, state)
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
pub fn array_fill(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use ref, length, _elements, state <- require_array(this, state)
  // Step 12 uses value; if not provided, defaults to undefined
  let fill_val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
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
  // Steps 12-13: fill loop, then return O
  wrap(fill_generic(state, ref, start, end, fill_val), this)
}

/// Implements §23.1.3.7 step 12: Repeat, while k < final.
fn fill_generic(
  state: State,
  ref: Ref,
  idx: Int,
  end: Int,
  val: JsValue,
) -> Result(State, #(JsValue, State)) {
  // Step 12: Repeat, while k < final
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
pub fn array_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
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
    False -> #(state, Ok(js_elements.get(elements, idx)))
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
pub fn array_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: If len = 0, return -1
  use <- bool.guard(length == 0, #(state, Ok(js_int(-1))))
  let search = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Steps 4-6: n = ToIntegerOrInfinity(fromIndex), handle ±Infinity
  case args {
    // Step 5: If n = +∞, return -1 (no index >= +∞)
    [_, JsNumber(value.Infinity), ..] -> #(state, Ok(js_int(-1)))
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
      let result =
        search_forward(
          elements,
          start,
          length,
          search,
          value.strict_equal,
          True,
        )
      #(state, Ok(js_int(result)))
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
pub fn array_last_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: If len = 0, return -1
  use <- bool.guard(length == 0, #(state, Ok(js_int(-1))))
  let search = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  // Steps 4-6: fromIndex present → ToIntegerOrInfinity; absent → len - 1
  // Checked by arg COUNT, not value (see MEMORY.md lastIndexOf gotcha).
  case args {
    // Step 6: If n = -∞, return -1
    [_, JsNumber(value.NegInfinity), ..] -> #(state, Ok(js_int(-1)))
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
      let result = search_backward(elements, start, search)
      #(state, Ok(js_int(result)))
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
pub fn array_includes(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: If len = 0, return false
  use <- bool.guard(length == 0, #(state, Ok(JsBool(False))))
  let search = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
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
      let result =
        search_forward(
          elements,
          start,
          length,
          search,
          value.same_value_zero,
          False,
        )
      #(state, Ok(JsBool(result >= 0)))
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
  elements: JsElements,
  idx: Int,
  length: Int,
  search: JsValue,
  eq: fn(JsValue, JsValue) -> Bool,
  skip_holes: Bool,
) -> Int {
  // Loop condition: k < len (both specs)
  case idx >= length {
    // Step 10: return -1 (indexOf) / return false (includes; caller converts)
    True -> -1
    False ->
      // indexOf step 9a: kPresent = HasProperty(O, k) — skip if absent
      case skip_holes && !js_elements.has(elements, idx) {
        True ->
          // indexOf step 9c: k = k + 1 (hole skipped)
          search_forward(elements, idx + 1, length, search, eq, skip_holes)
        False ->
          // indexOf step 9b.i-ii / includes step 9a-b: Get + compare
          case eq(js_elements.get(elements, idx), search) {
            True -> idx
            False ->
              // Step 9c: k = k + 1
              search_forward(elements, idx + 1, length, search, eq, skip_holes)
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
fn search_backward(elements: JsElements, idx: Int, search: JsValue) -> Int {
  // Loop condition: k >= 0
  case idx < 0 {
    // Step 10: return -1
    True -> -1
    False ->
      // Step 9a: kPresent = HasProperty(O, k) — skip holes
      case js_elements.has(elements, idx) {
        False ->
          // Step 9c: k = k - 1 (hole skipped)
          search_backward(elements, idx - 1, search)
        True ->
          // Step 9b.i-ii: elementK = Get(O, k), IsStrictlyEqual check
          case value.strict_equal(js_elements.get(elements, idx), search) {
            True -> idx
            False ->
              // Step 9c: k = k - 1
              search_backward(elements, idx - 1, search)
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
///     implements the HasProperty check (step 5b) via js_elements.has and
///     calls the callback with (kValue, k, O) arguments (step 5c.ii).
///   - Step 6: return undefined.
pub fn array_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: O = ToObject(this), len = LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-5: k = 0; Repeat while k < len (iterate_array handles the loop,
  // HasProperty check via SkipHoles, and Call(callbackfn, thisArg, «kValue, k, O»))
  use _final_elem, _final_idx, state <- iterate_array(
    state,
    elements,
    length,
    cb,
    this_arg,
    this,
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
///   - Step 6c.iii: CreateDataPropertyOrThrow is done via js_elements.set
///     on the accumulator elements (equivalent for dense arrays).
///   - Step 7: finish_array allocates the result array with the collected
///     elements and the original length.
pub fn array_map(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: O = ToObject(this), len = LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-7: ArraySpeciesCreate(O, len), iterate, return A
  // (map_loop does steps 5-6, finish_array does steps 4+7)
  map_loop(state, elements, 0, length, cb, this_arg, this, js_elements.new())
  |> finish_array(length)
}

/// Implements the iteration loop of Array.prototype.map (§23.1.3.19 steps 5-6).
///
/// Steps 5-6: k = 0; Repeat while k < len:
///   6a. Pk = ToString(k)
///   6b. kPresent = HasProperty(O, Pk)
///   6c. If kPresent, then:
///       i.   kValue = Get(O, Pk)
///       ii.  mappedValue = Call(callbackfn, thisArg, « kValue, k, O »)
///       iii. CreateDataPropertyOrThrow(A, Pk, mappedValue)
///   6d. k = k + 1
fn map_loop(
  state: State,
  elements: JsElements,
  idx: Int,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  arr: JsValue,
  acc: JsElements,
) -> #(State, Result(JsElements, JsValue)) {
  // Step 6 loop condition: k < len
  case idx >= length {
    True -> #(state, Ok(acc))
    False ->
      // Step 6b: kPresent = HasProperty(O, Pk)
      case js_elements.has(elements, idx) {
        False ->
          // Hole: kPresent is false, skip (step 6d: k = k + 1)
          map_loop(state, elements, idx + 1, length, cb, this_arg, arr, acc)
        True -> {
          // Step 6c.i: kValue = Get(O, Pk)
          let elem = js_elements.get(elements, idx)
          // Step 6c.ii: mappedValue = Call(callbackfn, thisArg, « kValue, k, O »)
          case frame.call(state, cb, this_arg, [elem, js_int(idx), arr]) {
            Ok(#(result, state)) ->
              // Step 6c.iii: CreateDataPropertyOrThrow(A, Pk, mappedValue)
              // (done via js_elements.set on the accumulator)
              // Step 6d: k = k + 1
              map_loop(
                state,
                elements,
                idx + 1,
                length,
                cb,
                this_arg,
                arr,
                js_elements.set(acc, idx, result),
              )
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
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(length),
            properties: dict.new(),
            elements:,
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
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
pub fn array_filter(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: O = ToObject(this), len = LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: If IsCallable(callbackfn) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-8: ArraySpeciesCreate(O, 0), iterate with filter, return A
  case filter_loop(state, elements, 0, length, cb, this_arg, this, []) {
    #(state, Ok(kept)) -> {
      // Step 8: Return A (allocate from collected values)
      let vals = list.reverse(kept)
      let #(heap, ref) = common.alloc_array(state.heap, vals, array_proto)
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

/// Implements the iteration loop of Array.prototype.filter (§23.1.3.8 steps 5-7).
///
/// Steps 5-7: k = 0, to = 0; Repeat while k < len:
///   7a. Pk = ToString(k)
///   7b. kPresent = HasProperty(O, Pk)
///   7c. If kPresent, then:
///       i.   kValue = Get(O, Pk)
///       ii.  selected = ToBoolean(Call(callbackfn, thisArg, « kValue, k, O »))
///       iii. If selected is true:
///            1. CreateDataPropertyOrThrow(A, ToString(to), kValue)
///            2. to = to + 1
///   7d. k = k + 1
///
/// Note: the "to" index is implicit — kept values are prepended to a list
/// (reversed on return), so their final index equals their insertion order.
fn filter_loop(
  state: State,
  elements: JsElements,
  idx: Int,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  arr: JsValue,
  acc: List(JsValue),
) -> #(State, Result(List(JsValue), JsValue)) {
  // Step 7 loop condition: k < len
  case idx >= length {
    True -> #(state, Ok(acc))
    False ->
      // Step 7b: kPresent = HasProperty(O, Pk)
      case js_elements.has(elements, idx) {
        False ->
          // kPresent is false, skip (step 7d: k = k + 1)
          filter_loop(state, elements, idx + 1, length, cb, this_arg, arr, acc)
        True -> {
          // Step 7c.i: kValue = Get(O, Pk)
          let elem = js_elements.get(elements, idx)
          // Step 7c.ii: selected = ToBoolean(Call(callbackfn, thisArg, « kValue, k, O »))
          case frame.call(state, cb, this_arg, [elem, js_int(idx), arr]) {
            Ok(#(result, state)) -> {
              // Step 7c.iii: If selected is true, keep kValue
              let new_acc = case value.is_truthy(result) {
                // Step 7c.iii.1: CreateDataPropertyOrThrow(A, ToString(to), kValue)
                // Step 7c.iii.2: to = to + 1 (implicit via list prepend)
                True -> [elem, ..acc]
                False -> acc
              }
              // Step 7d: k = k + 1
              filter_loop(
                state,
                elements,
                idx + 1,
                length,
                cb,
                this_arg,
                arr,
                new_acc,
              )
            }
            Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn array_every(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + LengthOfArrayLike (via require_array).
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: IsCallable check (via require_callback).
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-5: k = 0, repeat while k < len.
  // iterate_array handles the loop. SkipHoles implements step 5b (HasProperty).
  // stop_on = !is_truthy implements step 5c.iii (stop when testResult is false).
  use _elem, idx, state <- iterate_array(
    state,
    elements,
    length,
    cb,
    this_arg,
    this,
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
pub fn array_some(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + LengthOfArrayLike (via require_array).
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: IsCallable check (via require_callback).
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-5: k = 0, repeat while k < len.
  // SkipHoles implements step 5b (HasProperty check).
  // stop_on = is_truthy implements step 5c.iii (stop when testResult is true).
  use _elem, idx, state <- iterate_array(
    state,
    elements,
    length,
    cb,
    this_arg,
    this,
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
pub fn array_find(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + LengthOfArrayLike (via require_array).
  use _ref, length, elements, state <- require_array(this, state)
  // FindViaPredicate step 1: IsCallable check (via require_callback).
  use cb, this_arg, state <- require_callback(args, state)
  // FindViaPredicate steps 2-4: iterate [0..len-1] ascending.
  // VisitHoles: step 4b uses Get without HasProperty, so holes → undefined.
  // stop_on = is_truthy: step 4d stops when testResult is true.
  use elem, idx, state <- iterate_array(
    state,
    elements,
    length,
    cb,
    this_arg,
    this,
    VisitHoles,
    value.is_truthy,
  )
  // Step 4: Return findRec.[[Value]].
  // FindViaPredicate step 4d: found → kValue; step 5: not found → undefined.
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
pub fn array_find_index(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + LengthOfArrayLike (via require_array).
  use _ref, length, elements, state <- require_array(this, state)
  // FindViaPredicate step 1: IsCallable check (via require_callback).
  use cb, this_arg, state <- require_callback(args, state)
  // FindViaPredicate steps 2-4: iterate [0..len-1] ascending.
  // VisitHoles: step 4b uses Get without HasProperty, so holes → undefined.
  // stop_on = is_truthy: step 4d stops when testResult is true.
  use _elem, idx, state <- iterate_array(
    state,
    elements,
    length,
    cb,
    this_arg,
    this,
    VisitHoles,
    value.is_truthy,
  )
  // Step 4: Return findRec.[[Index]].
  // FindViaPredicate step 4d: found → 𝔽(k); step 5: not found → -1𝔽.
  case idx < length {
    True -> #(state, Ok(js_int(idx)))
    False -> #(state, Ok(js_int(-1)))
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
pub fn array_sort(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: If comparefn is not undefined and not callable, throw TypeError.
  let comparefn = case args {
    [c, ..] -> c
    [] -> JsUndefined
  }
  case comparefn {
    JsUndefined -> {
      // No comparefn — sort by string conversion (default sort).
      use ref, length, elements, state <- require_array(this, state)
      sort_default(state, ref, length, elements, this)
    }
    _ ->
      case helpers.is_callable(state.heap, comparefn) {
        True -> {
          // comparefn is callable — sort using it for comparisons.
          use ref, length, elements, state <- require_array(this, state)
          sort_with_comparefn(state, ref, length, elements, comparefn, this)
        }
        False ->
          frame.type_error(
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
  elements: JsElements,
  this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  // Collect defined (non-hole) elements. Undefined values go to end.
  let #(defined, undefs) = collect_sort_elements(elements, length, 0, [], 0)
  // Convert each defined element to string for comparison via ToString.
  case stringify_elements(state, defined, []) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(pairs, state)) -> {
      // Sort by string key lexicographically (stable sort).
      let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
      // Write sorted values back, then undefineds, then delete trailing holes.
      let sorted_values = list.map(sorted, fn(pair) { pair.1 })
      let all_values =
        list.append(sorted_values, list.repeat(JsUndefined, undefs))
      wrap(write_sort_result(state, ref, all_values, length, 0), this)
    }
  }
}

/// Collect defined elements from the array for sorting.
/// Returns #(defined_values_reversed, undefined_count).
/// Holes are skipped entirely (not counted). Undefineds are counted separately.
fn collect_sort_elements(
  elements: JsElements,
  length: Int,
  idx: Int,
  acc: List(JsValue),
  undefs: Int,
) -> #(List(JsValue), Int) {
  case idx >= length {
    True -> #(list.reverse(acc), undefs)
    False ->
      case js_elements.has(elements, idx) {
        False ->
          // Hole — skip entirely.
          collect_sort_elements(elements, length, idx + 1, acc, undefs)
        True ->
          case js_elements.get(elements, idx) {
            JsUndefined ->
              // Undefined — count but don't include in sort.
              collect_sort_elements(elements, length, idx + 1, acc, undefs + 1)
            val ->
              collect_sort_elements(
                elements,
                length,
                idx + 1,
                [val, ..acc],
                undefs,
              )
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
    [val, ..rest] ->
      case frame.to_string(state, val) {
        Ok(#(str, state)) ->
          stringify_elements(state, rest, [#(str, val), ..acc])
        Error(err) -> Error(err)
      }
  }
}

/// Sort with a user-provided comparefn. Uses insertion sort since each
/// comparison requires re-entering the VM to call the JS function.
/// Undefined values sort to the end per spec; holes are removed.
fn sort_with_comparefn(
  state: State,
  ref: Ref,
  length: Int,
  elements: JsElements,
  comparefn: JsValue,
  this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  // Collect defined (non-hole) elements. Undefined values go to end.
  let #(defined, undefs) = collect_sort_elements(elements, length, 0, [], 0)
  // Sort using insertion sort with comparefn.
  case insertion_sort(state, defined, comparefn, []) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(sorted, state)) -> {
      let all_values = list.append(sorted, list.repeat(JsUndefined, undefs))
      wrap(write_sort_result(state, ref, all_values, length, 0), this)
    }
  }
}

/// Insertion sort that calls comparefn for each comparison.
/// Processes elements one at a time, inserting each into the correct
/// position in the already-sorted accumulator.
fn insertion_sort(
  state: State,
  remaining: List(JsValue),
  comparefn: JsValue,
  sorted: List(JsValue),
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case remaining {
    [] -> Ok(#(sorted, state))
    [elem, ..rest] ->
      case insert_into_sorted(state, sorted, elem, comparefn, []) {
        Error(err) -> Error(err)
        Ok(#(new_sorted, state)) ->
          insertion_sort(state, rest, comparefn, new_sorted)
      }
  }
}

/// Insert an element into its correct position in a sorted list.
/// Walks through `sorted` comparing `elem` against each entry.
/// When comparefn(elem, entry) <= 0, inserts before that entry.
fn insert_into_sorted(
  state: State,
  sorted: List(JsValue),
  elem: JsValue,
  comparefn: JsValue,
  before: List(JsValue),
) -> Result(#(List(JsValue), State), #(JsValue, State)) {
  case sorted {
    [] ->
      // End of list — insert here.
      Ok(#(list.reverse([elem, ..before]), state))
    [head, ..tail] ->
      // Call comparefn(elem, head) to determine ordering.
      case frame.call(state, comparefn, JsUndefined, [elem, head]) {
        Error(err) -> Error(err)
        Ok(#(result, state)) -> {
          // SortCompare: if result is NaN, treat as +0 (equal).
          let cmp = case result {
            JsNumber(Finite(n)) -> n
            // NaN or non-number → treat as 0 (no swap).
            _ -> 0.0
          }
          case cmp <=. 0.0 {
            // elem <= head: insert elem before head.
            True ->
              Ok(#(
                list.append(list.reverse([elem, ..before]), [head, ..tail]),
                state,
              ))
            // elem > head: keep walking.
            False ->
              insert_into_sorted(state, tail, elem, comparefn, [head, ..before])
          }
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
///   - SkipHoles → Pattern A (HasProperty check via js_elements.has)
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
  elements: JsElements,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  arr: JsValue,
  hole_mode: HoleMode,
  stop_on: fn(JsValue) -> Bool,
  cont: fn(JsValue, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  // Step 4 (Pattern A/B): k = 0, begin loop.
  iterate_loop(
    state,
    elements,
    0,
    length,
    cb,
    this_arg,
    arr,
    hole_mode,
    stop_on,
    cont,
  )
}

/// Inner loop of iterate_array. Each recursive call corresponds to one
/// iteration of the spec's "Repeat, while k < len" (Pattern A) or
/// "For each integer k of indices" (Pattern B).
fn iterate_loop(
  state: State,
  elements: JsElements,
  idx: Int,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  arr: JsValue,
  hole_mode: HoleMode,
  stop_on: fn(JsValue) -> Bool,
  cont: fn(JsValue, Int, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  // Loop termination: k >= len → completed without early exit.
  // Pattern A step 6 / Pattern B step 5.
  case idx >= length {
    True -> cont(JsUndefined, length, state)
    False -> {
      // Pattern A step 5b: kPresent = HasProperty(O, Pk).
      // Pattern B: no HasProperty check, always visits.
      let should_visit = case hole_mode {
        VisitHoles -> True
        SkipHoles -> js_elements.has(elements, idx)
      }
      case should_visit {
        // Pattern A step 5b-c: kPresent is false → skip, advance k.
        False ->
          iterate_loop(
            state,
            elements,
            idx + 1,
            length,
            cb,
            this_arg,
            arr,
            hole_mode,
            stop_on,
            cont,
          )
        True -> {
          // Pattern A step 5c.i / Pattern B step 4b: kValue = Get(O, Pk).
          // For holes with VisitHoles, js_elements.get returns JsUndefined.
          let elem = js_elements.get(elements, idx)
          // Pattern A step 5c.ii / Pattern B step 4c:
          // testResult = ToBoolean(Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
          case frame.call(state, cb, this_arg, [elem, js_int(idx), arr]) {
            // Abrupt completion from callback → propagate.
            Error(#(thrown, state)) -> #(state, Error(thrown))
            Ok(#(result, state)) ->
              // Pattern A step 5c.iii / Pattern B step 4d:
              // If testResult matches stop condition, return early.
              case stop_on(result) {
                True -> cont(elem, idx, state)
                // Step 5d / continue to next k.
                False ->
                  iterate_loop(
                    state,
                    elements,
                    idx + 1,
                    length,
                    cb,
                    this_arg,
                    arr,
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
pub fn array_reduce(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: Let O be ? ToObject(this value). Let len be ? LengthOfArrayLike(O).
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3 setup: extract callbackfn argument
  let cb = case args {
    [c, ..] -> c
    [] -> JsUndefined
  }
  // Step 3: If IsCallable(callbackfn) is false, throw a TypeError exception.
  use <- bool.guard(
    !helpers.is_callable(state.heap, cb),
    frame.type_error(
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
    True -> reduce_loop(state, elements, 0, length, cb, this, init, 1)
    False ->
      // Steps 8a-8c: Find first present element as initial accumulator.
      // find_present implements step 8b's loop: scan forward for HasProperty=true.
      case find_present(elements, 0, length, 1) {
        // Step 4/8c: If len=0 or no present element found, throw TypeError.
        None ->
          frame.type_error(state, "Reduce of empty array with no initial value")
        // Step 8b.iii: Set accumulator to Get(O, Pk), then k = k + 1.
        // Step 9: Enter main loop starting at first_idx + 1.
        Some(#(first_idx, first_val)) ->
          reduce_loop(
            state,
            elements,
            first_idx + 1,
            length,
            cb,
            this,
            first_val,
            1,
          )
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
pub fn array_reduce_right(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: Let O be ? ToObject(this value). Let len be ? LengthOfArrayLike(O).
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3 setup: extract callbackfn argument
  let cb = case args {
    [c, ..] -> c
    [] -> JsUndefined
  }
  // Step 3: If IsCallable(callbackfn) is false, throw a TypeError exception.
  use <- bool.guard(
    !helpers.is_callable(state.heap, cb),
    frame.type_error(
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
    True -> reduce_loop(state, elements, length - 1, -1, cb, this, init, -1)
    False ->
      // Steps 8a-8c: Find last present element as initial accumulator.
      // find_present with step=-1 scans backward (step 8b: while k ≥ 0).
      case find_present(elements, length - 1, -1, -1) {
        // Step 4/8c: If len=0 or no present element found, throw TypeError.
        None ->
          frame.type_error(state, "Reduce of empty array with no initial value")
        // Step 8b.iii: Set accumulator to Get(O, Pk), then k = k - 1.
        // Step 9: Enter main loop starting at first_idx - 1 (backward).
        Some(#(first_idx, first_val)) ->
          reduce_loop(
            state,
            elements,
            first_idx - 1,
            -1,
            cb,
            this,
            first_val,
            -1,
          )
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
/// Step 8b.ii: Set kPresent to ? HasProperty(O, Pk) — implemented via js_elements.has.
/// Step 8b.iii.1: Set accumulator to ? Get(O, Pk) — implemented via js_elements.get.
/// Step 8b.iv: Set k to k ± 1 — implemented by idx + step.
///
/// Returns Ok(#(index, value)) on the first present element found (step 8b.iii),
/// or Error(Nil) if no present element exists (triggering step 8c TypeError).
fn find_present(
  elements: JsElements,
  idx: Int,
  end: Int,
  step: Int,
) -> Option(#(Int, JsValue)) {
  // Loop termination: k < len (forward) or k ≥ 0 (backward)
  case idx == end {
    // Step 8c: kPresent is false after exhausting all indices
    True -> None
    False ->
      // Step 8b.ii: Set kPresent to ? HasProperty(O, Pk)
      case js_elements.has(elements, idx) {
        // Step 8b.iii: kPresent is true — set accumulator to Get(O, Pk)
        True -> Some(#(idx, js_elements.get(elements, idx)))
        // Step 8b.iv: Set k to k + 1 (or k - 1 for reduceRight)
        False -> find_present(elements, idx + step, end, step)
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
/// TODO(Deviation): Reads directly from JsElements instead of HasProperty + Get on a
/// generic object O. Equivalent for Array exotic objects but would differ for
/// generic array-like objects passed via .call/.apply (getters on the proto
/// chain would be missed).
fn reduce_loop(
  state: State,
  elements: JsElements,
  idx: Int,
  end: Int,
  cb: JsValue,
  arr: JsValue,
  acc: JsValue,
  step: Int,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 9 loop condition: k < len (forward) or k ≥ 0 (backward)
  case idx == end {
    // Step 10: Return accumulator.
    True -> #(state, Ok(acc))
    False ->
      // Step 9b: Let kPresent be ? HasProperty(O, Pk).
      case js_elements.has(elements, idx) {
        // kPresent is false — skip this index (hole).
        // Step 9d: Set k to k + 1 (or k - 1 for reduceRight).
        False ->
          reduce_loop(state, elements, idx + step, end, cb, arr, acc, step)
        // Step 9c: If kPresent is true, then
        True -> {
          // Step 9c.i: Let kValue be ? Get(O, Pk).
          let elem = js_elements.get(elements, idx)
          // Step 9c.ii: Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
          // Note: thisArg is always undefined for reduce/reduceRight (no thisArg parameter).
          use result, state <- frame.try_call(state, cb, JsUndefined, [
            acc,
            elem,
            js_int(idx),
            arr,
          ])
          // Step 9d: Set k to k + 1 (or k - 1 for reduceRight).
          reduce_loop(state, elements, idx + step, end, cb, arr, result, step)
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
pub fn array_splice(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use ref, length, elements, state <- require_array(this, state)
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
  let removed_elements =
    copy_range(
      elements,
      actual_start,
      0,
      actual_delete_count,
      js_elements.new(),
    )
  let #(heap, removed_ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(actual_delete_count),
        properties: dict.new(),
        elements: removed_elements,
        prototype: Some(array_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  let state = State(..state, heap:)
  let removed_arr = JsObject(removed_ref)
  // Steps 12-17: Shift elements and insert items.
  let item_count = list.length(items)
  let shift = item_count - actual_delete_count
  // If shift > 0: we need to move elements right. If shift < 0: move left.
  // If shift == 0: no shifting needed.
  case
    splice_shift(state, ref, actual_start, actual_delete_count, length, shift)
  {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(state) ->
      // Step 15: Insert items at actualStart.
      case splice_insert(state, ref, actual_start, items) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(state) -> {
          // Step 17: Set length.
          let new_length = length + shift
          wrap(generic_set_length(state, ref, new_length), removed_arr)
        }
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
pub fn array_find_last(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + LengthOfArrayLike
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: If IsCallable(predicate) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // FindViaPredicate with descending direction: iterate [len-1..0]
  find_last_loop(state, elements, length - 1, cb, this_arg, this, True)
}

/// Array.prototype.findLastIndex ( predicate [ , thisArg ] )
/// ES2024 §23.1.3.12
///
/// Like findIndex() but searches from end to start.
pub fn array_find_last_index(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject + LengthOfArrayLike
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: If IsCallable(predicate) is false, throw TypeError
  use cb, this_arg, state <- require_callback(args, state)
  // FindViaPredicate with descending direction
  find_last_loop(state, elements, length - 1, cb, this_arg, this, False)
}

/// Backward iteration loop for findLast/findLastIndex.
/// FindViaPredicate (§23.1.3.9.1) with descending direction.
///
/// When return_value is True, returns the element (findLast).
/// When return_value is False, returns the index (findLastIndex).
fn find_last_loop(
  state: State,
  elements: JsElements,
  idx: Int,
  cb: JsValue,
  this_arg: JsValue,
  arr: JsValue,
  return_value: Bool,
) -> #(State, Result(JsValue, JsValue)) {
  case idx < 0 {
    // Loop completed without finding: findLast returns undefined, findLastIndex returns -1
    True ->
      case return_value {
        True -> #(state, Ok(JsUndefined))
        False -> #(state, Ok(js_int(-1)))
      }
    False -> {
      // FindViaPredicate step 4b: kValue = Get(O, Pk)
      // (holes become undefined, like find/findIndex)
      let elem = js_elements.get(elements, idx)
      // Step 4c: testResult = ToBoolean(Call(predicate, thisArg, « kValue, 𝔽(k), O »))
      case frame.call(state, cb, this_arg, [elem, js_int(idx), arr]) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(result, state)) ->
          case value.is_truthy(result) {
            // Step 4d: testResult is true → return
            True ->
              case return_value {
                True -> #(state, Ok(elem))
                False -> #(state, Ok(js_int(idx)))
              }
            // Continue to previous index
            False ->
              find_last_loop(
                state,
                elements,
                idx - 1,
                cb,
                this_arg,
                arr,
                return_value,
              )
          }
      }
    }
  }
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
pub fn array_flat(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject + LengthOfArrayLike
  use _ref, length, elements, state <- require_array(this, state)
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
  // Steps 6-7: FlattenIntoArray
  case flatten_into(state, elements, length, depth, []) {
    #(state, Ok(flat_list)) -> {
      let vals = list.reverse(flat_list)
      let count = list.length(vals)
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(count),
            properties: dict.new(),
            elements: js_elements.from_list(vals),
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

/// FlattenIntoArray (ES2024 §23.1.3.13.1)
///
/// Recursively flattens array elements up to the given depth.
/// Returns elements in reverse order (caller must reverse).
fn flatten_into(
  state: State,
  elements: JsElements,
  length: Int,
  depth: Int,
  acc: List(JsValue),
) -> #(State, Result(List(JsValue), JsValue)) {
  flatten_into_loop(state, elements, 0, length, depth, acc)
}

fn flatten_into_loop(
  state: State,
  elements: JsElements,
  idx: Int,
  length: Int,
  depth: Int,
  acc: List(JsValue),
) -> #(State, Result(List(JsValue), JsValue)) {
  case idx >= length {
    True -> #(state, Ok(acc))
    False ->
      case js_elements.has(elements, idx) {
        False ->
          // Hole: skip
          flatten_into_loop(state, elements, idx + 1, length, depth, acc)
        True -> {
          let elem = js_elements.get(elements, idx)
          // If depth > 0 and element is an array, recursively flatten
          case depth > 0 {
            True ->
              case is_array_value(elem, state.heap) {
                Some(#(sub_len, sub_elements)) -> {
                  // Recurse with depth - 1
                  case
                    flatten_into(state, sub_elements, sub_len, depth - 1, acc)
                  {
                    #(state, Ok(new_acc)) ->
                      flatten_into_loop(
                        state,
                        elements,
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
                  flatten_into_loop(state, elements, idx + 1, length, depth, [
                    elem,
                    ..acc
                  ])
              }
            False ->
              // Depth is 0, just append
              flatten_into_loop(state, elements, idx + 1, length, depth, [
                elem,
                ..acc
              ])
          }
        }
      }
  }
}

/// Check if a JsValue is an array, returning its length and elements if so.
fn is_array_value(val: JsValue, h: Heap) -> Option(#(Int, JsElements)) {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) ->
          Some(#(length, elements))
        _ -> None
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
pub fn array_flat_map(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject + LengthOfArrayLike
  use _ref, length, elements, state <- require_array(this, state)
  // Step 3: IsCallable check
  use cb, this_arg, state <- require_callback(args, state)
  // Steps 4-6: Map each element, then flatten depth=1
  case flat_map_loop(state, elements, 0, length, cb, this_arg, this, []) {
    #(state, Ok(flat_list)) -> {
      let vals = list.reverse(flat_list)
      let count = list.length(vals)
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(count),
            properties: dict.new(),
            elements: js_elements.from_list(vals),
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

/// flatMap iteration loop: call mapper on each element, then flatten
/// the result one level deep.
fn flat_map_loop(
  state: State,
  elements: JsElements,
  idx: Int,
  length: Int,
  cb: JsValue,
  this_arg: JsValue,
  arr: JsValue,
  acc: List(JsValue),
) -> #(State, Result(List(JsValue), JsValue)) {
  case idx >= length {
    True -> #(state, Ok(acc))
    False ->
      case js_elements.has(elements, idx) {
        False ->
          // Hole: skip
          flat_map_loop(
            state,
            elements,
            idx + 1,
            length,
            cb,
            this_arg,
            arr,
            acc,
          )
        True -> {
          let elem = js_elements.get(elements, idx)
          // Call mapper: mapperFunction(kValue, k, O)
          case frame.call(state, cb, this_arg, [elem, js_int(idx), arr]) {
            Error(#(thrown, state)) -> #(state, Error(thrown))
            Ok(#(mapped, state)) ->
              // Flatten the result one level: if it's an array, spread its elements
              case is_array_value(mapped, state.heap) {
                Some(#(sub_len, sub_elements)) -> {
                  // Collect all elements from the sub-array
                  let new_acc = collect_flat(sub_elements, 0, sub_len, acc)
                  flat_map_loop(
                    state,
                    elements,
                    idx + 1,
                    length,
                    cb,
                    this_arg,
                    arr,
                    new_acc,
                  )
                }
                None ->
                  // Not an array — add directly
                  flat_map_loop(
                    state,
                    elements,
                    idx + 1,
                    length,
                    cb,
                    this_arg,
                    arr,
                    [mapped, ..acc],
                  )
              }
          }
        }
      }
  }
}

/// Collect elements from a sub-array into the accumulator (in reverse order).
fn collect_flat(
  elements: JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> acc
    False ->
      case js_elements.has(elements, idx) {
        True ->
          collect_flat(elements, idx + 1, length, [
            js_elements.get(elements, idx),
            ..acc
          ])
        False ->
          // Preserve holes as undefined for flatMap
          collect_flat(elements, idx + 1, length, [JsUndefined, ..acc])
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
pub fn array_copy_within(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use ref, length, _elements, state <- require_array(this, state)
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
pub fn array_from(
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
              frame.type_error(
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
fn array_from_array_like(
  items: JsValue,
  map_fn: Option(JsValue),
  this_arg: JsValue,
  array_proto: Ref,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Get items as array-like: read length and elements
  case items {
    JsNull | JsUndefined ->
      frame.type_error(
        state,
        "Cannot create array from " <> common.typeof_value(items, state.heap),
      )
    JsString(s) -> {
      // String is iterable — convert each character
      let #(length, elements) = string_to_elements(s)
      case map_fn {
        None -> {
          let #(heap, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: ArrayObject(length),
                properties: dict.new(),
                elements:,
                prototype: Some(array_proto),
                symbol_properties: dict.new(),
                extensible: True,
              ),
            )
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
        Some(mf) ->
          array_from_mapped_loop(
            state,
            elements,
            0,
            length,
            mf,
            this_arg,
            array_proto,
            [],
          )
      }
    }
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) ->
          case map_fn {
            None -> {
              // Fast path: copy elements directly
              let copied = copy_range(elements, 0, 0, length, js_elements.new())
              let #(heap, new_ref) =
                heap.alloc(
                  state.heap,
                  ObjectSlot(
                    kind: ArrayObject(length),
                    properties: dict.new(),
                    elements: copied,
                    prototype: Some(array_proto),
                    symbol_properties: dict.new(),
                    extensible: True,
                  ),
                )
              #(State(..state, heap:), Ok(JsObject(new_ref)))
            }
            Some(mf) ->
              array_from_mapped_loop(
                state,
                elements,
                0,
                length,
                mf,
                this_arg,
                array_proto,
                [],
              )
          }
        Some(ObjectSlot(properties:, elements:, ..)) -> {
          // Generic array-like: read length property
          let length = read_length_property(properties)
          let elements = gather_indexed(properties, elements, length)
          case map_fn {
            None -> {
              let #(heap, new_ref) =
                heap.alloc(
                  state.heap,
                  ObjectSlot(
                    kind: ArrayObject(length),
                    properties: dict.new(),
                    elements:,
                    prototype: Some(array_proto),
                    symbol_properties: dict.new(),
                    extensible: True,
                  ),
                )
              #(State(..state, heap:), Ok(JsObject(new_ref)))
            }
            Some(mf) ->
              array_from_mapped_loop(
                state,
                elements,
                0,
                length,
                mf,
                this_arg,
                array_proto,
                [],
              )
          }
        }
        _ -> {
          // Not a valid object — return empty array
          let #(heap, ref2) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: ArrayObject(0),
                properties: dict.new(),
                elements: js_elements.new(),
                prototype: Some(array_proto),
                symbol_properties: dict.new(),
                extensible: True,
              ),
            )
          #(State(..state, heap:), Ok(JsObject(ref2)))
        }
      }
    // Primitives (number, boolean, etc.) — return empty array
    _ -> {
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(0),
            properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// Array.from with mapping function: iterate elements, apply mapFn to each.
fn array_from_mapped_loop(
  state: State,
  elements: JsElements,
  idx: Int,
  length: Int,
  map_fn: JsValue,
  this_arg: JsValue,
  array_proto: Ref,
  acc: List(JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  case idx >= length {
    True -> {
      let vals = list.reverse(acc)
      let count = list.length(vals)
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(count),
            properties: dict.new(),
            elements: js_elements.from_list(vals),
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    False -> {
      let elem = js_elements.get(elements, idx)
      // Call mapFn(element, index)
      case frame.call(state, map_fn, this_arg, [elem, js_int(idx)]) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(mapped, state)) ->
          array_from_mapped_loop(
            state,
            elements,
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

// ============================================================================
// Array.of (ES2024 §23.1.2.3) — static method
// ============================================================================

/// Array.of ( ...items )
/// ES2024 §23.1.2.3
///
/// Creates a new Array instance from a variable number of arguments,
/// regardless of number or type of the arguments.
/// Unlike Array(), Array.of(7) creates [7] not an array with length 7.
pub fn array_of(
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
        elements: js_elements.from_list(args),
        prototype: Some(array_proto),
        symbol_properties: dict.new(),
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
pub fn array_to_spliced(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
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
  let new_elements = copy_range(elements, 0, 0, actual_start, js_elements.new())
  // Insert items at actualStart
  let new_elements = insert_items(new_elements, actual_start, items)
  // Copy [actualStart + actualSkipCount, length) from source
  let src_from = actual_start + actual_skip_count
  let dst_from = actual_start + item_count
  let remaining = length - src_from
  let new_elements =
    copy_range(elements, src_from, dst_from, remaining, new_elements)
  // Step 15: Return A
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(new_len),
        properties: dict.new(),
        elements: new_elements,
        prototype: Some(array_proto),
        symbol_properties: dict.new(),
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
      insert_items(js_elements.set(elements, start, item), start + 1, rest)
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
pub fn array_with(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1-2: ToObject(this), LengthOfArrayLike(O)
  use _ref, length, elements, state <- require_array(this, state)
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
    True -> frame.range_error(state, "Invalid index")
    False -> {
      // Get the replacement value
      let replacement = case args {
        [_, v, ..] -> v
        _ -> JsUndefined
      }
      // Steps 7-11: Copy all elements, replacing actualIndex with value
      let new_elements = copy_range(elements, 0, 0, length, js_elements.new())
      let new_elements =
        js_elements.set(new_elements, actual_index, replacement)
      // Step 12: Return A
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(length),
            properties: dict.new(),
            elements: new_elements,
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
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
pub fn array_to_sorted(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let comparefn = case args {
    [c, ..] -> c
    [] -> JsUndefined
  }
  case comparefn {
    JsUndefined -> {
      use _ref, length, elements, state <- require_array(this, state)
      to_sorted_default(state, length, elements)
    }
    _ ->
      case helpers.is_callable(state.heap, comparefn) {
        True -> {
          use _ref, length, elements, state <- require_array(this, state)
          to_sorted_with_comparefn(state, length, elements, comparefn)
        }
        False ->
          frame.type_error(
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
  elements: JsElements,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  let #(defined, undefs) = collect_sort_elements(elements, length, 0, [], 0)
  case stringify_elements(state, defined, []) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(pairs, state)) -> {
      let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
      let sorted_values = list.map(sorted, fn(pair) { pair.1 })
      let all_values =
        list.append(sorted_values, list.repeat(JsUndefined, undefs))
      let new_elements =
        build_elements_from_list(all_values, 0, js_elements.new())
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(length),
            properties: dict.new(),
            elements: new_elements,
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// Comparefn sort for toSorted: collect elements, insertion-sort using comparefn,
/// then build a new array from the result.
fn to_sorted_with_comparefn(
  state: State,
  length: Int,
  elements: JsElements,
  comparefn: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  let #(defined, undefs) = collect_sort_elements(elements, length, 0, [], 0)
  case insertion_sort(state, defined, comparefn, []) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(sorted, state)) -> {
      let all_values = list.append(sorted, list.repeat(JsUndefined, undefs))
      let new_elements =
        build_elements_from_list(all_values, 0, js_elements.new())
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: ArrayObject(length),
            properties: dict.new(),
            elements: new_elements,
            prototype: Some(array_proto),
            symbol_properties: dict.new(),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
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
      build_elements_from_list(rest, idx + 1, js_elements.set(acc, idx, val))
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
pub fn array_to_reversed(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  use _ref, length, elements, state <- require_array(this, state)
  // Collect all elements; holes become undefined (spec step 5c: Get returns undefined for holes).
  let all_values = collect_all_elements(elements, length, 0, [])
  // Reverse the collected list.
  let reversed = list.reverse(all_values)
  let new_elements = build_elements_from_list(reversed, 0, js_elements.new())
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(length),
        properties: dict.new(),
        elements: new_elements,
        prototype: Some(array_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Collect all elements at indices [0, length), reading holes as JsUndefined.
/// Per §23.1.3.33 step 5c: Get(O, from) — holes are treated as undefined.
fn collect_all_elements(
  elements: JsElements,
  length: Int,
  idx: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False ->
      collect_all_elements(elements, length, idx + 1, [
        js_elements.get(elements, idx),
        ..acc
      ])
  }
}
