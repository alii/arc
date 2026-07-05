import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/builtins/regexp_ops
import arc/vm/builtins/substitution
import arc/vm/heap
import arc/vm/js_string
import arc/vm/key
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/unicode_case
import arc/vm/value.{
  type JsValue, type Ref, type StringNativeFn, Finite, JsNull, JsNumber,
  JsObject, JsString, JsUndefined, NaN, StringFromCharCode, StringFromCodePoint,
  StringNative, StringPrototypeAnchor, StringPrototypeAt, StringPrototypeBig,
  StringPrototypeBlink, StringPrototypeBold, StringPrototypeCharAt,
  StringPrototypeCharCodeAt, StringPrototypeCodePointAt, StringPrototypeConcat,
  StringPrototypeEndsWith, StringPrototypeFixed, StringPrototypeFontcolor,
  StringPrototypeFontsize, StringPrototypeIncludes, StringPrototypeIndexOf,
  StringPrototypeIsWellFormed, StringPrototypeItalics,
  StringPrototypeLastIndexOf, StringPrototypeLink, StringPrototypeLocaleCompare,
  StringPrototypeMatch, StringPrototypeMatchAll, StringPrototypeNormalize,
  StringPrototypePadEnd, StringPrototypePadStart, StringPrototypeRepeat,
  StringPrototypeReplace, StringPrototypeReplaceAll, StringPrototypeSearch,
  StringPrototypeSlice, StringPrototypeSmall, StringPrototypeSplit,
  StringPrototypeStartsWith, StringPrototypeStrike, StringPrototypeSub,
  StringPrototypeSubstr, StringPrototypeSubstring, StringPrototypeSup,
  StringPrototypeSymbolIterator, StringPrototypeToLocaleLowerCase,
  StringPrototypeToLocaleUpperCase, StringPrototypeToLowerCase,
  StringPrototypeToString, StringPrototypeToUpperCase,
  StringPrototypeToWellFormed, StringPrototypeTrim, StringPrototypeTrimEnd,
  StringPrototypeTrimStart, StringPrototypeValueOf, StringRaw,
}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result
import gleam/string

/// Set up String constructor + String.prototype.
/// ES2024 22.1.2 — Properties of the String Constructor
/// ES2024 22.1.3 — Properties of the String Prototype Object
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("charAt", StringNative(StringPrototypeCharAt), 1),
      #("charCodeAt", StringNative(StringPrototypeCharCodeAt), 1),
      #("indexOf", StringNative(StringPrototypeIndexOf), 1),
      #("lastIndexOf", StringNative(StringPrototypeLastIndexOf), 1),
      #("includes", StringNative(StringPrototypeIncludes), 1),
      #("startsWith", StringNative(StringPrototypeStartsWith), 1),
      #("endsWith", StringNative(StringPrototypeEndsWith), 1),
      #("slice", StringNative(StringPrototypeSlice), 2),
      #("substring", StringNative(StringPrototypeSubstring), 2),
      #("toLowerCase", StringNative(StringPrototypeToLowerCase), 0),
      #("toUpperCase", StringNative(StringPrototypeToUpperCase), 0),
      #("toLocaleLowerCase", StringNative(StringPrototypeToLocaleLowerCase), 0),
      #("toLocaleUpperCase", StringNative(StringPrototypeToLocaleUpperCase), 0),
      #("trim", StringNative(StringPrototypeTrim), 0),
      #("trimStart", StringNative(StringPrototypeTrimStart), 0),
      #("trimEnd", StringNative(StringPrototypeTrimEnd), 0),
      #("trimLeft", StringNative(StringPrototypeTrimStart), 0),
      #("trimRight", StringNative(StringPrototypeTrimEnd), 0),
      #("split", StringNative(StringPrototypeSplit), 2),
      #("concat", StringNative(StringPrototypeConcat), 1),
      #("toString", StringNative(StringPrototypeToString), 0),
      #("valueOf", StringNative(StringPrototypeValueOf), 0),
      #("repeat", StringNative(StringPrototypeRepeat), 1),
      #("padStart", StringNative(StringPrototypePadStart), 1),
      #("padEnd", StringNative(StringPrototypePadEnd), 1),
      #("at", StringNative(StringPrototypeAt), 1),
      #("codePointAt", StringNative(StringPrototypeCodePointAt), 1),
      #("normalize", StringNative(StringPrototypeNormalize), 0),
      #("match", StringNative(StringPrototypeMatch), 1),
      #("search", StringNative(StringPrototypeSearch), 1),
      #("replace", StringNative(StringPrototypeReplace), 2),
      #("replaceAll", StringNative(StringPrototypeReplaceAll), 2),
      #("substr", StringNative(StringPrototypeSubstr), 2),
      #("localeCompare", StringNative(StringPrototypeLocaleCompare), 1),
      #("matchAll", StringNative(StringPrototypeMatchAll), 1),
      #("isWellFormed", StringNative(StringPrototypeIsWellFormed), 0),
      #("toWellFormed", StringNative(StringPrototypeToWellFormed), 0),
      // Annex B HTML wrapper methods
      #("anchor", StringNative(StringPrototypeAnchor), 1),
      #("big", StringNative(StringPrototypeBig), 0),
      #("blink", StringNative(StringPrototypeBlink), 0),
      #("bold", StringNative(StringPrototypeBold), 0),
      #("fixed", StringNative(StringPrototypeFixed), 0),
      #("fontcolor", StringNative(StringPrototypeFontcolor), 1),
      #("fontsize", StringNative(StringPrototypeFontsize), 1),
      #("italics", StringNative(StringPrototypeItalics), 0),
      #("link", StringNative(StringPrototypeLink), 1),
      #("small", StringNative(StringPrototypeSmall), 0),
      #("strike", StringNative(StringPrototypeStrike), 0),
      #("sub", StringNative(StringPrototypeSub), 0),
      #("sup", StringNative(StringPrototypeSup), 0),
    ])
  // Static methods on the String constructor
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("raw", StringNative(StringRaw), 1),
      #("fromCharCode", StringNative(StringFromCharCode), 1),
      #("fromCodePoint", StringNative(StringFromCodePoint), 1),
    ])
  // Note: StringConstructor stays VM-level (needs ToPrimitive/ToString)
  // ES2024 §22.1.3: the String prototype object is itself a String exotic
  // object, with a [[StringData]] internal slot whose value is "".
  let #(h, bt) =
    common.init_wrapper_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(_) { value.Call(value.StringConstructor) },
      "String",
      1,
      static_methods,
      proto_kind: value.StringObject(value: ""),
    )
  // §22.1.3.36 String.prototype [ @@iterator ] ( ) — yields code points.
  let #(h, iter_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      StringNative(StringPrototypeSymbolIterator),
      "[Symbol.iterator]",
      0,
    )
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_iterator,
      value.builtin_property(JsObject(iter_fn)),
    )
  #(h, bt)
}

/// Per-module dispatch for String native functions.
pub fn dispatch(
  native: StringNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    StringPrototypeSymbolIterator -> string_symbol_iterator(this, state)
    StringPrototypeCharAt -> string_char_at(this, args, state)
    StringPrototypeCharCodeAt -> string_char_code_at(this, args, state)
    StringPrototypeIndexOf -> string_index_of(this, args, state)
    StringPrototypeLastIndexOf -> string_last_index_of(this, args, state)
    StringPrototypeIncludes -> string_includes(this, args, state)
    StringPrototypeStartsWith -> string_starts_with(this, args, state)
    StringPrototypeEndsWith -> string_ends_with(this, args, state)
    StringPrototypeSlice -> string_slice(this, args, state)
    StringPrototypeSubstring -> string_substring(this, args, state)
    StringPrototypeToLowerCase | StringPrototypeToLocaleLowerCase ->
      string_to_lower_case(this, args, state)
    StringPrototypeToUpperCase | StringPrototypeToLocaleUpperCase ->
      string_to_upper_case(this, args, state)
    StringPrototypeTrim -> string_trim(this, args, state)
    StringPrototypeTrimStart -> string_trim_start(this, args, state)
    StringPrototypeTrimEnd -> string_trim_end(this, args, state)
    StringPrototypeSplit -> string_split(this, args, state)
    StringPrototypeConcat -> string_concat(this, args, state)
    StringPrototypeToString -> string_to_string(this, args, state)
    StringPrototypeValueOf -> string_value_of(this, args, state)
    StringPrototypeRepeat -> string_repeat(this, args, state)
    StringPrototypePadStart -> string_pad_start(this, args, state)
    StringPrototypePadEnd -> string_pad_end(this, args, state)
    StringPrototypeAt -> string_at(this, args, state)
    StringPrototypeCodePointAt -> string_code_point_at(this, args, state)
    StringPrototypeNormalize -> string_normalize(this, args, state)
    StringPrototypeMatch -> string_match(this, args, state)
    StringPrototypeSearch -> string_search(this, args, state)
    StringPrototypeReplace -> string_replace(this, args, state)
    StringPrototypeReplaceAll -> string_replace_all(this, args, state)
    StringPrototypeSubstr -> string_substr(this, args, state)
    StringPrototypeLocaleCompare -> string_locale_compare(this, args, state)
    StringPrototypeMatchAll -> string_match_all(this, args, state)
    StringPrototypeIsWellFormed -> string_is_well_formed(this, state)
    StringPrototypeToWellFormed -> string_to_well_formed(this, state)
    // Annex B HTML wrapper methods
    StringPrototypeAnchor -> html_wrap_attr(this, args, state, "a", "name")
    StringPrototypeBig -> html_wrap(this, state, "big")
    StringPrototypeBlink -> html_wrap(this, state, "blink")
    StringPrototypeBold -> html_wrap(this, state, "b")
    StringPrototypeFixed -> html_wrap(this, state, "tt")
    StringPrototypeFontcolor ->
      html_wrap_attr(this, args, state, "font", "color")
    StringPrototypeFontsize -> html_wrap_attr(this, args, state, "font", "size")
    StringPrototypeItalics -> html_wrap(this, state, "i")
    StringPrototypeLink -> html_wrap_attr(this, args, state, "a", "href")
    StringPrototypeSmall -> html_wrap(this, state, "small")
    StringPrototypeStrike -> html_wrap(this, state, "strike")
    StringPrototypeSub -> html_wrap(this, state, "sub")
    StringPrototypeSup -> html_wrap(this, state, "sup")
    // Static methods
    StringRaw -> string_raw(args, state)
    StringFromCharCode -> string_from_char_code(args, state)
    StringFromCodePoint -> string_from_code_point(args, state)
  }
}

// ============================================================================
// String.prototype method implementations
// ============================================================================

/// ES2024 §22.1.3.36 — String.prototype [ @@iterator ] ( )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let s be ? ToString(O).
///   3. Return a String Iterator over s's code points.
/// The iterator object shares %ArrayIteratorPrototype% — the same shape the
/// VM's GetIterator string fast path allocates, so its .next() handles it.
fn string_symbol_iterator(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use s, state <- with_this_string(this, state)
  let #(heap, iter_ref) =
    common.alloc_wrapper(
      state.heap,
      value.StringIteratorObject(remaining: string.to_utf_codepoints(s)),
      state.builtins.array_iterator_proto,
    )
  #(State(..state, heap:), Ok(JsObject(iter_ref)))
}

/// ES2024 22.1.3.1 — String.prototype.charAt ( pos )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let position be ? ToIntegerOrInfinity(pos).
///   4. Let size be the length of S.
///   5. If position < 0 or position >= size, return the empty String.
///   6. Return the substring of S from position to position + 1.
///
fn string_char_at(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 3: ToIntegerOrInfinity(pos)
  use idx, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Steps 4-6: bounds check built into FFI, returns None if out of range
  case idx >= 0 {
    True ->
      case js_string.char_at(s, idx) {
        Some(ch) -> #(state, Ok(JsString(ch)))
        None -> #(state, Ok(JsString("")))
      }
    False -> #(state, Ok(JsString("")))
  }
}

/// ES2024 22.1.3.2 — String.prototype.charCodeAt ( pos )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let position be ? ToIntegerOrInfinity(pos).
///   4. Let size be the length of S.
///   5. If position < 0 or position >= size, return NaN.
///   6. Return the Number value for the code unit at index position
///      within S.
///
/// TODO(Deviation): Uses UTF codepoint extraction rather than UTF-16 code unit.
/// For BMP characters this is equivalent, but for supplementary chars
/// (U+10000+) this returns the full codepoint rather than the leading
/// surrogate. Needs UTF-16 surrogate pair splitting.
fn string_char_code_at(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 3: ToIntegerOrInfinity(pos)
  use idx, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Steps 4-6: bounds check built into FFI, returns None if out of range
  let ch = case idx >= 0 {
    True -> js_string.char_at(s, idx)
    False -> None
  }
  case ch {
    Some(ch) ->
      case string.to_utf_codepoints(ch) {
        [cp, ..] -> {
          let code = string.utf_codepoint_to_int(cp)
          #(state, Ok(value.from_int(code)))
        }
        [] -> #(state, Ok(JsNumber(NaN)))
      }
    None -> #(state, Ok(JsNumber(NaN)))
  }
}

/// ES2024 22.1.3.9 — String.prototype.indexOf ( searchString [ , position ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let searchStr be ? ToString(searchString).
///   4. Let pos be ? ToIntegerOrInfinity(position).
///   5. Assert: If position is undefined, then pos is 0.
///   6. Let len be the length of S.
///   7. Let start be the result of clamping pos between 0 and len.
///   8. Return StringIndexOf(S, searchStr, start).
///
fn string_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 3: ToString(searchString)
  use search, state <- coerce.try_to_string(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Steps 4-7: ToIntegerOrInfinity(position), clamped to a valid start index
  // in [0, len] (the step-7 clamp): NaN/±0 → 0, +∞ → len, -∞ → 0. This goes
  // through ToNumber, so an object position argument (e.g.
  // `{valueOf(){return 1}}`) is coerced via valueOf/@@toPrimitive rather than
  // silently falling back to 0.
  let pos_val = case helpers.list_at(args, 1) {
    Some(v) -> v
    None -> JsUndefined
  }
  use pos, state <- coerce.try_to_integer_or_infinity(state, pos_val)
  let from = int.clamp(pos, 0, js_string.length(s))
  // Step 8: StringIndexOf(S, searchStr, start). None (not found) is where the
  // JS -1 sentinel is minted — see js_string.index_of.
  let result = js_string.index_of(s, search, from) |> option.unwrap(-1)
  #(state, Ok(value.from_int(result)))
}

/// ES2024 22.1.3.11 — String.prototype.lastIndexOf ( searchString [ , position ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let searchStr be ? ToString(searchString).
///   4. Let numPos be ? ToNumber(position).
///   5. Assert: If position is undefined, then numPos is NaN.
///   6. If numPos is NaN, let pos be +inf; otherwise, let pos be
///      ToIntegerOrInfinity(numPos).
///   7. Let len be the length of S.
///   8. Let start be the result of clamping pos between 0 and len.
///   9. Let searchLen be the length of searchStr.
///  10. For each non-negative integer i such that i <= start, in
///      descending order, do
///     a. Let candidate be the substring of S from i to i + searchLen.
///     b. If candidate is searchStr, return i.
///  11. Return -1.
///
/// Note: Steps 4-6 use ? ToNumber (coerce.try_to_number); NaN maps to len
/// (equivalent to +inf clamped to len).
fn string_last_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 3: ToString(searchString)
  use search, state <- coerce.try_to_string(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Step 7: len = length of S
  let len = js_string.length(s)
  // Steps 4-6, 8: ToNumber(position), handle NaN => len, clamp
  use num, state <- coerce.try_to_number(state, helpers.arg_at(args, 1))
  let from = case num {
    // Step 6: NaN => +inf, clamped to len
    NaN -> len
    _ -> int.clamp(coerce.jsnum_to_integer_or_infinity(num), 0, len)
  }
  // Steps 10-11: search backwards from start; None => -1 (step 11).
  let result = js_string.last_index_of(s, search, from) |> option.unwrap(-1)
  #(state, Ok(value.from_int(result)))
}

/// ES2024 22.1.3.8 — String.prototype.includes ( searchString [ , position ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let isRegExp be ? IsRegExp(searchString).
///   4. If isRegExp is true, throw a TypeError exception.
///   5. Let searchStr be ? ToString(searchString).
///   6. Let pos be ? ToIntegerOrInfinity(position).
///   7. Assert: If position is undefined, then pos is 0.
///   8. Let len be the length of S.
///   9. Let start be the result of clamping pos between 0 and len.
///  10. Let index be StringIndexOf(S, searchStr, start).
///  11. If index is not -1, return true.
///  12. Return false.
///
/// Steps 3-4 (IsRegExp check → TypeError) are handled by regexp_ops.is_regexp in
/// string_search_bool.
fn string_includes(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_search_bool(this, args, state, "includes", string.contains)
}

/// Shared body for includes/startsWith: coerce this + searchString to
/// strings, clamp position, drop the prefix, apply the predicate.
fn string_search_bool(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  name: String,
  predicate: fn(String, String) -> Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  // RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  let search_val = helpers.first_arg_or_undefined(args)
  // Steps 3-4: If IsRegExp(searchString), throw TypeError
  use is_re, state <- regexp_ops.is_regexp(state, search_val)
  case is_re {
    True ->
      state.type_error(
        state,
        "First argument to String.prototype."
          <> name
          <> " must not be a regular expression",
      )
    False -> {
      // ToString(searchString)
      use search, state <- coerce.try_to_string(state, search_val)
      // ToIntegerOrInfinity(position), clamp, then test from that offset
      use pos, state <- coerce.try_to_integer_or_infinity(
        state,
        helpers.arg_at(args, 1),
      )
      let from = int.clamp(pos, 0, js_string.length(s))
      let sub = js_string.drop_start(s, from)
      #(state, Ok(value.JsBool(predicate(sub, search))))
    }
  }
}

/// ES2024 22.1.3.22 — String.prototype.startsWith ( searchString [ , position ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let isRegExp be ? IsRegExp(searchString).
///   4. If isRegExp is true, throw a TypeError exception.
///   5. Let searchStr be ? ToString(searchString).
///   6. Let len be the length of S.
///   7. If position is undefined, let pos be 0; otherwise, let pos be
///      ? ToIntegerOrInfinity(position).
///   8. Let start be the result of clamping pos between 0 and len.
///   9. Let searchLength be the length of searchStr.
///  10. If searchLength + start > len, return false.
///  11. If the substring of S from start to start + searchLength is
///      searchStr, return true.
///  12. Return false.
///
/// Steps 3-4 (IsRegExp check → TypeError) are handled by regexp_ops.is_regexp in
/// string_search_bool.
fn string_starts_with(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_search_bool(this, args, state, "startsWith", string.starts_with)
}

/// ES2024 22.1.3.7 — String.prototype.endsWith ( searchString [ , endPosition ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let isRegExp be ? IsRegExp(searchString).
///   4. If isRegExp is true, throw a TypeError exception.
///   5. Let searchStr be ? ToString(searchString).
///   6. Let len be the length of S.
///   7. If endPosition is undefined, let pos be len; otherwise, let pos be
///      ? ToIntegerOrInfinity(endPosition).
///   8. Let end be the result of clamping pos between 0 and len.
///   9. Let searchLength be the length of searchStr.
///  10. If searchLength > end, return false.
///  11. Let start be end - searchLength.
///  12. If the substring of S from start to end is searchStr, return true.
///  13. Return false.
fn string_ends_with(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  let search_val = helpers.first_arg_or_undefined(args)
  // Steps 3-4: If IsRegExp(searchString), throw TypeError
  use is_re, state <- regexp_ops.is_regexp(state, search_val)
  case is_re {
    True ->
      state.type_error(
        state,
        "First argument to String.prototype.endsWith must not be a regular expression",
      )
    False -> {
      // Step 5: ToString(searchString)
      use search, state <- coerce.try_to_string(state, search_val)
      // Step 6: len = length of S
      let len = js_string.length(s)
      // Steps 7-8: endPosition handling, clamp to [0, len]
      use end_pos, state <- second_arg_index_or_len(state, args, len, fn(n, l) {
        int.clamp(n, 0, l)
      })
      // Steps 10-13: take prefix of length end_pos, check ends_with
      let sub = js_string.slice(s, 0, end_pos)
      #(state, Ok(value.JsBool(string.ends_with(sub, search))))
    }
  }
}

/// Shared shape for endsWith/slice/substring's second argument: if it is
/// absent or undefined the result is `len`, otherwise ToIntegerOrInfinity
/// passed through `map` (which resolves/clamps per that method's spec).
fn second_arg_index_or_len(
  state: State(host),
  args: List(JsValue),
  len: Int,
  map: fn(Int, Int) -> Int,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [_, JsUndefined, ..] -> cont(len, state)
    [_, v, ..] -> {
      use n, state <- coerce.try_to_integer_or_infinity(state, v)
      cont(map(n, len), state)
    }
    _ -> cont(len, state)
  }
}

/// ES2024 22.1.3.20 — String.prototype.slice ( start, end )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let len be the length of S.
///   4. Let intStart be ? ToIntegerOrInfinity(start).
///   5. If intStart = -inf, let from be 0.
///   6. Else if intStart < 0, let from be max(len + intStart, 0).
///   7. Else, let from be min(intStart, len).
///   8. If end is undefined, let intEnd be len; otherwise let intEnd be
///      ? ToIntegerOrInfinity(end).
///   9. If intEnd = -inf, let to be 0.
///  10. Else if intEnd < 0, let to be max(len + intEnd, 0).
///  11. Else, let to be min(intEnd, len).
///  12. If from >= to, return the empty String.
///  13. Return the substring of S from from to to.
///
fn string_slice(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 3: len = length of S
  let len = js_string.length(s)
  // Steps 4-7: ToIntegerOrInfinity(start), resolve negatives
  use int_start, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  let start = resolve_slice_index(int_start, len)
  // Steps 8-11: end handling, resolve negatives
  use end, state <- second_arg_index_or_len(
    state,
    args,
    len,
    resolve_slice_index,
  )
  // Steps 12-13: if from >= to return "", else return substring
  case end > start {
    True -> #(state, Ok(JsString(js_string.slice(s, start, end - start))))
    False -> #(state, Ok(JsString("")))
  }
}

/// Resolve a slice index: negative → max(len+n, 0), non-negative → min(n, len).
/// Used by String.prototype.slice (steps 6-7 and 10-11).
fn resolve_slice_index(n: Int, len: Int) -> Int {
  case n < 0 {
    True -> int.max(len + n, 0)
    False -> int.min(n, len)
  }
}

/// ES2024 22.1.3.24 — String.prototype.substring ( start, end )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let len be the length of S.
///   4. Let intStart be ? ToIntegerOrInfinity(start).
///   5. If end is undefined, let intEnd be len; otherwise let intEnd be
///      ? ToIntegerOrInfinity(end).
///   6. Let finalStart be the result of clamping intStart between 0 and len.
///   7. Let finalEnd be the result of clamping intEnd between 0 and len.
///   8. Let from be min(finalStart, finalEnd).
///   9. Let to be max(finalStart, finalEnd).
///  10. Return the substring of S from from to to.
///
/// Note: Unlike slice(), substring() does NOT support negative indices.
/// Negative values are clamped to 0. Arguments are swapped if start > end.
fn string_substring(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 3: len = length of S
  let len = js_string.length(s)
  // Step 4: ToIntegerOrInfinity(start)
  use raw_start, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Step 5: end handling
  use raw_end, state <- second_arg_index_or_len(state, args, len, fn(n, _len) {
    n
  })
  // Steps 6-7: clamp to [0, len]
  let start = int.clamp(raw_start, 0, len)
  let end = int.clamp(raw_end, 0, len)
  // Steps 8-9: swap if start > end (from = min, to = max)
  let #(start, end) = case start > end {
    True -> #(end, start)
    False -> #(start, end)
  }
  // Step 10: return substring from..to
  #(state, Ok(JsString(js_string.slice(s, start, end - start))))
}

/// ES2024 22.1.3.27 — String.prototype.toLowerCase ( )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let sText be StringToCodePoints(S).
///   4. Let lowerText be toLowercase(sText) according to the Unicode
///      Default Case Conversion algorithm.
///   5. Let L be CodePointsToString(lowerText).
///   6. Return L.
fn string_to_lower_case(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_transform(this, state, unicode_case.to_lower_case)
}

/// ES2024 22.1.3.28 — String.prototype.toUpperCase ( )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let sText be StringToCodePoints(S).
///   4. Let upperText be toUppercase(sText) according to the Unicode
///      Default Case Conversion algorithm.
///   5. Let U be CodePointsToString(upperText).
///   6. Return U.
///
/// Note: This method interprets the String value as a sequence of UTF-16
/// encoded code points, as described in 6.1.4.
fn string_to_upper_case(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_transform(this, state, unicode_case.to_upper_case)
}

/// ES2024 22.1.3.29 — String.prototype.trim ( )
///   1. Let S be the this value.
///   2. Return ? TrimString(S, start+end).
///
/// ES2024 22.1.3.33.1 — TrimString ( string, where )
///   1. Let str be ? RequireObjectCoercible(string).
///   2. Let S be ? ToString(str).
///   3. If where is start+end, let T be the String value that is a copy of
///      S with both leading and trailing white space removed.
///   4. Return T.
fn string_trim(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_transform(this, state, fn(s) {
    value.trim_leading_js_whitespace(value.trim_trailing_js_whitespace(s))
  })
}

/// ES2024 22.1.3.30 — String.prototype.trimStart ( )
///   1. Let S be the this value.
///   2. Return ? TrimString(S, start).
///
/// TrimString with where=start removes only leading whitespace.
fn string_trim_start(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_transform(this, state, value.trim_leading_js_whitespace)
}

/// ES2024 22.1.3.31 — String.prototype.trimEnd ( )
///   1. Let S be the this value.
///   2. Return ? TrimString(S, end).
///
/// TrimString with where=end removes only trailing whitespace.
fn string_trim_end(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_transform(this, state, value.trim_trailing_js_whitespace)
}

// ---------------------------------------------------------------------------
// Symbol method delegation helpers
// ---------------------------------------------------------------------------

/// **`if V is an Object: ? GetMethod ( V, @@symbol )`** — the guard + §7.3.11
/// pair that String.prototype.match/matchAll/replace/replaceAll/search/split
/// all open with (§22.1.3.13/.14/.19/.20/.21/.22 step 2).
///
/// GetMethod:
///   1. Let func be ? GetV(V, P).
///   2. If func is either undefined or null, return undefined.
///   3. If IsCallable(func) is false, throw a TypeError exception.
///   4. Return func.
///
/// The **object-only guard is deliberate**, not an approximation of GetV: a
/// primitive `searchValue`/`separator` must NOT box and consult its prototype
/// (test262 `cstm-replace-on-string-primitive.js` and its five siblings install
/// a throwing `String.prototype[@@replace]` getter and require it never runs).
/// Step 3, on the other hand, is real: `"a".replace({[Symbol.replace]: 3})`
/// throws a TypeError before any ToString of `this`.
fn get_method(
  state: State(host),
  val: JsValue,
  symbol: value.SymbolId,
) -> Result(#(option.Option(JsValue), State(host)), #(JsValue, State(host))) {
  case val {
    JsObject(ref) -> {
      // Step 1: func = ? GetV(V, P) — a getter or a proxy `get` trap runs here.
      use #(func, state) <- result.try(object.get_symbol_value(
        state,
        ref,
        symbol,
        val,
      ))
      case func {
        // Step 2: undefined/null → the method is absent.
        JsUndefined | JsNull -> Ok(#(None, state))
        // Steps 3-4: present, must be callable.
        _ ->
          case helpers.is_callable(state.heap, func) {
            True -> Ok(#(Some(func), state))
            False ->
              Error(state.type_error_value(state, not_a_function(symbol)))
          }
      }
    }
    // Not an Object — the caller's guard declines the delegation entirely.
    _ -> Ok(#(None, state))
  }
}

/// The TypeError message for a well-known symbol method that isn't callable.
fn not_a_function(symbol: value.SymbolId) -> String {
  value.well_known_symbol_description(symbol)
  |> option.unwrap("Symbol method")
  |> string.append(" is not a function")
}

/// Call a Symbol method on an object: method.call(obj, args)
fn call_symbol_method(
  state: State(host),
  method: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  use result, state <- state.try_call(state, method, this_val, args)
  #(state, Ok(result))
}

/// Delegate to a Symbol method on `val` if it has one, passing the ORIGINAL
/// `this` value (spec §22.1.3.13/§22.1.3.20 step 2 — ToString(this) must not
/// be observable before delegation). Otherwise ToString(this), construct a
/// RegExp from `val`, and invoke the symbol method on that with the string.
/// Shared by match/search.
fn delegate_or_regexp(
  state: State(host),
  val: JsValue,
  symbol: value.SymbolId,
  this: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use method_opt, state <- state.try_op(get_method(state, val, symbol))
  case method_opt {
    Some(method) -> call_symbol_method(state, method, val, [this])
    None -> {
      // Step 3: S = ToString(O) — only now, after delegation was declined.
      use s, state <- coerce.try_to_string(state, this)
      // Step 4: construct RegExp from the argument, then delegate to its
      // symbol method.
      use rx, state <- state.try_call(
        state,
        JsObject(state.builtins.regexp.constructor),
        JsUndefined,
        [val],
      )
      // Invoke(rx, symbol): a missing/undefined method means calling
      // undefined, which throws TypeError; a throwing getter propagates.
      use method_opt, state <- state.try_op(get_method(state, rx, symbol))
      case method_opt {
        Some(method) -> call_symbol_method(state, method, rx, [JsString(s)])
        None -> state.type_error(state, not_a_function(symbol))
      }
    }
  }
}

/// ES2024 §22.1.3.12 String.prototype.match(regexp)
fn string_match(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: RequireObjectCoercible(O) — ToString deferred so a @@match
  // delegation never observes ToString(this).
  use Nil, state <- require_object_coercible(this, state, "match")
  let regexp_val = helpers.first_arg_or_undefined(args)
  // Step 2-3: delegate to Symbol.match, or construct RegExp and delegate
  delegate_or_regexp(state, regexp_val, value.symbol_match, this)
}

/// ES2024 §22.1.3.20 String.prototype.search(regexp)
fn string_search(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: RequireObjectCoercible(O) — ToString deferred so a @@search
  // delegation never observes ToString(this).
  use Nil, state <- require_object_coercible(this, state, "search")
  let regexp_val = helpers.first_arg_or_undefined(args)
  // Step 2-3: delegate to Symbol.search, or construct RegExp and delegate
  delegate_or_regexp(state, regexp_val, value.symbol_search, this)
}

/// ES2024 §22.1.3.18 String.prototype.replace(searchValue, replaceValue)
fn string_replace(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: RequireObjectCoercible(O) — ToString deferred to step 3 so a
  // @@replace delegation never observes ToString(this).
  use Nil, state <- require_object_coercible(this, state, "replace")
  let search_val = helpers.first_arg_or_undefined(args)
  let replace_val = case args {
    [_, rv, ..] -> rv
    _ -> JsUndefined
  }
  // Step 2: replacer = ? GetMethod(searchValue, @@replace); delegate if set,
  // passing the original this value.
  use method_opt, state <- state.try_op(get_method(
    state,
    search_val,
    value.symbol_replace,
  ))
  case method_opt {
    Some(method) ->
      call_symbol_method(state, method, search_val, [this, replace_val])
    None -> {
      // Steps 3-4: string = ToString(O), searchString = ToString(searchValue)
      use s, state <- coerce.try_to_string(state, this)
      use search_str, state <- coerce.try_to_string(state, search_val)
      // Steps 5-11: replace the first occurrence
      replace_string_search(state, s, search_str, replace_val, False)
    }
  }
}

/// ES2024 §22.1.3.19 String.prototype.replaceAll(searchValue, replaceValue)
fn string_replace_all(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: RequireObjectCoercible(O) — ToString deferred to step 3.
  use Nil, state <- require_object_coercible(this, state, "replaceAll")
  let search_val = helpers.first_arg_or_undefined(args)
  let replace_val = case args {
    [_, rv, ..] -> rv
    _ -> JsUndefined
  }
  // Step 2a: if IsRegExp(searchValue), Get(searchValue, "flags") must be
  // object-coercible and its string must contain "g".
  use is_re, state <- regexp_ops.is_regexp(state, search_val)
  use Nil, state <- require_global_when_regexp(state, search_val, is_re, "replaceAll")
  // Step 2b: replacer = ? GetMethod(searchValue, @@replace); delegate if set.
  use method_opt, state <- state.try_op(get_method(
    state,
    search_val,
    value.symbol_replace,
  ))
  case method_opt {
    Some(method) ->
      call_symbol_method(state, method, search_val, [this, replace_val])
    None -> {
      // Steps 3-4: string = ToString(O), searchString = ToString(searchValue)
      use s, state <- coerce.try_to_string(state, this)
      use search_str, state <- coerce.try_to_string(state, search_val)
      // Steps 5-16: replace every occurrence
      replace_string_search(state, s, search_str, replace_val, True)
    }
  }
}

/// §7.2.1 RequireObjectCoercible as a CPS guard for methods that must defer
/// ToString(this) past argument processing.
fn require_object_coercible(
  this: JsValue,
  state: State(host),
  name: String,
  cont: fn(Nil, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsNull | JsUndefined ->
      state.type_error(
        state,
        "String.prototype." <> name <> " called on null or undefined",
      )
    _ -> cont(Nil, state)
  }
}

/// `matchAll` step 2.b / `replaceAll` step 2.a: the "must be a global RegExp"
/// guard, but only when the argument IsRegExp.
fn require_global_when_regexp(
  state: State(host),
  val: JsValue,
  is_re: Bool,
  method: String,
  cont: fn(Nil, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case is_re {
    True -> regexp_ops.require_global_flags(state, val, method, cont)
    False -> cont(Nil, state)
  }
}

/// Shared string-search engine for replace (first match) and replaceAll
/// (every match): §22.1.3.18 steps 5-11 / §22.1.3.19 steps 5-16.
fn replace_string_search(
  state: State(host),
  s: String,
  search_str: String,
  replace_val: JsValue,
  all: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let search_len = js_string.length(search_str)
  // Step 5: functionalReplace = IsCallable(replaceValue)
  case helpers.is_callable(state.heap, replace_val) {
    True ->
      replace_loop_functional(
        state,
        s,
        s,
        search_str,
        search_len,
        0,
        [],
        replace_val,
        all,
      )
    False -> {
      // Step 6: replaceValue = ToString(replaceValue) — runs even when
      // there is no match.
      use template, state <- coerce.try_to_string(state, replace_val)
      // Tokenize once, not once per match. A string search has no capture
      // groups, so "$<" is always the literal "$<": plain mode.
      let segments = substitution.tokenize_plain(template)
      // `before` is a slice of the consumed prefix rebuilt at every match —
      // only accumulate it when a segment actually asks for it.
      let needs_before = list.contains(segments, substitution.BeforeSeg)
      let parts =
        replace_loop_template(
          s,
          search_str,
          search_len,
          segments,
          needs_before,
          "",
          [],
          all,
        )
      concat_within_limit(state, parts)
    }
  }
}

/// Concatenate a replace loop's reversed accumulator, honouring the
/// engine-wide invariant (see `arc/vm/limits`) that no builtin materialises a
/// string longer than `limits.max_string_bytes`.
fn concat_within_limit(
  state: State(host),
  parts_rev: List(String),
) -> #(State(host), Result(JsValue, JsValue)) {
  let parts = list.reverse(parts_rev)
  let total = list.fold(parts, 0, fn(sum, part) { sum + string.byte_size(part) })
  case total > limits.max_string_bytes {
    True -> state.range_error(state, "Invalid string length")
    False -> #(state, Ok(JsString(string.concat(parts))))
  }
}

/// Functional replaceValue: for each match, replacement =
/// ToString(? Call(replaceValue, undefined, « searched, position, string »)).
/// Searches within the remaining `tail` of the subject (tracking the
/// absolute codepoint offset in `abs_pos`) so each segment is walked once —
/// O(n) total instead of O(n^2) with absolute indices into `s`. `acc` holds
/// the output pieces in reverse; `concat_within_limit` joins them.
fn replace_loop_functional(
  state: State(host),
  tail: String,
  s: String,
  search_str: String,
  search_len: Int,
  abs_pos: Int,
  acc: List(String),
  replace_fn: JsValue,
  all: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case js_string.index_of(tail, search_str, 0) {
    None -> concat_within_limit(state, [tail, ..acc])
    Some(rel) -> {
      let preserved = js_string.slice(tail, 0, rel)
      let after = js_string.drop_start(tail, rel + search_len)
      let p = abs_pos + rel
      use result, state <- state.try_call(state, replace_fn, JsUndefined, [
        JsString(search_str),
        value.from_int(p),
        JsString(s),
      ])
      use replacement, state <- coerce.try_to_string(state, result)
      let acc = [replacement, preserved, ..acc]
      case all, search_len {
        False, _ -> concat_within_limit(state, [after, ..acc])
        // Empty search matches at every index including the end: emit one
        // codepoint and continue, or stop once the end-of-string match ran.
        True, 0 ->
          case after {
            "" -> concat_within_limit(state, acc)
            _ ->
              replace_loop_functional(
                state,
                js_string.drop_start(after, 1),
                s,
                search_str,
                search_len,
                p + 1,
                [js_string.slice(after, 0, 1), ..acc],
                replace_fn,
                all,
              )
          }
        True, _ ->
          replace_loop_functional(
            state,
            after,
            s,
            search_str,
            search_len,
            p + search_len,
            acc,
            replace_fn,
            all,
          )
      }
    }
  }
}

/// String replaceValue: replacement = GetSubstitution per match.
/// Same suffix-walking scheme as replace_loop_functional; `before` is the
/// already-consumed prefix of the subject, accumulated incrementally and only
/// when `needs_before` (i.e. some segment is a `BeforeSeg`). Returns the
/// output pieces in reverse for `concat_within_limit`.
fn replace_loop_template(
  tail: String,
  search_str: String,
  search_len: Int,
  segments: List(substitution.PlainSegment),
  needs_before: Bool,
  before: String,
  acc: List(String),
  all: Bool,
) -> List(String) {
  case js_string.index_of(tail, search_str, 0) {
    None -> [tail, ..acc]
    Some(rel) -> {
      let preserved = js_string.slice(tail, 0, rel)
      let after = js_string.drop_start(tail, rel + search_len)
      // A template that is one literal (the '$'-free case, and "$$"-only)
      // needs no Ctx — skip building it, and its `before`/`after` slices.
      let replacement = case segments {
        [substitution.LiteralSeg(text)] -> text
        _ ->
          substitution.resolve_without_named(
            segments,
            substitution.Ctx(
              matched: search_str,
              before: fn() { before <> preserved },
              after: fn() { after },
              // A string search has no capture groups: m = 0, so `capture` is
              // never called and "$1"/"$<n>" stay literal.
              capture: fn(_) { "" },
              m: 0,
            ),
          )
      }
      let acc = [replacement, preserved, ..acc]
      case all, search_len {
        False, _ -> [after, ..acc]
        // Empty search matches at every index including the end: emit one
        // codepoint and continue, or stop once the end-of-string match ran.
        True, 0 ->
          case after {
            "" -> acc
            _ -> {
              let cp = js_string.slice(after, 0, 1)
              let before = case needs_before {
                True -> before <> cp
                False -> ""
              }
              replace_loop_template(
                js_string.drop_start(after, 1),
                search_str,
                search_len,
                segments,
                needs_before,
                before,
                [cp, ..acc],
                all,
              )
            }
          }
        True, _ -> {
          let before = case needs_before {
            True -> before <> preserved <> search_str
            False -> ""
          }
          replace_loop_template(
            after,
            search_str,
            search_len,
            segments,
            needs_before,
            before,
            acc,
            all,
          )
        }
      }
    }
  }
}

/// ES2024 22.1.3.21 — String.prototype.split ( separator, limit )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. If separator is not nullish,
///     a. Let splitter be ? GetMethod(separator, @@split).
///     b. If splitter is not undefined, return
///        ? Call(splitter, separator, << O, limit >>).
///   3. Let S be ? ToString(O).
///   4. If limit is undefined, let lim be 2^32 - 1; otherwise let lim be
///      ? ToUint32(limit).
///   5. Let R be ? ToString(separator).
///   6. If lim = 0, return CreateArrayFromList(<< >>).
///   7. If separator is undefined, return CreateArrayFromList(<< S >>).
///   8. Let separatorLength be the length of R.
///   9. If separatorLength = 0, return
///      CreateArrayFromList(StringToCodePoints(S)) limited to lim entries.
///  10-15. (General splitting algorithm, collect substrings between
///      matches of R in S, up to lim entries.)
///
/// Step 4 uses the real ToUint32 (see split_limit).
///
/// TODO(Deviation): Step 9 uses graphemes instead of UTF-16 code units for
/// empty-string split — needs UTF-16 string model.
fn string_split(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: RequireObjectCoercible(O) — must run before the @@split lookup.
  use Nil, state <- require_object_coercible(this, state, "split")
  let sep_val = helpers.first_arg_or_undefined(args)
  let limit_val = case args {
    [_, l, ..] -> l
    _ -> JsUndefined
  }
  // Step 2: splitter = ? GetMethod(separator, @@split); delegate if set.
  use method_opt, state <- state.try_op(get_method(
    state,
    sep_val,
    value.symbol_split,
  ))
  case method_opt {
    Some(method) ->
      call_symbol_method(state, method, sep_val, [this, limit_val])
    None -> {
      // Step 3: string = ToString(O)
      use s, state <- with_this_string(this, state)
      // Step 4: If limit is undefined, let lim be 2^32-1; else ToUint32(limit).
      use lim, state <- split_limit(state, limit_val)
      string_split_parts(state, s, sep_val, lim)
    }
  }
}

/// Split step 4: limit undefined => 2^32 - 1, else ToUint32(limit) — full
/// ToNumber (incl. ToPrimitive, which can throw) then modulo 2^32.
fn split_limit(
  state: State(host),
  limit_val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case limit_val {
    JsUndefined -> cont(4_294_967_295, state)
    _ -> coerce.try_to_uint32(state, limit_val, cont)
  }
}

/// Steps 5-15 of String.prototype.split: compute parts and allocate array.
/// Step 5 (ToString(separator)) runs before the step-6 lim == 0 check, so a
/// throwing separator toString propagates even when lim is 0.
fn string_split_parts(
  state: State(host),
  s: String,
  sep_val: JsValue,
  lim: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  case sep_val {
    // Steps 6-7: separator undefined => [S] (or [] when lim = 0); R is
    // "undefined" but never observed, so ToString is skipped.
    JsUndefined ->
      case lim {
        0 -> state.ok_array(state, [])
        _ -> state.ok_array(state, [JsString(s)])
      }
    _ -> {
      // Step 5: R = ToString(separator)
      use sep, state <- coerce.try_to_string(state, sep_val)
      // Step 6: If lim = 0, return empty array.
      case lim {
        0 -> state.ok_array(state, [])
        _ -> {
          let parts = case sep {
            "" -> js_string.explode(s) |> list.map(JsString)
            _ -> string.split(s, sep) |> list.map(JsString)
          }
          state.ok_array(state, list.take(parts, lim))
        }
      }
    }
  }
}

/// ES2024 22.1.3.5 — String.prototype.concat ( ...args )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let R be S.
///   4. For each element next of args, do
///     a. Let nextString be ? ToString(next).
///     b. Set R to the string-concatenation of R and nextString.
///   5. Return R.
fn string_concat(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Steps 3-5: R = S, then concatenate each arg
  concat_loop(args, s, state)
}

/// Step 4 of concat: iterate args, ToString each, append to accumulator.
fn concat_loop(
  args: List(JsValue),
  acc: String,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    // Step 5: return R
    [] -> #(state, Ok(JsString(acc)))
    // Step 4a-4b: ToString(next), R = R + nextString
    [arg, ..rest] -> {
      use s, state <- coerce.try_to_string(state, arg)
      concat_loop(rest, acc <> s, state)
    }
  }
}

/// ES2024 22.1.3 — thisStringValue ( value )
///   1. If value is a String, return value.
///   2. If value is an Object and value has a [[StringData]] internal slot,
///     a. Let s be value.[[StringData]].
///     b. Assert: s is a String.
///     c. Return s.
///   3. Throw a TypeError exception.
///
fn this_string_value(
  state: State(host),
  this: JsValue,
) -> option.Option(String) {
  case this {
    // Step 1: value is a String primitive
    JsString(s) -> Some(s)
    // Step 2: value is a String wrapper object
    JsObject(ref) -> heap.read_string_object(state.heap, ref)
    // Step 3: would throw TypeError (caller handles)
    _ -> None
  }
}

/// ES2024 22.1.3.26 — String.prototype.toString ( )
///   1. Return ? thisStringValue(this value).
fn string_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: thisStringValue(this)
  case this_string_value(state, this) {
    Some(s) -> #(state, Ok(JsString(s)))
    None ->
      state.type_error(
        state,
        "String.prototype.toString requires that 'this' be a String",
      )
  }
}

/// ES2024 22.1.3.33 — String.prototype.valueOf ( )
///   1. Return ? thisStringValue(this value).
fn string_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: thisStringValue(this)
  case this_string_value(state, this) {
    Some(s) -> #(state, Ok(JsString(s)))
    None ->
      state.type_error(
        state,
        "String.prototype.valueOf requires that 'this' be a String",
      )
  }
}

/// ES2024 22.1.3.16 — String.prototype.repeat ( count )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let n be ? ToIntegerOrInfinity(count).
///   4. If n < 0 or n = +inf, throw a RangeError exception.
///   5. If n = 0, return the empty String.
///   6. Return the String value that is made from n copies of S appended
///      together.
///
fn string_repeat(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 3: Let n be ? ToIntegerOrInfinity(count).
  // Step 4: If n < 0 or n = +∞, throw a RangeError.
  let count_val = helpers.first_arg_or_undefined(args)
  use num, state <- coerce.try_to_number(state, count_val)
  case num {
    value.Infinity | value.NegInfinity ->
      state.range_error(state, "Invalid count value: Infinity")
    _ -> {
      let count = coerce.jsnum_to_integer_or_infinity(num)
      case count < 0 {
        True ->
          state.range_error(
            state,
            "Invalid count value: " <> int.to_string(count),
          )
        // Steps 5-6: If n = 0 return "", else return n copies of S
        False ->
          case limits.repeat(s, count) {
            Ok(r) -> #(state, Ok(JsString(r)))
            Error(Nil) -> state.range_error(state, "Invalid string length")
          }
      }
    }
  }
}

/// ES2024 22.1.3.16.1 — StringPad ( O, maxLength, fillString, placement )
/// Called by padStart (placement = start) and padEnd (placement = end):
///   1. Let S be ? ToString(O).
///   2. Let intMaxLength be R(? ToLength(maxLength)).
///   3. Let stringLength be the length of S.
///   4. If intMaxLength <= stringLength, return S.
///   5. If fillString is undefined, let filler be " " (a String of a
///      single space character).
///   6. Else, let filler be ? ToString(fillString).
///   7. If filler is the empty String, return S.
///   8. Let fillLen be intMaxLength - stringLength.
///   9. Let truncatedStringFiller be the String value consisting of
///      repeated concatenations of filler truncated to fillLen.
///  10. If placement is start, return truncatedStringFiller + S.
///  11. Else, return S + truncatedStringFiller.
/// ES2024 22.1.3.17 — String.prototype.padStart ( maxLength [ , fillString ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Return ? StringPad(O, maxLength, fillString, start).
fn string_pad_start(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_pad(this, args, state, limits.pad_start)
}

/// ES2024 22.1.3.16 — String.prototype.padEnd ( maxLength [ , fillString ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Return ? StringPad(O, maxLength, fillString, end).
fn string_pad_end(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  string_pad(this, args, state, limits.pad_end)
}

/// Internal: implements StringPad (ES2024 22.1.3.16.1) with a
/// configurable pad function for start vs end placement.
fn string_pad(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  pad_fn: fn(String, Int, String) -> Result(String, Nil),
) -> #(State(host), Result(JsValue, JsValue)) {
  // StringPad step 1: ToString(O)
  use s, state <- with_this_string(this, state)
  // StringPad step 2: ToLength(maxLength) — ToNumber then clamp to
  // [0, 2^53 - 1] (negative and NaN become 0).
  use max_len, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  let target_len = int.max(max_len, 0)
  let finish = fn(state, filler) {
    case pad_fn(s, target_len, filler) {
      Ok(r) -> #(state, Ok(JsString(r)))
      Error(Nil) -> state.range_error(state, "Invalid string length")
    }
  }
  case args {
    // StringPad step 5: fillString is undefined => " "
    [_, JsUndefined, ..] | [_] | [] -> finish(state, " ")
    [_, v, ..] -> {
      // StringPad step 6: ToString(fillString)
      use pad, state <- coerce.try_to_string(state, v)
      // StringPad steps 7-11: pad and return
      finish(state, pad)
    }
  }
}

/// ES2024 22.1.3.1 — String.prototype.at ( index )
/// (Added by the "Relative Indexing Method" proposal, TC39 stage 4)
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let len be the length of S.
///   4. Let relativeIndex be ? ToIntegerOrInfinity(index).
///   5. If relativeIndex >= 0, then
///     a. Let k be relativeIndex.
///   6. Else,
///     a. Let k be len + relativeIndex.
///   7. If k < 0 or k >= len, return undefined.
///   8. Return the substring of S from k to k + 1.
fn string_at(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 4: ToIntegerOrInfinity(index)
  use idx, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Step 3: len = length of S
  let len = js_string.length(s)
  // Steps 5-6: resolve relative index
  let actual_idx = case idx < 0 {
    True -> len + idx
    False -> idx
  }
  // Steps 7-8: bounds check, return char or undefined
  case actual_idx >= 0 && actual_idx < len {
    True -> #(state, Ok(JsString(js_string.slice(s, actual_idx, 1))))
    False -> #(state, Ok(JsUndefined))
  }
}

/// ES2024 22.1.3.3 — String.prototype.codePointAt ( pos )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let position be ? ToIntegerOrInfinity(pos).
///   4. Let size be the length of S.
///   5. If position < 0 or position >= size, return undefined.
///   6. Let cp be CodePointAt(S, position).
///   7. Return the Number value of cp.[[CodePoint]].
///
/// Note: Gleam strings are UTF-8 internally. The FFI walks the UTF-8 binary
/// directly to position `pos` and returns the integer codepoint — O(pos)
/// with zero list allocation, vs materializing the whole codepoint list per
/// call (which made the canonical `for (i=0;i<s.length;) cp=s.codePointAt(i)`
/// scan O(n^2)). Supplementary characters (U+10000+) are single codepoints,
/// matching the JS spec's CodePointAt semantics. Steps 4-5's bounds check is
/// folded into the walk: walking off the end means pos >= size.
fn string_code_point_at(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Step 3: ToIntegerOrInfinity(pos)
  use pos, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Steps 4-7: out of bounds (including negative) => undefined, else the
  // codepoint at pos
  let cp = case pos >= 0 {
    True -> js_string.codepoint_at(s, pos)
    False -> None
  }
  case cp {
    Some(cp) -> #(state, Ok(value.from_int(cp)))
    None -> #(state, Ok(JsUndefined))
  }
}

/// ES2024 22.1.3.13 — String.prototype.normalize ( [ form ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. If form is undefined, let f be "NFC".
///   4. Else, let f be ? ToString(form).
///   5. If f is not "NFC", "NFD", "NFKC", or "NFKD", throw a RangeError.
///   6. Let ns be the result of the Unicode Normalization Algorithm applied
///      to S using normalization form f.
///   7. Return ns.
///
/// Step 6 (the Unicode Normalization Algorithm) is performed for real:
/// ffi_nfc / ffi_nfd / ffi_nfkc / ffi_nfkd bind directly to OTP's
/// `unicode:characters_to_nf*_binary`. Step 5's form validation throws a
/// RangeError on anything other than the four spec-named forms.
fn string_normalize(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  use s, state <- with_this_string(this, state)
  // Steps 3-4: resolve normalization form (undefined => "NFC")
  case helpers.first_arg_or_undefined(args) {
    JsUndefined -> #(state, Ok(JsString(ffi_nfc(s))))
    form_val -> {
      // Step 4: ToString(form)
      use form, state <- coerce.try_to_string(state, form_val)
      // Step 5: validate form
      case form {
        // Steps 6-7: apply the Unicode Normalization Algorithm
        "NFC" -> #(state, Ok(JsString(ffi_nfc(s))))
        "NFD" -> #(state, Ok(JsString(ffi_nfd(s))))
        "NFKC" -> #(state, Ok(JsString(ffi_nfkc(s))))
        "NFKD" -> #(state, Ok(JsString(ffi_nfkd(s))))
        _ ->
          state.range_error(
            state,
            "The normalization form should be one of NFC, NFD, NFKC, NFKD",
          )
      }
    }
  }
}

// Unicode normalization via OTP's unicode module. Our strings are always
// valid UTF-8, so the error tuple return arm is unreachable.
@external(erlang, "unicode", "characters_to_nfc_binary")
fn ffi_nfc(s: String) -> String

@external(erlang, "unicode", "characters_to_nfd_binary")
fn ffi_nfd(s: String) -> String

@external(erlang, "unicode", "characters_to_nfkc_binary")
fn ffi_nfkc(s: String) -> String

@external(erlang, "unicode", "characters_to_nfkd_binary")
fn ffi_nfkd(s: String) -> String

// ============================================================================
// Static methods (String.raw, String.fromCharCode, String.fromCodePoint)
// ============================================================================

/// ES2024 22.1.2.4 — String.raw ( template, ...substitutions )
///   1. Let numberOfSubstitutions be the number of elements in substitutions.
///   2. Let cooked be ? ToObject(template).
///   3. Let literals be ? ToObject(? Get(cooked, "raw")).
///   4. Let literalCount be ? LengthOfArrayLike(literals).
///   5. If literalCount <= 0, return the empty String.
///   6. Let R be the empty String.
///   7. Let nextIndex be 0.
///   8. Repeat,
///     a. Let nextLiteralVal be ? Get(literals, ! ToString(nextIndex)).
///     b. Let nextLiteral be ? ToString(nextLiteralVal).
///     c. Set R to the string-concatenation of R and nextLiteral.
///     d. If nextIndex + 1 = literalCount, return R.
///     e. If nextIndex < numberOfSubstitutions, then
///        i. Let nextSubVal be substitutions[nextIndex].
///        ii. Let nextSub be ? ToString(nextSubVal).
///        iii. Set R to the string-concatenation of R and nextSub.
///     f. Set nextIndex to nextIndex + 1.
fn string_raw(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 2: ToObject(template)
  let template = helpers.first_arg_or_undefined(args)
  let substitutions = case args {
    [_, ..rest] -> rest
    [] -> []
  }
  // Step 3: Get(cooked, "raw")
  use raw_val, state <- try_get_of(state, template, "raw")
  // Step 4: LengthOfArrayLike(literals) — ? ToLength(? Get(raw, "length")).
  // ToNumber runs ToPrimitive on an object-valued "length" (its valueOf can
  // run user code and throw), so it threads State.
  use len_val, state <- try_get_of(state, raw_val, "length")
  use len_num, state <- coerce.try_to_number(state, len_val)
  let literal_count = case len_num {
    Finite(f) -> value.float_to_int(f)
    // NaN / ±Infinity: treated as no literals.
    _ -> 0
  }
  // Step 5: If literalCount <= 0, return ""
  case literal_count <= 0 {
    True -> #(state, Ok(JsString("")))
    False ->
      string_raw_loop(raw_val, substitutions, literal_count, 0, "", state)
  }
}

/// CPS wrapper for `object.get_value_of` (top-level [[Get]] on any JsValue).
fn try_get_of(
  state: State(host),
  val: JsValue,
  key: String,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case object.get_value_of(state, val, key.canonical_key(key)) {
    Ok(#(v, state)) -> cont(v, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Step 8 of String.raw: iterate through raw strings and substitutions.
fn string_raw_loop(
  raw_val: JsValue,
  substitutions: List(JsValue),
  literal_count: Int,
  index: Int,
  acc: String,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 8a: Get(literals, ToString(nextIndex))
  use lit_val, state <- try_get_of(state, raw_val, int.to_string(index))
  // Step 8b: ToString(nextLiteralVal)
  use lit, state <- coerce.try_to_string(state, lit_val)
  let acc = acc <> lit
  // Step 8d: If nextIndex + 1 = literalCount, return R
  case index + 1 == literal_count {
    True -> #(state, Ok(JsString(acc)))
    False ->
      // Step 8e: If nextIndex < numberOfSubstitutions, add substitution
      string_raw_add_sub(
        raw_val,
        substitutions,
        literal_count,
        index,
        acc,
        state,
      )
  }
}

/// Step 8e-8f of String.raw: add substitution and continue loop.
fn string_raw_add_sub(
  raw_val: JsValue,
  substitutions: List(JsValue),
  literal_count: Int,
  index: Int,
  acc: String,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case substitutions {
    [sub_val, ..rest_subs] -> {
      // Step 8e.ii: ToString(nextSubVal)
      use sub, state <- coerce.try_to_string(state, sub_val)
      // Step 8f: nextIndex = nextIndex + 1
      string_raw_loop(
        raw_val,
        rest_subs,
        literal_count,
        index + 1,
        acc <> sub,
        state,
      )
    }
    [] ->
      // No more substitutions, continue with just literals
      string_raw_loop(raw_val, [], literal_count, index + 1, acc, state)
  }
}

/// ES2024 22.1.2.1 — String.fromCharCode ( ...codeUnits )
///   1. Let result be the empty String.
///   2. For each element next of codeUnits, do
///     a. Let nextCU be the code unit whose numeric value is ? ToUint16(next).
///     b. Set result to the string-concatenation of result and nextCU.
///   3. Return result.
///
/// Note: fromCharCode takes UTF-16 code units. For BMP chars (0-0xFFFF), this
/// maps directly to codepoints. For surrogate pairs, we combine them.
fn string_from_char_code(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use codes, state <- from_char_code_coerce(args, state, [])
  let result_str = char_codes_to_string(list.reverse(codes), [])
  #(state, Ok(JsString(result_str)))
}

fn from_char_code_coerce(
  args: List(JsValue),
  state: State(host),
  acc: List(Int),
  cont: fn(List(Int), State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [] -> cont(acc, state)
    [arg, ..rest] -> {
      // §22.1.2.1 step 2.a: ToUint16(nextCU) → ? ToNumber (ToPrimitive on
      // objects; TypeError on Symbol/BigInt propagates).
      use num, state <- coerce.try_to_number(state, arg)
      // §7.1.8 ToUint16: NaN/±0/±Infinity → +0, else truncate mod 2^16.
      let n = case num {
        Finite(f) -> value.float_to_int(f)
        _ -> 0
      }
      from_char_code_coerce(rest, state, [modulo_uint16(n), ..acc], cont)
    }
  }
}

/// Convert a list of UTF-16 code units to a string.
/// Handles surrogate pairs: if a high surrogate (0xD800-0xDBFF) is followed by
/// a low surrogate (0xDC00-0xDFFF), combine them into a single codepoint.
fn char_codes_to_string(codes: List(Int), acc: List(UtfCodepoint)) -> String {
  case codes {
    [] -> string.from_utf_codepoints(list.reverse(acc))
    [high, low, ..rest]
      if high >= 0xD800 && high <= 0xDBFF && low >= 0xDC00 && low <= 0xDFFF
    -> {
      // Combine surrogate pair into a full codepoint
      let codepoint = { high - 0xD800 } * 0x400 + { low - 0xDC00 } + 0x10000
      char_codes_to_string(rest, [codepoint_or_replacement(codepoint), ..acc])
    }
    [code, ..rest] -> {
      char_codes_to_string(rest, [codepoint_or_replacement(code), ..acc])
    }
  }
}

/// `string.utf_codepoint(i)` with the one failure mode this engine can hit
/// mapped to U+FFFD.
///
/// TODO(Deviation): the only ints in 0..0x10FFFF that `utf_codepoint` rejects
/// are the lone surrogates D800..DFFF, which JS strings are allowed to hold
/// (they are UTF-16 code unit sequences, not scalar values). Arc stores
/// strings as UTF-8, so `String.fromCharCode(0xD800)` yields U+FFFD instead
/// of a lone surrogate, and `"\uD800".charCodeAt(0)` is 0xFFFD, not 0xD800.
/// Fixing this needs UTF-16 (or WTF-8) string storage — see js_string.gleam.
fn codepoint_or_replacement(i: Int) -> UtfCodepoint {
  case string.utf_codepoint(i) {
    Ok(cp) -> cp
    Error(Nil) -> js_string.replacement_codepoint()
  }
}

/// ToUint16: modulo 65536 (2^16), always returns 0..65535.
fn modulo_uint16(n: Int) -> Int {
  let m = n % 65_536
  case m < 0 {
    True -> m + 65_536
    False -> m
  }
}

/// ES2024 22.1.2.2 — String.fromCodePoint ( ...codePoints )
///   1. Let result be the empty String.
///   2. For each element next of codePoints, do
///     a. Let nextCP be ? ToNumber(next).
///     b. If nextCP is not an integral Number, throw a RangeError.
///     c. If nextCP < 0 or nextCP > 0x10FFFF, throw a RangeError.
///     d. Set result to the string-concatenation of result and
///        UTF16EncodeCodePoint(nextCP).
///   3. Return result.
fn string_from_code_point(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  from_code_point_loop(args, [], state)
}

/// Iterate over args for String.fromCodePoint.
fn from_code_point_loop(
  args: List(JsValue),
  acc: List(UtfCodepoint),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsString(string.from_utf_codepoints(list.reverse(acc)))))
    [arg, ..rest] -> {
      // Step 2a: ? ToNumber(next) — ToPrimitive re-entry for objects,
      // TypeError for Symbol/BigInt (which propagates, per spec).
      use num, state <- coerce.try_to_number(state, arg)
      case num {
        // Step 2b: must be integral. -0 IS integral (and encodes to U+0000),
        // so this has to go through the ±0-safe predicate.
        Finite(f) -> {
          case value.integral_int(f) {
            // Step 2c: must be in [0, 0x10FFFF]
            Some(i) if i >= 0 && i <= 0x10FFFF -> {
              // Step 2d: UTF16EncodeCodePoint
              from_code_point_loop(
                rest,
                [codepoint_or_replacement(i), ..acc],
                state,
              )
            }
            _ ->
              state.range_error(
                state,
                "Invalid code point " <> value.js_format_number(f),
              )
          }
        }
        NaN -> state.range_error(state, "Invalid code point NaN")
        _ -> state.range_error(state, "Invalid code point Infinity")
      }
    }
  }
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Annex B §B.2.2.2 String.prototype.substr ( start, length )
fn string_substr(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use s, state <- with_this_string(this, state)
  let size = js_string.length(s)
  // B.2.2.1 step 3: intStart = ToIntegerOrInfinity(start)
  use raw_start, state <- coerce.try_to_integer_or_infinity(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Step 4: -inf => 0; negative => max(size+intStart, 0); else min(_, size)
  let start = case raw_start < 0 {
    True -> int.max(size + raw_start, 0)
    False -> int.min(raw_start, size)
  }
  // Step 5: length undefined => size, else ToIntegerOrInfinity(length)
  use raw_len, state <- substr_length(state, args, size)
  // Step 6: clamp intLength to [0, size]
  let len = int.clamp(raw_len, 0, size)
  // Steps 7-9: intEnd = min(intStart + intLength, size); empty if start >= end
  let end = int.min(start + len, size)
  case start >= end {
    True -> #(state, Ok(JsString("")))
    False -> #(state, Ok(JsString(js_string.slice(s, start, end - start))))
  }
}

/// substr step 5: length undefined => size, else ToIntegerOrInfinity.
fn substr_length(
  state: State(host),
  args: List(JsValue),
  size: Int,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [_, JsUndefined, ..] -> cont(size, state)
    [_, length_arg, ..] ->
      coerce.try_to_integer_or_infinity(state, length_arg, cont)
    _ -> cont(size, state)
  }
}

/// ES2024 §22.1.3.13 String.prototype.localeCompare ( that )
/// Simplified — uses byte comparison (no locale support).
fn string_locale_compare(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use s, state <- with_this_string(this, state)
  use that, state <- coerce.try_to_string(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // §22.1.3.10: canonically equivalent strings must compare equal, so
  // compare NFC normalizations (no locale-sensitive collation support).
  let n = case string.compare(ffi_nfc(s), ffi_nfc(that)) {
    order.Lt -> -1.0
    order.Eq -> 0.0
    order.Gt -> 1.0
  }
  #(state, Ok(JsNumber(Finite(n))))
}

/// ES2024 §22.1.3.14 String.prototype.matchAll ( regexp )
/// Step 2.b: when the argument is a regexp (IsRegExp), its flags must be
/// object-coercible and contain "g", else TypeError — checked BEFORE
/// delegating to the @@matchAll method.
fn string_match_all(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: RequireObjectCoercible(this) — ToString is deferred to step 3.
  case this {
    JsNull | JsUndefined ->
      state.type_error(
        state,
        "String.prototype.matchAll called on null or undefined",
      )
    _ -> {
      let regexp_arg = helpers.first_arg_or_undefined(args)
      // Step 2: only when regexp is an Object — primitives (including
      // null/undefined) never have their @@matchAll accessed.
      case regexp_arg {
        JsObject(ref) -> {
          // Step 2a: if IsRegExp(regexp), its flags must contain "g"
          use is_re, state <- regexp_ops.is_regexp(state, regexp_arg)
          use Nil, state <- require_global_when_regexp(
            state,
            regexp_arg,
            is_re,
            "matchAll",
          )
          // Step 2b: matcher = GetMethod(regexp, @@matchAll)
          use match_all_fn, state <- state.try_op(object.get_symbol_value(
            state,
            ref,
            value.symbol_match_all,
            regexp_arg,
          ))
          case match_all_fn {
            JsUndefined | JsNull ->
              match_all_create_regexp(state, this, regexp_arg)
            _ ->
              case helpers.is_callable(state.heap, match_all_fn) {
                True -> {
                  use result, state <- state.try_call(
                    state,
                    match_all_fn,
                    regexp_arg,
                    [this],
                  )
                  #(state, Ok(result))
                }
                False ->
                  state.type_error(state, "@@matchAll method is not a function")
              }
          }
        }
        _ -> match_all_create_regexp(state, this, regexp_arg)
      }
    }
  }
}

/// matchAll steps 3-5: S = ToString(O); rx = RegExpCreate(regexp, "g");
/// return Invoke(rx, @@matchAll, « S »).
fn match_all_create_regexp(
  state: State(host),
  this: JsValue,
  regexp_arg: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 3: S = ToString(O)
  use s, state <- coerce.try_to_string(state, this)
  // Step 4: rx = RegExpCreate(R, "g"). null is stringified by the RegExp
  // constructor ("null" pattern), undefined becomes the empty pattern.
  use rx, state <- state.try_call(
    state,
    JsObject(state.builtins.regexp.constructor),
    JsUndefined,
    [regexp_arg, JsString("g")],
  )
  // Step 5: Invoke(rx, @@matchAll, « S ») — a missing method means calling
  // undefined, i.e. TypeError.
  use method_opt, state <- state.try_op(get_method(
    state,
    rx,
    value.symbol_match_all,
  ))
  case method_opt {
    Some(method) -> call_symbol_method(state, method, rx, [JsString(s)])
    None -> state.type_error(state, not_a_function(value.symbol_match_all))
  }
}

/// ES2024 §22.1.3.12 String.prototype.isWellFormed ( )
fn string_is_well_formed(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _s, state <- with_this_string(this, state)
  // Gleam strings are valid UTF-8 so always well-formed
  #(state, Ok(value.JsBool(True)))
}

/// ES2024 §22.1.3.33 String.prototype.toWellFormed ( )
fn string_to_well_formed(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Gleam strings are valid UTF-8 — already well-formed
  string_transform(this, state, fn(s) { s })
}

/// Annex B §B.2.2.x — HTML wrapper with no attribute: <tag>str</tag>
fn html_wrap(
  this: JsValue,
  state: State(host),
  tag: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use s, state <- with_this_string(this, state)
  #(state, Ok(JsString("<" <> tag <> ">" <> s <> "</" <> tag <> ">")))
}

/// Annex B §B.2.2.x — HTML wrapper with attribute: <tag attr="val">str</tag>
fn html_wrap_attr(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  tag: String,
  attr: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use s, state <- with_this_string(this, state)
  use attr_val, state <- coerce.try_to_string(
    state,
    helpers.first_arg_or_undefined(args),
  )
  // Escape quotes in attribute value per spec
  let escaped = string.replace(attr_val, "\"", "&quot;")
  #(
    state,
    Ok(JsString(
      "<"
      <> tag
      <> " "
      <> attr
      <> "=\""
      <> escaped
      <> "\">"
      <> s
      <> "</"
      <> tag
      <> ">",
    )),
  )
}

/// Extract the string value from `this`. Primitive strings pass through
/// directly (fast path). For other values, first performs RequireObjectCoercible
/// (throws TypeError on null/undefined), then calls ToString.
fn coerce_to_string(
  this: JsValue,
  state: State(host),
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  case this {
    JsString(s) -> Ok(#(s, state))
    JsNull | JsUndefined -> {
      let type_name = case this {
        JsNull -> "null"
        _ -> "undefined"
      }
      coerce.thrown_type_error(state, "Cannot read properties of " <> type_name)
    }
    _ -> coerce.js_to_string(state, this)
  }
}

/// CPS wrapper for coerce_to_string. Use with `use` syntax:
///   use s, state <- with_this_string(this, state)
/// Eliminates the `Error(#(thrown, state)) -> #(state, Error(thrown))` boilerplate
/// that appears in every String.prototype method.
fn with_this_string(
  this: JsValue,
  state: State(host),
  cont: fn(String, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> cont(s, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Coerce `this` to string, apply a pure transformation, return the result.
/// Used by toLowerCase, toUpperCase, trim, trimStart, trimEnd.
fn string_transform(
  this: JsValue,
  state: State(host),
  transform: fn(String) -> String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use s, state <- with_this_string(this, state)
  #(state, Ok(JsString(transform(s))))
}

// StringIndexOf (ES2024 7.1.18) and its reverse (22.1.3.11 steps 10-11) are
// `js_string.index_of` / `js_string.last_index_of` — total, Option-returning,
// empty needle and all. Only the two spec-facing builtins above turn a None
// into the JS -1.
//
// The codepoint string primitives (`js_string.slice`,
// `js_string.drop_start`, `js_string.explode`, `js_string.length`,
// `js_string.char_at`) all live in `arc/vm/js_string` — one place that
// defines what a JS string index means for the whole engine.
