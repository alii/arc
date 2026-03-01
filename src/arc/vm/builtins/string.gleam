import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/object
import arc/vm/value.{
  type JsValue, type Ref, type StringNativeFn, Finite, JsNull, JsNumber,
  JsObject, JsString, JsUndefined, NaN, ObjectSlot, StringFromCharCode,
  StringFromCodePoint, StringNative, StringPrototypeAt, StringPrototypeCharAt,
  StringPrototypeCharCodeAt, StringPrototypeCodePointAt, StringPrototypeConcat,
  StringPrototypeEndsWith, StringPrototypeIncludes, StringPrototypeIndexOf,
  StringPrototypeLastIndexOf, StringPrototypeNormalize, StringPrototypePadEnd,
  StringPrototypePadStart, StringPrototypeRepeat, StringPrototypeSlice,
  StringPrototypeSplit, StringPrototypeStartsWith, StringPrototypeSubstring,
  StringPrototypeToLocaleLowerCase, StringPrototypeToLocaleUpperCase,
  StringPrototypeToLowerCase, StringPrototypeToString,
  StringPrototypeToUpperCase, StringPrototypeTrim, StringPrototypeTrimEnd,
  StringPrototypeTrimStart, StringPrototypeValueOf, StringRaw,
}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

/// Set up String constructor + String.prototype.
/// ES2024 22.1.2 — Properties of the String Constructor
/// ES2024 22.1.3 — Properties of the String Prototype Object
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
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
    ])
  // Static methods on the String constructor
  let #(h, static_methods) =
    common.alloc_methods(h, function_proto, [
      #("raw", StringNative(StringRaw), 1),
      #("fromCharCode", StringNative(StringFromCharCode), 1),
      #("fromCodePoint", StringNative(StringFromCodePoint), 1),
    ])
  // Note: StringConstructor stays VM-level (needs ToPrimitive/ToString)
  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_methods,
    fn(_) { value.Call(value.StringConstructor) },
    "String",
    1,
    static_methods,
  )
}

/// Per-module dispatch for String native functions.
pub fn dispatch(
  native: StringNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
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
    // Static methods
    StringRaw -> string_raw(args, state)
    StringFromCharCode -> string_from_char_code(args, state)
    StringFromCodePoint -> string_from_code_point(args, state)
  }
}

/// ES2024 22.1.1.1 — String ( value )
/// When String is called as a function (not as a constructor):
///   1. If value is not present, let s be the empty String.
///   2. Else,
///     a. If NewTarget is undefined and value is a Symbol, return
///        SymbolDescriptiveString(value).
///     b. Let s be ? ToString(value).
///   3. If NewTarget is undefined, return s.
///
/// TODO(Deviation): Step 2a (Symbol descriptive string) needs Symbol.toPrimitive support.
/// Note: Constructor path (NewTarget defined) is handled separately
/// in vm.gleam, this only covers the function-call path (step 3).
pub fn call_as_function(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    // Step 1: no value => empty string
    [] -> #(state, Ok(JsString("")))
    // Step 2b: ToString(value)
    [val, ..] -> {
      use s, state <- frame.try_to_string(state, val)
      // Step 3: return s (NewTarget is always undefined here)
      #(state, Ok(JsString(s)))
    }
  }
}

// ============================================================================
// String.prototype method implementations
// ============================================================================

/// ES2024 22.1.3.1 — String.prototype.charAt ( pos )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let position be ? ToIntegerOrInfinity(pos).
///   4. Let size be the length of S.
///   5. If position < 0 or position >= size, return the empty String.
///   6. Return the substring of S from position to position + 1.
///
pub fn string_char_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // Step 3: ToIntegerOrInfinity(pos)
      let idx = helpers.get_int_arg(args, 0, 0)
      // Step 4: size = length of S
      let len = string.length(s)
      // Steps 5-6: bounds check, return char or ""
      case idx >= 0 && idx < len {
        True -> #(state, Ok(JsString(string.slice(s, idx, 1))))
        False -> #(state, Ok(JsString("")))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn string_char_code_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // Step 3: ToIntegerOrInfinity(pos)
      let idx = helpers.get_int_arg(args, 0, 0)
      // Step 4: size = length of S
      let len = string.length(s)
      // Step 5: out of bounds => NaN
      case idx >= 0 && idx < len {
        True -> {
          // Step 6: return code unit value
          let ch = string.slice(s, idx, 1)
          case string.to_utf_codepoints(ch) {
            [cp, ..] -> {
              let code = string.utf_codepoint_to_int(cp)
              #(state, Ok(JsNumber(Finite(int.to_float(code)))))
            }
            [] -> #(state, Ok(JsNumber(NaN)))
          }
        }
        False -> #(state, Ok(JsNumber(NaN)))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn string_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      // Step 3: ToString(searchString)
      use search, state <- frame.try_to_string(state, search_val)
      // Steps 4-7: ToIntegerOrInfinity(position), clamp
      let from = helpers.get_int_arg(args, 1, 0)
      // Step 8: StringIndexOf(S, searchStr, start)
      let result = index_of_from(s, search, from)
      #(state, Ok(JsNumber(Finite(int.to_float(result)))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
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
/// Note: Steps 4-6 use to_number_int which returns None for NaN,
/// and None maps to len (equivalent to +inf clamped to len).
pub fn string_last_index_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      // Step 3: ToString(searchString)
      use search, state <- frame.try_to_string(state, search_val)
      // Step 7: len = length of S
      let len = string.length(s)
      // Steps 4-6, 8: ToNumber(position), handle NaN => len, clamp
      let from = case args {
        [_, pos_val, ..] ->
          case helpers.to_number_int(pos_val) {
            // Step 8: clamp pos between 0 and len
            Some(n) -> int.min(n, len)
            // Steps 5-6: NaN => pos = +inf => clamped to len
            None -> len
          }
        // Step 5: position is undefined => NaN => len
        _ -> len
      }
      // Steps 10-11: search backwards from start
      let result = last_index_of_from(s, search, from)
      #(state, Ok(JsNumber(Finite(int.to_float(result)))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
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
/// TODO(Deviation): Steps 3-4 (IsRegExp check) not implemented — needs RegExp.
/// Passing a RegExp will be coerced to string instead of throwing TypeError.
pub fn string_includes(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      // Step 5: ToString(searchString)
      use search, state <- frame.try_to_string(state, search_val)
      // Steps 6-9: ToIntegerOrInfinity(position), clamp
      let from = helpers.get_int_arg(args, 1, 0)
      // Steps 10-12: StringIndexOf and return boolean
      let sub = string.drop_start(s, from)
      #(state, Ok(value.JsBool(string.contains(sub, search))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
/// TODO(Deviation): Steps 3-4 (IsRegExp check) not implemented — needs RegExp.
pub fn string_starts_with(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      // Step 5: ToString(searchString)
      use search, state <- frame.try_to_string(state, search_val)
      // Steps 7-8: position handling + clamp
      let from = helpers.get_int_arg(args, 1, 0)
      // Steps 10-12: drop prefix, check starts_with
      let sub = string.drop_start(s, from)
      #(state, Ok(value.JsBool(string.starts_with(sub, search))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
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
///
/// TODO(Deviation): Steps 3-4 (IsRegExp check) not implemented — needs RegExp.
pub fn string_ends_with(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let search_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      // Step 5: ToString(searchString)
      use search, state <- frame.try_to_string(state, search_val)
      // Step 6: len = length of S
      let len = string.length(s)
      // Steps 7-8: endPosition handling, clamp to [0, len]
      let end_pos = case args {
        [_, pos_val, ..] ->
          case pos_val {
            // Step 7: undefined => len
            JsUndefined -> len
            _ ->
              case helpers.to_number_int(pos_val) {
                // Step 8: clamp pos between 0 and len
                Some(n) -> int.clamp(n, 0, len)
                // NaN => 0 (ToIntegerOrInfinity(NaN) = 0)
                None -> 0
              }
          }
        _ -> len
      }
      // Steps 10-13: take prefix of length end_pos, check ends_with
      let sub = string.slice(s, 0, end_pos)
      #(state, Ok(value.JsBool(string.ends_with(sub, search))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn string_slice(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // Step 3: len = length of S
      let len = string.length(s)
      // Steps 4-7: ToIntegerOrInfinity(start), resolve negatives
      let start = case args {
        [v, ..] ->
          case helpers.to_number_int(v) {
            Some(n) ->
              case n < 0 {
                // Step 6: max(len + intStart, 0)
                True -> int.max(len + n, 0)
                // Step 7: min(intStart, len)
                False -> int.min(n, len)
              }
            None -> 0
          }
        [] -> 0
      }
      // Steps 8-11: end handling, resolve negatives
      let end = case args {
        // Step 8: end is undefined => intEnd = len
        [_, JsUndefined, ..] -> len
        [_, v, ..] ->
          case helpers.to_number_int(v) {
            Some(n) ->
              case n < 0 {
                // Step 10: max(len + intEnd, 0)
                True -> int.max(len + n, 0)
                // Step 11: min(intEnd, len)
                False -> int.min(n, len)
              }
            // NaN => 0
            None -> 0
          }
        _ -> len
      }
      // Steps 12-13: if from >= to return "", else return substring
      case end > start {
        True -> #(state, Ok(JsString(string.slice(s, start, end - start))))
        False -> #(state, Ok(JsString("")))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn string_substring(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // Step 3: len = length of S
      let len = string.length(s)
      // Step 4: ToIntegerOrInfinity(start)
      let raw_start = case args {
        [v, ..] ->
          case helpers.to_number_int(v) {
            Some(n) -> n
            None -> 0
          }
        [] -> 0
      }
      // Step 5: end handling
      let raw_end = case args {
        [_, JsUndefined, ..] -> len
        [_, v, ..] ->
          case helpers.to_number_int(v) {
            Some(n) -> n
            None -> 0
          }
        _ -> len
      }
      // Steps 6-7: clamp to [0, len]
      let start = int.clamp(raw_start, 0, len)
      let end = int.clamp(raw_end, 0, len)
      // Steps 8-9: swap if start > end (from = min, to = max)
      let #(start, end) = case start > end {
        True -> #(end, start)
        False -> #(start, end)
      }
      // Step 10: return substring from..to
      #(state, Ok(JsString(string.slice(s, start, end - start))))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// ES2024 22.1.3.27 — String.prototype.toLowerCase ( )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Let S be ? ToString(O).
///   3. Let sText be StringToCodePoints(S).
///   4. Let lowerText be toLowercase(sText) according to the Unicode
///      Default Case Conversion algorithm.
///   5. Let L be CodePointsToString(lowerText).
///   6. Return L.
pub fn string_to_lower_case(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.lowercase)
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
pub fn string_to_upper_case(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.uppercase)
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
pub fn string_trim(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.trim)
}

/// ES2024 22.1.3.30 — String.prototype.trimStart ( )
///   1. Let S be the this value.
///   2. Return ? TrimString(S, start).
///
/// TrimString with where=start removes only leading whitespace.
pub fn string_trim_start(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.trim_start)
}

/// ES2024 22.1.3.31 — String.prototype.trimEnd ( )
///   1. Let S be the this value.
///   2. Return ? TrimString(S, end).
///
/// TrimString with where=end removes only trailing whitespace.
pub fn string_trim_end(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_transform(this, state, string.trim_end)
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
/// TODO(Deviation): Steps 2a-2b (@@split symbol dispatch) not implemented — needs
/// Symbol.split. RegExp and custom splitters will be coerced to strings.
/// TODO(Deviation): Step 4 uses ToIntegerOrInfinity instead of ToUint32 for limit.
/// TODO(Deviation): Step 9 uses graphemes instead of UTF-16 code units for
/// empty-string split — needs UTF-16 string model.
pub fn string_split(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let array_proto = state.builtins.array.prototype
  // Steps 1, 3: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      let sep_val = case args {
        [s, ..] -> s
        [] -> JsUndefined
      }
      let limit_val = case args {
        [_, l, ..] -> l
        _ -> JsUndefined
      }
      // Step 4: If limit is undefined, let lim be 2^32-1; else ToUint32(limit).
      // ToUint32 of NaN or non-numeric string is 0.
      let lim = case limit_val {
        JsUndefined -> 4_294_967_295
        _ ->
          case helpers.to_number_int(limit_val) {
            Some(n) if n >= 0 -> n
            _ -> 0
          }
      }
      // Step 6: If lim = 0, return empty array.
      case lim {
        0 -> {
          let #(heap, ref) = common.alloc_array(state.heap, [], array_proto)
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
        _ ->
          case sep_val {
            // Step 7: If separator is undefined, return [S].
            JsUndefined -> {
              let #(heap, ref) =
                common.alloc_array(state.heap, [JsString(s)], array_proto)
              #(State(..state, heap:), Ok(JsObject(ref)))
            }
            _ -> {
              // Step 5: R = ToString(separator)
              case frame.to_string(state, sep_val) {
                Ok(#(sep, state)) -> {
                  // Steps 8-9: empty separator => split into individual chars
                  // Steps 10-15: general splitting
                  let parts = case sep {
                    "" -> string.to_graphemes(s) |> list.map(JsString)
                    _ -> string.split(s, sep) |> list.map(JsString)
                  }
                  // Apply limit
                  let parts = list.take(parts, lim)
                  let #(heap, ref) =
                    common.alloc_array(state.heap, parts, array_proto)
                  #(State(..state, heap:), Ok(JsObject(ref)))
                }
                Error(#(thrown, state)) -> #(state, Error(thrown))
              }
            }
          }
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn string_concat(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    // Steps 3-5: R = S, then concatenate each arg
    Ok(#(s, state)) -> concat_loop(args, s, state)
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Step 4 of concat: iterate args, ToString each, append to accumulator.
fn concat_loop(
  args: List(JsValue),
  acc: String,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    // Step 5: return R
    [] -> #(state, Ok(JsString(acc)))
    // Step 4a-4b: ToString(next), R = R + nextString
    [arg, ..rest] ->
      case frame.to_string(state, arg) {
        Ok(#(s, state)) -> concat_loop(rest, acc <> s, state)
        Error(#(thrown, state)) -> #(state, Error(thrown))
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
fn this_string_value(state: State, this: JsValue) -> option.Option(String) {
  case this {
    // Step 1: value is a String primitive
    JsString(s) -> Some(s)
    // Step 2: value is a String wrapper object
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.StringObject(value: s), ..)) -> Some(s)
        _ -> None
      }
    // Step 3: would throw TypeError (caller handles)
    _ -> None
  }
}

/// ES2024 22.1.3.26 — String.prototype.toString ( )
///   1. Return ? thisStringValue(this value).
pub fn string_to_string(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: thisStringValue(this)
  case this_string_value(state, this) {
    Some(s) -> #(state, Ok(JsString(s)))
    None ->
      frame.type_error(
        state,
        "String.prototype.toString requires that 'this' be a String",
      )
  }
}

/// ES2024 22.1.3.33 — String.prototype.valueOf ( )
///   1. Return ? thisStringValue(this value).
pub fn string_value_of(
  this: JsValue,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 1: thisStringValue(this)
  case this_string_value(state, this) {
    Some(s) -> #(state, Ok(JsString(s)))
    None ->
      frame.type_error(
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
pub fn string_repeat(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // Step 3: Let n be ? ToIntegerOrInfinity(count).
      // Step 4: If n < 0 or n = +∞, throw a RangeError.
      let count_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      case count_val {
        JsNumber(value.Infinity) | JsNumber(value.NegInfinity) ->
          frame.range_error(state, "Invalid count value: Infinity")
        _ -> {
          let count = helpers.to_number_int(count_val) |> option.unwrap(0)
          case count < 0 {
            True ->
              frame.range_error(
                state,
                "Invalid count value: " <> int.to_string(count),
              )
            // Steps 5-6: If n = 0 return "", else return n copies of S
            False -> #(state, Ok(JsString(string.repeat(s, count))))
          }
        }
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn string_pad_start(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_pad(this, args, state, string.pad_start)
}

/// ES2024 22.1.3.16 — String.prototype.padEnd ( maxLength [ , fillString ] )
///   1. Let O be ? RequireObjectCoercible(this value).
///   2. Return ? StringPad(O, maxLength, fillString, end).
pub fn string_pad_end(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  string_pad(this, args, state, string.pad_end)
}

/// Internal: implements StringPad (ES2024 22.1.3.16.1) with a
/// configurable pad function for start vs end placement.
fn string_pad(
  this: JsValue,
  args: List(JsValue),
  state: State,
  pad_fn: fn(String, Int, String) -> String,
) -> #(State, Result(JsValue, JsValue)) {
  // StringPad step 1: ToString(O)
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // StringPad step 2: ToLength(maxLength)
      let target_len = helpers.get_int_arg(args, 0, 0)
      case args {
        [_, v, ..] ->
          case v {
            // StringPad step 5: fillString is undefined => " "
            JsUndefined -> #(state, Ok(JsString(pad_fn(s, target_len, " "))))
            _ -> {
              // StringPad step 6: ToString(fillString)
              use pad, state <- frame.try_to_string(state, v)
              // StringPad steps 7-11: pad and return
              #(state, Ok(JsString(pad_fn(s, target_len, pad))))
            }
          }
        // No fillString arg => default to " "
        _ -> #(state, Ok(JsString(pad_fn(s, target_len, " "))))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
pub fn string_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // Step 4: ToIntegerOrInfinity(index)
      let idx = helpers.get_int_arg(args, 0, 0)
      // Step 3: len = length of S
      let len = string.length(s)
      // Steps 5-6: resolve relative index
      let actual_idx = case idx < 0 {
        True -> len + idx
        False -> idx
      }
      // Steps 7-8: bounds check, return char or undefined
      case actual_idx >= 0 && actual_idx < len {
        True -> #(state, Ok(JsString(string.slice(s, actual_idx, 1))))
        False -> #(state, Ok(JsUndefined))
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
/// Note: Gleam strings are UTF-8 internally. We convert to a list of Unicode
/// codepoints with string.to_utf_codepoints, then index into that list.
/// This correctly handles supplementary characters (U+10000+) as single
/// codepoints, matching the JS spec's CodePointAt semantics.
pub fn string_code_point_at(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // Step 3: ToIntegerOrInfinity(pos)
      let pos = helpers.get_int_arg(args, 0, 0)
      // Step 4: size = length of S (in codepoints)
      let codepoints = string.to_utf_codepoints(s)
      let size = list.length(codepoints)
      // Step 5: out of bounds => undefined
      case pos >= 0 && pos < size {
        False -> #(state, Ok(JsUndefined))
        True -> {
          // Step 6-7: get the codepoint at position and return it
          let cp_value =
            list.drop(codepoints, pos)
            |> list.first
          case cp_value {
            Ok(cp) -> #(
              state,
              Ok(
                JsNumber(Finite(int.to_float(string.utf_codepoint_to_int(cp)))),
              ),
            )
            Error(Nil) -> #(state, Ok(JsUndefined))
          }
        }
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
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
/// Note: This is a stub that returns the string unchanged. A full
/// implementation requires Unicode normalization tables (NFC/NFD/NFKC/NFKD).
/// Most JS strings encountered in practice are already in NFC form, so this
/// is sufficient for basic tests. The form argument validation is done to
/// match spec behaviour (throw on invalid form), but valid forms return the
/// input unchanged.
fn string_normalize(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Steps 1-2: RequireObjectCoercible + ToString
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> {
      // Steps 3-4: resolve normalization form
      let form_val = case args {
        [v, ..] -> v
        [] -> JsUndefined
      }
      case form_val {
        JsUndefined -> #(state, Ok(JsString(s)))
        _ -> {
          // Step 4: ToString(form)
          use form, state <- frame.try_to_string(state, form_val)
          // Step 5: validate form
          case form {
            "NFC" | "NFD" | "NFKC" | "NFKD" ->
              // Step 6-7: return string unchanged (stub — no normalization tables)
              #(state, Ok(JsString(s)))
            _ ->
              frame.range_error(
                state,
                "The normalization form should be one of NFC, NFD, NFKC, NFKD",
              )
          }
        }
      }
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 2: ToObject(template)
  let template = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  let substitutions = case args {
    [_, ..rest] -> rest
    [] -> []
  }
  // Step 3: Get(cooked, "raw")
  case object.get_value_of(state, template, "raw") {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(raw_val, state)) -> {
      // Step 4: LengthOfArrayLike(literals) — read "length" from raw
      case object.get_value_of(state, raw_val, "length") {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(len_val, state)) -> {
          let literal_count = helpers.to_number_int(len_val) |> option.unwrap(0)
          // Step 5: If literalCount <= 0, return ""
          case literal_count <= 0 {
            True -> #(state, Ok(JsString("")))
            False ->
              string_raw_loop(
                raw_val,
                substitutions,
                literal_count,
                0,
                "",
                state,
              )
          }
        }
      }
    }
  }
}

/// Step 8 of String.raw: iterate through raw strings and substitutions.
fn string_raw_loop(
  raw_val: JsValue,
  substitutions: List(JsValue),
  literal_count: Int,
  index: Int,
  acc: String,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Step 8a: Get(literals, ToString(nextIndex))
  case object.get_value_of(state, raw_val, int.to_string(index)) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(lit_val, state)) -> {
      // Step 8b: ToString(nextLiteralVal)
      case frame.to_string(state, lit_val) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(lit, state)) -> {
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
      }
    }
  }
}

/// Step 8e-8f of String.raw: add substitution and continue loop.
fn string_raw_add_sub(
  raw_val: JsValue,
  substitutions: List(JsValue),
  literal_count: Int,
  index: Int,
  acc: String,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case substitutions {
    [sub_val, ..rest_subs] -> {
      // Step 8e.ii: ToString(nextSubVal)
      case frame.to_string(state, sub_val) {
        Error(#(thrown, state)) -> #(state, Error(thrown))
        Ok(#(sub, state)) ->
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let codes =
    list.map(args, fn(arg) {
      let n = helpers.to_number_int(arg) |> option.unwrap(0)
      // ToUint16: modulo 2^16
      let code = modulo_uint16(n)
      code
    })
  let result_str = char_codes_to_string(codes, "")
  #(state, Ok(JsString(result_str)))
}

/// Convert a list of UTF-16 code units to a string.
/// Handles surrogate pairs: if a high surrogate (0xD800-0xDBFF) is followed by
/// a low surrogate (0xDC00-0xDFFF), combine them into a single codepoint.
fn char_codes_to_string(codes: List(Int), acc: String) -> String {
  case codes {
    [] -> acc
    [high, low, ..rest]
      if high >= 0xD800 && high <= 0xDBFF && low >= 0xDC00 && low <= 0xDFFF
    -> {
      // Combine surrogate pair into a full codepoint
      let codepoint = { high - 0xD800 } * 0x400 + { low - 0xDC00 } + 0x10000
      let ch = case string.utf_codepoint(codepoint) {
        Ok(cp) -> string.from_utf_codepoints([cp])
        Error(_) -> "\u{FFFD}"
      }
      char_codes_to_string(rest, acc <> ch)
    }
    [code, ..rest] -> {
      let ch = case string.utf_codepoint(code) {
        Ok(cp) -> string.from_utf_codepoints([cp])
        Error(_) -> "\u{FFFD}"
      }
      char_codes_to_string(rest, acc <> ch)
    }
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
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  from_code_point_loop(args, "", state)
}

/// Iterate over args for String.fromCodePoint.
fn from_code_point_loop(
  args: List(JsValue),
  acc: String,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case args {
    [] -> #(state, Ok(JsString(acc)))
    [arg, ..rest] -> {
      // Step 2a: ToNumber(next) — inline simplified ToNumber
      case to_number_for_code_point(arg) {
        // Step 2b: must be integral
        Ok(Finite(f)) -> {
          let i = value.float_to_int(f)
          case int.to_float(i) == f {
            // Step 2c: must be in [0, 0x10FFFF]
            True if i >= 0 && i <= 0x10FFFF -> {
              // Step 2d: UTF16EncodeCodePoint
              let ch = case string.utf_codepoint(i) {
                Ok(cp) -> string.from_utf_codepoints([cp])
                Error(_) -> "\u{FFFD}"
              }
              from_code_point_loop(rest, acc <> ch, state)
            }
            _ ->
              frame.range_error(
                state,
                "Invalid code point " <> value.js_format_number(f),
              )
          }
        }
        Ok(NaN) -> frame.range_error(state, "Invalid code point NaN")
        Ok(_) -> frame.range_error(state, "Invalid code point Infinity")
        // Non-numeric (e.g. NaN from undefined/non-numeric string)
        Error(Nil) -> frame.range_error(state, "Invalid code point NaN")
      }
    }
  }
}

/// Simplified ToNumber for fromCodePoint — returns the JsNum or Error(Nil)
/// for values that would produce NaN from non-obvious sources.
fn to_number_for_code_point(val: JsValue) -> Result(value.JsNum, Nil) {
  case val {
    JsNumber(n) -> Ok(n)
    JsUndefined -> Error(Nil)
    value.JsNull -> Ok(Finite(0.0))
    value.JsBool(True) -> Ok(Finite(1.0))
    value.JsBool(False) -> Ok(Finite(0.0))
    JsString(s) ->
      case int.parse(s) {
        Ok(n) -> Ok(Finite(int.to_float(n)))
        Error(Nil) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Extract the string value from `this`. Primitive strings pass through
/// directly (fast path). For other values, first performs RequireObjectCoercible
/// (throws TypeError on null/undefined), then calls ToString.
fn coerce_to_string(
  this: JsValue,
  state: State,
) -> Result(#(String, State), #(JsValue, State)) {
  case this {
    JsString(s) -> Ok(#(s, state))
    JsNull | JsUndefined -> {
      let type_name = case this {
        JsNull -> "null"
        _ -> "undefined"
      }
      let #(h, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "Cannot read properties of " <> type_name,
        )
      Error(#(err, State(..state, heap: h)))
    }
    _ -> frame.to_string(state, this)
  }
}

/// Coerce `this` to string, apply a pure transformation, return the result.
/// Used by toLowerCase, toUpperCase, trim, trimStart, trimEnd.
fn string_transform(
  this: JsValue,
  state: State,
  transform: fn(String) -> String,
) -> #(State, Result(JsValue, JsValue)) {
  case coerce_to_string(this, state) {
    Ok(#(s, state)) -> #(state, Ok(JsString(transform(s))))
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Implements the StringIndexOf abstract operation.
/// ES2024 7.1.18 — StringIndexOf ( string, searchValue, fromIndex )
///   1. Let len be the length of string.
///   2. If searchValue is the empty String and fromIndex <= len, return
///      fromIndex.
///   3. Let searchLen be the length of searchValue.
///   4. For each integer i such that fromIndex <= i <= len - searchLen, in
///      ascending order, do
///     a. Let candidate be the substring of string from i to i + searchLen.
///     b. If candidate is searchValue, return i.
///   5. Return -1 (not found).
fn index_of_from(s: String, search: String, from: Int) -> Int {
  let len = string.length(s)
  let search_len = string.length(search)
  // Step 2: empty search at valid position returns that position
  case search_len == 0 {
    True ->
      case from >= 0 && from <= len {
        True -> from
        False ->
          case from < 0 {
            True -> 0
            False -> len
          }
      }
    // Steps 3-5: linear scan
    False -> index_of_loop(s, search, int.max(from, 0), len, search_len)
  }
}

/// Step 4 of StringIndexOf: linear scan from pos to len - search_len.
fn index_of_loop(
  s: String,
  search: String,
  pos: Int,
  len: Int,
  search_len: Int,
) -> Int {
  // Step 4: i <= len - searchLen (equivalently, pos + search_len <= len)
  case pos + search_len > len {
    True -> -1
    False ->
      // Step 4a-4b: candidate = substring, check equality
      case string.slice(s, pos, search_len) == search {
        True -> pos
        False -> index_of_loop(s, search, pos + 1, len, search_len)
      }
  }
}

/// Reverse StringIndexOf: find last occurrence of `search` in `s`
/// searching backwards from index `from`.
/// Used by String.prototype.lastIndexOf (ES2024 22.1.3.11, steps 10-11).
fn last_index_of_from(s: String, search: String, from: Int) -> Int {
  let len = string.length(s)
  let search_len = string.length(search)
  // Empty search: return min(from, len)
  case search_len == 0 {
    True -> int.min(from, len)
    False -> {
      // Start from min(from, len - searchLen) and scan backwards
      let start = int.min(from, len - search_len)
      last_index_of_loop(s, search, start, search_len)
    }
  }
}

/// Backwards scan for lastIndexOf: check each position from start down to 0.
fn last_index_of_loop(
  s: String,
  search: String,
  pos: Int,
  search_len: Int,
) -> Int {
  case pos < 0 {
    True -> -1
    False ->
      case string.slice(s, pos, search_len) == search {
        True -> pos
        False -> last_index_of_loop(s, search, pos - 1, search_len)
      }
  }
}
