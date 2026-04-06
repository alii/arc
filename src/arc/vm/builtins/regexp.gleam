/// ES2024 §22.2 RegExp Objects
///
/// RegExp constructor, prototype methods (test, exec, toString),
/// and accessor getters (source, flags, global, ignoreCase, etc.).
/// Uses Erlang's `re` module (PCRE) via FFI for actual matching.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type RegExpNativeFn, DataProperty, Dispatch, Finite,
  JsBool, JsNull, JsNumber, JsObject, JsString, JsUndefined, Named, ObjectSlot,
  RegExpConstructor, RegExpGetDotAll, RegExpGetFlags, RegExpGetGlobal,
  RegExpGetHasIndices, RegExpGetIgnoreCase, RegExpGetMultiline, RegExpGetSource,
  RegExpGetSticky, RegExpGetUnicode, RegExpNative, RegExpObject,
  RegExpPrototypeExec, RegExpPrototypeTest, RegExpPrototypeToString,
  RegExpSymbolMatch, RegExpSymbolReplace, RegExpSymbolSearch, RegExpSymbolSplit,
}
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// FFI: byte-sort flag string. Spec canonical order "dgimsuvy" (§22.2.6.4) is
/// ascending ASCII, so a byte-sort yields it.
@external(erlang, "arc_regexp_ffi", "canonical_flags")
@external(javascript, "./arc_regexp_ffi.mjs", "canonical_flags")
fn canonical_flags(flags: String) -> String

/// FFI: test if pattern matches string
@external(erlang, "arc_regexp_ffi", "regexp_test")
fn ffi_regexp_test(pattern: String, flags: String, string: String) -> Bool

/// FFI: execute pattern on string at offset, returning match indices
@external(erlang, "arc_regexp_ffi", "regexp_exec")
fn ffi_regexp_exec(
  pattern: String,
  flags: String,
  string: String,
  offset: Int,
) -> Result(List(#(Int, Int)), Nil)

/// Set up RegExp constructor + RegExp.prototype.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  // Allocate prototype methods
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("test", RegExpNative(RegExpPrototypeTest), 1),
      #("exec", RegExpNative(RegExpPrototypeExec), 1),
      #("toString", RegExpNative(RegExpPrototypeToString), 0),
    ])

  // Allocate accessor getter functions for flag properties
  let #(h, getters) =
    common.alloc_getters(h, function_proto, [
      #("source", RegExpNative(RegExpGetSource)),
      #("flags", RegExpNative(RegExpGetFlags)),
      #("global", RegExpNative(RegExpGetGlobal)),
      #("ignoreCase", RegExpNative(RegExpGetIgnoreCase)),
      #("multiline", RegExpNative(RegExpGetMultiline)),
      #("dotAll", RegExpNative(RegExpGetDotAll)),
      #("sticky", RegExpNative(RegExpGetSticky)),
      #("unicode", RegExpNative(RegExpGetUnicode)),
      #("hasIndices", RegExpNative(RegExpGetHasIndices)),
    ])

  let proto_props = list.append(proto_methods, getters)

  let #(h, builtin) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_props,
      fn(_) { Dispatch(RegExpNative(RegExpConstructor)) },
      "RegExp",
      2,
      [],
    )

  // Allocate Symbol method functions and patch onto prototype
  let #(h, match_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpSymbolMatch),
      "[Symbol.match]",
      1,
    )
  let #(h, replace_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpSymbolReplace),
      "[Symbol.replace]",
      2,
    )
  let #(h, search_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpSymbolSearch),
      "[Symbol.search]",
      1,
    )
  let #(h, split_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpSymbolSplit),
      "[Symbol.split]",
      2,
    )
  let h =
    heap.update(h, builtin.prototype, fn(slot) {
      case slot {
        ObjectSlot(symbol_properties: sp, ..) ->
          ObjectSlot(
            ..slot,
            symbol_properties: sp
              |> list.key_set(
                value.symbol_match,
                value.builtin_property(JsObject(match_fn)),
              )
              |> list.key_set(
                value.symbol_replace,
                value.builtin_property(JsObject(replace_fn)),
              )
              |> list.key_set(
                value.symbol_search,
                value.builtin_property(JsObject(search_fn)),
              )
              |> list.key_set(
                value.symbol_split,
                value.builtin_property(JsObject(split_fn)),
              ),
          )
        other -> other
      }
    })
  #(h, builtin)
}

/// Per-module dispatch for RegExp native functions.
pub fn dispatch(
  native: RegExpNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    RegExpConstructor -> regexp_constructor(args, state)
    RegExpPrototypeTest -> regexp_test(this, args, state)
    RegExpPrototypeExec -> regexp_exec(this, args, state)
    RegExpPrototypeToString -> regexp_to_string(this, state)
    RegExpGetSource -> regexp_get_source(this, state)
    RegExpGetFlags -> regexp_get_flags(this, state)
    RegExpGetGlobal -> regexp_flag_getter(this, "g", state)
    RegExpGetIgnoreCase -> regexp_flag_getter(this, "i", state)
    RegExpGetMultiline -> regexp_flag_getter(this, "m", state)
    RegExpGetDotAll -> regexp_flag_getter(this, "s", state)
    RegExpGetSticky -> regexp_flag_getter(this, "y", state)
    RegExpGetUnicode -> regexp_flag_getter(this, "u", state)
    RegExpGetHasIndices -> regexp_flag_getter(this, "d", state)
    RegExpSymbolMatch -> regexp_symbol_match(this, args, state)
    RegExpSymbolReplace -> regexp_symbol_replace(this, args, state)
    RegExpSymbolSearch -> regexp_symbol_search(this, args, state)
    RegExpSymbolSplit -> regexp_symbol_split(this, args, state)
  }
}

/// ES2024 §22.2.3.1 RegExp(pattern, flags) — called as function.
/// Simplified: always creates a new RegExp object from string args.
fn regexp_constructor(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(pattern, flags) = case args {
    [JsString(p), JsString(f), ..] -> #(p, f)
    [JsString(p), ..] -> #(p, "")
    [JsUndefined, JsString(f), ..] -> #("", f)
    [JsUndefined, ..] | [] -> #("", "")
    // If first arg is already a RegExp object, extract pattern/flags
    [JsObject(ref), ..rest] ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: RegExpObject(pattern: p, flags: f), ..)) ->
          case rest {
            [JsString(new_flags), ..] -> #(p, new_flags)
            _ -> #(p, f)
          }
        _ -> #("", "")
      }
    _ -> #("", "")
  }

  let #(heap, ref) =
    alloc_regexp(state.heap, state.builtins.regexp.prototype, pattern, flags)
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Allocate a RegExp object on the heap.
pub fn alloc_regexp(
  h: Heap,
  regexp_proto: Ref,
  pattern: String,
  flags: String,
) -> #(Heap, Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind: RegExpObject(pattern:, flags:),
      properties: common.named_props([
        #(
          "lastIndex",
          DataProperty(
            value: JsNumber(Finite(0.0)),
            writable: True,
            enumerable: False,
            configurable: False,
          ),
        ),
      ]),
      elements: elements.new(),
      prototype: Some(regexp_proto),
      symbol_properties: [],
      extensible: True,
    ),
  )
}

/// Unwrap `this` as a RegExp or return a TypeError.
/// CPS-style — `use pattern, flags, ref, state <- require_regexp(this, state, "method")`.
fn require_regexp(
  this: JsValue,
  state: State,
  method: String,
  cont: fn(String, String, Ref, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: RegExpObject(pattern:, flags:), ..)) ->
          cont(pattern, flags, ref, state)
        _ -> not_regexp(state, method)
      }
    _ -> not_regexp(state, method)
  }
}

fn not_regexp(
  state: State,
  method: String,
) -> #(State, Result(JsValue, JsValue)) {
  state.type_error(
    state,
    "RegExp.prototype." <> method <> " requires that 'this' be a RegExp",
  )
}

/// Coerce first arg to a string, defaulting to "undefined".
/// NOTE: silently drops ToString side-effects on state — existing behavior preserved.
fn string_arg(state: State, args: List(JsValue)) -> String {
  case args {
    [arg, ..] ->
      case coerce.js_to_string(state, arg) {
        Ok(#(s, _)) -> s
        Error(_) -> "undefined"
      }
    [] -> "undefined"
  }
}

/// Read lastIndex from a RegExp object's properties.
fn read_last_index(state: State, ref: Ref) -> Int {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, Named("lastIndex")) {
        Ok(DataProperty(value: JsNumber(Finite(f)), ..)) ->
          value.float_to_int(f)
        _ -> 0
      }
    _ -> 0
  }
}

/// Write lastIndex to a RegExp object's properties.
fn write_last_index(state: State, ref: Ref, idx: Int) -> State {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(properties:, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(
              properties,
              Named("lastIndex"),
              DataProperty(
                value: JsNumber(Finite(int.to_float(idx))),
                writable: True,
                enumerable: False,
                configurable: False,
              ),
            ),
          )
        other -> other
      }
    })
  State(..state, heap:)
}

/// ES2024 §22.2.5.13 RegExp.prototype.test(string)
fn regexp_test(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use pattern, flags, ref, state <- require_regexp(this, state, "test")
  let str = case args {
    [JsString(s), ..] -> s
    _ -> "undefined"
  }
  let is_global_or_sticky =
    string.contains(flags, "g") || string.contains(flags, "y")
  case is_global_or_sticky {
    False -> #(state, Ok(JsBool(ffi_regexp_test(pattern, flags, str))))
    True -> {
      let last_index = read_last_index(state, ref)
      case ffi_regexp_exec(pattern, flags, str, last_index) {
        Ok([#(start, len), ..]) -> {
          let state = write_last_index(state, ref, start + len)
          #(state, Ok(JsBool(True)))
        }
        _ -> {
          let state = write_last_index(state, ref, 0)
          #(state, Ok(JsBool(False)))
        }
      }
    }
  }
}

/// ES2024 §22.2.5.5 RegExp.prototype.exec(string)
fn regexp_exec(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use pattern, flags, ref, state <- require_regexp(this, state, "exec")
  let str = case args {
    [JsString(s), ..] -> s
    _ -> "undefined"
  }
  let is_global_or_sticky =
    string.contains(flags, "g") || string.contains(flags, "y")
  let offset = case is_global_or_sticky {
    True -> read_last_index(state, ref)
    False -> 0
  }
  case ffi_regexp_exec(pattern, flags, str, offset) {
    Ok(captures) -> {
      // Build the result array: [full_match, group1, group2, ...]
      let #(match_strings, match_start) = case captures {
        [#(start, len), ..rest] -> {
          let full = string.slice(str, start, len)
          let groups = list.map(rest, capture_to_value(str, _))
          #([JsString(full), ..groups], start)
        }
        [] -> #([JsString("")], 0)
      }

      // Update lastIndex for global/sticky
      let state = case is_global_or_sticky, captures {
        True, [#(start, len), ..] -> write_last_index(state, ref, start + len)
        _, _ -> state
      }

      // Allocate the result array
      let #(heap, arr_ref) =
        common.alloc_array(
          state.heap,
          match_strings,
          state.builtins.array.prototype,
        )

      // Set index and input properties on the array
      let heap =
        heap.update(heap, arr_ref, fn(slot) {
          case slot {
            ObjectSlot(properties: props, ..) ->
              ObjectSlot(
                ..slot,
                properties: props
                  |> dict.insert(
                    Named("index"),
                    value.data_property(
                      JsNumber(Finite(int.to_float(match_start))),
                    ),
                  )
                  |> dict.insert(
                    Named("input"),
                    value.data_property(JsString(str)),
                  ),
              )
            other -> other
          }
        })

      #(State(..state, heap:), Ok(JsObject(arr_ref)))
    }
    Error(Nil) -> {
      // No match — reset lastIndex for global/sticky, return null
      let state = case is_global_or_sticky {
        True -> write_last_index(state, ref, 0)
        False -> state
      }
      #(state, Ok(JsNull))
    }
  }
}

/// ES2024 §22.2.5.14 RegExp.prototype.toString()
fn regexp_to_string(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use pattern, flags, _ref, state <- require_regexp(this, state, "toString")
  #(state, Ok(JsString("/" <> source_string(pattern) <> "/" <> flags)))
}

/// RegExp.prototype.source getter
fn regexp_get_source(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use pattern, _flags, _ref, state <- require_regexp(this, state, "source")
  #(state, Ok(JsString(source_string(pattern))))
}

/// RegExp.prototype.flags getter — ES §22.2.6.4
/// Returns flags in canonical spec order regardless of source order.
fn regexp_get_flags(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _pattern, flags, _ref, state <- require_regexp(this, state, "flags")
  #(state, Ok(JsString(canonical_flags(flags))))
}

/// Generic flag getter — checks if a specific flag character is in the flags string.
fn regexp_flag_getter(
  this: JsValue,
  flag: String,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use _pattern, flags, _ref, state <- require_regexp(this, state, "flag getter")
  #(state, Ok(JsBool(string.contains(flags, flag))))
}

// ---------------------------------------------------------------------------
// Symbol methods
// ---------------------------------------------------------------------------

/// ES2024 §22.2.5.8 RegExp.prototype[@@match](string)
fn regexp_symbol_match(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use pattern, flags, ref, state <- require_regexp(this, state, "[@@match]")
  let str = string_arg(state, args)
  case string.contains(flags, "g") {
    // Non-global: delegate to exec
    False -> regexp_exec(this, [JsString(str)], state)
    // Global: collect all matches
    True -> {
      let state = write_last_index(state, ref, 0)
      let #(state, matches) =
        collect_global_matches(pattern, flags, str, ref, state, [])
      case matches {
        [] -> #(state, Ok(JsNull))
        _ -> {
          let vals = list.reverse(matches)
          let #(heap, arr_ref) =
            common.alloc_array(state.heap, vals, state.builtins.array.prototype)
          #(State(..state, heap:), Ok(JsObject(arr_ref)))
        }
      }
    }
  }
}

/// Collect all global matches by looping ffi_regexp_exec.
fn collect_global_matches(
  pattern: String,
  flags: String,
  str: String,
  ref: Ref,
  state: State,
  acc: List(JsValue),
) -> #(State, List(JsValue)) {
  let last_index = read_last_index(state, ref)
  case ffi_regexp_exec(pattern, flags, str, last_index) {
    Ok([#(start, len), ..]) -> {
      let matched = string.slice(str, start, len)
      // Advance lastIndex; handle empty match by stepping +1
      let next_index = case len {
        0 -> start + 1
        _ -> start + len
      }
      let state = write_last_index(state, ref, next_index)
      collect_global_matches(pattern, flags, str, ref, state, [
        JsString(matched),
        ..acc
      ])
    }
    _ -> {
      let state = write_last_index(state, ref, 0)
      #(state, acc)
    }
  }
}

/// ES2024 §22.2.5.11 RegExp.prototype[@@search](string)
fn regexp_symbol_search(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use pattern, flags, ref, state <- require_regexp(this, state, "[@@search]")
  let str = string_arg(state, args)
  // Save previous lastIndex, set to 0, execute, restore.
  let previous_last_index = read_last_index(state, ref)
  let state = write_last_index(state, ref, 0)
  let result = case ffi_regexp_exec(pattern, flags, str, 0) {
    Ok([#(start, _), ..]) -> JsNumber(Finite(int.to_float(start)))
    _ -> JsNumber(Finite(-1.0))
  }
  let state = write_last_index(state, ref, previous_last_index)
  #(state, Ok(result))
}

/// ES2024 §22.2.5.10 RegExp.prototype[@@replace](string, replaceValue)
fn regexp_symbol_replace(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use pattern, flags, ref, state <- require_regexp(this, state, "[@@replace]")
  let str = string_arg(state, args)
  let replace_value = case args {
    [_, rv, ..] -> rv
    _ -> JsUndefined
  }
  let functional_replace = helpers.is_callable(state.heap, replace_value)

  case string.contains(flags, "g") {
    True -> {
      let state = write_last_index(state, ref, 0)
      let #(state, matches) =
        collect_replace_matches(pattern, flags, str, ref, state, [])
      let matches = list.reverse(matches)
      apply_replacements(
        str,
        matches,
        replace_value,
        functional_replace,
        state,
        0,
        "",
      )
    }
    False -> {
      let offset = case string.contains(flags, "y") {
        True -> read_last_index(state, ref)
        False -> 0
      }
      case ffi_regexp_exec(pattern, flags, str, offset) {
        Ok(captures) -> {
          let match_info = extract_match_info(str, captures)
          apply_replacements(
            str,
            [match_info],
            replace_value,
            functional_replace,
            state,
            0,
            "",
          )
        }
        Error(Nil) -> #(state, Ok(JsString(str)))
      }
    }
  }
}

/// Match info: (matched_string, position, capture_groups)
type MatchInfo {
  MatchInfo(matched: String, position: Int, captures: List(JsValue))
}

/// Extract match info from ffi_regexp_exec captures.
fn extract_match_info(str: String, captures: List(#(Int, Int))) -> MatchInfo {
  case captures {
    [#(start, len), ..rest] -> {
      let matched = string.slice(str, start, len)
      let groups = list.map(rest, capture_to_value(str, _))
      MatchInfo(matched:, position: start, captures: groups)
    }
    [] -> MatchInfo(matched: "", position: 0, captures: [])
  }
}

/// Collect all replace matches (returns MatchInfo list).
fn collect_replace_matches(
  pattern: String,
  flags: String,
  str: String,
  ref: Ref,
  state: State,
  acc: List(MatchInfo),
) -> #(State, List(MatchInfo)) {
  let last_index = read_last_index(state, ref)
  case ffi_regexp_exec(pattern, flags, str, last_index) {
    Ok(captures) -> {
      let info = extract_match_info(str, captures)
      let match_len = string.length(info.matched)
      let next_index = case match_len {
        0 -> info.position + 1
        _ -> info.position + match_len
      }
      let state = write_last_index(state, ref, next_index)
      collect_replace_matches(pattern, flags, str, ref, state, [info, ..acc])
    }
    _ -> {
      let state = write_last_index(state, ref, 0)
      #(state, acc)
    }
  }
}

/// Apply replacement for each match, building the result string.
fn apply_replacements(
  str: String,
  matches: List(MatchInfo),
  replace_value: JsValue,
  functional_replace: Bool,
  state: State,
  prev_end: Int,
  acc: String,
) -> #(State, Result(JsValue, JsValue)) {
  case matches {
    [] -> {
      // Append remainder
      let remainder = string.drop_start(str, prev_end)
      #(state, Ok(JsString(acc <> remainder)))
    }
    [match, ..rest] -> {
      // Append text before this match
      let before = string.slice(str, prev_end, match.position - prev_end)
      let acc = acc <> before

      case functional_replace {
        True -> {
          // Build args: matched, ...captures, position, str
          let call_args =
            list.flatten([
              [JsString(match.matched)],
              match.captures,
              [JsNumber(Finite(int.to_float(match.position))), JsString(str)],
            ])
          use result, state <- state.try_call(
            state,
            replace_value,
            JsUndefined,
            call_args,
          )
          use replacement, state <- coerce.try_to_string(state, result)
          let acc = acc <> replacement
          let prev_end = match.position + string.length(match.matched)
          apply_replacements(
            str,
            rest,
            replace_value,
            functional_replace,
            state,
            prev_end,
            acc,
          )
        }
        False -> {
          use template, state <- coerce.try_to_string(state, replace_value)
          case
            get_substitution(
              match.matched,
              str,
              match.position,
              match.captures,
              template,
            )
          {
            Error(Nil) -> {
              let #(heap, err) =
                common.make_range_error(
                  state.heap,
                  state.builtins,
                  "Invalid string length",
                )
              #(State(..state, heap:), Error(err))
            }
            Ok(replacement) -> {
              let acc = acc <> replacement
              let prev_end = match.position + string.length(match.matched)
              apply_replacements(
                str,
                rest,
                replace_value,
                functional_replace,
                state,
                prev_end,
                acc,
              )
            }
          }
        }
      }
    }
  }
}

/// ES2024 §22.1.3.18.1 GetSubstitution — process replacement template.
/// Returns Error(Nil) if the result would exceed limits.max_string_bytes.
fn get_substitution(
  matched: String,
  str: String,
  position: Int,
  captures: List(JsValue),
  template: String,
) -> Result(String, Nil) {
  let chars = string.to_graphemes(template)
  // Estimate output length upfront — bail immediately if it would exceed the
  // limit. This avoids building hundreds of MB of string incrementally (the
  // reason replace-math.js was slow: 32768 * 1MB = 32GB expected output).
  let estimated =
    estimate_substitution_length(matched, str, position, captures, chars, 0)
  case estimated > limits.max_string_bytes {
    True -> Error(Nil)
    False -> get_substitution_loop(matched, str, position, captures, chars, "")
  }
}

/// Estimate the output byte length of GetSubstitution without building the
/// string. Scans the template chars to compute how many bytes each $-reference
/// would contribute. Used to bail early on pathological inputs (e.g. 32768
/// backrefs each expanding to 1MB → 32GB expected output).
fn estimate_substitution_length(
  matched: String,
  str: String,
  position: Int,
  captures: List(JsValue),
  chars: List(String),
  acc: Int,
) -> Int {
  case chars {
    [] -> acc
    ["$", "$", ..rest] ->
      estimate_substitution_length(
        matched,
        str,
        position,
        captures,
        rest,
        acc + 1,
      )
    ["$", "&", ..rest] ->
      estimate_substitution_length(
        matched,
        str,
        position,
        captures,
        rest,
        acc + string.byte_size(matched),
      )
    ["$", "`", ..rest] ->
      // $` → everything before the match (position bytes for ASCII)
      estimate_substitution_length(
        matched,
        str,
        position,
        captures,
        rest,
        acc + position,
      )
    ["$", "'", ..rest] -> {
      // $' → everything after the match
      let after_len =
        string.byte_size(str) - position - string.byte_size(matched)
      let after_len = case after_len < 0 {
        True -> 0
        False -> after_len
      }
      estimate_substitution_length(
        matched,
        str,
        position,
        captures,
        rest,
        acc + after_len,
      )
    }
    ["$", d1, d2, ..rest] ->
      case two_digit_capture(captures, d1, d2) {
        Some(s) ->
          estimate_substitution_length(
            matched,
            str,
            position,
            captures,
            rest,
            acc + string.byte_size(s),
          )
        None ->
          estimate_single_digit_len(
            matched,
            str,
            position,
            captures,
            d1,
            [d2, ..rest],
            acc,
          )
      }
    ["$", d1] ->
      estimate_single_digit_len(matched, str, position, captures, d1, [], acc)
    [_ch, ..rest] ->
      estimate_substitution_length(
        matched,
        str,
        position,
        captures,
        rest,
        acc + 1,
      )
  }
}

/// Estimate length for a single-digit $N reference (mirrors try_single_digit_ref).
fn estimate_single_digit_len(
  matched: String,
  str: String,
  position: Int,
  captures: List(JsValue),
  d1: String,
  rest: List(String),
  acc: Int,
) -> Int {
  case is_digit(d1) {
    True ->
      case int.parse(d1) {
        Ok(idx) if idx >= 1 ->
          case helpers.list_at(captures, idx - 1) {
            Some(JsString(s)) ->
              estimate_substitution_length(
                matched,
                str,
                position,
                captures,
                rest,
                acc + string.byte_size(s),
              )
            _ ->
              estimate_substitution_length(
                matched,
                str,
                position,
                captures,
                rest,
                acc,
              )
          }
        _ ->
          // "$" + d1 literal
          estimate_substitution_length(
            matched,
            str,
            position,
            captures,
            rest,
            acc + 2,
          )
      }
    False ->
      // Not a digit — "$" literal + reprocess d1
      estimate_substitution_length(
        matched,
        str,
        position,
        captures,
        [d1, ..rest],
        acc + 1,
      )
  }
}

fn get_substitution_loop(
  matched: String,
  str: String,
  position: Int,
  captures: List(JsValue),
  chars: List(String),
  acc: String,
) -> Result(String, Nil) {
  case chars {
    [] -> Ok(acc)
    ["$", "$", ..rest] ->
      get_substitution_loop(matched, str, position, captures, rest, acc <> "$")
    ["$", "&", ..rest] ->
      get_substitution_loop(
        matched,
        str,
        position,
        captures,
        rest,
        acc <> matched,
      )
    ["$", "`", ..rest] -> {
      let before = string.slice(str, 0, position)
      get_substitution_loop(
        matched,
        str,
        position,
        captures,
        rest,
        acc <> before,
      )
    }
    ["$", "'", ..rest] -> {
      let after = string.drop_start(str, position + string.length(matched))
      get_substitution_loop(
        matched,
        str,
        position,
        captures,
        rest,
        acc <> after,
      )
    }
    ["$", d1, d2, ..rest] ->
      case two_digit_capture(captures, d1, d2) {
        Some(s) ->
          get_substitution_loop(
            matched,
            str,
            position,
            captures,
            rest,
            acc <> s,
          )
        None ->
          try_single_digit_ref(
            matched,
            str,
            position,
            captures,
            d1,
            [d2, ..rest],
            acc,
          )
      }
    ["$", d1] ->
      try_single_digit_ref(matched, str, position, captures, d1, [], acc)
    [ch, ..rest] ->
      get_substitution_loop(matched, str, position, captures, rest, acc <> ch)
  }
}

/// Try to interpret d1 as a single-digit capture reference ($1-$9).
/// If it's not a digit or out of range, emit literal "$" + d1.
fn try_single_digit_ref(
  matched: String,
  str: String,
  position: Int,
  captures: List(JsValue),
  d1: String,
  rest: List(String),
  acc: String,
) -> Result(String, Nil) {
  case is_digit(d1) {
    True ->
      case int.parse(d1) {
        Ok(idx) if idx >= 1 ->
          case helpers.list_at(captures, idx - 1) {
            Some(JsString(s)) ->
              get_substitution_loop(
                matched,
                str,
                position,
                captures,
                rest,
                acc <> s,
              )
            _ ->
              get_substitution_loop(matched, str, position, captures, rest, acc)
          }
        _ ->
          get_substitution_loop(
            matched,
            str,
            position,
            captures,
            rest,
            acc <> "$" <> d1,
          )
      }
    False ->
      // Not a digit at all, emit "$" literally and reprocess d1
      get_substitution_loop(
        matched,
        str,
        position,
        captures,
        [d1, ..rest],
        acc <> "$",
      )
  }
}

/// Look up a two-digit capture group $NN in the captures list. Returns
/// Some(captured_string) if d1 and d2 are digits, the index is >= 1, and
/// captures[idx-1] is a JsString. None → caller falls back to single-digit.
fn two_digit_capture(
  captures: List(JsValue),
  d1: String,
  d2: String,
) -> Option(String) {
  use <- bool.guard(!is_digit(d1) || !is_digit(d2), None)
  int.parse(d1 <> d2)
  |> option.from_result
  |> option.then(capture_string(captures, _))
}

/// Look up captures[idx-1] as a string. None if out of range, <1, or not a string.
fn capture_string(captures: List(JsValue), idx: Int) -> Option(String) {
  use <- bool.guard(idx < 1, None)
  case helpers.list_at(captures, idx - 1) {
    Some(JsString(s)) -> Some(s)
    _ -> None
  }
}

fn is_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Convert a capture tuple (start, len) to JsString slice or JsUndefined if no-match (start<0).
fn capture_to_value(str: String, cap: #(Int, Int)) -> JsValue {
  case cap {
    #(s, l) if s >= 0 -> JsString(string.slice(str, s, l))
    _ -> JsUndefined
  }
}

/// Empty pattern displays as "(?:)" per spec §22.2.5.12.
fn source_string(pattern: String) -> String {
  case pattern {
    "" -> "(?:)"
    p -> p
  }
}

/// ES2024 §22.2.5.12 RegExp.prototype[@@split](string, limit)
fn regexp_symbol_split(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use pattern, flags, _ref, state <- require_regexp(this, state, "[@@split]")
  let str = string_arg(state, args)
  let lim = case args {
    [_, JsUndefined, ..] | [_] | [] -> 4_294_967_295
    [_, l, ..] ->
      case helpers.to_number_int(l) {
        Some(n) if n >= 0 -> n
        _ -> 0
      }
  }
  let array_proto = state.builtins.array.prototype
  let alloc_result = fn(state: State, parts: List(JsValue)) {
    let #(heap, ref) = common.alloc_array(state.heap, parts, array_proto)
    #(State(..state, heap:), Ok(JsObject(ref)))
  }

  case lim, string.length(str) {
    // If limit is 0, return empty array
    0, _ -> alloc_result(state, [])
    // Empty string: if regex matches → [], else → [""]
    _, 0 ->
      case ffi_regexp_exec(pattern, flags, str, 0) {
        Ok(_) -> alloc_result(state, [])
        Error(Nil) -> alloc_result(state, [JsString(str)])
      }
    _, str_len -> {
      let parts = split_loop(pattern, flags, str, str_len, 0, 0, lim, [])
      alloc_result(state, parts)
    }
  }
}

/// Loop for regexp split: search from `search_from`, last split at `prev_end`.
fn split_loop(
  pattern: String,
  flags: String,
  str: String,
  str_len: Int,
  prev_end: Int,
  search_from: Int,
  lim: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case search_from > str_len || list.length(acc) >= lim {
    True -> {
      // Append remainder if under limit
      case list.length(acc) >= lim {
        True -> list.reverse(acc)
        False -> {
          let remainder = string.drop_start(str, prev_end)
          list.reverse([JsString(remainder), ..acc])
        }
      }
    }
    False ->
      case ffi_regexp_exec(pattern, flags, str, search_from) {
        Error(Nil) -> {
          // No more matches, append remainder
          let remainder = string.drop_start(str, prev_end)
          list.reverse([JsString(remainder), ..acc])
        }
        Ok(captures) -> {
          let #(match_start, match_end, cap_groups) = case captures {
            [#(start, len), ..rest] -> #(
              start,
              start + len,
              list.map(rest, capture_to_value(str, _)),
            )
            [] -> #(search_from, search_from, [])
          }
          // If the match is at the end of the string with zero width, skip
          case match_end == prev_end && match_start == search_from {
            True ->
              split_loop(
                pattern,
                flags,
                str,
                str_len,
                prev_end,
                search_from + 1,
                lim,
                acc,
              )
            False -> {
              // Add substring before match
              let part = string.slice(str, prev_end, match_start - prev_end)
              let acc = [JsString(part), ..acc]
              // Check limit
              case list.length(acc) >= lim {
                True -> list.reverse(acc)
                False -> {
                  // Add capture groups
                  let acc = add_captures_with_limit(acc, cap_groups, lim)
                  case list.length(acc) >= lim {
                    True -> list.reverse(acc)
                    False ->
                      split_loop(
                        pattern,
                        flags,
                        str,
                        str_len,
                        match_end,
                        match_end,
                        lim,
                        acc,
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

/// Add capture groups to accumulator, respecting the limit.
fn add_captures_with_limit(
  acc: List(JsValue),
  captures: List(JsValue),
  lim: Int,
) -> List(JsValue) {
  case captures, list.length(acc) >= lim {
    _, True -> acc
    [], _ -> acc
    [cap, ..rest], False -> add_captures_with_limit([cap, ..acc], rest, lim)
  }
}
