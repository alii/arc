/// ES2024 §22.2 RegExp Objects
///
/// RegExp constructor, prototype methods (test, exec, toString),
/// and accessor getters (source, flags, global, ignoreCase, etc.).
/// Uses Erlang's `re` module (PCRE) via FFI for actual matching.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, type RegExpNativeFn, AccessorProperty, DataProperty,
  Dispatch, Finite, JsBool, JsNull, JsNumber, JsObject, JsString, JsUndefined,
  ObjectSlot, RegExpConstructor, RegExpGetDotAll, RegExpGetFlags,
  RegExpGetGlobal, RegExpGetHasIndices, RegExpGetIgnoreCase, RegExpGetMultiline,
  RegExpGetSource, RegExpGetSticky, RegExpGetUnicode, RegExpNative, RegExpObject,
  RegExpPrototypeExec, RegExpPrototypeTest, RegExpPrototypeToString,
  RegExpSymbolMatch, RegExpSymbolReplace, RegExpSymbolSearch, RegExpSymbolSplit,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

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
  let #(h, source_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetSource),
      "get source",
      0,
    )
  let #(h, flags_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetFlags),
      "get flags",
      0,
    )
  let #(h, global_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetGlobal),
      "get global",
      0,
    )
  let #(h, ignore_case_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetIgnoreCase),
      "get ignoreCase",
      0,
    )
  let #(h, multiline_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetMultiline),
      "get multiline",
      0,
    )
  let #(h, dotall_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetDotAll),
      "get dotAll",
      0,
    )
  let #(h, sticky_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetSticky),
      "get sticky",
      0,
    )
  let #(h, unicode_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetUnicode),
      "get unicode",
      0,
    )
  let #(h, has_indices_getter) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpGetHasIndices),
      "get hasIndices",
      0,
    )

  let accessor = fn(getter_ref: Ref) -> value.Property {
    AccessorProperty(
      get: Some(JsObject(getter_ref)),
      set: None,
      enumerable: False,
      configurable: True,
    )
  }

  let proto_props =
    list.flatten([
      proto_methods,
      [
        #("source", accessor(source_getter)),
        #("flags", accessor(flags_getter)),
        #("global", accessor(global_getter)),
        #("ignoreCase", accessor(ignore_case_getter)),
        #("multiline", accessor(multiline_getter)),
        #("dotAll", accessor(dotall_getter)),
        #("sticky", accessor(sticky_getter)),
        #("unicode", accessor(unicode_getter)),
        #("hasIndices", accessor(has_indices_getter)),
      ],
    ])

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
              |> dict.insert(
                value.symbol_match,
                value.builtin_property(JsObject(match_fn)),
              )
              |> dict.insert(
                value.symbol_replace,
                value.builtin_property(JsObject(replace_fn)),
              )
              |> dict.insert(
                value.symbol_search,
                value.builtin_property(JsObject(search_fn)),
              )
              |> dict.insert(
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
      properties: dict.from_list([
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
      elements: js_elements.new(),
      prototype: Some(regexp_proto),
      symbol_properties: dict.new(),
      extensible: True,
    ),
  )
}

/// Extract pattern and flags from a RegExp `this` value.
fn this_regexp_value(
  state: State,
  this: JsValue,
) -> Result(#(String, String, Ref), Nil) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: RegExpObject(pattern:, flags:), ..)) ->
          Ok(#(pattern, flags, ref))
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// Read lastIndex from a RegExp object's properties.
fn read_last_index(state: State, ref: Ref) -> Int {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, "lastIndex") {
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
              "lastIndex",
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
  case this_regexp_value(state, this) {
    Ok(#(pattern, flags, ref)) -> {
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
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype.test requires that 'this' be a RegExp",
      )
  }
}

/// ES2024 §22.2.5.5 RegExp.prototype.exec(string)
fn regexp_exec(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this_regexp_value(state, this) {
    Ok(#(pattern, flags, ref)) -> {
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
              let groups =
                list.map(rest, fn(cap) {
                  case cap {
                    #(s, l) if s >= 0 -> JsString(string.slice(str, s, l))
                    _ -> JsUndefined
                  }
                })
              #([JsString(full), ..groups], start)
            }
            [] -> #([JsString("")], 0)
          }

          // Update lastIndex for global/sticky
          let state = case is_global_or_sticky, captures {
            True, [#(start, len), ..] ->
              write_last_index(state, ref, start + len)
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
                        "index",
                        value.data_property(
                          JsNumber(Finite(int.to_float(match_start))),
                        ),
                      )
                      |> dict.insert(
                        "input",
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
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype.exec requires that 'this' be a RegExp",
      )
  }
}

/// ES2024 §22.2.5.14 RegExp.prototype.toString()
fn regexp_to_string(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this_regexp_value(state, this) {
    Ok(#(pattern, flags, _ref)) -> {
      let source = case pattern {
        "" -> "(?:)"
        p -> p
      }
      #(state, Ok(JsString("/" <> source <> "/" <> flags)))
    }
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype.toString requires that 'this' be a RegExp",
      )
  }
}

/// RegExp.prototype.source getter
fn regexp_get_source(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this_regexp_value(state, this) {
    Ok(#(pattern, _flags, _ref)) -> {
      let source = case pattern {
        "" -> "(?:)"
        p -> p
      }
      #(state, Ok(JsString(source)))
    }
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype.source requires that 'this' be a RegExp",
      )
  }
}

/// RegExp.prototype.flags getter
fn regexp_get_flags(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this_regexp_value(state, this) {
    Ok(#(_pattern, flags, _ref)) -> #(state, Ok(JsString(flags)))
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype.flags requires that 'this' be a RegExp",
      )
  }
}

/// Generic flag getter — checks if a specific flag character is in the flags string.
fn regexp_flag_getter(
  this: JsValue,
  flag: String,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this_regexp_value(state, this) {
    Ok(#(_pattern, flags, _ref)) -> #(
      state,
      Ok(JsBool(string.contains(flags, flag))),
    )
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype flag getter requires that 'this' be a RegExp",
      )
  }
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
  case this_regexp_value(state, this) {
    Ok(#(pattern, flags, ref)) -> {
      let str = case args {
        [arg, ..] ->
          case frame.to_string(state, arg) {
            Ok(#(s, _)) -> s
            _ -> "undefined"
          }
        [] -> "undefined"
      }
      let is_global = string.contains(flags, "g")
      case is_global {
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
                common.alloc_array(
                  state.heap,
                  vals,
                  state.builtins.array.prototype,
                )
              #(State(..state, heap:), Ok(JsObject(arr_ref)))
            }
          }
        }
      }
    }
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype[@@match] requires that 'this' be a RegExp",
      )
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
  case this_regexp_value(state, this) {
    Ok(#(pattern, flags, ref)) -> {
      let str = case args {
        [arg, ..] ->
          case frame.to_string(state, arg) {
            Ok(#(s, _)) -> s
            _ -> "undefined"
          }
        [] -> "undefined"
      }
      // Save previous lastIndex, set to 0
      let previous_last_index = read_last_index(state, ref)
      let state = write_last_index(state, ref, 0)
      // Execute
      let result = case ffi_regexp_exec(pattern, flags, str, 0) {
        Ok([#(start, _), ..]) -> JsNumber(Finite(int.to_float(start)))
        _ -> JsNumber(Finite(-1.0))
      }
      // Restore lastIndex
      let state = write_last_index(state, ref, previous_last_index)
      #(state, Ok(result))
    }
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype[@@search] requires that 'this' be a RegExp",
      )
  }
}

/// ES2024 §22.2.5.10 RegExp.prototype[@@replace](string, replaceValue)
fn regexp_symbol_replace(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this_regexp_value(state, this) {
    Ok(#(pattern, flags, ref)) -> {
      let str = case args {
        [arg, ..] ->
          case frame.to_string(state, arg) {
            Ok(#(s, _)) -> s
            _ -> "undefined"
          }
        [] -> "undefined"
      }
      let replace_value = case args {
        [_, rv, ..] -> rv
        _ -> JsUndefined
      }
      let functional_replace = helpers.is_callable(state.heap, replace_value)
      let is_global = string.contains(flags, "g")

      // Collect matches
      case is_global {
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
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype[@@replace] requires that 'this' be a RegExp",
      )
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
      let groups =
        list.map(rest, fn(cap) {
          case cap {
            #(s, l) if s >= 0 -> JsString(string.slice(str, s, l))
            _ -> JsUndefined
          }
        })
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
          case frame.call(state, replace_value, JsUndefined, call_args) {
            Ok(#(result, state)) ->
              case frame.to_string(state, result) {
                Ok(#(replacement, state)) -> {
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
                Error(#(thrown, state)) -> #(state, Error(thrown))
              }
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        }
        False -> {
          case frame.to_string(state, replace_value) {
            Ok(#(template, state)) -> {
              let replacement =
                get_substitution(
                  match.matched,
                  str,
                  match.position,
                  match.captures,
                  template,
                )
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
            Error(#(thrown, state)) -> #(state, Error(thrown))
          }
        }
      }
    }
  }
}

/// ES2024 §22.1.3.18.1 GetSubstitution — process replacement template
fn get_substitution(
  matched: String,
  str: String,
  position: Int,
  captures: List(JsValue),
  template: String,
) -> String {
  get_substitution_loop(
    matched,
    str,
    position,
    captures,
    string.to_graphemes(template),
    "",
  )
}

fn get_substitution_loop(
  matched: String,
  str: String,
  position: Int,
  captures: List(JsValue),
  chars: List(String),
  acc: String,
) -> String {
  case chars {
    [] -> acc
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
    ["$", d1, d2, ..rest] -> {
      // Try two-digit capture reference ($10-$99), then single-digit ($1-$9)
      case is_digit(d1) && is_digit(d2) {
        True -> {
          let idx_str = d1 <> d2
          case int.parse(idx_str) {
            Ok(idx) if idx >= 1 ->
              case list_at(captures, idx - 1) {
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
                  // Two-digit ref out of range, try single-digit
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
            _ ->
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
        }
        False ->
          // d1 might be a single digit, d2 is not
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
    }
    // Handle "$" followed by exactly one more char (end of template)
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
) -> String {
  case is_digit(d1) {
    True ->
      case int.parse(d1) {
        Ok(idx) if idx >= 1 ->
          case list_at(captures, idx - 1) {
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

fn is_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn list_at(lst: List(a), idx: Int) -> option.Option(a) {
  case idx, lst {
    0, [x, ..] -> Some(x)
    n, [_, ..rest] if n > 0 -> list_at(rest, n - 1)
    _, _ -> None
  }
}

/// ES2024 §22.2.5.12 RegExp.prototype[@@split](string, limit)
fn regexp_symbol_split(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this_regexp_value(state, this) {
    Ok(#(pattern, flags, _ref)) -> {
      let str = case args {
        [arg, ..] ->
          case frame.to_string(state, arg) {
            Ok(#(s, _)) -> s
            _ -> "undefined"
          }
        [] -> "undefined"
      }
      let limit_val = case args {
        [_, l, ..] -> l
        _ -> JsUndefined
      }
      let lim = case limit_val {
        JsUndefined -> 4_294_967_295
        _ ->
          case helpers.to_number_int(limit_val) {
            Some(n) if n >= 0 -> n
            _ -> 0
          }
      }
      let array_proto = state.builtins.array.prototype

      // If limit is 0, return empty array
      case lim {
        0 -> {
          let #(heap, ref) = common.alloc_array(state.heap, [], array_proto)
          #(State(..state, heap:), Ok(JsObject(ref)))
        }
        _ -> {
          let str_len = string.length(str)
          // Empty string: try to match; if match → [], else → [str]
          case str_len {
            0 ->
              case ffi_regexp_exec(pattern, flags, str, 0) {
                Ok(_) -> {
                  let #(heap, ref) =
                    common.alloc_array(state.heap, [], array_proto)
                  #(State(..state, heap:), Ok(JsObject(ref)))
                }
                Error(Nil) -> {
                  let #(heap, ref) =
                    common.alloc_array(state.heap, [JsString(str)], array_proto)
                  #(State(..state, heap:), Ok(JsObject(ref)))
                }
              }
            _ -> {
              let parts =
                split_loop(pattern, flags, str, str_len, 0, 0, lim, [])
              let #(heap, ref) =
                common.alloc_array(state.heap, parts, array_proto)
              #(State(..state, heap:), Ok(JsObject(ref)))
            }
          }
        }
      }
    }
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype[@@split] requires that 'this' be a RegExp",
      )
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
            [#(start, len), ..rest] -> {
              let groups =
                list.map(rest, fn(cap) {
                  case cap {
                    #(s, l) if s >= 0 -> JsString(string.slice(str, s, l))
                    _ -> JsUndefined
                  }
                })
              #(start, start + len, groups)
            }
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
