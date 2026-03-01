/// ES2024 §22.2 RegExp Objects
///
/// RegExp constructor, prototype methods (test, exec, toString),
/// and accessor getters (source, flags, global, ignoreCase, etc.).
/// Uses Erlang's `re` module (PCRE) via FFI for actual matching.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, type RegExpNativeFn, AccessorProperty, DataProperty,
  Dispatch, Finite, JsBool, JsNull, JsNumber, JsObject, JsString, JsUndefined,
  ObjectSlot, RegExpConstructor, RegExpGetDotAll, RegExpGetFlags, RegExpGetGlobal,
  RegExpGetHasIndices, RegExpGetIgnoreCase, RegExpGetMultiline, RegExpGetSource,
  RegExpGetSticky, RegExpGetUnicode, RegExpNative, RegExpObject,
  RegExpPrototypeExec, RegExpPrototypeTest, RegExpPrototypeToString,
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
                        value.data_property(JsNumber(Finite(int.to_float(
                          match_start,
                        )))),
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
    Ok(#(_pattern, flags, _ref)) ->
      #(state, Ok(JsBool(string.contains(flags, flag))))
    Error(Nil) ->
      frame.type_error(
        state,
        "RegExp.prototype flag getter requires that 'this' be a RegExp",
      )
  }
}
