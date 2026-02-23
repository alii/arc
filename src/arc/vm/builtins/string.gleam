import arc/vm/builtins/common.{alloc_proto}
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, ArrayObject, Finite, JsNumber, JsObject, JsString,
  JsUndefined, NaN, NativeFunction, NativeStringPrototypeAt,
  NativeStringPrototypeCharAt, NativeStringPrototypeCharCodeAt,
  NativeStringPrototypeConcat, NativeStringPrototypeEndsWith,
  NativeStringPrototypeIncludes, NativeStringPrototypeIndexOf,
  NativeStringPrototypeLastIndexOf, NativeStringPrototypePadEnd,
  NativeStringPrototypePadStart, NativeStringPrototypeRepeat,
  NativeStringPrototypeSlice, NativeStringPrototypeSplit,
  NativeStringPrototypeStartsWith, NativeStringPrototypeSubstring,
  NativeStringPrototypeToLowerCase, NativeStringPrototypeToString,
  NativeStringPrototypeToUpperCase, NativeStringPrototypeTrim,
  NativeStringPrototypeTrimEnd, NativeStringPrototypeTrimStart,
  NativeStringPrototypeValueOf, ObjectSlot,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

/// Set up String.prototype.
/// String is NOT a constructor yet — only the prototype for primitive access.
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  let #(h, string_proto) = alloc_proto(h, Some(object_proto), dict.new())

  // Add all String.prototype methods
  let methods = [
    #("charAt", NativeStringPrototypeCharAt, 1),
    #("charCodeAt", NativeStringPrototypeCharCodeAt, 1),
    #("indexOf", NativeStringPrototypeIndexOf, 1),
    #("lastIndexOf", NativeStringPrototypeLastIndexOf, 1),
    #("includes", NativeStringPrototypeIncludes, 1),
    #("startsWith", NativeStringPrototypeStartsWith, 1),
    #("endsWith", NativeStringPrototypeEndsWith, 1),
    #("slice", NativeStringPrototypeSlice, 2),
    #("substring", NativeStringPrototypeSubstring, 2),
    #("toLowerCase", NativeStringPrototypeToLowerCase, 0),
    #("toUpperCase", NativeStringPrototypeToUpperCase, 0),
    #("trim", NativeStringPrototypeTrim, 0),
    #("trimStart", NativeStringPrototypeTrimStart, 0),
    #("trimEnd", NativeStringPrototypeTrimEnd, 0),
    #("split", NativeStringPrototypeSplit, 2),
    #("concat", NativeStringPrototypeConcat, 1),
    #("toString", NativeStringPrototypeToString, 0),
    #("valueOf", NativeStringPrototypeValueOf, 0),
    #("repeat", NativeStringPrototypeRepeat, 1),
    #("padStart", NativeStringPrototypePadStart, 1),
    #("padEnd", NativeStringPrototypePadEnd, 1),
    #("at", NativeStringPrototypeAt, 1),
  ]

  let h =
    list.fold(methods, h, fn(h, method) {
      let #(name, native, length) = method
      let #(h, fn_ref) =
        alloc_native_fn(h, function_proto, native, name, length)
      add_method(h, string_proto, name, fn_ref)
    })

  #(h, string_proto)
}

// ============================================================================
// String.prototype method implementations
// ============================================================================

/// String.prototype.charAt(index)
pub fn string_char_at(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let idx = get_int_arg(args, 0, 0)
  let len = string.length(s)
  case idx >= 0 && idx < len {
    True -> #(heap, Ok(JsString(string.slice(s, idx, 1))))
    False -> #(heap, Ok(JsString("")))
  }
}

/// String.prototype.charCodeAt(index)
pub fn string_char_code_at(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let idx = get_int_arg(args, 0, 0)
  let len = string.length(s)
  case idx >= 0 && idx < len {
    True -> {
      let ch = string.slice(s, idx, 1)
      case string.to_utf_codepoints(ch) {
        [cp, ..] -> {
          let code = string.utf_codepoint_to_int(cp)
          #(heap, Ok(JsNumber(Finite(int.to_float(code)))))
        }
        [] -> #(heap, Ok(JsNumber(NaN)))
      }
    }
    False -> #(heap, Ok(JsNumber(NaN)))
  }
}

/// String.prototype.indexOf(searchString, position?)
pub fn string_index_of(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let search = case args {
    [v, ..] -> value.to_js_string(v)
    [] -> "undefined"
  }
  let from = get_int_arg_or(args, 1, 0)
  let result = index_of_from(s, search, from)
  #(heap, Ok(JsNumber(Finite(int.to_float(result)))))
}

/// String.prototype.lastIndexOf(searchString, position?)
pub fn string_last_index_of(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let search = case args {
    [v, ..] -> value.to_js_string(v)
    [] -> "undefined"
  }
  let len = string.length(s)
  let from = case args {
    [_, pos_val, ..] ->
      case to_number_int(pos_val) {
        Ok(n) -> int.min(n, len)
        Error(_) -> len
      }
    _ -> len
  }
  let result = last_index_of_from(s, search, from)
  #(heap, Ok(JsNumber(Finite(int.to_float(result)))))
}

/// String.prototype.includes(searchString, position?)
pub fn string_includes(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let search = case args {
    [v, ..] -> value.to_js_string(v)
    [] -> "undefined"
  }
  let from = get_int_arg_or(args, 1, 0)
  let sub = string.drop_start(s, from)
  #(heap, Ok(value.JsBool(string.contains(sub, search))))
}

/// String.prototype.startsWith(searchString, position?)
pub fn string_starts_with(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let search = case args {
    [v, ..] -> value.to_js_string(v)
    [] -> "undefined"
  }
  let from = get_int_arg_or(args, 1, 0)
  let sub = string.drop_start(s, from)
  #(heap, Ok(value.JsBool(string.starts_with(sub, search))))
}

/// String.prototype.endsWith(searchString, endPosition?)
pub fn string_ends_with(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let search = case args {
    [v, ..] -> value.to_js_string(v)
    [] -> "undefined"
  }
  let len = string.length(s)
  let end_pos = case args {
    [_, pos_val, ..] ->
      case pos_val {
        JsUndefined -> len
        _ ->
          case to_number_int(pos_val) {
            Ok(n) -> int.clamp(n, 0, len)
            Error(_) -> len
          }
      }
    _ -> len
  }
  let sub = string.slice(s, 0, end_pos)
  #(heap, Ok(value.JsBool(string.ends_with(sub, search))))
}

/// String.prototype.slice(start, end?)
pub fn string_slice(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let len = string.length(s)
  let start = case args {
    [v, ..] ->
      case to_number_int(v) {
        Ok(n) ->
          case n < 0 {
            True -> int.max(len + n, 0)
            False -> int.min(n, len)
          }
        Error(_) -> 0
      }
    [] -> 0
  }
  let end = case args {
    [_, JsUndefined, ..] -> len
    [_, v, ..] ->
      case to_number_int(v) {
        Ok(n) ->
          case n < 0 {
            True -> int.max(len + n, 0)
            False -> int.min(n, len)
          }
        Error(_) -> 0
      }
    _ -> len
  }
  case end > start {
    True -> #(heap, Ok(JsString(string.slice(s, start, end - start))))
    False -> #(heap, Ok(JsString("")))
  }
}

/// String.prototype.substring(start, end?)
pub fn string_substring(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let len = string.length(s)
  let raw_start = case args {
    [v, ..] ->
      case to_number_int(v) {
        Ok(n) -> n
        Error(_) -> 0
      }
    [] -> 0
  }
  let raw_end = case args {
    [_, JsUndefined, ..] -> len
    [_, v, ..] ->
      case to_number_int(v) {
        Ok(n) -> n
        Error(_) -> 0
      }
    _ -> len
  }
  // Clamp to [0, len], then swap if start > end
  let start = int.clamp(raw_start, 0, len)
  let end = int.clamp(raw_end, 0, len)
  let #(start, end) = case start > end {
    True -> #(end, start)
    False -> #(start, end)
  }
  #(heap, Ok(JsString(string.slice(s, start, end - start))))
}

/// String.prototype.toLowerCase()
pub fn string_to_lower_case(
  this: JsValue,
  _args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  #(heap, Ok(JsString(string.lowercase(s))))
}

/// String.prototype.toUpperCase()
pub fn string_to_upper_case(
  this: JsValue,
  _args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  #(heap, Ok(JsString(string.uppercase(s))))
}

/// String.prototype.trim()
pub fn string_trim(
  this: JsValue,
  _args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  #(heap, Ok(JsString(string.trim(s))))
}

/// String.prototype.trimStart()
pub fn string_trim_start(
  this: JsValue,
  _args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  #(heap, Ok(JsString(string.trim_start(s))))
}

/// String.prototype.trimEnd()
pub fn string_trim_end(
  this: JsValue,
  _args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  #(heap, Ok(JsString(string.trim_end(s))))
}

/// String.prototype.split(separator, limit?)
pub fn string_split(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  case args {
    [JsUndefined, ..] | [] -> {
      // No separator: return [this_string]
      let #(heap, ref) =
        heap.alloc(
          heap,
          ObjectSlot(
            kind: ArrayObject(1),
            properties: dict.new(),
            elements: dict.from_list([#(0, JsString(s))]),
            prototype: Some(array_proto),
          ),
        )
      #(heap, Ok(JsObject(ref)))
    }
    [sep_val, ..] -> {
      let sep = value.to_js_string(sep_val)
      let parts = case sep {
        "" -> string.to_graphemes(s) |> list.map(JsString)
        _ -> string.split(s, sep) |> list.map(JsString)
      }
      // Apply limit if provided
      let parts = case args {
        [_, limit_val, ..] ->
          case to_number_int(limit_val) {
            Ok(limit) -> list.take(parts, limit)
            Error(_) -> parts
          }
        _ -> parts
      }
      let count = list.length(parts)
      let elems =
        parts
        |> list.index_map(fn(val, idx) { #(idx, val) })
        |> dict.from_list()
      let #(heap, ref) =
        heap.alloc(
          heap,
          ObjectSlot(
            kind: ArrayObject(count),
            properties: dict.new(),
            elements: elems,
            prototype: Some(array_proto),
          ),
        )
      #(heap, Ok(JsObject(ref)))
    }
  }
}

/// String.prototype.concat(...args)
pub fn string_concat(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let result =
    list.fold(args, s, fn(acc, arg) { acc <> value.to_js_string(arg) })
  #(heap, Ok(JsString(result)))
}

/// String.prototype.toString()
pub fn string_to_string(
  this: JsValue,
  _args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  #(heap, Ok(JsString(coerce_to_string(this))))
}

/// String.prototype.valueOf()
pub fn string_value_of(
  this: JsValue,
  _args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  #(heap, Ok(JsString(coerce_to_string(this))))
}

/// String.prototype.repeat(count)
pub fn string_repeat(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let count = get_int_arg(args, 0, 0)
  case count < 0 {
    True -> #(heap, Ok(JsString("")))
    False -> #(heap, Ok(JsString(string.repeat(s, count))))
  }
}

/// String.prototype.padStart(targetLength, padString?)
pub fn string_pad_start(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let target_len = get_int_arg(args, 0, 0)
  let pad = case args {
    [_, v, ..] ->
      case v {
        JsUndefined -> " "
        _ -> value.to_js_string(v)
      }
    _ -> " "
  }
  #(heap, Ok(JsString(string.pad_start(s, target_len, pad))))
}

/// String.prototype.padEnd(targetLength, padString?)
pub fn string_pad_end(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let target_len = get_int_arg(args, 0, 0)
  let pad = case args {
    [_, v, ..] ->
      case v {
        JsUndefined -> " "
        _ -> value.to_js_string(v)
      }
    _ -> " "
  }
  #(heap, Ok(JsString(string.pad_end(s, target_len, pad))))
}

/// String.prototype.at(index) — relative indexing
pub fn string_at(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let s = coerce_to_string(this)
  let idx = get_int_arg(args, 0, 0)
  let len = string.length(s)
  let actual_idx = case idx < 0 {
    True -> len + idx
    False -> idx
  }
  case actual_idx >= 0 && actual_idx < len {
    True -> #(heap, Ok(JsString(string.slice(s, actual_idx, 1))))
    False -> #(heap, Ok(JsUndefined))
  }
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Extract the string value from `this`. Primitive strings are the common case.
fn coerce_to_string(this: JsValue) -> String {
  case this {
    JsString(s) -> s
    _ -> value.to_js_string(this)
  }
}

/// Get an integer argument at position `idx` with a default value.
fn get_int_arg(args: List(JsValue), idx: Int, default: Int) -> Int {
  case list_at(args, idx) {
    Some(v) ->
      case to_number_int(v) {
        Ok(n) -> n
        Error(_) -> default
      }
    None -> default
  }
}

/// Get an integer from arg at position `idx` with fallback.
/// Unlike get_int_arg, this checks if the arg exists and returns default only if missing.
fn get_int_arg_or(args: List(JsValue), idx: Int, default: Int) -> Int {
  get_int_arg(args, idx, default)
}

/// Convert a JsValue to an integer (truncating).
fn to_number_int(val: JsValue) -> Result(Int, Nil) {
  case val {
    JsNumber(Finite(n)) -> Ok(float_to_int(n))
    JsNumber(_) -> Error(Nil)
    JsUndefined -> Error(Nil)
    value.JsNull -> Ok(0)
    value.JsBool(True) -> Ok(1)
    value.JsBool(False) -> Ok(0)
    JsString(s) ->
      case int.parse(s) {
        Ok(n) -> Ok(n)
        Error(_) ->
          case gleam_stdlib_parse_float(s) {
            Ok(f) -> Ok(float_to_int(f))
            Error(_) -> Error(Nil)
          }
      }
    _ -> Error(Nil)
  }
}

fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float.truncate(float.negate(f))
    False -> float.truncate(f)
  }
}

fn list_at(lst: List(a), idx: Int) -> option.Option(a) {
  case idx, lst {
    0, [x, ..] -> Some(x)
    n, [_, ..rest] -> list_at(rest, n - 1)
    _, [] -> None
  }
}

/// Find first occurrence of `search` in `s` starting from index `from`.
fn index_of_from(s: String, search: String, from: Int) -> Int {
  let len = string.length(s)
  let search_len = string.length(search)
  // Empty search at valid position returns that position
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
    False -> index_of_loop(s, search, int.max(from, 0), len, search_len)
  }
}

fn index_of_loop(
  s: String,
  search: String,
  pos: Int,
  len: Int,
  search_len: Int,
) -> Int {
  case pos + search_len > len {
    True -> -1
    False ->
      case string.slice(s, pos, search_len) == search {
        True -> pos
        False -> index_of_loop(s, search, pos + 1, len, search_len)
      }
  }
}

/// Find last occurrence of `search` in `s` starting from index `from`.
fn last_index_of_from(s: String, search: String, from: Int) -> Int {
  let len = string.length(s)
  let search_len = string.length(search)
  case search_len == 0 {
    True -> int.min(from, len)
    False -> {
      let start = int.min(from, len - search_len)
      last_index_of_loop(s, search, start, search_len)
    }
  }
}

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

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)

/// Allocate a native function object on the heap, root it, and return the ref.
fn alloc_native_fn(
  h: Heap,
  function_proto: Ref,
  native: value.NativeFn,
  name: String,
  length: Int,
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(native),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString(name))),
          #(
            "length",
            value.builtin_property(JsNumber(Finite(int.to_float(length)))),
          ),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Add a non-enumerable method property to an object on the heap.
fn add_method(h: Heap, obj_ref: Ref, name: String, fn_ref: Ref) -> Heap {
  case heap.read(h, obj_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props =
        dict.insert(properties, name, value.builtin_property(JsObject(fn_ref)))
      heap.write(
        h,
        obj_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }
}
