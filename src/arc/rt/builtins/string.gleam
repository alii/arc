//// `rt_builtins/string` — String constructor + %String.prototype%
//// (ES2024 §22.1). Port of `arc/vm/builtins/string.gleam` over the threaded
//// `Agent` model (D7/R1).

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/builtins/regexp
import arc/rt/builtins/substitution
import arc/rt/call as rt_call
import arc/rt/js_string
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type StringNative,
  type SymbolId, JFloat, JInt, JNan, KHandle, KNull, KStr, KUndef, Named,
  NoElements, SObject, StringConstructor, StringFromCharCode,
  StringFromCodePoint, StringIterator, StringKey, StringN, StringObj,
  StringPrototypeAnchor, StringPrototypeAt, StringPrototypeBig,
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
  StringPrototypeTrimStart, StringPrototypeValueOf, StringRaw, SymbolKey,
  classify, mk_bool, mk_number, mk_object, mk_string, mk_undefined,
  well_known_symbol_description,
} as rt_types
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string

/// Set up String constructor + String.prototype (§22.1.2 / §22.1.3).
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("charAt", StringN(StringPrototypeCharAt), 1),
      #("charCodeAt", StringN(StringPrototypeCharCodeAt), 1),
      #("indexOf", StringN(StringPrototypeIndexOf), 1),
      #("lastIndexOf", StringN(StringPrototypeLastIndexOf), 1),
      #("includes", StringN(StringPrototypeIncludes), 1),
      #("startsWith", StringN(StringPrototypeStartsWith), 1),
      #("endsWith", StringN(StringPrototypeEndsWith), 1),
      #("slice", StringN(StringPrototypeSlice), 2),
      #("substring", StringN(StringPrototypeSubstring), 2),
      #("toLowerCase", StringN(StringPrototypeToLowerCase), 0),
      #("toUpperCase", StringN(StringPrototypeToUpperCase), 0),
      #("toLocaleLowerCase", StringN(StringPrototypeToLocaleLowerCase), 0),
      #("toLocaleUpperCase", StringN(StringPrototypeToLocaleUpperCase), 0),
      #("trim", StringN(StringPrototypeTrim), 0),
      #("trimStart", StringN(StringPrototypeTrimStart), 0),
      #("trimEnd", StringN(StringPrototypeTrimEnd), 0),
      #("trimLeft", StringN(StringPrototypeTrimStart), 0),
      #("trimRight", StringN(StringPrototypeTrimEnd), 0),
      #("split", StringN(StringPrototypeSplit), 2),
      #("concat", StringN(StringPrototypeConcat), 1),
      #("toString", StringN(StringPrototypeToString), 0),
      #("valueOf", StringN(StringPrototypeValueOf), 0),
      #("repeat", StringN(StringPrototypeRepeat), 1),
      #("padStart", StringN(StringPrototypePadStart), 1),
      #("padEnd", StringN(StringPrototypePadEnd), 1),
      #("at", StringN(StringPrototypeAt), 1),
      #("codePointAt", StringN(StringPrototypeCodePointAt), 1),
      #("normalize", StringN(StringPrototypeNormalize), 0),
      #("match", StringN(StringPrototypeMatch), 1),
      #("search", StringN(StringPrototypeSearch), 1),
      #("replace", StringN(StringPrototypeReplace), 2),
      #("replaceAll", StringN(StringPrototypeReplaceAll), 2),
      #("substr", StringN(StringPrototypeSubstr), 2),
      #("localeCompare", StringN(StringPrototypeLocaleCompare), 1),
      #("matchAll", StringN(StringPrototypeMatchAll), 1),
      #("isWellFormed", StringN(StringPrototypeIsWellFormed), 0),
      #("toWellFormed", StringN(StringPrototypeToWellFormed), 0),
      // Annex B HTML wrapper methods
      #("anchor", StringN(StringPrototypeAnchor), 1),
      #("big", StringN(StringPrototypeBig), 0),
      #("blink", StringN(StringPrototypeBlink), 0),
      #("bold", StringN(StringPrototypeBold), 0),
      #("fixed", StringN(StringPrototypeFixed), 0),
      #("fontcolor", StringN(StringPrototypeFontcolor), 1),
      #("fontsize", StringN(StringPrototypeFontsize), 1),
      #("italics", StringN(StringPrototypeItalics), 0),
      #("link", StringN(StringPrototypeLink), 1),
      #("small", StringN(StringPrototypeSmall), 0),
      #("strike", StringN(StringPrototypeStrike), 0),
      #("sub", StringN(StringPrototypeSub), 0),
      #("sup", StringN(StringPrototypeSup), 0),
    ])
  // Static methods on the String constructor.
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("raw", StringN(StringRaw), 1),
      #("fromCharCode", StringN(StringFromCharCode), 1),
      #("fromCodePoint", StringN(StringFromCodePoint), 1),
    ])
  // §22.1.3: the String prototype object is itself a String exotic object
  // with [[StringData]] = "".
  let #(bt, st) =
    common.init_wrapper_type(
      st,
      object_proto,
      fn_proto,
      proto_methods,
      fn(_) { StringN(StringConstructor) },
      "String",
      1,
      static_methods,
      proto_kind: StringObj(value: ""),
    )
  // §22.1.3.36 String.prototype [ @@iterator ] ( ) — yields code points.
  let #(iter_fn, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      StringN(StringPrototypeSymbolIterator),
      "[Symbol.iterator]",
      0,
    )
  let #(iter_prop, st) = common.builtin_property(st, mk_object(iter_fn))
  let st =
    common.add_symbol_property(
      st,
      bt.prototype,
      rt_types.symbol_iterator,
      iter_prop,
    )
  #(bt, st)
}

/// Per-module dispatch for String native functions.
pub fn dispatch(
  st: Agent,
  native: StringNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    StringConstructor -> call_as_function(st, args)
    StringPrototypeSymbolIterator -> string_symbol_iterator(st, this)
    StringPrototypeCharAt -> string_char_at(st, this, args)
    StringPrototypeCharCodeAt -> string_char_code_at(st, this, args)
    StringPrototypeIndexOf -> string_index_of(st, this, args)
    StringPrototypeLastIndexOf -> string_last_index_of(st, this, args)
    StringPrototypeIncludes -> string_includes(st, this, args)
    StringPrototypeStartsWith -> string_starts_with(st, this, args)
    StringPrototypeEndsWith -> string_ends_with(st, this, args)
    StringPrototypeSlice -> string_slice(st, this, args)
    StringPrototypeSubstring -> string_substring(st, this, args)
    StringPrototypeToLowerCase | StringPrototypeToLocaleLowerCase ->
      string_transform(st, this, to_lower_case)
    StringPrototypeToUpperCase | StringPrototypeToLocaleUpperCase ->
      string_transform(st, this, string.uppercase)
    StringPrototypeTrim -> string_transform(st, this, trim_js_ws)
    StringPrototypeTrimStart -> string_transform(st, this, trim_leading_js_ws)
    StringPrototypeTrimEnd -> string_transform(st, this, trim_trailing_js_ws)
    StringPrototypeSplit -> string_split(st, this, args)
    StringPrototypeConcat -> string_concat(st, this, args)
    StringPrototypeToString -> string_this_value(st, this, "toString")
    StringPrototypeValueOf -> string_this_value(st, this, "valueOf")
    StringPrototypeRepeat -> string_repeat(st, this, args)
    StringPrototypePadStart -> string_pad(st, this, args, limits.pad_start)
    StringPrototypePadEnd -> string_pad(st, this, args, limits.pad_end)
    StringPrototypeAt -> string_at(st, this, args)
    StringPrototypeCodePointAt -> string_code_point_at(st, this, args)
    StringPrototypeNormalize -> string_normalize(st, this, args)
    StringPrototypeMatch -> string_match(st, this, args)
    StringPrototypeSearch -> string_search(st, this, args)
    StringPrototypeReplace -> string_replace(st, this, args)
    StringPrototypeReplaceAll -> string_replace_all(st, this, args)
    StringPrototypeSubstr -> string_substr(st, this, args)
    StringPrototypeLocaleCompare -> string_locale_compare(st, this, args)
    StringPrototypeMatchAll -> string_match_all(st, this, args)
    StringPrototypeIsWellFormed -> string_is_well_formed(st, this)
    StringPrototypeToWellFormed -> string_transform(st, this, fn(s) { s })
    // Annex B HTML wrapper methods
    StringPrototypeAnchor -> html_wrap_attr(st, this, args, "a", "name")
    StringPrototypeBig -> html_wrap(st, this, "big")
    StringPrototypeBlink -> html_wrap(st, this, "blink")
    StringPrototypeBold -> html_wrap(st, this, "b")
    StringPrototypeFixed -> html_wrap(st, this, "tt")
    StringPrototypeFontcolor -> html_wrap_attr(st, this, args, "font", "color")
    StringPrototypeFontsize -> html_wrap_attr(st, this, args, "font", "size")
    StringPrototypeItalics -> html_wrap(st, this, "i")
    StringPrototypeLink -> html_wrap_attr(st, this, args, "a", "href")
    StringPrototypeSmall -> html_wrap(st, this, "small")
    StringPrototypeStrike -> html_wrap(st, this, "strike")
    StringPrototypeSub -> html_wrap(st, this, "sub")
    StringPrototypeSup -> html_wrap(st, this, "sup")
    // Statics
    StringRaw -> string_raw(st, args)
    StringFromCharCode -> string_from_char_code(st, args)
    StringFromCodePoint -> string_from_code_point(st, args)
  }
}

// ── String constructor ──────────────────────────────────────────────────────

/// §22.1.1.1 String(value) called as a function. Step 1.a: no args → "".
/// Step 1.b: symbol → SymbolDescriptiveString (unlike ToString which throws).
/// `new String` is intercepted in `t_construct` before dispatch.
fn call_as_function(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case args {
    [] -> #(mk_string(""), st)
    [v, ..] ->
      case classify(v) {
        rt_types.KSym(id) -> #(
          mk_string(rt_types.symbol_descriptive_string(id)),
          st,
        )
        _ -> {
          let #(s, st) = rt_val.t_to_string(st, v)
          #(mk_string(s), st)
        }
      }
  }
}

// ── §22.1.3 String.prototype methods ────────────────────────────────────────

/// §22.1.3.36 String.prototype [ @@iterator ] ( ).
fn string_symbol_iterator(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let realm = st.realm
  let #(iter_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: StringIterator(source: s, index: 0),
        proto: Some(realm.string_iter_proto),
        props: common.named_props([]),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(iter_h), st)
}

/// §22.1.3.1 String.prototype.charAt ( pos ).
fn string_char_at(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(idx, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  case idx >= 0 {
    True ->
      case js_string.char_at(s, idx) {
        Some(ch) -> #(mk_string(ch), st)
        None -> #(mk_string(""), st)
      }
    False -> #(mk_string(""), st)
  }
}

/// §22.1.3.2 String.prototype.charCodeAt ( pos ).
fn string_char_code_at(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(idx, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let ch = case idx >= 0 {
    True -> js_string.char_at(s, idx)
    False -> None
  }
  case ch {
    Some(ch) ->
      case string.to_utf_codepoints(ch) {
        [cp, ..] -> #(mk_number(JInt(string.utf_codepoint_to_int(cp))), st)
        [] -> #(mk_number(JNan), st)
      }
    None -> #(mk_number(JNan), st)
  }
}

/// §22.1.3.9 String.prototype.indexOf ( searchString [ , position ] ).
fn string_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(search, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let #(pos, st) = rt_val.t_to_integer_or_infinity(st, helpers.arg_at(args, 1))
  let from = int.clamp(pos, 0, js_string.length(s))
  let result = js_string.index_of(s, search, from) |> option.unwrap(-1)
  #(mk_number(JInt(result)), st)
}

/// §22.1.3.11 String.prototype.lastIndexOf ( searchString [ , position ] ).
fn string_last_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(search, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let len = js_string.length(s)
  // Steps 4-6: ToNumber(position); NaN → +∞ (clamped to len).
  let #(num, st) = rt_val.t_to_number(st, helpers.arg_at(args, 1))
  let from = case num {
    JNan -> len
    _ -> int.clamp(rt_val.jsnum_to_integer_or_infinity(num), 0, len)
  }
  let result = js_string.last_index_of(s, search, from) |> option.unwrap(-1)
  #(mk_number(JInt(result)), st)
}

/// §22.1.3.8 String.prototype.includes.
fn string_includes(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  string_search_bool(st, this, args, "includes", string.contains)
}

/// §22.1.3.22 String.prototype.startsWith.
fn string_starts_with(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  string_search_bool(st, this, args, "startsWith", string.starts_with)
}

/// Shared body for includes/startsWith: coerce this + searchString, clamp
/// position, drop the prefix, apply the predicate.
fn string_search_bool(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
  predicate: fn(String, String) -> Bool,
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let search_val = helpers.first_arg_or_undefined(args)
  // Steps 3-4: IsRegExp(searchString) → TypeError.
  let #(is_re, st) = regexp.is_regexp(st, search_val)
  case is_re {
    True ->
      rt_val.t_throw_type_error(
        st,
        "First argument to String.prototype."
          <> name
          <> " must not be a regular expression",
      )
    False -> {
      let #(search, st) = rt_val.t_to_string(st, search_val)
      let #(pos, st) =
        rt_val.t_to_integer_or_infinity(st, helpers.arg_at(args, 1))
      let from = int.clamp(pos, 0, js_string.length(s))
      let sub = js_string.drop_start(s, from)
      #(mk_bool(predicate(sub, search)), st)
    }
  }
}

/// §22.1.3.7 String.prototype.endsWith.
fn string_ends_with(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let search_val = helpers.first_arg_or_undefined(args)
  let #(is_re, st) = regexp.is_regexp(st, search_val)
  case is_re {
    True ->
      rt_val.t_throw_type_error(
        st,
        "First argument to String.prototype.endsWith must not be a regular expression",
      )
    False -> {
      let #(search, st) = rt_val.t_to_string(st, search_val)
      let len = js_string.length(s)
      let #(end_pos, st) = second_arg_index_or_len(st, args, len, int.clamp)
      let sub = js_string.slice(s, 0, end_pos)
      #(mk_bool(string.ends_with(sub, search)), st)
    }
  }
}

/// endsWith/slice/substring's second argument: absent/undefined → `len`,
/// else ToIntegerOrInfinity through `map`.
fn second_arg_index_or_len(
  st: Agent,
  args: List(JsVal),
  len: Int,
  map: fn(Int, Int, Int) -> Int,
) -> #(Int, Agent) {
  case args {
    [_, v, ..] ->
      case classify(v) {
        KUndef -> #(len, st)
        _ -> {
          let #(n, st) = rt_val.t_to_integer_or_infinity(st, v)
          #(map(n, 0, len), st)
        }
      }
    _ -> #(len, st)
  }
}

/// §22.1.3.20 String.prototype.slice ( start, end ).
fn string_slice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let len = js_string.length(s)
  let #(start, st) =
    relative_index(st, helpers.first_arg_or_undefined(args), len, 0)
  let #(end, st) = relative_index(st, helpers.arg_at(args, 1), len, len)
  case end > start {
    True -> #(mk_string(js_string.slice(s, start, end - start)), st)
    False -> #(mk_string(""), st)
  }
}

/// §22.1.3.24 String.prototype.substring ( start, end ) — no negative
/// indices; args swapped if start > end.
fn string_substring(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let len = js_string.length(s)
  let #(raw_start, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let #(raw_end, st) = second_arg_index_or_len(st, args, len, fn(n, _, _) { n })
  let start = int.clamp(raw_start, 0, len)
  let end = int.clamp(raw_end, 0, len)
  let #(start, end) = case start > end {
    True -> #(end, start)
    False -> #(start, end)
  }
  #(mk_string(js_string.slice(s, start, end - start)), st)
}

/// §22.1.3.5 String.prototype.concat ( ...args ).
fn string_concat(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  concat_loop(st, args, [s])
}

fn concat_loop(
  st: Agent,
  args: List(JsVal),
  acc_rev: List(String),
) -> #(JsVal, Agent) {
  case args {
    [] -> concat_within_limit(st, acc_rev)
    [arg, ..rest] -> {
      let #(s, st) = rt_val.t_to_string(st, arg)
      concat_loop(st, rest, [s, ..acc_rev])
    }
  }
}

/// §22.1.3.16 String.prototype.repeat ( count ).
fn string_repeat(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(num, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  case num {
    rt_types.JPosInf | rt_types.JNegInf ->
      rt_val.t_throw_range_error(st, "Invalid count value: Infinity")
    _ -> {
      let count = rt_val.jsnum_to_integer_or_infinity(num)
      case count < 0 {
        True ->
          rt_val.t_throw_range_error(
            st,
            "Invalid count value: " <> int.to_string(count),
          )
        False ->
          case limits.repeat(s, count) {
            Ok(r) -> #(mk_string(r), st)
            Error(Nil) ->
              rt_val.t_throw_range_error(st, "Invalid string length")
          }
      }
    }
  }
}

/// §22.1.3.16.1 StringPad — shared body for padStart/padEnd.
fn string_pad(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  pad_fn: fn(String, Int, String) -> Result(String, Nil),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(max_len, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let target_len = int.max(max_len, 0)
  let #(filler, st) = case args {
    [_, v, ..] ->
      case classify(v) {
        KUndef -> #(" ", st)
        _ -> rt_val.t_to_string(st, v)
      }
    _ -> #(" ", st)
  }
  case pad_fn(s, target_len, filler) {
    Ok(r) -> #(mk_string(r), st)
    Error(Nil) -> rt_val.t_throw_range_error(st, "Invalid string length")
  }
}

/// §22.1.3.1 String.prototype.at ( index ).
fn string_at(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(idx, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let len = js_string.length(s)
  let actual = case idx < 0 {
    True -> len + idx
    False -> idx
  }
  case actual >= 0 && actual < len {
    True -> #(mk_string(js_string.slice(s, actual, 1)), st)
    False -> #(mk_undefined(), st)
  }
}

/// §22.1.3.3 String.prototype.codePointAt ( pos ).
fn string_code_point_at(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(pos, st) =
    rt_val.t_to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let cp = case pos >= 0 {
    True -> js_string.codepoint_at(s, pos)
    False -> None
  }
  case cp {
    Some(cp) -> #(mk_number(JInt(cp)), st)
    None -> #(mk_undefined(), st)
  }
}

/// §22.1.3.13 String.prototype.normalize ( [ form ] ).
fn string_normalize(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  case classify(helpers.first_arg_or_undefined(args)) {
    KUndef -> #(mk_string(ffi_nfc(s)), st)
    _ -> {
      let #(form, st) =
        rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
      case form {
        "NFC" -> #(mk_string(ffi_nfc(s)), st)
        "NFD" -> #(mk_string(ffi_nfd(s)), st)
        "NFKC" -> #(mk_string(ffi_nfkc(s)), st)
        "NFKD" -> #(mk_string(ffi_nfkd(s)), st)
        _ ->
          rt_val.t_throw_range_error(
            st,
            "The normalization form should be one of NFC, NFD, NFKC, NFKD",
          )
      }
    }
  }
}

/// Annex B §B.2.2.1 String.prototype.substr ( start, length ).
fn string_substr(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let size = js_string.length(s)
  let #(start, st) =
    relative_index(st, helpers.first_arg_or_undefined(args), size, 0)
  let #(raw_len, st) =
    second_arg_index_or_len(st, args, size, fn(n, _, _) { n })
  let len = int.clamp(raw_len, 0, size)
  let end = int.min(start + len, size)
  case start >= end {
    True -> #(mk_string(""), st)
    False -> #(mk_string(js_string.slice(s, start, end - start)), st)
  }
}

/// §22.1.3.10 String.prototype.localeCompare ( that ) — no locale support:
/// NFC-normalize then byte compare.
fn string_locale_compare(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(that, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let n = case string.compare(ffi_nfc(s), ffi_nfc(that)) {
    order.Lt -> -1
    order.Eq -> 0
    order.Gt -> 1
  }
  #(mk_number(JInt(n)), st)
}

/// §22.1.3.12 String.prototype.isWellFormed ( ).
fn string_is_well_formed(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(_s, st) = with_this_string(st, this)
  // Gleam strings are valid UTF-8 → always well-formed.
  #(mk_bool(True), st)
}

/// §22.1.3.26 / §22.1.3.33 String.prototype.{toString,valueOf}.
fn string_this_value(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(JsVal, Agent) {
  #(mk_string(this_string_value(st, this, method)), st)
}

// ── Symbol-method delegation (match/matchAll/replace/replaceAll/search/split)

/// **`if V is an Object: ? GetMethod(V, @@symbol)`** (§22.1.3.13/.14/.19/.20/.21/.22
/// step 2). The object-only guard is deliberate: a primitive searchValue must
/// NOT box and consult its prototype.
fn get_method(
  st: Agent,
  val: JsVal,
  symbol: SymbolId,
) -> #(Option(JsVal), Agent) {
  case classify(val) {
    KHandle(_) -> {
      let #(func, st) = rt_obj.t_get_prop(st, val, SymbolKey(symbol))
      case rt_val.is_nullish(func) {
        True -> #(None, st)
        False -> {
          let #(callable, st) = rt_val.t_is_callable(st, func)
          case callable {
            True -> #(Some(func), st)
            False -> rt_val.t_throw_type_error(st, not_a_function(symbol))
          }
        }
      }
    }
    _ -> #(None, st)
  }
}

fn not_a_function(symbol: SymbolId) -> String {
  well_known_symbol_description(symbol)
  |> option.unwrap("Symbol method")
  |> string.append(" is not a function")
}

/// Delegate to a Symbol method on `val` if it has one, passing the ORIGINAL
/// `this`; otherwise S = ToString(this), rx = RegExpCreate(val, undefined),
/// Invoke(rx, @@symbol, « S »). Shared by match/search.
fn delegate_or_regexp(
  st: Agent,
  val: JsVal,
  symbol: SymbolId,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(method_opt, st) = get_method(st, val, symbol)
  case method_opt {
    Some(method) -> rt_call.t_call_checked(st, method, val, [this])
    None -> {
      let #(s, st) = rt_val.t_to_string(st, this)
      let #(rx, st) = regexp.regexp_create(st, val, mk_undefined())
      let #(method_opt, st) = get_method(st, rx, symbol)
      case method_opt {
        Some(method) -> rt_call.t_call_checked(st, method, rx, [mk_string(s)])
        None -> rt_val.t_throw_type_error(st, not_a_function(symbol))
      }
    }
  }
}

/// §22.1.3.12 String.prototype.match(regexp).
fn string_match(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "match")
  delegate_or_regexp(
    st,
    helpers.first_arg_or_undefined(args),
    rt_types.symbol_match,
    this,
  )
}

/// §22.1.3.20 String.prototype.search(regexp).
fn string_search(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "search")
  delegate_or_regexp(
    st,
    helpers.first_arg_or_undefined(args),
    rt_types.symbol_search,
    this,
  )
}

/// §22.1.3.18 String.prototype.replace(searchValue, replaceValue).
fn string_replace(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "replace")
  let search_val = helpers.first_arg_or_undefined(args)
  let replace_val = helpers.arg_at(args, 1)
  let #(method_opt, st) = get_method(st, search_val, rt_types.symbol_replace)
  case method_opt {
    Some(method) ->
      rt_call.t_call_checked(st, method, search_val, [this, replace_val])
    None -> {
      let #(s, st) = rt_val.t_to_string(st, this)
      let #(search_str, st) = rt_val.t_to_string(st, search_val)
      replace_string_search(st, s, search_str, replace_val, False)
    }
  }
}

/// §22.1.3.19 String.prototype.replaceAll(searchValue, replaceValue).
fn string_replace_all(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "replaceAll")
  let search_val = helpers.first_arg_or_undefined(args)
  let replace_val = helpers.arg_at(args, 1)
  let #(is_re, st) = regexp.is_regexp(st, search_val)
  let st = require_global_when_regexp(st, search_val, is_re, "replaceAll")
  let #(method_opt, st) = get_method(st, search_val, rt_types.symbol_replace)
  case method_opt {
    Some(method) ->
      rt_call.t_call_checked(st, method, search_val, [this, replace_val])
    None -> {
      let #(s, st) = rt_val.t_to_string(st, this)
      let #(search_str, st) = rt_val.t_to_string(st, search_val)
      replace_string_search(st, s, search_str, replace_val, True)
    }
  }
}

/// §22.1.3.14 String.prototype.matchAll ( regexp ).
fn string_match_all(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "matchAll")
  let regexp_arg = helpers.first_arg_or_undefined(args)
  let #(is_re, st) = regexp.is_regexp(st, regexp_arg)
  let st = require_global_when_regexp(st, regexp_arg, is_re, "matchAll")
  let #(method_opt, st) = get_method(st, regexp_arg, rt_types.symbol_match_all)
  case method_opt {
    Some(method) -> rt_call.t_call_checked(st, method, regexp_arg, [this])
    None -> {
      // Steps 3-5: S = ToString(O); rx = RegExpCreate(regexp, "g"); Invoke.
      let #(s, st) = rt_val.t_to_string(st, this)
      let #(rx, st) = regexp.regexp_create(st, regexp_arg, mk_string("g"))
      let #(method_opt, st) = get_method(st, rx, rt_types.symbol_match_all)
      case method_opt {
        Some(method) -> rt_call.t_call_checked(st, method, rx, [mk_string(s)])
        None ->
          rt_val.t_throw_type_error(
            st,
            not_a_function(rt_types.symbol_match_all),
          )
      }
    }
  }
}

/// §22.1.3.21 String.prototype.split ( separator, limit ).
fn string_split(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "split")
  let sep_val = helpers.first_arg_or_undefined(args)
  let limit_val = helpers.arg_at(args, 1)
  let #(method_opt, st) = get_method(st, sep_val, rt_types.symbol_split)
  case method_opt {
    Some(method) ->
      rt_call.t_call_checked(st, method, sep_val, [this, limit_val])
    None -> {
      let #(s, st) = with_this_string(st, this)
      // Step 4: limit undefined → 2^32-1, else ToUint32(limit).
      let #(lim, st) = case classify(limit_val) {
        KUndef -> #(4_294_967_295, st)
        _ -> rt_val.t_to_uint32(st, limit_val)
      }
      string_split_parts(st, s, sep_val, lim)
    }
  }
}

fn string_split_parts(
  st: Agent,
  s: String,
  sep_val: JsVal,
  lim: Int,
) -> #(JsVal, Agent) {
  case classify(sep_val) {
    // Steps 6-7: separator undefined → [S] (or [] when lim = 0).
    KUndef ->
      case lim {
        0 -> ok_array(st, [])
        _ -> ok_array(st, [mk_string(s)])
      }
    _ -> {
      // Step 5: R = ToString(separator) — runs before the lim=0 check.
      let #(sep, st) = rt_val.t_to_string(st, sep_val)
      case lim {
        0 -> ok_array(st, [])
        _ -> {
          let parts = case sep {
            "" -> js_string.explode(s) |> list.map(mk_string)
            _ -> string.split(s, sep) |> list.map(mk_string)
          }
          ok_array(st, list.take(parts, lim))
        }
      }
    }
  }
}

// ── replace / replaceAll string-search engine (§22.1.3.18/.19 steps 5+) ────

fn replace_string_search(
  st: Agent,
  s: String,
  search_str: String,
  replace_val: JsVal,
  all: Bool,
) -> #(JsVal, Agent) {
  let search_len = js_string.length(search_str)
  let #(callable, st) = rt_val.t_is_callable(st, replace_val)
  case callable {
    True ->
      replace_loop_functional(
        st,
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
      let #(template, st) = rt_val.t_to_string(st, replace_val)
      let segments = substitution.tokenize_plain(template)
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
      concat_within_limit(st, parts)
    }
  }
}

fn replace_loop_functional(
  st: Agent,
  tail: String,
  s: String,
  search_str: String,
  search_len: Int,
  abs_pos: Int,
  acc: List(String),
  replace_fn: JsVal,
  all: Bool,
) -> #(JsVal, Agent) {
  case js_string.index_of(tail, search_str, 0) {
    None -> concat_within_limit(st, [tail, ..acc])
    Some(rel) -> {
      let preserved = js_string.slice(tail, 0, rel)
      let after = js_string.drop_start(tail, rel + search_len)
      let p = abs_pos + rel
      let #(result, st) =
        rt_call.t_call_checked(st, replace_fn, mk_undefined(), [
          mk_string(search_str),
          mk_number(JInt(p)),
          mk_string(s),
        ])
      let #(replacement, st) = rt_val.t_to_string(st, result)
      let acc = [replacement, preserved, ..acc]
      case all, search_len {
        False, _ -> concat_within_limit(st, [after, ..acc])
        True, 0 ->
          case after {
            "" -> concat_within_limit(st, acc)
            _ ->
              replace_loop_functional(
                st,
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
            st,
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
      let replacement = case segments {
        [substitution.LiteralSeg(text)] -> text
        _ ->
          substitution.resolve_without_named(
            segments,
            substitution.Ctx(
              matched: search_str,
              before: fn() { before <> preserved },
              after: fn() { after },
              capture: fn(_) { "" },
              m: 0,
            ),
          )
      }
      let acc = [replacement, preserved, ..acc]
      case all, search_len {
        False, _ -> [after, ..acc]
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

// ── §22.1.2 String static methods ───────────────────────────────────────────

/// §22.1.2.4 String.raw ( template, ...substitutions ).
fn string_raw(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let template = helpers.first_arg_or_undefined(args)
  let subs = case args {
    [_, ..rest] -> rest
    [] -> []
  }
  let #(raw_val, st) = rt_obj.t_get_prop(st, template, StringKey(Named("raw")))
  let #(len_val, st) =
    rt_obj.t_get_prop(st, raw_val, StringKey(Named("length")))
  let #(literal_count, st) = rt_val.t_to_length(st, len_val)
  case literal_count {
    0 -> #(mk_string(""), st)
    _ -> string_raw_loop(st, raw_val, subs, literal_count, 0, [])
  }
}

fn string_raw_loop(
  st: Agent,
  raw_val: JsVal,
  subs: List(JsVal),
  literal_count: Int,
  index: Int,
  acc_rev: List(String),
) -> #(JsVal, Agent) {
  let #(lit_val, st) =
    rt_obj.t_get_prop(
      st,
      raw_val,
      StringKey(rt_types.canonical_key(int.to_string(index))),
    )
  let #(lit, st) = rt_val.t_to_string(st, lit_val)
  let acc_rev = [lit, ..acc_rev]
  case index + 1 == literal_count {
    True -> concat_within_limit(st, acc_rev)
    False ->
      case subs {
        [sub_val, ..rest] -> {
          let #(sub, st) = rt_val.t_to_string(st, sub_val)
          string_raw_loop(st, raw_val, rest, literal_count, index + 1, [
            sub,
            ..acc_rev
          ])
        }
        [] ->
          string_raw_loop(st, raw_val, [], literal_count, index + 1, acc_rev)
      }
  }
}

/// §22.1.2.1 String.fromCharCode ( ...codeUnits ).
fn string_from_char_code(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(codes, st) = from_char_code_coerce(st, args, [])
  #(mk_string(char_codes_to_string(list.reverse(codes), [])), st)
}

fn from_char_code_coerce(
  st: Agent,
  args: List(JsVal),
  acc: List(Int),
) -> #(List(Int), Agent) {
  case args {
    [] -> #(acc, st)
    [arg, ..rest] -> {
      let #(num, st) = rt_val.t_to_number(st, arg)
      // §7.1.8 ToUint16: NaN/±0/±∞ → +0, else truncate mod 2^16.
      let n = case num {
        JInt(i) -> i
        JFloat(f) -> rt_val.float_to_int(f)
        _ -> 0
      }
      from_char_code_coerce(st, rest, [modulo_uint16(n), ..acc])
    }
  }
}

/// UTF-16 code units → string, combining surrogate pairs.
fn char_codes_to_string(codes: List(Int), acc: List(UtfCodepoint)) -> String {
  case codes {
    [] -> string.from_utf_codepoints(list.reverse(acc))
    [code, ..rest] -> {
      let #(cp, remaining) = case is_high_surrogate(code), rest {
        True, [low, ..after] ->
          case is_low_surrogate(low) {
            True -> #(combine_surrogates(code, low), after)
            False -> #(code, rest)
          }
        _, _ -> #(code, rest)
      }
      char_codes_to_string(remaining, [codepoint_or_replacement(cp), ..acc])
    }
  }
}

/// §22.1.2.2 String.fromCodePoint ( ...codePoints ).
fn string_from_code_point(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  from_code_point_loop(st, args, [])
}

fn from_code_point_loop(
  st: Agent,
  args: List(JsVal),
  acc: List(UtfCodepoint),
) -> #(JsVal, Agent) {
  case args {
    [] -> #(mk_string(string.from_utf_codepoints(list.reverse(acc))), st)
    [arg, ..rest] -> {
      let #(num, st) = rt_val.t_to_number(st, arg)
      case num {
        JInt(i) if i >= 0 && i <= 0x10FFFF ->
          from_code_point_loop(st, rest, [codepoint_or_replacement(i), ..acc])
        JFloat(f) ->
          case rt_val.integral_int(f) {
            Some(i) if i >= 0 && i <= 0x10FFFF ->
              from_code_point_loop(st, rest, [
                codepoint_or_replacement(i),
                ..acc
              ])
            _ ->
              rt_val.t_throw_range_error(
                st,
                "Invalid code point " <> rt_val.js_format_float(f),
              )
          }
        JNan -> rt_val.t_throw_range_error(st, "Invalid code point NaN")
        JInt(i) ->
          rt_val.t_throw_range_error(
            st,
            "Invalid code point " <> int.to_string(i),
          )
        _ -> rt_val.t_throw_range_error(st, "Invalid code point Infinity")
      }
    }
  }
}

// ── Annex B §B.2.2 HTML wrapper methods ─────────────────────────────────────

fn html_wrap(st: Agent, this: JsVal, tag: String) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  #(mk_string("<" <> tag <> ">" <> s <> "</" <> tag <> ">"), st)
}

fn html_wrap_attr(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  tag: String,
  attr: String,
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  let #(attr_val, st) =
    rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let escaped = string.replace(attr_val, "\"", "&quot;")
  #(
    mk_string(
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
    ),
    st,
  )
}

// ── internal helpers ────────────────────────────────────────────────────────

/// matchAll step 2.b / replaceAll step 2.a: when IsRegExp, its "flags" must
/// be object-coercible and its string must contain "g".
fn require_global_when_regexp(
  st: Agent,
  val: JsVal,
  is_re: Bool,
  method: String,
) -> Agent {
  case is_re {
    False -> st
    True -> {
      let #(flags, st) = rt_obj.t_get_prop(st, val, StringKey(Named("flags")))
      let #(flags, st) = rt_val.t_require_object_coercible(st, flags)
      let #(s, st) = rt_val.t_to_string(st, flags)
      case string.contains(s, "g") {
        True -> st
        False ->
          rt_val.t_throw_type_error(
            st,
            "String.prototype."
              <> method
              <> " called with a non-global RegExp argument",
          )
      }
    }
  }
}

/// §7.2.1 RequireObjectCoercible for methods that defer ToString(this).
fn require_object_coercible(st: Agent, this: JsVal, name: String) -> Agent {
  case classify(this) {
    KNull | KUndef ->
      rt_val.t_throw_type_error(
        st,
        "String.prototype." <> name <> " called on null or undefined",
      )
    _ -> st
  }
}

/// Coerce `this` to string. Primitive strings pass through; null/undefined →
/// TypeError; anything else → ToString.
fn with_this_string(st: Agent, this: JsVal) -> #(String, Agent) {
  case classify(this) {
    KStr(s) -> #(s, st)
    KNull -> rt_val.t_throw_type_error(st, "Cannot read properties of null")
    KUndef ->
      rt_val.t_throw_type_error(st, "Cannot read properties of undefined")
    _ -> rt_val.t_to_string(st, this)
  }
}

/// Coerce `this` to string, apply a pure transformation, return the result.
fn string_transform(
  st: Agent,
  this: JsVal,
  transform: fn(String) -> String,
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_string(st, this)
  #(mk_string(transform(s)), st)
}

/// §22.1.3 thisStringValue(value): String primitive or [[StringData]] slot.
fn this_string_value(st: Agent, this: JsVal, method: String) -> String {
  case classify(this) {
    KStr(s) -> s
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: StringObj(value: s), ..) -> s
        _ -> not_a_string(st, method)
      }
    _ -> not_a_string(st, method)
  }
}

fn not_a_string(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "String.prototype." <> method <> " requires that 'this' be a String",
  )
}

/// Resolve a relative index (§7.1.22-style): -∞ → 0, negative → max(len+n,0),
/// else min(n, len). undefined → `default`.
fn relative_index(
  st: Agent,
  v: JsVal,
  len: Int,
  default: Int,
) -> #(Int, Agent) {
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(n, st) = rt_val.t_to_integer_or_infinity(st, v)
      case n < 0 {
        True -> #(int.max(len + n, 0), st)
        False -> #(int.min(n, len), st)
      }
    }
  }
}

/// Concatenate a reversed accumulator, honouring `limits.max_string_bytes`.
fn concat_within_limit(st: Agent, parts_rev: List(String)) -> #(JsVal, Agent) {
  let parts = list.reverse(parts_rev)
  let total =
    list.fold(parts, 0, fn(sum, part) { sum + string.byte_size(part) })
  case total > limits.max_string_bytes {
    True -> rt_val.t_throw_range_error(st, "Invalid string length")
    False -> #(mk_string(string.concat(parts)), st)
  }
}

fn ok_array(st: Agent, values: List(JsVal)) -> #(JsVal, Agent) {
  let #(h, st) = realm_ops.alloc_array(st, values)
  #(mk_object(h), st)
}

// ── UTF-16 surrogate helpers (arc/internal/utf16.gleam:34-52 inlined) ───────

fn is_high_surrogate(cu: Int) -> Bool {
  cu >= 0xD800 && cu <= 0xDBFF
}

fn is_low_surrogate(cu: Int) -> Bool {
  cu >= 0xDC00 && cu <= 0xDFFF
}

fn combine_surrogates(high: Int, low: Int) -> Int {
  0x10000 + { high - 0xD800 } * 0x400 + { low - 0xDC00 }
}

fn modulo_uint16(n: Int) -> Int {
  let m = n % 65_536
  case m < 0 {
    True -> m + 65_536
    False -> m
  }
}

fn codepoint_or_replacement(i: Int) -> UtfCodepoint {
  case string.utf_codepoint(i) {
    Ok(cp) -> cp
    Error(Nil) -> js_string.replacement_codepoint()
  }
}

// ── Unicode Default Case Conversion (port of arc/rt/unicode_case.gleam) ────

/// toLowercase per §22.1.3.27, including the SpecialCasing Final_Sigma rule:
/// U+03A3 Σ → ς when preceded by a cased char (skipping case-ignorable chars)
/// and not followed by one; else → σ. Erlang string:lowercase/1 does NOT.
pub fn to_lower_case(s: String) -> String {
  let cps = string.to_utf_codepoints(s) |> list.map(string.utf_codepoint_to_int)
  case list.contains(cps, 0x03A3) {
    False -> string.lowercase(s)
    True -> sigma_assemble(split_cps_on_sigma(cps, [], []), True)
  }
}

fn split_cps_on_sigma(
  cps: List(Int),
  cur: List(Int),
  acc: List(List(Int)),
) -> List(List(Int)) {
  case cps {
    [] -> list.reverse([list.reverse(cur), ..acc])
    [0x03A3, ..rest] -> split_cps_on_sigma(rest, [], [list.reverse(cur), ..acc])
    [cp, ..rest] -> split_cps_on_sigma(rest, [cp, ..cur], acc)
  }
}

fn sigma_assemble(parts: List(List(Int)), is_first: Bool) -> String {
  case parts {
    [] -> ""
    [last] -> lowercase_cps(last)
    [part, ..rest] -> {
      let preceded = case first_non_ignorable_cased(list.reverse(part)) {
        Some(cased) -> cased
        None -> !is_first
      }
      let followed = case rest {
        [next, ..more] ->
          case first_non_ignorable_cased(next) {
            Some(cased) -> cased
            None -> more != []
          }
        [] -> False
      }
      let sigma = case preceded && !followed {
        True -> "\u{03C2}"
        False -> "\u{03C3}"
      }
      lowercase_cps(part) <> sigma <> sigma_assemble(rest, False)
    }
  }
}

fn lowercase_cps(cps: List(Int)) -> String {
  cps
  |> list.filter_map(string.utf_codepoint)
  |> string.from_utf_codepoints
  |> string.lowercase
}

fn first_non_ignorable_cased(cps: List(Int)) -> Option(Bool) {
  case cps {
    [] -> None
    [cp, ..rest] ->
      case is_case_ignorable_cp(cp) {
        True -> first_non_ignorable_cased(rest)
        False -> Some(is_cased_cp(cp))
      }
  }
}

fn is_cased_cp(cp: Int) -> Bool {
  case cp {
    _ if cp >= 0x41 && cp <= 0x5A -> True
    _ if cp >= 0x61 && cp <= 0x7A -> True
    0xAA | 0xB5 | 0xBA -> True
    _ if cp >= 0xC0 && cp <= 0xD6 -> True
    _ if cp >= 0xD8 && cp <= 0xF6 -> True
    _ if cp >= 0xF8 && cp <= 0x2AF -> True
    _ if cp >= 0x370 && cp <= 0x373 -> True
    0x376 | 0x377 | 0x37F | 0x386 -> True
    _ if cp >= 0x37B && cp <= 0x37D -> True
    _ if cp >= 0x388 && cp <= 0x481 -> True
    _ if cp >= 0x48A && cp <= 0x52F -> True
    _ if cp >= 0x531 && cp <= 0x556 -> True
    _ if cp >= 0x560 && cp <= 0x588 -> True
    _ if cp >= 0x10A0 && cp <= 0x10CD -> True
    _ if cp >= 0x13A0 && cp <= 0x13FD -> True
    _ if cp >= 0x1C80 && cp <= 0x1C88 -> True
    _ if cp >= 0x1C90 && cp <= 0x1CBF -> True
    _ if cp >= 0x1E00 && cp <= 0x1FFC -> True
    _ if cp >= 0x2126 && cp <= 0x212B -> True
    _ if cp >= 0x2160 && cp <= 0x217F -> True
    0x2183 | 0x2184 -> True
    _ if cp >= 0x24B6 && cp <= 0x24E9 -> True
    _ if cp >= 0x2C00 && cp <= 0x2D2D -> True
    _ if cp >= 0xA640 && cp <= 0xA66D -> True
    _ if cp >= 0xA680 && cp <= 0xA69B -> True
    _ if cp >= 0xA722 && cp <= 0xA787 -> True
    _ if cp >= 0xA78B && cp <= 0xA7CA -> True
    _ if cp >= 0xAB70 && cp <= 0xABBF -> True
    _ if cp >= 0xFB00 && cp <= 0xFB17 -> True
    _ if cp >= 0xFF21 && cp <= 0xFF3A -> True
    _ if cp >= 0xFF41 && cp <= 0xFF5A -> True
    _ if cp >= 0x10400 && cp <= 0x104FB -> True
    _ if cp >= 0x10C80 && cp <= 0x10CFF -> True
    _ if cp >= 0x118A0 && cp <= 0x118DF -> True
    _ if cp >= 0x16E40 && cp <= 0x16E7F -> True
    _ if cp >= 0x1D400 && cp <= 0x1D7CB -> True
    _ if cp >= 0x1E900 && cp <= 0x1E943 -> True
    _ -> False
  }
}

fn is_case_ignorable_cp(cp: Int) -> Bool {
  case cp {
    0x27
    | 0x2E
    | 0x3A
    | 0x5E
    | 0x60
    | 0xA8
    | 0xAD
    | 0xAF
    | 0xB4
    | 0xB7
    | 0xB8 -> True
    _ if cp >= 0x2B0 && cp <= 0x36F -> True
    0x374 | 0x375 | 0x37A | 0x384 | 0x385 | 0x387 -> True
    _ if cp >= 0x483 && cp <= 0x489 -> True
    _ if cp >= 0x559 && cp <= 0x55F -> True
    _ if cp >= 0x591 && cp <= 0x5C7 -> True
    0x5F3 | 0x5F4 -> True
    _ if cp >= 0x600 && cp <= 0x605 -> True
    _ if cp >= 0x610 && cp <= 0x61A -> True
    0x61C | 0x640 | 0x670 | 0x6DD | 0x70F | 0x711 -> True
    _ if cp >= 0x64B && cp <= 0x65F -> True
    _ if cp >= 0x6D6 && cp <= 0x6DC -> True
    _ if cp >= 0x6DF && cp <= 0x6E8 -> True
    _ if cp >= 0x6EA && cp <= 0x6ED -> True
    _ if cp >= 0x730 && cp <= 0x74A -> True
    _ if cp >= 0x7A6 && cp <= 0x7B0 -> True
    _ if cp >= 0x7EB && cp <= 0x7F5 -> True
    _ if cp >= 0x816 && cp <= 0x82D -> True
    _ if cp >= 0x180B && cp <= 0x180E -> True
    _ if cp >= 0x1AB0 && cp <= 0x1AFF -> True
    _ if cp >= 0x1C78 && cp <= 0x1C7D -> True
    _ if cp >= 0x1DC0 && cp <= 0x1DFF -> True
    0x1FBD -> True
    _ if cp >= 0x1FBF && cp <= 0x1FC1 -> True
    _ if cp >= 0x1FCD && cp <= 0x1FCF -> True
    _ if cp >= 0x1FDD && cp <= 0x1FDF -> True
    _ if cp >= 0x1FED && cp <= 0x1FEF -> True
    0x1FFD | 0x1FFE -> True
    0x2018 | 0x2019 | 0x2024 | 0x2027 -> True
    _ if cp >= 0x200B && cp <= 0x200F -> True
    _ if cp >= 0x202A && cp <= 0x202E -> True
    _ if cp >= 0x2060 && cp <= 0x2064 -> True
    _ if cp >= 0x2066 && cp <= 0x206F -> True
    0x2071 | 0x207F -> True
    _ if cp >= 0x2090 && cp <= 0x209C -> True
    _ if cp >= 0x20D0 && cp <= 0x20F0 -> True
    0x2C7C | 0x2C7D | 0x2D6F | 0x2D7F | 0x2E2F -> True
    _ if cp >= 0x2DE0 && cp <= 0x2DFF -> True
    0x3005 | 0x303B | 0x309B | 0x309C | 0xFB1E -> True
    _ if cp >= 0xA66F && cp <= 0xA672 -> True
    _ if cp >= 0xA674 && cp <= 0xA67D -> True
    0xA67F | 0xA69C | 0xA69D | 0xA69E | 0xA69F -> True
    _ if cp >= 0xA700 && cp <= 0xA721 -> True
    0xA770 | 0xA788 | 0xA789 | 0xA78A | 0xA7F8 | 0xA7F9 -> True
    _ if cp >= 0xFE00 && cp <= 0xFE0F -> True
    _ if cp >= 0xFE20 && cp <= 0xFE2F -> True
    0xFE13 | 0xFE52 | 0xFE55 | 0xFEFF | 0xFF07 | 0xFF0E | 0xFF1A -> True
    0xFF3E | 0xFF40 | 0xFF70 | 0xFF9E | 0xFF9F | 0xFFE3 -> True
    _ if cp >= 0x1D165 && cp <= 0x1D244 -> True
    _ if cp >= 0xE0001 && cp <= 0xE01EF -> True
    _ -> False
  }
}

// ── FFI ─────────────────────────────────────────────────────────────────────

@external(erlang, "arc_rt_string_ffi", "trim_js_ws")
fn trim_js_ws(s: String) -> String

@external(erlang, "arc_rt_string_ffi", "trim_leading_js_ws")
fn trim_leading_js_ws(s: String) -> String

@external(erlang, "arc_rt_string_ffi", "trim_trailing_js_ws")
fn trim_trailing_js_ws(s: String) -> String

@external(erlang, "unicode", "characters_to_nfc_binary")
fn ffi_nfc(s: String) -> String

@external(erlang, "unicode", "characters_to_nfd_binary")
fn ffi_nfd(s: String) -> String

@external(erlang, "unicode", "characters_to_nfkc_binary")
fn ffi_nfkc(s: String) -> String

@external(erlang, "unicode", "characters_to_nfkd_binary")
fn ffi_nfkd(s: String) -> String
