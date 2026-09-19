import arc/bytecode/key.{Named}
import arc/internal/utf16
import arc/rt/abstract_ops as rt_abstract_ops
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/builtins/regexp as b_regexp
import arc/rt/builtins/substitution
import arc/rt/call as rt_call
import arc/rt/js_string
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type StringNative,
  type SymbolId, JFloat, JInt, JNan, KHandle, KNull, KStr, KUndef, SObject,
  StringConstructor, StringFromCharCode, StringFromCodePoint, StringIterator,
  StringKey, StringN, StringObj, StringPrototypeAnchor, StringPrototypeAt,
  StringPrototypeBig, StringPrototypeBlink, StringPrototypeBold,
  StringPrototypeCharAt, StringPrototypeCharCodeAt, StringPrototypeCodePointAt,
  StringPrototypeConcat, StringPrototypeEndsWith, StringPrototypeFixed,
  StringPrototypeFontcolor, StringPrototypeFontsize, StringPrototypeIncludes,
  StringPrototypeIndexOf, StringPrototypeIsWellFormed, StringPrototypeItalics,
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
  StringPrototypeTrimStart, StringPrototypeValueOf, StringRaw, classify, mk_bool,
  mk_int, mk_number, mk_object, mk_string, mk_undefined, plain_object,
  symbol_description,
}
import arc/rt/unicode_case
import arc/rt/utf8
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string

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
  let #(static_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("raw", StringN(StringRaw), 1),
      #("fromCharCode", StringN(StringFromCharCode), 1),
      #("fromCodePoint", StringN(StringFromCodePoint), 1),
    ])
  // the prototype is itself a string object with value ""
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
  let #(iter_fn, st) =
    common.alloc_rooted_native_fn(
      st,
      fn_proto,
      StringN(StringPrototypeSymbolIterator),
      "[Symbol.iterator]",
      0,
    )
  let #(iter_prop, st) = rt_store.builtin_property(st, mk_object(iter_fn))
  let st =
    common.add_symbol_property(
      st,
      bt.prototype,
      types.symbol_iterator,
      iter_prop,
    )
  #(bt, st)
}

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
      string_transform(st, this, unicode_case.to_lower_case)
    StringPrototypeToUpperCase | StringPrototypeToLocaleUpperCase ->
      string_transform(st, this, unicode_case.to_upper_case)
    StringPrototypeTrim -> string_transform(st, this, utf8.trim_js_ws)
    StringPrototypeTrimStart ->
      string_transform(st, this, utf8.trim_leading_js_ws)
    StringPrototypeTrimEnd ->
      string_transform(st, this, utf8.trim_trailing_js_ws)
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
    StringRaw -> string_raw(st, args)
    StringFromCharCode -> string_from_char_code(st, args)
    StringFromCodePoint -> string_from_code_point(st, args)
  }
}

// §22.1.1.1 string(value) as a function, symbols don't throw here
fn call_as_function(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  case args {
    [] -> #(mk_string(""), st)
    [v, ..] ->
      case classify(v) {
        types.KSym(id) -> #(mk_string(types.symbol_descriptive_string(id)), st)
        _ -> {
          let #(s, st) = rt_val.to_string(st, v)
          #(mk_string(s), st)
        }
      }
  }
}

fn string_symbol_iterator(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  let realm = st.realm
  let #(iter_h, st) =
    rt_store.cell_new(
      st,
      plain_object(
        StringIterator(source: s, index: 0),
        Some(realm.string_iter_proto),
        common.named_props([]),
      ),
    )
  #(mk_object(iter_h), st)
}

fn string_char_at(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_str(st, this)
  let #(idx, st) =
    rt_val.to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  case js_string.char_at(s, idx) {
    Some(ch) -> #(ch, st)
    None -> #(mk_string(""), st)
  }
}

fn string_char_code_at(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_str(st, this)
  let #(idx, st) =
    rt_val.to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  case js_string.codepoint_at(s, idx) {
    Some(cp) -> #(mk_int(cp), st)
    None -> #(mk_number(JNan), st)
  }
}

fn string_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_str(st, this)
  let #(search, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let #(pos, st) = rt_val.to_integer_or_infinity(st, helpers.arg_at(args, 1))
  let from = int.clamp(pos, 0, js_string.length(s))
  let result = js_string.index_of(s, mk_string(search), from)
  #(mk_int(option.unwrap(result, -1)), st)
}

fn string_last_index_of(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(v, st) = with_this_str(st, this)
  let s = js_string.text(v)
  let #(search, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let #(num, st) = rt_val.to_number(st, helpers.arg_at(args, 1))
  let result = case num {
    JNan | types.JPosInf -> utf8.last_index_of_all(s, search)
    _ -> {
      let len = js_string.length(v)
      let from = int.clamp(rt_val.jsnum_to_integer_or_infinity(num), 0, len)
      utf8.last_index_of(s, search, from)
    }
  }
  #(mk_int(option.unwrap(result, -1)), st)
}

fn string_includes(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  string_search_bool(st, this, args, "includes", utf8.contains)
}

fn string_starts_with(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  string_search_bool(st, this, args, "startsWith", string.starts_with)
}

fn string_search_bool(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  name: String,
  predicate: fn(String, String) -> Bool,
) -> #(JsVal, Agent) {
  let #(v, st) = with_this_str(st, this)
  let s = js_string.text(v)
  let search_val = helpers.first_arg_or_undefined(args)
  let #(is_re, st) = b_regexp.is_regexp(st, search_val)
  case is_re {
    True ->
      rt_val.throw_type_error(
        st,
        "First argument to String.prototype."
          <> name
          <> " must not be a regular expression",
      )
    False -> {
      let #(search, st) = rt_val.to_string(st, search_val)
      let #(pos, st) =
        rt_val.to_integer_or_infinity(st, helpers.arg_at(args, 1))
      let sub = case pos <= 0 {
        True -> s
        False -> {
          let len = js_string.length(v)
          let pos = int.min(pos, len)
          js_string.text(js_string.substring(v, pos, len - pos))
        }
      }
      #(mk_bool(predicate(sub, search)), st)
    }
  }
}

fn string_ends_with(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(v, st) = with_this_str(st, this)
  let s = js_string.text(v)
  let search_val = helpers.first_arg_or_undefined(args)
  let #(is_re, st) = b_regexp.is_regexp(st, search_val)
  case is_re {
    True ->
      rt_val.throw_type_error(
        st,
        "First argument to String.prototype.endsWith must not be a regular expression",
      )
    False -> {
      let #(search, st) = rt_val.to_string(st, search_val)
      let len = js_string.length(v)
      let #(end_pos, st) = second_arg_index_or_len(st, args, len, int.clamp)
      let sub = case end_pos == len {
        True -> s
        False -> js_string.text(js_string.substring(v, 0, end_pos))
      }
      #(mk_bool(string.ends_with(sub, search)), st)
    }
  }
}

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
          let #(n, st) = rt_val.to_integer_or_infinity(st, v)
          #(map(n, 0, len), st)
        }
      }
    _ -> #(len, st)
  }
}

fn string_slice(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_str(st, this)
  let len = js_string.length(s)
  let #(start, st) =
    rt_abstract_ops.relative_index(
      st,
      helpers.first_arg_or_undefined(args),
      len,
      0,
    )
  let #(end, st) =
    rt_abstract_ops.relative_index(st, helpers.arg_at(args, 1), len, len)
  case end > start {
    True -> #(js_string.substring(s, start, end - start), st)
    False -> #(mk_string(""), st)
  }
}

fn string_substring(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_str(st, this)
  let len = js_string.length(s)
  let #(raw_start, st) =
    rt_val.to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let #(raw_end, st) = second_arg_index_or_len(st, args, len, fn(n, _, _) { n })
  let start = int.clamp(raw_start, 0, len)
  let end = int.clamp(raw_end, 0, len)
  let #(start, end) = case start > end {
    True -> #(end, start)
    False -> #(start, end)
  }
  #(js_string.substring(s, start, end - start), st)
}

fn string_concat(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  string_concat_loop(st, args, [s])
}

fn string_concat_loop(
  st: Agent,
  args: List(JsVal),
  acc_rev: List(String),
) -> #(JsVal, Agent) {
  case args {
    [] -> concat_within_limit(st, acc_rev)
    [arg, ..rest] -> {
      let #(s, st) = rt_val.to_string(st, arg)
      string_concat_loop(st, rest, [s, ..acc_rev])
    }
  }
}

fn string_repeat(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  let #(num, st) = rt_val.to_number(st, helpers.first_arg_or_undefined(args))
  case num {
    types.JPosInf | types.JNegInf ->
      rt_val.throw_range_error(st, "Invalid count value: Infinity")
    _ -> {
      let count = rt_val.jsnum_to_integer_or_infinity(num)
      case count < 0 {
        True ->
          rt_val.throw_range_error(
            st,
            "Invalid count value: " <> int.to_string(count),
          )
        False ->
          case limits.repeat(s, count) {
            Ok(r) -> #(mk_string(r), st)
            Error(Nil) -> rt_val.throw_range_error(st, "Invalid string length")
          }
      }
    }
  }
}

// §22.1.3.16.1 stringpad
fn string_pad(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  pad_fn: fn(String, Int, String) -> Result(String, Nil),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  let #(max_len, st) =
    rt_val.to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let target_len = int.max(max_len, 0)
  let #(filler, st) = case args {
    [_, v, ..] ->
      case classify(v) {
        KUndef -> #(" ", st)
        _ -> rt_val.to_string(st, v)
      }
    _ -> #(" ", st)
  }
  case pad_fn(s, target_len, filler) {
    Ok(r) -> #(mk_string(r), st)
    Error(Nil) -> rt_val.throw_range_error(st, "Invalid string length")
  }
}

fn string_at(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_str(st, this)
  let #(idx, st) =
    rt_val.to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  let actual = case idx < 0 {
    True -> js_string.length(s) + idx
    False -> idx
  }
  case js_string.char_at(s, actual) {
    Some(ch) -> #(ch, st)
    None -> #(mk_undefined(), st)
  }
}

fn string_code_point_at(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_str(st, this)
  let #(pos, st) =
    rt_val.to_integer_or_infinity(st, helpers.first_arg_or_undefined(args))
  case js_string.codepoint_at(s, pos) {
    Some(cp) -> #(mk_int(cp), st)
    None -> #(mk_undefined(), st)
  }
}

fn string_normalize(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  case classify(helpers.first_arg_or_undefined(args)) {
    KUndef -> #(mk_string(nfc(s)), st)
    _ -> {
      let #(form, st) =
        rt_val.to_string(st, helpers.first_arg_or_undefined(args))
      case form {
        "NFC" -> #(mk_string(nfc(s)), st)
        "NFD" -> #(mk_string(nfd(s)), st)
        "NFKC" -> #(mk_string(nfkc(s)), st)
        "NFKD" -> #(mk_string(nfkd(s)), st)
        _ ->
          rt_val.throw_range_error(
            st,
            "The normalization form should be one of NFC, NFD, NFKC, NFKD",
          )
      }
    }
  }
}

// annex b §b.2.2.1
fn string_substr(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(s, st) = with_this_str(st, this)
  let size = js_string.length(s)
  let #(start, st) =
    rt_abstract_ops.relative_index(
      st,
      helpers.first_arg_or_undefined(args),
      size,
      0,
    )
  let #(raw_len, st) =
    second_arg_index_or_len(st, args, size, fn(n, _, _) { n })
  let len = int.clamp(raw_len, 0, size)
  let end = int.min(start + len, size)
  case start >= end {
    True -> #(mk_string(""), st)
    False -> #(js_string.substring(s, start, end - start), st)
  }
}

// no locale support: nfc normalize then compare
fn string_locale_compare(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  let #(that, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let n = case string.compare(nfc(s), nfc(that)) {
    order.Lt -> -1
    order.Eq -> 0
    order.Gt -> 1
  }
  #(mk_int(n), st)
}

fn string_is_well_formed(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(_s, st) = with_this_text(st, this)
  #(mk_bool(True), st)
}

fn string_this_value(
  st: Agent,
  this: JsVal,
  method: String,
) -> #(JsVal, Agent) {
  #(mk_string(this_string_value(st, this, method)), st)
}

// objects only, a primitive is not wrapped to consult its prototype
fn get_method(
  st: Agent,
  val: JsVal,
  symbol: SymbolId,
) -> #(Option(JsVal), Agent) {
  case classify(val) {
    KHandle(_) -> {
      let #(func, st) = rt_val.get_symbol(st, val, symbol)
      case rt_val.is_nullish(func) {
        True -> #(None, st)
        False -> {
          case rt_val.is_callable(st, func) {
            True -> #(Some(func), st)
            False -> rt_val.throw_type_error(st, not_a_function(symbol))
          }
        }
      }
    }
    _ -> #(None, st)
  }
}

fn not_a_function(symbol: SymbolId) -> String {
  symbol_description(symbol)
  |> option.unwrap("Symbol method")
  |> string.append(" is not a function")
}

fn delegate_or_regexp(
  st: Agent,
  val: JsVal,
  symbol: SymbolId,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(method_opt, st) = get_method(st, val, symbol)
  case method_opt {
    Some(method) -> rt_call.call(st, method, val, [this])
    None -> {
      let #(s, st) = rt_val.to_string(st, this)
      let #(rx, st) = b_regexp.create(st, val, mk_undefined())
      let #(method_opt, st) = get_method(st, rx, symbol)
      case method_opt {
        Some(method) -> rt_call.call(st, method, rx, [mk_string(s)])
        None -> rt_val.throw_type_error(st, not_a_function(symbol))
      }
    }
  }
}

fn string_match(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "match")
  delegate_or_regexp(
    st,
    helpers.first_arg_or_undefined(args),
    types.symbol_match,
    this,
  )
}

fn string_search(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "search")
  delegate_or_regexp(
    st,
    helpers.first_arg_or_undefined(args),
    types.symbol_search,
    this,
  )
}

fn string_replace(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "replace")
  let search_val = helpers.first_arg_or_undefined(args)
  let replace_val = helpers.arg_at(args, 1)
  let #(method_opt, st) = get_method(st, search_val, types.symbol_replace)
  case method_opt {
    Some(method) -> rt_call.call(st, method, search_val, [this, replace_val])
    None -> {
      let #(s, st) = rt_val.to_string(st, this)
      let #(search_text, st) = rt_val.to_string(st, search_val)
      replace_string_search(st, s, search_text, replace_val, all: False)
    }
  }
}

fn string_replace_all(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "replaceAll")
  let search_val = helpers.first_arg_or_undefined(args)
  let replace_val = helpers.arg_at(args, 1)
  let #(is_re, st) = b_regexp.is_regexp(st, search_val)
  let st = require_global_when_regexp(st, search_val, is_re, "replaceAll")
  let #(method_opt, st) = get_method(st, search_val, types.symbol_replace)
  case method_opt {
    Some(method) -> rt_call.call(st, method, search_val, [this, replace_val])
    None -> {
      let #(s, st) = rt_val.to_string(st, this)
      let #(search_text, st) = rt_val.to_string(st, search_val)
      replace_string_search(st, s, search_text, replace_val, all: True)
    }
  }
}

fn string_match_all(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "matchAll")
  let regexp_arg = helpers.first_arg_or_undefined(args)
  let #(is_re, st) = b_regexp.is_regexp(st, regexp_arg)
  let st = require_global_when_regexp(st, regexp_arg, is_re, "matchAll")
  let #(method_opt, st) = get_method(st, regexp_arg, types.symbol_match_all)
  case method_opt {
    Some(method) -> rt_call.call(st, method, regexp_arg, [this])
    None -> {
      let #(s, st) = rt_val.to_string(st, this)
      let #(rx, st) = b_regexp.create(st, regexp_arg, mk_string("g"))
      let #(method_opt, st) = get_method(st, rx, types.symbol_match_all)
      case method_opt {
        Some(method) -> rt_call.call(st, method, rx, [mk_string(s)])
        None ->
          rt_val.throw_type_error(st, not_a_function(types.symbol_match_all))
      }
    }
  }
}

fn string_split(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let st = require_object_coercible(st, this, "split")
  let sep_val = helpers.first_arg_or_undefined(args)
  let limit_val = helpers.arg_at(args, 1)
  let #(method_opt, st) = get_method(st, sep_val, types.symbol_split)
  case method_opt {
    Some(method) -> rt_call.call(st, method, sep_val, [this, limit_val])
    None -> {
      let #(s, st) = with_this_text(st, this)
      let #(lim, st) = case classify(limit_val) {
        KUndef -> #(4_294_967_295, st)
        _ -> rt_val.to_uint32(st, limit_val)
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
    KUndef ->
      case lim {
        0 -> realm_ops.new_array(st, [])
        _ -> realm_ops.new_array(st, [mk_string(s)])
      }
    _ -> {
      // tostring(separator) runs before the lim=0 check
      let #(sep, st) = rt_val.to_string(st, sep_val)
      case lim {
        0 -> realm_ops.new_array(st, [])
        _ -> {
          let parts = case sep {
            "" -> utf8.explode(s) |> list.take(lim)
            _ -> utf8.split(s, sep, lim)
          }
          realm_ops.new_array(st, js_string.from_texts(parts))
        }
      }
    }
  }
}

// string-search path of replace and replaceall
fn replace_string_search(
  st: Agent,
  s: String,
  search_text: String,
  replace_val: JsVal,
  all all: Bool,
) -> #(JsVal, Agent) {
  let search_len = utf8.length(search_text)
  case rt_val.is_callable(st, replace_val) {
    True ->
      replace_each_with_fn(
        st,
        s,
        s,
        search_text,
        search_len,
        0,
        [],
        replace_val,
        all,
      )
    False -> {
      let #(template, st) = rt_val.to_string(st, replace_val)
      let segments = substitution.tokenize_plain(template)
      let literal = case segments {
        [] -> Some("")
        [substitution.LiteralSegment(text)] -> Some(text)
        _ -> None
      }
      case literal, search_text {
        Some(text), _ if search_text != "" ->
          string_within_limit(
            st,
            utf8.replace_literal(s, search_text, text, all),
          )
        _, _ -> {
          let needs_before = list.contains(segments, substitution.BeforeSegment)
          let parts =
            replace_each_with_template(
              s,
              search_text,
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
  }
}

fn string_within_limit(st: Agent, s: String) -> #(JsVal, Agent) {
  case string.byte_size(s) > limits.max_string_bytes {
    True -> rt_val.throw_range_error(st, "Invalid string length")
    False -> #(mk_string(s), st)
  }
}

fn replace_each_with_fn(
  st: Agent,
  tail: String,
  s: String,
  search_text: String,
  search_len: Int,
  abs_pos: Int,
  acc: List(String),
  replace_fn: JsVal,
  all all: Bool,
) -> #(JsVal, Agent) {
  case utf8.index_of(tail, search_text, 0) {
    None -> concat_within_limit(st, [tail, ..acc])
    Some(rel) -> {
      let preserved = utf8.slice(tail, 0, rel)
      let after = utf8.drop_start(tail, rel + search_len)
      let p = abs_pos + rel
      let #(result, st) =
        rt_call.call(st, replace_fn, mk_undefined(), [
          mk_string(search_text),
          mk_int(p),
          mk_string(s),
        ])
      let #(replacement, st) = rt_val.to_string(st, result)
      let acc = [replacement, preserved, ..acc]
      case all, search_len {
        False, _ -> concat_within_limit(st, [after, ..acc])
        True, 0 ->
          case after {
            "" -> concat_within_limit(st, acc)
            _ ->
              replace_each_with_fn(
                st,
                utf8.drop_start(after, 1),
                s,
                search_text,
                search_len,
                p + 1,
                [utf8.slice(after, 0, 1), ..acc],
                replace_fn,
                all,
              )
          }
        True, _ ->
          replace_each_with_fn(
            st,
            after,
            s,
            search_text,
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

fn replace_each_with_template(
  tail: String,
  search_text: String,
  search_len: Int,
  segments: List(substitution.PlainSegment),
  needs_before needs_before: Bool,
  before before: String,
  acc acc: List(String),
  all all: Bool,
) -> List(String) {
  case utf8.index_of(tail, search_text, 0) {
    None -> [tail, ..acc]
    Some(rel) -> {
      let preserved = utf8.slice(tail, 0, rel)
      let after = utf8.drop_start(tail, rel + search_len)
      let replacement = case segments {
        [substitution.LiteralSegment(text)] -> text
        _ ->
          substitution.expand_without_named(
            segments,
            substitution.MatchContext(
              matched: search_text,
              before: fn() { before <> preserved },
              after: fn() { after },
              capture: fn(_) { "" },
              capture_count: 0,
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
              let cp = utf8.slice(after, 0, 1)
              let before = case needs_before {
                True -> before <> cp
                False -> ""
              }
              replace_each_with_template(
                utf8.drop_start(after, 1),
                search_text,
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
            True -> before <> preserved <> search_text
            False -> ""
          }
          replace_each_with_template(
            after,
            search_text,
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

fn string_raw(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let template = helpers.first_arg_or_undefined(args)
  let subs = case args {
    [_, ..rest] -> rest
    [] -> []
  }
  let #(raw_val, st) = rt_obj.get_prop(st, template, StringKey(Named("raw")))
  let #(literal_count, st) = rt_abstract_ops.length_of_array_like(st, raw_val)
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
    rt_obj.get_prop(st, raw_val, StringKey(key.canonical(int.to_string(index))))
  let #(lit, st) = rt_val.to_string(st, lit_val)
  let acc_rev = [lit, ..acc_rev]
  case index + 1 == literal_count {
    True -> concat_within_limit(st, acc_rev)
    False ->
      case subs {
        [sub_val, ..rest] -> {
          let #(sub, st) = rt_val.to_string(st, sub_val)
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
      let #(num, st) = rt_val.to_number(st, arg)
      // §7.1.8 touint16
      let n = case num {
        JInt(i) -> i
        JFloat(f) -> rt_val.float_to_int(f)
        _ -> 0
      }
      from_char_code_coerce(st, rest, [modulo_uint16(n), ..acc])
    }
  }
}

fn char_codes_to_string(codes: List(Int), acc: List(UtfCodepoint)) -> String {
  case codes {
    [] -> string.from_utf_codepoints(list.reverse(acc))
    [code, ..rest] -> {
      let #(cp, remaining) = case utf16.is_high(code), rest {
        True, [low, ..after] ->
          case utf16.is_low(low) {
            True -> #(utf16.combine(code, low), after)
            False -> #(code, rest)
          }
        _, _ -> #(code, rest)
      }
      char_codes_to_string(remaining, [codepoint_or_replacement(cp), ..acc])
    }
  }
}

fn string_from_code_point(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  string_from_code_point_loop(st, args, [])
}

fn string_from_code_point_loop(
  st: Agent,
  args: List(JsVal),
  acc: List(UtfCodepoint),
) -> #(JsVal, Agent) {
  case args {
    [] -> #(mk_string(string.from_utf_codepoints(list.reverse(acc))), st)
    [arg, ..rest] -> {
      let #(num, st) = rt_val.to_number(st, arg)
      case num {
        JInt(i) if i >= 0 && i <= 0x10FFFF ->
          string_from_code_point_loop(st, rest, [
            codepoint_or_replacement(i),
            ..acc
          ])
        JFloat(f) ->
          case rt_val.integral_int(f) {
            Some(i) if i >= 0 && i <= 0x10FFFF ->
              string_from_code_point_loop(st, rest, [
                codepoint_or_replacement(i),
                ..acc
              ])
            _ ->
              rt_val.throw_range_error(
                st,
                "Invalid code point " <> rt_val.js_format_float(f),
              )
          }
        JNan -> rt_val.throw_range_error(st, "Invalid code point NaN")
        JInt(i) ->
          rt_val.throw_range_error(
            st,
            "Invalid code point " <> int.to_string(i),
          )
        _ -> rt_val.throw_range_error(st, "Invalid code point Infinity")
      }
    }
  }
}

// annex b §b.2.2 html methods
fn html_wrap(st: Agent, this: JsVal, tag: String) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  #(mk_string("<" <> tag <> ">" <> s <> "</" <> tag <> ">"), st)
}

fn html_wrap_attr(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  tag: String,
  attr: String,
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  let #(attr_val, st) =
    rt_val.to_string(st, helpers.first_arg_or_undefined(args))
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

// an isregexp arg needs coercible flags containing "g"
fn require_global_when_regexp(
  st: Agent,
  val: JsVal,
  is_re is_re: Bool,
  method method: String,
) -> Agent {
  case is_re {
    False -> st
    True -> {
      let #(flags, st) = rt_obj.get_prop(st, val, StringKey(Named("flags")))
      let flags = rt_val.require_object_coercible(st, flags)
      let #(s, st) = rt_val.to_string(st, flags)
      case b_regexp.has_flag(s, "g") {
        True -> st
        False ->
          rt_val.throw_type_error(
            st,
            "String.prototype."
              <> method
              <> " called with a non-global RegExp argument",
          )
      }
    }
  }
}

fn require_object_coercible(st: Agent, this: JsVal, name: String) -> Agent {
  case classify(this) {
    KNull | KUndef ->
      rt_val.throw_type_error(
        st,
        "String.prototype." <> name <> " called on null or undefined",
      )
    _ -> st
  }
}

fn with_this_text(st: Agent, this: JsVal) -> #(String, Agent) {
  case js_string.is_str(this) {
    True -> #(js_string.text(this), st)
    False -> coerce_this_text(st, this)
  }
}

// the js value itself, so length and indexing stay o(1)
fn with_this_str(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case js_string.is_str(this) {
    True -> #(this, st)
    False -> {
      let #(s, st) = coerce_this_text(st, this)
      #(mk_string(s), st)
    }
  }
}

fn coerce_this_text(st: Agent, this: JsVal) -> #(String, Agent) {
  case classify(this) {
    KNull -> rt_val.throw_type_error(st, "Cannot read properties of null")
    KUndef -> rt_val.throw_type_error(st, "Cannot read properties of undefined")
    _ -> rt_val.to_string(st, this)
  }
}

fn string_transform(
  st: Agent,
  this: JsVal,
  transform: fn(String) -> String,
) -> #(JsVal, Agent) {
  let #(s, st) = with_this_text(st, this)
  #(mk_string(transform(s)), st)
}

// §22.1.3 thisstringvalue
fn this_string_value(st: Agent, this: JsVal, method: String) -> String {
  case classify(this) {
    KStr(s) -> s
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: StringObj(value: s), ..) -> s
        _ -> not_a_string(st, method)
      }
    _ -> not_a_string(st, method)
  }
}

fn not_a_string(st: Agent, method: String) -> a {
  rt_val.throw_type_error(
    st,
    "String.prototype." <> method <> " requires that 'this' be a String",
  )
}

fn concat_within_limit(st: Agent, parts_rev: List(String)) -> #(JsVal, Agent) {
  let parts = list.reverse(parts_rev)
  let total =
    list.fold(parts, 0, fn(sum, part) { sum + string.byte_size(part) })
  case total > limits.max_string_bytes {
    True -> rt_val.throw_range_error(st, "Invalid string length")
    False -> #(mk_string(string.concat(parts)), st)
  }
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
    Error(Nil) -> utf8.replacement_codepoint()
  }
}

@external(erlang, "unicode", "characters_to_nfc_binary")
fn nfc(s: String) -> String

@external(erlang, "unicode", "characters_to_nfd_binary")
fn nfd(s: String) -> String

@external(erlang, "unicode", "characters_to_nfkc_binary")
fn nfkc(s: String) -> String

@external(erlang, "unicode", "characters_to_nfkd_binary")
fn nfkd(s: String) -> String
