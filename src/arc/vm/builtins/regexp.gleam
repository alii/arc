/// ES2024 §22.2 RegExp Objects
///
/// RegExp constructor, prototype methods (test, exec, toString),
/// accessor getters (source, flags, global, ignoreCase, etc.), and the
/// Symbol methods (@@match, @@matchAll, @@replace, @@search, @@split).
///
/// The Symbol methods and `flags` getter are GENERIC per spec — they operate
/// on any object via the observable Get/Set/Call protocol (RegExpExec calls
/// the object's own `exec` if callable; lastIndex goes through [[Get]]/[[Set]]
/// so poisoned getters/valueOf and non-writable lastIndex behave per spec).
///
/// Uses Erlang's `re` module (PCRE) via FFI for actual matching. Indices are
/// BYTE offsets into the UTF-8 subject (deviation: spec uses UTF-16 code
/// units; identical for ASCII subjects).
import arc/parser/regex
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object as ops_object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type RegExpNativeFn, DataProperty, Dispatch, Finite,
  Index, Infinity, JsBool, JsNull, JsNumber, JsObject, JsString, JsUndefined,
  NaN, Named, NativeFunction, NegInfinity, ObjectSlot, OrdinaryObject,
  RegExpConstructor, RegExpGetDotAll, RegExpGetFlags, RegExpGetGlobal,
  RegExpGetHasIndices, RegExpGetIgnoreCase, RegExpGetMultiline, RegExpGetSource,
  RegExpGetSticky, RegExpGetUnicode, RegExpGetUnicodeSets, RegExpLegacyGetter,
  RegExpLegacyInputSetter, RegExpNative, RegExpObject, RegExpPrototypeCompile,
  RegExpPrototypeExec, RegExpPrototypeTest, RegExpPrototypeToString,
  RegExpStringIteratorNext, RegExpSymbolMatch, RegExpSymbolMatchAll,
  RegExpSymbolReplace, RegExpSymbolSearch, RegExpSymbolSplit, WellKnownSymbol,
}
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// FFI: execute pattern on string at byte offset. Returns the capture list
/// padded to group_count+1 entries ({-1,0} = unset group), the total
/// capturing-group count, and [(name, capture_index)] for named groups.
/// `sticky` anchors the match exactly at `offset` (JS `y` semantics).
@external(erlang, "arc_regexp_ffi", "regexp_exec_info")
fn ffi_regexp_exec_info(
  pattern: String,
  flags: String,
  string: String,
  offset: Int,
  sticky: Bool,
) -> Result(#(List(#(Int, Int)), Int, List(#(String, Int))), Nil)

/// FFI: O(1) sub-binary slice by byte offsets. regexp_exec_info returns byte
/// indices (re:run), so all slicing of the subject string must be byte-based —
/// grapheme-based string.slice would be both wrong and O(offset+len).
@external(erlang, "arc_regexp_ffi", "byte_slice")
fn byte_slice(string: String, start: Int, len: Int) -> String

/// FFI: O(1) suffix of the string from a byte offset.
@external(erlang, "arc_regexp_ffi", "byte_drop_start")
fn byte_drop_start(string: String, start: Int) -> String

/// FFI: smallest UTF-8 character boundary strictly after a byte offset
/// (AdvanceStringIndex). Stepping a byte offset by +1 can land mid-character,
/// which makes re:run raise badarg. May return past the end of the
/// string, which loops use as their termination signal.
@external(erlang, "arc_regexp_ffi", "next_char_boundary")
fn next_char_boundary(string: String, position: Int) -> Int

// ---------------------------------------------------------------------------
// Internal slots for RegExp String Iterator objects (§22.2.9).
// Stored as symbol properties under reserved ids no user code can construct.
// ---------------------------------------------------------------------------

const iter_slot_matcher = WellKnownSymbol(9101)

const iter_slot_string = WellKnownSymbol(9102)

const iter_slot_global = WellKnownSymbol(9103)

const iter_slot_unicode = WellKnownSymbol(9104)

const iter_slot_done = WellKnownSymbol(9105)

// ---------------------------------------------------------------------------
// Legacy static property slots (tc39 proposal-regexp-legacy-features).
// Stored in the `legacy` dict inside the %RegExp% constructor's
// NativeFunction(RegExpConstructor) kind, so each realm's constructor
// carries its own state while keeping the slots invisible to property
// enumeration (they are internal slots, not symbol-keyed properties).
// Ids must stay in sync with the `slot` payload of RegExpLegacyGetter.
// ---------------------------------------------------------------------------

const legacy_slot_input = 9110

const legacy_slot_last_match = 9111

const legacy_slot_last_paren = 9112

const legacy_slot_left_context = 9113

const legacy_slot_right_context = 9114

/// $N (1-9) is stored at legacy_slot_paren_base + N.
const legacy_slot_paren_base = 9114

/// Set up RegExp constructor + RegExp.prototype.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  // Allocate prototype methods
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("test", RegExpNative(RegExpPrototypeTest), 1),
      #("exec", RegExpNative(RegExpPrototypeExec), 1),
      #("toString", RegExpNative(RegExpPrototypeToString), 0),
      #("compile", RegExpNative(RegExpPrototypeCompile), 2),
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
      #("unicodeSets", RegExpNative(RegExpGetUnicodeSets)),
      #("hasIndices", RegExpNative(RegExpGetHasIndices)),
    ])

  let proto_props = list.append(proto_methods, getters)

  let #(h, builtin) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_props,
      fn(_) { Dispatch(RegExpNative(RegExpConstructor(dict.new()))) },
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
  let #(h, match_all_fn) =
    common.alloc_native_fn(
      h,
      function_proto,
      RegExpNative(RegExpSymbolMatchAll),
      "[Symbol.matchAll]",
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
                value.symbol_match_all,
                value.builtin_property(JsObject(match_all_fn)),
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
  let h = install_legacy_accessors(h, function_proto, builtin.constructor)
  #(h, builtin)
}

/// Annex B / legacy-regexp proposal: install RegExp.input/$_, lastMatch/$&,
/// lastParen/$+, leftContext/$`, rightContext/$', $1-$9 as accessor
/// properties on the constructor ({enumerable: false, configurable: true};
/// only input/$_ has a setter).
fn install_legacy_accessors(
  h: Heap(host),
  function_proto: Ref,
  ctor: Ref,
) -> Heap(host) {
  let getter_only =
    [
      #("lastMatch", legacy_slot_last_match),
      #("$&", legacy_slot_last_match),
      #("lastParen", legacy_slot_last_paren),
      #("$+", legacy_slot_last_paren),
      #("leftContext", legacy_slot_left_context),
      #("$`", legacy_slot_left_context),
      #("rightContext", legacy_slot_right_context),
      #("$'", legacy_slot_right_context),
    ]
    |> list.append(
      int.range(1, 10, [], fn(acc, n) {
        [#("$" <> int.to_string(n), legacy_slot_paren_base + n), ..acc]
      }),
    )

  // input/$_ get a setter as well; everything else is getter-only.
  let #(h, props) =
    list.fold(["input", "$_"], #(h, []), fn(acc, name) {
      let #(h, props) = acc
      let #(h, get_ref) =
        common.alloc_native_fn(
          h,
          function_proto,
          RegExpNative(RegExpLegacyGetter(ctor, legacy_slot_input)),
          "get " <> name,
          0,
        )
      let #(h, set_ref) =
        common.alloc_native_fn(
          h,
          function_proto,
          RegExpNative(RegExpLegacyInputSetter(ctor)),
          "set " <> name,
          1,
        )
      let prop =
        value.accessor(
          get: Some(JsObject(get_ref)),
          set: Some(JsObject(set_ref)),
          enumerable: False,
          configurable: True,
        )
      #(h, [#(name, prop), ..props])
    })
  let #(h, props) =
    list.fold(getter_only, #(h, props), fn(acc, spec) {
      let #(h, props) = acc
      let #(name, slot) = spec
      let #(h, get_ref) =
        common.alloc_native_fn(
          h,
          function_proto,
          RegExpNative(RegExpLegacyGetter(ctor, slot)),
          "get " <> name,
          0,
        )
      let prop =
        value.accessor(
          get: Some(JsObject(get_ref)),
          set: None,
          enumerable: False,
          configurable: True,
        )
      #(h, [#(name, prop), ..props])
    })

  heap.update(h, ctor, fn(slot) {
    case slot {
      ObjectSlot(properties: existing, ..) ->
        ObjectSlot(
          ..slot,
          properties: list.fold(props, existing, fn(acc, pair) {
            dict.insert(acc, Named(pair.0), pair.1)
          }),
        )
      other -> other
    }
  })
}

/// Per-module dispatch for RegExp native functions.
pub fn dispatch(
  native: RegExpNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    RegExpConstructor(legacy: _) -> regexp_constructor(args, state)
    RegExpPrototypeTest -> regexp_test(this, args, state)
    RegExpPrototypeExec -> regexp_exec(this, args, state)
    RegExpPrototypeToString -> regexp_to_string(this, state)
    RegExpPrototypeCompile -> regexp_compile(this, args, state)
    RegExpGetSource -> regexp_get_source(this, state)
    RegExpGetFlags -> regexp_get_flags(this, state)
    RegExpGetGlobal -> regexp_flag_getter(this, "g", state)
    RegExpGetIgnoreCase -> regexp_flag_getter(this, "i", state)
    RegExpGetMultiline -> regexp_flag_getter(this, "m", state)
    RegExpGetDotAll -> regexp_flag_getter(this, "s", state)
    RegExpGetSticky -> regexp_flag_getter(this, "y", state)
    RegExpGetUnicode -> regexp_flag_getter(this, "u", state)
    RegExpGetUnicodeSets -> regexp_flag_getter(this, "v", state)
    RegExpGetHasIndices -> regexp_flag_getter(this, "d", state)
    RegExpSymbolMatch -> regexp_symbol_match(this, args, state)
    RegExpSymbolMatchAll -> regexp_symbol_match_all(this, args, state)
    RegExpSymbolReplace -> regexp_symbol_replace(this, args, state)
    RegExpSymbolSearch -> regexp_symbol_search(this, args, state)
    RegExpSymbolSplit -> regexp_symbol_split(this, args, state)
    RegExpStringIteratorNext -> regexp_string_iterator_next(this, state)
    RegExpLegacyGetter(ctor, slot) -> legacy_static_get(this, ctor, slot, state)
    RegExpLegacyInputSetter(ctor) ->
      legacy_static_set_input(this, args, ctor, state)
  }
}

// ---------------------------------------------------------------------------
// Legacy static accessors (tc39 proposal-regexp-legacy-features)
// ---------------------------------------------------------------------------

/// GetLegacyRegExpStaticProperty(C, thisValue, slot): throw TypeError unless
/// SameValue(C, thisValue); return the slot's string ("" before any match —
/// InitializeLegacyRegExpStaticProperties sets every slot to the empty
/// String).
fn legacy_static_get(
  this: JsValue,
  ctor: Ref,
  slot: Int,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this == JsObject(ctor) {
    False ->
      state.type_error(
        state,
        "RegExp legacy static properties may only be accessed on the RegExp constructor",
      )
    True -> #(state, Ok(JsString(read_legacy_slot(state, ctor, slot))))
  }
}

/// SetLegacyRegExpStaticProperty(C, thisValue, [[RegExpInput]], val): throw
/// TypeError unless SameValue(C, thisValue); slot = ? ToString(val).
fn legacy_static_set_input(
  this: JsValue,
  args: List(JsValue),
  ctor: Ref,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this == JsObject(ctor) {
    False ->
      state.type_error(
        state,
        "RegExp legacy static properties may only be accessed on the RegExp constructor",
      )
    True -> {
      let val = helpers.first_arg_or_undefined(args)
      use s, state <- coerce.try_to_string(state, val)
      let heap = write_legacy_slots(state.heap, ctor, [#(legacy_slot_input, s)])
      #(State(..state, heap:), Ok(JsUndefined))
    }
  }
}

/// Read one legacy slot off the constructor ("" when never written).
fn read_legacy_slot(state: State(host), ctor: Ref, slot: Int) -> String {
  case heap.read(state.heap, ctor) {
    Some(ObjectSlot(
      kind: NativeFunction(
        native: Dispatch(RegExpNative(RegExpConstructor(legacy:))),
        ..,
      ),
      ..,
    )) -> dict.get(legacy, slot) |> result.unwrap("")
    _ -> ""
  }
}

/// Write legacy slots into the constructor kind's hidden `legacy` state —
/// internal slots, deliberately NOT properties, so they never appear in
/// Object.getOwnPropertySymbols(RegExp) / Reflect.ownKeys(RegExp).
fn write_legacy_slots(
  h: Heap(host),
  ctor: Ref,
  values: List(#(Int, String)),
) -> Heap(host) {
  heap.update(h, ctor, fn(slot) {
    case slot {
      ObjectSlot(
        kind: NativeFunction(
          native: Dispatch(RegExpNative(RegExpConstructor(legacy:))),
          constructible:,
        ),
        ..,
      ) ->
        ObjectSlot(
          ..slot,
          kind: NativeFunction(
            native: Dispatch(
              RegExpNative(
                RegExpConstructor(
                  legacy: list.fold(values, legacy, fn(acc, pair) {
                    dict.insert(acc, pair.0, pair.1)
                  }),
                ),
              ),
            ),
            constructible:,
          ),
        )
      other -> other
    }
  })
}

/// UpdateLegacyRegExpStaticProperties: refresh %RegExp%'s legacy state after
/// a successful RegExpBuiltinExec. `captures` is the raw byte-offset capture
/// list (whole match first, unset groups as start -1).
fn update_legacy_statics(
  state: State(host),
  s: String,
  captures: List(#(Int, Int)),
) -> State(host) {
  let #(match_start, match_len) = case captures {
    [first, ..] -> first
    [] -> #(0, 0)
  }
  let groups = case captures {
    [_, ..rest] -> rest
    [] -> []
  }
  let group_strings = list.map(groups, capture_to_legacy_string(s, _))
  let last_paren = list.last(group_strings) |> result.unwrap("")
  let parens =
    int.range(1, 10, [], fn(acc, n) {
      [
        #(
          legacy_slot_paren_base + n,
          helpers.list_at(group_strings, n - 1) |> option.unwrap(""),
        ),
        ..acc
      ]
    })
  let heap =
    write_legacy_slots(state.heap, state.builtins.regexp.constructor, [
      #(legacy_slot_input, s),
      #(legacy_slot_last_match, byte_slice(s, match_start, match_len)),
      #(legacy_slot_last_paren, last_paren),
      #(legacy_slot_left_context, byte_slice(s, 0, match_start)),
      #(legacy_slot_right_context, byte_drop_start(s, match_start + match_len)),
      ..parens
    ])
  State(..state, heap:)
}

/// A capture's matched text for legacy statics — unset groups become "".
fn capture_to_legacy_string(s: String, cap: #(Int, Int)) -> String {
  case cap {
    #(start, len) if start >= 0 -> byte_slice(s, start, len)
    _ -> ""
  }
}

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

/// ES2024 §22.2.4.1 RegExp(pattern, flags)
fn regexp_constructor(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let pattern = helpers.first_arg_or_undefined(args)
  let flags_arg = case args {
    [_, f, ..] -> f
    _ -> JsUndefined
  }
  // Step 1: Let patternIsRegExp be ? IsRegExp(pattern).
  use pattern_is_regexp, state <- try_is_regexp(state, pattern)
  case pattern {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        // Step 5: pattern has [[RegExpMatcher]] — reuse original source/flags.
        Some(ObjectSlot(kind: RegExpObject(pattern: p, flags: f), ..)) -> {
          let f_val = case flags_arg {
            JsUndefined -> JsString(f)
            other -> other
          }
          regexp_initialize(state, JsString(p), f_val)
        }
        // Step 6: patternIsRegExp — read source/flags via Get.
        _ ->
          case pattern_is_regexp {
            True -> {
              use p_val, state <- try_get(state, ref, "source")
              case flags_arg {
                JsUndefined -> {
                  use f_val, state <- try_get(state, ref, "flags")
                  regexp_initialize(state, p_val, f_val)
                }
                other -> regexp_initialize(state, p_val, other)
              }
            }
            False -> regexp_initialize(state, pattern, flags_arg)
          }
      }
    _ -> regexp_initialize(state, pattern, flags_arg)
  }
}

/// §22.2.3.4 RegExpInitialize steps: coerce pattern/flags (undefined → ""),
/// validate flags, allocate.
fn regexp_initialize(
  state: State(host),
  p_val: JsValue,
  f_val: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use pattern, state <- to_string_or_empty(state, p_val)
  use flags, state <- to_string_or_empty(state, f_val)
  case validate_flags_and_pattern(pattern, flags) {
    Error(msg) -> state.syntax_error(state, msg)
    Ok(Nil) -> {
      let #(heap, ref) =
        alloc_regexp(
          state.heap,
          state.builtins.regexp.prototype,
          pattern,
          flags,
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// §22.2.3.4 RegExpInitialize steps 5-8: validate the flags string, then
/// parse the pattern against the ECMAScript Pattern grammar (Annex B
/// extended grammar without u/v, strict grammar with it). An invalid
/// pattern is a SyntaxError at construction time — same validator the
/// parser runs on regex literals. The returned string is the SyntaxError
/// message to throw.
fn validate_flags_and_pattern(
  pattern: String,
  flags: String,
) -> Result(Nil, String) {
  let flag_list = string.to_graphemes(flags)
  use Nil <- result.try(
    list.try_fold(flag_list, [], validate_flag)
    |> result.replace(Nil),
  )
  let bytes = <<pattern:utf8>>
  regex.validate_pattern(bytes, 0, bit_array.byte_size(bytes), flag_list)
  |> result.map_error(regex.pattern_error_message)
}

/// ToString, except undefined → "" (RegExpInitialize steps 1-2).
fn to_string_or_empty(
  state: State(host),
  val: JsValue,
  cont: fn(String, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    JsUndefined -> cont("", state)
    _ -> coerce.try_to_string(state, val, cont)
  }
}

/// §7.2.6 IsRegExp ( argument )
fn try_is_regexp(
  state: State(host),
  val: JsValue,
  cont: fn(Bool, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    JsObject(ref) -> {
      use matcher, state <- state.try_op(ops_object.get_symbol_value(
        state,
        ref,
        value.symbol_match,
        val,
      ))
      case matcher {
        JsUndefined -> cont(has_regexp_slot(state, ref), state)
        _ -> cont(value.is_truthy(matcher), state)
      }
    }
    _ -> cont(False, state)
  }
}

fn has_regexp_slot(state: State(host), ref: Ref) -> Bool {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: RegExpObject(..), ..)) -> True
    _ -> False
  }
}

/// Fold step for RegExp flag validation: accumulate seen flags, rejecting any
/// invalid character or duplicate.
fn validate_flag(
  seen: List(String),
  f: String,
) -> Result(List(String), String) {
  let is_valid = case f {
    "d" | "g" | "i" | "m" | "s" | "u" | "v" | "y" -> True
    _ -> False
  }
  case is_valid, list.contains(seen, f) {
    False, _ -> Error("Invalid regular expression flag '" <> f <> "'")
    True, True -> Error("Duplicate regular expression flag '" <> f <> "'")
    True, False -> Ok([f, ..seen])
  }
}

/// Allocate a RegExp object on the heap.
pub fn alloc_regexp(
  h: Heap(host),
  regexp_proto: Ref,
  pattern: String,
  flags: String,
) -> #(Heap(host), Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind: RegExpObject(pattern:, flags:),
      properties: common.named_props([
        #("lastIndex", value.data(JsNumber(Finite(0.0))) |> value.writable()),
      ]),
      elements: elements.new(),
      prototype: Some(regexp_proto),
      symbol_properties: [],
      extensible: True,
    ),
  )
}

// ---------------------------------------------------------------------------
// Generic protocol helpers
// ---------------------------------------------------------------------------

/// Require `this` to be an Object; pass its ref. TypeError otherwise.
fn require_object(
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(Ref, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> cont(ref, state)
    _ ->
      state.type_error(
        state,
        "RegExp.prototype" <> method <> " requires that 'this' be an Object",
      )
  }
}

/// ? Get(O, name) through the full observable protocol (getters, proxies).
fn try_get(
  state: State(host),
  ref: Ref,
  name: String,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  state.try_op(
    ops_object.get_value(state, ref, Named(name), JsObject(ref)),
    cont,
  )
}

/// ? Get(O, key) on an arbitrary JsValue receiver (match-result objects).
fn try_get_of(
  state: State(host),
  val: JsValue,
  key: value.PropertyKey,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  state.try_op(ops_object.get_value_of(state, val, key), cont)
}

/// ? Set(O, name, V, true) — throws TypeError when [[Set]] returns false
/// (e.g. non-writable lastIndex).
fn try_set_throw(
  state: State(host),
  ref: Ref,
  name: String,
  val: JsValue,
  cont: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case ops_object.set_value(state, ref, Named(name), val, JsObject(ref)) {
    Ok(#(state, True)) -> cont(state)
    Ok(#(state, False)) ->
      state.type_error(
        state,
        "Cannot assign to read only property '" <> name <> "' of object",
      )
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// §7.1.17 ToLength — full observable ToNumber (valueOf may run), clamped to
/// [0, 2^53-1].
fn try_to_length(
  state: State(host),
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use n, state <- coerce.try_to_number(state, val)
  let len = case n {
    Finite(f) -> int.clamp(value.float_to_int(f), 0, limits.max_safe_integer)
    NaN -> 0
    Infinity -> limits.max_safe_integer
    NegInfinity -> 0
  }
  cont(len, state)
}

/// §7.1.5 ToIntegerOrInfinity followed by a clamp to [low, high].
fn try_to_integer_clamp(
  state: State(host),
  val: JsValue,
  low: Int,
  high: Int,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use n, state <- coerce.try_to_number(state, val)
  let i = case n {
    Finite(f) -> int.clamp(value.float_to_int(f), low, high)
    NaN -> int.clamp(0, low, high)
    Infinity -> high
    NegInfinity -> low
  }
  cont(i, state)
}

/// §7.1.7 ToUint32.
fn try_to_uint32(
  state: State(host),
  val: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use n, state <- coerce.try_to_number(state, val)
  let u = case n {
    Finite(f) -> {
      let m = value.float_to_int(f) % 4_294_967_296
      case m < 0 {
        True -> m + 4_294_967_296
        False -> m
      }
    }
    NaN | Infinity | NegInfinity -> 0
  }
  cont(u, state)
}

/// §22.2.7.1 RegExpExec ( R, S ) — calls R.exec if callable (validating the
/// return is Object or null), else falls back to RegExpBuiltinExec for real
/// RegExp objects.
fn try_regexp_exec(
  state: State(host),
  rx: JsValue,
  s: String,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case rx {
    JsObject(ref) -> {
      use exec_fn, state <- try_get(state, ref, "exec")
      case helpers.is_callable(state.heap, exec_fn) {
        True -> {
          use result, state <- state.try_call(state, exec_fn, rx, [JsString(s)])
          case result {
            JsObject(_) | JsNull -> cont(result, state)
            _ ->
              state.type_error(
                state,
                "exec method returned something other than an Object or null",
              )
          }
        }
        False ->
          case heap.read(state.heap, ref) {
            Some(ObjectSlot(kind: RegExpObject(pattern:, flags:), ..)) -> {
              use result, state <- try_builtin_exec(
                state,
                ref,
                pattern,
                flags,
                s,
              )
              cont(result, state)
            }
            _ ->
              state.type_error(
                state,
                "Method called on incompatible receiver: not a RegExp",
              )
          }
      }
    }
    _ -> state.type_error(state, "RegExpExec requires an Object")
  }
}

// ---------------------------------------------------------------------------
// RegExpBuiltinExec
// ---------------------------------------------------------------------------

/// §22.2.7.2 RegExpBuiltinExec ( R, S ) — returns the match-result array or
/// null. lastIndex is read/written through the observable Get/Set protocol.
fn try_builtin_exec(
  state: State(host),
  ref: Ref,
  pattern: String,
  flags: String,
  s: String,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let length = string.byte_size(s)
  // Step 2: lastIndex = ? ToLength(? Get(R, "lastIndex")) — always read.
  use li_val, state <- try_get(state, ref, "lastIndex")
  use last_index, state <- try_to_length(state, li_val)
  let global = string.contains(flags, "g")
  let sticky = string.contains(flags, "y")
  let has_indices = string.contains(flags, "d")
  // Step 5: if neither global nor sticky, lastIndex = 0.
  let last_index = case global || sticky {
    True -> last_index
    False -> 0
  }
  let exec_result = case last_index > length {
    True -> Error(Nil)
    False -> ffi_regexp_exec_info(pattern, flags, s, last_index, sticky)
  }
  case exec_result {
    // Match failure: reset lastIndex (observable Set) iff global or sticky.
    Error(Nil) ->
      case global || sticky {
        True -> {
          use state <-
            try_set_throw(state, ref, "lastIndex", JsNumber(Finite(0.0)), _)
          cont(JsNull, state)
        }
        False -> cont(JsNull, state)
      }
    Ok(#(captures, _group_count, names)) -> {
      let #(match_start, match_len) = case captures {
        [first, ..] -> first
        [] -> #(last_index, 0)
      }
      let e = match_start + match_len
      // Step 16: Set(R, "lastIndex", e, true) iff global or sticky.
      use state <- maybe_write_last_index(state, ref, global || sticky, e)
      // Legacy-regexp proposal: UpdateLegacyRegExpStaticProperties on every
      // successful builtin exec (RegExp.input, RegExp.$1-$9, etc.).
      let state = update_legacy_statics(state, s, captures)
      build_exec_result(
        state,
        s,
        captures,
        names,
        match_start,
        has_indices,
        cont,
      )
    }
  }
}

fn maybe_write_last_index(
  state: State(host),
  ref: Ref,
  write: Bool,
  e: Int,
  cont: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case write {
    True -> try_set_throw(state, ref, "lastIndex", value.from_int(e), cont)
    False -> cont(state)
  }
}

/// Steps 17-34: build the match-result array with index/input/groups (and
/// indices when the `d` flag is set).
fn build_exec_result(
  state: State(host),
  s: String,
  captures: List(#(Int, Int)),
  names: List(#(String, Int)),
  match_start: Int,
  has_indices: Bool,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let match_values = case captures {
    [#(start, len), ..rest] -> [
      JsString(byte_slice(s, start, len)),
      ..list.map(rest, capture_to_value(s, _))
    ]
    [] -> [JsString("")]
  }
  // groups: undefined when the pattern has no named groups, else a
  // null-prototype object mapping each name to its capture value.
  let #(heap, groups_val) = case names {
    [] -> #(state.heap, JsUndefined)
    _ -> {
      let values =
        list.map(names, fn(pair) {
          let #(name, idx) = pair
          let v =
            helpers.list_at(captures, idx)
            |> option.map(capture_to_value(s, _))
            |> option.unwrap(JsUndefined)
          #(name, v)
        })
      let #(heap, ref) =
        alloc_null_proto_object(state.heap, dedupe_group_values(values))
      #(heap, JsObject(ref))
    }
  }
  let state = State(..state, heap:)

  // indices (when hasIndices): array of [start, end] pairs (undefined for
  // unset groups) with a parallel `groups` object.
  let #(state, indices_val) = case has_indices {
    False -> #(state, JsUndefined)
    True -> make_indices(state, captures, names)
  }

  let #(heap, arr_ref) =
    common.alloc_array(state.heap, match_values, state.builtins.array.prototype)
  let extra = case indices_val {
    JsUndefined -> []
    _ -> [#("indices", value.data_property(indices_val))]
  }
  let heap =
    heap.update(heap, arr_ref, fn(slot) {
      case slot {
        ObjectSlot(properties: props, ..) ->
          ObjectSlot(
            ..slot,
            properties: list.fold(
              [
                #("index", value.data_property(value.from_int(match_start))),
                #("input", value.data_property(JsString(s))),
                #("groups", value.data_property(groups_val)),
                ..extra
              ],
              props,
              fn(acc, pair) { dict.insert(acc, Named(pair.0), pair.1) },
            ),
          )
        other -> other
      }
    })
  cont(JsObject(arr_ref), State(..state, heap:))
}

/// §22.2.7.8 MakeMatchIndicesIndexPairArray (byte-offset approximation).
fn make_indices(
  state: State(host),
  captures: List(#(Int, Int)),
  names: List(#(String, Int)),
) -> #(State(host), JsValue) {
  let #(state, pair_values) =
    list.fold(captures, #(state, []), fn(acc, cap) {
      let #(state, vals) = acc
      case cap {
        #(start, len) if start >= 0 -> {
          let #(heap, pair_ref) =
            common.alloc_array(
              state.heap,
              [value.from_int(start), value.from_int(start + len)],
              state.builtins.array.prototype,
            )
          #(State(..state, heap:), [JsObject(pair_ref), ..vals])
        }
        _ -> #(state, [JsUndefined, ..vals])
      }
    })
  let pair_values = list.reverse(pair_values)
  let #(state, groups_val) = case names {
    [] -> #(state, JsUndefined)
    _ -> {
      let values =
        list.map(names, fn(pair) {
          let #(name, idx) = pair
          let v =
            helpers.list_at(pair_values, idx)
            |> option.unwrap(JsUndefined)
          #(name, v)
        })
      let #(heap, ref) =
        alloc_null_proto_object(state.heap, dedupe_group_values(values))
      #(State(..state, heap:), JsObject(ref))
    }
  }
  let #(heap, arr_ref) =
    common.alloc_array(state.heap, pair_values, state.builtins.array.prototype)
  let heap =
    heap.update(heap, arr_ref, fn(slot) {
      case slot {
        ObjectSlot(properties: props, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(
              props,
              Named("groups"),
              value.data_property(groups_val),
            ),
          )
        other -> other
      }
    })
  #(State(..state, heap:), JsObject(arr_ref))
}

/// One property per unique group name (ES2025 duplicate named groups): the
/// first PARTICIPATING capture wins — at most one alternative can match, so
/// a later non-undefined value replaces an earlier undefined placeholder.
fn dedupe_group_values(
  values: List(#(String, JsValue)),
) -> List(#(String, value.Property)) {
  list.fold(values, [], fn(acc, pair) {
    let #(name, v) = pair
    case list.key_find(acc, name) {
      Ok(JsUndefined) -> list.key_set(acc, name, v)
      Ok(_) -> acc
      Error(Nil) -> list.append(acc, [#(name, v)])
    }
  })
  |> list.map(fn(pair) { #(pair.0, value.data_property(pair.1)) })
}

/// OrdinaryObjectCreate(null) with named data properties.
fn alloc_null_proto_object(
  h: Heap(host),
  props: List(#(String, value.Property)),
) -> #(Heap(host), Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind: OrdinaryObject,
      properties: common.named_props(props),
      elements: elements.new(),
      prototype: None,
      symbol_properties: [],
      extensible: True,
    ),
  )
}

/// Convert a capture tuple (start, len) to JsString slice or JsUndefined if
/// the group did not participate in the match (start < 0).
fn capture_to_value(str: String, cap: #(Int, Int)) -> JsValue {
  case cap {
    #(s, l) if s >= 0 -> JsString(byte_slice(str, s, l))
    _ -> JsUndefined
  }
}

// ---------------------------------------------------------------------------
// Prototype methods
// ---------------------------------------------------------------------------

/// ES2024 §22.2.6.16 RegExp.prototype.test(string) — generic: uses
/// RegExpExec, so a user-defined `exec` is honored.
fn regexp_test(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _ref, state <- require_object(this, state, ".test")
  let arg = helpers.first_arg_or_undefined(args)
  use str, state <- coerce.try_to_string(state, arg)
  use m, state <- try_regexp_exec(state, this, str)
  #(state, Ok(JsBool(m != JsNull)))
}

/// ES2024 §22.2.6.2 RegExp.prototype.exec(string) — requires a real RegExp.
fn regexp_exec(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: RegExpObject(pattern:, flags:), ..)) -> {
          let arg = helpers.first_arg_or_undefined(args)
          use str, state <- coerce.try_to_string(state, arg)
          use result, state <- try_builtin_exec(state, ref, pattern, flags, str)
          #(state, Ok(result))
        }
        _ -> not_regexp(state, "exec")
      }
    _ -> not_regexp(state, "exec")
  }
}

fn not_regexp(
  state: State(host),
  method: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  state.type_error(
    state,
    "RegExp.prototype." <> method <> " requires that 'this' be a RegExp",
  )
}

/// Annex B §B.2.4.1 RegExp.prototype.compile(pattern, flags) — re-initialize
/// an existing RegExp in place and reset lastIndex through [[Set]].
fn regexp_compile(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case compile_receiver_ok(state, ref) {
        False -> not_regexp(state, "compile")
        True -> {
          let pattern = helpers.first_arg_or_undefined(args)
          let flags_arg = case args {
            [_, f, ..] -> f
            _ -> JsUndefined
          }
          case pattern {
            JsObject(p_ref) ->
              case heap.read(state.heap, p_ref) {
                Some(ObjectSlot(kind: RegExpObject(pattern: p, flags: f), ..)) ->
                  // Step 3.a: pattern is a RegExp — flags must be undefined.
                  case flags_arg {
                    JsUndefined ->
                      do_compile(state, ref, JsString(p), JsString(f))
                    _ ->
                      state.type_error(
                        state,
                        "Cannot supply flags when constructing one RegExp from another",
                      )
                  }
                _ -> do_compile(state, ref, pattern, flags_arg)
              }
            _ -> do_compile(state, ref, pattern, flags_arg)
          }
        }
      }
    _ -> not_regexp(state, "compile")
  }
}

/// compile requires a real RegExp whose [[Prototype]] is %RegExp.prototype% —
/// the legacy-regexp proposal makes compile throw TypeError on subclass
/// instances ([[LegacyFeaturesEnabled]] is false for them). Prototype
/// identity is our approximation of that internal slot.
fn compile_receiver_ok(state: State(host), ref: Ref) -> Bool {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: RegExpObject(..), prototype: Some(proto), ..)) ->
      proto == state.builtins.regexp.prototype
    _ -> False
  }
}

/// RegExpInitialize (§22.2.3.4) applied to an existing object: coerce and
/// validate, swap [[OriginalSource]]/[[OriginalFlags]] in place, then
/// Set(obj, "lastIndex", +0, true).
fn do_compile(
  state: State(host),
  ref: Ref,
  p_val: JsValue,
  f_val: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use pattern, state <- to_string_or_empty(state, p_val)
  use flags, state <- to_string_or_empty(state, f_val)
  case validate_flags_and_pattern(pattern, flags) {
    Error(msg) -> state.syntax_error(state, msg)
    Ok(Nil) -> {
      let heap =
        heap.update(state.heap, ref, fn(slot) {
          case slot {
            ObjectSlot(kind: RegExpObject(..), ..) ->
              ObjectSlot(..slot, kind: RegExpObject(pattern:, flags:))
            other -> other
          }
        })
      let state = State(..state, heap:)
      use state <- try_set_throw(state, ref, "lastIndex", JsNumber(Finite(0.0)))
      #(state, Ok(JsObject(ref)))
    }
  }
}

/// ES2024 §22.2.6.17 RegExp.prototype.toString() — generic: reads `source`
/// and `flags` through Get.
fn regexp_to_string(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_object(this, state, ".toString")
  use src_val, state <- try_get(state, ref, "source")
  use src, state <- coerce.try_to_string(state, src_val)
  use flags_val, state <- try_get(state, ref, "flags")
  use flags, state <- coerce.try_to_string(state, flags_val)
  #(state, Ok(JsString("/" <> src <> "/" <> flags)))
}

/// §22.2.6.13 get RegExp.prototype.source — own [[OriginalSource]] or the
/// "(?:)" special case for %RegExp.prototype% itself.
fn regexp_get_source(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: RegExpObject(pattern:, ..), ..)) -> #(
          state,
          Ok(JsString(source_string(pattern))),
        )
        _ ->
          case ref == state.builtins.regexp.prototype {
            True -> #(state, Ok(JsString("(?:)")))
            False -> not_regexp(state, "source")
          }
      }
    _ -> not_regexp(state, "source")
  }
}

/// §22.2.6.4 get RegExp.prototype.flags — GENERIC: reads each flag property
/// through Get on any object and concatenates the canonical letters.
fn regexp_get_flags(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_object(this, state, ".flags")
  build_flags(
    state,
    ref,
    [
      #("hasIndices", "d"),
      #("global", "g"),
      #("ignoreCase", "i"),
      #("multiline", "m"),
      #("dotAll", "s"),
      #("unicode", "u"),
      #("unicodeSets", "v"),
      #("sticky", "y"),
    ],
    "",
  )
}

fn build_flags(
  state: State(host),
  ref: Ref,
  pairs: List(#(String, String)),
  acc: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  case pairs {
    [] -> #(state, Ok(JsString(acc)))
    [#(prop, ch), ..rest] -> {
      use v, state <- try_get(state, ref, prop)
      let acc = case value.is_truthy(v) {
        True -> acc <> ch
        False -> acc
      }
      build_flags(state, ref, rest, acc)
    }
  }
}

/// Per-flag getter (§22.2.6.3 RegExpHasFlag): real RegExp → boolean;
/// %RegExp.prototype% itself → undefined; anything else → TypeError.
fn regexp_flag_getter(
  this: JsValue,
  flag: String,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: RegExpObject(flags:, ..), ..)) -> #(
          state,
          Ok(JsBool(string.contains(flags, flag))),
        )
        _ ->
          case ref == state.builtins.regexp.prototype {
            True -> #(state, Ok(JsUndefined))
            False -> not_regexp(state, "flag getter")
          }
      }
    _ -> not_regexp(state, "flag getter")
  }
}

/// §22.2.6.13.1 EscapeRegExpPattern: empty pattern displays as "(?:)";
/// unescaped "/" becomes "\/" and literal line terminators are escaped so
/// that "/" <> source <> "/" re-parses as the same RegExp literal.
fn source_string(pattern: String) -> String {
  case pattern {
    "" -> "(?:)"
    p ->
      case
        string.contains(p, "/")
        || string.contains(p, "\n")
        || string.contains(p, "\r")
        || string.contains(p, "\u{2028}")
        || string.contains(p, "\u{2029}")
      {
        False -> p
        True -> escape_pattern(string.to_graphemes(p), "")
      }
  }
}

fn escape_pattern(chars: List(String), acc: String) -> String {
  case chars {
    [] -> acc
    // Keep escape pairs together; an escaped line terminator is rewritten
    // to its escape-sequence form (same matcher semantics, single line).
    ["\\", next, ..rest] ->
      escape_pattern(rest, acc <> "\\" <> escape_terminator(next))
    ["\\"] -> acc <> "\\"
    ["/", ..rest] -> escape_pattern(rest, acc <> "\\/")
    ["\n", ..rest] -> escape_pattern(rest, acc <> "\\n")
    ["\r", ..rest] -> escape_pattern(rest, acc <> "\\r")
    ["\u{2028}", ..rest] -> escape_pattern(rest, acc <> "\\u2028")
    ["\u{2029}", ..rest] -> escape_pattern(rest, acc <> "\\u2029")
    [ch, ..rest] -> escape_pattern(rest, acc <> ch)
  }
}

/// The character following a backslash, rewritten if it is a literal line
/// terminator ("\<LF>" → "\n" keeps the escape's meaning on one line).
fn escape_terminator(ch: String) -> String {
  case ch {
    "\n" -> "n"
    "\r" -> "r"
    "\u{2028}" -> "u2028"
    "\u{2029}" -> "u2029"
    other -> other
  }
}

// ---------------------------------------------------------------------------
// @@match
// ---------------------------------------------------------------------------

/// ES2024 §22.2.6.8 RegExp.prototype[@@match](string)
fn regexp_symbol_match(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_object(this, state, "[Symbol.match]")
  let arg = helpers.first_arg_or_undefined(args)
  use s, state <- coerce.try_to_string(state, arg)
  use flags_val, state <- try_get(state, ref, "flags")
  use flags, state <- coerce.try_to_string(state, flags_val)
  case string.contains(flags, "g") {
    False -> try_regexp_exec(state, this, s, fn(r, state) { #(state, Ok(r)) })
    True -> {
      use state <-
        try_set_throw(state, ref, "lastIndex", JsNumber(Finite(0.0)), _)
      match_global_loop(state, this, ref, s, [], 0)
    }
  }
}

fn match_global_loop(
  state: State(host),
  rx: JsValue,
  ref: Ref,
  s: String,
  acc: List(JsValue),
  n: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  use result, state <- try_regexp_exec(state, rx, s)
  case result {
    JsNull ->
      case n {
        0 -> #(state, Ok(JsNull))
        _ -> state.ok_array(state, list.reverse(acc))
      }
    _ -> {
      use m_val, state <- try_get_of(state, result, Index(0))
      use match_str, state <- coerce.try_to_string(state, m_val)
      use state <- advance_if_empty(state, ref, s, match_str)
      match_global_loop(state, rx, ref, s, [JsString(match_str), ..acc], n + 1)
    }
  }
}

/// On an empty match: lastIndex = AdvanceStringIndex(S, ToLength(Get(R,
/// "lastIndex"))) via the observable protocol (§22.2.6.8 step 6.d.iv).
fn advance_if_empty(
  state: State(host),
  ref: Ref,
  s: String,
  match_str: String,
  cont: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case match_str {
    "" -> {
      use li_val, state <- try_get(state, ref, "lastIndex")
      use this_index, state <- try_to_length(state, li_val)
      let next = next_char_boundary(s, this_index)
      try_set_throw(state, ref, "lastIndex", value.from_int(next), cont)
    }
    _ -> cont(state)
  }
}

// ---------------------------------------------------------------------------
// @@search
// ---------------------------------------------------------------------------

/// ES2024 §22.2.6.12 RegExp.prototype[@@search](string)
fn regexp_symbol_search(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_object(this, state, "[Symbol.search]")
  let arg = helpers.first_arg_or_undefined(args)
  use s, state <- coerce.try_to_string(state, arg)
  use previous, state <- try_get(state, ref, "lastIndex")
  use state <- set_unless_same_value(
    state,
    ref,
    previous,
    JsNumber(Finite(0.0)),
  )
  use result, state <- try_regexp_exec(state, this, s)
  use current, state <- try_get(state, ref, "lastIndex")
  use state <- set_unless_same_value(state, ref, current, previous)
  case result {
    JsNull -> #(state, Ok(JsNumber(Finite(-1.0))))
    _ -> {
      use idx, state <- try_get_of(state, result, Named("index"))
      #(state, Ok(idx))
    }
  }
}

/// If SameValue(current, target) is false, perform ? Set(R, "lastIndex",
/// target, true) (§22.2.6.12 steps 4 and 8).
fn set_unless_same_value(
  state: State(host),
  ref: Ref,
  current: JsValue,
  target: JsValue,
  cont: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case value.same_value(current, target) {
    True -> cont(state)
    False -> try_set_throw(state, ref, "lastIndex", target, cont)
  }
}

// ---------------------------------------------------------------------------
// @@replace
// ---------------------------------------------------------------------------

/// ES2024 §22.2.6.11 RegExp.prototype[@@replace](string, replaceValue)
fn regexp_symbol_replace(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_object(this, state, "[Symbol.replace]")
  let arg = helpers.first_arg_or_undefined(args)
  use s, state <- coerce.try_to_string(state, arg)
  let length_s = string.byte_size(s)
  let replace_value = case args {
    [_, rv, ..] -> rv
    _ -> JsUndefined
  }
  let functional = helpers.is_callable(state.heap, replace_value)
  // Step 5: non-callable replaceValue is coerced to a string (and the
  // template tokenized) exactly once, before the match loop.
  use replacer, state <- with_replacer(state, replace_value, functional)
  use flags_val, state <- try_get(state, ref, "flags")
  use flags, state <- coerce.try_to_string(state, flags_val)
  let global = string.contains(flags, "g")
  case global {
    True -> {
      use state <-
        try_set_throw(state, ref, "lastIndex", JsNumber(Finite(0.0)), _)
      use results, state <- collect_replace_results(state, this, ref, s, [])
      process_replace_results(state, results, s, length_s, replacer, 0, "")
    }
    False -> {
      use result, state <- try_regexp_exec(state, this, s)
      case result {
        JsNull -> #(state, Ok(JsString(s)))
        _ ->
          process_replace_results(state, [result], s, length_s, replacer, 0, "")
      }
    }
  }
}

/// Step 11: repeat RegExpExec, collecting non-null results; on an empty
/// matched string advance lastIndex (observable).
fn collect_replace_results(
  state: State(host),
  rx: JsValue,
  ref: Ref,
  s: String,
  acc: List(JsValue),
  cont: fn(List(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use result, state <- try_regexp_exec(state, rx, s)
  case result {
    JsNull -> cont(list.reverse(acc), state)
    _ -> {
      use m_val, state <- try_get_of(state, result, Index(0))
      use match_str, state <- coerce.try_to_string(state, m_val)
      use state <- advance_if_empty(state, ref, s, match_str)
      collect_replace_results(state, rx, ref, s, [result, ..acc], cont)
    }
  }
}

/// Step 14: process each match result in order, accumulating the output.
fn process_replace_results(
  state: State(host),
  results: List(JsValue),
  s: String,
  length_s: Int,
  replacer: Replacer,
  next_pos: Int,
  acc: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  case results {
    [] -> #(state, Ok(JsString(acc <> byte_drop_start(s, next_pos))))
    [result, ..rest] -> {
      // 14.a-b: nCaptures = max(LengthOfArrayLike(result) - 1, 0).
      use len_val, state <- try_get_of(state, result, Named("length"))
      use result_length, state <- try_to_length(state, len_val)
      let n_captures = int.max(result_length - 1, 0)
      // 14.c: matched = ToString(Get(result, "0")).
      use m_val, state <- try_get_of(state, result, Index(0))
      use matched, state <- coerce.try_to_string(state, m_val)
      // 14.e-f: position = clamp(ToIntegerOrInfinity(Get(result, "index"))).
      use pos_val, state <- try_get_of(state, result, Named("index"))
      use position, state <- try_to_integer_clamp(state, pos_val, 0, length_s)
      // 14.g: captures (each coerced to String unless undefined).
      use captures, state <- collect_coerced_captures(
        state,
        result,
        1,
        n_captures,
        [],
      )
      // 14.h: namedCaptures = Get(result, "groups").
      use named_captures, state <- try_get_of(state, result, Named("groups"))
      use replacement, state <- compute_replacement(
        state,
        matched,
        s,
        position,
        captures,
        n_captures,
        named_captures,
        replacer,
      )
      case position >= next_pos {
        True -> {
          let acc =
            acc <> byte_slice(s, next_pos, position - next_pos) <> replacement
          process_replace_results(
            state,
            rest,
            s,
            length_s,
            replacer,
            position + string.byte_size(matched),
            acc,
          )
        }
        False ->
          process_replace_results(
            state,
            rest,
            s,
            length_s,
            replacer,
            next_pos,
            acc,
          )
      }
    }
  }
}

/// 14.g: capN = Get(result, n); if not undefined, capN = ToString(capN).
fn collect_coerced_captures(
  state: State(host),
  result: JsValue,
  n: Int,
  n_captures: Int,
  acc: List(JsValue),
  cont: fn(List(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case n > n_captures {
    True -> cont(list.reverse(acc), state)
    False -> {
      use cap, state <- try_get_of(state, result, Index(n))
      case cap {
        JsUndefined ->
          collect_coerced_captures(
            state,
            result,
            n + 1,
            n_captures,
            [JsUndefined, ..acc],
            cont,
          )
        _ -> {
          use cap_str, state <- coerce.try_to_string(state, cap)
          collect_coerced_captures(
            state,
            result,
            n + 1,
            n_captures,
            [JsString(cap_str), ..acc],
            cont,
          )
        }
      }
    }
  }
}

/// 14.i-j: functional replace via Call, else GetSubstitution on the
/// pre-tokenized template.
fn compute_replacement(
  state: State(host),
  matched: String,
  s: String,
  position: Int,
  captures: List(JsValue),
  n_captures: Int,
  named_captures: JsValue,
  replacer: Replacer,
  cont: fn(String, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case replacer {
    FunctionalReplacer(fun) -> {
      let base =
        list.flatten([
          [JsString(matched)],
          captures,
          [value.from_int(position), JsString(s)],
        ])
      let call_args = case named_captures {
        JsUndefined -> base
        _ -> list.append(base, [named_captures])
      }
      use result, state <- state.try_call(state, fun, JsUndefined, call_args)
      coerce.try_to_string(state, result, cont)
    }
    TemplateReplacer(with_named, without_named) -> {
      // 14.j.i: namedCaptures (when present) is ToObject'd.
      use named, state <- to_named_captures_object(state, named_captures)
      let segments = case named {
        Some(_) -> with_named
        None -> without_named
      }
      resolve_segments(
        state,
        segments,
        matched,
        s,
        position,
        captures,
        n_captures,
        named,
        [],
        cont,
      )
    }
  }
}

fn to_named_captures_object(
  state: State(host),
  named_captures: JsValue,
  cont: fn(Option(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case named_captures {
    JsUndefined -> cont(None, state)
    JsNull -> state.type_error(state, "Cannot convert null to object")
    _ -> cont(Some(named_captures), state)
  }
}

/// How each match gets replaced: call a function, or expand a pre-tokenized
/// string template. "$<" parses differently depending on whether the match
/// result has a defined `groups` (namedCaptures) — with groups it is a named
/// reference scanned to ">"; without, it is the 2-char literal "$<" and
/// scanning resumes immediately after it — so both tokenizations are kept
/// and resolve_segments picks per result.
type Replacer {
  FunctionalReplacer(fun: JsValue)
  TemplateReplacer(
    with_named: List(ReplaceSegment),
    without_named: List(ReplaceSegment),
  )
}

/// Build the Replacer for [@@replace]. Non-functional replaceValue is coerced
/// to a string and tokenized exactly once here.
fn with_replacer(
  state: State(host),
  replace_value: JsValue,
  functional_replace: Bool,
  cont: fn(Replacer, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case functional_replace {
    True -> cont(FunctionalReplacer(replace_value), state)
    False -> {
      use template, state <- coerce.try_to_string(state, replace_value)
      cont(
        TemplateReplacer(
          tokenize_template(template, True),
          tokenize_template(template, False),
        ),
        state,
      )
    }
  }
}

/// One pre-tokenized piece of a replacement template (GetSubstitution,
/// ES2024 §22.1.3.19.1). The template is tokenized once per replace call so
/// each match only resolves segments. The common no-dollar template is a
/// single LiteralSeg.
type ReplaceSegment {
  /// Literal text — also covers "$$" → "$".
  LiteralSeg(text: String)
  /// "$&" — the matched substring.
  MatchedSeg
  /// "$`" — the portion of the string before the match.
  BeforeSeg
  /// "$'" — the portion of the string after the match.
  AfterSeg
  /// "$N" (N in 1-9) — capture group N if N ≤ m, else the literal "$N".
  CaptureSeg(idx: Int)
  /// "$NN" (first digit 1-9): group NN if NN ≤ m; else group N (first digit)
  /// followed by the literal second digit if N ≤ m; else literal.
  TwoDigitSeg(two_idx: Int, one_idx: Int, suffix: String)
  /// "$0d" (d in 1-9): group d if d ≤ m, else the literal "$0d".
  ZeroDigitSeg(two_idx: Int, literal: String)
  /// "$<name>" — named capture: Get(namedCaptures, name) then ToString
  /// (undefined → ""). When namedCaptures is undefined the whole "$<name>"
  /// is literal text.
  NamedSeg(name: String)
}

/// Tokenize a replacement template into segments. Called once per replace
/// call; templates without "$" skip grapheme segmentation entirely.
/// `named_mode` selects the "$<" interpretation (see Replacer).
fn tokenize_template(
  template: String,
  named_mode: Bool,
) -> List(ReplaceSegment) {
  case string.contains(template, "$") {
    False -> [LiteralSeg(template)]
    True -> tokenize_loop(string.to_graphemes(template), named_mode, "", [])
  }
}

fn flush_literal(
  lit: String,
  segs: List(ReplaceSegment),
) -> List(ReplaceSegment) {
  case lit {
    "" -> segs
    _ -> [LiteralSeg(lit), ..segs]
  }
}

fn tokenize_loop(
  chars: List(String),
  named_mode: Bool,
  lit: String,
  segs: List(ReplaceSegment),
) -> List(ReplaceSegment) {
  case chars {
    [] -> list.reverse(flush_literal(lit, segs))
    ["$", "$", ..rest] -> tokenize_loop(rest, named_mode, lit <> "$", segs)
    ["$", "&", ..rest] ->
      tokenize_loop(rest, named_mode, "", [
        MatchedSeg,
        ..flush_literal(lit, segs)
      ])
    ["$", "`", ..rest] ->
      tokenize_loop(rest, named_mode, "", [
        BeforeSeg,
        ..flush_literal(lit, segs)
      ])
    ["$", "'", ..rest] ->
      tokenize_loop(rest, named_mode, "", [AfterSeg, ..flush_literal(lit, segs)])
    // "$<": named reference in named mode (scanned to ">"); otherwise the
    // 2-char literal "$<" with scanning resumed right after it.
    ["$", "<", ..rest] ->
      case named_mode {
        True ->
          case take_group_name(rest, "") {
            Some(#(name, rest2)) ->
              tokenize_loop(rest2, named_mode, "", [
                NamedSeg(name),
                ..flush_literal(lit, segs)
              ])
            None -> tokenize_loop(rest, named_mode, lit <> "$<", segs)
          }
        False -> tokenize_loop(rest, named_mode, lit <> "$<", segs)
      }
    ["$", d1, d2, ..rest] ->
      case is_digit(d1), is_digit(d2) {
        True, True -> tokenize_two_digit(d1, d2, rest, named_mode, lit, segs)
        // "$N" followed by a non-digit — d2 is rescanned (it may start "$&").
        True, False ->
          tokenize_one_digit(d1, [d2, ..rest], named_mode, lit, segs)
        // Not a reference — "$" is literal, rescan from d1.
        False, _ ->
          tokenize_loop([d1, d2, ..rest], named_mode, lit <> "$", segs)
      }
    ["$", d1] ->
      case is_digit(d1) {
        True -> tokenize_one_digit(d1, [], named_mode, lit, segs)
        False -> tokenize_loop([d1], named_mode, lit <> "$", segs)
      }
    [ch, ..rest] -> tokenize_loop(rest, named_mode, lit <> ch, segs)
  }
}

/// Scan a "$<name>" group name up to the closing ">". None if unterminated.
fn take_group_name(
  chars: List(String),
  acc: String,
) -> Option(#(String, List(String))) {
  case chars {
    [] -> None
    [">", ..rest] -> Some(#(acc, rest))
    [ch, ..rest] -> take_group_name(rest, acc <> ch)
  }
}

/// "$N": a CaptureSeg for $1-$9; "$0" stays literal.
fn tokenize_one_digit(
  d1: String,
  rest: List(String),
  named_mode: Bool,
  lit: String,
  segs: List(ReplaceSegment),
) -> List(ReplaceSegment) {
  case digit_value(d1) {
    0 -> tokenize_loop(rest, named_mode, lit <> "$0", segs)
    idx ->
      tokenize_loop(rest, named_mode, "", [
        CaptureSeg(idx),
        ..flush_literal(lit, segs)
      ])
  }
}

/// "$NN": prefer the two-digit group, falling back to the single-digit group
/// + literal second digit, or the literal "$0d" when the first digit is 0.
fn tokenize_two_digit(
  d1: String,
  d2: String,
  rest: List(String),
  named_mode: Bool,
  lit: String,
  segs: List(ReplaceSegment),
) -> List(ReplaceSegment) {
  let two_idx = digit_value(d1) * 10 + digit_value(d2)
  case digit_value(d1), two_idx {
    // "$00" can never resolve to a group — always literal.
    0, 0 -> tokenize_loop(rest, named_mode, lit <> "$00", segs)
    0, _ ->
      tokenize_loop(rest, named_mode, "", [
        ZeroDigitSeg(two_idx, "$0" <> d2),
        ..flush_literal(lit, segs)
      ])
    one_idx, _ ->
      tokenize_loop(rest, named_mode, "", [
        TwoDigitSeg(two_idx, one_idx, d2),
        ..flush_literal(lit, segs)
      ])
  }
}

/// §22.1.3.19.1 GetSubstitution over a pre-tokenized template, CPS because
/// "$<name>" performs an observable Get + ToString per occurrence.
fn resolve_segments(
  state: State(host),
  segments: List(ReplaceSegment),
  matched: String,
  s: String,
  position: Int,
  captures: List(JsValue),
  m: Int,
  named: Option(JsValue),
  acc: List(String),
  cont: fn(String, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case segments {
    [] -> {
      let parts = list.reverse(acc)
      let total =
        list.fold(parts, 0, fn(sum, part) { sum + string.byte_size(part) })
      case total > limits.max_string_bytes {
        True -> state.range_error(state, "Invalid string length")
        False -> cont(string.concat(parts), state)
      }
    }
    [seg, ..rest] -> {
      let plain = fn(text: String, state: State(host)) {
        resolve_segments(
          state,
          rest,
          matched,
          s,
          position,
          captures,
          m,
          named,
          [text, ..acc],
          cont,
        )
      }
      case seg {
        LiteralSeg(text) -> plain(text, state)
        MatchedSeg -> plain(matched, state)
        BeforeSeg -> plain(byte_slice(s, 0, position), state)
        AfterSeg ->
          plain(byte_drop_start(s, position + string.byte_size(matched)), state)
        CaptureSeg(idx) ->
          case idx <= m {
            True -> plain(capture_or_empty(captures, idx), state)
            False -> plain("$" <> int.to_string(idx), state)
          }
        TwoDigitSeg(two_idx, one_idx, suffix) ->
          case two_idx <= m, one_idx <= m {
            True, _ -> plain(capture_or_empty(captures, two_idx), state)
            False, True ->
              plain(capture_or_empty(captures, one_idx) <> suffix, state)
            False, False ->
              plain("$" <> int.to_string(one_idx) <> suffix, state)
          }
        ZeroDigitSeg(two_idx, literal) ->
          case two_idx <= m && two_idx >= 1 {
            True -> plain(capture_or_empty(captures, two_idx), state)
            False -> plain(literal, state)
          }
        NamedSeg(name) ->
          case named {
            None -> plain("$<" <> name <> ">", state)
            Some(nc) -> {
              use cap, state <- try_get_of(state, nc, Named(name))
              case cap {
                JsUndefined -> plain("", state)
                _ -> {
                  use cap_str, state <- coerce.try_to_string(state, cap)
                  plain(cap_str, state)
                }
              }
            }
          }
      }
    }
  }
}

/// captures[idx-1] as a string; "" for an in-range undefined capture.
fn capture_or_empty(captures: List(JsValue), idx: Int) -> String {
  use <- bool.guard(idx < 1, "")
  case helpers.list_at(captures, idx - 1) {
    Some(JsString(s)) -> s
    _ -> ""
  }
}

fn digit_value(ch: String) -> Int {
  case ch {
    "1" -> 1
    "2" -> 2
    "3" -> 3
    "4" -> 4
    "5" -> 5
    "6" -> 6
    "7" -> 7
    "8" -> 8
    "9" -> 9
    _ -> 0
  }
}

fn is_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

// ---------------------------------------------------------------------------
// @@split
// ---------------------------------------------------------------------------

/// ES2024 §22.2.6.14 RegExp.prototype[@@split](string, limit)
fn regexp_symbol_split(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_object(this, state, "[Symbol.split]")
  let arg = helpers.first_arg_or_undefined(args)
  use s, state <- coerce.try_to_string(state, arg)
  // Step 3: C = SpeciesConstructor(rx, %RegExp%).
  use c, state <- try_species_constructor(state, ref)
  use flags_val, state <- try_get(state, ref, "flags")
  use flags, state <- coerce.try_to_string(state, flags_val)
  let new_flags = case string.contains(flags, "y") {
    True -> flags
    False -> flags <> "y"
  }
  // Step 10: splitter = Construct(C, «rx, newFlags»).
  use splitter, state <- state.try_op(
    state.construct(state, c, [
      this,
      JsString(new_flags),
    ]),
  )
  case splitter {
    JsObject(sp_ref) -> {
      // Step 13: lim = limit undefined ? 2^32-1 : ToUint32(limit).
      let limit_arg = case args {
        [_, l, ..] -> l
        _ -> JsUndefined
      }
      use lim, state <- split_limit(state, limit_arg)
      let size = string.byte_size(s)
      case lim, size {
        0, _ -> state.ok_array(state, [])
        _, 0 -> {
          // Step 16: empty string — single exec decides [] vs [""].
          use z, state <- try_regexp_exec(state, splitter, s)
          case z {
            JsNull -> state.ok_array(state, [JsString(s)])
            _ -> state.ok_array(state, [])
          }
        }
        _, _ -> split_loop(state, splitter, sp_ref, s, size, lim, 0, 0, [], 0)
      }
    }
    _ -> state.type_error(state, "splitter is not an object")
  }
}

fn split_limit(
  state: State(host),
  limit_arg: JsValue,
  cont: fn(Int, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case limit_arg {
    JsUndefined -> cont(4_294_967_295, state)
    _ -> try_to_uint32(state, limit_arg, cont)
  }
}

/// §22.2.6.14 steps 17-19: scan from q, splitting at sticky matches.
/// p = end of the last match consumed; q = current scan position.
/// AdvanceStringIndex is next_char_boundary (a full UTF-8 character step,
/// covering both unicode and non-unicode modes in the byte domain).
fn split_loop(
  state: State(host),
  splitter: JsValue,
  sp_ref: Ref,
  s: String,
  size: Int,
  lim: Int,
  p: Int,
  q: Int,
  acc: List(JsValue),
  count: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  case q >= size {
    // Step 18-19: append the remainder S[p..size] and return.
    True ->
      state.ok_array(
        state,
        list.reverse([JsString(byte_drop_start(s, p)), ..acc]),
      )
    False -> {
      // 17.a: Set(splitter, "lastIndex", q, true).
      use state <- try_set_throw(state, sp_ref, "lastIndex", value.from_int(q))
      use z, state <- try_regexp_exec(state, splitter, s)
      case z {
        JsNull ->
          split_loop(
            state,
            splitter,
            sp_ref,
            s,
            size,
            lim,
            p,
            next_char_boundary(s, q),
            acc,
            count,
          )
        _ -> {
          // 17.d.i-ii: e = min(ToLength(Get(splitter, "lastIndex")), size).
          use li_val, state <- try_get(state, sp_ref, "lastIndex")
          use e0, state <- try_to_length(state, li_val)
          let e = int.min(e0, size)
          case e == p {
            True ->
              split_loop(
                state,
                splitter,
                sp_ref,
                s,
                size,
                lim,
                p,
                next_char_boundary(s, q),
                acc,
                count,
              )
            False -> {
              let acc = [JsString(byte_slice(s, p, q - p)), ..acc]
              let count = count + 1
              case count == lim {
                True -> state.ok_array(state, list.reverse(acc))
                False -> {
                  use len_val, state <- try_get_of(state, z, Named("length"))
                  use z_len, state <- try_to_length(state, len_val)
                  let n_caps = int.max(z_len - 1, 0)
                  use #(acc, count, hit_limit), state <- split_captures(
                    state,
                    z,
                    1,
                    n_caps,
                    acc,
                    count,
                    lim,
                  )
                  case hit_limit {
                    True -> state.ok_array(state, list.reverse(acc))
                    False ->
                      split_loop(
                        state,
                        splitter,
                        sp_ref,
                        s,
                        size,
                        lim,
                        e,
                        e,
                        acc,
                        count,
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
}

/// 17.d.iv.7-10: append captures Get(z, i) for i in 1..numberOfCaptures,
/// stopping when the limit is reached.
fn split_captures(
  state: State(host),
  z: JsValue,
  i: Int,
  n_caps: Int,
  acc: List(JsValue),
  count: Int,
  lim: Int,
  cont: fn(#(List(JsValue), Int, Bool), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case i > n_caps {
    True -> cont(#(acc, count, False), state)
    False -> {
      use cap, state <- try_get_of(state, z, Index(i))
      let acc = [cap, ..acc]
      let count = count + 1
      case count == lim {
        True -> cont(#(acc, count, True), state)
        False -> split_captures(state, z, i + 1, n_caps, acc, count, lim, cont)
      }
    }
  }
}

/// §7.3.22 SpeciesConstructor ( O, %RegExp% )
fn try_species_constructor(
  state: State(host),
  ref: Ref,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let default_ctor = JsObject(state.builtins.regexp.constructor)
  use c, state <- try_get(state, ref, "constructor")
  case c {
    JsUndefined -> cont(default_ctor, state)
    JsObject(c_ref) -> {
      use species, state <- state.try_op(ops_object.get_symbol_value(
        state,
        c_ref,
        value.symbol_species,
        c,
      ))
      case species {
        JsUndefined | JsNull -> cont(default_ctor, state)
        _ ->
          case ops_object.is_constructor(state.heap, species) {
            True -> cont(species, state)
            False ->
              state.type_error(
                state,
                "species constructor is not a constructor",
              )
          }
      }
    }
    _ -> state.type_error(state, "constructor property is not an object")
  }
}

// ---------------------------------------------------------------------------
// @@matchAll + RegExp String Iterator
// ---------------------------------------------------------------------------

/// ES2024 §22.2.6.9 RegExp.prototype[@@matchAll](string)
fn regexp_symbol_match_all(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, state <- require_object(this, state, "[Symbol.matchAll]")
  let arg = helpers.first_arg_or_undefined(args)
  use s, state <- coerce.try_to_string(state, arg)
  use c, state <- try_species_constructor(state, ref)
  use flags_val, state <- try_get(state, ref, "flags")
  use flags, state <- coerce.try_to_string(state, flags_val)
  use matcher, state <- state.try_op(
    state.construct(state, c, [
      this,
      JsString(flags),
    ]),
  )
  use li_val, state <- try_get(state, ref, "lastIndex")
  use last_index, state <- try_to_length(state, li_val)
  case matcher {
    JsObject(m_ref) -> {
      use state <- try_set_throw(
        state,
        m_ref,
        "lastIndex",
        value.from_int(last_index),
      )
      let global = string.contains(flags, "g")
      let full_unicode =
        string.contains(flags, "u") || string.contains(flags, "v")
      create_regexp_string_iterator(state, matcher, s, global, full_unicode)
    }
    _ -> state.type_error(state, "constructed matcher is not an object")
  }
}

/// §22.2.9.1 CreateRegExpStringIterator. The iterator inherits from
/// %Iterator.prototype% and carries an own `next` method; its internal state
/// lives in reserved symbol slots.
fn create_regexp_string_iterator(
  state: State(host),
  matcher: JsValue,
  s: String,
  global: Bool,
  full_unicode: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, next_fn) =
    common.alloc_native_fn(
      state.heap,
      state.builtins.function.prototype,
      RegExpNative(RegExpStringIteratorNext),
      "next",
      0,
    )
  let #(heap, iter_ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: common.named_props([
          #("next", value.builtin_property(JsObject(next_fn))),
        ]),
        elements: elements.new(),
        prototype: Some(state.builtins.iterator.prototype),
        symbol_properties: [
          #(iter_slot_matcher, value.data(matcher)),
          #(iter_slot_string, value.data(JsString(s))),
          #(iter_slot_global, value.data(JsBool(global))),
          #(iter_slot_unicode, value.data(JsBool(full_unicode))),
          #(iter_slot_done, value.data(JsBool(False))),
        ],
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(iter_ref)))
}

/// Internal iterator state read out of the reserved symbol slots.
type IterState {
  IterState(matcher: JsValue, string: String, global: Bool, done: Bool)
}

fn read_iter_state(state: State(host), ref: Ref) -> Option(IterState) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(symbol_properties: sp, ..)) -> {
      let find = fn(key) {
        case list.key_find(sp, key) {
          Ok(DataProperty(value: v, ..)) -> Some(v)
          _ -> None
        }
      }
      case
        find(iter_slot_matcher),
        find(iter_slot_string),
        find(iter_slot_global),
        find(iter_slot_done)
      {
        Some(matcher),
          Some(JsString(s)),
          Some(JsBool(global)),
          Some(JsBool(done))
        -> Some(IterState(matcher:, string: s, global:, done:))
        _, _, _, _ -> None
      }
    }
    _ -> None
  }
}

fn mark_iter_done(state: State(host), ref: Ref) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(symbol_properties: sp, ..) ->
          ObjectSlot(
            ..slot,
            symbol_properties: list.key_set(
              sp,
              iter_slot_done,
              value.data(JsBool(True)),
            ),
          )
        other -> other
      }
    })
  State(..state, heap:)
}

/// §22.2.9.2.1 %RegExpStringIteratorPrototype%.next ( )
fn regexp_string_iterator_next(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case read_iter_state(state, ref) {
        None ->
          state.type_error(
            state,
            "next method called on incompatible receiver: not a RegExp String Iterator",
          )
        Some(IterState(done: True, ..)) -> iter_result(state, JsUndefined, True)
        Some(IterState(matcher:, string: s, global:, done: False)) -> {
          use match, state <- try_regexp_exec(state, matcher, s)
          case match {
            JsNull -> {
              let state = mark_iter_done(state, ref)
              iter_result(state, JsUndefined, True)
            }
            _ ->
              case global {
                False -> {
                  let state = mark_iter_done(state, ref)
                  iter_result(state, match, False)
                }
                True -> {
                  use m_val, state <- try_get_of(state, match, Index(0))
                  use match_str, state <- coerce.try_to_string(state, m_val)
                  case matcher {
                    JsObject(m_ref) -> {
                      use state <- advance_if_empty(state, m_ref, s, match_str)
                      iter_result(state, match, False)
                    }
                    _ -> iter_result(state, match, False)
                  }
                }
              }
          }
        }
      }
    _ ->
      state.type_error(
        state,
        "next method called on incompatible receiver: not an Object",
      )
  }
}

fn iter_result(
  state: State(host),
  val: JsValue,
  done: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, result) =
    common.create_iter_result(state.heap, state.builtins, val, done)
  #(State(..state, heap:), Ok(result))
}
