//// ES2024 §22.2 RegExp Objects
////
//// Internal storage: `RegExpObj(source, flags, last_index, compiled)` exotic
//// kind where `compiled` is an opaque `CompiledRegExp` (§10 vendored engine),
//// filled on first exec and kept in the cell so a pattern compiles once per
//// object. Port of arc `builtins/regexp.gleam` init/dispatch under D7/R1.
//// Actual pattern matching is the `ffi_regexp_exec_compiled` @external stub
//// (§10 `arc_regexp_ffi.erl`); every method body around it — exec,
//// test, [@@match/matchAll/replace/search/split], match-array construction,
//// lastIndex advancement, GetSubstitution — is ported here in full Gleam.

import arc/parser/regex
import arc/parser/regex_error
import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/substitution
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type LegacySlot,
  type LegacyStatics, type ObjectKey, type RegExpFlag, type RegExpNative, Index,
  JInt, KHandle, KNative, KNull, KUndef, LegacyInput, LegacyLastMatch,
  LegacyLastParen, LegacyLeftContext, LegacyParen1, LegacyParen2, LegacyParen3,
  LegacyParen4, LegacyParen5, LegacyParen6, LegacyParen7, LegacyParen8,
  LegacyParen9, LegacyRightContext, LegacyStatics, Named, NoElements, Ordinary,
  RFDotAll, RFGlobal, RFHasIndices, RFIgnoreCase, RFMultiline, RFSticky,
  RFUnicode, RFUnicodeSets, RegExpConstructor, RegExpGetFlag, RegExpGetFlags,
  RegExpGetSource, RegExpLegacyGetter, RegExpLegacyInputSetter, RegExpN,
  RegExpObj, RegExpPrototypeCompile, RegExpPrototypeExec, RegExpPrototypeTest,
  RegExpPrototypeToString, RegExpStringIteratorNext, RegExpSymbolMatch,
  RegExpSymbolMatchAll, RegExpSymbolReplace, RegExpSymbolSearch,
  RegExpSymbolSplit, ReturnThis, SObject, StringKey, classify, mk_bool, mk_null,
  mk_number, mk_object, mk_string, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import arc/vm/limits
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ═══════════════════════════════════════════════════════════════════════════
// Init — RegExp constructor + RegExp.prototype
// ═══════════════════════════════════════════════════════════════════════════

/// Set up RegExp constructor + RegExp.prototype (§22.2.5/6). RegExp.length is 2.
pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  // Prototype methods.
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("exec", RegExpN(RegExpPrototypeExec), 1),
      #("test", RegExpN(RegExpPrototypeTest), 1),
      #("toString", RegExpN(RegExpPrototypeToString), 0),
      #("compile", RegExpN(RegExpPrototypeCompile), 2),
    ])
  // Accessor getters: source, flags, and one per flag.
  let flag_getters =
    list.map(all_flags, fn(f) { #(flag_property(f), RegExpN(RegExpGetFlag(f))) })
  let #(getters, st) =
    common.alloc_getters(
      st,
      fn_proto,
      list.append(
        [
          #("source", RegExpN(RegExpGetSource)),
          #("flags", RegExpN(RegExpGetFlags)),
        ],
        flag_getters,
      ),
    )
  let proto_props = list.append(proto_methods, getters)
  let #(bt, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      proto_props,
      fn(_) { RegExpN(RegExpConstructor(rt_types.empty_legacy_statics())) },
      "RegExp",
      2,
      [],
    )
  let st = install_legacy_accessors(st, fn_proto, bt.constructor)
  // §22.2.6.8-12 Symbol methods — each its own function object.
  let #(st, _) =
    list.fold(
      [
        #(rt_types.symbol_match, RegExpSymbolMatch, "[Symbol.match]", 1),
        #(
          rt_types.symbol_match_all,
          RegExpSymbolMatchAll,
          "[Symbol.matchAll]",
          1,
        ),
        #(rt_types.symbol_replace, RegExpSymbolReplace, "[Symbol.replace]", 2),
        #(rt_types.symbol_search, RegExpSymbolSearch, "[Symbol.search]", 1),
        #(rt_types.symbol_split, RegExpSymbolSplit, "[Symbol.split]", 2),
      ],
      #(st, Nil),
      fn(acc, spec) {
        let #(st, _) = acc
        let #(sym, tok, name, arity) = spec
        let #(fn_h, st) =
          common.alloc_rooted_native_fn(st, fn_proto, RegExpN(tok), name, arity)
        let #(prop, st) = common.builtin_property(st, mk_object(fn_h))
        #(common.add_symbol_property(st, bt.prototype, sym, prop), Nil)
      },
    )
  // §22.2.5.2 get RegExp[@@species].
  let st = common.add_species_accessor(st, fn_proto, bt.constructor, ReturnThis)
  #(bt, st)
}

/// Annex B / legacy-regexp proposal: install RegExp.input/$_, lastMatch/$&,
/// lastParen/$+, leftContext/$`, rightContext/$', $1-$9 as accessor
/// properties on the constructor ({enumerable: false, configurable: true};
/// only input/$_ has a setter).
fn install_legacy_accessors(
  st: Agent,
  fn_proto: Handle,
  ctor: Handle,
) -> Agent {
  let getter_only = [
    #("lastMatch", LegacyLastMatch),
    #("$&", LegacyLastMatch),
    #("lastParen", LegacyLastParen),
    #("$+", LegacyLastParen),
    #("leftContext", LegacyLeftContext),
    #("$`", LegacyLeftContext),
    #("rightContext", LegacyRightContext),
    #("$'", LegacyRightContext),
    #("$1", LegacyParen1),
    #("$2", LegacyParen2),
    #("$3", LegacyParen3),
    #("$4", LegacyParen4),
    #("$5", LegacyParen5),
    #("$6", LegacyParen6),
    #("$7", LegacyParen7),
    #("$8", LegacyParen8),
    #("$9", LegacyParen9),
  ]
  // input/$_ get a setter as well; everything else is getter-only.
  let st =
    list.fold(["input", "$_"], st, fn(st, name) {
      let #(prop, st) =
        common.alloc_get_set_accessor(
          st,
          fn_proto,
          RegExpN(RegExpLegacyGetter(ctor, LegacyInput)),
          RegExpN(RegExpLegacyInputSetter(ctor)),
          name,
        )
      common.add_named_property(st, ctor, name, prop)
    })
  list.fold(getter_only, st, fn(st, spec) {
    let #(name, slot) = spec
    let #(get_h, st) =
      common.alloc_rooted_native_fn(
        st,
        fn_proto,
        RegExpN(RegExpLegacyGetter(ctor, slot)),
        "get " <> name,
        0,
      )
    let #(prop, st) =
      common.accessor_prop(
        st,
        get: Some(mk_object(get_h)),
        set: None,
        enumerable: False,
        configurable: True,
      )
    common.add_named_property(st, ctor, name, prop)
  })
}

// ═══════════════════════════════════════════════════════════════════════════
// Dispatch
// ═══════════════════════════════════════════════════════════════════════════

/// Per-module [[Call]] dispatch for RegExp native functions.
pub fn dispatch(
  st: Agent,
  native: RegExpNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    RegExpConstructor(legacy: _) -> regexp_call(st, args)
    RegExpLegacyGetter(ctor:, slot:) -> legacy_static_get(st, this, ctor, slot)
    RegExpLegacyInputSetter(ctor:) ->
      legacy_static_set_input(st, this, args, ctor)
    RegExpGetSource -> get_source(st, this)
    RegExpGetFlags -> get_flags(st, this)
    RegExpGetFlag(f) -> get_flag(st, this, f)
    RegExpPrototypeToString -> to_string(st, this)
    RegExpPrototypeExec -> regexp_exec(st, this, args)
    RegExpPrototypeTest -> regexp_test(st, this, args)
    RegExpPrototypeCompile -> regexp_compile(st, this, args)
    RegExpSymbolMatch -> regexp_symbol_match(st, this, args)
    RegExpSymbolMatchAll -> regexp_symbol_match_all(st, this, args)
    RegExpSymbolReplace -> regexp_symbol_replace(st, this, args)
    RegExpSymbolSearch -> regexp_symbol_search(st, this, args)
    RegExpSymbolSplit -> regexp_symbol_split(st, this, args)
    RegExpStringIteratorNext -> regexp_string_iterator_next(st, this)
  }
}

/// Per-module [[Construct]] dispatch — §22.2.4.1 with NewTarget defined.
pub fn dispatch_construct(
  st: Agent,
  native: RegExpNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    RegExpConstructor(legacy: _) -> {
      let #(pattern, flags) = helpers.two_args_or_undefined(args)
      // Step 1: Let patternIsRegExp be ? IsRegExp(pattern).
      let #(pattern_is_regexp, st) = is_regexp(st, pattern)
      construct_regexp(st, pattern, pattern_is_regexp, flags, new_target)
    }
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

/// ES2024 §22.2.4.1 RegExp(pattern, flags), NewTarget undefined.
///
///   1. Let patternIsRegExp be ? IsRegExp(pattern).
///   2. If NewTarget is undefined:
///      a. Let newTarget be the active function object (%RegExp%).
///      b. If patternIsRegExp is true and flags is undefined:
///         i.  Let patternConstructor be ? Get(pattern, "constructor").
///         ii. If SameValue(newTarget, patternConstructor), return pattern.
///   4-8. construct_regexp.
fn regexp_call(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(pattern, flags) = helpers.two_args_or_undefined(args)
  let #(pattern_is_regexp, st) = is_regexp(st, pattern)
  let new_target = mk_object(st.realm.regexp.constructor)
  let construct = fn(st) {
    let #(h, st) =
      construct_regexp(st, pattern, pattern_is_regexp, flags, new_target)
    #(mk_object(h), st)
  }
  case pattern_is_regexp, classify(flags) {
    True, KUndef -> {
      let #(ctor, st) = get_named(st, pattern, "constructor")
      case rt_val.same_value(ctor, new_target) {
        True -> #(pattern, st)
        False -> construct(st)
      }
    }
    _, _ -> construct(st)
  }
}

/// §22.2.4.1 steps 4-8: derive P and F, then RegExpAlloc(newTarget) +
/// RegExpInitialize. The Get(newTarget, "prototype") happens AFTER the
/// source/flags reads.
fn construct_regexp(
  st: Agent,
  pattern: JsVal,
  pattern_is_regexp: Bool,
  flags: JsVal,
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(p, f, st) = case regexp_slot(st, pattern) {
    // Step 4: pattern has [[RegExpMatcher]] — reuse original source/flags.
    Some(#(source, orig_flags)) ->
      case classify(flags) {
        KUndef -> #(mk_string(source), mk_string(orig_flags), st)
        _ -> #(mk_string(source), flags, st)
      }
    None ->
      case pattern_is_regexp {
        // Step 5: patternIsRegExp — read source/flags via Get.
        True -> {
          let #(p, st) = get_named(st, pattern, "source")
          case classify(flags) {
            KUndef -> {
              let #(f, st) = get_named(st, pattern, "flags")
              #(p, f, st)
            }
            _ -> #(p, flags, st)
          }
        }
        // Step 6.
        False -> #(pattern, flags, st)
      }
  }
  // Step 7: O = ? RegExpAlloc(newTarget).
  let #(proto, st) =
    proto_from_new_target(st, new_target, st.realm.regexp.prototype)
  // Step 8: ? RegExpInitialize(O, P, F).
  let #(source, flags, st) = pattern_and_flags_from_strings(st, p, f)
  validate_pattern_and_flags(st, source, flags)
  alloc_regexp_with_proto(st, source, flags, proto)
}

/// §22.2.3.2 RegExpCreate(P, F): RegExpAlloc(%RegExp%) + RegExpInitialize.
pub fn regexp_create(st: Agent, p: JsVal, f: JsVal) -> #(JsVal, Agent) {
  let #(source, flags, st) = pattern_and_flags_from_strings(st, p, f)
  validate_pattern_and_flags(st, source, flags)
  let #(h, st) =
    alloc_regexp_with_proto(st, source, flags, st.realm.regexp.prototype)
  #(mk_object(h), st)
}

/// §7.2.8 IsRegExp: an Object whose @@match is truthy, or (when @@match is
/// undefined) one with a [[RegExpMatcher]] slot. The Get is observable.
pub fn is_regexp(st: Agent, val: JsVal) -> #(Bool, Agent) {
  case classify(val) {
    KHandle(_) -> {
      let #(matcher, st) =
        rt_obj.t_get_prop(st, val, rt_types.SymbolKey(rt_types.symbol_match))
      case classify(matcher) {
        KUndef -> #(is_regexp_object(st, val), st)
        _ -> #(rt_val.to_boolean(matcher), st)
      }
    }
    _ -> #(False, st)
  }
}

/// [[OriginalSource]]/[[OriginalFlags]] if `v` has a [[RegExpMatcher]] slot.
fn regexp_slot(st: Agent, v: JsVal) -> Option(#(String, String)) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: RegExpObj(source:, flags:, ..), ..) ->
          Some(#(source, flags))
        _ -> None
      }
    _ -> None
  }
}

// ── legacy static accessors (tc39 proposal-regexp-legacy-features) ──────────

const legacy_receiver_error = "RegExp legacy static properties may only be accessed on the RegExp constructor"

/// GetLegacyRegExpStaticProperty(C, thisValue, slot): throw TypeError unless
/// SameValue(C, thisValue); return the slot's string ("" before any match:
/// InitializeLegacyRegExpStaticProperties sets every slot to the empty
/// String).
fn legacy_static_get(
  st: Agent,
  this: JsVal,
  ctor: Handle,
  slot: LegacySlot,
) -> #(JsVal, Agent) {
  case is_handle(this, ctor) {
    False -> rt_val.t_throw_type_error(st, legacy_receiver_error)
    True ->
      case read_legacy_statics(st, ctor) {
        Some(statics) -> #(mk_string(rt_types.legacy_slot(statics, slot)), st)
        None -> rt_val.t_throw_type_error(st, legacy_receiver_error)
      }
  }
}

/// SetLegacyRegExpStaticProperty(C, thisValue, [[RegExpInput]], val): throw
/// TypeError unless SameValue(C, thisValue); slot = ? ToString(val).
fn legacy_static_set_input(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  ctor: Handle,
) -> #(JsVal, Agent) {
  case is_handle(this, ctor) {
    False -> rt_val.t_throw_type_error(st, legacy_receiver_error)
    True -> {
      let #(s, st) =
        rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
      let st =
        write_legacy_statics(st, ctor, fn(statics) {
          LegacyStatics(..statics, input: s)
        })
      #(mk_undefined(), st)
    }
  }
}

fn is_handle(v: JsVal, h: Handle) -> Bool {
  case classify(v) {
    KHandle(vh) -> vh == h
    _ -> False
  }
}

/// Read the constructor's legacy statics. `None` only when `ctor` is not a
/// %RegExp% constructor object at all, never "this slot was never set", which
/// the typed record makes unrepresentable (every slot always holds a String).
fn read_legacy_statics(st: Agent, ctor: Handle) -> Option(LegacyStatics) {
  case rt_store.t_cell_get(st, ctor) {
    SObject(kind: KNative(tag: RegExpN(RegExpConstructor(legacy:)), ..), ..) ->
      Some(legacy)
    _ -> None
  }
}

/// Rewrite the constructor kind's hidden `legacy` record: internal slots,
/// deliberately NOT properties, so they never appear in
/// Object.getOwnPropertySymbols(RegExp) / Reflect.ownKeys(RegExp).
fn write_legacy_statics(
  st: Agent,
  ctor: Handle,
  update: fn(LegacyStatics) -> LegacyStatics,
) -> Agent {
  use slot <- rt_store.t_cell_update(st, ctor)
  case slot {
    SObject(
      kind: KNative(
        tag: RegExpN(RegExpConstructor(legacy:)),
        name:,
        length:,
        constructible:,
      ),
      ..,
    ) ->
      SObject(
        ..slot,
        kind: KNative(
          tag: RegExpN(RegExpConstructor(update(legacy))),
          name:,
          length:,
          constructible:,
        ),
      )
    other -> other
  }
}

/// UpdateLegacyRegExpStaticProperties: refresh the current realm's %RegExp%
/// legacy state after a successful RegExpBuiltinExec. `whole` is the raw
/// byte-offset span of the whole match; `groups` is captures 1..N (unset
/// groups as start -1).
fn update_legacy_statics(
  st: Agent,
  s: String,
  whole: #(Int, Int),
  groups: List(#(Int, Int)),
) -> Agent {
  let #(match_start, match_len) = whole
  let group_strings = list.map(groups, capture_to_legacy_string(s, _))
  let last_paren = list.last(group_strings) |> result.unwrap("")
  // Groups the pattern doesn't have read as "": the spec's [[RegExpParenN]]
  // for N > the group count.
  let paren = fn(n) {
    helpers.list_at(group_strings, n - 1) |> option.unwrap("")
  }
  use _previous <- write_legacy_statics(st, st.realm.regexp.constructor)
  LegacyStatics(
    input: s,
    last_match: byte_slice(s, match_start, match_len),
    last_paren:,
    left_context: byte_slice(s, 0, match_start),
    right_context: byte_drop_start(s, match_start + match_len),
    paren1: paren(1),
    paren2: paren(2),
    paren3: paren(3),
    paren4: paren(4),
    paren5: paren(5),
    paren6: paren(6),
    paren7: paren(7),
    paren8: paren(8),
    paren9: paren(9),
  )
}

/// A capture's matched text for legacy statics; unset groups become "".
fn capture_to_legacy_string(s: String, cap: #(Int, Int)) -> String {
  case cap {
    #(start, len) if start >= 0 -> byte_slice(s, start, len)
    _ -> ""
  }
}

// ── prototype accessors / toString ──────────────────────────────────────────

fn get_source(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case require_regexp_or_proto(st, this, "source") {
    RSlot(s, _, _) -> #(mk_string(source_string(s)), st)
    RProto -> #(mk_string("(?:)"), st)
  }
}

/// §22.2.6.13.1 EscapeRegExpPattern: empty pattern displays as "(?:)";
/// unescaped "/" becomes "\/" and literal line terminators are escaped so
/// that "/" <> source <> "/" re-parses as the same RegExp literal.
fn source_string(pattern: String) -> String {
  case pattern {
    "" -> "(?:)"
    p -> escape_pattern(bit_array.from_string(p), "")
  }
}

/// Walks code points, not grapheme clusters (`string.contains` and
/// `string.to_graphemes` are cluster-based, so "/" followed by a combining
/// mark would otherwise go unescaped); "\" pairs with exactly the next code
/// point.
fn escape_pattern(chars: BitArray, acc: String) -> String {
  case chars {
    // Keep escape pairs together; an escaped line terminator is rewritten
    // to its escape-sequence form (same matcher semantics, single line).
    <<"\\":utf8, next:utf8_codepoint, rest:bits>> ->
      escape_pattern(rest, acc <> "\\" <> escape_terminator(next))
    <<"/":utf8, rest:bits>> -> escape_pattern(rest, acc <> "\\/")
    <<"\n":utf8, rest:bits>> -> escape_pattern(rest, acc <> "\\n")
    <<"\r":utf8, rest:bits>> -> escape_pattern(rest, acc <> "\\r")
    <<"\u{2028}":utf8, rest:bits>> -> escape_pattern(rest, acc <> "\\u2028")
    <<"\u{2029}":utf8, rest:bits>> -> escape_pattern(rest, acc <> "\\u2029")
    <<ch:utf8_codepoint, rest:bits>> ->
      escape_pattern(rest, acc <> string.from_utf_codepoints([ch]))
    _ -> acc
  }
}

/// The code point following a backslash, rewritten if it is a literal line
/// terminator ("\<LF>" → "\n" keeps the escape's meaning on one line).
fn escape_terminator(cp: UtfCodepoint) -> String {
  case string.utf_codepoint_to_int(cp) {
    0x0A -> "n"
    0x0D -> "r"
    0x2028 -> "u2028"
    0x2029 -> "u2029"
    _ -> string.from_utf_codepoints([cp])
  }
}

fn get_flags(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  // §22.2.6.4: Get() each flag property off `this` in canonical order; the
  // reads are observable (own getters, subclasses) and any object qualifies.
  case classify(this) {
    KHandle(_) -> build_flags(st, this, all_flags, "")
    _ ->
      rt_val.t_throw_type_error(
        st,
        "RegExp.prototype.flags getter called on non-object",
      )
  }
}

fn build_flags(
  st: Agent,
  this: JsVal,
  remaining: List(RegExpFlag),
  acc: String,
) -> #(JsVal, Agent) {
  case remaining {
    [] -> #(mk_string(acc), st)
    [flag, ..rest] -> {
      let #(v, st) = get_named(st, this, flag_property(flag))
      let acc = case rt_val.to_boolean(v) {
        True -> acc <> flag_char(flag)
        False -> acc
      }
      build_flags(st, this, rest, acc)
    }
  }
}

fn get_flag(st: Agent, this: JsVal, flag: RegExpFlag) -> #(JsVal, Agent) {
  case require_regexp_or_proto(st, this, flag_property(flag)) {
    RSlot(_, flags, _) -> #(
      mk_bool(string.contains(flags, flag_char(flag))),
      st,
    )
    RProto -> #(mk_undefined(), st)
  }
}

fn to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  // §22.2.6.17: "/" + source + "/" + flags. Throws if `this` is not an object.
  case classify(this) {
    KHandle(_) -> Nil
    _ ->
      rt_val.t_throw_type_error(
        st,
        "RegExp.prototype.toString called on non-object",
      )
  }
  let #(src_v, st) = get_named(st, this, "source")
  let #(src, st) = rt_val.t_to_string(st, src_v)
  let #(flags_v, st) = get_named(st, this, "flags")
  let #(flags, st) = rt_val.t_to_string(st, flags_v)
  #(mk_string("/" <> src <> "/" <> flags), st)
}

// ── allocation / brand checks ───────────────────────────────────────────────

fn pattern_and_flags_from_strings(
  st: Agent,
  pattern_v: JsVal,
  flags_v: JsVal,
) -> #(String, String, Agent) {
  let #(source, st) = case classify(pattern_v) {
    rt_types.KUndef -> #("", st)
    _ -> rt_val.t_to_string(st, pattern_v)
  }
  let #(flags, st) = case classify(flags_v) {
    rt_types.KUndef -> #("", st)
    _ -> rt_val.t_to_string(st, flags_v)
  }
  #(source, flags, st)
}

/// §13.2.7.3 regular expression literal: RegExpCreate(pattern, flags) on
/// %RegExp.prototype%. Pattern and flags were validated by the parser.
pub fn regexp_create_literal(
  st: Agent,
  source: String,
  flags: String,
) -> #(JsVal, Agent) {
  let #(h, st) =
    alloc_regexp_with_proto(st, source, flags, st.realm.regexp.prototype)
  #(rt_types.mk_object(h), st)
}

fn alloc_regexp_with_proto(
  st: Agent,
  source: String,
  flags: String,
  proto: Handle,
) -> #(Handle, Agent) {
  // §22.2.3.1 RegExpAlloc step 2: lastIndex is {W: true, E: false, C: false}.
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  let li_prop =
    rt_types.DataProperty(
      value: mk_number(JInt(0)),
      writable: True,
      enumerable: False,
      configurable: False,
      seq:,
    )
  rt_store.t_cell_new(
    st,
    SObject(
      kind: RegExpObj(
        source: case source {
          "" -> "(?:)"
          _ -> source
        },
        flags:,
        last_index: 0,
        compiled: uncompiled_regexp(),
      ),
      proto: option.Some(proto),
      props: common.named_props([#("lastIndex", li_prop)]),
      symbol_props: [],
      elements: rt_types.NoElements,
      extensible: True,
    ),
  )
}

fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  fallback: Handle,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_obj.t_get_prop(
      st,
      new_target,
      rt_types.StringKey(rt_types.Named("prototype")),
    )
  case classify(proto) {
    KHandle(h) -> #(h, st)
    _ -> #(fallback, st)
  }
}

/// §22.2.3.4 RegExpInitialize steps 5-8: validate the flags string, then
/// parse the pattern against the ECMAScript Pattern grammar (Annex B
/// extended grammar without u/v, strict grammar with it) — the same
/// validators the parser runs on regex literals. SyntaxError on failure.
fn validate_pattern_and_flags(
  st: Agent,
  pattern: String,
  flags: String,
) -> Nil {
  let bytes = <<pattern:utf8>>
  let checked = {
    use parsed <- result.try(regex.validate_flags(flags))
    regex.validate_pattern(bytes, 0, bit_array.byte_size(bytes), parsed)
  }
  case checked {
    Ok(Nil) -> Nil
    Error(err) ->
      rt_val.t_throw_syntax_error(st, regex_error.pattern_error_message(err))
  }
}

type RegExpRead {
  RSlot(source: String, flags: String, last_index: Int)
  RProto
}

/// §22.2.6.14/4: on the intrinsic %RegExp.prototype% (which is NOT a RegExp
/// instance) the getters return the fallback rather than throwing.
fn require_regexp_or_proto(st: Agent, v: JsVal, op: String) -> RegExpRead {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: RegExpObj(source:, flags:, last_index:, ..), ..) ->
          RSlot(source, flags, last_index)
        _ ->
          case h == st.realm.regexp.prototype {
            True -> RProto
            False -> throw_receiver(st, op)
          }
      }
    _ -> throw_receiver(st, op)
  }
}

fn is_regexp_object(st: Agent, v: JsVal) -> Bool {
  case classify(v) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: RegExpObj(..), ..) -> True
        _ -> False
      }
    _ -> False
  }
}

fn throw_receiver(st: Agent, op: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "Method RegExp.prototype." <> op <> " called on incompatible receiver",
  )
}

// ═══════════════════════════════════════════════════════════════════════════
// §22.2.7 RegExpExec / RegExpBuiltinExec
// ═══════════════════════════════════════════════════════════════════════════

/// Why `ffi_regexp_exec_compiled` produced no match. arc `ExecFailure`.
type ExecFailure {
  NoMatch
  OffsetOutOfRange
  PatternCompileFailed(reason: String)
}

/// §10 FFI: translate and `re:compile` `pattern` under `flags`. The result
/// (matcher + group count + named groups, or the compile failure) is what a
/// RegExpObj keeps in its `compiled` slot.
@external(erlang, "arc_regexp_ffi", "regexp_compile")
fn ffi_regexp_compile(pattern: String, flags: String) -> rt_types.CompiledRegExp

/// Whether a `compiled` slot holds an `ffi_regexp_compile` result yet (as
/// opposed to the `uncompiled_regexp` sentinel).
@external(erlang, "arc_regexp_ffi", "is_compiled")
fn ffi_is_compiled(compiled: rt_types.CompiledRegExp) -> Bool

/// §10 FFI: run a compiled pattern against `s` at byte `offset`. Returns
/// whole-match span, per-group spans (`{-1,0}` = did-not-participate), group
/// count, and (name, capture-index) for named groups.
@external(erlang, "arc_regexp_ffi", "regexp_exec_compiled")
fn ffi_regexp_exec_compiled(
  compiled: rt_types.CompiledRegExp,
  s: String,
  offset: Int,
  sticky: Bool,
) -> Result(
  #(#(Int, Int), List(#(Int, Int)), Int, List(#(String, Int))),
  ExecFailure,
)

/// O(1) sub-binary by byte offsets — regexp indices are bytes (re:run).
/// Offsets are clamped into the string and never raise: a user `exec` may
/// hand back a `matched`/`index` that points past the end of the subject.
@external(erlang, "arc_bytes_ffi", "unsafe_slice")
fn byte_slice(s: String, start: Int, len: Int) -> String

/// O(1) suffix from a byte offset (clamped).
@external(erlang, "arc_bytes_ffi", "drop_start")
fn byte_drop_start(s: String, start: Int) -> String

/// Smallest UTF-8 char boundary strictly > `pos` (AdvanceStringIndex). May
/// return past the end of the string, which loops use as termination.
@external(erlang, "arc_bytes_ffi", "next_char_boundary")
fn next_char_boundary(s: String, pos: Int) -> Int

/// ? Get(O, P) via the observable protocol.
fn try_get(st: Agent, o: JsVal, key: ObjectKey) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, o, key)
}

fn get_named(st: Agent, o: JsVal, name: String) -> #(JsVal, Agent) {
  try_get(st, o, StringKey(Named(name)))
}

/// ? Set(O, P, V, true) — TypeError when [[Set]] returns false.
fn set_throw(st: Agent, h: Handle, name: String, v: JsVal) -> Agent {
  let #(ok, st) = rt_obj.t_set_prop(st, mk_object(h), StringKey(Named(name)), v)
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot assign to read only property '" <> name <> "' of object",
      )
  }
}

fn require_object(st: Agent, v: JsVal, op: String) -> Handle {
  case classify(v) {
    KHandle(h) -> h
    _ ->
      rt_val.t_throw_type_error(
        st,
        "RegExp.prototype" <> op <> " called on non-object",
      )
  }
}

/// §22.2.7.1 RegExpExec(R, S) — calls R.exec if callable (validating result is
/// Object|null), else RegExpBuiltinExec for real RegExps. arc `try_regexp_exec`.
fn regexp_exec_abstract(st: Agent, rx: JsVal, s: String) -> #(JsVal, Agent) {
  let h = require_object(st, rx, ".exec")
  let #(exec_fn, st) = get_named(st, rx, "exec")
  let #(is_call, st) = rt_val.t_is_callable(st, exec_fn)
  case is_call {
    True -> {
      let js = st.store
      let #(result, st) = js.ops.call(st, exec_fn, rx, [mk_string(s)])
      case classify(result) {
        KHandle(_) | KNull -> #(result, st)
        _ ->
          rt_val.t_throw_type_error(
            st,
            "exec method returned something other than an Object or null",
          )
      }
    }
    False ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: RegExpObj(..), ..) -> builtin_exec(st, h, s)
        _ ->
          rt_val.t_throw_type_error(
            st,
            "Method called on incompatible receiver: not a RegExp",
          )
      }
  }
}

/// §22.2.7.2 RegExpBuiltinExec(R, S). arc `try_builtin_exec`.
fn builtin_exec(st: Agent, h: Handle, s: String) -> #(JsVal, Agent) {
  // Step 2: lastIndex = ? ToLength(? Get(R, "lastIndex")).
  let #(li_v, st) = get_named(st, mk_object(h), "lastIndex")
  let #(last_index, st) = rt_val.t_to_length(st, li_v)
  // Re-read [[OriginalFlags]]/[[RegExpMatcher]] AFTER the observable Get —
  // a poisoned lastIndex getter may have compile()'d.
  let #(flags, compiled, st) = regexp_matcher(st, h)
  let global = string.contains(flags, "g")
  let sticky = string.contains(flags, "y")
  let has_indices = string.contains(flags, "d")
  let last_index = case global || sticky {
    True -> last_index
    False -> 0
  }
  case ffi_regexp_exec_compiled(compiled, s, last_index, sticky) {
    Error(NoMatch) | Error(OffsetOutOfRange) | Error(PatternCompileFailed(_)) -> {
      let st = case global || sticky {
        True -> set_throw(st, h, "lastIndex", mk_number(JInt(0)))
        False -> st
      }
      #(mk_null(), st)
    }
    Ok(#(whole, groups, _gc, names)) -> {
      let #(match_start, match_len) = whole
      let e = match_start + match_len
      let st = case global || sticky {
        True -> set_throw(st, h, "lastIndex", mk_number(JInt(e)))
        False -> st
      }
      // Legacy-regexp proposal: UpdateLegacyRegExpStaticProperties on every
      // successful builtin exec (RegExp.input, RegExp.$1-$9, etc.).
      // Unconditional, matching V8/JSC/SpiderMonkey. The proposal gates this on
      // R.[[LegacyFeaturesEnabled]] and otherwise runs
      // InvalidateLegacyRegExpStaticProperties (making the getters throw
      // TypeError); we implement neither half. Gating alone would be strictly
      // wrong: it would leave a *stale* previous match readable through
      // RegExp.$1 & co, a result no engine and no spec produces.
      let st = update_legacy_statics(st, s, whole, groups)
      build_exec_result(st, s, whole, groups, names, has_indices)
    }
  }
}

/// [[OriginalFlags]] and [[RegExpMatcher]] of `h`. The matcher is compiled on
/// first use and written back into the cell, so each RegExp object translates
/// and compiles its pattern at most once.
fn regexp_matcher(
  st: Agent,
  h: Handle,
) -> #(String, rt_types.CompiledRegExp, Agent) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: RegExpObj(source:, flags:, last_index:, compiled:), ..) as slot ->
      case ffi_is_compiled(compiled) {
        True -> #(flags, compiled, st)
        False -> {
          let compiled = ffi_regexp_compile(source, flags)
          let kind = RegExpObj(source:, flags:, last_index:, compiled:)
          let st = rt_store.t_cell_set(st, h, SObject(..slot, kind:))
          #(flags, compiled, st)
        }
      }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "RegExp.prototype.exec requires that 'this' be a RegExp",
      )
  }
}

/// §22.2.7.2 steps 17-34: build the match array with index/input/groups.
fn build_exec_result(
  st: Agent,
  s: String,
  whole: #(Int, Int),
  groups: List(#(Int, Int)),
  names: List(#(String, Int)),
  has_indices: Bool,
) -> #(JsVal, Agent) {
  let #(match_start, match_len) = whole
  let match_values = [
    mk_string(byte_slice(s, match_start, match_len)),
    ..list.map(groups, capture_to_value(s, _))
  ]
  // groups: undefined if no named groups, else null-proto {name: value}.
  let #(groups_val, st) = case names {
    [] -> #(mk_undefined(), st)
    _ -> {
      let values =
        list.map(names, fn(pair) {
          let #(name, idx) = pair
          let v =
            helpers.list_at(groups, idx - 1)
            |> option.map(capture_to_value(s, _))
            |> option.unwrap(mk_undefined())
          #(name, v)
        })
      alloc_null_proto_object(st, dedupe_group_values(values))
    }
  }
  // indices (d flag): array of [start, end] pairs + parallel groups.
  let #(indices_val, st) = case has_indices {
    False -> #(mk_undefined(), st)
    True -> make_indices(st, whole, groups, names)
  }
  let realm = st.realm
  let #(arr_h, st) = common.alloc_array(st, match_values, realm.array.prototype)
  let extra = case classify(indices_val) {
    KUndef -> []
    _ -> [#("indices", indices_val)]
  }
  let st =
    add_own_data_props(st, arr_h, [
      #("index", mk_number(JInt(match_start))),
      #("input", mk_string(s)),
      #("groups", groups_val),
      ..extra
    ])
  #(mk_object(arr_h), st)
}

/// §22.2.7.8 MakeMatchIndicesIndexPairArray (byte-offset).
fn make_indices(
  st: Agent,
  whole: #(Int, Int),
  groups: List(#(Int, Int)),
  names: List(#(String, Int)),
) -> #(JsVal, Agent) {
  let realm = st.realm
  let #(rev_pairs, st) =
    list.fold([whole, ..groups], #([], st), fn(acc, cap) {
      let #(vals, st) = acc
      let #(start, len) = cap
      case start >= 0 {
        True -> {
          let #(pair_h, st) =
            common.alloc_array(
              st,
              [mk_number(JInt(start)), mk_number(JInt(start + len))],
              realm.array.prototype,
            )
          #([mk_object(pair_h), ..vals], st)
        }
        False -> #([mk_undefined(), ..vals], st)
      }
    })
  let pair_values = list.reverse(rev_pairs)
  let #(groups_val, st) = case names {
    [] -> #(mk_undefined(), st)
    _ -> {
      let values =
        list.map(names, fn(pair) {
          let #(name, idx) = pair
          #(
            name,
            helpers.list_at(pair_values, idx) |> option.unwrap(mk_undefined()),
          )
        })
      alloc_null_proto_object(st, dedupe_group_values(values))
    }
  }
  let #(arr_h, st) = common.alloc_array(st, pair_values, realm.array.prototype)
  let st = add_own_data_props(st, arr_h, [#("groups", groups_val)])
  #(mk_object(arr_h), st)
}

/// ES2025 duplicate named groups: first participating capture wins.
fn dedupe_group_values(
  values: List(#(String, JsVal)),
) -> List(#(String, JsVal)) {
  list.fold(values, [], fn(acc, pair) {
    let #(name, v) = pair
    case list.key_find(acc, name) {
      Ok(prev) ->
        case classify(prev) {
          KUndef -> list.key_set(acc, name, v)
          _ -> acc
        }
      Error(Nil) -> list.append(acc, [#(name, v)])
    }
  })
}

fn alloc_null_proto_object(
  st: Agent,
  entries: List(#(String, JsVal)),
) -> #(JsVal, Agent) {
  let #(props, st) =
    list.fold(entries, #([], st), fn(acc, kv) {
      let #(ps, st) = acc
      let #(k, v) = kv
      let #(prop, st) = common.data_property(st, v)
      #([#(k, prop), ..ps], st)
    })
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: None,
        props: common.named_props(list.reverse(props)),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(h), st)
}

fn capture_to_value(s: String, cap: #(Int, Int)) -> JsVal {
  let #(start, len) = cap
  case start >= 0 {
    True -> mk_string(byte_slice(s, start, len))
    False -> mk_undefined()
  }
}

fn add_own_data_props(
  st: Agent,
  h: Handle,
  entries: List(#(String, JsVal)),
) -> Agent {
  list.fold(entries, st, fn(st, kv) {
    let #(k, v) = kv
    let #(prop, st) = common.data_property(st, v)
    rt_store.t_cell_update(st, h, fn(slot) {
      case slot {
        SObject(props:, ..) ->
          SObject(..slot, props: dict.insert(props, Named(k), prop))
        _ -> slot
      }
    })
  })
}

// ── prototype methods ───────────────────────────────────────────────────────

/// §22.2.6.2 RegExp.prototype.exec(string) — requires a real RegExp.
fn regexp_exec(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: RegExpObj(..), ..) -> {
          let #(s, st) =
            rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
          builtin_exec(st, h, s)
        }
        _ -> not_regexp(st, "exec")
      }
    _ -> not_regexp(st, "exec")
  }
}

/// §22.2.6.16 RegExp.prototype.test(string) — generic (RegExpExec).
fn regexp_test(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let _ = require_object(st, this, ".test")
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let #(m, st) = regexp_exec_abstract(st, this, s)
  #(mk_bool(classify(m) != KNull), st)
}

/// Annex B §B.2.4.1 RegExp.prototype.compile(pattern, flags).
fn regexp_compile(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let realm_proto = st.realm.regexp.prototype
  let h = case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: RegExpObj(..), proto: Some(proto), ..)
          if proto == realm_proto
        -> h
        _ -> not_regexp(st, "compile")
      }
    _ -> not_regexp(st, "compile")
  }
  let #(pattern_v, flags_v) = helpers.two_args_or_undefined(args)
  let #(source, flags, st) = case classify(pattern_v) {
    KHandle(ph) ->
      case rt_store.t_cell_get(st, ph) {
        SObject(kind: RegExpObj(source: p, flags: f, ..), ..) ->
          case classify(flags_v) {
            KUndef -> #(p, f, st)
            _ ->
              rt_val.t_throw_type_error(
                st,
                "Cannot supply flags when constructing one RegExp from another",
              )
          }
        _ -> pattern_and_flags_from_strings(st, pattern_v, flags_v)
      }
    _ -> pattern_and_flags_from_strings(st, pattern_v, flags_v)
  }
  validate_pattern_and_flags(st, source, flags)
  let source = case source {
    "" -> "(?:)"
    _ -> source
  }
  let st =
    rt_store.t_cell_update(st, h, fn(slot) {
      case slot {
        SObject(kind: RegExpObj(..), ..) ->
          SObject(
            ..slot,
            kind: RegExpObj(
              source:,
              flags:,
              last_index: 0,
              compiled: uncompiled_regexp(),
            ),
          )
        _ -> slot
      }
    })
  let st = set_throw(st, h, "lastIndex", mk_number(JInt(0)))
  #(this, st)
}

fn not_regexp(st: Agent, method: String) -> a {
  rt_val.t_throw_type_error(
    st,
    "RegExp.prototype." <> method <> " requires that 'this' be a RegExp",
  )
}

// ── @@match ────────────────────────────────────────────────────────────────

/// §22.2.6.8 RegExp.prototype[@@match](string).
fn regexp_symbol_match(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.match]")
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let #(flags_v, st) = get_named(st, this, "flags")
  let #(flags, st) = rt_val.t_to_string(st, flags_v)
  case string.contains(flags, "g") {
    False -> regexp_exec_abstract(st, this, s)
    True -> {
      let st = set_throw(st, h, "lastIndex", mk_number(JInt(0)))
      match_global_loop(st, this, h, s, [], 0)
    }
  }
}

fn match_global_loop(
  st: Agent,
  rx: JsVal,
  h: Handle,
  s: String,
  acc: List(JsVal),
  n: Int,
) -> #(JsVal, Agent) {
  let #(result, st) = regexp_exec_abstract(st, rx, s)
  case classify(result) {
    KNull ->
      case n {
        0 -> #(mk_null(), st)
        _ -> ok_array(st, list.reverse(acc))
      }
    _ -> {
      let #(m_v, st) = try_get(st, result, StringKey(Index(0)))
      let #(match_str, st) = rt_val.t_to_string(st, m_v)
      let st = advance_if_empty(st, h, s, match_str)
      match_global_loop(st, rx, h, s, [mk_string(match_str), ..acc], n + 1)
    }
  }
}

/// §22.2.6.8 step 6.d.iv: on empty match, lastIndex = AdvanceStringIndex.
fn advance_if_empty(
  st: Agent,
  h: Handle,
  s: String,
  match_str: String,
) -> Agent {
  case match_str {
    "" -> {
      let #(li_v, st) = get_named(st, mk_object(h), "lastIndex")
      let #(this_index, st) = rt_val.t_to_length(st, li_v)
      set_throw(
        st,
        h,
        "lastIndex",
        mk_number(JInt(next_char_boundary(s, this_index))),
      )
    }
    _ -> st
  }
}

// ── @@search ───────────────────────────────────────────────────────────────

/// §22.2.6.12 RegExp.prototype[@@search](string).
fn regexp_symbol_search(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.search]")
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let #(previous, st) = get_named(st, this, "lastIndex")
  let st = set_unless_same_value(st, h, previous, mk_number(JInt(0)))
  let #(result, st) = regexp_exec_abstract(st, this, s)
  let #(current, st) = get_named(st, this, "lastIndex")
  let st = set_unless_same_value(st, h, current, previous)
  case classify(result) {
    KNull -> #(mk_number(JInt(-1)), st)
    _ -> get_named(st, result, "index")
  }
}

fn set_unless_same_value(
  st: Agent,
  h: Handle,
  current: JsVal,
  target: JsVal,
) -> Agent {
  case rt_val.same_value(current, target) {
    True -> st
    False -> set_throw(st, h, "lastIndex", target)
  }
}

// ── @@replace ──────────────────────────────────────────────────────────────

type Replacer {
  FunctionalReplacer(fun: JsVal)
  TemplateReplacer(
    with_named: List(substitution.NamedSegment),
    without_named: List(substitution.PlainSegment),
  )
}

/// §22.2.6.11 RegExp.prototype[@@replace](string, replaceValue).
fn regexp_symbol_replace(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.replace]")
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let length_s = string.byte_size(s)
  let replace_value = helpers.arg_at(args, 1)
  let #(is_fn, st) = rt_val.t_is_callable(st, replace_value)
  let #(replacer, st) = case is_fn {
    True -> #(FunctionalReplacer(replace_value), st)
    False -> {
      let #(tpl, st) = rt_val.t_to_string(st, replace_value)
      #(
        TemplateReplacer(
          substitution.tokenize_named(tpl),
          substitution.tokenize_plain(tpl),
        ),
        st,
      )
    }
  }
  let #(flags_v, st) = get_named(st, this, "flags")
  let #(flags, st) = rt_val.t_to_string(st, flags_v)
  let global = string.contains(flags, "g")
  let #(results, st) = case global {
    True -> {
      let st = set_throw(st, h, "lastIndex", mk_number(JInt(0)))
      collect_replace_results(st, this, h, s, [])
    }
    False -> {
      let #(result, st) = regexp_exec_abstract(st, this, s)
      case classify(result) {
        KNull -> #([], st)
        _ -> #([result], st)
      }
    }
  }
  process_replace_results(st, results, s, length_s, replacer, 0, "")
}

fn collect_replace_results(
  st: Agent,
  rx: JsVal,
  h: Handle,
  s: String,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  let #(result, st) = regexp_exec_abstract(st, rx, s)
  case classify(result) {
    KNull -> #(list.reverse(acc), st)
    _ -> {
      let #(m_v, st) = try_get(st, result, StringKey(Index(0)))
      let #(match_str, st) = rt_val.t_to_string(st, m_v)
      let st = advance_if_empty(st, h, s, match_str)
      collect_replace_results(st, rx, h, s, [result, ..acc])
    }
  }
}

fn process_replace_results(
  st: Agent,
  results: List(JsVal),
  s: String,
  length_s: Int,
  replacer: Replacer,
  next_pos: Int,
  acc: String,
) -> #(JsVal, Agent) {
  case results {
    [] -> #(mk_string(acc <> byte_drop_start(s, next_pos)), st)
    [result, ..rest] -> {
      let #(len_v, st) = get_named(st, result, "length")
      let #(result_length, st) = rt_val.t_to_length(st, len_v)
      let n_captures = int.max(result_length - 1, 0)
      let #(m_v, st) = try_get(st, result, StringKey(Index(0)))
      let #(matched, st) = rt_val.t_to_string(st, m_v)
      let #(pos_v, st) = get_named(st, result, "index")
      let #(pos_raw, st) = rt_val.t_to_integer_or_infinity(st, pos_v)
      let position = int.clamp(pos_raw, 0, length_s)
      let #(captures, st) =
        collect_coerced_captures(st, result, 1, n_captures, [])
      let #(named_captures, st) = get_named(st, result, "groups")
      let #(replacement, st) =
        compute_replacement(
          st,
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
            st,
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
            st,
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

fn collect_coerced_captures(
  st: Agent,
  result: JsVal,
  n: Int,
  n_captures: Int,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case n > n_captures {
    True -> #(list.reverse(acc), st)
    False -> {
      let #(cap, st) = try_get(st, result, StringKey(Index(n)))
      case classify(cap) {
        KUndef ->
          collect_coerced_captures(st, result, n + 1, n_captures, [
            mk_undefined(),
            ..acc
          ])
        _ -> {
          let #(cap_str, st) = rt_val.t_to_string(st, cap)
          collect_coerced_captures(st, result, n + 1, n_captures, [
            mk_string(cap_str),
            ..acc
          ])
        }
      }
    }
  }
}

fn compute_replacement(
  st: Agent,
  matched: String,
  s: String,
  position: Int,
  captures: List(JsVal),
  n_captures: Int,
  named_captures: JsVal,
  replacer: Replacer,
) -> #(String, Agent) {
  case replacer {
    FunctionalReplacer(fun) -> {
      let base =
        list.flatten([
          [mk_string(matched)],
          captures,
          [mk_number(JInt(position)), mk_string(s)],
        ])
      let call_args = case classify(named_captures) {
        KUndef -> base
        _ -> list.append(base, [named_captures])
      }
      let js = st.store
      let #(result, st) = js.ops.call(st, fun, mk_undefined(), call_args)
      rt_val.t_to_string(st, result)
    }
    TemplateReplacer(with_named, without_named) -> {
      let ctx =
        substitution.Ctx(
          matched:,
          before: fn() { byte_slice(s, 0, position) },
          after: fn() {
            byte_drop_start(s, position + string.byte_size(matched))
          },
          capture: fn(idx) { capture_or_empty(captures, idx) },
          m: n_captures,
        )
      // 14.l.i: namedCaptures (when present) is ? ToObject'd.
      case classify(named_captures) {
        // No `groups`: the template was tokenized without named references,
        // so nothing here is observable — resolve it in one pass.
        KUndef ->
          finish_replacement(
            st,
            list.reverse(substitution.resolve_plain_parts(without_named, ctx)),
          )
        KNull -> rt_val.t_throw_type_error(st, "Cannot convert null to object")
        _ -> resolve_segments(st, with_named, ctx, named_captures, [])
      }
    }
  }
}

fn resolve_segments(
  st: Agent,
  segments: List(substitution.NamedSegment),
  ctx: substitution.Ctx,
  nc: JsVal,
  acc: List(String),
) -> #(String, Agent) {
  case segments {
    [] -> finish_replacement(st, acc)
    [seg, ..rest] ->
      case substitution.resolve(seg, ctx) {
        substitution.Text(text) ->
          resolve_segments(st, rest, ctx, nc, [text, ..acc])
        substitution.NamedRef(name) -> {
          let #(cap, st) = get_named(st, nc, name)
          case classify(cap) {
            KUndef -> resolve_segments(st, rest, ctx, nc, ["", ..acc])
            _ -> {
              let #(cap_str, st) = rt_val.t_to_string(st, cap)
              resolve_segments(st, rest, ctx, nc, [cap_str, ..acc])
            }
          }
        }
      }
  }
}

fn finish_replacement(st: Agent, rev_parts: List(String)) -> #(String, Agent) {
  let parts = list.reverse(rev_parts)
  let total = list.fold(parts, 0, fn(sum, p) { sum + string.byte_size(p) })
  case total > limits.max_string_bytes {
    True -> rt_val.t_throw_range_error(st, "Invalid string length")
    False -> #(string.concat(parts), st)
  }
}

fn capture_or_empty(captures: List(JsVal), idx: Int) -> String {
  case idx < 1 {
    True -> ""
    False ->
      case helpers.list_at(captures, idx - 1) {
        Some(v) ->
          case classify(v) {
            rt_types.KStr(s) -> s
            _ -> ""
          }
        None -> ""
      }
  }
}

// ── @@split ────────────────────────────────────────────────────────────────

/// §22.2.6.14 RegExp.prototype[@@split](string, limit).
fn regexp_symbol_split(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.split]")
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let realm = st.realm
  let #(c, st) = species_constructor(st, mk_object(h), realm.regexp.constructor)
  let #(flags_v, st) = get_named(st, this, "flags")
  let #(flags, st) = rt_val.t_to_string(st, flags_v)
  let new_flags = case string.contains(flags, "y") {
    True -> flags
    False -> flags <> "y"
  }
  let #(sp_h, st) = rt_call.t_construct(st, c, [this, mk_string(new_flags)], c)
  let splitter = mk_object(sp_h)
  let limit_arg = helpers.arg_at(args, 1)
  let #(lim, st) = case classify(limit_arg) {
    KUndef -> #(4_294_967_295, st)
    _ -> rt_val.t_to_uint32(st, limit_arg)
  }
  let size = string.byte_size(s)
  case lim, size {
    0, _ -> ok_array(st, [])
    _, 0 -> {
      let #(z, st) = regexp_exec_abstract(st, splitter, s)
      case classify(z) {
        KNull -> ok_array(st, [mk_string(s)])
        _ -> ok_array(st, [])
      }
    }
    _, _ -> split_loop(st, splitter, sp_h, s, size, lim, 0, 0, [], 0)
  }
}

fn split_loop(
  st: Agent,
  splitter: JsVal,
  sp_h: Handle,
  s: String,
  size: Int,
  lim: Int,
  p: Int,
  q: Int,
  acc: List(JsVal),
  count: Int,
) -> #(JsVal, Agent) {
  case q >= size {
    True ->
      ok_array(st, list.reverse([mk_string(byte_drop_start(s, p)), ..acc]))
    False -> {
      let st = set_throw(st, sp_h, "lastIndex", mk_number(JInt(q)))
      let #(z, st) = regexp_exec_abstract(st, splitter, s)
      case classify(z) {
        KNull ->
          split_loop(
            st,
            splitter,
            sp_h,
            s,
            size,
            lim,
            p,
            next_char_boundary(s, q),
            acc,
            count,
          )
        _ -> {
          let #(li_v, st) = get_named(st, splitter, "lastIndex")
          let #(e0, st) = rt_val.t_to_length(st, li_v)
          let e = int.min(e0, size)
          case e == p {
            True ->
              split_loop(
                st,
                splitter,
                sp_h,
                s,
                size,
                lim,
                p,
                next_char_boundary(s, q),
                acc,
                count,
              )
            False -> {
              let acc = [mk_string(byte_slice(s, p, q - p)), ..acc]
              let count = count + 1
              case count == lim {
                True -> ok_array(st, list.reverse(acc))
                False -> {
                  let #(len_v, st) = get_named(st, z, "length")
                  let #(z_len, st) = rt_val.t_to_length(st, len_v)
                  let n_caps = int.max(z_len - 1, 0)
                  let #(acc, count, hit, st) =
                    split_captures(st, z, 1, n_caps, acc, count, lim)
                  case hit {
                    True -> ok_array(st, list.reverse(acc))
                    False ->
                      split_loop(
                        st,
                        splitter,
                        sp_h,
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

fn split_captures(
  st: Agent,
  z: JsVal,
  i: Int,
  n_caps: Int,
  acc: List(JsVal),
  count: Int,
  lim: Int,
) -> #(List(JsVal), Int, Bool, Agent) {
  case i > n_caps {
    True -> #(acc, count, False, st)
    False -> {
      let #(cap, st) = try_get(st, z, StringKey(Index(i)))
      let acc = [cap, ..acc]
      let count = count + 1
      case count == lim {
        True -> #(acc, count, True, st)
        False -> split_captures(st, z, i + 1, n_caps, acc, count, lim)
      }
    }
  }
}

// ── @@matchAll + RegExp String Iterator ────────────────────────────────────

/// §22.2.6.9 RegExp.prototype[@@matchAll](string).
fn regexp_symbol_match_all(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.matchAll]")
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  let realm = st.realm
  let #(c, st) = species_constructor(st, mk_object(h), realm.regexp.constructor)
  let #(flags_v, st) = get_named(st, this, "flags")
  let #(flags, st) = rt_val.t_to_string(st, flags_v)
  let #(m_h, st) = rt_call.t_construct(st, c, [this, mk_string(flags)], c)
  let #(li_v, st) = get_named(st, this, "lastIndex")
  let #(last_index, st) = rt_val.t_to_length(st, li_v)
  let st = set_throw(st, m_h, "lastIndex", mk_number(JInt(last_index)))
  let global = string.contains(flags, "g")
  create_regexp_string_iterator(st, m_h, s, global)
}

/// §22.2.9.1 CreateRegExpStringIterator. Iterator state is stored as own data
/// props on an Ordinary object (a `RegExpStringIterator` ObjKind variant would
/// break `rt_gc.gleam`'s exhaustive match, which is out of this port's
/// write-set); `next` is an own method so brand-check == "has these props".
fn create_regexp_string_iterator(
  st: Agent,
  matcher: Handle,
  s: String,
  global: Bool,
) -> #(JsVal, Agent) {
  let realm = st.realm
  let #(next_h, st) =
    rt_call.t_native_new(
      st,
      Some(realm.function.prototype),
      RegExpN(RegExpStringIteratorNext),
      "next",
      0,
      False,
    )
  let #(next_prop, st) = common.builtin_property(st, mk_object(next_h))
  let #(matcher_prop, st) = common.data_prop(st, mk_object(matcher))
  let #(string_prop, st) = common.data_prop(st, mk_string(s))
  let #(global_prop, st) = common.data_prop(st, mk_bool(global))
  let #(done_prop, st) = common.data_property(st, mk_bool(False))
  let #(iter_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: Some(realm.iterator_proto),
        props: common.named_props([
          #("next", next_prop),
          #(rsi_matcher, matcher_prop),
          #(rsi_string, string_prop),
          #(rsi_global, global_prop),
          #(rsi_done, done_prop),
        ]),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(iter_h), st)
}

const rsi_matcher = "[[IteratingRegExp]]"

const rsi_string = "[[IteratedString]]"

const rsi_global = "[[Global]]"

const rsi_done = "[[Done]]"

/// §22.2.9.2.1 %RegExpStringIteratorPrototype%.next().
fn regexp_string_iterator_next(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let h = case classify(this) {
    KHandle(h) -> h
    _ ->
      rt_val.t_throw_type_error(
        st,
        "next method called on incompatible receiver: not an Object",
      )
  }
  let #(matcher, s, global, done) = case read_rsi_state(st, h) {
    Some(state) -> state
    None ->
      rt_val.t_throw_type_error(
        st,
        "next method called on incompatible receiver: not a RegExp String Iterator",
      )
  }
  case done {
    True -> iter_result(st, mk_undefined(), True)
    False -> {
      let #(match, st) = regexp_exec_abstract(st, mk_object(matcher), s)
      case classify(match) {
        KNull -> {
          let st = mark_iter_done(st, h)
          iter_result(st, mk_undefined(), True)
        }
        _ ->
          case global {
            False -> {
              let st = mark_iter_done(st, h)
              iter_result(st, match, False)
            }
            True -> {
              let #(m_v, st) = try_get(st, match, StringKey(Index(0)))
              let #(match_str, st) = rt_val.t_to_string(st, m_v)
              let st = advance_if_empty(st, matcher, s, match_str)
              iter_result(st, match, False)
            }
          }
      }
    }
  }
}

fn read_rsi_state(
  st: Agent,
  h: Handle,
) -> Option(#(Handle, String, Bool, Bool)) {
  case rt_store.t_cell_get(st, h) {
    SObject(props:, ..) -> {
      use m <- option.then(case dict.get(props, Named(rsi_matcher)) {
        Ok(rt_types.DataProperty(value:, ..)) ->
          case classify(value) {
            KHandle(mh) -> Some(mh)
            _ -> None
          }
        _ -> None
      })
      use s <- option.then(case dict.get(props, Named(rsi_string)) {
        Ok(rt_types.DataProperty(value:, ..)) ->
          case classify(value) {
            rt_types.KStr(s) -> Some(s)
            _ -> None
          }
        _ -> None
      })
      use g <- option.then(case dict.get(props, Named(rsi_global)) {
        Ok(rt_types.DataProperty(value:, ..)) -> Some(rt_val.to_boolean(value))
        _ -> None
      })
      use d <- option.map(case dict.get(props, Named(rsi_done)) {
        Ok(rt_types.DataProperty(value:, ..)) -> Some(rt_val.to_boolean(value))
        _ -> None
      })
      #(m, s, g, d)
    }
    _ -> None
  }
}

fn mark_iter_done(st: Agent, h: Handle) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    case slot {
      SObject(props:, ..) ->
        case dict.get(props, Named(rsi_done)) {
          Ok(rt_types.DataProperty(seq:, ..)) ->
            SObject(
              ..slot,
              props: dict.insert(
                props,
                Named(rsi_done),
                rt_types.DataProperty(
                  value: mk_bool(True),
                  writable: True,
                  enumerable: True,
                  configurable: True,
                  seq:,
                ),
              ),
            )
          _ -> slot
        }
      _ -> slot
    }
  })
}

fn iter_result(st: Agent, v: JsVal, done: Bool) -> #(JsVal, Agent) {
  let #(h, st) = rt_async.alloc_iter_result(st, v, done)
  #(mk_object(h), st)
}

// ── shared helpers ─────────────────────────────────────────────────────────

fn ok_array(st: Agent, vals: List(JsVal)) -> #(JsVal, Agent) {
  let #(h, st) = common.alloc_array(st, vals, st.realm.array.prototype)
  #(mk_object(h), st)
}

/// §7.3.22 SpeciesConstructor(O, defaultConstructor).
fn species_constructor(
  st: Agent,
  o: JsVal,
  default_ctor: Handle,
) -> #(JsVal, Agent) {
  let #(c, st) = get_named(st, o, "constructor")
  case classify(c) {
    KUndef -> #(mk_object(default_ctor), st)
    KHandle(_) -> {
      let #(s, st) =
        rt_obj.t_get_prop(st, c, rt_types.SymbolKey(rt_types.symbol_species))
      case classify(s) {
        KUndef | KNull -> #(mk_object(default_ctor), st)
        KHandle(_) -> #(s, st)
        _ ->
          rt_val.t_throw_type_error(
            st,
            "constructor[Symbol.species] is not a constructor",
          )
      }
    }
    _ -> rt_val.t_throw_type_error(st, "object.constructor is not an Object")
  }
}

// ── flag metadata ───────────────────────────────────────────────────────────

const all_flags = [
  RFHasIndices,
  RFGlobal,
  RFIgnoreCase,
  RFMultiline,
  RFDotAll,
  RFUnicode,
  RFUnicodeSets,
  RFSticky,
]

fn flag_property(f: RegExpFlag) -> String {
  case f {
    RFHasIndices -> "hasIndices"
    RFGlobal -> "global"
    RFIgnoreCase -> "ignoreCase"
    RFMultiline -> "multiline"
    RFDotAll -> "dotAll"
    RFUnicode -> "unicode"
    RFUnicodeSets -> "unicodeSets"
    RFSticky -> "sticky"
  }
}

fn flag_char(f: RegExpFlag) -> String {
  case f {
    RFHasIndices -> "d"
    RFGlobal -> "g"
    RFIgnoreCase -> "i"
    RFMultiline -> "m"
    RFDotAll -> "s"
    RFUnicode -> "u"
    RFUnicodeSets -> "v"
    RFSticky -> "y"
  }
}

/// The "not-yet-compiled" sentinel `CompiledRegExp`: a bare atom that
/// `ffi_is_compiled` rejects, replaced by the real matcher on first exec
/// (`regexp_matcher`) and restored whenever source/flags change or the
/// object is written to a snapshot.
@external(erlang, "arc_rt_val_ffi", "mk_undefined")
pub fn uncompiled_regexp() -> rt_types.CompiledRegExp
