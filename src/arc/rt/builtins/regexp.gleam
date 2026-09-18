import arc/bytecode/key.{type PropertyKey, Named}
import arc/internal/bytes
import arc/internal/unsafe
import arc/parser/regex
import arc/parser/regex_error
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/call as rt_call
import arc/rt/elements
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type LegacyStatic,
  type LegacyStatics, type Property, type RegExpFlag, type RegExpNative,
  ArrayObj, DataProperty, DotAllFlag, GlobalFlag, HasIndicesFlag, IgnoreCaseFlag,
  KHandle, KNull, KUndef, LegacyInput, LegacyLastMatch, LegacyLastParen,
  LegacyLeftContext, LegacyParen1, LegacyParen2, LegacyParen3, LegacyParen4,
  LegacyParen5, LegacyParen6, LegacyParen7, LegacyParen8, LegacyParen9,
  LegacyRightContext, LegacyStatics, MultilineFlag, NativeFn, Ordinary,
  RegExpConstructor, RegExpGetFlag, RegExpGetFlags, RegExpGetSource,
  RegExpLegacyGetter, RegExpLegacyInputSetter, RegExpN, RegExpObj,
  RegExpPrototypeCompile, RegExpPrototypeExec, RegExpPrototypeTest,
  RegExpPrototypeToString, RegExpStringIteratorNext, RegExpSymbolMatch,
  RegExpSymbolMatchAll, RegExpSymbolReplace, RegExpSymbolSearch,
  RegExpSymbolSplit, ReturnThis, SObject, StickyFlag, UnicodeFlag,
  UnicodeSetsFlag, classify, mk_bool, mk_int, mk_null, mk_object, mk_string,
  mk_undefined, plain_object,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(proto_methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("exec", RegExpN(RegExpPrototypeExec), 1),
      #("test", RegExpN(RegExpPrototypeTest), 1),
      #("toString", RegExpN(RegExpPrototypeToString), 0),
      #("compile", RegExpN(RegExpPrototypeCompile), 2),
    ])
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
      fn(_) {
        RegExpN(RegExpConstructor(
          types.empty_legacy_statics(),
          None,
          dict.new(),
        ))
      },
      "RegExp",
      2,
      [],
    )
  let st = install_legacy_accessors(st, fn_proto, bt.constructor)
  let st =
    list.fold(
      [
        #(types.symbol_match, RegExpSymbolMatch, "[Symbol.match]", 1),
        #(types.symbol_match_all, RegExpSymbolMatchAll, "[Symbol.matchAll]", 1),
        #(types.symbol_replace, RegExpSymbolReplace, "[Symbol.replace]", 2),
        #(types.symbol_search, RegExpSymbolSearch, "[Symbol.search]", 1),
        #(types.symbol_split, RegExpSymbolSplit, "[Symbol.split]", 2),
      ],
      st,
      fn(st, spec) {
        let #(sym, tok, name, arity) = spec
        let #(fn_h, st) =
          common.alloc_rooted_native_fn(st, fn_proto, RegExpN(tok), name, arity)
        let #(prop, st) = rt_store.builtin_property(st, mk_object(fn_h))
        common.add_symbol_property(st, bt.prototype, sym, prop)
      },
    )
  let st = common.add_species_accessor(st, fn_proto, bt.constructor, ReturnThis)
  #(bt, st)
}

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
    let #(name, which) = spec
    let #(get_h, st) =
      common.alloc_rooted_native_fn(
        st,
        fn_proto,
        RegExpN(RegExpLegacyGetter(ctor, which)),
        "get " <> name,
        0,
      )
    let #(prop, st) =
      common.accessor_property(
        st,
        get: Some(mk_object(get_h)),
        set: None,
        enumerable: False,
        configurable: True,
      )
    common.add_named_property(st, ctor, name, prop)
  })
}

pub fn dispatch(
  st: Agent,
  native: RegExpNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    RegExpSymbolMatch
    | RegExpSymbolMatchAll
    | RegExpSymbolReplace
    | RegExpSymbolSearch
    | RegExpSymbolSplit
    | RegExpStringIteratorNext -> panic as "routed by builtins.dispatch_native"
    RegExpConstructor(..) -> regexp_call(st, args)
    RegExpLegacyGetter(ctor:, which:) ->
      legacy_static_get(st, this, ctor, which)
    RegExpLegacyInputSetter(ctor:) ->
      legacy_static_set_input(st, this, args, ctor)
    RegExpGetSource -> get_source(st, this)
    RegExpGetFlags -> get_flags(st, this)
    RegExpGetFlag(f) -> get_flag(st, this, f)
    RegExpPrototypeToString -> to_string(st, this)
    RegExpPrototypeExec -> regexp_exec(st, this, args)
    RegExpPrototypeTest -> regexp_test(st, this, args)
    RegExpPrototypeCompile -> prototype_compile(st, this, args)
  }
}

pub fn dispatch_construct(
  st: Agent,
  native: RegExpNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    RegExpConstructor(..) -> {
      let #(pattern, flags) = helpers.two_args_or_undefined(args)
      let #(pattern_is_regexp, st) = is_regexp(st, pattern)
      construct_regexp(st, pattern, pattern_is_regexp, flags, new_target)
    }
    _ -> rt_val.throw_type_error(st, "not a constructor")
  }
}

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

fn construct_regexp(
  st: Agent,
  pattern: JsVal,
  pattern_is_regexp pattern_is_regexp: Bool,
  flags flags: JsVal,
  new_target new_target: JsVal,
) -> #(Handle, Agent) {
  let #(p, f, st) = case regexp_source_flags(st, pattern) {
    Some(#(source, orig_flags)) ->
      case classify(flags) {
        KUndef -> #(mk_string(source), mk_string(orig_flags), st)
        _ -> #(mk_string(source), flags, st)
      }
    None ->
      case pattern_is_regexp {
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
        False -> #(pattern, flags, st)
      }
  }
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(r) {
      r.regexp.prototype
    })
  let #(source, flags, st) = pattern_and_flags_from_strings(st, p, f)
  validate_pattern_and_flags(st, source, flags)
  alloc_regexp_with_proto(st, source, flags, proto)
}

pub fn create(st: Agent, p: JsVal, f: JsVal) -> #(JsVal, Agent) {
  let #(source, flags, st) = pattern_and_flags_from_strings(st, p, f)
  validate_pattern_and_flags(st, source, flags)
  let #(h, st) =
    alloc_regexp_with_proto(st, source, flags, st.realm.regexp.prototype)
  #(mk_object(h), st)
}

pub fn is_regexp(st: Agent, val: JsVal) -> #(Bool, Agent) {
  case classify(val) {
    KHandle(_) -> {
      let #(matcher, st) = helpers.get_symbol(st, val, types.symbol_match)
      case classify(matcher) {
        KUndef -> #(is_regexp_object(st, val), st)
        _ -> #(rt_val.to_boolean(matcher), st)
      }
    }
    _ -> #(False, st)
  }
}

fn regexp_source_flags(st: Agent, v: JsVal) -> Option(#(String, String)) {
  case classify(v) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: RegExpObj(source:, flags:, ..), ..) ->
          Some(#(source, flags))
        _ -> None
      }
    _ -> None
  }
}

const legacy_receiver_error = "RegExp legacy static properties may only be accessed on the RegExp constructor"

fn legacy_static_get(
  st: Agent,
  this: JsVal,
  ctor: Handle,
  which: LegacyStatic,
) -> #(JsVal, Agent) {
  case is_handle(this, ctor) {
    False -> rt_val.throw_type_error(st, legacy_receiver_error)
    True ->
      case read_legacy_statics(st, ctor) {
        Some(statics) -> #(mk_string(legacy_static_value(statics, which)), st)
        None -> rt_val.throw_type_error(st, legacy_receiver_error)
      }
  }
}

fn legacy_static_set_input(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  ctor: Handle,
) -> #(JsVal, Agent) {
  case is_handle(this, ctor) {
    False -> rt_val.throw_type_error(st, legacy_receiver_error)
    True -> {
      let #(s, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
      let st =
        write_legacy_statics(st, ctor, fn(statics) {
          LegacyStatics(..statics, input: s)
        })
      #(mk_undefined(), st)
    }
  }
}

pub fn is_handle(v: JsVal, h: Handle) -> Bool {
  case classify(v) {
    KHandle(vh) -> vh == h
    _ -> False
  }
}

fn read_legacy_statics(st: Agent, ctor: Handle) -> Option(LegacyStatics) {
  case rt_store.cell_get(st, ctor) {
    SObject(
      kind: NativeFn(token: RegExpN(RegExpConstructor(legacy:, ..)), ..),
      ..,
    ) -> Some(legacy)
    _ -> None
  }
}

fn write_legacy_statics(
  st: Agent,
  ctor: Handle,
  update: fn(LegacyStatics) -> LegacyStatics,
) -> Agent {
  use state <- update_constructor(st, ctor)
  CtorState(..state, legacy: update(state.legacy))
}

type CtorState {
  CtorState(
    legacy: LegacyStatics,
    proto_props: Option(Dict(PropertyKey, Property)),
    compiled: Dict(String, types.CompiledRegExp),
  )
}

fn ctor_state(st: Agent) -> Option(CtorState) {
  case rt_store.cell_get(st, st.realm.regexp.constructor) {
    SObject(
      kind: NativeFn(
        token: RegExpN(RegExpConstructor(legacy:, proto_props:, compiled:)),
        ..,
      ),
      ..,
    ) -> Some(CtorState(legacy:, proto_props:, compiled:))
    _ -> None
  }
}

fn update_constructor(
  st: Agent,
  ctor: Handle,
  update: fn(CtorState) -> CtorState,
) -> Agent {
  use cell <- rt_store.cell_update(st, ctor)
  case cell {
    SObject(
      kind: NativeFn(
        token: RegExpN(RegExpConstructor(legacy:, proto_props:, compiled:)),
        name:,
        length:,
        constructible:,
      ),
      ..,
    ) -> {
      let CtorState(legacy:, proto_props:, compiled:) =
        update(CtorState(legacy:, proto_props:, compiled:))
      SObject(
        ..cell,
        kind: NativeFn(
          token: RegExpN(RegExpConstructor(legacy:, proto_props:, compiled:)),
          name:,
          length:,
          constructible:,
        ),
      )
    }
    other -> other
  }
}

@external(erlang, "erlang", "=:=")
fn same_props(
  a: Dict(PropertyKey, Property),
  b: Dict(PropertyKey, Property),
) -> Bool

// usually one compare against the last props map seen pristine
fn proto_pristine(st: Agent) -> #(Bool, Agent) {
  case rt_store.cell_get(st, st.realm.regexp.prototype), ctor_state(st) {
    SObject(props:, ..), Some(CtorState(proto_props:, ..)) ->
      case proto_props {
        Some(seen) ->
          case same_props(seen, props) {
            True -> #(True, st)
            False -> verify_pristine(st, props)
          }
        None -> verify_pristine(st, props)
      }
    _, _ -> #(False, st)
  }
}

fn verify_pristine(
  st: Agent,
  props: Dict(PropertyKey, Property),
) -> #(Bool, Agent) {
  let exec = case dict.get(props, Named("exec")) {
    Ok(DataProperty(value:, ..)) -> is_intrinsic_exec(st, value)
    _ -> False
  }
  let pristine =
    exec
    && intrinsic_getter(st, props, "flags", RegExpGetFlags)
    && list.all(all_flags, fn(f) {
      intrinsic_getter(st, props, flag_property(f), RegExpGetFlag(f))
    })
  case pristine {
    False -> #(False, st)
    True -> {
      let st = {
        use state <- update_constructor(st, st.realm.regexp.constructor)
        CtorState(..state, proto_props: Some(props))
      }
      #(True, st)
    }
  }
}

pub fn update_legacy_statics(
  st: Agent,
  s: String,
  whole: #(Int, Int),
  groups: List(#(Int, Int)),
) -> Agent {
  use _previous <- write_legacy_statics(st, st.realm.regexp.constructor)
  LegacyStatics(input: s, subject: s, whole:, groups:)
}

fn legacy_static_value(statics: LegacyStatics, which: LegacyStatic) -> String {
  let LegacyStatics(input:, subject: s, whole: #(start, len), groups:) = statics
  let paren = fn(n) {
    helpers.list_at(groups, n - 1)
    |> option.map(capture_to_legacy_string(s, _))
    |> option.unwrap("")
  }
  case which {
    types.LegacyInput -> input
    types.LegacyLastMatch -> bytes.unsafe_slice(s, start, len)
    types.LegacyLastParen ->
      list.last(groups)
      |> result.map(capture_to_legacy_string(s, _))
      |> result.unwrap("")
    types.LegacyLeftContext -> bytes.unsafe_slice(s, 0, start)
    types.LegacyRightContext -> bytes.drop_start(s, start + len)
    types.LegacyParen1 -> paren(1)
    types.LegacyParen2 -> paren(2)
    types.LegacyParen3 -> paren(3)
    types.LegacyParen4 -> paren(4)
    types.LegacyParen5 -> paren(5)
    types.LegacyParen6 -> paren(6)
    types.LegacyParen7 -> paren(7)
    types.LegacyParen8 -> paren(8)
    types.LegacyParen9 -> paren(9)
  }
}

fn capture_to_legacy_string(s: String, cap: #(Int, Int)) -> String {
  case cap {
    #(start, len) if start >= 0 -> bytes.unsafe_slice(s, start, len)
    _ -> ""
  }
}

fn get_source(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case require_regexp_or_proto(st, this, "source") {
    ReadRegExp(s, _, _) -> #(mk_string(source_string(s)), st)
    ReadRegExpProto -> #(mk_string("(?:)"), st)
  }
}

fn source_string(pattern: String) -> String {
  case pattern {
    "" -> "(?:)"
    p -> escape_pattern(bit_array.from_string(p), "")
  }
}

fn escape_pattern(chars: BitArray, acc: String) -> String {
  case chars {
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
  case classify(this) {
    KHandle(h) ->
      case own_flags(st, h) {
        #(Some(flags), st) -> #(mk_string(flags), st)
        #(None, st) -> build_flags(st, this, all_flags, "")
      }
    _ ->
      rt_val.throw_type_error(
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
    ReadRegExp(_, flags, _) -> #(mk_bool(has_flag(flags, flag_char(flag))), st)
    ReadRegExpProto -> #(mk_undefined(), st)
  }
}

fn to_string(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(_) -> Nil
    _ ->
      rt_val.throw_type_error(
        st,
        "RegExp.prototype.toString called on non-object",
      )
  }
  let #(src_v, st) = get_named(st, this, "source")
  let #(src, st) = rt_val.to_string(st, src_v)
  let #(flags, st) = read_flags(st, this)
  #(mk_string("/" <> src <> "/" <> flags), st)
}

fn pattern_and_flags_from_strings(
  st: Agent,
  pattern_v: JsVal,
  flags_v: JsVal,
) -> #(String, String, Agent) {
  let #(source, st) = case classify(pattern_v) {
    types.KUndef -> #("", st)
    _ -> rt_val.to_string(st, pattern_v)
  }
  let #(flags, st) = case classify(flags_v) {
    types.KUndef -> #("", st)
    _ -> rt_val.to_string(st, flags_v)
  }
  #(source, flags, st)
}

pub fn create_literal(
  st: Agent,
  source: String,
  flags: String,
) -> #(JsVal, Agent) {
  let #(h, st) =
    alloc_regexp_with_proto(st, source, flags, st.realm.regexp.prototype)
  #(types.mk_object(h), st)
}

fn alloc_regexp_with_proto(
  st: Agent,
  source: String,
  flags: String,
  proto: Handle,
) -> #(Handle, Agent) {
  let #(seq, st) = rt_store.next_prop_seq(st)
  let li_prop =
    types.DataProperty(
      value: mk_int(0),
      writable: True,
      enumerable: False,
      configurable: False,
      seq:,
    )
  rt_store.cell_new(
    st,
    plain_object(
      RegExpObj(
        source: case source {
          "" -> "(?:)"
          _ -> source
        },
        flags: canonical_flags(flags),
        last_index: 0,
        compiled: uncompiled(),
      ),
      option.Some(proto),
      common.named_props([#("lastIndex", li_prop)]),
    ),
  )
}

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
      rt_val.throw_syntax_error(st, regex_error.pattern_error_message(err))
  }
}

type RegExpRead {
  ReadRegExp(source: String, flags: String, last_index: Int)
  ReadRegExpProto
}

fn require_regexp_or_proto(st: Agent, v: JsVal, op: String) -> RegExpRead {
  case classify(v) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: RegExpObj(source:, flags:, last_index:, ..), ..) ->
          ReadRegExp(source, flags, last_index)
        _ ->
          case h == st.realm.regexp.prototype {
            True -> ReadRegExpProto
            False -> throw_receiver(st, op)
          }
      }
    _ -> throw_receiver(st, op)
  }
}

fn is_regexp_object(st: Agent, v: JsVal) -> Bool {
  case classify(v) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: RegExpObj(..), ..) -> True
        _ -> False
      }
    _ -> False
  }
}

fn throw_receiver(st: Agent, op: String) -> a {
  rt_val.throw_type_error(
    st,
    "Method RegExp.prototype." <> op <> " called on incompatible receiver",
  )
}

pub type ExecFailure {
  NoMatch
  OffsetOutOfRange
  PatternCompileFailed(reason: String)
}

@external(erlang, "arc_regexp_ffi", "has_flag")
pub fn has_flag(flags: String, flag: String) -> Bool

@external(erlang, "arc_regexp_ffi", "regexp_compile")
fn regexp_compile(pattern: String, flags: String) -> types.CompiledRegExp

@external(erlang, "arc_regexp_ffi", "is_compiled")
fn is_compiled(compiled: types.CompiledRegExp) -> Bool

@external(erlang, "arc_regexp_ffi", "regexp_exec_compiled")
pub fn regexp_exec_compiled(
  compiled: types.CompiledRegExp,
  s: String,
  offset: Int,
  sticky sticky: Bool,
) -> Result(
  #(#(Int, Int), List(#(Int, Int)), Int, List(#(String, Int))),
  ExecFailure,
)

pub fn get_named(st: Agent, o: JsVal, name: String) -> #(JsVal, Agent) {
  helpers.get_named(st, o, name)
}

pub fn set_throw(st: Agent, h: Handle, name: String, v: JsVal) -> Agent {
  helpers.set_named(st, mk_object(h), name, v, strict: True)
}

pub fn require_object(st: Agent, v: JsVal, op: String) -> Handle {
  case classify(v) {
    KHandle(h) -> h
    _ ->
      rt_val.throw_type_error(
        st,
        "RegExp.prototype" <> op <> " called on non-object",
      )
  }
}

pub fn regexp_exec_abstract(
  st: Agent,
  rx: JsVal,
  s: String,
) -> #(JsVal, Agent) {
  regexp_exec_mode(st, rx, s, MatchArray)
}

fn regexp_exec_mode(
  st: Agent,
  rx: JsVal,
  s: String,
  mode: ExecMode,
) -> #(JsVal, Agent) {
  let h = require_object(st, rx, ".exec")
  let #(exec_fn, st) = get_named(st, rx, "exec")
  let receiver = rt_store.cell_get(st, h)
  case receiver, is_intrinsic_exec(st, exec_fn) {
    SObject(kind: RegExpObj(..), ..), True -> builtin_exec_mode(st, h, s, mode)
    _, _ -> {
      case rt_val.is_callable(st, exec_fn) {
        True -> {
          let store = st.store
          let #(result, st) = store.ops.call(st, exec_fn, rx, [mk_string(s)])
          case classify(result) {
            KHandle(_) | KNull -> #(result, st)
            _ ->
              rt_val.throw_type_error(
                st,
                "exec method returned something other than an Object or null",
              )
          }
        }
        False ->
          case receiver {
            SObject(kind: RegExpObj(..), ..) ->
              builtin_exec_mode(st, h, s, mode)
            _ ->
              rt_val.throw_type_error(
                st,
                "Method called on incompatible receiver: not a RegExp",
              )
          }
      }
    }
  }
}

fn is_intrinsic_exec(st: Agent, f: JsVal) -> Bool {
  case classify(f) {
    KHandle(fh) ->
      case rt_store.cell_get(st, fh) {
        SObject(kind: NativeFn(token: RegExpN(RegExpPrototypeExec), ..), ..) ->
          True
        _ -> False
      }
    _ -> False
  }
}

pub type ExecMode {
  MatchArray
  MatchOnly
}

pub type MatchRanges {
  RangesHit(
    whole: #(Int, Int),
    groups: List(#(Int, Int)),
    names: List(#(String, Int)),
  )
  RangesMiss
}

// §22.2.7.2 regexpbuiltinexec up to the result array
pub fn builtin_exec_ranges(
  st: Agent,
  h: Handle,
  s: String,
) -> #(MatchRanges, String, Agent) {
  let #(li_v, st) = get_named(st, mk_object(h), "lastIndex")
  let #(last_index, st) = rt_val.to_length(st, li_v)
  // re-read after the get, a getter may have recompiled
  let #(flags, compiled, st) = regexp_matcher(st, h)
  let global = has_flag(flags, "g")
  let sticky = has_flag(flags, "y")
  let last_index = case global || sticky {
    True -> last_index
    False -> 0
  }
  case regexp_exec_compiled(compiled, s, last_index, sticky) {
    Error(NoMatch) | Error(OffsetOutOfRange) | Error(PatternCompileFailed(_)) -> {
      let st = case global || sticky {
        True -> set_throw(st, h, "lastIndex", mk_int(0))
        False -> st
      }
      #(RangesMiss, flags, st)
    }
    Ok(#(whole, groups, _gc, names)) -> {
      let #(match_start, match_len) = whole
      let st = case global || sticky {
        True -> set_throw(st, h, "lastIndex", mk_int(match_start + match_len))
        False -> st
      }
      // unconditional like v8, gating would leave stale statics
      let st = update_legacy_statics(st, s, whole, groups)
      #(RangesHit(whole:, groups:, names:), flags, st)
    }
  }
}

pub fn builtin_exec_mode(
  st: Agent,
  h: Handle,
  s: String,
  mode: ExecMode,
) -> #(JsVal, Agent) {
  let #(ranges, flags, st) = builtin_exec_ranges(st, h, s)
  case ranges, mode {
    RangesMiss, _ -> #(mk_null(), st)
    RangesHit(..), MatchOnly -> #(mk_bool(True), st)
    RangesHit(whole:, groups:, names:), MatchArray ->
      build_exec_result(st, s, whole, groups, names, has_flag(flags, "d"))
  }
}

pub fn regexp_matcher(
  st: Agent,
  h: Handle,
) -> #(String, types.CompiledRegExp, Agent) {
  case rt_store.cell_get(st, h) {
    SObject(kind: RegExpObj(source:, flags:, last_index:, compiled:), ..) as cell ->
      case is_compiled(compiled) {
        True -> #(flags, compiled, st)
        False -> {
          let #(compiled, st) = compile_cached(st, source, flags)
          let kind = RegExpObj(source:, flags:, last_index:, compiled:)
          let st = rt_store.cell_set(st, h, SObject(..cell, kind:))
          #(flags, compiled, st)
        }
      }
    _ ->
      rt_val.throw_type_error(
        st,
        "RegExp.prototype.exec requires that 'this' be a RegExp",
      )
  }
}

fn compile_cached(
  st: Agent,
  source: String,
  flags: String,
) -> #(types.CompiledRegExp, Agent) {
  let key = flags <> "/" <> source
  let cached = case ctor_state(st) {
    Some(CtorState(compiled:, ..)) -> dict.get(compiled, key)
    None -> Error(Nil)
  }
  case cached {
    Ok(compiled) -> #(compiled, st)
    Error(Nil) -> {
      let fresh = regexp_compile(source, flags)
      let st = {
        use state <- update_constructor(st, st.realm.regexp.constructor)
        let cache = case dict.size(state.compiled) < 256 {
          True -> state.compiled
          False -> dict.new()
        }
        CtorState(..state, compiled: dict.insert(cache, key, fresh))
      }
      #(fresh, st)
    }
  }
}

fn build_exec_result(
  st: Agent,
  s: String,
  whole: #(Int, Int),
  groups: List(#(Int, Int)),
  names: List(#(String, Int)),
  has_indices has_indices: Bool,
) -> #(JsVal, Agent) {
  let #(match_start, match_len) = whole
  let match_values = [
    mk_string(bytes.unsafe_slice(s, match_start, match_len)),
    ..list.map(groups, capture_to_value(s, _))
  ]
  let #(groups_val, st) = groups_object(st, s, groups, names)
  let #(indices_val, st) = case has_indices {
    False -> #(mk_undefined(), st)
    True -> make_indices(st, whole, groups, names)
  }
  let extra = case classify(indices_val) {
    KUndef -> []
    _ -> [#("indices", indices_val)]
  }
  let #(arr_h, st) =
    alloc_array_with_props(st, match_values, [
      #("index", mk_int(match_start)),
      #("input", mk_string(s)),
      #("groups", groups_val),
      ..extra
    ])
  #(mk_object(arr_h), st)
}

pub fn groups_object(
  st: Agent,
  s: String,
  groups: List(#(Int, Int)),
  names: List(#(String, Int)),
) -> #(JsVal, Agent) {
  case names {
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
}

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
              [mk_int(start), mk_int(start + len)],
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
  let #(arr_h, st) =
    alloc_array_with_props(st, pair_values, [#("groups", groups_val)])
  #(mk_object(arr_h), st)
}

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
      let #(prop, st) = rt_store.plain_property(st, v)
      #([#(k, prop), ..ps], st)
    })
  let #(h, st) =
    rt_store.cell_new(
      st,
      plain_object(Ordinary, None, common.named_props(list.reverse(props))),
    )
  #(mk_object(h), st)
}

pub fn capture_to_value(s: String, cap: #(Int, Int)) -> JsVal {
  let #(start, len) = cap
  case start >= 0 {
    True -> mk_string(bytes.unsafe_slice(s, start, len))
    False -> mk_undefined()
  }
}

fn alloc_array_with_props(
  st: Agent,
  values: List(JsVal),
  entries: List(#(String, JsVal)),
) -> #(Handle, Agent) {
  let array_proto = st.realm.array.prototype
  use seq <- rt_store.cell_new_with(st, list.length(entries))
  let props =
    list.index_map(entries, fn(kv, i) {
      #(Named(kv.0), types.plain_property(kv.1, seq + i))
    })
  SObject(
    kind: ArrayObj(list.length(values)),
    proto: Some(array_proto),
    props: dict.from_list(props),
    symbol_props: [],
    elements: elements.from_list(values),
    extensible: True,
  )
}

fn regexp_exec(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: RegExpObj(..), ..) -> {
          let #(s, st) =
            rt_val.to_string(st, helpers.first_arg_or_undefined(args))
          builtin_exec_mode(st, h, s, MatchArray)
        }
        _ -> not_regexp(st, "exec")
      }
    _ -> not_regexp(st, "exec")
  }
}

fn regexp_test(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let _ = require_object(st, this, ".test")
  let #(s, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let #(m, st) = regexp_exec_mode(st, this, s, MatchOnly)
  #(mk_bool(classify(m) != KNull), st)
}

fn prototype_compile(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let realm_proto = st.realm.regexp.prototype
  let h = case classify(this) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
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
      case rt_store.cell_get(st, ph) {
        SObject(kind: RegExpObj(source: p, flags: f, ..), ..) ->
          case classify(flags_v) {
            KUndef -> #(p, f, st)
            _ ->
              rt_val.throw_type_error(
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
    rt_store.cell_update(st, h, fn(cell) {
      case cell {
        SObject(kind: RegExpObj(..), ..) ->
          SObject(
            ..cell,
            kind: RegExpObj(
              source:,
              flags: canonical_flags(flags),
              last_index: 0,
              compiled: uncompiled(),
            ),
          )
        _ -> cell
      }
    })
  let st = set_throw(st, h, "lastIndex", mk_int(0))
  #(this, st)
}

pub fn not_regexp(st: Agent, method: String) -> a {
  rt_val.throw_type_error(
    st,
    "RegExp.prototype." <> method <> " requires that 'this' be a RegExp",
  )
}

fn own_flags(st: Agent, h: Handle) -> #(Option(String), Agent) {
  let proto = st.realm.regexp.prototype
  case rt_store.cell_get(st, h) {
    SObject(kind: RegExpObj(flags:, ..), proto: Some(p), props:, ..)
      if p == proto
    ->
      case dict.size(props) == 1 {
        False -> #(None, st)
        True -> {
          // stored canonical at allocation
          let #(pristine, st) = proto_pristine(st)
          case pristine {
            True -> #(Some(flags), st)
            False -> #(None, st)
          }
        }
      }
    _ -> #(None, st)
  }
}

fn canonical_flags(flags: String) -> String {
  // zero or one flag is already in order
  case string.byte_size(flags) <= 1 {
    True -> flags
    False ->
      list.fold(all_flags, "", fn(acc, f) {
        case has_flag(flags, flag_char(f)) {
          True -> acc <> flag_char(f)
          False -> acc
        }
      })
  }
}

fn intrinsic_getter(
  st: Agent,
  props: dict.Dict(PropertyKey, types.Property),
  name: String,
  expected: RegExpNative,
) -> Bool {
  case dict.get(props, Named(name)) {
    Ok(types.AccessorProperty(get: Some(g), ..)) ->
      case classify(g) {
        KHandle(gh) ->
          case rt_store.cell_get(st, gh) {
            SObject(kind: NativeFn(token: RegExpN(native), ..), ..) ->
              native == expected
            _ -> False
          }
        _ -> False
      }
    _ -> False
  }
}

// get(rx, "flags") short of the accessor call when nothing is observable
pub fn read_flags(st: Agent, rx: JsVal) -> #(String, Agent) {
  let #(pristine, st) = case classify(rx) {
    KHandle(h) -> own_flags(st, h)
    _ -> #(None, st)
  }
  case pristine {
    Some(flags) -> #(flags, st)
    None -> {
      let #(flags_v, st) = get_named(st, rx, "flags")
      rt_val.to_string(st, flags_v)
    }
  }
}

// own exec absent and the proto's exec is the untouched intrinsic
pub fn pristine_exec(st: Agent, h: Handle) -> #(Bool, Agent) {
  let proto = st.realm.regexp.prototype
  case rt_store.cell_get(st, h) {
    SObject(kind: RegExpObj(..), proto: Some(p), props:, ..) if p == proto ->
      case dict.has_key(props, Named("exec")) {
        True -> #(False, st)
        False -> proto_pristine(st)
      }
    _ -> #(False, st)
  }
}

// unanchored search replaces the per-index sticky probe, same matches
pub fn species_constructor(
  st: Agent,
  o: JsVal,
  default_ctor: Handle,
) -> #(JsVal, Agent) {
  let #(c, st) = get_named(st, o, "constructor")
  case classify(c) {
    KUndef -> #(mk_object(default_ctor), st)
    KHandle(_) -> {
      let #(s, st) = helpers.get_symbol(st, c, types.symbol_species)
      case classify(s) {
        KUndef | KNull -> #(mk_object(default_ctor), st)
        KHandle(_) -> #(s, st)
        _ ->
          rt_val.throw_type_error(
            st,
            "constructor[Symbol.species] is not a constructor",
          )
      }
    }
    _ -> rt_val.throw_type_error(st, "object.constructor is not an Object")
  }
}

const all_flags = [
  HasIndicesFlag,
  GlobalFlag,
  IgnoreCaseFlag,
  MultilineFlag,
  DotAllFlag,
  UnicodeFlag,
  UnicodeSetsFlag,
  StickyFlag,
]

fn flag_property(f: RegExpFlag) -> String {
  case f {
    HasIndicesFlag -> "hasIndices"
    GlobalFlag -> "global"
    IgnoreCaseFlag -> "ignoreCase"
    MultilineFlag -> "multiline"
    DotAllFlag -> "dotAll"
    UnicodeFlag -> "unicode"
    UnicodeSetsFlag -> "unicodeSets"
    StickyFlag -> "sticky"
  }
}

fn flag_char(f: RegExpFlag) -> String {
  case f {
    HasIndicesFlag -> "d"
    GlobalFlag -> "g"
    IgnoreCaseFlag -> "i"
    MultilineFlag -> "m"
    DotAllFlag -> "s"
    UnicodeFlag -> "u"
    UnicodeSetsFlag -> "v"
    StickyFlag -> "y"
  }
}

// sentinel until first exec compiles the real matcher
pub fn uncompiled() -> types.CompiledRegExp {
  unsafe.coerce(types.mk_undefined())
}
