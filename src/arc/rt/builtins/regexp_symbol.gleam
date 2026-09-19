// §22.2.6 regexp.prototype [symbol.*] methods and the regexp string iterator

import arc/bytecode/key.{Index, Named}
import arc/internal/bytes
import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/builtins/regexp.{
  type MatchRanges, MatchArray, NoMatch, OffsetOutOfRange, PatternCompileFailed,
  RangesHit, RangesMiss, builtin_exec_mode, builtin_exec_ranges,
  capture_to_value, groups_object, has_flag, is_handle, pristine_exec,
  read_flags, regexp_exec_compiled, require_object, set_last_index,
  species_constructor, update_legacy_statics,
}
import arc/rt/builtins/substitution
import arc/rt/call as rt_call
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, KHandle, KNull, KUndef, Ordinary, RegExpN,
  RegExpStringIteratorNext, SObject, StringKey, classify, mk_bool, mk_int,
  mk_null, mk_object, mk_string, mk_undefined, plain_object,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn symbol_match(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.match]")
  let #(s, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let #(flags, st) = read_flags(st, this)
  let #(pristine, st) = pristine_exec(st, h)
  case has_flag(flags, "g"), pristine {
    False, True -> builtin_exec_mode(st, h, s, MatchArray)
    False, False -> regexp.exec_abstract(st, this, s)
    True, pristine -> {
      let st = set_last_index(st, h, mk_int(0))
      case pristine {
        True -> match_global_pristine(st, h, s)
        False -> collect_global_matches(st, this, h, s, [], 0)
      }
    }
  }
}

// no user code can run mid-loop, so lastIndex and statics land once
fn match_global_pristine(st: Agent, h: Handle, s: String) -> #(JsVal, Agent) {
  let #(hits, st) = global_hits(st, h, s)
  case hits {
    [] -> #(mk_null(), st)
    _ ->
      realm_ops.new_array(
        st,
        list.map(hits, fn(hit) {
          mk_string(bytes.unsafe_slice(s, hit.start, hit.length))
        }),
      )
  }
}

type GlobalHit {
  GlobalHit(
    start: Int,
    length: Int,
    groups: List(#(Int, Int)),
    names: List(#(String, Int)),
  )
}

// §22.2.7.2 iterated with the global flag until the first miss
fn global_hits(st: Agent, h: Handle, s: String) -> #(List(GlobalHit), Agent) {
  let #(flags, compiled, st) = regexp.matcher(st, h)
  let rev =
    scan_hits(compiled, s, sticky: has_flag(flags, "y"), from: 0, acc: [])
  let st = case rev {
    [GlobalHit(start:, length:, groups:, ..), ..] ->
      update_legacy_statics(st, s, #(start, length), groups)
    [] -> st
  }
  let st = set_last_index(st, h, mk_int(0))
  #(list.reverse(rev), st)
}

fn scan_hits(
  compiled: types.CompiledRegExp,
  s: String,
  sticky sticky: Bool,
  from from: Int,
  acc acc: List(GlobalHit),
) -> List(GlobalHit) {
  case regexp_exec_compiled(compiled, s, from, sticky) {
    Error(NoMatch) | Error(OffsetOutOfRange) | Error(PatternCompileFailed(_)) ->
      acc
    Ok(#(#(ms, ml), groups, _gc, names)) -> {
      let next = case ml {
        0 -> bytes.next_char_boundary(s, ms)
        _ -> ms + ml
      }
      let hit = GlobalHit(start: ms, length: ml, groups:, names:)
      scan_hits(compiled, s, sticky:, from: next, acc: [hit, ..acc])
    }
  }
}

fn collect_global_matches(
  st: Agent,
  rx: JsVal,
  h: Handle,
  s: String,
  acc: List(JsVal),
  n: Int,
) -> #(JsVal, Agent) {
  let #(result, st) = regexp.exec_abstract(st, rx, s)
  case classify(result) {
    KNull ->
      case n {
        0 -> #(mk_null(), st)
        _ -> realm_ops.new_array(st, list.reverse(acc))
      }
    _ -> {
      let #(m_v, st) = rt_obj.get_prop(st, result, StringKey(Index(0)))
      let #(match_text, st) = rt_val.to_string(st, m_v)
      let st = advance_if_empty(st, h, s, match_text)
      collect_global_matches(
        st,
        rx,
        h,
        s,
        [mk_string(match_text), ..acc],
        n + 1,
      )
    }
  }
}

fn advance_if_empty(
  st: Agent,
  h: Handle,
  s: String,
  match_text: String,
) -> Agent {
  case match_text {
    "" -> {
      let #(li_v, st) = rt_val.get_named(st, mk_object(h), "lastIndex", None)
      let #(this_index, st) = rt_val.to_length(st, li_v)
      set_last_index(st, h, mk_int(bytes.next_char_boundary(s, this_index)))
    }
    _ -> st
  }
}

pub fn symbol_search(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.search]")
  let #(s, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let #(previous, st) = rt_val.get_named(st, this, "lastIndex", None)
  let st = set_unless_same_value(st, h, previous, mk_int(0))
  let #(pristine, st) = pristine_exec(st, h)
  case pristine {
    True -> {
      let #(ranges, _flags, st) = builtin_exec_ranges(st, h, s)
      let st = restore_last_index(st, h, previous)
      case ranges {
        RangesMiss -> #(mk_int(-1), st)
        RangesHit(whole: #(ms, _), ..) -> #(mk_int(ms), st)
      }
    }
    False -> {
      let #(result, st) = regexp.exec_abstract(st, this, s)
      let st = restore_last_index(st, h, previous)
      case classify(result) {
        KNull -> #(mk_int(-1), st)
        _ -> rt_val.get_named(st, result, "index", None)
      }
    }
  }
}

fn restore_last_index(st: Agent, h: Handle, previous: JsVal) -> Agent {
  let #(current, st) = rt_val.get_named(st, mk_object(h), "lastIndex", None)
  set_unless_same_value(st, h, current, previous)
}

fn set_unless_same_value(
  st: Agent,
  h: Handle,
  current: JsVal,
  target: JsVal,
) -> Agent {
  case rt_val.same_value(current, target) {
    True -> st
    False -> set_last_index(st, h, target)
  }
}

type Replacer {
  FunctionalReplacer(fun: JsVal)
  TemplateReplacer(
    with_named: List(substitution.NamedSegment),
    without_named: List(substitution.PlainSegment),
  )
}

pub fn symbol_replace(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.replace]")
  let #(s, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let length_s = string.byte_size(s)
  let replace_value = helpers.arg_at(args, 1)
  let #(replacer, st) = case rt_val.is_callable(st, replace_value) {
    True -> #(FunctionalReplacer(replace_value), st)
    False -> {
      let #(tpl, st) = rt_val.to_string(st, replace_value)
      #(
        TemplateReplacer(
          substitution.tokenize_named(tpl),
          substitution.tokenize_plain(tpl),
        ),
        st,
      )
    }
  }
  let #(flags, st) = read_flags(st, this)
  let global = has_flag(flags, "g")
  let st = case global {
    True -> set_last_index(st, h, mk_int(0))
    False -> st
  }
  let #(pristine, st) = pristine_exec(st, h)
  case pristine {
    True -> {
      let #(hits, st) = collect_raw_results(st, h, s, global)
      process_raw_results(st, hits, s, replacer, 0, "")
    }
    False -> {
      let #(results, st) = case global {
        True -> collect_replace_results(st, this, h, s, [])
        False -> {
          let #(result, st) = regexp.exec_abstract(st, this, s)
          case classify(result) {
            KNull -> #([], st)
            _ -> #([result], st)
          }
        }
      }
      process_replace_results(st, results, s, length_s, replacer, 0, "")
    }
  }
}

fn collect_raw_results(
  st: Agent,
  h: Handle,
  s: String,
  global global: Bool,
) -> #(List(MatchRanges), Agent) {
  case global {
    True -> {
      let #(hits, st) = global_hits(st, h, s)
      #(
        list.map(hits, fn(hit) {
          RangesHit(
            whole: #(hit.start, hit.length),
            groups: hit.groups,
            names: hit.names,
          )
        }),
        st,
      )
    }
    False -> {
      let #(ranges, _flags, st) = builtin_exec_ranges(st, h, s)
      case ranges {
        RangesMiss -> #([], st)
        RangesHit(..) -> #([ranges], st)
      }
    }
  }
}

fn process_raw_results(
  st: Agent,
  hits: List(MatchRanges),
  s: String,
  replacer: Replacer,
  next_pos: Int,
  acc: String,
) -> #(JsVal, Agent) {
  case hits {
    [] | [RangesMiss, ..] -> #(
      mk_string(acc <> bytes.drop_start(s, next_pos)),
      st,
    )
    [RangesHit(whole: #(ms, ml), groups:, names:), ..rest] -> {
      let matched = bytes.unsafe_slice(s, ms, ml)
      let captures = list.map(groups, capture_to_value(s, _))
      let #(named_captures, st) = groups_object(st, s, groups, names)
      let #(replacement, st) =
        compute_replacement(
          st,
          matched,
          s,
          ms,
          captures,
          list.length(groups),
          named_captures,
          replacer,
        )
      let #(acc, next_pos) =
        splice_replacement(acc, s, next_pos, ms, matched, replacement)
      process_raw_results(st, rest, s, replacer, next_pos, acc)
    }
  }
}

fn splice_replacement(
  acc: String,
  s: String,
  next_pos: Int,
  position: Int,
  matched: String,
  replacement: String,
) -> #(String, Int) {
  case position >= next_pos {
    True -> #(
      acc <> bytes.unsafe_slice(s, next_pos, position - next_pos) <> replacement,
      position + string.byte_size(matched),
    )
    False -> #(acc, next_pos)
  }
}

fn collect_replace_results(
  st: Agent,
  rx: JsVal,
  h: Handle,
  s: String,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  let #(result, st) = regexp.exec_abstract(st, rx, s)
  case classify(result) {
    KNull -> #(list.reverse(acc), st)
    _ -> {
      let #(m_v, st) = rt_obj.get_prop(st, result, StringKey(Index(0)))
      let #(match_text, st) = rt_val.to_string(st, m_v)
      let st = advance_if_empty(st, h, s, match_text)
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
    [] -> #(mk_string(acc <> bytes.drop_start(s, next_pos)), st)
    [result, ..rest] -> {
      let #(len_v, st) = rt_val.get_named(st, result, "length", None)
      let #(result_length, st) = rt_val.to_length(st, len_v)
      let n_captures = int.max(result_length - 1, 0)
      let #(m_v, st) = rt_obj.get_prop(st, result, StringKey(Index(0)))
      let #(matched, st) = rt_val.to_string(st, m_v)
      let #(pos_v, st) = rt_val.get_named(st, result, "index", None)
      let #(pos_raw, st) = rt_val.to_integer_or_infinity(st, pos_v)
      let position = int.clamp(pos_raw, 0, length_s)
      let #(captures, st) =
        collect_coerced_captures(st, result, 1, n_captures, [])
      let #(named_captures, st) = rt_val.get_named(st, result, "groups", None)
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
      let #(acc, next_pos) =
        splice_replacement(acc, s, next_pos, position, matched, replacement)
      process_replace_results(st, rest, s, length_s, replacer, next_pos, acc)
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
      let #(cap, st) = rt_obj.get_prop(st, result, StringKey(Index(n)))
      case classify(cap) {
        KUndef ->
          collect_coerced_captures(st, result, n + 1, n_captures, [
            mk_undefined(),
            ..acc
          ])
        _ -> {
          let #(cap_text, st) = rt_val.to_string(st, cap)
          collect_coerced_captures(st, result, n + 1, n_captures, [
            mk_string(cap_text),
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
          [mk_int(position), mk_string(s)],
        ])
      let call_args = case classify(named_captures) {
        KUndef -> base
        _ -> list.append(base, [named_captures])
      }
      let store = st.store
      let #(result, st) = store.ops.call(st, fun, mk_undefined(), call_args)
      rt_val.to_string(st, result)
    }
    TemplateReplacer(with_named, without_named) -> {
      let ctx =
        substitution.MatchContext(
          matched:,
          before: fn() { bytes.unsafe_slice(s, 0, position) },
          after: fn() {
            bytes.drop_start(s, position + string.byte_size(matched))
          },
          capture: fn(idx) { capture_or_empty(captures, idx) },
          capture_count: n_captures,
        )
      case classify(named_captures) {
        KUndef ->
          finish_replacement(
            st,
            list.reverse(substitution.expand_plain_parts(without_named, ctx)),
          )
        KNull -> rt_val.throw_type_error(st, "Cannot convert null to object")
        _ -> expand_segments(st, with_named, ctx, named_captures, [])
      }
    }
  }
}

fn expand_segments(
  st: Agent,
  segments: List(substitution.NamedSegment),
  ctx: substitution.MatchContext,
  nc: JsVal,
  acc: List(String),
) -> #(String, Agent) {
  case segments {
    [] -> finish_replacement(st, acc)
    [segment, ..rest] ->
      case substitution.expand(segment, ctx) {
        substitution.Text(text) ->
          expand_segments(st, rest, ctx, nc, [text, ..acc])
        substitution.NamedGroup(name) -> {
          let #(cap, st) = rt_val.get_named(st, nc, name, None)
          case classify(cap) {
            KUndef -> expand_segments(st, rest, ctx, nc, ["", ..acc])
            _ -> {
              let #(cap_text, st) = rt_val.to_string(st, cap)
              expand_segments(st, rest, ctx, nc, [cap_text, ..acc])
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
    True -> rt_val.throw_range_error(st, "Invalid string length")
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
            types.KStr(s) -> s
            _ -> ""
          }
        None -> ""
      }
  }
}

pub fn symbol_split(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.split]")
  let #(s, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let realm = st.realm
  let #(c, st) = species_constructor(st, mk_object(h), realm.regexp.constructor)
  let #(flags, st) = read_flags(st, this)
  let new_flags = case has_flag(flags, "y") {
    True -> flags
    False -> flags <> "y"
  }
  let #(sp_h, st) = rt_call.construct(st, c, [this, mk_string(new_flags)], c)
  let splitter = mk_object(sp_h)
  let limit_arg = helpers.arg_at(args, 1)
  let #(lim, st) = case classify(limit_arg) {
    KUndef -> #(4_294_967_295, st)
    _ -> rt_val.to_uint32(st, limit_arg)
  }
  let size = string.byte_size(s)
  case lim, size {
    0, _ -> realm_ops.new_array(st, [])
    _, 0 -> {
      let #(z, st) = regexp.exec_abstract(st, splitter, s)
      case classify(z) {
        KNull -> realm_ops.new_array(st, [mk_string(s)])
        _ -> realm_ops.new_array(st, [])
      }
    }
    _, _ -> {
      let #(pristine, st) = case is_handle(c, realm.regexp.constructor) {
        True -> pristine_exec(st, sp_h)
        False -> #(False, st)
      }
      case pristine {
        True -> {
          let #(_flags, compiled, st) = regexp.matcher(st, sp_h)
          split_pristine(st, compiled, s, size, lim, 0, 0, [], 0)
        }
        False ->
          symbol_split_loop(st, splitter, sp_h, s, size, lim, 0, 0, [], 0)
      }
    }
  }
}

// §22.2.6.4 result when rx is plain and the proto getters are intrinsic

fn split_pristine(
  st: Agent,
  compiled: types.CompiledRegExp,
  s: String,
  size: Int,
  lim: Int,
  p: Int,
  q: Int,
  acc: List(JsVal),
  count: Int,
) -> #(JsVal, Agent) {
  let rest = fn(st) {
    realm_ops.new_array(
      st,
      list.reverse([mk_string(bytes.drop_start(s, p)), ..acc]),
    )
  }
  use <- bool.lazy_guard(q >= size, fn() { rest(st) })
  case regexp_exec_compiled(compiled, s, q, sticky: False) {
    Error(NoMatch) | Error(OffsetOutOfRange) | Error(PatternCompileFailed(_)) ->
      rest(st)
    // the spec never probes at size itself, so a match there is a miss
    Ok(#(#(ms, _), _, _, _)) if ms >= size -> rest(st)
    Ok(#(whole, groups, _gc, _names)) -> {
      let #(ms, ml) = whole
      let e = int.min(ms + ml, size)
      let st = update_legacy_statics(st, s, whole, groups)
      case e == p {
        True ->
          split_pristine(
            st,
            compiled,
            s,
            size,
            lim,
            p,
            bytes.next_char_boundary(s, ms),
            acc,
            count,
          )
        False -> {
          let acc = [mk_string(bytes.unsafe_slice(s, p, ms - p)), ..acc]
          let count = count + 1
          use <- bool.lazy_guard(count == lim, fn() {
            realm_ops.new_array(st, list.reverse(acc))
          })
          let #(acc, count, hit) =
            split_pristine_captures(s, groups, acc, count, lim)
          case hit {
            True -> realm_ops.new_array(st, list.reverse(acc))
            False ->
              split_pristine(st, compiled, s, size, lim, e, e, acc, count)
          }
        }
      }
    }
  }
}

fn split_pristine_captures(
  s: String,
  groups: List(#(Int, Int)),
  acc: List(JsVal),
  count: Int,
  lim: Int,
) -> #(List(JsVal), Int, Bool) {
  case groups {
    [] -> #(acc, count, False)
    [cap, ..groups] -> {
      let acc = [capture_to_value(s, cap), ..acc]
      let count = count + 1
      case count == lim {
        True -> #(acc, count, True)
        False -> split_pristine_captures(s, groups, acc, count, lim)
      }
    }
  }
}

fn symbol_split_loop(
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
      realm_ops.new_array(
        st,
        list.reverse([mk_string(bytes.drop_start(s, p)), ..acc]),
      )
    False -> {
      let st = set_last_index(st, sp_h, mk_int(q))
      let #(z, st) = regexp.exec_abstract(st, splitter, s)
      case classify(z) {
        KNull ->
          symbol_split_loop(
            st,
            splitter,
            sp_h,
            s,
            size,
            lim,
            p,
            bytes.next_char_boundary(s, q),
            acc,
            count,
          )
        _ -> {
          let #(li_v, st) = rt_val.get_named(st, splitter, "lastIndex", None)
          let #(e0, st) = rt_val.to_length(st, li_v)
          let e = int.min(e0, size)
          case e == p {
            True ->
              symbol_split_loop(
                st,
                splitter,
                sp_h,
                s,
                size,
                lim,
                p,
                bytes.next_char_boundary(s, q),
                acc,
                count,
              )
            False -> {
              let acc = [mk_string(bytes.unsafe_slice(s, p, q - p)), ..acc]
              let count = count + 1
              case count == lim {
                True -> realm_ops.new_array(st, list.reverse(acc))
                False -> {
                  let #(len_v, st) = rt_val.get_named(st, z, "length", None)
                  let #(z_len, st) = rt_val.to_length(st, len_v)
                  let n_caps = int.max(z_len - 1, 0)
                  let #(acc, count, hit, st) =
                    split_captures(st, z, 1, n_caps, acc, count, lim)
                  case hit {
                    True -> realm_ops.new_array(st, list.reverse(acc))
                    False ->
                      symbol_split_loop(
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
      let #(cap, st) = rt_obj.get_prop(st, z, StringKey(Index(i)))
      let acc = [cap, ..acc]
      let count = count + 1
      case count == lim {
        True -> #(acc, count, True, st)
        False -> split_captures(st, z, i + 1, n_caps, acc, count, lim)
      }
    }
  }
}

pub fn symbol_match_all(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let h = require_object(st, this, "[Symbol.matchAll]")
  let #(s, st) = rt_val.to_string(st, helpers.first_arg_or_undefined(args))
  let realm = st.realm
  let #(c, st) = species_constructor(st, mk_object(h), realm.regexp.constructor)
  let #(flags, st) = read_flags(st, this)
  let #(m_h, st) = rt_call.construct(st, c, [this, mk_string(flags)], c)
  let #(li_v, st) = rt_val.get_named(st, this, "lastIndex", None)
  let #(last_index, st) = rt_val.to_length(st, li_v)
  let st = set_last_index(st, m_h, mk_int(last_index))
  let global = has_flag(flags, "g")
  create_regexp_string_iterator(st, m_h, s, global)
}

// state lives in own props on an ordinary object
fn create_regexp_string_iterator(
  st: Agent,
  matcher: Handle,
  s: String,
  global global: Bool,
) -> #(JsVal, Agent) {
  let realm = st.realm
  let #(next_h, st) =
    rt_call.native_new(
      st,
      Some(realm.function.prototype),
      RegExpN(RegExpStringIteratorNext),
      "next",
      0,
      constructible: False,
    )
  let #(next_prop, st) = rt_store.builtin_property(st, mk_object(next_h))
  let #(matcher_prop, st) = rt_store.frozen_property(st, mk_object(matcher))
  let #(string_prop, st) = rt_store.frozen_property(st, mk_string(s))
  let #(global_prop, st) = rt_store.frozen_property(st, mk_bool(global))
  let #(done_prop, st) = rt_store.plain_property(st, mk_bool(False))
  let #(iter_h, st) =
    rt_store.cell_new(
      st,
      plain_object(
        Ordinary,
        Some(realm.iterator_proto),
        common.named_props([
          #("next", next_prop),
          #(iter_matcher_key, matcher_prop),
          #(iter_subject_key, string_prop),
          #(iter_global_key, global_prop),
          #(iter_done_key, done_prop),
        ]),
      ),
    )
  #(mk_object(iter_h), st)
}

const iter_matcher_key = "[[IteratingRegExp]]"

const iter_subject_key = "[[IteratedString]]"

const iter_global_key = "[[Global]]"

const iter_done_key = "[[Done]]"

pub fn string_iterator_next(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let h = case classify(this) {
    KHandle(h) -> h
    _ ->
      rt_val.throw_type_error(
        st,
        "next method called on incompatible receiver: not an Object",
      )
  }
  let RegExpIterState(matcher:, subject: s, global:, done:) = case
    read_iter_state(st, h)
  {
    Some(state) -> state
    None ->
      rt_val.throw_type_error(
        st,
        "next method called on incompatible receiver: not a RegExp String Iterator",
      )
  }
  case done {
    True -> iter_result(st, mk_undefined(), done: True)
    False -> {
      let #(match, st) = regexp.exec_abstract(st, mk_object(matcher), s)
      case classify(match) {
        KNull -> {
          let st = mark_iter_done(st, h)
          iter_result(st, mk_undefined(), done: True)
        }
        _ ->
          case global {
            False -> {
              let st = mark_iter_done(st, h)
              iter_result(st, match, done: False)
            }
            True -> {
              let #(m_v, st) = rt_obj.get_prop(st, match, StringKey(Index(0)))
              let #(match_text, st) = rt_val.to_string(st, m_v)
              let st = advance_if_empty(st, matcher, s, match_text)
              iter_result(st, match, done: False)
            }
          }
      }
    }
  }
}

type RegExpIterState {
  RegExpIterState(matcher: Handle, subject: String, global: Bool, done: Bool)
}

fn read_iter_state(st: Agent, h: Handle) -> Option(RegExpIterState) {
  case rt_store.cell_get(st, h) {
    SObject(props:, ..) -> {
      use m <- option.then(case dict.get(props, Named(iter_matcher_key)) {
        Ok(types.DataProperty(value:, ..)) ->
          case classify(value) {
            KHandle(mh) -> Some(mh)
            _ -> None
          }
        _ -> None
      })
      use s <- option.then(case dict.get(props, Named(iter_subject_key)) {
        Ok(types.DataProperty(value:, ..)) ->
          case classify(value) {
            types.KStr(s) -> Some(s)
            _ -> None
          }
        _ -> None
      })
      use g <- option.then(case dict.get(props, Named(iter_global_key)) {
        Ok(types.DataProperty(value:, ..)) -> Some(rt_val.to_boolean(value))
        _ -> None
      })
      use d <- option.map(case dict.get(props, Named(iter_done_key)) {
        Ok(types.DataProperty(value:, ..)) -> Some(rt_val.to_boolean(value))
        _ -> None
      })
      RegExpIterState(matcher: m, subject: s, global: g, done: d)
    }
    _ -> None
  }
}

fn mark_iter_done(st: Agent, h: Handle) -> Agent {
  rt_store.cell_update(st, h, fn(cell) {
    case cell {
      SObject(props:, ..) ->
        case dict.get(props, Named(iter_done_key)) {
          Ok(types.DataProperty(seq:, ..)) ->
            SObject(
              ..cell,
              props: dict.insert(
                props,
                Named(iter_done_key),
                types.plain_property(mk_bool(True), seq),
              ),
            )
          _ -> cell
        }
      _ -> cell
    }
  })
}

fn iter_result(st: Agent, v: JsVal, done done: Bool) -> #(JsVal, Agent) {
  let #(h, st) = rt_async.alloc_iter_result(st, v, done)
  #(mk_object(h), st)
}
