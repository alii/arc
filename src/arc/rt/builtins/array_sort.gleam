import arc/rt/builtins/array.{
  type ElementFn, type HoleMode, SkipHoles, VisitHoles, alloc_array,
  delete_trailing, dense_snapshot, generic_set_index, get_index_if_present,
  hole_is_inherited, not_a_function, require_array, with_plain_elements,
  within_budget,
}
import arc/rt/builtins/helpers
import arc/rt/call as rt_call
import arc/rt/elements
import arc/rt/types.{
  type Agent, type Handle, type JsElements, type JsVal, JFloat, JInt, JNan,
  JNegInf, JPosInf, KUndef, classify, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn sort(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use comparefn, st <- with_comparefn(st, args)
  use this, h, length, st <- require_array(st, this)
  use <- within_budget(st, length)
  case comparefn {
    None -> sort_default(st, h, length, this)
    Some(cmp) -> sort_with_comparefn(st, h, length, cmp, this)
  }
}

fn with_comparefn(
  st: Agent,
  args: List(JsVal),
  cont: fn(Option(JsVal), Agent) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let comparefn = helpers.first_arg_or_undefined(args)
  case classify(comparefn) {
    KUndef -> cont(None, st)
    _ -> {
      use comparefn <- helpers.require_callable(st, comparefn, fn() {
        not_a_function(st, comparefn)
      })
      cont(Some(comparefn), st)
    }
  }
}

fn sort_default(
  st: Agent,
  h: Handle,
  length: Int,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(#(defined, undefs), st) =
    collect_sort_elements(st, this, length, SkipHoles)
  let #(pairs, st) = stringify_elements(st, defined, [])
  let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
  let sorted_values = list.map(sorted, fn(pair) { pair.1 })
  let all_values =
    list.append(sorted_values, list.repeat(mk_undefined(), undefs))
  #(this, write_sort_result(st, h, all_values, length, 0))
}

fn sort_with_comparefn(
  st: Agent,
  h: Handle,
  length: Int,
  comparefn: JsVal,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(#(defined, undefs), st) =
    collect_sort_elements(st, this, length, SkipHoles)
  let #(sorted, st) = merge_sort(st, defined, comparefn)
  let all_values = list.append(sorted, list.repeat(mk_undefined(), undefs))
  #(this, write_sort_result(st, h, all_values, length, 0))
}

fn collect_sort_elements(
  st: Agent,
  this: JsVal,
  length: Int,
  hole_mode: HoleMode,
) -> #(#(List(JsVal), Int), Agent) {
  case dense_snapshot(st, this) {
    Some(#(els, proto)) ->
      collect_sort_elements_snapshot(
        st,
        this,
        els,
        proto,
        length,
        0,
        [],
        0,
        hole_mode,
      )
    None -> collect_sort_elements_generic(st, this, length, 0, [], 0, hole_mode)
  }
}

fn collect_sort_elements_snapshot(
  st: Agent,
  this: JsVal,
  els: JsElements,
  proto: Option(Handle),
  length: Int,
  idx: Int,
  acc: List(JsVal),
  undefs: Int,
  hole_mode: HoleMode,
) -> #(#(List(JsVal), Int), Agent) {
  case idx >= length {
    True -> #(#(list.reverse(acc), undefs), st)
    False ->
      case elements.get_option(els, idx) {
        Some(v) ->
          case classify(v) {
            KUndef ->
              collect_sort_elements_snapshot(
                st,
                this,
                els,
                proto,
                length,
                idx + 1,
                acc,
                undefs + 1,
                hole_mode,
              )
            _ ->
              collect_sort_elements_snapshot(
                st,
                this,
                els,
                proto,
                length,
                idx + 1,
                [v, ..acc],
                undefs,
                hole_mode,
              )
          }
        None -> {
          let #(inherited, st) = hole_is_inherited(st, proto, idx)
          case inherited {
            False ->
              collect_sort_elements_snapshot(
                st,
                this,
                els,
                proto,
                length,
                idx + 1,
                acc,
                case hole_mode {
                  VisitHoles -> undefs + 1
                  SkipHoles -> undefs
                },
                hole_mode,
              )
            True ->
              collect_sort_elements_generic(
                st,
                this,
                length,
                idx,
                acc,
                undefs,
                hole_mode,
              )
          }
        }
      }
  }
}

fn collect_sort_elements_generic(
  st: Agent,
  this: JsVal,
  length: Int,
  idx: Int,
  acc: List(JsVal),
  undefs: Int,
  hole_mode: HoleMode,
) -> #(#(List(JsVal), Int), Agent) {
  case idx >= length {
    True -> #(#(list.reverse(acc), undefs), st)
    False -> {
      let #(maybe_val, st) = get_index_if_present(st, this, idx)
      case maybe_val {
        None ->
          collect_sort_elements_generic(
            st,
            this,
            length,
            idx + 1,
            acc,
            case hole_mode {
              VisitHoles -> undefs + 1
              SkipHoles -> undefs
            },
            hole_mode,
          )
        Some(val) ->
          case classify(val) {
            KUndef ->
              collect_sort_elements_generic(
                st,
                this,
                length,
                idx + 1,
                acc,
                undefs + 1,
                hole_mode,
              )
            _ ->
              collect_sort_elements_generic(
                st,
                this,
                length,
                idx + 1,
                [val, ..acc],
                undefs,
                hole_mode,
              )
          }
      }
    }
  }
}

fn stringify_elements(
  st: Agent,
  values: List(JsVal),
  acc: List(#(String, JsVal)),
) -> #(List(#(String, JsVal)), Agent) {
  case values {
    [] -> #(list.reverse(acc), st)
    [val, ..rest] -> {
      let #(s, st) = rt_val.to_string(st, val)
      stringify_elements(st, rest, [#(s, val), ..acc])
    }
  }
}

@external(erlang, "lists", "reverse")
fn reverse_onto(items: List(a), tail: List(a)) -> List(a)

fn merge_sort(
  st: Agent,
  items: List(JsVal),
  comparefn: JsVal,
) -> #(List(JsVal), Agent) {
  case items {
    [] | [_] -> #(items, st)
    _ ->
      merge_all(
        st,
        list.map(items, fn(x) { [x] }),
        rt_call.prepare_call(st, comparefn, mk_undefined()),
      )
  }
}

fn merge_all(
  st: Agent,
  runs: List(List(JsVal)),
  comparefn: ElementFn,
) -> #(List(JsVal), Agent) {
  case runs {
    [] -> #([], st)
    [done] -> #(done, st)
    _ -> {
      let #(next, st) = merge_pairs(st, runs, comparefn, [])
      merge_all(st, next, comparefn)
    }
  }
}

fn merge_pairs(
  st: Agent,
  runs: List(List(JsVal)),
  comparefn: ElementFn,
  acc: List(List(JsVal)),
) -> #(List(List(JsVal)), Agent) {
  case runs {
    [] -> #(list.reverse(acc), st)
    [a] -> #(list.reverse([a, ..acc]), st)
    [a, b, ..rest] -> {
      let #(ab, st) = merge_two(st, a, b, comparefn, [])
      merge_pairs(st, rest, comparefn, [ab, ..acc])
    }
  }
}

fn merge_two(
  st: Agent,
  left: List(JsVal),
  right: List(JsVal),
  comparefn: ElementFn,
  acc: List(JsVal),
) -> #(List(JsVal), Agent) {
  case left, right {
    [], _ -> #(reverse_onto(acc, right), st)
    _, [] -> #(reverse_onto(acc, left), st)
    [l, ..ls], [r, ..rs] -> {
      let #(res, st) = comparefn(st, [l, r])
      let #(num, st) = rt_val.to_number(st, res)
      let cmp = case num {
        JInt(n) -> int.to_float(n)
        JFloat(f) -> f
        JPosInf -> 1.0
        JNegInf -> -1.0
        JNan -> 0.0
      }
      case cmp <=. 0.0 {
        True -> merge_two(st, ls, right, comparefn, [l, ..acc])
        False -> merge_two(st, left, rs, comparefn, [r, ..acc])
      }
    }
  }
}

fn write_sort_result(
  st: Agent,
  h: Handle,
  values: List(JsVal),
  length: Int,
  idx: Int,
) -> Agent {
  let dense = case idx == 0 {
    True -> {
      use _els, len <- with_plain_elements(st, h, length, 0, length)
      #(elements.from_list(values), len, Nil)
    }
    False -> None
  }
  case dense {
    Some(#(Nil, st)) -> st
    None ->
      case values {
        [val, ..rest] -> {
          let st = generic_set_index(st, h, idx, val)
          write_sort_result(st, h, rest, length, idx + 1)
        }
        [] -> delete_trailing(st, h, idx, length)
      }
  }
}

pub fn to_sorted(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use comparefn, st <- with_comparefn(st, args)
  use this, _h, length, st <- require_array(st, this)
  use <- within_budget(st, length)
  case comparefn {
    None -> to_sorted_with(st, length, this, sort_values_default)
    Some(cmp) ->
      to_sorted_with(st, length, this, fn(st, defined) {
        merge_sort(st, defined, cmp)
      })
  }
}

fn to_sorted_with(
  st: Agent,
  length: Int,
  this: JsVal,
  sort: fn(Agent, List(JsVal)) -> #(List(JsVal), Agent),
) -> #(JsVal, Agent) {
  let array_proto = st.realm.array.prototype
  let #(#(defined, undefs), st) =
    collect_sort_elements(st, this, length, VisitHoles)
  let #(sorted, st) = sort(st, defined)
  let all_values = list.append(sorted, list.repeat(mk_undefined(), undefs))
  alloc_array(st, length, elements.from_list(all_values), array_proto)
}

fn sort_values_default(
  st: Agent,
  defined: List(JsVal),
) -> #(List(JsVal), Agent) {
  let #(pairs, st) = stringify_elements(st, defined, [])
  let sorted = list.sort(pairs, fn(a, b) { string.compare(a.0, b.0) })
  #(list.map(sorted, fn(pair) { pair.1 }), st)
}
