import arc/rt/abstract_ops as rt_abstract_ops
import arc/rt/builtins/array.{
  alloc_array, generic_set_length, lazy_guard_spread, not_a_function,
  within_budget, write_species_element,
}
import arc/rt/builtins/helpers
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/elements
import arc/rt/lang as rt_lang
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, KHandle, KNull, KUndef, SymbolKey,
  classify, mk_int, mk_object, mk_undefined, symbol_iterator,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{type Option, None, Some}

type FromTarget {
  FreshArray(acc: List(JsVal))
  Constructed(target: Handle)
}

fn from_target(
  st: Agent,
  ctor: JsVal,
  ctor_args: List(JsVal),
) -> #(FromTarget, Agent) {
  case classify(ctor) {
    KHandle(h) if h != st.realm.array.constructor ->
      case rt_call.is_constructor(st, ctor) {
        True -> {
          let #(a, st) = rt_call.construct(st, ctor, ctor_args, ctor)
          #(Constructed(a), st)
        }
        False -> #(FreshArray([]), st)
      }
    _ -> #(FreshArray([]), st)
  }
}

fn from_put(
  st: Agent,
  t: FromTarget,
  idx: Int,
  v: JsVal,
) -> #(FromTarget, Agent) {
  case t {
    FreshArray(acc) -> #(FreshArray([v, ..acc]), st)
    Constructed(target) -> #(t, write_species_element(st, target, idx, v))
  }
}

fn from_finish(st: Agent, t: FromTarget, len: Int) -> #(JsVal, Agent) {
  case t {
    FreshArray(acc) -> {
      let array_proto = st.realm.array.prototype
      alloc_array(st, len, elements.from_list(list.reverse(acc)), array_proto)
    }
    Constructed(target) -> {
      let st = generic_set_length(st, target, len)
      #(mk_object(target), st)
    }
  }
}

pub fn from(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(items_val, map_fn, this_arg) = helpers.three_args_or_undefined(args)
  case classify(map_fn) {
    KUndef -> array_from_array_like(st, this, items_val, None, this_arg)
    _ -> {
      use mf <- helpers.require_callable(st, map_fn, fn() {
        not_a_function(st, map_fn)
      })
      array_from_array_like(st, this, items_val, Some(mf), this_arg)
    }
  }
}

fn array_from_array_like(
  st: Agent,
  ctor: JsVal,
  items: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
) -> #(JsVal, Agent) {
  case classify(items) {
    KNull | KUndef -> {
      rt_val.throw_type_error(
        st,
        "Cannot create array from " <> rt_val.type_of(st, items),
      )
    }
    _ -> {
      // a plain array under the intrinsic constructor is a copy
      let plain = case classify(ctor), map_fn {
        KHandle(h), None if h == st.realm.array.constructor ->
          rt_lang.array_spread(st, items)
        _, _ -> rt_lang.SpreadMiss
      }
      use <- lazy_guard_spread(plain, fn(values) {
        realm_ops.new_array(st, values)
      })
      let #(iter_method, st) =
        rt_obj.get_prop(st, items, SymbolKey(symbol_iterator))
      case classify(iter_method) {
        KUndef | KNull -> {
          let #(length, st) = rt_abstract_ops.length_of_array_like(st, items)
          use <- within_budget(st, length)
          let #(target, st) = from_target(st, ctor, [mk_int(length)])
          array_from_array_like_loop(
            st,
            items,
            0,
            length,
            map_fn,
            this_arg,
            target,
          )
        }
        _ -> {
          use m <- helpers.require_callable(st, iter_method, fn() {
            not_a_function(st, iter_method)
          })
          let #(target, st) = from_target(st, ctor, [])
          array_from_iterator(st, items, m, map_fn, this_arg, target)
        }
      }
    }
  }
}

fn array_from_iterator(
  st: Agent,
  items: JsVal,
  iter_method: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  target: FromTarget,
) -> #(JsVal, Agent) {
  let #(rec, st) =
    iter_protocol.get_iterator_from_method(st, items, iter_method)
  array_from_iterator_loop(st, rec, map_fn, this_arg, 0, target)
}

fn array_from_iterator_loop(
  st: Agent,
  rec: types.IteratorRecord,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  k: Int,
  target: FromTarget,
) -> #(JsVal, Agent) {
  let #(step, st) = iter_protocol.iterator_step_value(st, rec)
  case step {
    None -> from_finish(st, target, k)
    Some(item) -> {
      let #(mapped, st) = case map_fn {
        Some(mf) -> {
          use mapped, st <- iter_protocol.or_close(st, rec.iterator, fn(st) {
            rt_call.call(st, mf, this_arg, [item, mk_int(k)])
          })
          #(mapped, st)
        }
        None -> #(item, st)
      }
      let #(target, st) = case target {
        FreshArray(_) -> from_put(st, target, k, mapped)
        Constructed(t) -> {
          use _undef, st <- iter_protocol.or_close(st, rec.iterator, fn(st) {
            #(mk_undefined(), write_species_element(st, t, k, mapped))
          })
          #(target, st)
        }
      }
      array_from_iterator_loop(st, rec, map_fn, this_arg, k + 1, target)
    }
  }
}

fn array_from_array_like_loop(
  st: Agent,
  items: JsVal,
  idx: Int,
  length: Int,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  target: FromTarget,
) -> #(JsVal, Agent) {
  case idx >= length {
    True -> from_finish(st, target, length)
    False -> {
      let #(elem, st) = rt_abstract_ops.get_index(st, items, idx)
      let #(mapped, st) = case map_fn {
        None -> #(elem, st)
        Some(mf) -> rt_call.call(st, mf, this_arg, [elem, mk_int(idx)])
      }
      let #(target, st) = from_put(st, target, idx, mapped)
      array_from_array_like_loop(
        st,
        items,
        idx + 1,
        length,
        map_fn,
        this_arg,
        target,
      )
    }
  }
}

pub fn of(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  let len = list.length(args)
  let #(target, st) = from_target(st, this, [mk_int(len)])
  let #(target, st) =
    list.index_fold(args, #(target, st), fn(acc, item, k) {
      let #(target, st) = acc
      from_put(st, target, k, item)
    })
  from_finish(st, target, len)
}
