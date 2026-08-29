import arc/bytecode/key.{max_array_length}
import arc/rt/async as rt_async
import arc/rt/builtins/helpers
import arc/rt/builtins/iter_protocol
import arc/rt/call as rt_call
import arc/rt/elements
import arc/rt/name_keys as nk
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FromAsyncCtx, type FromAsyncLikeCtx, type JsVal,
  type NativeToken, ArrayFromAsyncCloseReject, ArrayFromAsyncLikeOnMapped,
  ArrayFromAsyncLikeOnValue, ArrayFromAsyncOnMapped, ArrayFromAsyncOnNext,
  ArrayFromAsyncRejectWith, ArrayN, ArrayObj, FromAsyncCtx, FromAsyncLikeCtx,
  JInt, KHandle, KNull, KUndef, SObject, StringKey, SymbolKey, classify,
  mk_number, mk_object, mk_undefined, symbol_async_iterator, symbol_iterator,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}

// wire-compatible with rt_call.Completion
type ProtOut(a) {
  NormalCompletion(a)
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(st: Agent, body: fn(Agent) -> #(a, Agent)) -> #(ProtOut(a), Agent)

fn attempt(
  st: Agent,
  body: fn(Agent) -> Agent,
) -> Result(Agent, #(JsVal, Agent)) {
  case protected(st, fn(st) { #(Nil, body(st)) }) {
    #(NormalCompletion(Nil), st) -> Ok(st)
    #(ThrowCompletion(thrown), st) -> Error(#(thrown, st))
  }
}

fn attempt_value(
  st: Agent,
  body: fn(Agent) -> #(a, Agent),
) -> Result(#(a, Agent), #(JsVal, Agent)) {
  case protected(st, body) {
    #(NormalCompletion(v), st) -> Ok(#(v, st))
    #(ThrowCompletion(thrown), st) -> Error(#(thrown, st))
  }
}

fn from_int(n: Int) -> JsVal {
  mk_number(JInt(n))
}

fn settle(st: Agent, target: JsVal, arg: JsVal) -> Agent {
  let #(_, st) = rt_call.t_call_checked(st, target, mk_undefined(), [arg])
  st
}

fn alloc_closure(st: Agent, tag: NativeToken) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_call.t_native_new(
      st,
      Some(st.realm.function.prototype),
      tag,
      "",
      1,
      False,
    )
  #(mk_object(h), st)
}

fn type_name(st: Agent, v: JsVal) -> String {
  case classify(v) {
    KNull -> "null"
    _ -> {
      let #(ty, _) = rt_val.t_type_of(st, v)
      ty
    }
  }
}

fn from_async_handler(
  st: Agent,
  args: List(JsVal),
  reject: JsVal,
  body: fn(Agent, JsVal) -> Agent,
) -> #(JsVal, Agent) {
  let arg = helpers.first_arg_or_undefined(args)
  let st = case attempt(st, fn(st) { body(st, arg) }) {
    Ok(st) -> st
    Error(#(thrown, st)) -> settle(st, reject, thrown)
  }
  #(mk_undefined(), st)
}

// §23.1.2.1, sync abrupt completions reject the promise
pub fn from_async(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(#(promise_h, resolve_h, reject_h), st) =
    rt_async.t_new_promise_capability(st)
  let resolve = mk_object(resolve_h)
  let reject = mk_object(reject_h)
  let st = case
    attempt(st, fn(st) { from_async_closure(st, this, args, resolve, reject) })
  {
    Ok(st) -> st
    Error(#(thrown, st)) -> rt_async.t_promise_reject(st, promise_h, thrown)
  }
  #(mk_object(promise_h), st)
}

fn from_async_closure(
  st: Agent,
  c: JsVal,
  args: List(JsVal),
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let #(items, map_fn, this_arg) = helpers.three_args_or_undefined(args)
  let map_fn = case classify(map_fn) {
    KUndef -> None
    _ ->
      case rt_call.is_callable(st, map_fn) {
        True -> Some(map_fn)
        False ->
          rt_val.t_throw_type_error(
            st,
            type_name(st, map_fn) <> " is not a function",
          )
      }
  }
  let st = case classify(items) {
    KUndef | KNull ->
      rt_val.t_throw_type_error(
        st,
        "Cannot convert " <> type_name(st, items) <> " to object",
      )
    _ -> st
  }
  let #(async_method, st) =
    from_async_get_method(st, items, SymbolKey(symbol_async_iterator))
  case classify(async_method) {
    KUndef -> {
      let #(sync_method, st) =
        from_async_get_method(st, items, SymbolKey(symbol_iterator))
      case classify(sync_method) {
        KUndef ->
          from_async_array_like(st, c, items, map_fn, this_arg, resolve, reject)
        _ -> {
          let #(sync_rec, st) =
            iter_protocol.get_iterator_from_method(st, items, sync_method)
          let #(rec, st) = iter_protocol.create_async_from_sync(st, sync_rec)
          from_async_iterate(
            st,
            c,
            rec.iterator,
            rec.next_method,
            map_fn,
            this_arg,
            resolve,
            reject,
          )
        }
      }
    }
    _ -> {
      let #(iter_val, st) = rt_call.t_call_checked(st, async_method, items, [])
      let st = case classify(iter_val) {
        KHandle(_) -> st
        _ -> rt_val.t_throw_type_error(st, "The iterator is not an object")
      }
      let #(next_method, st) =
        rt_obj.t_get_prop(st, iter_val, StringKey(nk.next))
      from_async_iterate(
        st,
        c,
        iter_val,
        next_method,
        map_fn,
        this_arg,
        resolve,
        reject,
      )
    }
  }
}

fn from_async_get_method(
  st: Agent,
  v: JsVal,
  key: rt_types.ObjectKey,
) -> #(JsVal, Agent) {
  let #(method, st) = rt_obj.t_get_prop(st, v, key)
  case classify(method) {
    KUndef | KNull -> #(mk_undefined(), st)
    _ ->
      case rt_call.is_callable(st, method) {
        True -> #(method, st)
        False ->
          rt_val.t_throw_type_error(
            st,
            type_name(st, method) <> " is not a function",
          )
      }
  }
}

fn from_async_iterate(
  st: Agent,
  c: JsVal,
  iter: JsVal,
  next_method: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let #(target, st) = case rt_call.is_constructor(st, c) {
    True -> {
      let #(h, st) = rt_call.t_construct(st, c, [], c)
      #(mk_object(h), st)
    }
    False -> from_async_array_create(st, 0)
  }
  from_async_request_next(
    st,
    FromAsyncCtx(
      iter:,
      next_method:,
      map_fn:,
      this_arg:,
      target:,
      k: 0,
      resolve:,
      reject:,
    ),
  )
}

fn from_async_request_next(st: Agent, ctx: FromAsyncCtx) -> Agent {
  let #(next_result, st) =
    rt_call.t_call_checked(st, ctx.next_method, ctx.iter, [])
  from_async_await(
    st,
    next_result,
    ArrayN(ArrayFromAsyncOnNext(ctx)),
    ctx.reject,
  )
}

fn from_async_await(
  st: Agent,
  v: JsVal,
  on_fulfilled: NativeToken,
  on_rejected: JsVal,
) -> Agent {
  let #(on_f, st) = alloc_closure(st, on_fulfilled)
  let #(awaited_h, st) = rt_async.promise_resolve_static(st, v)
  let #(_child, st) = rt_async.t_promise_then(st, awaited_h, on_f, on_rejected)
  st
}

pub fn on_next(
  st: Agent,
  ctx: FromAsyncCtx,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, next_result <- from_async_handler(st, args, ctx.reject)
  from_async_next_steps(st, ctx, next_result)
}

fn from_async_next_steps(
  st: Agent,
  ctx: FromAsyncCtx,
  next_result: JsVal,
) -> Agent {
  let st = case classify(next_result) {
    KHandle(_) -> st
    _ -> rt_val.t_throw_type_error(st, "Iterator result is not an object")
  }
  let #(done_val, st) = rt_obj.t_get_prop(st, next_result, StringKey(nk.done))
  case rt_val.to_boolean(done_val) {
    True -> {
      let st = from_async_set_length(st, ctx.target, ctx.k)
      settle(st, ctx.resolve, ctx.target)
    }
    False -> {
      let #(next_value, st) =
        rt_obj.t_get_prop(st, next_result, StringKey(nk.value))
      case ctx.map_fn {
        None -> from_async_define_and_continue(st, ctx, next_value)
        Some(map_fn) ->
          case
            attempt_value(st, fn(st) {
              rt_call.t_call_checked(st, map_fn, ctx.this_arg, [
                next_value,
                from_int(ctx.k),
              ])
            })
          {
            Ok(#(mapped, st)) -> {
              let #(on_r, st) =
                alloc_closure(
                  st,
                  ArrayN(ArrayFromAsyncCloseReject(
                    iter: ctx.iter,
                    reject: ctx.reject,
                  )),
                )
              from_async_await(
                st,
                mapped,
                ArrayN(ArrayFromAsyncOnMapped(ctx)),
                on_r,
              )
            }
            Error(#(thrown, st)) ->
              from_async_close_then_reject(st, ctx.iter, thrown, ctx.reject)
          }
      }
    }
  }
}

pub fn on_mapped(
  st: Agent,
  ctx: FromAsyncCtx,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, mapped <- from_async_handler(st, args, ctx.reject)
  from_async_define_and_continue(st, ctx, mapped)
}

fn from_async_define_and_continue(
  st: Agent,
  ctx: FromAsyncCtx,
  v: JsVal,
) -> Agent {
  case attempt(st, fn(st) { from_async_define_own(st, ctx.target, ctx.k, v) }) {
    Error(#(thrown, st)) ->
      from_async_close_then_reject(st, ctx.iter, thrown, ctx.reject)
    Ok(st) -> from_async_request_next(st, FromAsyncCtx(..ctx, k: ctx.k + 1))
  }
}

pub fn close_reject(
  st: Agent,
  iter: JsVal,
  reject: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let err = helpers.first_arg_or_undefined(args)
  #(mk_undefined(), from_async_close_then_reject(st, iter, err, reject))
}

pub fn reject_with(st: Agent, error: JsVal, reject: JsVal) -> #(JsVal, Agent) {
  #(mk_undefined(), settle(st, reject, error))
}

// §7.4.13 under throw completion: original error always wins
fn from_async_close_then_reject(
  st: Agent,
  iter: JsVal,
  err: JsVal,
  reject: JsVal,
) -> Agent {
  case call_return_method(st, iter) {
    #(None, st) -> settle(st, reject, err)
    #(Some(inner), st) -> {
      let #(rw, st) =
        alloc_closure(st, ArrayN(ArrayFromAsyncRejectWith(error: err, reject:)))
      let #(inner_h, st) = rt_async.promise_resolve_static(st, inner)
      let #(_child, st) = rt_async.t_promise_then(st, inner_h, rw, rw)
      st
    }
  }
}

fn call_return_method(st: Agent, iter: JsVal) -> #(Option(JsVal), Agent) {
  case classify(iter) {
    KHandle(_) -> {
      let got =
        attempt_value(st, fn(st) {
          rt_obj.t_get_prop(st, iter, StringKey(nk.return))
        })
      case got {
        Error(#(_inner_thrown, st)) -> #(None, st)
        Ok(#(ret_fn, st)) -> call_if_callable(st, ret_fn, iter)
      }
    }
    _ -> #(None, st)
  }
}

fn call_if_callable(
  st: Agent,
  ret_fn: JsVal,
  iter: JsVal,
) -> #(Option(JsVal), Agent) {
  case rt_call.is_callable(st, ret_fn) {
    False -> #(None, st)
    True ->
      case rt_call.t_call(st, ret_fn, iter, []) {
        #(rt_call.ThrowCompletion(_inner_thrown), st) -> #(None, st)
        #(rt_call.NormalCompletion(inner), st) -> #(Some(inner), st)
      }
  }
}

fn from_async_array_like(
  st: Agent,
  c: JsVal,
  items: JsVal,
  map_fn: Option(JsVal),
  this_arg: JsVal,
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let #(len_val, st) = rt_obj.t_get_prop(st, items, StringKey(nk.length))
  let #(len, st) = rt_val.t_to_length(st, len_val)
  let #(target, st) = case rt_call.is_constructor(st, c) {
    True -> {
      let #(h, st) = rt_call.t_construct(st, c, [from_int(len)], c)
      #(mk_object(h), st)
    }
    False -> from_async_array_create(st, len)
  }
  from_async_like_step(
    st,
    FromAsyncLikeCtx(
      items:,
      map_fn:,
      this_arg:,
      target:,
      k: 0,
      len:,
      resolve:,
      reject:,
    ),
  )
}

fn from_async_like_step(st: Agent, ctx: FromAsyncLikeCtx) -> Agent {
  case ctx.k < ctx.len {
    False -> {
      let st = from_async_set_length(st, ctx.target, ctx.len)
      settle(st, ctx.resolve, ctx.target)
    }
    True -> {
      let #(k_val, st) = rt_obj.t_get_index(st, ctx.items, ctx.k)
      from_async_await(
        st,
        k_val,
        ArrayN(ArrayFromAsyncLikeOnValue(ctx)),
        ctx.reject,
      )
    }
  }
}

pub fn like_on_value(
  st: Agent,
  ctx: FromAsyncLikeCtx,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, v <- from_async_handler(st, args, ctx.reject)
  from_async_like_value_steps(st, ctx, v)
}

fn from_async_like_value_steps(
  st: Agent,
  ctx: FromAsyncLikeCtx,
  v: JsVal,
) -> Agent {
  case ctx.map_fn {
    None -> from_async_like_define_and_continue(st, ctx, v)
    Some(map_fn) -> {
      let #(mapped, st) =
        rt_call.t_call_checked(st, map_fn, ctx.this_arg, [v, from_int(ctx.k)])
      from_async_await(
        st,
        mapped,
        ArrayN(ArrayFromAsyncLikeOnMapped(ctx)),
        ctx.reject,
      )
    }
  }
}

pub fn like_on_mapped(
  st: Agent,
  ctx: FromAsyncLikeCtx,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st, mapped <- from_async_handler(st, args, ctx.reject)
  from_async_like_define_and_continue(st, ctx, mapped)
}

fn from_async_like_define_and_continue(
  st: Agent,
  ctx: FromAsyncLikeCtx,
  v: JsVal,
) -> Agent {
  let st = from_async_define_own(st, ctx.target, ctx.k, v)
  from_async_like_step(st, FromAsyncLikeCtx(..ctx, k: ctx.k + 1))
}

fn from_async_array_create(st: Agent, len: Int) -> #(JsVal, Agent) {
  case len > max_array_length {
    True -> rt_val.t_throw_range_error(st, "Invalid array length")
    False -> {
      let #(h, st) =
        rt_store.t_cell_new(
          st,
          SObject(
            kind: ArrayObj(len),
            proto: Some(st.realm.array.prototype),
            props: dict.new(),
            symbol_props: [],
            elements: elements.new(),
            extensible: True,
          ),
        )
      #(mk_object(h), st)
    }
  }
}

fn from_async_define_own(st: Agent, target: JsVal, k: Int, v: JsVal) -> Agent {
  let ref = case classify(target) {
    KHandle(r) -> r
    _ -> rt_val.t_throw_type_error(st, "Cannot define property on a primitive")
  }
  let #(key, st) = rt_store.t_key_of_int(st, k)
  let #(ok, st) =
    rt_obj.t_define_own_data(st, ref, StringKey(key), v, True, True, True)
  case ok {
    True -> st
    False ->
      rt_val.t_throw_type_error(
        st,
        "Cannot define property " <> int.to_string(k) <> " on object",
      )
  }
}

fn from_async_set_length(st: Agent, target: JsVal, n: Int) -> Agent {
  case classify(target) {
    KHandle(_) -> {
      let #(ok, st) =
        rt_obj.t_set_prop(st, target, StringKey(nk.length), from_int(n))
      case ok {
        True -> st
        False ->
          rt_val.t_throw_type_error(
            st,
            "Cannot set property length, it is read-only",
          )
      }
    }
    _ -> st
  }
}
