//// §27.1.6 %AsyncFromSyncIteratorPrototype% methods

import arc/bytecode/key.{Named}
import arc/rt/async as rt_async
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/iter_protocol
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type NativeToken, AsyncFromSyncClose,
  AsyncFromSyncIterator, AsyncFromSyncUnwrap, IteratorN, KHandle, SObject,
  StringKey, classify, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/option.{Some}

type AsyncFromSyncForward {
  ForwardNext
  ForwardReturn
  ForwardThrow
}

pub fn next(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  async_from_sync(st, this, args, ForwardNext)
}

pub fn return(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  async_from_sync(st, this, args, ForwardReturn)
}

pub fn throw(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  async_from_sync(st, this, args, ForwardThrow)
}

pub fn unwrap(st: Agent, args: List(JsVal), done: Bool) -> #(JsVal, Agent) {
  let v = first_arg_or_undefined(args)
  let #(h, st) = rt_async.alloc_iter_result(st, v, done)
  #(mk_object(h), st)
}

pub fn close(
  st: Agent,
  args: List(JsVal),
  sync_iter: Handle,
) -> #(JsVal, Agent) {
  let err = first_arg_or_undefined(args)
  iter_protocol.close_throw(st, mk_object(sync_iter), err)
}

fn async_from_sync(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  kind: AsyncFromSyncForward,
) -> #(JsVal, Agent) {
  let #(#(promise_h, resolve_h, reject_h), st) =
    rt_async.new_promise_capability(st)
  let cap_resolve = mk_object(resolve_h)
  let cap_reject = mk_object(reject_h)
  let #(outcome, st) =
    rt_call.try_run(st, fn(st) {
      forward_to_sync_iterator(st, this, args, kind, cap_resolve, cap_reject)
    })
  let st = case outcome {
    NormalCompletion(_) -> st
    ThrowCompletion(e) -> rt_async.promise_reject(st, promise_h, e)
  }
  #(mk_object(promise_h), st)
}

fn forward_to_sync_iterator(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  kind: AsyncFromSyncForward,
  cap_resolve: JsVal,
  cap_reject: JsVal,
) -> #(JsVal, Agent) {
  let sync =
    iter_protocol.sync_iterator_record(st, require_async_from_sync(st, this))
  let sync_iter = sync.iterator
  let sync_rec = case classify(sync_iter) {
    KHandle(h) -> h
    _ -> rt_val.throw_type_error(st, "not an Async-from-Sync Iterator")
  }
  let #(method, st) = case kind {
    ForwardNext -> #(sync.next_method, st)
    ForwardReturn -> rt_obj.get_prop(st, sync_iter, StringKey(Named("return")))
    ForwardThrow -> rt_obj.get_prop(st, sync_iter, StringKey(Named("throw")))
  }
  case kind, rt_val.is_callable(st, method) {
    ForwardReturn, False -> {
      let arg = first_arg_or_undefined(args)
      let #(ir_h, st) = rt_async.alloc_iter_result(st, arg, done: True)
      let #(_, st) =
        rt_call.call(st, cap_resolve, mk_undefined(), [
          mk_object(ir_h),
        ])
      #(mk_undefined(), st)
    }
    ForwardThrow, False -> {
      let st = iter_protocol.iterator_close_normal(st, sync_iter)
      rt_val.throw_type_error(
        st,
        "The iterator does not provide a 'throw' method.",
      )
    }
    _, _ -> {
      let #(result_val, st) = rt_call.call(st, method, sync_iter, args)
      case classify(result_val) {
        KHandle(result_h) -> {
          let close_on_rejection = case kind {
            ForwardReturn -> False
            ForwardNext | ForwardThrow -> True
          }
          afs_continuation(
            st,
            result_h,
            sync_rec,
            close_on_rejection,
            cap_resolve,
            cap_reject,
          )
        }
        _ -> rt_val.throw_type_error(st, "Iterator result is not an object")
      }
    }
  }
}

// §27.1.4.4 asyncfromsynciteratorcontinuation
fn afs_continuation(
  st: Agent,
  result_h: Handle,
  sync_rec: Handle,
  close_on_rejection close_on_rejection: Bool,
  cap_resolve cap_resolve: JsVal,
  cap_reject cap_reject: JsVal,
) -> #(JsVal, Agent) {
  let result = mk_object(result_h)
  let #(done_v, st) = rt_obj.get_prop(st, result, StringKey(Named("done")))
  let done = rt_val.to_boolean(done_v)
  let #(inner, st) = rt_obj.get_prop(st, result, StringKey(Named("value")))
  let #(on_fulfilled, st) =
    alloc_closure(st, IteratorN(AsyncFromSyncUnwrap(done:)))
  let #(on_rejected, st) = case done || !close_on_rejection {
    True -> #(mk_undefined(), st)
    False ->
      alloc_closure(st, IteratorN(AsyncFromSyncClose(sync_iter: sync_rec)))
  }
  let #(inner_p, st) = rt_async.promise_resolve_static(st, inner)
  let st =
    rt_async.perform_then(
      st,
      inner_p,
      on_fulfilled,
      on_rejected,
      cap_resolve,
      cap_reject,
    )
  #(mk_undefined(), st)
}

fn alloc_closure(st: Agent, token: NativeToken) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_call.native_new(
      st,
      Some(st.realm.function.prototype),
      token,
      "",
      1,
      constructible: False,
    )
  #(mk_object(h), st)
}

fn require_async_from_sync(st: Agent, this: JsVal) -> Handle {
  case classify(this) {
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(kind: AsyncFromSyncIterator(sync_rec:), ..) -> sync_rec
        _ -> rt_val.throw_type_error(st, "not an Async-from-Sync Iterator")
      }
    _ -> rt_val.throw_type_error(st, "not an Async-from-Sync Iterator")
  }
}
