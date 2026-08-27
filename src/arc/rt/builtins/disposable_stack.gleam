import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/error as b_error
import arc/rt/builtins/helpers.{first_arg_or_undefined, two_args_or_undefined}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type DisposableStackNative, type DisposableState,
  type DisposeResource, type Handle, type JsVal, type SymbolId,
  AsyncDisposableStackConstructor, AsyncDisposableStackDisposedGetter,
  AsyncDisposableStackPrototypeAdopt, AsyncDisposableStackPrototypeDefer,
  AsyncDisposableStackPrototypeDisposeAsync, AsyncDisposableStackPrototypeMove,
  AsyncDisposableStackPrototypeUse, AsyncDisposeContinue, AsyncFallbackDispose,
  DisposableStackConstructor, DisposableStackDisposedGetter, DisposableStackN,
  DisposableStackObj, DisposableStackPrototypeAdopt,
  DisposableStackPrototypeDefer, DisposableStackPrototypeDispose,
  DisposableStackPrototypeMove, DisposableStackPrototypeUse, DisposeCallback,
  Disposed, KHandle, KNull, KUndef, MethodDispose, NoElements, NullDispose,
  Pending, SDisposeCapability, SObject, SymbolKey, TypeErr, classify, mk_bool,
  mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(BuiltinPair, Agent) {
  init_stack_type(
    st,
    object_proto,
    function_proto,
    name: "DisposableStack",
    ctor_fn: fn(proto) { DisposableStackConstructor(proto:) },
    use_fn: DisposableStackPrototypeUse,
    adopt_fn: DisposableStackPrototypeAdopt,
    defer_fn: DisposableStackPrototypeDefer,
    move_fn: fn(proto) { DisposableStackPrototypeMove(proto:) },
    dispose_name: "dispose",
    dispose_fn: DisposableStackPrototypeDispose,
    dispose_symbol: types.symbol_dispose,
    disposed_fn: DisposableStackDisposedGetter,
  )
}

pub fn init_async(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(BuiltinPair, Agent) {
  init_stack_type(
    st,
    object_proto,
    function_proto,
    name: "AsyncDisposableStack",
    ctor_fn: fn(proto) { AsyncDisposableStackConstructor(proto:) },
    use_fn: AsyncDisposableStackPrototypeUse,
    adopt_fn: AsyncDisposableStackPrototypeAdopt,
    defer_fn: AsyncDisposableStackPrototypeDefer,
    move_fn: fn(proto) { AsyncDisposableStackPrototypeMove(proto:) },
    dispose_name: "disposeAsync",
    dispose_fn: AsyncDisposableStackPrototypeDisposeAsync,
    dispose_symbol: types.symbol_async_dispose,
    disposed_fn: AsyncDisposableStackDisposedGetter,
  )
}

fn init_stack_type(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
  name name: String,
  ctor_fn ctor_fn: fn(Handle) -> DisposableStackNative,
  use_fn use_fn: DisposableStackNative,
  adopt_fn adopt_fn: DisposableStackNative,
  defer_fn defer_fn: DisposableStackNative,
  move_fn move_fn: fn(Handle) -> DisposableStackNative,
  dispose_name dispose_name: String,
  dispose_fn dispose_fn: DisposableStackNative,
  dispose_symbol dispose_symbol: SymbolId,
  disposed_fn disposed_fn: DisposableStackNative,
) -> #(BuiltinPair, Agent) {
  let #(proto_h, st) = common.alloc_proto(st, Some(object_proto), dict.new())

  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("use", DisposableStackN(use_fn), 1),
      #("adopt", DisposableStackN(adopt_fn), 2),
      #("defer", DisposableStackN(defer_fn), 1),
      #("move", DisposableStackN(move_fn(proto_h)), 0),
    ])
  // symbol alias must be the same function object
  let #(dispose_h, st) =
    common.alloc_rooted_native_fn(
      st,
      function_proto,
      DisposableStackN(dispose_fn),
      dispose_name,
      0,
    )
  let #(dispose_prop, st) = common.builtin_property(st, mk_object(dispose_h))
  let #(getters, st) =
    common.alloc_getters(st, function_proto, [
      #("disposed", DisposableStackN(disposed_fn)),
    ])
  let proto_props =
    list.flatten([getters, [#(dispose_name, dispose_prop)], methods])

  let #(pair, st) =
    common.init_type_on(
      st,
      proto_h,
      function_proto,
      proto_props,
      fn(proto) { DisposableStackN(ctor_fn(proto)) },
      name,
      0,
      [],
      True,
    )
  let st = common.add_to_string_tag(st, proto_h, name)
  let #(dispose_alias, st) = common.restamp(st, dispose_prop)
  let st =
    common.add_symbol_property(st, proto_h, dispose_symbol, dispose_alias)
  #(pair, st)
}

pub fn dispatch(
  st: Agent,
  native: DisposableStackNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    DisposableStackConstructor(..) ->
      rt_val.t_throw_type_error(
        st,
        "Constructor DisposableStack requires 'new'",
      )
    DisposableStackPrototypeDispose -> dispose(st, this)
    DisposableStackPrototypeUse -> use_resource(st, this, args)
    DisposableStackPrototypeAdopt -> adopt(st, this, args, async: False)
    DisposableStackPrototypeDefer -> defer(st, this, args, async: False)
    DisposableStackPrototypeMove(proto:) -> move(st, this, proto, async: False)
    DisposableStackDisposedGetter -> disposed_getter(st, this, async: False)
    AsyncDisposableStackConstructor(..) ->
      rt_val.t_throw_type_error(
        st,
        "Constructor AsyncDisposableStack requires 'new'",
      )
    AsyncDisposableStackPrototypeDisposeAsync -> dispose_async(st, this)
    AsyncDisposableStackPrototypeUse -> use_resource_async(st, this, args)
    AsyncDisposableStackPrototypeAdopt -> adopt(st, this, args, async: True)
    AsyncDisposableStackPrototypeDefer -> defer(st, this, args, async: True)
    AsyncDisposableStackPrototypeMove(proto:) ->
      move(st, this, proto, async: True)
    AsyncDisposableStackDisposedGetter -> disposed_getter(st, this, async: True)
    AsyncDisposeContinue(remaining:, pending:, resolve:, reject:, is_reject:) ->
      async_dispose_continue(
        st,
        args,
        remaining,
        pending,
        resolve,
        reject,
        is_reject,
      )
  }
}

pub fn dispatch_construct(
  st: Agent,
  native: DisposableStackNative,
  _args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case native {
    DisposableStackConstructor(proto:) ->
      construct(st, proto, new_target, async: False)
    AsyncDisposableStackConstructor(proto:) ->
      construct(st, proto, new_target, async: True)
    _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

fn construct(
  st: Agent,
  proto: Handle,
  new_target: JsVal,
  async async: Bool,
) -> #(Handle, Agent) {
  let #(proto_h, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) { proto })
  let #(capability, st) = new_capability(st)
  alloc_stack(st, proto_h, async:, disposable_state: Pending(capability:))
}

fn new_capability(st: Agent) -> #(Handle, Agent) {
  rt_store.t_cell_new(st, SDisposeCapability(resources: []))
}

fn read_capability(st: Agent, cap: Handle) -> List(DisposeResource) {
  let assert SDisposeCapability(resources:) = rt_store.t_cell_get(st, cap)
    as "disposable_stack: capability handle is not an SDisposeCapability cell"
  resources
}

// newest first, so dispose walks in reverse order
fn push_resource(st: Agent, cap: Handle, resource: DisposeResource) -> Agent {
  let resources = read_capability(st, cap)
  rt_store.t_cell_set(
    st,
    cap,
    SDisposeCapability(resources: [resource, ..resources]),
  )
}

fn alloc_stack(
  st: Agent,
  proto: Handle,
  async async: Bool,
  disposable_state disposable_state: DisposableState,
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind: DisposableStackObj(async:, state: disposable_state),
      proto: Some(proto),
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

fn read_stack(
  st: Agent,
  this: JsVal,
  async: Bool,
) -> Option(#(Handle, DisposableState)) {
  case classify(this) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: DisposableStackObj(async: a, state: disposable_state), ..)
          if a == async
        -> Some(#(h, disposable_state))
        _ -> None
      }
    _ -> None
  }
}

fn stack_type_name(async: Bool) -> String {
  case async {
    True -> "AsyncDisposableStack"
    False -> "DisposableStack"
  }
}

fn require_stack(
  st: Agent,
  this: JsVal,
  async: Bool,
  method: String,
  cont: fn(Handle, DisposableState) -> #(a, Agent),
) -> #(a, Agent) {
  case read_stack(st, this, async) {
    Some(#(h, disposable_state)) -> cont(h, disposable_state)
    None ->
      rt_val.t_throw_type_error(
        st,
        "Method "
          <> stack_type_name(async)
          <> ".prototype."
          <> method
          <> " called on incompatible receiver",
      )
  }
}

fn mark_disposed(st: Agent, h: Handle) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert SObject(kind: DisposableStackObj(async:, ..), ..) = slot
      as "disposable_stack: mark_disposed on a non-stack cell"
    SObject(..slot, kind: DisposableStackObj(async:, state: Disposed))
  })
}

fn disposed_getter(
  st: Agent,
  this: JsVal,
  async async: Bool,
) -> #(JsVal, Agent) {
  use _h, disposable_state <- require_stack(st, this, async, "disposed")
  case disposable_state {
    Disposed -> #(mk_bool(True), st)
    Pending(_) -> #(mk_bool(False), st)
  }
}

fn dispose(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use h, disposable_state <- require_stack(st, this, False, "dispose")
  case disposable_state {
    Disposed -> #(mk_undefined(), st)
    Pending(capability:) -> {
      // mark disposed before running disposers
      let resources = read_capability(st, capability)
      let st = mark_disposed(st, h)
      case dispose_resources(st, resources, NormalCompletion(mk_undefined())) {
        #(NormalCompletion(v), st) -> #(v, st)
        #(ThrowCompletion(thrown), st) -> rt_store.t_throw(st, thrown)
      }
    }
  }
}

// §3.1.3 disposeresources, sync
fn dispose_resources(
  st: Agent,
  resources: List(DisposeResource),
  completion: rt_call.Completion,
) -> #(rt_call.Completion, Agent) {
  case resources {
    [] -> #(completion, st)
    [resource, ..rest] -> {
      let #(result, st) = case resource {
        MethodDispose(value: v, method:) ->
          case classify(method) {
            KUndef -> #(NormalCompletion(mk_undefined()), st)
            _ -> rt_call.t_call(st, method, v, [])
          }
        DisposeCallback(callback:, args:) ->
          rt_call.t_call(st, callback, mk_undefined(), args)
        AsyncFallbackDispose(..) | NullDispose ->
          panic as "sync DisposableStack holds async-only resource variant — engine invariant"
      }
      case result {
        NormalCompletion(_) -> dispose_resources(st, rest, completion)
        ThrowCompletion(thrown) ->
          case completion {
            NormalCompletion(_) ->
              dispose_resources(st, rest, ThrowCompletion(thrown))
            ThrowCompletion(prev) -> {
              let #(err, st) = b_error.make_suppressed_error(st, thrown, prev)
              dispose_resources(st, rest, ThrowCompletion(err))
            }
          }
      }
    }
  }
}

fn use_resource(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use _h, disposable_state <- require_stack(st, this, False, "use")
  use capability <- try_pending(st, disposable_state, async: False)
  let val = first_arg_or_undefined(args)
  case classify(val) {
    KUndef | KNull -> #(val, st)
    KHandle(_) -> {
      let #(dispose_method, st) = get_dispose_method(st, val, is_async: False)
      let resource = case dispose_method {
        DirectDispose(method) | SyncFallbackDispose(method) ->
          MethodDispose(value: val, method: mk_object(method))
      }
      #(val, push_resource(st, capability, resource))
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "DisposableStack.prototype.use called with a non-object, non-nullish value",
      )
  }
}

fn use_resource_async(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use _h, disposable_state <- require_stack(st, this, True, "use")
  use capability <- try_pending(st, disposable_state, async: True)
  let val = first_arg_or_undefined(args)
  case classify(val) {
    KUndef | KNull -> #(val, push_resource(st, capability, NullDispose))
    KHandle(_) -> {
      let #(dispose_method, st) = get_dispose_method(st, val, is_async: True)
      let resource = case dispose_method {
        DirectDispose(method) ->
          MethodDispose(value: val, method: mk_object(method))
        SyncFallbackDispose(method) ->
          AsyncFallbackDispose(value: val, method: mk_object(method))
      }
      #(val, push_resource(st, capability, resource))
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "AsyncDisposableStack.prototype.use called with a non-object, non-nullish value",
      )
  }
}

fn get_method(
  st: Agent,
  val: JsVal,
  symbol: SymbolId,
) -> #(Option(Handle), Agent) {
  let #(method, st) = rt_obj.t_get_prop(st, val, SymbolKey(symbol))
  case classify(method) {
    KUndef | KNull -> #(None, st)
    KHandle(h) ->
      case rt_call.is_callable(st, method) {
        True -> #(Some(h), st)
        False ->
          rt_val.t_throw_type_error(
            st,
            "Dispose method property is not callable",
          )
      }
    _ ->
      rt_val.t_throw_type_error(st, "Dispose method property is not callable")
  }
}

pub type DisposeMethod {
  DirectDispose(method: Handle)
  // async fallback to @@dispose: result discarded, awaits undefined
  SyncFallbackDispose(method: Handle)
}

// §3.1.2 getdisposemethod, shared with using declarations
pub fn get_dispose_method(
  st: Agent,
  val: JsVal,
  is_async is_async: Bool,
) -> #(DisposeMethod, Agent) {
  case is_async {
    False -> {
      let #(method, st) = get_method(st, val, types.symbol_dispose)
      case method {
        None -> no_dispose_method(st, is_async)
        Some(m) -> #(DirectDispose(m), st)
      }
    }
    True -> {
      let #(method, st) = get_method(st, val, types.symbol_async_dispose)
      case method {
        None -> {
          let #(sync_method, st) = get_method(st, val, types.symbol_dispose)
          case sync_method {
            None -> no_dispose_method(st, is_async)
            Some(m) -> #(SyncFallbackDispose(m), st)
          }
        }
        Some(m) -> #(DirectDispose(m), st)
      }
    }
  }
}

fn no_dispose_method(st: Agent, is_async: Bool) -> a {
  case is_async {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Object does not have a [Symbol.asyncDispose] or [Symbol.dispose] method",
      )
    False ->
      rt_val.t_throw_type_error(
        st,
        "Object does not have a [Symbol.dispose] method",
      )
  }
}

fn adopt(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  async async: Bool,
) -> #(JsVal, Agent) {
  use _h, disposable_state <- require_stack(st, this, async, "adopt")
  use capability <- try_pending(st, disposable_state, async:)
  let #(val, on_dispose) = two_args_or_undefined(args)
  case rt_call.is_callable(st, on_dispose) {
    False -> rt_val.t_throw_type_error(st, "onDispose is not a function")
    True -> {
      let resource = DisposeCallback(callback: on_dispose, args: [val])
      #(val, push_resource(st, capability, resource))
    }
  }
}

fn defer(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  async async: Bool,
) -> #(JsVal, Agent) {
  use _h, disposable_state <- require_stack(st, this, async, "defer")
  use capability <- try_pending(st, disposable_state, async:)
  let on_dispose = first_arg_or_undefined(args)
  case rt_call.is_callable(st, on_dispose) {
    False -> rt_val.t_throw_type_error(st, "onDispose is not a function")
    True -> {
      let resource = DisposeCallback(callback: on_dispose, args: [])
      #(mk_undefined(), push_resource(st, capability, resource))
    }
  }
}

fn move(
  st: Agent,
  this: JsVal,
  proto: Handle,
  async async: Bool,
) -> #(JsVal, Agent) {
  use h, disposable_state <- require_stack(st, this, async, "move")
  use capability <- try_pending(st, disposable_state, async:)
  let #(new_h, st) =
    alloc_stack(st, proto, async:, disposable_state: Pending(capability:))
  let st = mark_disposed(st, h)
  #(mk_object(new_h), st)
}

fn try_pending(
  st: Agent,
  disposable_state: DisposableState,
  async async: Bool,
  cont cont: fn(Handle) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case disposable_state {
    Disposed ->
      rt_val.t_throw_reference_error(
        st,
        stack_type_name(async) <> " already disposed",
      )
    Pending(capability:) -> cont(capability)
  }
}

fn dispose_async(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let #(#(promise_h, resolve_h, reject_h), st) =
    rt_async.t_new_promise_capability(st)
  let promise = mk_object(promise_h)
  let resolve = mk_object(resolve_h)
  let reject = mk_object(reject_h)
  case read_stack(st, this, True) {
    None -> {
      let #(err, st) =
        st.store.ops.new_error(
          st,
          TypeErr,
          "Method "
            <> stack_type_name(True)
            <> ".prototype.disposeAsync called on incompatible receiver",
        )
      let st = settle_capability(st, reject, err)
      #(promise, st)
    }
    Some(#(_h, Disposed)) -> {
      let st = settle_capability(st, resolve, mk_undefined())
      #(promise, st)
    }
    Some(#(h, Pending(capability:))) -> {
      let resources = read_capability(st, capability)
      let st = mark_disposed(st, h)
      let st =
        async_dispose_loop(
          st,
          resources,
          pending: None,
          needs_await: False,
          has_awaited: False,
          resolve:,
          reject:,
        )
      #(promise, st)
    }
  }
}

// §3.1.3 disposeresources, async, resumed across microtasks
fn async_dispose_loop(
  st: Agent,
  resources: List(DisposeResource),
  pending pending: Option(JsVal),
  needs_await needs_await: Bool,
  has_awaited has_awaited: Bool,
  resolve resolve: JsVal,
  reject reject: JsVal,
) -> Agent {
  case resources {
    [] ->
      case needs_await && !has_awaited {
        True -> attach_await(st, mk_undefined(), [], pending, resolve, reject)
        False ->
          case pending {
            None -> settle_capability(st, resolve, mk_undefined())
            Some(err) -> settle_capability(st, reject, err)
          }
      }
    [resource, ..rest] -> {
      let call_then_await = fn(callee, this, args) {
        case rt_call.t_call(st, callee, this, args) {
          #(NormalCompletion(result), st) ->
            attach_await(st, result, rest, pending, resolve, reject)
          #(ThrowCompletion(thrown), st) -> {
            let #(pending, st) = fold_error(st, pending, thrown)
            async_dispose_loop(
              st,
              rest,
              pending:,
              needs_await:,
              has_awaited:,
              resolve:,
              reject:,
            )
          }
        }
      }
      case resource {
        MethodDispose(value: v, method:) -> call_then_await(method, v, [])
        // awaited so an async ondispose rejection propagates
        DisposeCallback(callback:, args:) ->
          call_then_await(callback, mk_undefined(), args)
        // sync throw becomes a rejected promise, folded after a hop
        AsyncFallbackDispose(value: v, method:) ->
          case rt_call.t_call(st, method, v, []) {
            #(NormalCompletion(_discarded), st) ->
              attach_await(st, mk_undefined(), rest, pending, resolve, reject)
            #(ThrowCompletion(thrown), st) ->
              attach_await_rejected(st, thrown, rest, pending, resolve, reject)
          }
        NullDispose ->
          async_dispose_loop(
            st,
            rest,
            pending:,
            needs_await: True,
            has_awaited:,
            resolve:,
            reject:,
          )
      }
    }
  }
}

fn fold_error(
  st: Agent,
  pending: Option(JsVal),
  thrown: JsVal,
) -> #(Option(JsVal), Agent) {
  case pending {
    None -> #(Some(thrown), st)
    Some(prev) -> {
      let #(err, st) = b_error.make_suppressed_error(st, thrown, prev)
      #(Some(err), st)
    }
  }
}

fn attach_await(
  st: Agent,
  awaited: JsVal,
  rest: List(DisposeResource),
  pending: Option(JsVal),
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let #(promise_h, st) = rt_async.promise_resolve_static(st, awaited)
  attach_reactions(st, promise_h, rest, pending, resolve, reject)
}

fn attach_await_rejected(
  st: Agent,
  thrown: JsVal,
  rest: List(DisposeResource),
  pending: Option(JsVal),
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let #(promise_h, st) = rt_async.t_new_promise(st)
  let st = rt_async.t_promise_reject(st, promise_h, thrown)
  attach_reactions(st, promise_h, rest, pending, resolve, reject)
}

fn attach_reactions(
  st: Agent,
  promise_h: Handle,
  rest: List(DisposeResource),
  pending: Option(JsVal),
  resolve: JsVal,
  reject: JsVal,
) -> Agent {
  let #(on_fulfill, st) =
    alloc_continue(st, rest, pending, resolve, reject, is_reject: False)
  let #(on_reject, st) =
    alloc_continue(st, rest, pending, resolve, reject, is_reject: True)
  let #(_child, st) =
    rt_async.t_promise_then(
      st,
      promise_h,
      mk_object(on_fulfill),
      mk_object(on_reject),
    )
  st
}

fn alloc_continue(
  st: Agent,
  rest: List(DisposeResource),
  pending: Option(JsVal),
  resolve: JsVal,
  reject: JsVal,
  is_reject is_reject: Bool,
) -> #(Handle, Agent) {
  rt_call.t_native_new(
    st,
    Some(st.realm.function.prototype),
    DisposableStackN(AsyncDisposeContinue(
      remaining: rest,
      pending:,
      resolve:,
      reject:,
      is_reject:,
    )),
    "",
    1,
    False,
  )
}

fn async_dispose_continue(
  st: Agent,
  args: List(JsVal),
  remaining: List(DisposeResource),
  pending: Option(JsVal),
  resolve: JsVal,
  reject: JsVal,
  is_reject: Bool,
) -> #(JsVal, Agent) {
  let #(pending, st) = case is_reject {
    True -> fold_error(st, pending, first_arg_or_undefined(args))
    False -> #(pending, st)
  }
  let st =
    async_dispose_loop(
      st,
      remaining,
      pending:,
      needs_await: False,
      has_awaited: True,
      resolve:,
      reject:,
    )
  #(mk_undefined(), st)
}

// intrinsic resolving functions never throw
fn settle_capability(st: Agent, fun: JsVal, arg: JsVal) -> Agent {
  let #(_val, st) = rt_call.t_call_checked(st, fun, mk_undefined(), [arg])
  st
}
