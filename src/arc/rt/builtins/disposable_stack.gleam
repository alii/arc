//// DisposableStack / AsyncDisposableStack Objects — Explicit Resource
//// Management proposal §12.3 / §12.4
//// (https://tc39.es/proposal-explicit-resource-management/). Port of
//// `arc/vm/builtins/disposable_stack.gleam`.
////
//// Both stacks hold a [[DisposableResourceStack]] of DisposableResource
//// records and a disposable state (pending | disposed). Resources are stored
//// NEWEST-FIRST in the `DisposableStackObj` kind, so dispose() walks the list
//// head-first — the spec's "reverse list order". The `async` field is the
//// brand: sync methods require async=False, async methods async=True.

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
  Disposed, KHandle, KNull, KUndef, MethodDispose, Named, NoElements,
  NullDispose, Pending, SObject, StringKey, SymbolKey, TypeErr, classify,
  mk_bool, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

/// Set up DisposableStack.prototype and the DisposableStack constructor.
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

/// Set up AsyncDisposableStack.prototype and its constructor.
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

/// Shared init scaffold for both stack types: prototype with
/// use/adopt/defer/move, a dispose method aliased under its well-known
/// symbol (the SAME function object — test262 asserts identity), a
/// `disposed` getter, @@toStringTag, and the constructor.
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
  // Reserve the prototype first: move() embeds it in its native-fn token
  // (the moved-to stack is always created from the intrinsic prototype,
  // never from new.target).
  let #(proto_h, st) = common.alloc_proto(st, Some(object_proto), dict.new())

  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("use", DisposableStackN(use_fn), 1),
      #("adopt", DisposableStackN(adopt_fn), 2),
      #("defer", DisposableStackN(defer_fn), 1),
      #("move", DisposableStackN(move_fn(proto_h)), 0),
    ])
  // dispose/disposeAsync allocated separately: the prototype's well-known
  // symbol property must be the SAME function object as the named method.
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

  // §12.3.2.1: `prototype` { writable: false, enumerable: false,
  // configurable: false }; length 0. Built on the reserved proto ref.
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

/// Per-module dispatch for DisposableStack/AsyncDisposableStack natives
/// reached through [[Call]]. The constructors require `new`
/// (§12.3.1.1 / §12.4.1.1 step 1: NewTarget undefined → TypeError).
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

/// [[Construct]] dispatch: only the two constructors are constructible.
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

/// §12.3.1.1 DisposableStack ( ) / §12.4.1.1 AsyncDisposableStack ( )
///
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. Let disposableStack be ? OrdinaryCreateFromConstructor(NewTarget,
///      "%(Async)DisposableStack.prototype%", « [[(Async)DisposableState]],
///      [[DisposeCapability]] »).
///   3. Set the state to pending.
///   4. Set [[DisposeCapability]] to NewDisposeCapability().
///   5. Return disposableStack.
fn construct(
  st: Agent,
  proto: Handle,
  new_target: JsVal,
  async async: Bool,
) -> #(Handle, Agent) {
  // Step 2: GetPrototypeFromConstructor(NewTarget, intrinsic).
  let #(proto_h, st) = proto_from_new_target(st, new_target, proto)
  // Steps 3-4: pending state, empty resource stack
  alloc_stack(st, proto_h, async:, disposable_state: Pending([]))
}

/// §10.1.13.2 GetPrototypeFromConstructor: `Get(newTarget, "prototype")` or
/// fall back to the intrinsic.
fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  fallback: Handle,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_obj.t_get_prop(st, new_target, StringKey(Named("prototype")))
  case classify(proto) {
    KHandle(h) -> #(h, st)
    _ -> #(fallback, st)
  }
}

/// Allocate a DisposableStackObj with the given brand and disposable state.
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

/// The single [[DisposableState]]/[[AsyncDisposableState]] slot read: `Some`
/// only when `this` is a DisposableStackObj with the matching `async` brand.
///
/// Both entry points go through it — `require_stack` turns a `None` into a
/// thrown TypeError, `dispose_async` into a rejected promise — so the brand
/// check itself is written once.
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

/// "DisposableStack" / "AsyncDisposableStack", for TypeError messages.
fn stack_type_name(async: Bool) -> String {
  case async {
    True -> "AsyncDisposableStack"
    False -> "DisposableStack"
  }
}

/// RequireInternalSlot(this, [[DisposableState]] / [[AsyncDisposableState]])
/// — this must be a DisposableStackObj with the matching brand, else
/// TypeError.
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

/// Write back a stack's disposable state (which carries the
/// [[DisposableResourceStack]]). The brand (`async`) never changes after
/// allocation, and writing `Disposed` structurally drops the resources.
///
/// Every caller reaches here only after `require_stack` (RequireInternalSlot)
/// has proven `h` is a live DisposableStackObj, so any other slot shape
/// is a wiring bug — crash rather than silently drop the write.
fn write_stack(
  st: Agent,
  h: Handle,
  disposable_state: DisposableState,
) -> Agent {
  rt_store.t_cell_update(st, h, fn(slot) {
    let assert SObject(kind: DisposableStackObj(async:, ..), ..) = slot
      as "disposable_stack: write_stack on a non-stack cell"
    SObject(..slot, kind: DisposableStackObj(async:, state: disposable_state))
  })
}

/// AddDisposableResource step 3: append to the capability's CURRENT
/// [[DisposableResourceStack]]. CreateDisposableResource (steps 1-2) runs
/// user code (@@dispose getters, proxy traps) that may re-entrantly
/// defer/dispose/move on this same stack, so the slot is re-read here rather
/// than reusing the pre-call snapshot. A stack disposed or moved during that
/// user code stays Disposed: the spec appends to a capability that is never
/// disposed again, which is observably the same as dropping the resource.
fn append_resource(
  st: Agent,
  this: JsVal,
  async: Bool,
  resource: DisposeResource,
) -> Agent {
  case read_stack(st, this, async) {
    Some(#(h, Pending(current))) ->
      write_stack(st, h, Pending([resource, ..current]))
    _ -> st
  }
}

/// §12.3.3.4 / §12.4.3.4 get (Async)DisposableStack.prototype.disposed
///
///   1. Let stack be the this value.
///   2. Perform ? RequireInternalSlot(stack, [[(Async)DisposableState]]).
///   3. If the state is disposed, return true; otherwise return false.
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

/// §12.3.3.3 DisposableStack.prototype.dispose ( )
///
///   1. Let disposableStack be the this value.
///   2. Perform ? RequireInternalSlot(disposableStack, [[DisposableState]]).
///   3. If disposableStack.[[DisposableState]] is disposed, return undefined.
///   4. Set disposableStack.[[DisposableState]] to disposed.
///   5. Return ? DisposeResources(disposableStack.[[DisposeCapability]],
///      NormalCompletion(undefined)).
fn dispose(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  use h, disposable_state <- require_stack(st, this, False, "dispose")
  case disposable_state {
    // Step 3: already disposed — no-op
    Disposed -> #(mk_undefined(), st)
    Pending(resources:) -> {
      // Step 4: mark disposed — which drops the resource stack — BEFORE
      // running disposers (re-entrant dispose() must not re-invoke them).
      let st = write_stack(st, h, Disposed)
      // Step 5: DisposeResources — resources is newest-first, which is the
      // spec's reverse list order.
      case dispose_resources(st, resources, NormalCompletion(mk_undefined())) {
        #(NormalCompletion(v), st) -> #(v, st)
        #(ThrowCompletion(thrown), st) -> rt_store.t_throw(st, thrown)
      }
    }
  }
}

/// DisposeResources ( disposeCapability, completion ) — proposal §3.1.3,
/// sync subset (every resource carries hint sync-dispose).
///
///   1. For each resource of [[DisposableResourceStack]], in reverse list
///      order, do
///      a. Let result be Completion(Dispose(resource.[[ResourceValue]],
///         resource.[[Hint]], resource.[[DisposeMethod]])).
///      b. If result is a throw completion, then
///         i. If completion is a throw completion, then
///            1-5. Let error be a newly created SuppressedError object with
///               .error = result.[[Value]] and .suppressed = completion.[[Value]].
///            6. Set completion to ThrowCompletion(error).
///         ii. Else, set completion to result.
///   2. Return ? completion.
fn dispose_resources(
  st: Agent,
  resources: List(DisposeResource),
  completion: rt_call.Completion,
) -> #(rt_call.Completion, Agent) {
  case resources {
    [] -> #(completion, st)
    [resource, ..rest] -> {
      // Step 1.a: Dispose(V, sync-dispose, method) — Call(method, V) for
      // use() resources, Call(callback, undefined, args) for adopt/defer.
      let #(result, st) = case resource {
        MethodDispose(value: v, method:) ->
          case classify(method) {
            // Dispose step 1: method undefined → result is undefined
            KUndef -> #(NormalCompletion(mk_undefined()), st)
            _ -> rt_call.t_call(st, method, v, [])
          }
        DisposeCallback(callback:, args:) ->
          rt_call.t_call(st, callback, mk_undefined(), args)
        // Async-only variants (created only by use_resource_async) cannot
        // reach the sync loop — brand is fixed at construction and move()
        // preserves it. Reaching here is an engine wiring bug.
        AsyncFallbackDispose(..) | NullDispose ->
          panic as "sync DisposableStack holds async-only resource variant — engine invariant"
      }
      case result {
        NormalCompletion(_) -> dispose_resources(st, rest, completion)
        // Step 1.b: throw completion
        ThrowCompletion(thrown) ->
          case completion {
            // Step 1.b.ii: first error becomes the pending completion
            NormalCompletion(_) ->
              dispose_resources(st, rest, ThrowCompletion(thrown))
            // Step 1.b.i: wrap into SuppressedError(error=new, suppressed=old)
            ThrowCompletion(prev) -> {
              let #(err, st) = b_error.make_suppressed_error(st, thrown, prev)
              dispose_resources(st, rest, ThrowCompletion(err))
            }
          }
      }
    }
  }
}

/// §12.3.3.6 DisposableStack.prototype.use ( value )
///
///   1. Let disposableStack be the this value.
///   2. Perform ? RequireInternalSlot(disposableStack, [[DisposableState]]).
///   3. If disposableStack.[[DisposableState]] is disposed, throw a
///      ReferenceError exception.
///   4. Perform ? AddDisposableResource(disposableStack.[[DisposeCapability]],
///      value, sync-dispose).
///   5. Return value.
fn use_resource(st: Agent, this: JsVal, args: List(JsVal)) -> #(JsVal, Agent) {
  use _h, disposable_state <- require_stack(st, this, False, "use")
  use _resources <- try_pending(st, disposable_state, async: False)
  let val = first_arg_or_undefined(args)
  case classify(val) {
    // AddDisposableResource step 1.a: null/undefined with sync-dispose and
    // no method → nothing is added.
    KUndef | KNull -> #(val, st)
    // CreateDisposableResource step 1.b.i: non-object → TypeError
    KHandle(_) -> {
      // CreateDisposableResource step 1.b.ii: GetDisposeMethod(V, sync-dispose)
      // = GetMethod(V, @@dispose) — read ONCE here, not again at dispose time.
      // The sync hint never yields the async fallback variant.
      let #(dispose_method, st) = get_dispose_method(st, val, is_async: False)
      let resource = case dispose_method {
        DirectDispose(method) | SyncFallbackDispose(method) ->
          MethodDispose(value: val, method: mk_object(method))
      }
      #(val, append_resource(st, this, False, resource))
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "DisposableStack.prototype.use called with a non-object, non-nullish value",
      )
  }
}

/// §12.4.3.6 AsyncDisposableStack.prototype.use ( value )
///
/// Same shape as the sync version, but AddDisposableResource with hint
/// async-dispose: null/undefined ARE added (as a method-less resource that
/// only forces an await), and GetDisposeMethod tries @@asyncDispose first,
/// then falls back to a wrapper around @@dispose.
fn use_resource_async(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use _h, disposable_state <- require_stack(st, this, True, "use")
  use _resources <- try_pending(st, disposable_state, async: True)
  let val = first_arg_or_undefined(args)
  case classify(val) {
    // CreateDisposableResource step 1.a: V null/undefined with async-dispose
    // → V = undefined, method = undefined. DisposeResources will still
    // perform one Await(undefined) for it (needsAwait).
    KUndef | KNull -> #(val, append_resource(st, this, True, NullDispose))
    KHandle(_) -> {
      // GetDisposeMethod(V, async-dispose): GetMethod(V, @@asyncDispose),
      // falling back to a closure around GetMethod(V, @@dispose).
      let #(dispose_method, st) = get_dispose_method(st, val, is_async: True)
      let resource = case dispose_method {
        DirectDispose(method) ->
          MethodDispose(value: val, method: mk_object(method))
        // GetDisposeMethod step 1.b.ii: wrapper closure — call the sync
        // method, discard its result, await undefined.
        SyncFallbackDispose(method) ->
          AsyncFallbackDispose(value: val, method: mk_object(method))
      }
      #(val, append_resource(st, this, True, resource))
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "AsyncDisposableStack.prototype.use called with a non-object, non-nullish value",
      )
  }
}

/// GetMethod(V, @@symbol) — §7.3.10: Get the property; undefined/null →
/// None; non-callable → TypeError; else the function's handle.
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

/// The outcome of GetDisposeMethod(V, hint) — which method was found and
/// therefore how it must be invoked at dispose time.
pub type DisposeMethod {
  /// The hint's own method: @@dispose for sync-dispose, @@asyncDispose for
  /// async-dispose. Its result is used as-is (awaited on an async stack).
  DirectDispose(method: Handle)
  /// async-dispose falling back to @@dispose (GetDisposeMethod step 1.b.ii):
  /// the wrapper closure calls the sync method, DISCARDS its result and
  /// awaits undefined instead. Only ever produced for the async hint.
  SyncFallbackDispose(method: Handle)
}

/// GetDisposeMethod ( V, hint ) — proposal §3.1.2. For sync-dispose:
/// GetMethod(V, @@dispose). For async-dispose: GetMethod(V, @@asyncDispose),
/// falling back to a result-discarding wrapper around GetMethod(V, @@dispose).
/// A missing method is a TypeError. V is always an object here. Shared with
/// the interpreter's `using` / `await using` CreateDisposableResource.
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

/// GetDisposeMethod's "no method for this hint" TypeError.
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

/// §12.3.3.1 / §12.4.3.1 (Async)DisposableStack.prototype.adopt ( value, onDispose )
///
///   1. Let stack be the this value.
///   2. Perform ? RequireInternalSlot(stack, [[(Async)DisposableState]]).
///   3. If the state is disposed, throw a ReferenceError exception.
///   4. If IsCallable(onDispose) is false, throw a TypeError exception.
///   5-6. Let F be a built-in closure performing Call(onDispose, undefined, « value »).
///   7. Perform ? AddDisposableResource([[DisposeCapability]], undefined, hint, F).
///   8. Return value.
fn adopt(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  async async: Bool,
) -> #(JsVal, Agent) {
  use h, disposable_state <- require_stack(st, this, async, "adopt")
  use resources <- try_pending(st, disposable_state, async:)
  let #(val, on_dispose) = two_args_or_undefined(args)
  case rt_call.is_callable(st, on_dispose) {
    // Step 4: onDispose must be callable
    False -> rt_val.t_throw_type_error(st, "onDispose is not a function")
    True -> {
      // Steps 5-7: stored as DisposeCallback — Call(onDispose, undefined, « value »)
      let resource = DisposeCallback(callback: on_dispose, args: [val])
      let st = write_stack(st, h, Pending([resource, ..resources]))
      // Step 8
      #(val, st)
    }
  }
}

/// §12.3.3.2 / §12.4.3.2 (Async)DisposableStack.prototype.defer ( onDispose )
///
/// Same as adopt but with no value: the closure performs
/// Call(onDispose, undefined, « ») and defer returns undefined.
fn defer(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
  async async: Bool,
) -> #(JsVal, Agent) {
  use h, disposable_state <- require_stack(st, this, async, "defer")
  use resources <- try_pending(st, disposable_state, async:)
  let on_dispose = first_arg_or_undefined(args)
  case rt_call.is_callable(st, on_dispose) {
    // Step 4: onDispose must be callable
    False -> rt_val.t_throw_type_error(st, "onDispose is not a function")
    True -> {
      // Step 5: Call(onDispose, undefined) with no arguments at dispose time
      let resource = DisposeCallback(callback: on_dispose, args: [])
      let st = write_stack(st, h, Pending([resource, ..resources]))
      // Step 6
      #(mk_undefined(), st)
    }
  }
}

/// §12.3.3.5 / §12.4.3.5 (Async)DisposableStack.prototype.move ( )
///
///   1. Let stack be the this value.
///   2. Perform ? RequireInternalSlot(stack, [[(Async)DisposableState]]).
///   3. If the state is disposed, throw a ReferenceError exception.
///   4. Let newStack be ? OrdinaryCreateFromConstructor(%(Async)DisposableStack%,
///      "%(Async)DisposableStack.prototype%", …) — always the intrinsic
///      prototype, even for subclass instances.
///   5. Set newStack's state to pending.
///   6. Set newStack.[[DisposeCapability]] to stack.[[DisposeCapability]].
///   7. Set stack.[[DisposeCapability]] to NewDisposeCapability().
///   8. Set stack's state to disposed.
///   9. Return newStack.
fn move(
  st: Agent,
  this: JsVal,
  proto: Handle,
  async async: Bool,
) -> #(JsVal, Agent) {
  use h, disposable_state <- require_stack(st, this, async, "move")
  use resources <- try_pending(st, disposable_state, async:)
  // Steps 4-6: new pending stack takes over the resources
  let #(new_h, st) =
    alloc_stack(st, proto, async:, disposable_state: Pending(resources))
  // Steps 7-8: original becomes disposed — Disposed carries no resources, so
  // its capability is emptied by construction.
  let st = write_stack(st, h, Disposed)
  #(mk_object(new_h), st)
}

/// Step 3 of use/adopt/defer/move: disposed stacks reject mutation with a
/// ReferenceError; a pending stack yields its resource stack.
fn try_pending(
  st: Agent,
  disposable_state: DisposableState,
  async async: Bool,
  cont cont: fn(List(DisposeResource)) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  case disposable_state {
    Disposed ->
      rt_val.t_throw_reference_error(
        st,
        stack_type_name(async) <> " already disposed",
      )
    Pending(resources:) -> cont(resources)
  }
}

// ============================================================================
// disposeAsync — the async DisposeResources loop
// ============================================================================

/// §12.4.3.3 AsyncDisposableStack.prototype.disposeAsync ( )
///
///   1. Let asyncDisposableStack be the this value.
///   2. Let promiseCapability be ! NewPromiseCapability(%Promise%).
///   3. If asyncDisposableStack does not have an [[AsyncDisposableState]]
///      internal slot, reject with a TypeError and return the promise.
///   4. If the state is disposed, resolve with undefined and return.
///   5. Set the state to disposed.
///   6. Let result be DisposeResources([[DisposeCapability]],
///      NormalCompletion(undefined)).
///   7. IfAbruptRejectPromise(result, promiseCapability).
///   8. Resolve with undefined.
///   9. Return promiseCapability.[[Promise]].
fn dispose_async(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  // Step 2: NewPromiseCapability(%Promise%)
  let #(#(promise_h, resolve_h, reject_h), st) =
    rt_async.t_new_promise_capability(st)
  let promise = mk_object(promise_h)
  let resolve = mk_object(resolve_h)
  let reject = mk_object(reject_h)
  // Step 3: same RequireInternalSlot as `require_stack`, but a failure must
  // REJECT the promise rather than throw — hence the shared `read_stack`.
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
    // Step 4: already disposed → resolve with undefined
    Some(#(_h, Disposed)) -> {
      let st = settle_capability(st, resolve, mk_undefined())
      #(promise, st)
    }
    Some(#(h, Pending(resources:))) -> {
      // Step 5: mark disposed — which drops the resource stack — before
      // running disposers
      let st = write_stack(st, h, Disposed)
      // Step 6: async DisposeResources loop
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

/// DisposeResources ( disposeCapability, completion ) — async subset, driven
/// step-by-step across microtasks. Each successfully called disposer's result
/// is awaited via a promise reaction carrying an AsyncDisposeContinue token;
/// throws are folded into `pending` (the spec's throw completion) and the
/// loop continues synchronously.
///
///   1. Let needsAwait be false. 2. Let hasAwaited be false.
///   3. For each resource, in reverse list order:
///      b. If method is not undefined:
///         i.  Let result be Completion(Call(method, value)).
///         ii. If result is normal and hint is async-dispose:
///             Set result to Completion(Await(result.[[Value]])).
///             Set hasAwaited to true.
///         iii. If result is throw → fold into completion (SuppressedError).
///      f. Else: set needsAwait to true (null/undefined `await using` value).
///   4. If needsAwait is true and hasAwaited is false, perform ! Await(undefined).
///   5. Return completion — here: settle the disposeAsync capability.
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
        // Step 4: Await(undefined) — one extra microtask hop before settling
        True -> attach_await(st, mk_undefined(), [], pending, resolve, reject)
        False ->
          // Step 5: settle the capability with the final completion
          case pending {
            None -> settle_capability(st, resolve, mk_undefined())
            Some(err) -> settle_capability(st, reject, err)
          }
      }
    [resource, ..rest] -> {
      // Steps 3.b.i-iii for the kinds whose disposer result is Awaited: call
      // it, Await a normal result, fold a throw into `pending` and continue
      // synchronously.
      let call_then_await = fn(callee, this, args) {
        case rt_call.t_call(st, callee, this, args) {
          // Step 3.b.ii: Await(result) — hasAwaited becomes true
          #(NormalCompletion(result), st) ->
            attach_await(st, result, rest, pending, resolve, reject)
          // Step 3.b.iii: fold the throw in and continue synchronously
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
        // @@asyncDispose captured by use().
        MethodDispose(value: v, method:) -> call_then_await(method, v, [])
        // adopt/defer closure. The spec closure RETURNS the Call result, so an
        // async onDispose's rejected promise must reject disposeAsync — Await
        // it like any other dispose method.
        DisposeCallback(callback:, args:) ->
          call_then_await(callback, mk_undefined(), args)
        // @@dispose fallback wrapper: call, DISCARD result, Await(undefined).
        // GetDisposeMethod's closure performs IfAbruptRejectPromise, so a
        // synchronous throw becomes a REJECTED promise that the loop then
        // Awaits (hasAwaited := true, error folded after a microtask hop) —
        // it is never folded synchronously.
        AsyncFallbackDispose(value: v, method:) ->
          case rt_call.t_call(st, method, v, []) {
            #(NormalCompletion(_discarded), st) ->
              attach_await(st, mk_undefined(), rest, pending, resolve, reject)
            #(ThrowCompletion(thrown), st) ->
              attach_await_rejected(st, thrown, rest, pending, resolve, reject)
          }
        // Step 3.f: method-less resource (use(null/undefined)) — needsAwait
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

/// DisposeResources step 1.b error folding: first error becomes the pending
/// completion; later errors wrap it in SuppressedError(error=new, suppressed=old).
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

/// Await(value): PromiseResolve(%Promise%, value) — an existing native
/// promise is reused, a thenable is assimilated via PromiseResolveThenableJob
/// (its `then` drives settlement), a throwing `then` getter rejects, anything
/// else fulfills immediately — then attach AsyncDisposeContinue
/// fulfill/reject reactions that resume the loop when it settles.
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

/// Await of a synchronously-thrown error: GetDisposeMethod's fallback closure
/// performs IfAbruptRejectPromise, so the throw surfaces as a rejected
/// promise that DisposeResources then Awaits. The reject reaction folds the
/// error into the pending completion and resumes with hasAwaited = true.
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

/// Attach the AsyncDisposeContinue fulfill/reject reactions to a promise so
/// the loop resumes when it settles. The child capability is a throwaway
/// %Promise% one — the loop settles the disposeAsync promise itself.
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

/// Allocate an AsyncDisposeContinue reaction handler function object
/// (non-rooted, GC-governed through the promise reaction that holds it).
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

/// Reaction handler resuming the async dispose loop after an Await settles.
/// A rejected await folds the rejection reason into the pending completion
/// (DisposeResources step 3.b.iii applied to the Await result).
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
      // An await just completed: hasAwaited is true from here on.
      needs_await: False,
      has_awaited: True,
      resolve:,
      reject:,
    )
  #(mk_undefined(), st)
}

/// Call the promise capability's resolve/reject function with one argument.
/// The capability is always `NewPromiseCapability(%Promise%)` — its intrinsic
/// resolving functions never throw (§27.2.1.3).
fn settle_capability(st: Agent, fun: JsVal, arg: JsVal) -> Agent {
  let #(_val, st) = rt_call.t_call_checked(st, fun, mk_undefined(), [arg])
  st
}
