/// DisposableStack / AsyncDisposableStack Objects — Explicit Resource
/// Management proposal §12.3 / §12.4
/// (https://tc39.es/proposal-explicit-resource-management/).
///
/// Both stacks hold a [[DisposableResourceStack]] of DisposableResource
/// records and a disposable state (pending | disposed). Resources are stored
/// NEWEST-FIRST in the DisposableStackObject exotic kind, so dispose() walks
/// the list head-first — the spec's "reverse list order". The `async` field
/// is the brand: sync methods require async=False, async methods async=True.
import arc/vm/builtins/common.{type BuiltinType, BuiltinType}
import arc/vm/builtins/error
import arc/vm/builtins/helpers
import arc/vm/builtins/promise as builtins_promise
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/ops/object
import arc/vm/state.{type State, State}
import arc/vm/value.{
  type DisposableStackNativeFn, type DisposableState, type DisposeResource,
  type JsValue, type Ref, AsyncDisposableStackConstructor,
  AsyncDisposableStackDisposedGetter, AsyncDisposableStackPrototypeAdopt,
  AsyncDisposableStackPrototypeDefer, AsyncDisposableStackPrototypeDisposeAsync,
  AsyncDisposableStackPrototypeMove, AsyncDisposableStackPrototypeUse,
  AsyncDisposeContinue, AsyncFallbackDispose, Dispatch,
  DisposableStackConstructor, DisposableStackDisposedGetter,
  DisposableStackNative, DisposableStackObject, DisposableStackPrototypeAdopt,
  DisposableStackPrototypeDefer, DisposableStackPrototypeDispose,
  DisposableStackPrototypeMove, DisposableStackPrototypeUse, DisposeCallback,
  Disposed, JsBool, JsNull, JsObject, JsUndefined, NullDispose, ObjectSlot,
  Pending, SyncDispose,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

/// Set up DisposableStack.prototype and the DisposableStack constructor.
pub fn init(
  h: state.Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(state.Heap(host), BuiltinType) {
  init_stack_type(
    h,
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
    dispose_symbol: value.symbol_dispose,
    disposed_fn: DisposableStackDisposedGetter,
  )
}

/// Set up AsyncDisposableStack.prototype and its constructor.
pub fn init_async(
  h: state.Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(state.Heap(host), BuiltinType) {
  init_stack_type(
    h,
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
    dispose_symbol: value.symbol_async_dispose,
    disposed_fn: AsyncDisposableStackDisposedGetter,
  )
}

/// Shared init scaffold for both stack types: prototype with
/// use/adopt/defer/move, a dispose method aliased under its well-known
/// symbol (the SAME function object — test262 asserts identity), a
/// `disposed` getter, @@toStringTag, and the constructor.
fn init_stack_type(
  h: state.Heap(host),
  object_proto: Ref,
  function_proto: Ref,
  name name: String,
  ctor_fn ctor_fn: fn(Ref) -> DisposableStackNativeFn,
  use_fn use_fn: DisposableStackNativeFn,
  adopt_fn adopt_fn: DisposableStackNativeFn,
  defer_fn defer_fn: DisposableStackNativeFn,
  move_fn move_fn: fn(Ref) -> DisposableStackNativeFn,
  dispose_name dispose_name: String,
  dispose_fn dispose_fn: DisposableStackNativeFn,
  dispose_symbol dispose_symbol: value.SymbolId,
  disposed_fn disposed_fn: DisposableStackNativeFn,
) -> #(state.Heap(host), BuiltinType) {
  // Reserve the prototype ref first: move() embeds it in its native-fn token
  // (the moved-to stack is always created from the intrinsic prototype,
  // never from new.target).
  let #(h, proto_ref) = heap.reserve(h)
  let h = heap.root(h, proto_ref)

  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("use", DisposableStackNative(use_fn), 1),
      #("adopt", DisposableStackNative(adopt_fn), 2),
      #("defer", DisposableStackNative(defer_fn), 1),
      #("move", DisposableStackNative(move_fn(proto_ref)), 0),
    ])
  // dispose/disposeAsync allocated separately: the prototype's well-known
  // symbol property must be the SAME function object as the named method.
  let #(h, dispose_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      DisposableStackNative(dispose_fn),
      dispose_name,
      0,
    )
  let dispose_prop = value.builtin_property(JsObject(dispose_ref))
  let #(h, getters) =
    common.alloc_getters(h, function_proto, [
      #("disposed", DisposableStackNative(disposed_fn)),
    ])
  let proto_props =
    list.flatten([getters, [#(dispose_name, dispose_prop)], methods])

  // common.init_type reserves its own proto ref; we already reserved ours
  // (move() needs it before the prototype exists), so build ctor + proto by
  // hand, mirroring common.init_type.
  let #(h, ctor_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: value.NativeFunction(
          Dispatch(DisposableStackNative(ctor_fn(proto_ref))),
          constructible: True,
        ),
        properties: common.named_props([
          // §12.3.2.1: { writable: false, enumerable: false, configurable: false }
          #("prototype", common.fn_prototype_property(proto_ref)),
          #("length", common.fn_length_property(0)),
          #("name", common.fn_name_property(name)),
        ]),
        elements: elements.new(),
        prototype: Some(function_proto),
        symbol_properties: [],
        extensible: True,
      ),
    )
  let h = heap.root(h, ctor_ref)

  let h =
    heap.write(
      h,
      proto_ref,
      ObjectSlot(
        kind: value.OrdinaryObject,
        properties: common.named_props([
          #("constructor", value.builtin_property(JsObject(ctor_ref))),
          ..proto_props
        ]),
        elements: elements.new(),
        prototype: Some(object_proto),
        symbol_properties: [
          common.to_string_tag(name),
          #(dispose_symbol, dispose_prop),
        ],
        extensible: True,
      ),
    )

  #(h, BuiltinType(prototype: proto_ref, constructor: ctor_ref))
}

/// Per-module dispatch for DisposableStack/AsyncDisposableStack natives.
pub fn dispatch(
  native: DisposableStackNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    DisposableStackConstructor(proto:) ->
      construct(proto, state, async: False, name: "DisposableStack")
    DisposableStackPrototypeDispose -> dispose(this, state)
    DisposableStackPrototypeUse -> use_resource(this, args, state)
    DisposableStackPrototypeAdopt -> adopt(this, args, state, async: False)
    DisposableStackPrototypeDefer -> defer(this, args, state, async: False)
    DisposableStackPrototypeMove(proto:) ->
      move(this, proto, state, async: False)
    DisposableStackDisposedGetter -> disposed_getter(this, state, async: False)
    AsyncDisposableStackConstructor(proto:) ->
      construct(proto, state, async: True, name: "AsyncDisposableStack")
    AsyncDisposableStackPrototypeDisposeAsync -> dispose_async(this, state)
    AsyncDisposableStackPrototypeUse -> use_resource_async(this, args, state)
    AsyncDisposableStackPrototypeAdopt -> adopt(this, args, state, async: True)
    AsyncDisposableStackPrototypeDefer -> defer(this, args, state, async: True)
    AsyncDisposableStackPrototypeMove(proto:) ->
      move(this, proto, state, async: True)
    AsyncDisposableStackDisposedGetter ->
      disposed_getter(this, state, async: True)
    AsyncDisposeContinue(remaining:, pending:, resolve:, reject:, is_reject:) ->
      async_dispose_continue(
        args,
        state,
        remaining,
        pending,
        resolve,
        reject,
        is_reject,
      )
    value.UsingDisposer(method:, value: resource, discard:) ->
      case state.call(state, method, resource, []) {
        Ok(#(result, state)) ->
          case discard {
            // GetDisposeMethod step 1.b.ii closure: drop the sync @@dispose
            // result so the desugared `await` awaits undefined instead.
            True -> #(state, Ok(JsUndefined))
            False -> #(state, Ok(result))
          }
        Error(#(thrown, state)) ->
          case discard {
            // GetDisposeMethod step 1.b.ii closure performs
            // IfAbruptRejectPromise: a sync @@dispose throw must surface as
            // a REJECTED promise that the desugared `await` then consumes,
            // so the error lands one Await hop later — never synchronously.
            True -> {
              let #(h, builtins_promise.PromiseRefs(promise:, data:)) =
                builtins_promise.create_promise(
                  state.heap,
                  state.builtins.promise.prototype,
                )
              let state =
                builtins_promise.reject_promise(
                  State(..state, heap: h),
                  data,
                  thrown,
                )
              #(state, Ok(JsObject(promise)))
            }
            False -> #(state, Error(thrown))
          }
      }
  }
}

/// CreateDisposableResource(V, hint) for `using` / `await using`
/// declarations (GetDisposer opcode). Returns the disposer callable, or
/// undefined when V is null/undefined (nothing to dispose — for the async
/// hint the desugared finally still performs a coalesced Await(undefined)).
/// Throws TypeError when V is not an object or has no dispose method.
pub fn make_using_disposer(
  state: State(host),
  val: JsValue,
  is_async is_async: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    // CreateDisposableResource step 1.a: null/undefined → method undefined.
    JsUndefined | JsNull -> #(state, Ok(JsUndefined))
    JsObject(_) -> {
      // GetDisposeMethod(V, hint) — captured ONCE here, not again at dispose
      // time. The async hint's @@dispose fallback wraps the method in a
      // result-discarding closure (GetDisposeMethod step 1.b.ii).
      use dispose_method, state <- get_dispose_method(state, val, is_async:)
      case dispose_method {
        DirectDispose(method) ->
          alloc_using_disposer(state, method, val, discard: False)
        SyncFallbackDispose(method) ->
          alloc_using_disposer(state, method, val, discard: True)
      }
    }
    _ ->
      state.type_error(
        state,
        "using declaration initializer is not an object, null, or undefined",
      )
  }
}

/// Allocate the (non-rooted, GC-governed) disposer function object.
fn alloc_using_disposer(
  state: State(host),
  method: JsValue,
  val: JsValue,
  discard discard: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      value.NativeFunction(
        Dispatch(
          DisposableStackNative(value.UsingDisposer(
            method:,
            value: val,
            discard:,
          )),
        ),
        constructible: False,
      ),
      state.builtins.function.prototype,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
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
  proto: Ref,
  state: State(host),
  async async: Bool,
  name name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: require NewTarget, then
  // GetPrototypeFromConstructor(NewTarget, intrinsic).
  use proto_ref, state <- helpers.require_new_target(state, name, proto)
  // Steps 3-4: pending state, empty resource stack
  let #(heap, ref) =
    alloc_stack(state.heap, proto_ref, async:, disposable_state: Pending([]))
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Allocate a DisposableStackObject with the given brand and disposable state.
fn alloc_stack(
  h: state.Heap(host),
  proto: Ref,
  async async: Bool,
  disposable_state disposable_state: DisposableState,
) -> #(state.Heap(host), Ref) {
  heap.alloc(
    h,
    ObjectSlot(
      kind: DisposableStackObject(async:, state: disposable_state),
      properties: dict.new(),
      elements: elements.new(),
      prototype: Some(proto),
      symbol_properties: [],
      extensible: True,
    ),
  )
}

/// The single [[DisposableState]]/[[AsyncDisposableState]] slot read: `Some`
/// only when `this` is a DisposableStackObject with the matching `async` brand.
///
/// Both entry points go through it — `require_stack` turns a `None` into a
/// thrown TypeError, `dispose_async` into a rejected promise — so the brand
/// check itself is written once.
fn read_stack(
  state: State(host),
  this: JsValue,
  async: Bool,
) -> Option(#(Ref, DisposableState)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: DisposableStackObject(async: a, state: disposable_state),
          ..,
        ))
          if a == async
        -> Some(#(ref, disposable_state))
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
/// — this must be a DisposableStackObject with the matching brand, else
/// TypeError.
fn require_stack(
  this: JsValue,
  state: State(host),
  async: Bool,
  method: String,
  cont: fn(Ref, DisposableState, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case read_stack(state, this, async) {
    Some(#(ref, disposable_state)) -> cont(ref, disposable_state, state)
    None ->
      state.type_error(
        state,
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
/// has proven `ref` is a live DisposableStackObject, so any other slot shape
/// is a wiring bug — crash rather than silently drop the write.
fn write_stack(
  state: State(host),
  ref: Ref,
  disposable_state: DisposableState,
) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      let assert ObjectSlot(kind: DisposableStackObject(async:, ..), ..) = slot
      ObjectSlot(
        ..slot,
        kind: DisposableStackObject(async:, state: disposable_state),
      )
    })
  State(..state, heap:)
}

/// §12.3.3.4 / §12.4.3.4 get (Async)DisposableStack.prototype.disposed
///
///   1. Let stack be the this value.
///   2. Perform ? RequireInternalSlot(stack, [[(Async)DisposableState]]).
///   3. If the state is disposed, return true; otherwise return false.
fn disposed_getter(
  this: JsValue,
  state: State(host),
  async async: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _ref, disposable_state, state <- require_stack(
    this,
    state,
    async,
    "disposed",
  )
  case disposable_state {
    Disposed -> #(state, Ok(JsBool(True)))
    Pending(_) -> #(state, Ok(JsBool(False)))
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
fn dispose(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, disposable_state, state <- require_stack(
    this,
    state,
    False,
    "dispose",
  )
  case disposable_state {
    // Step 3: already disposed — no-op
    Disposed -> #(state, Ok(JsUndefined))
    Pending(resources:) -> {
      // Step 4: mark disposed — which drops the resource stack — BEFORE
      // running disposers (re-entrant dispose() must not re-invoke them).
      let state = write_stack(state, ref, Disposed)
      // Step 5: DisposeResources — resources is newest-first, which is the
      // spec's reverse list order.
      dispose_resources(state, resources, Ok(JsUndefined))
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
  state: State(host),
  resources: List(DisposeResource),
  completion: Result(JsValue, JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  case resources {
    [] -> #(state, completion)
    [resource, ..rest] -> {
      // Step 1.a: Dispose(V, sync-dispose, method) — Call(method, V) for
      // use() resources, Call(callback, undefined, args) for adopt/defer.
      let result = case resource {
        SyncDispose(value: v, method:)
        | AsyncFallbackDispose(value: v, method:) ->
          case method {
            // Dispose step 1: method undefined → result is undefined
            JsUndefined -> Ok(#(JsUndefined, state))
            _ -> state.call(state, method, v, [])
          }
        DisposeCallback(callback:, args:) ->
          state.call(state, callback, JsUndefined, args)
        NullDispose -> Ok(#(JsUndefined, state))
      }
      case result {
        Ok(#(_val, state)) -> dispose_resources(state, rest, completion)
        // Step 1.b: throw completion
        Error(#(thrown, state)) ->
          case completion {
            // Step 1.b.ii: first error becomes the pending completion
            Ok(_) -> dispose_resources(state, rest, Error(thrown))
            // Step 1.b.i: wrap into SuppressedError(error=new, suppressed=old)
            Error(prev) -> {
              let #(state, err) =
                error.make_suppressed_error(state, thrown, prev)
              dispose_resources(state, rest, Error(err))
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
fn use_resource(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, disposable_state, state <- require_stack(this, state, False, "use")
  use resources, state <- try_pending(state, disposable_state)
  let val = helpers.first_arg_or_undefined(args)
  case val {
    // AddDisposableResource step 1.a: null/undefined with sync-dispose and
    // no method → nothing is added.
    JsUndefined | JsNull -> #(state, Ok(val))
    // CreateDisposableResource step 1.b.i: non-object → TypeError
    JsObject(_) -> {
      // CreateDisposableResource step 1.b.ii: GetDisposeMethod(V, sync-dispose)
      // = GetMethod(V, @@dispose) — read ONCE here, not again at dispose time.
      // The sync hint never yields the async fallback variant.
      use dispose_method, state <- get_dispose_method(
        state,
        val,
        is_async: False,
      )
      let resource = case dispose_method {
        DirectDispose(method) | SyncFallbackDispose(method) ->
          SyncDispose(value: val, method:)
      }
      let state = write_stack(state, ref, Pending([resource, ..resources]))
      #(state, Ok(val))
    }
    _ ->
      state.type_error(
        state,
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
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, disposable_state, state <- require_stack(this, state, True, "use")
  use resources, state <- try_pending(state, disposable_state)
  let val = helpers.first_arg_or_undefined(args)
  case val {
    // CreateDisposableResource step 1.a: V null/undefined with async-dispose
    // → V = undefined, method = undefined. DisposeResources will still
    // perform one Await(undefined) for it (needsAwait).
    JsUndefined | JsNull -> {
      let state = write_stack(state, ref, Pending([NullDispose, ..resources]))
      #(state, Ok(val))
    }
    JsObject(_) -> {
      // GetDisposeMethod(V, async-dispose): GetMethod(V, @@asyncDispose),
      // falling back to a closure around GetMethod(V, @@dispose).
      use dispose_method, state <- get_dispose_method(
        state,
        val,
        is_async: True,
      )
      let resource = case dispose_method {
        DirectDispose(method) -> SyncDispose(value: val, method:)
        // GetDisposeMethod step 1.b.ii: wrapper closure — call the sync
        // method, discard its result, await undefined.
        SyncFallbackDispose(method) -> AsyncFallbackDispose(value: val, method:)
      }
      let state = write_stack(state, ref, Pending([resource, ..resources]))
      #(state, Ok(val))
    }
    _ ->
      state.type_error(
        state,
        "AsyncDisposableStack.prototype.use called with a non-object, non-nullish value",
      )
  }
}

/// GetMethod(V, @@symbol) — §7.3.10: Get the property; undefined/null →
/// undefined; non-callable → TypeError; else the function.
fn try_get_method(
  state: State(host),
  val: JsValue,
  symbol: value.SymbolId,
  cont: fn(JsValue, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use method, state <- state.try_op(object.get_symbol_value_of(
    state,
    val,
    symbol,
  ))
  case method {
    JsUndefined | JsNull -> cont(JsUndefined, state)
    _ ->
      case helpers.is_callable(state.heap, method) {
        False ->
          state.type_error(state, "Dispose method property is not callable")
        True -> cont(method, state)
      }
  }
}

/// The outcome of GetDisposeMethod(V, hint) — which method was found and
/// therefore how it must be invoked at dispose time.
type DisposeMethod {
  /// The hint's own method: @@dispose for sync-dispose, @@asyncDispose for
  /// async-dispose. Its result is used as-is (awaited on an async stack).
  DirectDispose(method: JsValue)
  /// async-dispose falling back to @@dispose (GetDisposeMethod step 1.b.ii):
  /// the wrapper closure calls the sync method, DISCARDS its result and
  /// awaits undefined instead. Only ever produced for the async hint.
  SyncFallbackDispose(method: JsValue)
}

/// GetDisposeMethod ( V, hint ) — proposal §3.1.2. For sync-dispose:
/// GetMethod(V, @@dispose). For async-dispose: GetMethod(V, @@asyncDispose),
/// falling back to a result-discarding wrapper around GetMethod(V, @@dispose).
/// A missing method is a TypeError. V is always an object here.
fn get_dispose_method(
  state: State(host),
  val: JsValue,
  is_async is_async: Bool,
  cont cont: fn(DisposeMethod, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case is_async {
    False -> {
      use method, state <- try_get_method(state, val, value.symbol_dispose)
      case method {
        JsUndefined -> no_dispose_method(state, is_async)
        _ -> cont(DirectDispose(method), state)
      }
    }
    True -> {
      use method, state <- try_get_method(
        state,
        val,
        value.symbol_async_dispose,
      )
      case method {
        JsUndefined -> {
          use sync_method, state <- try_get_method(
            state,
            val,
            value.symbol_dispose,
          )
          case sync_method {
            JsUndefined -> no_dispose_method(state, is_async)
            _ -> cont(SyncFallbackDispose(sync_method), state)
          }
        }
        _ -> cont(DirectDispose(method), state)
      }
    }
  }
}

/// GetDisposeMethod's "no method for this hint" TypeError.
fn no_dispose_method(
  state: State(host),
  is_async: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case is_async {
    True ->
      state.type_error(
        state,
        "Object does not have a [Symbol.asyncDispose] or [Symbol.dispose] method",
      )
    False ->
      state.type_error(state, "Object does not have a [Symbol.dispose] method")
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
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  async async: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, disposable_state, state <- require_stack(this, state, async, "adopt")
  use resources, state <- try_pending(state, disposable_state)
  let #(val, on_dispose) = helpers.two_args_or_undefined(args)
  case helpers.is_callable(state.heap, on_dispose) {
    // Step 4: onDispose must be callable
    False -> state.type_error(state, "onDispose is not a function")
    True -> {
      // Steps 5-7: stored as DisposeCallback — Call(onDispose, undefined, « value »)
      let resource = DisposeCallback(callback: on_dispose, args: [val])
      let state = write_stack(state, ref, Pending([resource, ..resources]))
      // Step 8
      #(state, Ok(val))
    }
  }
}

/// §12.3.3.2 / §12.4.3.2 (Async)DisposableStack.prototype.defer ( onDispose )
///
/// Same as adopt but with no value: the closure performs
/// Call(onDispose, undefined, « ») and defer returns undefined.
fn defer(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  async async: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, disposable_state, state <- require_stack(this, state, async, "defer")
  use resources, state <- try_pending(state, disposable_state)
  let on_dispose = helpers.first_arg_or_undefined(args)
  case helpers.is_callable(state.heap, on_dispose) {
    // Step 4: onDispose must be callable
    False -> state.type_error(state, "onDispose is not a function")
    True -> {
      // Step 5: Call(onDispose, undefined) with no arguments at dispose time
      let resource = DisposeCallback(callback: on_dispose, args: [])
      let state = write_stack(state, ref, Pending([resource, ..resources]))
      // Step 6
      #(state, Ok(JsUndefined))
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
  this: JsValue,
  proto: Ref,
  state: State(host),
  async async: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, disposable_state, state <- require_stack(this, state, async, "move")
  use resources, state <- try_pending(state, disposable_state)
  // Steps 4-6: new pending stack takes over the resources
  let #(heap, new_ref) =
    alloc_stack(state.heap, proto, async:, disposable_state: Pending(resources))
  let state = State(..state, heap:)
  // Steps 7-8: original becomes disposed — Disposed carries no resources, so
  // its capability is emptied by construction.
  let state = write_stack(state, ref, Disposed)
  #(state, Ok(JsObject(new_ref)))
}

/// Step 3 of use/adopt/defer/move: disposed stacks reject mutation with a
/// ReferenceError; a pending stack yields its resource stack.
fn try_pending(
  state: State(host),
  disposable_state: DisposableState,
  cont: fn(List(DisposeResource), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case disposable_state {
    Disposed -> state.reference_error(state, "DisposableStack already disposed")
    Pending(resources:) -> cont(resources, state)
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
fn dispose_async(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 2: NewPromiseCapability(%Promise%)
  let #(h, cap) =
    builtins_promise.new_promise_capability(state.heap, state.builtins)
  let state = State(..state, heap: h)
  let promise = JsObject(cap.promise)
  // Step 3: same RequireInternalSlot as `require_stack`, but a failure must
  // REJECT the promise rather than throw — hence the shared `read_stack`.
  case read_stack(state, this, True) {
    None -> {
      let #(err, state) =
        state.type_error_value(
          state,
          "Method "
            <> stack_type_name(True)
            <> ".prototype.disposeAsync called on incompatible receiver",
        )
      let state = settle_capability(state, cap.reject, err)
      #(state, Ok(promise))
    }
    // Step 4: already disposed → resolve with undefined
    Some(#(_ref, Disposed)) -> {
      let state = settle_capability(state, cap.resolve, JsUndefined)
      #(state, Ok(promise))
    }
    Some(#(ref, Pending(resources:))) -> {
      // Step 5: mark disposed — which drops the resource stack — before
      // running disposers
      let state = write_stack(state, ref, Disposed)
      // Step 6: async DisposeResources loop
      let state =
        async_dispose_loop(
          state,
          resources,
          pending: None,
          needs_await: False,
          has_awaited: False,
          resolve: cap.resolve,
          reject: cap.reject,
        )
      #(state, Ok(promise))
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
  state: State(host),
  resources: List(DisposeResource),
  pending pending: Option(JsValue),
  needs_await needs_await: Bool,
  has_awaited has_awaited: Bool,
  resolve resolve: JsValue,
  reject reject: JsValue,
) -> State(host) {
  case resources {
    [] ->
      case needs_await && !has_awaited {
        // Step 4: Await(undefined) — one extra microtask hop before settling
        True -> attach_await(state, JsUndefined, [], pending, resolve, reject)
        False ->
          // Step 5: settle the capability with the final completion
          case pending {
            None -> settle_capability(state, resolve, JsUndefined)
            Some(err) -> settle_capability(state, reject, err)
          }
      }
    [resource, ..rest] -> {
      // Steps 3.b.i-iii for the kinds whose disposer result is Awaited: call
      // it, Await a normal result, fold a throw into `pending` and continue
      // synchronously.
      let call_then_await = fn(callee, this, args) {
        case state.call(state, callee, this, args) {
          // Step 3.b.ii: Await(result) — hasAwaited becomes true
          Ok(#(result, state)) ->
            attach_await(state, result, rest, pending, resolve, reject)
          // Step 3.b.iii: fold the throw in and continue synchronously
          Error(#(thrown, state)) -> {
            let #(state, pending) = fold_error(state, pending, thrown)
            async_dispose_loop(
              state,
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
        // @@asyncDispose (or a sync @@dispose moved in here by move()).
        SyncDispose(value: v, method:) -> call_then_await(method, v, [])
        // adopt/defer closure. The spec closure RETURNS the Call result, so an
        // async onDispose's rejected promise must reject disposeAsync — Await
        // it like any other dispose method.
        DisposeCallback(callback:, args:) ->
          call_then_await(callback, JsUndefined, args)
        // @@dispose fallback wrapper: call, DISCARD result, Await(undefined).
        // GetDisposeMethod's closure performs IfAbruptRejectPromise, so a
        // synchronous throw becomes a REJECTED promise that the loop then
        // Awaits (hasAwaited := true, error folded after a microtask hop) —
        // it is never folded synchronously.
        AsyncFallbackDispose(value: v, method:) ->
          case state.call(state, method, v, []) {
            Ok(#(_discarded, state)) ->
              attach_await(state, JsUndefined, rest, pending, resolve, reject)
            Error(#(thrown, state)) ->
              attach_await_rejected(
                state,
                thrown,
                rest,
                pending,
                resolve,
                reject,
              )
          }
        // Step 3.f: method-less resource (use(null/undefined)) — needsAwait
        NullDispose ->
          async_dispose_loop(
            state,
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
  state: State(host),
  pending: Option(JsValue),
  thrown: JsValue,
) -> #(State(host), Option(JsValue)) {
  case pending {
    None -> #(state, Some(thrown))
    Some(prev) -> {
      let #(state, err) = error.make_suppressed_error(state, thrown, prev)
      #(state, Some(err))
    }
  }
}

/// Await(value): resolve into a promise via PromiseResolve semantics, then
/// attach AsyncDisposeContinue fulfill/reject reactions that resume the loop
/// when it settles.
fn attach_await(
  state: State(host),
  awaited: JsValue,
  rest: List(DisposeResource),
  pending: Option(JsValue),
  resolve: JsValue,
  reject: JsValue,
) -> State(host) {
  // PromiseResolve: reuse an existing native promise's data directly.
  let existing = case awaited {
    JsObject(ref) -> heap.read_promise_data_ref(state.heap, ref)
    _ -> None
  }
  case existing {
    Some(dr) -> attach_reactions(state, dr, rest, pending, resolve, reject)
    None -> {
      // Otherwise create a fresh promise and run the Promise Resolve
      // Functions steps 8-13 on it: a thenable is assimilated via
      // PromiseResolveThenableJob (its `then` drives settlement), a throwing
      // `then` getter rejects, anything else fulfills immediately.
      let #(h, builtins_promise.PromiseRefs(promise:, data:)) =
        builtins_promise.create_promise(
          state.heap,
          state.builtins.promise.prototype,
        )
      let state =
        builtins_promise.resolve_promise(
          State(..state, heap: h),
          promise,
          data,
          awaited,
        )
      attach_reactions(state, data, rest, pending, resolve, reject)
    }
  }
}

/// Await of a synchronously-thrown error: GetDisposeMethod's fallback closure
/// performs IfAbruptRejectPromise, so the throw surfaces as a rejected
/// promise that DisposeResources then Awaits. The reject reaction folds the
/// error into the pending completion and resumes with hasAwaited = true.
fn attach_await_rejected(
  state: State(host),
  thrown: JsValue,
  rest: List(DisposeResource),
  pending: Option(JsValue),
  resolve: JsValue,
  reject: JsValue,
) -> State(host) {
  let #(h, builtins_promise.PromiseRefs(promise: _, data:)) =
    builtins_promise.create_promise(
      state.heap,
      state.builtins.promise.prototype,
    )
  let state =
    builtins_promise.reject_promise(State(..state, heap: h), data, thrown)
  attach_reactions(state, data, rest, pending, resolve, reject)
}

/// Attach the AsyncDisposeContinue fulfill/reject reactions to a promise's
/// data slot so the loop resumes when it settles.
fn attach_reactions(
  state: State(host),
  promise_data: Ref,
  rest: List(DisposeResource),
  pending: Option(JsValue),
  resolve: JsValue,
  reject: JsValue,
) -> State(host) {
  let b = state.builtins
  let #(h, on_fulfill) =
    alloc_continue(
      state.heap,
      b,
      rest,
      pending,
      resolve,
      reject,
      is_reject: False,
    )
  let #(h, on_reject) =
    alloc_continue(h, b, rest, pending, resolve, reject, is_reject: True)
  builtins_promise.perform_promise_then(
    State(..state, heap: h),
    promise_data,
    JsObject(on_fulfill),
    JsObject(on_reject),
    // No child capability — the loop settles the disposeAsync promise itself.
    JsUndefined,
    JsUndefined,
  )
}

/// Allocate an AsyncDisposeContinue reaction handler function object.
fn alloc_continue(
  h: state.Heap(host),
  b: common.Builtins,
  rest: List(DisposeResource),
  pending: Option(JsValue),
  resolve: JsValue,
  reject: JsValue,
  is_reject is_reject: Bool,
) -> #(state.Heap(host), Ref) {
  common.alloc_wrapper(
    h,
    value.NativeFunction(
      Dispatch(
        DisposableStackNative(AsyncDisposeContinue(
          remaining: rest,
          pending:,
          resolve:,
          reject:,
          is_reject:,
        )),
      ),
      constructible: False,
    ),
    b.function.prototype,
  )
}

/// Reaction handler resuming the async dispose loop after an Await settles.
/// A rejected await folds the rejection reason into the pending completion
/// (DisposeResources step 3.b.iii applied to the Await result).
fn async_dispose_continue(
  args: List(JsValue),
  state: State(host),
  remaining: List(DisposeResource),
  pending: Option(JsValue),
  resolve: JsValue,
  reject: JsValue,
  is_reject: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, pending) = case is_reject {
    True -> fold_error(state, pending, helpers.first_arg_or_undefined(args))
    False -> #(state, pending)
  }
  let state =
    async_dispose_loop(
      state,
      remaining,
      pending:,
      // An await just completed: hasAwaited is true from here on.
      needs_await: False,
      has_awaited: True,
      resolve:,
      reject:,
    )
  #(state, Ok(JsUndefined))
}

/// Call the promise capability's resolve/reject function with one argument.
/// The capability is always `NewPromiseCapability(%Promise%)` — its intrinsic
/// resolving functions never throw (§27.2.1.3).
fn settle_capability(
  state: State(host),
  fun: JsValue,
  arg: JsValue,
) -> State(host) {
  let assert Ok(#(_val, state)) = state.call(state, fun, JsUndefined, [arg])
    as "disposable_stack: intrinsic resolving function threw"
  state
}
