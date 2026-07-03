/// ES2024 §24.4 WeakSet Objects
///
/// A WeakSet is a collection of values for which CanBeHeldWeakly is true:
/// objects, or non-registered Symbols. Members are stored as canonical
/// JsValues — `JsObject(ref)` compares by object identity, `JsSymbol(id)`
/// by symbol identity — exactly like WeakMap keys.
/// Not truly weak (GC doesn't collect entries) but API-compatible.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{
  can_be_held_weakly, first_arg_or_undefined, is_callable,
}
import arc/vm/builtins/iterator
import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakSetNativeFn, Dispatch, JsBool, JsNull,
  JsObject, JsUndefined, ObjectSlot, WeakSetConstructor, WeakSetNative,
  WeakSetObject, WeakSetPrototypeAdd, WeakSetPrototypeDelete,
  WeakSetPrototypeHas,
}
import gleam/dict.{type Dict}
import gleam/option.{None, Some}

/// Set up WeakSet.prototype and WeakSet constructor.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("add", WeakSetNative(WeakSetPrototypeAdd), 1),
      #("has", WeakSetNative(WeakSetPrototypeHas), 1),
      #("delete", WeakSetNative(WeakSetPrototypeDelete), 1),
    ])

  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(proto) { Dispatch(WeakSetNative(WeakSetConstructor(proto:))) },
      "WeakSet",
      0,
      [],
    )
  let h = common.add_to_string_tag(h, bt.prototype, "WeakSet")
  #(h, bt)
}

/// Per-module dispatch for WeakSet native functions.
pub fn dispatch(
  native: WeakSetNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    WeakSetConstructor(proto:) -> construct(proto, args, state)
    WeakSetPrototypeAdd -> weak_set_add(this, args, state)
    WeakSetPrototypeHas -> weak_set_has(this, args, state)
    WeakSetPrototypeDelete -> weak_set_delete(this, args, state)
  }
}

/// ES2024 §24.4.1.1 WeakSet ( [ iterable ] )
///
///   1. If NewTarget is undefined, throw a TypeError exception.
///   2. Let set be ? OrdinaryCreateFromConstructor(NewTarget,
///      "%WeakSet.prototype%", « [[WeakSetData]] »).
///   3. Set set.[[WeakSetData]] to a new empty List.
///   4. If iterable is undefined or null, return set.
///   5-6. adder = ? Get(set, "add"); must be callable.
///   7. Iterate the iterable, calling adder(set, v) for each value.
fn construct(
  proto: Ref,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Steps 1-2: reject a plain call and resolve new.target.prototype.
  use proto, state <- helpers.require_new_target(state, "WeakSet", proto)
  let #(heap, set_ref) =
    common.alloc_wrapper(state.heap, WeakSetObject(data: dict.new()), proto)
  let state = State(..state, heap:)
  let set = JsObject(set_ref)
  case first_arg_or_undefined(args) {
    // Step 4: If iterable is undefined or null, return set.
    JsUndefined | JsNull -> #(state, Ok(set))
    iterable -> {
      // Steps 5-6: adder = ? Get(set, "add"); must be callable.
      use adder, state <- state.try_op(object.get_value_of(
        state,
        set,
        Named("add"),
      ))
      case is_callable(state.heap, adder) {
        False ->
          state.type_error(state, "'add' property of WeakSet is not a function")
        // Step 7: iterate the iterable's values, calling adder(set, v) for
        // each one and closing the iterator on any abrupt completion.
        True -> iterator.add_values_from_iterable(state, set, iterable, adder)
      }
    }
  }
}

/// RequireInternalSlot(this, [[WeakSetData]]) — proves `this` is a WeakSet and
/// hands over its membership dict, or throws a TypeError naming `method`.
/// CPS-style — `use data, ref, state <- require_weak_set(this, state, "add")`.
fn require_weak_set(
  this: JsValue,
  state: State(host),
  method: String,
  cont: fn(Dict(JsValue, Nil), Ref, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let found = case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakSetObject(data:), ..)) -> Some(#(data, ref))
        _ -> None
      }
    _ -> None
  }
  case found {
    Some(#(data, ref)) -> cont(data, ref, state)
    None ->
      state.type_error(
        state,
        "Method WeakSet.prototype."
          <> method
          <> " called on incompatible receiver",
      )
  }
}

/// ES2024 §24.4.3.1 WeakSet.prototype.add ( value )
/// Same §16.1 CanBeHeldWeakly gate as WeakMap keys: objects and
/// non-registered Symbols are valid members, everything else is a TypeError.
fn weak_set_add(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _data, ref, state <- require_weak_set(this, state, "add")
  let val = first_arg_or_undefined(args)
  case can_be_held_weakly(state, val) {
    True -> {
      let state = mutate(state, ref, dict.insert(_, val, Nil))
      #(state, Ok(this))
    }
    False -> state.type_error(state, "Invalid value used in weak set")
  }
}

/// ES2024 §24.4.3.3 WeakSet.prototype.has ( value )
/// Non-weakly-holdable values are never present, so this is a plain lookup.
fn weak_set_has(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, _ref, state <- require_weak_set(this, state, "has")
  let val = first_arg_or_undefined(args)
  #(state, Ok(JsBool(dict.has_key(data, val))))
}

/// ES2024 §24.4.3.2 WeakSet.prototype.delete ( value )
fn weak_set_delete(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use data, ref, state <- require_weak_set(this, state, "delete")
  let val = first_arg_or_undefined(args)
  case dict.has_key(data, val) {
    True -> {
      let state = mutate(state, ref, dict.delete(_, val))
      #(state, Ok(JsBool(True)))
    }
    False -> #(state, Ok(JsBool(False)))
  }
}

/// Read-modify-write the WeakSet's membership dict inside a single heap access.
/// `ref` must have been proved a WeakSet by `require_weak_set`.
///
/// Takes a *function* rather than a finished dict on purpose: a caller cannot
/// hand back a dict it captured before running user code, so a stale write can
/// never silently revert a re-entrant mutation.
fn mutate(
  state: State(host),
  ref: Ref,
  update: fn(Dict(JsValue, Nil)) -> Dict(JsValue, Nil),
) -> State(host) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(kind: WeakSetObject(data:), ..)) -> {
      let heap =
        heap.update_kind(state.heap, ref, WeakSetObject(data: update(data)))
      State(..state, heap:)
    }
    // Unreachable: a heap slot's kind never changes after allocation.
    _ -> state
  }
}
