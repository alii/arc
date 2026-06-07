/// ES2024 §24.3 WeakMap Objects
///
/// A WeakMap is a collection of key-value pairs where keys must be values
/// for which CanBeHeldWeakly is true: objects, or non-registered Symbols.
/// Keys are stored as canonical JsValues — `JsObject(ref)` compares by
/// object identity, `JsSymbol(id)` by symbol identity.
/// Not truly weak (GC doesn't collect entries) but API-compatible.
///
/// Also hosts the shared weak-collection core (`WeakKind`, `weak_insert`,
/// `weak_has`, `weak_delete`) used by `weak_set` — Ref-keyed dict
/// operations parameterized over the heap kind.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined, is_callable, list_at}
import arc/vm/builtins/iterator
import arc/vm/heap
import arc/vm/ops/object
import arc/vm/state.{type ExoticKind, type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakMapNativeFn, Dispatch, Index, JsBool, JsNull,
  JsObject, JsSymbol, JsUndefined, Named, ObjectSlot, WeakMapConstructor,
  WeakMapNative, WeakMapObject, WeakMapPrototypeDelete, WeakMapPrototypeGet,
  WeakMapPrototypeGetOrInsert, WeakMapPrototypeGetOrInsertComputed,
  WeakMapPrototypeHas, WeakMapPrototypeSet,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

/// Set up WeakMap.prototype and WeakMap constructor.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("get", WeakMapNative(WeakMapPrototypeGet), 1),
      #("set", WeakMapNative(WeakMapPrototypeSet), 2),
      #("has", WeakMapNative(WeakMapPrototypeHas), 1),
      #("delete", WeakMapNative(WeakMapPrototypeDelete), 1),
      #("getOrInsert", WeakMapNative(WeakMapPrototypeGetOrInsert), 2),
      #(
        "getOrInsertComputed",
        WeakMapNative(WeakMapPrototypeGetOrInsertComputed),
        2,
      ),
    ])

  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_methods,
      fn(proto) { Dispatch(WeakMapNative(WeakMapConstructor(proto:))) },
      "WeakMap",
      0,
      [],
    )
  let h = common.add_to_string_tag(h, bt.prototype, "WeakMap")
  #(h, bt)
}

/// Per-module dispatch for WeakMap native functions.
pub fn dispatch(
  native: WeakMapNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    WeakMapConstructor(proto:) -> construct(proto, args, state)
    WeakMapPrototypeGet -> weak_map_get(this, args, state)
    WeakMapPrototypeSet -> weak_map_set(this, args, state)
    WeakMapPrototypeHas -> weak_map_has(this, args, state)
    WeakMapPrototypeDelete -> weak_map_delete(this, args, state)
    WeakMapPrototypeGetOrInsert -> get_or_insert(this, args, state)
    WeakMapPrototypeGetOrInsertComputed ->
      get_or_insert_computed(this, args, state)
  }
}

/// §16.1 CanBeHeldWeakly(v) — true for objects and non-registered Symbols
/// (a symbol minted by `Symbol.for` lives in the global registry and can
/// never be collected, so it can't be a weak key).
fn can_be_held_weakly(state: State, v: JsValue) -> Bool {
  case v {
    JsObject(_) -> True
    JsSymbol(id) -> !list.contains(dict.values(state.ctx.symbol_registry), id)
    _ -> False
  }
}

/// Unwrap `this` as a WeakMap, or throw TypeError. CPS-style — call with
/// `use data, ref, state <- map_require(this, state)`.
fn map_require(
  this: JsValue,
  state: State,
  cont: fn(Dict(JsValue, JsValue), Ref, State) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let found = case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: WeakMapObject(data:), ..)) -> Some(#(data, ref))
        _ -> None
      }
    _ -> None
  }
  case found {
    Some(#(data, ref)) -> cont(data, ref, state)
    None ->
      state.type_error(
        state,
        "Method WeakMap.prototype called on incompatible receiver",
      )
  }
}

/// ES2024 §24.3.1.1 WeakMap ( [ iterable ] )
fn construct(
  proto: Ref,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, map_ref) =
    common.alloc_wrapper(state.heap, WeakMapObject(data: dict.new()), proto)
  let state = State(..state, heap:)
  let map = JsObject(map_ref)
  case first_arg_or_undefined(args) {
    // Step 4: If iterable is undefined or null, return map.
    JsUndefined | JsNull -> #(state, Ok(map))
    iterable -> {
      // Steps 5-6: adder = ? Get(map, "set"); must be callable.
      use adder, state <- state.try_op(object.get_value_of(
        state,
        map,
        Named("set"),
      ))
      case is_callable(state.heap, adder) {
        False ->
          state.type_error(state, "'set' property of WeakMap is not a function")
        True -> add_entries_from_iterable(state, map, iterable, adder)
      }
    }
  }
}

/// §24.1.1.2 AddEntriesFromIterable(target, iterable, adder) — full
/// iterator protocol: GetIterator, then per entry Get "0"/"1" and call the
/// adder, closing the iterator on any abrupt completion inside the loop.
fn add_entries_from_iterable(
  state: State,
  target: JsValue,
  iterable: JsValue,
  adder: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  // §7.4.3 GetIterator(iterable, sync): @@iterator must be callable.
  use method, state <- state.try_op(object.get_symbol_value_of(
    state,
    iterable,
    value.symbol_iterator,
  ))
  case is_callable(state.heap, method) {
    False ->
      state.type_error(
        state,
        object.inspect(iterable, state.heap) <> " is not iterable",
      )
    True -> {
      use iter, state <- state.try_call(state, method, iterable, [])
      case iter {
        JsObject(_) -> {
          use next, state <- state.try_op(object.get_value_of(
            state,
            iter,
            Named("next"),
          ))
          add_entries_loop(state, target, iter, next, adder)
        }
        _ ->
          state.type_error(
            state,
            "Result of the Symbol.iterator method is not an object",
          )
      }
    }
  }
}

/// One IteratorStepValue + entry processing per iteration. Abrupt
/// completions from next()/Get(done)/Get(value) propagate without close
/// (§7.4.8 marks the iterator done); abrupt completions from the entry
/// reads or the adder call close the iterator first (§24.1.1.2 step 4).
fn add_entries_loop(
  state: State,
  target: JsValue,
  iter: JsValue,
  next: JsValue,
  adder: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  use step, state <- state.try_call(state, next, iter, [])
  case step {
    JsObject(_) -> {
      use done_val, state <- state.try_op(object.get_value_of(
        state,
        step,
        Named("done"),
      ))
      case value.is_truthy(done_val) {
        True -> #(state, Ok(target))
        False -> {
          use entry, state <- state.try_op(object.get_value_of(
            state,
            step,
            Named("value"),
          ))
          case entry {
            JsObject(_) -> {
              use k, state <- or_close(
                object.get_value_of(state, entry, Index(0)),
                iter,
              )
              use v, state <- or_close(
                object.get_value_of(state, entry, Index(1)),
                iter,
              )
              use _set_result, state <- or_close(
                state.call(state, adder, target, [k, v]),
                iter,
              )
              add_entries_loop(state, target, iter, next, adder)
            }
            _ ->
              iterator.close_throw_type(
                state,
                iter,
                "Iterator value "
                  <> object.inspect(entry, state.heap)
                  <> " is not an entry object",
              )
          }
        }
      }
    }
    _ -> state.type_error(state, "Iterator result is not an object")
  }
}

/// Unwrap an op result, or IteratorClose with the thrown error (original
/// error wins over any error from the close itself — §7.4.11).
fn or_close(
  res: Result(#(a, State), #(JsValue, State)),
  iter: JsValue,
  cont: fn(a, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case res {
    Ok(#(v, state)) -> cont(v, state)
    Error(#(thrown, state)) -> iterator.close_throw(state, iter, thrown)
  }
}

/// ES2024 §24.3.3.2 WeakMap.prototype.get ( key )
fn weak_map_get(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    True ->
      case dict.get(data, key) {
        Ok(val) -> #(state, Ok(val))
        Error(Nil) -> #(state, Ok(JsUndefined))
      }
    False -> #(state, Ok(JsUndefined))
  }
}

/// ES2024 §24.3.3.5 WeakMap.prototype.set ( key, value )
fn weak_map_set(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    True -> {
      let val = list_at(args, 1) |> option.unwrap(JsUndefined)
      let state = write_data(state, ref, dict.insert(data, key, val))
      #(state, Ok(this))
    }
    False -> state.type_error(state, "Invalid value used as weak map key")
  }
}

/// ES2024 §24.3.3.4 WeakMap.prototype.has ( key )
fn weak_map_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  #(state, Ok(JsBool(dict.has_key(data, key))))
}

/// ES2024 §24.3.3.3 WeakMap.prototype.delete ( key )
fn weak_map_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  case dict.has_key(data, key) {
    True -> {
      let state = write_data(state, ref, dict.delete(data, key))
      #(state, Ok(JsBool(True)))
    }
    False -> #(state, Ok(JsBool(False)))
  }
}

/// Upsert proposal — WeakMap.prototype.getOrInsert ( key, value )
fn get_or_insert(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  case can_be_held_weakly(state, key) {
    False -> state.type_error(state, "Invalid value used as weak map key")
    True ->
      case dict.get(data, key) {
        Ok(existing) -> #(state, Ok(existing))
        Error(Nil) -> {
          let val = list_at(args, 1) |> option.unwrap(JsUndefined)
          let state = write_data(state, ref, dict.insert(data, key, val))
          #(state, Ok(val))
        }
      }
  }
}

/// Upsert proposal — WeakMap.prototype.getOrInsertComputed ( key, callbackfn )
/// Validation order per spec: receiver, then IsCallable(callbackfn), then
/// CanBeHeldWeakly(key). After the callback runs the map is re-read so an
/// insert made by the callback for the same key is overwritten.
fn get_or_insert_computed(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _ref, state <- map_require(this, state)
  let key = first_arg_or_undefined(args)
  let callback = list_at(args, 1) |> option.unwrap(JsUndefined)
  case is_callable(state.heap, callback) {
    False ->
      state.type_error(
        state,
        object.inspect(callback, state.heap) <> " is not a function",
      )
    True ->
      case can_be_held_weakly(state, key) {
        False -> state.type_error(state, "Invalid value used as weak map key")
        True ->
          case dict.get(data, key) {
            Ok(existing) -> #(state, Ok(existing))
            Error(Nil) -> {
              use computed, state <- state.try_call(
                state,
                callback,
                JsUndefined,
                [key],
              )
              // The callback may have mutated the map — re-read and
              // overwrite any entry it inserted under this key.
              use data, ref, state <- map_require(this, state)
              let state =
                write_data(state, ref, dict.insert(data, key, computed))
              #(state, Ok(computed))
            }
          }
      }
  }
}

/// Write an updated entry dict back to the WeakMap's heap slot.
fn write_data(state: State, ref: Ref, data: Dict(JsValue, JsValue)) -> State {
  let heap = heap.update_kind(state.heap, ref, WeakMapObject(data:))
  State(..state, heap:)
}

// ---- shared weak-collection core (used by WeakSet) ----

/// How a weak collection is stored on the heap: WeakSet membership is
/// `Dict(Ref, Bool)` keyed by object Ref.
pub type WeakKind(v) {
  WeakKind(
    receiver_err: String,
    key_err: String,
    extract: fn(ExoticKind) -> Option(Dict(Ref, v)),
    rebuild: fn(Dict(Ref, v)) -> ExoticKind,
  )
}

/// Unwrap `this` as the weak collection recognized by `kind.extract`,
/// or return a TypeError. CPS-style — call with
/// `use data, ref, state <- weak_require(this, state, kind)`.
fn weak_require(
  this: JsValue,
  state: State,
  kind: WeakKind(v),
  cont: fn(Dict(Ref, v), Ref, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let found = case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: k, ..)) ->
          option.map(kind.extract(k), fn(data) { #(data, ref) })
        _ -> None
      }
    _ -> None
  }
  case found {
    Some(#(data, ref)) -> cont(data, ref, state)
    None -> state.type_error(state, kind.receiver_err)
  }
}

/// ES2024 §24.4.3.1 — WeakSet.prototype.add.
/// Inserts `val` under the first arg's Ref and returns `this`;
/// TypeError when the first arg is not an object.
pub fn weak_insert(
  this: JsValue,
  args: List(JsValue),
  val: v,
  state: State,
  kind: WeakKind(v),
) -> #(State, Result(JsValue, JsValue)) {
  use data, ref, state <- weak_require(this, state, kind)
  case first_arg_or_undefined(args) {
    JsObject(key_ref) -> {
      let data = dict.insert(data, key_ref, val)
      let heap = heap.update_kind(state.heap, ref, kind.rebuild(data))
      #(State(..state, heap:), Ok(this))
    }
    _ -> state.type_error(state, kind.key_err)
  }
}

/// ES2024 §24.4.3.3 — WeakSet.prototype.has.
pub fn weak_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
  kind: WeakKind(v),
) -> #(State, Result(JsValue, JsValue)) {
  use data, _ref, state <- weak_require(this, state, kind)
  case first_arg_or_undefined(args) {
    JsObject(key_ref) -> #(state, Ok(JsBool(dict.has_key(data, key_ref))))
    _ -> #(state, Ok(JsBool(False)))
  }
}

/// ES2024 §24.4.3.2 — WeakSet.prototype.delete.
pub fn weak_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
  kind: WeakKind(v),
) -> #(State, Result(JsValue, JsValue)) {
  use data, ref, state <- weak_require(this, state, kind)
  case first_arg_or_undefined(args) {
    JsObject(key_ref) -> {
      let had = dict.has_key(data, key_ref)
      let data = dict.delete(data, key_ref)
      let heap = heap.update_kind(state.heap, ref, kind.rebuild(data))
      #(State(..state, heap:), Ok(JsBool(had)))
    }
    _ -> #(state, Ok(JsBool(False)))
  }
}
