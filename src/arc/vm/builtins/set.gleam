/// ES2024 §24.2 Set Objects
///
/// A Set is a collection of unique values. Key equality follows the
/// SameValueZero algorithm (NaN === NaN, +0 === -0).
///
/// Stores values in a Dict(MapKey, JsValue) + List(MapKey) for insertion order.
/// The dict maps normalized MapKey → original JsValue.
/// The keys list preserves insertion order for forEach.
import arc/vm/array as vm_array
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type MapKey, type Ref, type SetNativeFn, AccessorProperty,
  Finite, JsBool, JsNull, JsNumber, JsObject, JsUndefined, ObjectSlot,
  SetConstructor, SetNative, SetObject, SetPrototypeAdd, SetPrototypeClear,
  SetPrototypeDelete, SetPrototypeForEach, SetPrototypeGetSize, SetPrototypeHas,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

/// Set up Set.prototype and Set constructor.
pub fn init(
  h: Heap,
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType) {
  // Allocate prototype methods
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("add", SetNative(SetPrototypeAdd), 1),
      #("has", SetNative(SetPrototypeHas), 1),
      #("delete", SetNative(SetPrototypeDelete), 1),
      #("clear", SetNative(SetPrototypeClear), 0),
      #("forEach", SetNative(SetPrototypeForEach), 1),
    ])

  // Allocate the size getter function
  let #(h, size_getter_ref) =
    common.alloc_native_fn(
      h,
      function_proto,
      SetNative(SetPrototypeGetSize),
      "get size",
      0,
    )

  // Add the size accessor property to prototype methods
  let proto_props = [
    #(
      "size",
      AccessorProperty(
        get: Some(JsObject(size_getter_ref)),
        set: None,
        enumerable: False,
        configurable: True,
      ),
    ),
    ..proto_methods
  ]

  common.init_type(
    h,
    object_proto,
    function_proto,
    proto_props,
    fn(proto) { SetNative(SetConstructor(proto:)) },
    "Set",
    0,
    [],
  )
}

/// Per-module dispatch for Set native functions.
pub fn dispatch(
  native: SetNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    SetConstructor(proto:) -> construct(proto, args, state)
    SetPrototypeAdd -> set_add(this, args, state)
    SetPrototypeHas -> set_has(this, args, state)
    SetPrototypeDelete -> set_delete(this, args, state)
    SetPrototypeClear -> set_clear(this, state)
    SetPrototypeForEach -> set_for_each(this, args, state)
    SetPrototypeGetSize -> set_size(this, state)
  }
}

/// ES2024 §24.2.1.1 Set ( [ iterable ] )
fn construct(
  set_proto: Ref,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // Gather initial values from array argument
  let initial_values = case args {
    [] -> []
    [JsUndefined, ..] -> []
    [JsNull, ..] -> []
    [JsObject(ref), ..] ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.ArrayObject(length:), elements:, ..)) ->
          read_array_elements(elements, 0, length, [])
        _ -> []
      }
    _ -> []
  }

  // Build data dict and keys list
  let #(data, keys) =
    list.fold(initial_values, #(dict.new(), []), fn(acc, val) {
      let #(d, ks) = acc
      let key = value.js_to_map_key(val)
      case dict.has_key(d, key) {
        True -> #(dict.insert(d, key, val), ks)
        False -> #(dict.insert(d, key, val), [key, ..ks])
      }
    })
  let keys = list.reverse(keys)

  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: SetObject(data:, keys:),
        properties: dict.new(),
        elements: js_elements.new(),
        prototype: Some(set_proto),
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Read elements from array into a list.
fn read_array_elements(
  elements: value.JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let val = case elements {
        value.DenseElements(data) ->
          case vm_array.get(idx, data) {
            Some(v) -> v
            None -> JsUndefined
          }
        value.SparseElements(data) ->
          case dict.get(data, idx) {
            Ok(v) -> v
            Error(Nil) -> JsUndefined
          }
      }
      read_array_elements(elements, idx + 1, length, [val, ..acc])
    }
  }
}

/// Helper to update a SetObject's data on the heap.
fn update_set(
  h: Heap,
  ref: Ref,
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
) -> Heap {
  heap.update(h, ref, fn(slot) {
    case slot {
      ObjectSlot(
        properties:,
        elements:,
        prototype:,
        symbol_properties:,
        extensible:,
        ..,
      ) ->
        ObjectSlot(
          kind: SetObject(data:, keys:),
          properties:,
          elements:,
          prototype:,
          symbol_properties:,
          extensible:,
        )
      other -> other
    }
  })
}

/// ES2024 §24.2.3.1 Set.prototype.add ( value )
fn set_add(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, keys:), ..)) -> {
          let key = value.js_to_map_key(val)
          let already_has = dict.has_key(data, key)
          let new_data = dict.insert(data, key, val)
          let new_keys = case already_has {
            True -> keys
            False -> list.append(keys, [key])
          }
          let heap = update_set(state.heap, ref, new_data, new_keys)
          #(State(..state, heap:), Ok(this))
        }
        _ ->
          frame.type_error(
            state,
            "Set.prototype.add requires that 'this' be a Set",
          )
      }
    _ ->
      frame.type_error(state, "Set.prototype.add requires that 'this' be a Set")
  }
}

/// ES2024 §24.2.3.4 Set.prototype.has ( value )
fn set_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, ..), ..)) -> {
          let key = value.js_to_map_key(val)
          #(state, Ok(JsBool(dict.has_key(data, key))))
        }
        _ ->
          frame.type_error(
            state,
            "Set.prototype.has requires that 'this' be a Set",
          )
      }
    _ ->
      frame.type_error(state, "Set.prototype.has requires that 'this' be a Set")
  }
}

/// ES2024 §24.2.3.3 Set.prototype.delete ( value )
fn set_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = case args {
    [v, ..] -> v
    [] -> JsUndefined
  }
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, keys:), ..)) -> {
          let key = value.js_to_map_key(val)
          let had = dict.has_key(data, key)
          let new_data = dict.delete(data, key)
          let new_keys = list.filter(keys, fn(k) { k != key })
          let heap = update_set(state.heap, ref, new_data, new_keys)
          #(State(..state, heap:), Ok(JsBool(had)))
        }
        _ ->
          frame.type_error(
            state,
            "Set.prototype.delete requires that 'this' be a Set",
          )
      }
    _ ->
      frame.type_error(
        state,
        "Set.prototype.delete requires that 'this' be a Set",
      )
  }
}

/// ES2024 §24.2.3.2 Set.prototype.clear ()
fn set_clear(this: JsValue, state: State) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(..), ..)) -> {
          let heap = update_set(state.heap, ref, dict.new(), [])
          #(State(..state, heap:), Ok(JsUndefined))
        }
        _ ->
          frame.type_error(
            state,
            "Set.prototype.clear requires that 'this' be a Set",
          )
      }
    _ ->
      frame.type_error(
        state,
        "Set.prototype.clear requires that 'this' be a Set",
      )
  }
}

/// ES2024 §24.2.3.5 get Set.prototype.size
fn set_size(this: JsValue, state: State) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, ..), ..)) -> {
          let count = dict.size(data)
          #(state, Ok(JsNumber(Finite(int.to_float(count)))))
        }
        _ ->
          frame.type_error(
            state,
            "Set.prototype.size requires that 'this' be a Set",
          )
      }
    _ ->
      frame.type_error(
        state,
        "Set.prototype.size requires that 'this' be a Set",
      )
  }
}

/// ES2024 §24.2.3.6 Set.prototype.forEach ( callbackfn [ , thisArg ] )
fn set_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let callback = case args {
    [cb, ..] -> cb
    [] -> JsUndefined
  }
  let this_arg = case args {
    [_, ta, ..] -> ta
    _ -> JsUndefined
  }

  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, keys:), ..)) -> {
          case is_callable(state.heap, callback) {
            False ->
              frame.type_error(
                state,
                "Set.prototype.forEach callback is not a function",
              )
            True -> {
              // Get values in insertion order
              let entries =
                list.filter_map(keys, fn(key) {
                  dict.get(data, key) |> result.map(fn(v) { #(key, v) })
                })
              for_each_loop(state, entries, callback, this_arg, this)
            }
          }
        }
        _ ->
          frame.type_error(
            state,
            "Set.prototype.forEach requires that 'this' be a Set",
          )
      }
    _ ->
      frame.type_error(
        state,
        "Set.prototype.forEach requires that 'this' be a Set",
      )
  }
}

/// Iterate over Set entries, calling callback(value, value, set) for each.
fn for_each_loop(
  state: State,
  entries: List(#(MapKey, JsValue)),
  callback: JsValue,
  this_arg: JsValue,
  set_this: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  case entries {
    [] -> #(state, Ok(JsUndefined))
    [#(_key, val), ..rest] -> {
      case frame.call(state, callback, this_arg, [val, val, set_this]) {
        Ok(#(_result, new_state)) ->
          for_each_loop(new_state, rest, callback, this_arg, set_this)
        Error(#(thrown, new_state)) -> #(new_state, Error(thrown))
      }
    }
  }
}

/// Check if a value is callable.
fn is_callable(h: Heap, val: JsValue) -> Bool {
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(kind: value.FunctionObject(..), ..)) -> True
        Some(ObjectSlot(kind: value.NativeFunction(_), ..)) -> True
        _ -> False
      }
    _ -> False
  }
}
