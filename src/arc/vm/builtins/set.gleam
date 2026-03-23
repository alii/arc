/// ES2024 §24.2 Set Objects
///
/// A Set is a collection of unique values. Key equality follows the
/// SameValueZero algorithm (NaN === NaN, +0 === -0).
///
/// Stores values in a Dict(MapKey, JsValue) + List(MapKey) for insertion order.
/// The dict maps normalized MapKey → original JsValue.
/// The keys list preserves insertion order for forEach.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg}
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type MapKey, type Ref, type SetNativeFn, AccessorProperty,
  ArrayObject, Dispatch, Finite, JsBool, JsNull, JsNumber, JsObject, JsUndefined,
  ObjectSlot, SetConstructor, SetNative, SetObject, SetPrototypeAdd,
  SetPrototypeClear, SetPrototypeDelete, SetPrototypeDifference,
  SetPrototypeEntries, SetPrototypeForEach, SetPrototypeGetSize, SetPrototypeHas,
  SetPrototypeIntersection, SetPrototypeIsDisjointFrom, SetPrototypeIsSubsetOf,
  SetPrototypeIsSupersetOf, SetPrototypeSymmetricDifference, SetPrototypeUnion,
  SetPrototypeValues,
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
      #("union", SetNative(SetPrototypeUnion), 1),
      #("intersection", SetNative(SetPrototypeIntersection), 1),
      #("difference", SetNative(SetPrototypeDifference), 1),
      #("symmetricDifference", SetNative(SetPrototypeSymmetricDifference), 1),
      #("isSubsetOf", SetNative(SetPrototypeIsSubsetOf), 1),
      #("isSupersetOf", SetNative(SetPrototypeIsSupersetOf), 1),
      #("isDisjointFrom", SetNative(SetPrototypeIsDisjointFrom), 1),
      #("values", SetNative(SetPrototypeValues), 0),
      #("entries", SetNative(SetPrototypeEntries), 0),
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

  let #(h, bt) =
    common.init_type(
      h,
      object_proto,
      function_proto,
      proto_props,
      fn(proto) { Dispatch(SetNative(SetConstructor(proto:))) },
      "Set",
      0,
      [],
    )
  // §24.2.3.16 Set.prototype [ @@toStringTag ] = "Set"
  let h =
    common.add_symbol_property(
      h,
      bt.prototype,
      value.symbol_to_string_tag,
      value.builtin_property(value.JsString("Set")),
    )
  #(h, bt)
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
    SetPrototypeUnion -> set_union(this, args, state)
    SetPrototypeIntersection -> set_intersection(this, args, state)
    SetPrototypeDifference -> set_difference(this, args, state)
    SetPrototypeSymmetricDifference ->
      set_symmetric_difference(this, args, state)
    SetPrototypeIsSubsetOf -> set_is_subset_of(this, args, state)
    SetPrototypeIsSupersetOf -> set_is_superset_of(this, args, state)
    SetPrototypeIsDisjointFrom -> set_is_disjoint_from(this, args, state)
    SetPrototypeValues -> set_values(this, state)
    SetPrototypeEntries -> set_entries(this, state)
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
      heap.read_array(state.heap, ref)
      |> option.map(fn(p) { read_array_elements(p.1, 0, p.0, []) })
      |> option.unwrap([])
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
        elements: elements.new(),
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
      let val = elements.get(elements, idx)
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
  use data, keys, ref, state <- require_set(this, state)
  let val = first_arg(args)
  let key = value.js_to_map_key(val)
  let new_data = dict.insert(data, key, val)
  let new_keys = case dict.has_key(data, key) {
    True -> keys
    False -> list.append(keys, [key])
  }
  let heap = update_set(state.heap, ref, new_data, new_keys)
  #(State(..state, heap:), Ok(this))
}

/// ES2024 §24.2.3.4 Set.prototype.has ( value )
fn set_has(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _keys, _ref, state <- require_set(this, state)
  let key = value.js_to_map_key(first_arg(args))
  #(state, Ok(JsBool(dict.has_key(data, key))))
}

/// ES2024 §24.2.3.3 Set.prototype.delete ( value )
fn set_delete(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, ref, state <- require_set(this, state)
  let key = value.js_to_map_key(first_arg(args))
  let had = dict.has_key(data, key)
  let new_data = dict.delete(data, key)
  let new_keys = list.filter(keys, fn(k) { k != key })
  let heap = update_set(state.heap, ref, new_data, new_keys)
  #(State(..state, heap:), Ok(JsBool(had)))
}

/// ES2024 §24.2.3.2 Set.prototype.clear ()
fn set_clear(this: JsValue, state: State) -> #(State, Result(JsValue, JsValue)) {
  use _data, _keys, ref, state <- require_set(this, state)
  let heap = update_set(state.heap, ref, dict.new(), [])
  #(State(..state, heap:), Ok(JsUndefined))
}

/// ES2024 §24.2.3.5 get Set.prototype.size
fn set_size(this: JsValue, state: State) -> #(State, Result(JsValue, JsValue)) {
  use data, _keys, _ref, state <- require_set(this, state)
  #(state, Ok(JsNumber(Finite(int.to_float(dict.size(data))))))
}

/// ES2024 §24.2.3.6 Set.prototype.forEach ( callbackfn [ , thisArg ] )
fn set_for_each(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  let callback = first_arg(args)
  let this_arg = case args {
    [_, ta, ..] -> ta
    _ -> JsUndefined
  }
  case helpers.is_callable(state.heap, callback) {
    False ->
      state.type_error(
        state,
        "Set.prototype.forEach callback is not a function",
      )
    True -> {
      let entries =
        list.filter_map(keys, fn(key) {
          dict.get(data, key) |> result.map(fn(v) { #(key, v) })
        })
      for_each_loop(state, entries, callback, this_arg, this)
    }
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
    [#(_key, val), ..rest] ->
      case state.call(state, callback, this_arg, [val, val, set_this]) {
        Ok(#(_result, new_state)) ->
          for_each_loop(new_state, rest, callback, this_arg, set_this)
        Error(#(thrown, new_state)) -> #(new_state, Error(thrown))
      }
  }
}

/// ES2025 §24.2.3.14 Set.prototype.union ( other )
fn set_union(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  case require_set_like(first_arg(args), state) {
    Error(r) -> r
    Ok(#(other_data, other_keys, state)) -> {
      // Start with this set's entries, then add entries from other.
      // Accumulate new keys by prepending (O(1)), reverse once at the end.
      let #(result_data, new_keys_rev) =
        list.fold(other_keys, #(data, []), fn(acc, key) {
          let #(d, ks) = acc
          case dict.has_key(d, key) {
            True -> #(d, ks)
            False ->
              case dict.get(other_data, key) {
                Ok(v) -> #(dict.insert(d, key, v), [key, ..ks])
                Error(Nil) -> #(d, ks)
              }
          }
        })
      let result_keys = list.append(keys, list.reverse(new_keys_rev))
      alloc_new_set(state, result_data, result_keys)
    }
  }
}

/// ES2025 §24.2.3.7 Set.prototype.intersection ( other )
fn set_intersection(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  case require_set_like(first_arg(args), state) {
    Error(r) -> r
    Ok(#(other_data, _other_keys, state)) -> {
      let #(result_data, result_keys) =
        list.fold(keys, #(dict.new(), []), fn(acc, key) {
          let #(d, ks) = acc
          case dict.has_key(other_data, key) {
            True ->
              case dict.get(data, key) {
                Ok(v) -> #(dict.insert(d, key, v), [key, ..ks])
                Error(Nil) -> #(d, ks)
              }
            False -> #(d, ks)
          }
        })
      alloc_new_set(state, result_data, list.reverse(result_keys))
    }
  }
}

/// ES2025 §24.2.3.3 Set.prototype.difference ( other )
fn set_difference(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  case require_set_like(first_arg(args), state) {
    Error(r) -> r
    Ok(#(other_data, _other_keys, state)) -> {
      let #(result_data, result_keys) =
        list.fold(keys, #(dict.new(), []), fn(acc, key) {
          let #(d, ks) = acc
          case dict.has_key(other_data, key) {
            False ->
              case dict.get(data, key) {
                Ok(v) -> #(dict.insert(d, key, v), [key, ..ks])
                Error(Nil) -> #(d, ks)
              }
            True -> #(d, ks)
          }
        })
      alloc_new_set(state, result_data, list.reverse(result_keys))
    }
  }
}

/// ES2025 §24.2.3.13 Set.prototype.symmetricDifference ( other )
fn set_symmetric_difference(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  case require_set_like(first_arg(args), state) {
    Error(r) -> r
    Ok(#(other_data, other_keys, state)) -> {
      // Start with elements in this but not other
      let #(result_data, result_keys) =
        list.fold(keys, #(dict.new(), []), fn(acc, key) {
          let #(d, ks) = acc
          case dict.has_key(other_data, key) {
            False ->
              case dict.get(data, key) {
                Ok(v) -> #(dict.insert(d, key, v), [key, ..ks])
                Error(Nil) -> #(d, ks)
              }
            True -> #(d, ks)
          }
        })
      // Add elements in other but not this
      let #(result_data, result_keys) =
        list.fold(other_keys, #(result_data, result_keys), fn(acc, key) {
          let #(d, ks) = acc
          case dict.has_key(data, key) {
            False ->
              case dict.get(other_data, key) {
                Ok(v) -> #(dict.insert(d, key, v), [key, ..ks])
                Error(Nil) -> #(d, ks)
              }
            True -> #(d, ks)
          }
        })
      alloc_new_set(state, result_data, list.reverse(result_keys))
    }
  }
}

/// ES2025 §24.2.3.9 Set.prototype.isSubsetOf ( other )
fn set_is_subset_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  case require_set_like(first_arg(args), state) {
    Error(r) -> r
    Ok(#(other_data, _other_keys, state)) -> {
      let is_subset =
        list.all(keys, fn(key) {
          dict.has_key(data, key) && dict.has_key(other_data, key)
        })
      #(state, Ok(JsBool(is_subset)))
    }
  }
}

/// ES2025 §24.2.3.10 Set.prototype.isSupersetOf ( other )
fn set_is_superset_of(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _keys, _ref, state <- require_set(this, state)
  case require_set_like(first_arg(args), state) {
    Error(r) -> r
    Ok(#(_other_data, other_keys, state)) -> {
      let is_superset =
        list.all(other_keys, fn(key) { dict.has_key(data, key) })
      #(state, Ok(JsBool(is_superset)))
    }
  }
}

/// ES2025 §24.2.3.8 Set.prototype.isDisjointFrom ( other )
fn set_is_disjoint_from(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  case require_set_like(first_arg(args), state) {
    Error(r) -> r
    Ok(#(other_data, _other_keys, state)) -> {
      let is_disjoint =
        list.all(keys, fn(key) {
          !{ dict.has_key(data, key) && dict.has_key(other_data, key) }
        })
      #(state, Ok(JsBool(is_disjoint)))
    }
  }
}

/// ES2024 §24.2.3.15 Set.prototype.values ()
/// Returns an array of the set's values (simplified — no iterator protocol).
fn set_values(this: JsValue, state: State) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  let values =
    list.filter_map(keys, fn(key) {
      dict.get(data, key) |> result.replace_error(Nil)
    })
  let #(heap, arr_ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: ArrayObject(list.length(values)),
        properties: dict.new(),
        elements: elements.from_list(values),
        prototype: None,
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(arr_ref)))
}

/// ES2024 §24.2.3.4 Set.prototype.entries ()
/// Returns an array of [value, value] pairs (simplified — no iterator protocol).
fn set_entries(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, keys, _ref, state <- require_set(this, state)
  let #(heap, entries) =
    list.fold(keys, #(state.heap, []), fn(acc, key) {
      let #(h, entries) = acc
      case dict.get(data, key) {
        Ok(v) -> {
          let #(h, pair_ref) =
            heap.alloc(
              h,
              ObjectSlot(
                kind: ArrayObject(2),
                properties: dict.new(),
                elements: elements.from_list([v, v]),
                prototype: None,
                symbol_properties: dict.new(),
                extensible: True,
              ),
            )
          #(h, [JsObject(pair_ref), ..entries])
        }
        Error(Nil) -> #(h, entries)
      }
    })
  let #(heap, arr_ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: ArrayObject(list.length(entries)),
        properties: dict.new(),
        elements: elements.from_list(list.reverse(entries)),
        prototype: None,
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(arr_ref)))
}

/// Allocate a new Set object from data + keys.
fn alloc_new_set(
  state: State,
  data: dict.Dict(MapKey, JsValue),
  keys: List(MapKey),
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, ref) =
    heap.alloc(
      state.heap,
      ObjectSlot(
        kind: SetObject(data:, keys:),
        properties: dict.new(),
        elements: elements.new(),
        prototype: None,
        symbol_properties: dict.new(),
        extensible: True,
      ),
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// Extract Set data from a value, or treat it as set-like if it has .has and .size.
/// For now, only supports actual Set objects.
fn require_set_like(
  val: JsValue,
  state: State,
) -> Result(
  #(dict.Dict(MapKey, JsValue), List(MapKey), State),
  #(State, Result(JsValue, JsValue)),
) {
  case val {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, keys:), ..)) ->
          Ok(#(data, keys, state))
        _ -> Error(state.type_error(state, "The .has method is not callable"))
      }
    _ -> Error(state.type_error(state, "The .has method is not callable"))
  }
}

// ---- helpers ----

/// Unwrap `this` as a Set or return a TypeError.
/// CPS-style — call with `use data, keys, ref, state <- require_set(this, state)`.
fn require_set(
  this: JsValue,
  state: State,
  cont: fn(dict.Dict(MapKey, JsValue), List(MapKey), Ref, State) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let err = "Method Set.prototype.* called on incompatible receiver"
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: SetObject(data:, keys:), ..)) ->
          cont(data, keys, ref, state)
        _ -> state.type_error(state, err)
      }
    _ -> state.type_error(state, err)
  }
}
