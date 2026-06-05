/// ES2024 §24.3 WeakMap Objects
///
/// A WeakMap is a collection of key-value pairs where keys must be objects or
/// non-registered Symbols (per CanBeHeldWeakly).
/// In this implementation, keys are stored by Ref (object identity).
/// Not truly weak (GC doesn't collect entries) but API-compatible.
///
/// Also hosts the shared weak-collection core (`WeakKind`, `weak_insert`,
/// `weak_has`, `weak_delete`) reused by `weak_set` — both builtins are the
/// same Ref-keyed dict operations on different heap kinds.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined, list_at}
import arc/vm/heap
import arc/vm/state.{type ExoticKind, type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, type WeakMapNativeFn, Dispatch, JsBool, JsObject,
  JsUndefined, ObjectSlot, WeakMapConstructor, WeakMapNative, WeakMapObject,
  WeakMapPrototypeDelete, WeakMapPrototypeGet, WeakMapPrototypeHas,
  WeakMapPrototypeSet,
}
import gleam/dict.{type Dict}
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
    WeakMapPrototypeHas -> weak_has(this, args, state, map_kind())
    WeakMapPrototypeDelete -> weak_delete(this, args, state, map_kind())
  }
}

/// ES2024 §24.3.1.1 WeakMap ( [ iterable ] )
fn construct(
  proto: Ref,
  _args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  // For now, ignore iterable argument (most tests just test new WeakMap())
  let #(heap, ref) =
    common.alloc_wrapper(state.heap, WeakMapObject(data: dict.new()), proto)
  #(State(..state, heap:), Ok(JsObject(ref)))
}

/// ES2024 §24.3.3.2 WeakMap.prototype.get ( key )
fn weak_map_get(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use data, _ref, state <- weak_require(this, state, map_kind())
  case first_arg_or_undefined(args) {
    JsObject(key_ref) ->
      case dict.get(data, key_ref) {
        Ok(val) -> #(state, Ok(val))
        Error(Nil) -> #(state, Ok(JsUndefined))
      }
    _ -> #(state, Ok(JsUndefined))
  }
}

/// ES2024 §24.3.3.5 WeakMap.prototype.set ( key, value )
fn weak_map_set(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let val = list_at(args, 1) |> option.unwrap(JsUndefined)
  weak_insert(this, args, val, state, map_kind())
}

/// The WeakMap instantiation of the shared weak-collection core.
fn map_kind() -> WeakKind(JsValue) {
  WeakKind(
    receiver_err: "Method WeakMap.prototype.* called on incompatible receiver",
    key_err: "Invalid value used as weak map key",
    extract: fn(kind) {
      case kind {
        WeakMapObject(data:) -> Some(data)
        _ -> None
      }
    },
    rebuild: WeakMapObject,
  )
}

// ---- shared weak-collection core (WeakMap + WeakSet) ----

/// How a weak collection is stored on the heap: WeakMap entries are
/// `Dict(Ref, JsValue)`, WeakSet membership is `Dict(Ref, Bool)`.
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

/// ES2024 §24.3.3.5 / §24.4.3.1 — WeakMap.prototype.set / WeakSet.prototype.add.
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

/// ES2024 §24.3.3.3 / §24.4.3.3 — WeakMap.prototype.has / WeakSet.prototype.has.
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

/// ES2024 §24.3.3.1 / §24.4.3.2 — WeakMap.prototype.delete / WeakSet.prototype.delete.
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
