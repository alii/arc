/// Symbol constructor and well-known symbol properties.
///
/// Symbol() is callable but NOT new-able (throws TypeError on `new Symbol()`).
/// Creates unique symbol values. Well-known symbols (Symbol.toStringTag, etc.)
/// are exposed as static properties on the Symbol function object.
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers
import arc/vm/heap.{type Heap}
import arc/vm/internal/elements
import arc/vm/ops/coerce
import arc/vm/state.{type State, RealmCtx, State}
import arc/vm/value.{
  type JsValue, type Ref, type SymbolNativeFn, JsObject, JsString, JsSymbol,
  JsUndefined, ObjectSlot, SymbolConstructor, SymbolDescriptionGetter, SymbolFor,
  SymbolKeyFor, SymbolNative, SymbolToPrimitive, SymbolToString, SymbolValueOf,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

/// Set up the Symbol constructor (with well-known symbol properties) and a
/// dedicated %Symbol.prototype% (§20.4.3) carrying toString/valueOf, the
/// `description` getter, @@toPrimitive and @@toStringTag.
pub fn init(
  h: Heap(ctx, host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(ctx, host), BuiltinType) {
  // Reserve the prototype ref first: the constructor's `prototype` property
  // and the prototype's `constructor` property point at each other.
  let #(h, prototype) = heap.reserve(h)
  let h = heap.root(h, prototype)

  // Allocate Symbol.for and Symbol.keyFor static method function objects
  let #(h, for_ref) =
    common.alloc_rooted_native_fn(h, function_proto, SymbolNative(SymbolFor), "for", 1)
  let #(h, key_for_ref) =
    common.alloc_rooted_native_fn(
      h,
      function_proto,
      SymbolNative(SymbolKeyFor),
      "keyFor",
      1,
    )
  // Symbol constructor function object with all properties pre-built.
  // §20.4.1: Symbol HAS [[Construct]] (it may appear in an `extends`
  // clause), so IsConstructor(Symbol) is true — but invoking it as a
  // constructor always throws (do_construct / super() handle that).
  let #(h, constructor) =
    common.alloc_native_fn_props(
      h,
      function_proto,
      SymbolNative(SymbolConstructor),
      constructible: True,
      props: [
        #("length", common.fn_length_property(0)),
        #("name", common.fn_name_property("Symbol")),
        #("prototype", common.fn_prototype_property(prototype)),
        #("for", value.builtin_property(JsObject(for_ref))),
        #("keyFor", value.builtin_property(JsObject(key_for_ref))),
        // Well-known symbol properties
        #("toStringTag", value.data(JsSymbol(value.symbol_to_string_tag))),
        #("iterator", value.data(JsSymbol(value.symbol_iterator))),
        #("hasInstance", value.data(JsSymbol(value.symbol_has_instance))),
        #(
          "isConcatSpreadable",
          value.data(JsSymbol(value.symbol_is_concat_spreadable)),
        ),
        #("toPrimitive", value.data(JsSymbol(value.symbol_to_primitive))),
        #("species", value.data(JsSymbol(value.symbol_species))),
        #("asyncIterator", value.data(JsSymbol(value.symbol_async_iterator))),
        #("match", value.data(JsSymbol(value.symbol_match))),
        #("matchAll", value.data(JsSymbol(value.symbol_match_all))),
        #("replace", value.data(JsSymbol(value.symbol_replace))),
        #("search", value.data(JsSymbol(value.symbol_search))),
        #("split", value.data(JsSymbol(value.symbol_split))),
        #("unscopables", value.data(JsSymbol(value.symbol_unscopables))),
        #("dispose", value.data(JsSymbol(value.symbol_dispose))),
        #("asyncDispose", value.data(JsSymbol(value.symbol_async_dispose))),
      ],
    )
  let h = heap.root(h, constructor)

  // %Symbol.prototype% methods (§20.4.3).
  let #(h, to_string_ref) =
    common.alloc_rooted_native_fn(
      h,
      function_proto,
      SymbolNative(SymbolToString),
      "toString",
      0,
    )
  let #(h, value_of_ref) =
    common.alloc_rooted_native_fn(
      h,
      function_proto,
      SymbolNative(SymbolValueOf),
      "valueOf",
      0,
    )
  // §20.4.3.5: @@toPrimitive { writable: false, enumerable: false,
  // configurable: true }, name "[Symbol.toPrimitive]", length 1.
  let #(h, to_primitive_ref) =
    common.alloc_rooted_native_fn(
      h,
      function_proto,
      SymbolNative(SymbolToPrimitive),
      "[Symbol.toPrimitive]",
      1,
    )
  // §20.4.3.2: get-only accessor `description`.
  let #(h, description_get_ref) =
    common.alloc_rooted_native_fn(
      h,
      function_proto,
      SymbolNative(SymbolDescriptionGetter),
      "get description",
      0,
    )
  let h =
    heap.write(
      h,
      prototype,
      ObjectSlot(
        kind: value.OrdinaryObject,
        properties: common.named_props([
          #("constructor", value.builtin_property(JsObject(constructor))),
          #("toString", value.builtin_property(JsObject(to_string_ref))),
          #("valueOf", value.builtin_property(JsObject(value_of_ref))),
          #(
            "description",
            value.accessor(
              get: Some(JsObject(description_get_ref)),
              set: None,
              enumerable: False,
              configurable: True,
            ),
          ),
        ]),
        elements: elements.new(),
        prototype: Some(object_proto),
        symbol_properties: [
          common.to_string_tag("Symbol"),
          #(
            value.symbol_to_primitive,
            value.data(JsObject(to_primitive_ref)) |> value.configurable(),
          ),
        ],
        extensible: True,
      ),
    )

  #(h, common.BuiltinType(constructor:, prototype:))
}

@external(erlang, "erlang", "make_ref")
fn make_ref() -> value.ErlangRef

/// Mint a new user symbol with the given [[Description]] (exposed for
/// Symbol.for, which stores the registry key as the description).
pub fn new_symbol(description: Option(String)) -> value.SymbolId {
  value.UserSymbol(make_ref(), description)
}

/// Per-module dispatch for Symbol native functions.
pub fn dispatch(
  native: SymbolNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    SymbolConstructor -> call_as_function(args, state)
    SymbolFor -> symbol_for(args, state)
    SymbolKeyFor -> symbol_key_for(args, state)
    SymbolToString -> to_string(this, state)
    SymbolValueOf -> this_symbol_result(state, this, "valueOf")
    // §20.4.3.5: @@toPrimitive ignores its hint argument entirely.
    SymbolToPrimitive -> this_symbol_result(state, this, "[Symbol.toPrimitive]")
    SymbolDescriptionGetter -> description_getter(this, state)
  }
}

/// §20.4.1.1 Symbol ( [ description ] ) — call semantics.
/// Step 1 (NewTarget throw) is handled in do_construct before dispatch.
/// Step 4 requires ToString(description) on any non-undefined argument, which
/// can run user code and throw — Symbol({toString(){throw x}}) must throw x.
fn call_as_function(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case helpers.first_arg_or_undefined(args) {
    JsUndefined -> #(state, Ok(JsSymbol(new_symbol(None))))
    desc -> {
      use s, state <- state.try_op(coerce.js_to_string(state, desc))
      #(state, Ok(JsSymbol(new_symbol(Some(s)))))
    }
  }
}

/// §20.4.2.2 Symbol.for ( key ) — global symbol registry lookup/insert.
fn symbol_for(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Step 1: Let stringKey be ? ToString(key).
  let key_val = helpers.first_arg_or_undefined(args)
  use key_str, state <- state.try_op(coerce.js_to_string(state, key_val))
  // Steps 2-4: Look up in GlobalSymbolRegistry, return existing or create new.
  case dict.get(state.ctx.symbol_registry, key_str) {
    Ok(existing_id) -> #(state, Ok(JsSymbol(existing_id)))
    Error(Nil) -> {
      // §20.4.2.2 step 4.a: the registered symbol's [[Description]] IS
      // the registry key.
      let id = new_symbol(Some(key_str))
      let new_registry = dict.insert(state.ctx.symbol_registry, key_str, id)
      let ctx = RealmCtx(..state.ctx, symbol_registry: new_registry)
      #(State(..state, ctx:), Ok(JsSymbol(id)))
    }
  }
}

/// §20.4.2.6 Symbol.keyFor ( sym ) — reverse lookup in the global registry.
fn symbol_key_for(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case args {
    [JsSymbol(id), ..] -> {
      let val =
        dict.to_list(state.ctx.symbol_registry)
        |> list.find(fn(pair) { pair.1 == id })
        |> option.from_result
        |> option.map(fn(pair) { JsString(pair.0) })
        |> option.unwrap(JsUndefined)
      #(state, Ok(val))
    }
    _ -> state.type_error(state, "Symbol.keyFor requires a Symbol argument")
  }
}

/// §20.4.3.3 Symbol.prototype.toString — SymbolDescriptiveString(thisSymbolValue).
fn to_string(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use id, state <- state.try_then(this_symbol_value(state, this, "toString"))
  #(state, Ok(JsString(value.symbol_descriptive_string(id))))
}

/// §20.4.3.2 get Symbol.prototype.description — [[Description]] or undefined.
fn description_getter(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use id, state <- state.try_then(this_symbol_value(state, this, "description"))
  let val =
    value.symbol_description(id)
    |> option.map(JsString)
    |> option.unwrap(JsUndefined)
  #(state, Ok(val))
}

/// valueOf / @@toPrimitive: return thisSymbolValue as a JsSymbol.
fn this_symbol_result(
  state: State(host),
  this: JsValue,
  method: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use id, state <- state.try_then(this_symbol_value(state, this, method))
  #(state, Ok(JsSymbol(id)))
}

/// §20.4.3 thisSymbolValue(value): a Symbol primitive, or a Symbol wrapper
/// object's [[SymbolData]]; anything else is a TypeError.
fn this_symbol_value(
  state: State(host),
  this: JsValue,
  method: String,
) -> #(State(host), Result(value.SymbolId, JsValue)) {
  case this {
    JsSymbol(id) -> #(state, Ok(id))
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.SymbolObject(value: id), ..)) -> #(
          state,
          Ok(id),
        )
        _ -> not_a_symbol(state, method)
      }
    _ -> not_a_symbol(state, method)
  }
}

fn not_a_symbol(
  state: State(host),
  method: String,
) -> #(State(host), Result(a, JsValue)) {
  state.type_error(
    state,
    "Symbol.prototype." <> method <> " requires that 'this' be a Symbol",
  )
}
