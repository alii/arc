/// ES2025 §27.1 Iteration — the Iterator constructor, Iterator.from, and
/// Iterator.prototype helper methods (map, filter, take, drop, flatMap,
/// toArray, forEach, reduce, some, every, find).
///
/// Prior art: QuickJS quickjs.c js_iterator_* (bellard/quickjs).
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined, is_callable}
import arc/vm/heap
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type IteratorHelperKind, type IteratorNativeFn, type JsValue, type Ref,
  Dispatch, Finite, HelperDrop, HelperFilter, HelperFlatMap, HelperMap,
  HelperTake, Infinity, IteratorConstructor, IteratorFrom, IteratorHelperNext,
  IteratorHelperObject, IteratorHelperReturn, IteratorNative,
  IteratorProtoGetConstructor, IteratorProtoGetToStringTag,
  IteratorProtoSetConstructor, IteratorProtoSetToStringTag,
  IteratorPrototypeDrop, IteratorPrototypeEvery, IteratorPrototypeFilter,
  IteratorPrototypeFind, IteratorPrototypeFlatMap, IteratorPrototypeForEach,
  IteratorPrototypeMap, IteratorPrototypeReduce, IteratorPrototypeSome,
  IteratorPrototypeTake, IteratorPrototypeToArray, JsBool, JsNull, JsObject,
  JsString, JsUndefined, NaN, Named, NegInfinity, ObjectSlot,
  WrapForValidIteratorNext, WrapForValidIteratorObject,
  WrapForValidIteratorReturn,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// ============================================================================
// Initialisation — wire up Iterator, %IteratorHelperPrototype%,
// %WrapForValidIteratorPrototype% onto the existing %IteratorPrototype%.
// ============================================================================

/// Set up the Iterator constructor, prototype methods, %IteratorHelperPrototype%
/// and %WrapForValidIteratorPrototype%. The base %IteratorPrototype% already
/// exists (allocated in builtins.gleam with [Symbol.iterator]() { return this }).
pub fn init(
  h: Heap,
  iterator_proto: Ref,
  function_proto: Ref,
) -> #(Heap, BuiltinType, Ref, Ref) {
  // Iterator.prototype methods — eager consumers + lazy producers.
  let #(h, proto_methods) =
    common.alloc_methods(h, function_proto, [
      #("map", IteratorNative(IteratorPrototypeMap), 1),
      #("filter", IteratorNative(IteratorPrototypeFilter), 1),
      #("take", IteratorNative(IteratorPrototypeTake), 1),
      #("drop", IteratorNative(IteratorPrototypeDrop), 1),
      #("flatMap", IteratorNative(IteratorPrototypeFlatMap), 1),
      #("toArray", IteratorNative(IteratorPrototypeToArray), 0),
      #("forEach", IteratorNative(IteratorPrototypeForEach), 1),
      #("reduce", IteratorNative(IteratorPrototypeReduce), 1),
      #("some", IteratorNative(IteratorPrototypeSome), 1),
      #("every", IteratorNative(IteratorPrototypeEvery), 1),
      #("find", IteratorNative(IteratorPrototypeFind), 1),
    ])

  // Iterator.from static method.
  let #(h, ctor_props) =
    common.alloc_methods(h, function_proto, [
      #("from", IteratorNative(IteratorFrom), 1),
    ])

  // Allocate constructor + merge proto methods onto the existing iterator_proto.
  let #(h, bt) =
    common.init_type_on(
      h,
      iterator_proto,
      function_proto,
      proto_methods,
      fn(_proto) { Dispatch(IteratorNative(IteratorConstructor)) },
      "Iterator",
      0,
      ctor_props,
    )

  // §27.1.3.2 Iterator.prototype.constructor and §27.1.3.13 [@@toStringTag] are
  // accessor properties (SetterThatIgnoresPrototypeProperties), not data props.
  // init_type_on already wrote a data .constructor — overwrite with accessor.
  let #(h, ctor_accessor) =
    common.alloc_get_set_accessor(
      h,
      function_proto,
      IteratorNative(IteratorProtoGetConstructor),
      IteratorNative(IteratorProtoSetConstructor),
      "constructor",
    )
  let #(h, tag_accessor) =
    common.alloc_get_set_accessor(
      h,
      function_proto,
      IteratorNative(IteratorProtoGetToStringTag),
      IteratorNative(IteratorProtoSetToStringTag),
      "[Symbol.toStringTag]",
    )
  let h =
    heap.update(h, iterator_proto, fn(slot) {
      case slot {
        ObjectSlot(properties:, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(
              properties,
              Named("constructor"),
              ctor_accessor,
            ),
          )
        other -> other
      }
    })
  let h =
    common.add_symbol_property(
      h,
      iterator_proto,
      value.symbol_to_string_tag,
      tag_accessor,
    )

  // %IteratorHelperPrototype% — ES2025 §27.1.3.2.1. Inherits from
  // %IteratorPrototype%, has next/return + @@toStringTag = "Iterator Helper".
  let #(h, helper_methods) =
    common.alloc_methods(h, function_proto, [
      #("next", IteratorNative(IteratorHelperNext), 0),
      #("return", IteratorNative(IteratorHelperReturn), 0),
    ])
  let #(h, helper_proto) =
    common.init_namespace(h, iterator_proto, "Iterator Helper", helper_methods)

  // %WrapForValidIteratorPrototype% — ES2025 §27.1.2.1.2. Inherits from
  // %IteratorPrototype%, has next/return.
  let #(h, wrap_methods) =
    common.alloc_methods(h, function_proto, [
      #("next", IteratorNative(WrapForValidIteratorNext), 0),
      #("return", IteratorNative(WrapForValidIteratorReturn), 0),
    ])
  let #(h, wrap_proto) =
    common.alloc_proto(
      h,
      Some(iterator_proto),
      common.named_props(wrap_methods),
    )

  #(h, bt, helper_proto, wrap_proto)
}

// ============================================================================
// Dispatch
// ============================================================================

pub fn dispatch(
  native: IteratorNativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    IteratorConstructor -> construct(this, state)
    IteratorFrom -> from(args, state)
    IteratorPrototypeMap -> lazy_helper(this, args, state, HelperMap, "map")
    IteratorPrototypeFilter ->
      lazy_helper(this, args, state, HelperFilter, "filter")
    IteratorPrototypeFlatMap ->
      lazy_helper(this, args, state, HelperFlatMap, "flatMap")
    IteratorPrototypeTake -> take_or_drop(this, args, state, HelperTake, "take")
    IteratorPrototypeDrop -> take_or_drop(this, args, state, HelperDrop, "drop")
    IteratorPrototypeToArray -> to_array(this, state)
    IteratorPrototypeForEach -> for_each(this, args, state)
    IteratorPrototypeReduce -> reduce(this, args, state)
    IteratorPrototypeSome -> bool_consumer(this, args, state, True, "some")
    IteratorPrototypeEvery -> bool_consumer(this, args, state, False, "every")
    IteratorPrototypeFind -> find(this, args, state)
    IteratorHelperNext -> helper_next(this, state)
    IteratorHelperReturn -> helper_return(this, state)
    WrapForValidIteratorNext -> wrap_next(this, state)
    WrapForValidIteratorReturn -> wrap_return(this, state)
    IteratorProtoGetToStringTag -> #(state, Ok(JsString("Iterator")))
    IteratorProtoGetConstructor -> #(
      state,
      Ok(JsObject(state.builtins.iterator.constructor)),
    )
    IteratorProtoSetToStringTag ->
      ignore_proto_setter(this, args, state, IgnoreSetTag)
    IteratorProtoSetConstructor ->
      ignore_proto_setter(this, args, state, IgnoreSetCtor)
  }
}

// ============================================================================
// §27.1.1.1 Iterator ( ) — abstract constructor
// ============================================================================

/// The Iterator constructor throws when called or new'd directly, but
/// returns `this` when reached via `super()` in a subclass. Arc's CallSuper
/// passes the freshly-allocated subclass instance as `this`; direct call /
/// `new Iterator()` both arrive with `this = undefined`.
fn construct(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(_) -> #(state, Ok(this))
    _ ->
      state.type_error(
        state,
        "Abstract class Iterator not directly constructable",
      )
  }
}

// ============================================================================
// §27.1.2.1 Iterator.from ( O )
// ============================================================================

fn from(
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  let o = first_arg_or_undefined(args)
  // GetIteratorFlattenable(O, iterate-strings): O must be Object or String.
  use Nil, state <- after(case o {
    JsObject(_) | JsString(_) -> Ok(#(Nil, state))
    _ -> err_type(state, "Iterator.from called on non-object")
  })
  // method = GetMethod(O, @@iterator)
  use method, state <- state.try_op(object.get_symbol_value_of(
    state,
    o,
    value.symbol_iterator,
  ))
  use iter, state <- after(case method {
    JsUndefined | JsNull -> Ok(#(o, state))
    _ ->
      case state.call(state, method, o, []) {
        Ok(#(v, state)) -> Ok(#(v, state))
        Error(#(thrown, state)) -> Error(#(state, Error(thrown)))
      }
  })
  use iter_ref <- require_object_of(
    iter,
    state,
    "Iterator.from: result of @@iterator is not an object",
  )
  // next = Get(iterator, "next")
  use next, state <- state.try_op(object.get_value_of(
    state,
    iter,
    Named("next"),
  ))
  // OrdinaryHasInstance(%Iterator%, iterator) — proto chain walk.
  let target = state.builtins.iterator.prototype
  case has_in_proto_chain(state.heap, iter_ref, target) {
    True -> #(state, Ok(iter))
    False -> {
      let #(heap, ref) =
        common.alloc_wrapper(
          state.heap,
          WrapForValidIteratorObject(iterated: iter, next_method: next),
          state.builtins.wrap_for_valid_iterator_proto,
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// Walk obj's prototype chain looking for target. Includes obj itself.
fn has_in_proto_chain(h: Heap, obj: Ref, target: Ref) -> Bool {
  case obj.id == target.id {
    True -> True
    False ->
      case heap.read(h, obj) {
        Some(ObjectSlot(prototype: Some(proto), ..)) ->
          has_in_proto_chain(h, proto, target)
        _ -> False
      }
  }
}

// ============================================================================
// Lazy producers — Iterator.prototype.{map,filter,flatMap}
// ============================================================================

fn lazy_helper(
  this: JsValue,
  args: List(JsValue),
  state: State,
  kind: IteratorHelperKind,
  name: String,
) -> #(State, Result(JsValue, JsValue)) {
  use _this_ref <- require_object_of(
    this,
    state,
    "Iterator.prototype." <> name <> " called on non-object",
  )
  // §27.1.4.5 step 3: validate callback BEFORE GetIteratorDirect. On failure,
  // close `this` (without having read `.next`) then throw.
  let func = first_arg_or_undefined(args)
  case is_callable(state.heap, func) {
    False -> close_throw_type(state, this, name <> " argument is not callable")
    True -> {
      use next, state <- state.try_op(object.get_value_of(
        state,
        this,
        Named("next"),
      ))
      alloc_helper(state, kind, this, next, func, 0)
    }
  }
}

// ============================================================================
// Lazy producers — Iterator.prototype.{take,drop}
// ============================================================================

fn take_or_drop(
  this: JsValue,
  args: List(JsValue),
  state: State,
  kind: IteratorHelperKind,
  name: String,
) -> #(State, Result(JsValue, JsValue)) {
  use _this_ref <- require_object_of(
    this,
    state,
    "Iterator.prototype." <> name <> " called on non-object",
  )
  // §27.1.4.10 step 3-6: ToNumber(limit) BEFORE GetIteratorDirect. On any
  // abrupt completion / NaN / negative, close `this` then throw.
  use count, state <- after(coerce_limit(state, this, args, name))
  use next, state <- state.try_op(object.get_value_of(
    state,
    this,
    Named("next"),
  ))
  alloc_helper(state, kind, this, next, JsUndefined, count)
}

/// ES2025 §27.1.3.10/12 step 3-6: ToIntegerOrInfinity(ToNumber(limit)) with
/// NaN/negative → RangeError. On any abrupt completion, close `this` first.
fn coerce_limit(
  state: State,
  this: JsValue,
  args: List(JsValue),
  name: String,
) -> Result(#(Int, State), #(State, Result(JsValue, JsValue))) {
  let arg = first_arg_or_undefined(args)
  // ToNumber: ToPrimitive(NumberHint) for objects (so valueOf/@@toPrimitive
  // can throw user errors), then primitive → JsNum.
  let prim_result = case arg {
    JsObject(_) -> coerce.to_primitive(state, arg, coerce.NumberHint)
    other -> Ok(#(other, state))
  }
  case prim_result {
    Error(#(thrown, state)) -> Error(close_throw(state, this, thrown))
    Ok(#(prim, state)) ->
      case value.to_number(prim) {
        Error(msg) -> Error(close_throw_type(state, this, msg))
        Ok(NaN) ->
          Error(close_throw_range(state, this, name <> " limit is NaN"))
        Ok(Infinity) -> Ok(#(limits.max_safe_integer, state))
        Ok(NegInfinity) ->
          Error(close_throw_range(state, this, name <> " limit is negative"))
        Ok(Finite(f)) ->
          case f <. 0.0 {
            True ->
              Error(close_throw_range(state, this, name <> " limit is negative"))
            False -> Ok(#(value.float_to_int(f), state))
          }
      }
  }
}

fn alloc_helper(
  state: State,
  kind: IteratorHelperKind,
  underlying: JsValue,
  next_method: JsValue,
  func: JsValue,
  count: Int,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      IteratorHelperObject(
        kind:,
        underlying:,
        next_method:,
        func:,
        inner: JsUndefined,
        inner_next: JsUndefined,
        count:,
        done: False,
      ),
      state.builtins.iterator_helper_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

// ============================================================================
// %IteratorHelperPrototype%.next / .return
// ============================================================================

fn helper_next(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, kind, underlying, next, func, inner, inner_next, count, done <- require_helper(
    this,
    state,
  )
  case done {
    True -> create_iter_result(state, JsUndefined, True)
    False ->
      case kind {
        HelperMap -> step_map(state, ref, underlying, next, func, count)
        HelperFilter -> step_filter(state, ref, underlying, next, func, count)
        HelperTake -> step_take(state, ref, underlying, next, count)
        HelperDrop -> step_drop(state, ref, underlying, next, count)
        HelperFlatMap ->
          step_flat_map(
            state,
            ref,
            underlying,
            next,
            func,
            inner,
            inner_next,
            count,
          )
      }
  }
}

fn helper_return(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use ref, kind, underlying, _next, _func, inner, _inner_next, _count, done <- require_helper(
    this,
    state,
  )
  case done {
    True -> create_iter_result(state, JsUndefined, True)
    False -> {
      let state = mark_done(state, ref)
      // For flatMap, close the inner iterator first (best-effort), then outer.
      let #(state, inner_res) = case kind, inner {
        HelperFlatMap, JsObject(_) -> iterator_close_normal(state, inner)
        _, _ -> #(state, Ok(Nil))
      }
      let #(state, outer_res) = iterator_close_normal(state, underlying)
      case inner_res, outer_res {
        Error(e), _ -> #(state, Error(e))
        _, Error(e) -> #(state, Error(e))
        Ok(Nil), Ok(Nil) -> create_iter_result(state, JsUndefined, True)
      }
    }
  }
}

fn step_map(
  state: State,
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  func: JsValue,
  count: Int,
) -> #(State, Result(JsValue, JsValue)) {
  use step, state <- after_step(state, ref, underlying, next)
  case step {
    None -> finish(state, ref)
    Some(v) -> {
      let state = write_count(state, ref, count + 1)
      let counter = value.from_int(count)
      case state.call(state, func, JsUndefined, [v, counter]) {
        Ok(#(mapped, state)) -> create_iter_result(state, mapped, False)
        Error(#(thrown, state)) ->
          close_throw(mark_done(state, ref), underlying, thrown)
      }
    }
  }
}

fn step_filter(
  state: State,
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  func: JsValue,
  count: Int,
) -> #(State, Result(JsValue, JsValue)) {
  use step, state <- after_step(state, ref, underlying, next)
  case step {
    None -> finish(state, ref)
    Some(v) -> {
      let state = write_count(state, ref, count + 1)
      let counter = value.from_int(count)
      case state.call(state, func, JsUndefined, [v, counter]) {
        Error(#(thrown, state)) ->
          close_throw(mark_done(state, ref), underlying, thrown)
        Ok(#(selected, state)) ->
          case value.is_truthy(selected) {
            True -> create_iter_result(state, v, False)
            False -> step_filter(state, ref, underlying, next, func, count + 1)
          }
      }
    }
  }
}

fn step_take(
  state: State,
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  remaining: Int,
) -> #(State, Result(JsValue, JsValue)) {
  case remaining <= 0 {
    True -> {
      // §27.1.3.3.11: when remaining is 0, Return ? IteratorClose(iterated,
      // ReturnCompletion(undefined)) and yield done.
      let state = mark_done(state, ref)
      let #(state, close_res) = iterator_close_normal(state, underlying)
      case close_res {
        Error(e) -> #(state, Error(e))
        Ok(Nil) -> create_iter_result(state, JsUndefined, True)
      }
    }
    False -> {
      use step, state <- after_step(state, ref, underlying, next)
      case step {
        None -> finish(state, ref)
        Some(v) -> {
          let state = write_count(state, ref, remaining - 1)
          create_iter_result(state, v, False)
        }
      }
    }
  }
}

fn step_drop(
  state: State,
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  remaining: Int,
) -> #(State, Result(JsValue, JsValue)) {
  use step, state <- after_step(state, ref, underlying, next)
  case step {
    None -> finish(state, ref)
    Some(v) ->
      case remaining > 0 {
        True -> {
          let state = write_count(state, ref, remaining - 1)
          step_drop(state, ref, underlying, next, remaining - 1)
        }
        False -> create_iter_result(state, v, False)
      }
  }
}

fn step_flat_map(
  state: State,
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  func: JsValue,
  inner: JsValue,
  inner_next: JsValue,
  count: Int,
) -> #(State, Result(JsValue, JsValue)) {
  case inner {
    // Have an active inner iterator — pull from it.
    JsObject(_) ->
      case iterator_step_value(state, inner, inner_next) {
        #(state, Error(thrown)) ->
          // inner.next() threw → close outer (inner is already broken).
          close_throw(mark_done(state, ref), underlying, thrown)
        #(state, Ok(Some(v))) -> create_iter_result(state, v, False)
        #(state, Ok(None)) -> {
          // Inner exhausted — clear and pull from outer.
          let state = write_inner(state, ref, JsUndefined, JsUndefined)
          step_flat_map(
            state,
            ref,
            underlying,
            next,
            func,
            JsUndefined,
            JsUndefined,
            count,
          )
        }
      }
    // No inner — pull from outer, map, open new inner.
    _ -> {
      use step, state <- after_step(state, ref, underlying, next)
      case step {
        None -> finish(state, ref)
        Some(v) -> {
          let counter = value.from_int(count)
          let state = write_count(state, ref, count + 1)
          case state.call(state, func, JsUndefined, [v, counter]) {
            Error(#(thrown, state)) ->
              close_throw(mark_done(state, ref), underlying, thrown)
            Ok(#(mapped, state)) ->
              // GetIteratorFlattenable(mapped, reject-strings) — must be Object
              case open_inner(state, mapped) {
                #(state, Error(thrown)) ->
                  close_throw(mark_done(state, ref), underlying, thrown)
                #(state, Ok(#(inner, inner_next))) -> {
                  let state = write_inner(state, ref, inner, inner_next)
                  step_flat_map(
                    state,
                    ref,
                    underlying,
                    next,
                    func,
                    inner,
                    inner_next,
                    count + 1,
                  )
                }
              }
          }
        }
      }
    }
  }
}

/// GetIteratorFlattenable(obj, reject-strings) for flatMap inner values.
fn open_inner(
  state: State,
  mapped: JsValue,
) -> #(State, Result(#(JsValue, JsValue), JsValue)) {
  case mapped {
    JsObject(_) -> {
      use method, state <- state.try_op(object.get_symbol_value_of(
        state,
        mapped,
        value.symbol_iterator,
      ))
      let iter_result = case method {
        JsUndefined | JsNull -> Ok(#(mapped, state))
        _ -> state.call(state, method, mapped, [])
      }
      use iter, state <- state.try_op(iter_result)
      case iter {
        JsObject(_) -> {
          use inner_next, state <- state.try_op(object.get_value_of(
            state,
            iter,
            Named("next"),
          ))
          #(state, Ok(#(iter, inner_next)))
        }
        _ -> type_error_any(state, "flatMap callback result is not iterable")
      }
    }
    _ -> type_error_any(state, "flatMap callback returned a non-object")
  }
}

// ============================================================================
// %WrapForValidIteratorPrototype%.next / .return
// ============================================================================

fn wrap_next(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use iterated, next_method <- require_wrap(this, state)
  use result, state <- state.try_call(state, next_method, iterated, [])
  #(state, Ok(result))
}

fn wrap_return(
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use iterated, _next_method <- require_wrap(this, state)
  use ret_fn, state <- state.try_op(object.get_value_of(
    state,
    iterated,
    Named("return"),
  ))
  case ret_fn {
    JsUndefined | JsNull -> create_iter_result(state, JsUndefined, True)
    _ -> {
      use result, state <- state.try_call(state, ret_fn, iterated, [])
      case result {
        JsObject(_) -> #(state, Ok(result))
        _ -> state.type_error(state, "Iterator return result is not an object")
      }
    }
  }
}

// ============================================================================
// Eager consumers — toArray, forEach, reduce, some, every, find
// ============================================================================

fn to_array(this: JsValue, state: State) -> #(State, Result(JsValue, JsValue)) {
  use this, next, state <- get_iterator_direct(this, state, "toArray")
  to_array_loop(state, this, next, [])
}

fn to_array_loop(
  state: State,
  iter: JsValue,
  next: JsValue,
  acc: List(JsValue),
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, step) = iterator_step_value(state, iter, next)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> state.ok_array(state, list.reverse(acc))
    Ok(Some(v)) -> to_array_loop(state, iter, next, [v, ..acc])
  }
}

fn for_each(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use this, next, func, state <- consumer_with_callback(
    this,
    args,
    state,
    "forEach",
  )
  for_each_loop(state, this, next, func, 0)
}

fn for_each_loop(
  state: State,
  iter: JsValue,
  next: JsValue,
  func: JsValue,
  counter: Int,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, step) = iterator_step_value(state, iter, next)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> #(state, Ok(JsUndefined))
    Ok(Some(v)) -> {
      let idx = value.from_int(counter)
      case state.call(state, func, JsUndefined, [v, idx]) {
        Error(#(thrown, state)) -> close_throw(state, iter, thrown)
        Ok(#(_result, state)) ->
          for_each_loop(state, iter, next, func, counter + 1)
      }
    }
  }
}

fn reduce(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use this, next, func, state <- consumer_with_callback(
    this,
    args,
    state,
    "reduce",
  )
  // §27.1.3.9: if initialValue is not present, seed accumulator with first step.
  case args {
    [_, initial, ..] -> reduce_loop(state, this, next, func, initial, 0)
    _ -> {
      let #(state, step) = iterator_step_value(state, this, next)
      case step {
        Error(thrown) -> #(state, Error(thrown))
        Ok(None) ->
          state.type_error(
            state,
            "Reduce of empty iterator with no initial value",
          )
        Ok(Some(seed)) -> reduce_loop(state, this, next, func, seed, 1)
      }
    }
  }
}

fn reduce_loop(
  state: State,
  iter: JsValue,
  next: JsValue,
  func: JsValue,
  acc: JsValue,
  counter: Int,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, step) = iterator_step_value(state, iter, next)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> #(state, Ok(acc))
    Ok(Some(v)) -> {
      let idx = value.from_int(counter)
      case state.call(state, func, JsUndefined, [acc, v, idx]) {
        Error(#(thrown, state)) -> close_throw(state, iter, thrown)
        Ok(#(new_acc, state)) ->
          reduce_loop(state, iter, next, func, new_acc, counter + 1)
      }
    }
  }
}

/// Shared body for some/every. `match_on` = the truthiness value that triggers
/// early exit. some → True (returns true), every → False (returns false).
fn bool_consumer(
  this: JsValue,
  args: List(JsValue),
  state: State,
  match_on: Bool,
  name: String,
) -> #(State, Result(JsValue, JsValue)) {
  use this, next, func, state <- consumer_with_callback(this, args, state, name)
  let #(state, res) = predicate_loop(state, this, next, func, 0, match_on)
  #(state, result.map(res, fn(m) { JsBool(option.is_some(m) == match_on) }))
}

/// Shared loop for some/every/find: step iterator, call predicate(v, idx),
/// early-exit (closing iterator) when truthiness == match_on. Some(v) = matched.
fn predicate_loop(
  state: State,
  iter: JsValue,
  next: JsValue,
  func: JsValue,
  counter: Int,
  match_on: Bool,
) -> #(State, Result(Option(JsValue), JsValue)) {
  let #(state, step) = iterator_step_value(state, iter, next)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> #(state, Ok(None))
    Ok(Some(v)) -> {
      let idx = value.from_int(counter)
      case state.call(state, func, JsUndefined, [v, idx]) {
        Error(#(thrown, state)) -> close_throw(state, iter, thrown)
        Ok(#(result, state)) ->
          case value.is_truthy(result) == match_on {
            True -> {
              let #(state, close_res) = iterator_close_normal(state, iter)
              case close_res {
                Error(e) -> #(state, Error(e))
                Ok(Nil) -> #(state, Ok(Some(v)))
              }
            }
            False ->
              predicate_loop(state, iter, next, func, counter + 1, match_on)
          }
      }
    }
  }
}

fn find(
  this: JsValue,
  args: List(JsValue),
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  use this, next, func, state <- consumer_with_callback(
    this,
    args,
    state,
    "find",
  )
  let #(state, res) = predicate_loop(state, this, next, func, 0, True)
  #(state, result.map(res, option.unwrap(_, JsUndefined)))
}

// ============================================================================
// SetterThatIgnoresPrototypeProperties — §27.1.3.2/.13
// ============================================================================

type IgnoreSetterKey {
  IgnoreSetCtor
  IgnoreSetTag
}

/// If `this` is %Iterator.prototype% itself → TypeError. If `this` is not an
/// Object → TypeError. Otherwise CreateDataProperty(this, key, val).
fn ignore_proto_setter(
  this: JsValue,
  args: List(JsValue),
  state: State,
  which: IgnoreSetterKey,
) -> #(State, Result(JsValue, JsValue)) {
  let proto = state.builtins.iterator.prototype
  case this {
    JsObject(ref) ->
      case ref.id == proto.id {
        True ->
          state.type_error(
            state,
            "Cannot assign to read only property of Iterator.prototype",
          )
        False -> {
          let val = first_arg_or_undefined(args)
          let heap =
            heap.update(state.heap, ref, fn(slot) {
              case slot, which {
                ObjectSlot(properties:, ..), IgnoreSetCtor ->
                  ObjectSlot(
                    ..slot,
                    properties: dict.insert(
                      properties,
                      Named("constructor"),
                      value.data(val)
                        |> value.writable
                        |> value.enumerable
                        |> value.configurable,
                    ),
                  )
                ObjectSlot(symbol_properties:, ..), IgnoreSetTag ->
                  ObjectSlot(
                    ..slot,
                    symbol_properties: list.key_set(
                      symbol_properties,
                      value.symbol_to_string_tag,
                      value.data(val)
                        |> value.writable
                        |> value.enumerable
                        |> value.configurable,
                    ),
                  )
                other, _ -> other
              }
            })
          #(State(..state, heap:), Ok(JsUndefined))
        }
      }
    _ ->
      state.type_error(
        state,
        "Cannot set property on non-object Iterator receiver",
      )
  }
}

// ============================================================================
// Core iteration helpers — GetIteratorDirect, IteratorStepValue, IteratorClose
// ============================================================================

/// §7.4.9 GetIteratorDirect ( obj ): obj must be an Object; nextMethod is read
/// once. CPS-style — `use this, next, state <- get_iterator_direct(...)`.
fn get_iterator_direct(
  this: JsValue,
  state: State,
  name: String,
  cont: fn(JsValue, JsValue, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  use _ref <- require_object_of(
    this,
    state,
    "Iterator.prototype." <> name <> " called on non-object",
  )
  use next, state <- state.try_op(object.get_value_of(
    state,
    this,
    Named("next"),
  ))
  cont(this, next, state)
}

/// Shared prologue for forEach/reduce/some/every/find: validate `this` is
/// Object, validate callback (closing `this` on failure WITHOUT having read
/// `.next`), then GetIteratorDirect.
fn consumer_with_callback(
  this: JsValue,
  args: List(JsValue),
  state: State,
  name: String,
  cont: fn(JsValue, JsValue, JsValue, State) ->
    #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  use _ref <- require_object_of(
    this,
    state,
    "Iterator.prototype." <> name <> " called on non-object",
  )
  let func = first_arg_or_undefined(args)
  case is_callable(state.heap, func) {
    False -> close_throw_type(state, this, name <> " argument is not callable")
    True -> {
      use next, state <- state.try_op(object.get_value_of(
        state,
        this,
        Named("next"),
      ))
      cont(this, next, func, state)
    }
  }
}

/// §7.4.8 IteratorStepValue: call next(obj), require result is Object,
/// read .done — if truthy return None; else read .value return Some.
fn iterator_step_value(
  state: State,
  obj: JsValue,
  next_method: JsValue,
) -> #(State, Result(Option(JsValue), JsValue)) {
  case state.call(state, next_method, obj, []) {
    Error(#(thrown, state)) -> #(state, Error(thrown))
    Ok(#(result, state)) ->
      case result {
        JsObject(_) -> {
          use done, state <- state.try_op(object.get_value_of(
            state,
            result,
            Named("done"),
          ))
          case value.is_truthy(done) {
            True -> #(state, Ok(None))
            False -> {
              use v, state <- state.try_op(object.get_value_of(
                state,
                result,
                Named("value"),
              ))
              #(state, Ok(Some(v)))
            }
          }
        }
        _ -> type_error_any(state, "Iterator result is not an object")
      }
  }
}

/// §7.4.11 IteratorClose with throw completion: get .return; if callable, call
/// it (swallowing any throw — original error wins); return original error.
fn close_throw(
  state: State,
  obj: JsValue,
  original: JsValue,
) -> #(State, Result(a, JsValue)) {
  let #(state, _ignored) = call_return(state, obj)
  #(state, Error(original))
}

/// IteratorClose with a freshly-allocated TypeError.
fn close_throw_type(
  state: State,
  obj: JsValue,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_type_error(state.heap, state.builtins, msg)
  close_throw(State(..state, heap:), obj, err)
}

/// IteratorClose with a freshly-allocated RangeError.
fn close_throw_range(
  state: State,
  obj: JsValue,
  msg: String,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, err) = common.make_range_error(state.heap, state.builtins, msg)
  close_throw(State(..state, heap:), obj, err)
}

/// §7.4.11 IteratorClose with normal completion: get .return; if undefined →
/// Ok; else call it; if call throws → propagate; if result not Object →
/// TypeError; else Ok.
fn iterator_close_normal(
  state: State,
  obj: JsValue,
) -> #(State, Result(Nil, JsValue)) {
  case call_return(state, obj) {
    #(state, Ok(JsUndefined)) -> #(state, Ok(Nil))
    #(state, Ok(JsObject(_))) -> #(state, Ok(Nil))
    #(state, Ok(_other)) ->
      type_error_any(state, "Iterator return result is not an object")
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

/// Shared body of IteratorClose: GetMethod(iterator, "return") and call it.
/// Ok(JsUndefined) means "no return method" (so the not-an-object check is
/// skipped). Ok(other) is the return method's result.
fn call_return(
  state: State,
  obj: JsValue,
) -> #(State, Result(JsValue, JsValue)) {
  use ret_fn, state <- state.try_op(object.get_value_of(
    state,
    obj,
    Named("return"),
  ))
  case ret_fn {
    JsUndefined | JsNull -> #(state, Ok(JsUndefined))
    _ -> {
      use result, state <- state.try_call(state, ret_fn, obj, [])
      #(state, Ok(result))
    }
  }
}

// ============================================================================
// Small helpers
// ============================================================================

/// CreateIterResultObject(value, done) — adapts common helper to State result.
fn create_iter_result(
  state: State,
  val: JsValue,
  done: Bool,
) -> #(State, Result(JsValue, JsValue)) {
  let #(heap, v) =
    common.create_iter_result(state.heap, state.builtins, val, done)
  #(State(..state, heap:), Ok(v))
}

/// Unwrap `this` as an Object ref or TypeError. CPS — `use ref <- ...`.
fn require_object_of(
  this: JsValue,
  state: State,
  msg: String,
  cont: fn(Ref) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> cont(ref)
    _ -> state.type_error(state, msg)
  }
}

/// Unwrap `this` as an IteratorHelperObject. CPS-style.
fn require_helper(
  this: JsValue,
  state: State,
  cont: fn(
    Ref,
    IteratorHelperKind,
    JsValue,
    JsValue,
    JsValue,
    JsValue,
    JsValue,
    Int,
    Bool,
  ) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let err = "Iterator Helper method called on incompatible receiver"
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: IteratorHelperObject(
            kind:,
            underlying:,
            next_method:,
            func:,
            inner:,
            inner_next:,
            count:,
            done:,
          ),
          ..,
        )) ->
          cont(
            ref,
            kind,
            underlying,
            next_method,
            func,
            inner,
            inner_next,
            count,
            done,
          )
        _ -> state.type_error(state, err)
      }
    _ -> state.type_error(state, err)
  }
}

/// Unwrap `this` as a WrapForValidIteratorObject. CPS-style.
fn require_wrap(
  this: JsValue,
  state: State,
  cont: fn(JsValue, JsValue) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let err = "WrapForValidIterator method called on incompatible receiver"
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: WrapForValidIteratorObject(iterated:, next_method:),
          ..,
        )) -> cont(iterated, next_method)
        _ -> state.type_error(state, err)
      }
    _ -> state.type_error(state, err)
  }
}

/// Thread a Result whose Error already carries the dispatch-shape tuple.
/// `use v, state <- after(result)`.
fn after(
  result: Result(#(a, State), #(State, Result(JsValue, JsValue))),
  cont: fn(a, State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  case result {
    Ok(#(v, state)) -> cont(v, state)
    Error(r) -> r
  }
}

fn err_type(
  state: State,
  msg: String,
) -> Result(a, #(State, Result(JsValue, JsValue))) {
  Error(state.type_error(state, msg))
}

/// state.type_error but polymorphic in the Ok type — for callsites where the
/// surrounding Result's Ok type isn't JsValue (so state.type_error won't unify).
fn type_error_any(state: State, msg: String) -> #(State, Result(a, JsValue)) {
  let #(heap, err) = common.make_type_error(state.heap, state.builtins, msg)
  #(State(..state, heap:), Error(err))
}

/// Step the underlying iterator. If next() throws, mark the helper done and
/// propagate WITHOUT calling close (the iterator is already broken).
fn after_step(
  state: State,
  ref: Ref,
  obj: JsValue,
  next: JsValue,
  cont: fn(Option(JsValue), State) -> #(State, Result(JsValue, JsValue)),
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, step) = iterator_step_value(state, obj, next)
  case step {
    Ok(v) -> cont(v, state)
    Error(thrown) -> #(mark_done(state, ref), Error(thrown))
  }
}

/// Mark a helper done and yield {value: undefined, done: true}.
fn finish(state: State, ref: Ref) -> #(State, Result(JsValue, JsValue)) {
  create_iter_result(mark_done(state, ref), JsUndefined, True)
}

fn mark_done(state: State, ref: Ref) -> State {
  update_helper(state, ref, None, None, None, True)
}

fn write_count(state: State, ref: Ref, count: Int) -> State {
  update_helper(state, ref, Some(count), None, None, False)
}

fn write_inner(
  state: State,
  ref: Ref,
  inner: JsValue,
  inner_next: JsValue,
) -> State {
  update_helper(state, ref, None, Some(inner), Some(inner_next), False)
}

/// Rewrite the IteratorHelperObject kind in place. Gleam's record-update
/// can't narrow an ExoticKind variant, so we re-match and rebuild manually.
fn update_helper(
  state: State,
  ref: Ref,
  new_count: Option(Int),
  new_inner: Option(JsValue),
  new_inner_next: Option(JsValue),
  set_done: Bool,
) -> State {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(
          kind: IteratorHelperObject(
            kind:,
            underlying:,
            next_method:,
            func:,
            inner:,
            inner_next:,
            count:,
            done:,
          ),
          ..,
        ) ->
          ObjectSlot(
            ..slot,
            kind: IteratorHelperObject(
              kind:,
              underlying:,
              next_method:,
              func:,
              inner: option.unwrap(new_inner, inner),
              inner_next: option.unwrap(new_inner_next, inner_next),
              count: option.unwrap(new_count, count),
              done: done || set_done,
            ),
          )
        other -> other
      }
    })
  State(..state, heap:)
}
