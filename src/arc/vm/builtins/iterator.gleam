/// ES2025 §27.1 Iteration — the Iterator constructor, Iterator.from, and
/// Iterator.prototype helper methods (map, filter, take, drop, flatMap,
/// toArray, forEach, reduce, some, every, find).
///
/// Prior art: QuickJS quickjs.c js_iterator_* (bellard/quickjs).
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined, is_callable}
import arc/vm/builtins/object as builtins_object
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, type StepResult, State}
import arc/vm/value.{
  type IteratorHelperKind, type IteratorNativeFn, type JsValue, type Ref,
  type ZipMember, type ZipMode, Dispatch, Finite, HelperDrop, HelperFilter,
  HelperFlatMap, HelperMap, HelperTake, Index, Infinity, IteratorConcat,
  IteratorConcatObject, IteratorConstructor, IteratorFrom, IteratorHelperNext,
  IteratorHelperObject, IteratorHelperReturn, IteratorNative,
  IteratorProtoGetConstructor, IteratorProtoGetToStringTag,
  IteratorProtoSetConstructor, IteratorProtoSetToStringTag,
  IteratorPrototypeDrop, IteratorPrototypeEvery, IteratorPrototypeFilter,
  IteratorPrototypeFind, IteratorPrototypeFlatMap, IteratorPrototypeForEach,
  IteratorPrototypeMap, IteratorPrototypeReduce, IteratorPrototypeSome,
  IteratorPrototypeTake, IteratorPrototypeToArray, IteratorZip, IteratorZipKeyed,
  IteratorZipObject, JsBool, JsNull, JsObject, JsString, JsSymbol, JsUndefined,
  NaN, Named, NegInfinity, ObjectSlot, OrdinaryObject, WrapForValidIteratorNext,
  WrapForValidIteratorObject, WrapForValidIteratorReturn, ZipExhausted,
  ZipLongest, ZipOpen, ZipShortest, ZipStrict,
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
  h: Heap(host),
  iterator_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), BuiltinType, Ref, Ref) {
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

  // Iterator.from / Iterator.concat / Iterator.zip / Iterator.zipKeyed
  // static methods.
  let #(h, ctor_props) =
    common.alloc_methods(h, function_proto, [
      #("from", IteratorNative(IteratorFrom), 1),
      #("concat", IteratorNative(IteratorConcat), 0),
      #("zip", IteratorNative(IteratorZip), 1),
      #("zipKeyed", IteratorNative(IteratorZipKeyed), 1),
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
      // §27.1.3.1: Iterator is an abstract constructor — it HAS [[Construct]]
      // (IsConstructor(Iterator) is true and it's subclassable), but `new
      // Iterator()` directly throws. The IteratorConstructor native enforces
      // that: it returns `this` for a subclass super() call, throws otherwise.
      True,
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    IteratorConstructor -> construct(state)
    IteratorFrom -> from(args, state)
    IteratorZip -> zip(args, state)
    IteratorZipKeyed -> zip_keyed(args, state)
    IteratorConcat -> concat(args, state)
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

/// §27.1.1.1 step 1: throw if NewTarget is undefined or the active function
/// (i.e., %Iterator% itself); otherwise OrdinaryCreateFromConstructor.
/// `do_construct` sets `state.new_target` before native dispatch; calling as a
/// function leaves it `JsUndefined`.
fn construct(state: State(host)) -> #(State(host), Result(JsValue, JsValue)) {
  let self = JsObject(state.builtins.iterator.constructor)
  let nt = state.new_target
  case nt == JsUndefined || nt == self {
    True ->
      state.type_error(
        state,
        "Abstract class Iterator not directly constructable",
      )
    False -> {
      let #(h, ref) =
        common.ordinary_create_from_constructor(
          state.heap,
          nt,
          state.builtins.iterator.prototype,
        )
      #(State(..state, heap: h), Ok(JsObject(ref)))
    }
  }
}

// ============================================================================
// §27.1.2.1 Iterator.from ( O )
// ============================================================================

fn from(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
fn has_in_proto_chain(h: Heap(host), obj: Ref, target: Ref) -> Bool {
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
  state: State(host),
  kind: IteratorHelperKind,
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use this, next, func, state <- consumer_with_callback(this, args, state, name)
  alloc_helper(state, kind, this, next, func, 0)
}

// ============================================================================
// Lazy producers — Iterator.prototype.{take,drop}
// ============================================================================

fn take_or_drop(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  kind: IteratorHelperKind,
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  name: String,
) -> Result(#(Int, State(host)), #(State(host), Result(JsValue, JsValue))) {
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
  state: State(host),
  kind: IteratorHelperKind,
  underlying: JsValue,
  next_method: JsValue,
  func: JsValue,
  count: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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

const helper_receiver_err = "Iterator Helper method called on incompatible receiver"

fn helper_next(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
          classic_helper_next(
            state,
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
        Some(ObjectSlot(
          kind: IteratorZipObject(
            members:,
            mode:,
            padding:,
            keys:,
            done:,
            running:,
            started: _,
          ),
          ..,
        )) ->
          case running {
            // GeneratorValidate: "executing" → TypeError.
            True -> state.type_error(state, helper_running_err)
            False -> {
              // GeneratorResume: state becomes "executing" (and the
              // generator is no longer suspended-start).
              let state = zip_set_flags(state, ref, True, True)
              let #(state, res) =
                zip_next(state, ref, members, mode, padding, keys, done)
              #(zip_set_flags(state, ref, False, False), res)
            }
          }
        Some(ObjectSlot(
          kind: IteratorConcatObject(remaining:, inner:, done:, running:),
          ..,
        )) ->
          case running {
            True -> state.type_error(state, helper_running_err)
            False -> {
              let state = concat_set_running(state, ref, True)
              let #(state, res) =
                concat_next(state, ref, remaining, inner, done)
              #(concat_set_running(state, ref, False), res)
            }
          }
        _ -> state.type_error(state, helper_receiver_err)
      }
    _ -> state.type_error(state, helper_receiver_err)
  }
}

const helper_running_err = "Iterator Helper is currently being iterated"

/// Set the zip helper's "executing" flag; `started` latches True once set.
fn zip_set_flags(
  state: State(host),
  ref: Ref,
  running: Bool,
  set_started: Bool,
) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: IteratorZipObject(started:, ..) as kind, ..) ->
          ObjectSlot(
            ..slot,
            kind: IteratorZipObject(
              ..kind,
              running:,
              started: started || set_started,
            ),
          )
        other -> other
      }
    })
  State(..state, heap:)
}

/// Set the concat helper's "executing" flag.
fn concat_set_running(
  state: State(host),
  ref: Ref,
  running: Bool,
) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: IteratorConcatObject(..) as kind, ..) ->
          ObjectSlot(..slot, kind: IteratorConcatObject(..kind, running:))
        other -> other
      }
    })
  State(..state, heap:)
}

fn classic_helper_next(
  state: State(host),
  ref: Ref,
  kind: IteratorHelperKind,
  underlying: JsValue,
  next: JsValue,
  func: JsValue,
  inner: JsValue,
  inner_next: JsValue,
  count: Int,
  done: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(
          kind: IteratorHelperObject(kind:, underlying:, inner:, done:, ..),
          ..,
        )) -> classic_helper_return(state, ref, kind, underlying, inner, done)
        Some(ObjectSlot(
          kind: IteratorZipObject(members:, done:, running:, started:, ..),
          ..,
        )) ->
          case running, done, started {
            // GeneratorValidate: "executing" → TypeError.
            True, _, _ -> state.type_error(state, helper_running_err)
            False, True, _ -> create_iter_result(state, JsUndefined, True)
            // Suspended-start: state goes straight to completed, THEN the
            // underlying iterators are closed — a reentrant next/return
            // during the closes sees "completed", not "executing".
            False, False, False -> zip_return(state, ref, members, False)
            // Suspended-yield: the generator resumes ("executing") to run
            // IteratorCloseAll — reentrant next/return throws.
            False, False, True -> {
              let state = zip_set_flags(state, ref, True, True)
              let #(state, res) = zip_return(state, ref, members, False)
              #(zip_set_flags(state, ref, False, False), res)
            }
          }
        Some(ObjectSlot(
          kind: IteratorConcatObject(inner:, done:, running:, ..),
          ..,
        )) ->
          case running, done, inner {
            True, _, _ -> state.type_error(state, helper_running_err)
            False, True, _ -> create_iter_result(state, JsUndefined, True)
            // Suspended-start / between iterables: nothing to close.
            False, False, None -> concat_return(state, ref, None, False)
            // Suspended-yield: closing the inner iterator runs user code
            // while "executing".
            False, False, Some(rec) -> {
              let state = concat_set_running(state, ref, True)
              let #(state, res) = concat_return(state, ref, Some(rec), False)
              #(concat_set_running(state, ref, False), res)
            }
          }
        _ -> state.type_error(state, helper_receiver_err)
      }
    _ -> state.type_error(state, helper_receiver_err)
  }
}

fn classic_helper_return(
  state: State(host),
  ref: Ref,
  kind: IteratorHelperKind,
  underlying: JsValue,
  inner: JsValue,
  done: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  func: JsValue,
  count: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  func: JsValue,
  count: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  remaining: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  remaining: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  ref: Ref,
  underlying: JsValue,
  next: JsValue,
  func: JsValue,
  inner: JsValue,
  inner_next: JsValue,
  count: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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
              case
                get_iterator_flattenable(
                  state,
                  mapped,
                  "flatMap callback result",
                )
              {
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

/// §7.4.13 GetIteratorFlattenable(obj, reject-primitives): obj must be an
/// Object; if it has an @@iterator method, call it (result must be an
/// Object), otherwise treat obj itself as the iterator. Caches the next
/// method (GetIteratorDirect). Used by flatMap inner values and
/// Iterator.zip/zipKeyed inputs.
fn get_iterator_flattenable(
  state: State(host),
  obj: JsValue,
  what: String,
) -> #(State(host), Result(#(JsValue, JsValue), JsValue)) {
  case obj {
    JsObject(_) -> {
      use method, state <- state.try_op(object.get_symbol_value_of(
        state,
        obj,
        value.symbol_iterator,
      ))
      let iter_result = case method {
        JsUndefined | JsNull -> Ok(#(obj, state))
        _ -> state.call(state, method, obj, [])
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
        _ -> type_error_any(state, what <> " is not iterable")
      }
    }
    _ -> type_error_any(state, what <> " is not an object")
  }
}

// ============================================================================
// %WrapForValidIteratorPrototype%.next / .return
// ============================================================================

fn wrap_next(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use iterated, next_method <- require_wrap(this, state)
  use result, state <- state.try_call(state, next_method, iterated, [])
  #(state, Ok(result))
}

fn wrap_return(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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

fn to_array(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use this, next, state <- get_iterator_direct(this, state, "toArray")
  to_array_loop(state, this, next, [])
}

fn to_array_loop(
  state: State(host),
  iter: JsValue,
  next: JsValue,
  acc: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use this, next, func, state <- consumer_with_callback(
    this,
    args,
    state,
    "forEach",
  )
  for_each_loop(state, this, next, func, 0)
}

fn for_each_loop(
  state: State(host),
  iter: JsValue,
  next: JsValue,
  func: JsValue,
  counter: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  iter: JsValue,
  next: JsValue,
  func: JsValue,
  acc: JsValue,
  counter: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  match_on: Bool,
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use this, next, func, state <- consumer_with_callback(this, args, state, name)
  let #(state, res) = predicate_loop(state, this, next, func, 0, match_on)
  #(state, result.map(res, fn(m) { JsBool(option.is_some(m) == match_on) }))
}

/// Shared loop for some/every/find: step iterator, call predicate(v, idx),
/// early-exit (closing iterator) when truthiness == match_on. Some(v) = matched.
fn predicate_loop(
  state: State(host),
  iter: JsValue,
  next: JsValue,
  func: JsValue,
  counter: Int,
  match_on: Bool,
) -> #(State(host), Result(Option(JsValue), JsValue)) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  which: IgnoreSetterKey,
) -> #(State(host), Result(JsValue, JsValue)) {
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
  state: State(host),
  name: String,
  cont: fn(JsValue, JsValue, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
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

/// Shared prologue for forEach/reduce/some/every/find and the lazy helpers
/// (map/filter/flatMap): validate `this` is Object, validate callback
/// (closing `this` on failure WITHOUT having read `.next` — §27.1.4.5 step 3
/// requires callback validation BEFORE GetIteratorDirect), then
/// GetIteratorDirect.
fn consumer_with_callback(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  name: String,
  cont: fn(JsValue, JsValue, JsValue, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
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

/// Shared §7.4.6/§7.4.8 prefix: call next(obj), require the result is an
/// Object, read .done; continue with the result object and the done flag.
fn iterator_step_result(
  state: State(host),
  obj: JsValue,
  next_method: JsValue,
  cont: fn(JsValue, Bool, State(host)) -> #(State(host), Result(a, JsValue)),
) -> #(State(host), Result(a, JsValue)) {
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
          cont(result, value.is_truthy(done), state)
        }
        _ -> type_error_any(state, "Iterator result is not an object")
      }
  }
}

/// §7.4.8 IteratorStepValue: if done return None; else read .value.
fn iterator_step_value(
  state: State(host),
  obj: JsValue,
  next_method: JsValue,
) -> #(State(host), Result(Option(JsValue), JsValue)) {
  use result, done, state <- iterator_step_result(state, obj, next_method)
  case done {
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

/// §7.4.11 IteratorClose with throw completion: get .return; if callable, call
/// it (swallowing any throw — original error wins); return original error.
pub fn close_throw(
  state: State(host),
  obj: JsValue,
  original: JsValue,
) -> #(State(host), Result(a, JsValue)) {
  let #(state, _ignored) = call_return(state, obj)
  #(state, Error(original))
}

/// IteratorClose with a freshly-allocated TypeError.
pub fn close_throw_type(
  state: State(host),
  obj: JsValue,
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(err, state) = state.type_error_value(state, msg)
  close_throw(state, obj, err)
}

/// IteratorClose with a freshly-allocated RangeError.
pub fn close_throw_range(
  state: State(host),
  obj: JsValue,
  msg: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(err, state) = state.range_error_value(state, msg)
  close_throw(state, obj, err)
}

/// §7.4.11 IteratorClose with normal completion: get .return; if undefined →
/// Ok; else call it; if call throws → propagate; if result not Object →
/// TypeError; else Ok.
pub fn iterator_close_normal(
  state: State(host),
  obj: JsValue,
) -> #(State(host), Result(Nil, JsValue)) {
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
pub fn call_return(
  state: State(host),
  obj: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use ret_fn, state <- state.try_op(object.get_value_of(
    state,
    obj,
    Named("return"),
  ))
  case ret_fn {
    JsUndefined | JsNull -> #(state, Ok(JsUndefined))
    _ ->
      // §7.3.10 GetMethod step 3: a non-callable `return` property is a
      // TypeError. The re-entrant call path silently passes non-callables
      // through (legacy promise-reaction behavior), so check here.
      case is_callable(state.heap, ret_fn) {
        False -> type_error_any(state, "iterator.return is not a function")
        True -> {
          use result, state <- state.try_call(state, ret_fn, obj, [])
          #(state, Ok(result))
        }
      }
  }
}

/// §13.15.5.3 / §14.3.3 BindingRestElement: drain an already-obtained
/// iterator object into a fresh Array via repeated .next() — does NOT
/// re-GetIterator, so works for bare {next} iterators that don't inherit
/// %IteratorPrototype%. .next() throwing propagates without close (caller
/// has already seated the [[Done]] sentinel).
pub fn iterator_rest(
  state: State(host),
  iter: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use next, state <- state.try_op(object.get_value_of(
    state,
    iter,
    Named("next"),
  ))
  iterator_rest_loop(state, iter, next, [])
}

fn iterator_rest_loop(
  state: State(host),
  iter: JsValue,
  next: JsValue,
  acc: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, step) = iterator_step_value(state, iter, next)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> state.ok_array(state, list.reverse(acc))
    Ok(Some(v)) -> iterator_rest_loop(state, iter, next, [v, ..acc])
  }
}

// ============================================================================
// Collection-constructor abstract ops — §24.1.1.2 AddEntriesFromIterable and
// the value-iteration analogue used by the Set/WeakSet constructors. These
// are the ONE place the Map/WeakMap/Set/WeakSet constructors feed their
// iterable argument through: a real GetIterator, an observable per-element
// call of the user-reachable adder (`set` / `add`), and IteratorClose on any
// abrupt completion inside the loop.
// ============================================================================

/// §24.1.1.2 AddEntriesFromIterable ( target, iterable, adder ) — full
/// iterator protocol: GetIterator, then per entry Get "0"/"1" and call the
/// adder, closing the iterator on any abrupt completion inside the loop.
/// Used by the Map (§24.1.1.1) and WeakMap (§24.3.1.1) constructors.
pub fn add_entries_from_iterable(
  state: State(host),
  target: JsValue,
  iterable: JsValue,
  adder: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use iter_rec, state <- state.try_op(get_iterator_sync(state, iterable))
  let #(iter, next) = iter_rec
  add_entries_loop(state, target, iter, next, adder)
}

/// One IteratorStepValue + entry processing per iteration. Abrupt
/// completions from next()/Get(done)/Get(value) propagate without close
/// (§7.4.8 marks the iterator done); abrupt completions from the entry
/// reads or the adder call close the iterator first (§24.1.1.2 step 4).
fn add_entries_loop(
  state: State(host),
  target: JsValue,
  iter: JsValue,
  next: JsValue,
  adder: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use step, done, state <- iterator_step_result(state, iter, next)
  case done {
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
          close_throw_type(
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

/// Value-iteration analogue of AddEntriesFromIterable — §24.2.1.1 Set steps
/// 6-8 / §24.4.1.1 WeakSet steps 5-7: for each iterator value `v`, call
/// `adder(target, [v])`, closing the iterator if the adder throws
/// (IfAbruptCloseIterator). Returns `target` on normal completion.
pub fn add_values_from_iterable(
  state: State(host),
  target: JsValue,
  iterable: JsValue,
  adder: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use iter_rec, state <- state.try_op(get_iterator_sync(state, iterable))
  let #(iter, next) = iter_rec
  add_values_loop(state, target, iter, next, adder)
}

/// One IteratorStepValue + adder call per iteration. Abrupt completions from
/// next()/Get(done)/Get(value) propagate without close (§7.4.8); an abrupt
/// completion from the adder call closes the iterator first.
fn add_values_loop(
  state: State(host),
  target: JsValue,
  iter: JsValue,
  next: JsValue,
  adder: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  use step, done, state <- iterator_step_result(state, iter, next)
  case done {
    True -> #(state, Ok(target))
    False -> {
      use v, state <- state.try_op(object.get_value_of(
        state,
        step,
        Named("value"),
      ))
      use _add_result, state <- or_close(
        state.call(state, adder, target, [v]),
        iter,
      )
      add_values_loop(state, target, iter, next, adder)
    }
  }
}

/// Unwrap an op result, or IteratorClose with the thrown error (original
/// error wins over any error from the close itself — §7.4.11).
fn or_close(
  res: Result(#(a, State(host)), #(JsValue, State(host))),
  iter: JsValue,
  cont: fn(a, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case res {
    Ok(#(v, state)) -> cont(v, state)
    Error(#(thrown, state)) -> close_throw(state, iter, thrown)
  }
}

// ============================================================================
// Small helpers
// ============================================================================

/// §7.4.5 IteratorComplete + §7.4.6 IteratorValue: read {done, value} from an
/// iterator result object; TypeError if it isn't an object. Both property reads
/// can run user getters, so the returned State is threaded through and a getter
/// throw propagates as a step-level Thrown. Shared by the interpreter's
/// IteratorNext/yield* paths and the generator delegate-forwarding path.
/// (§7.4.8 IteratorStep must NOT read `value` when done — that variant lives
/// in interpreter.gleam as `read_iter_step_result`.)
pub fn read_iter_result(
  state: State(host),
  res: JsValue,
) -> Result(#(Bool, JsValue, State(host)), #(StepResult, JsValue, State(host))) {
  case res {
    JsObject(rref) -> {
      use #(done, state) <- result.try(
        state.rethrow(object.get_value(state, rref, Named("done"), res)),
      )
      use #(val, state) <- result.map(
        state.rethrow(object.get_value(state, rref, Named("value"), res)),
      )
      #(value.is_truthy(done), val, state)
    }
    _ -> state.throw_type_error(state, "Iterator result is not an object")
  }
}

/// CreateIterResultObject(value, done) — adapts common helper to State result.
fn create_iter_result(
  state: State(host),
  val: JsValue,
  done: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, v) =
    common.create_iter_result(state.heap, state.builtins, val, done)
  #(State(..state, heap:), Ok(v))
}

/// Unwrap `this` as an Object ref or TypeError. CPS — `use ref <- ...`.
fn require_object_of(
  this: JsValue,
  state: State(host),
  msg: String,
  cont: fn(Ref) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) -> cont(ref)
    _ -> state.type_error(state, msg)
  }
}

/// Unwrap `this` as a WrapForValidIteratorObject. CPS-style.
fn require_wrap(
  this: JsValue,
  state: State(host),
  cont: fn(JsValue, JsValue) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
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
  result: Result(#(a, State(host)), #(State(host), Result(JsValue, JsValue))),
  cont: fn(a, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case result {
    Ok(#(v, state)) -> cont(v, state)
    Error(r) -> r
  }
}

fn err_type(
  state: State(host),
  msg: String,
) -> Result(a, #(State(host), Result(JsValue, JsValue))) {
  Error(state.type_error(state, msg))
}

/// state.type_error but polymorphic in the Ok type — for callsites where the
/// surrounding Result's Ok type isn't JsValue (so state.type_error won't unify).
fn type_error_any(
  state: State(host),
  msg: String,
) -> #(State(host), Result(a, JsValue)) {
  let #(err, state) = state.type_error_value(state, msg)
  #(state, Error(err))
}

/// Step the underlying iterator. If next() throws, mark the helper done and
/// propagate WITHOUT calling close (the iterator is already broken).
fn after_step(
  state: State(host),
  ref: Ref,
  obj: JsValue,
  next: JsValue,
  cont: fn(Option(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, step) = iterator_step_value(state, obj, next)
  case step {
    Ok(v) -> cont(v, state)
    Error(thrown) -> #(mark_done(state, ref), Error(thrown))
  }
}

/// Mark a helper done and yield {value: undefined, done: true}.
fn finish(
  state: State(host),
  ref: Ref,
) -> #(State(host), Result(JsValue, JsValue)) {
  create_iter_result(mark_done(state, ref), JsUndefined, True)
}

fn mark_done(state: State(host), ref: Ref) -> State(host) {
  update_helper(state, ref, None, None, None, True)
}

fn write_count(state: State(host), ref: Ref, count: Int) -> State(host) {
  update_helper(state, ref, Some(count), None, None, False)
}

fn write_inner(
  state: State(host),
  ref: Ref,
  inner: JsValue,
  inner_next: JsValue,
) -> State(host) {
  update_helper(state, ref, None, Some(inner), Some(inner_next), False)
}

/// Rewrite the IteratorHelperObject kind in place. Gleam's record-update
/// can't narrow an ExoticKind variant, so we re-match and rebuild manually.
fn update_helper(
  state: State(host),
  ref: Ref,
  new_count: Option(Int),
  new_inner: Option(JsValue),
  new_inner_next: Option(JsValue),
  set_done: Bool,
) -> State(host) {
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

// ============================================================================
// Iterator.zip / Iterator.zipKeyed — tc39 joint-iteration proposal (stage 4)
// ============================================================================

/// Iterator.zip ( iterables [ , options ] )
fn zip(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let iterables = first_arg_or_undefined(args)
  // Step 1: If iterables is not an Object, throw a TypeError.
  use _ref <- require_object_of(
    iterables,
    state,
    "Iterator.zip iterables argument is not an object",
  )
  // Steps 2-7: GetOptionsObject + mode + paddingOption.
  use #(mode, padding_option), state <- after(zip_options(state, args, "zip"))
  // Step 10: GetIterator(iterables, sync).
  use #(input_iter, input_next), state <- after(
    map_thrown(get_iterator_sync(state, iterables)),
  )
  // Step 12: drain the input iterator, flattening each value to an iterator.
  use iters, state <- after(zip_collect(state, input_iter, input_next, []))
  // Steps 13-14: resolve the padding list ("longest" mode only).
  use padding, state <- after(case mode {
    ZipLongest -> zip_padding_iterated(state, padding_option, iters)
    _ -> Ok(#([], state))
  })
  // Steps 15-16: finishResults = CreateArrayFromList — keys: None.
  alloc_zip(state, iters, mode, padding, None)
}

/// Iterator.zipKeyed ( iterables [ , options ] )
fn zip_keyed(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  let iterables = first_arg_or_undefined(args)
  use iterables_ref <- require_object_of(
    iterables,
    state,
    "Iterator.zipKeyed iterables argument is not an object",
  )
  use #(mode, padding_option), state <- after(zip_options(
    state,
    args,
    "zipKeyed",
  ))
  // Step 10: allKeys = iterables.[[OwnPropertyKeys]]().
  use all_keys, state <- after(
    map_thrown(builtins_object.own_keys_stateful(state, iterables_ref)),
  )
  // Steps 11-12: filter to enumerable, non-undefined-valued properties.
  use #(keys, iters), state <- after(
    zip_keyed_collect(state, iterables, iterables_ref, all_keys, [], []),
  )
  // Step 14: padding read per key from the padding object.
  use padding, state <- after(case mode {
    ZipLongest -> zip_keyed_padding(state, padding_option, keys, iters)
    _ -> Ok(#([], state))
  })
  // Steps 15-16: finishResults = null-proto object keyed by `keys`.
  alloc_zip(state, iters, mode, padding, Some(keys))
}

/// Steps 2-7 shared by zip/zipKeyed: GetOptionsObject(options), Get "mode",
/// validate it, and Get "padding" when mode is "longest". Returns the parsed
/// mode plus the raw paddingOption (undefined or an Object).
fn zip_options(
  state: State(host),
  args: List(JsValue),
  name: String,
) -> Result(
  #(#(ZipMode, JsValue), State(host)),
  #(State(host), Result(JsValue, JsValue)),
) {
  let options = case args {
    [_, opts, ..] -> opts
    _ -> JsUndefined
  }
  case options {
    // GetOptionsObject: undefined → OrdinaryObjectCreate(null), whose Gets
    // all yield undefined — mode is "shortest" and padding is never read.
    JsUndefined -> Ok(#(#(ZipShortest, JsUndefined), state))
    JsObject(_) -> {
      use #(mode_val, state) <- result.try(
        map_thrown(object.get_value_of(state, options, Named("mode"))),
      )
      // Step 5: no ToString coercion — only undefined or the exact strings.
      use #(mode, state) <- result.try(case mode_val {
        JsUndefined | JsString("shortest") -> Ok(#(ZipShortest, state))
        JsString("longest") -> Ok(#(ZipLongest, state))
        JsString("strict") -> Ok(#(ZipStrict, state))
        _ ->
          err_type(
            state,
            "Iterator."
              <> name
              <> " mode must be \"shortest\", \"longest\", or \"strict\"",
          )
      })
      case mode {
        ZipLongest -> {
          use #(padding, state) <- result.try(
            map_thrown(object.get_value_of(state, options, Named("padding"))),
          )
          case padding {
            JsUndefined | JsObject(_) -> Ok(#(#(mode, padding), state))
            _ ->
              err_type(
                state,
                "Iterator." <> name <> " padding is not an object",
              )
          }
        }
        _ -> Ok(#(#(mode, JsUndefined), state))
      }
    }
    _ -> err_type(state, "Iterator." <> name <> " options is not an object")
  }
}

/// Adapt an ops-style Result (Error carries #(thrown, state)) into the
/// dispatch-shaped Error consumed by `after`.
fn map_thrown(
  r: Result(#(a, State(host)), #(JsValue, State(host))),
) -> Result(#(a, State(host)), #(State(host), Result(JsValue, JsValue))) {
  case r {
    Ok(v) -> Ok(v)
    Error(#(thrown, state)) -> Error(#(state, Error(thrown)))
  }
}

/// §7.4.3 GetIterator(obj, sync): the @@iterator method must be callable and
/// its result must be an Object; caches the next method (GetIteratorDirect).
fn get_iterator_sync(
  state: State(host),
  obj: JsValue,
) -> Result(#(#(JsValue, JsValue), State(host)), #(JsValue, State(host))) {
  use #(method, state) <- result.try(object.get_symbol_value_of(
    state,
    obj,
    value.symbol_iterator,
  ))
  case is_callable(state.heap, method) {
    False ->
      Error(state.type_error_value(
        state,
        object.inspect(obj, state.heap) <> " is not iterable",
      ))
    True -> {
      use #(iter, state) <- result.try(state.call(state, method, obj, []))
      case iter {
        JsObject(_) -> {
          use #(next, state) <- result.map(object.get_value_of(
            state,
            iter,
            Named("next"),
          ))
          #(#(iter, next), state)
        }
        _ ->
          Error(state.type_error_value(
            state,
            "Result of the Symbol.iterator method is not an object",
          ))
      }
    }
  }
}

/// Iterator.zip step 12: drain the iterables iterator, converting each value
/// via GetIteratorFlattenable(·, reject-primitives). On abrupt completions,
/// already-collected iterators are closed per IfAbruptCloseIterators.
fn zip_collect(
  state: State(host),
  input_iter: JsValue,
  input_next: JsValue,
  acc: List(#(JsValue, JsValue)),
) -> Result(
  #(List(#(JsValue, JsValue)), State(host)),
  #(State(host), Result(JsValue, JsValue)),
) {
  let #(state, step) = iterator_step_value(state, input_iter, input_next)
  case step {
    // IfAbruptCloseIterators(next, iters) — the input iterator is broken,
    // close only the collected iterators (in reverse order).
    Error(thrown) -> Error(close_all_throw(state, collected_iters(acc), thrown))
    Ok(None) -> Ok(#(list.reverse(acc), state))
    Ok(Some(v)) ->
      case get_iterator_flattenable(state, v, "Iterator.zip input") {
        // IfAbruptCloseIterators(iter, « inputIter » + iters): reverse order
        // closes the collected iterators first, then the input iterator.
        #(state, Error(thrown)) ->
          Error(close_all_throw(
            state,
            [input_iter, ..collected_iters(acc)],
            thrown,
          ))
        #(state, Ok(rec)) ->
          zip_collect(state, input_iter, input_next, [rec, ..acc])
      }
  }
}

/// Iterator-object list (spec order) from a reversed (iter, next) accumulator.
fn collected_iters(acc: List(#(JsValue, JsValue))) -> List(JsValue) {
  list.reverse(acc) |> list.map(fn(rec) { rec.0 })
}

/// Iterator.zip step 14: the "longest" padding list comes from ITERATING the
/// padding object (unlike zipKeyed, which Gets per key).
fn zip_padding_iterated(
  state: State(host),
  padding_option: JsValue,
  iters: List(#(JsValue, JsValue)),
) -> Result(
  #(List(JsValue), State(host)),
  #(State(host), Result(JsValue, JsValue)),
) {
  let iter_count = list.length(iters)
  case padding_option {
    JsUndefined -> Ok(#(list.repeat(JsUndefined, iter_count), state))
    _ -> {
      let opened = list.map(iters, fn(rec) { rec.0 })
      case get_iterator_sync(state, padding_option) {
        Error(#(thrown, state)) -> Error(close_all_throw(state, opened, thrown))
        Ok(#(#(pad_iter, pad_next), state)) ->
          zip_padding_loop(state, pad_iter, pad_next, opened, iter_count, [])
      }
    }
  }
}

fn zip_padding_loop(
  state: State(host),
  pad_iter: JsValue,
  pad_next: JsValue,
  opened: List(JsValue),
  remaining: Int,
  acc: List(JsValue),
) -> Result(
  #(List(JsValue), State(host)),
  #(State(host), Result(JsValue, JsValue)),
) {
  case remaining <= 0 {
    True -> {
      // usingIterator is still true — IteratorClose(paddingIter, normal).
      let #(state, close_res) = iterator_close_normal(state, pad_iter)
      case close_res {
        Error(thrown) -> Error(close_all_throw(state, opened, thrown))
        Ok(Nil) -> Ok(#(list.reverse(acc), state))
      }
    }
    False -> {
      let #(state, step) = iterator_step_value(state, pad_iter, pad_next)
      case step {
        Error(thrown) -> Error(close_all_throw(state, opened, thrown))
        // Padding iterator exhausted: fill the rest with undefined; the
        // padding iterator is NOT closed (usingIterator becomes false).
        Ok(None) ->
          Ok(#(
            list.append(list.reverse(acc), list.repeat(JsUndefined, remaining)),
            state,
          ))
        Ok(Some(v)) ->
          zip_padding_loop(state, pad_iter, pad_next, opened, remaining - 1, [
            v,
            ..acc
          ])
      }
    }
  }
}

/// Iterator.zipKeyed step 12: walk allKeys, keeping enumerable own properties
/// whose value is not undefined; flatten each value to an iterator record.
fn zip_keyed_collect(
  state: State(host),
  iterables: JsValue,
  iterables_ref: Ref,
  keys_left: List(JsValue),
  keys_acc: List(JsValue),
  iters_acc: List(#(JsValue, JsValue)),
) -> Result(
  #(#(List(JsValue), List(#(JsValue, JsValue))), State(host)),
  #(State(host), Result(JsValue, JsValue)),
) {
  case keys_left {
    [] -> Ok(#(#(list.reverse(keys_acc), list.reverse(iters_acc)), state))
    [key, ..rest] ->
      case
        builtins_object.get_own_property_stateful(state, iterables_ref, key)
      {
        Error(#(thrown, state)) ->
          Error(close_all_throw(state, collected_iters(iters_acc), thrown))
        Ok(#(desc, state)) -> {
          let enumerable =
            option.map(desc, value.prop_enumerable) |> option.unwrap(False)
          case enumerable {
            False ->
              zip_keyed_collect(
                state,
                iterables,
                iterables_ref,
                rest,
                keys_acc,
                iters_acc,
              )
            True ->
              case get_keyed(state, iterables, key) {
                Error(#(thrown, state)) ->
                  Error(close_all_throw(
                    state,
                    collected_iters(iters_acc),
                    thrown,
                  ))
                Ok(#(JsUndefined, state)) ->
                  zip_keyed_collect(
                    state,
                    iterables,
                    iterables_ref,
                    rest,
                    keys_acc,
                    iters_acc,
                  )
                Ok(#(v, state)) ->
                  case
                    get_iterator_flattenable(
                      state,
                      v,
                      "Iterator.zipKeyed input",
                    )
                  {
                    #(state, Error(thrown)) ->
                      Error(close_all_throw(
                        state,
                        collected_iters(iters_acc),
                        thrown,
                      ))
                    #(state, Ok(rec)) ->
                      zip_keyed_collect(
                        state,
                        iterables,
                        iterables_ref,
                        rest,
                        [key, ..keys_acc],
                        [rec, ..iters_acc],
                      )
                  }
              }
          }
        }
      }
  }
}

/// [[Get]] keyed by a property-key JsValue (JsString or JsSymbol — the only
/// shapes [[OwnPropertyKeys]] yields).
fn get_keyed(
  state: State(host),
  obj: JsValue,
  key: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case key {
    JsSymbol(sym) -> object.get_symbol_value_of(state, obj, sym)
    JsString(s) -> object.get_value_of(state, obj, value.canonical_key(s))
    // Unreachable: own_keys_stateful only yields strings and symbols.
    _ -> Ok(#(JsUndefined, state))
  }
}

/// Iterator.zipKeyed step 14: the "longest" padding values are read per key
/// from the padding object.
fn zip_keyed_padding(
  state: State(host),
  padding_option: JsValue,
  keys: List(JsValue),
  iters: List(#(JsValue, JsValue)),
) -> Result(
  #(List(JsValue), State(host)),
  #(State(host), Result(JsValue, JsValue)),
) {
  case padding_option {
    JsUndefined -> Ok(#(list.repeat(JsUndefined, list.length(iters)), state))
    _ -> {
      let opened = list.map(iters, fn(rec) { rec.0 })
      zip_keyed_padding_loop(state, padding_option, opened, keys, [])
    }
  }
}

fn zip_keyed_padding_loop(
  state: State(host),
  padding_option: JsValue,
  opened: List(JsValue),
  keys_left: List(JsValue),
  acc: List(JsValue),
) -> Result(
  #(List(JsValue), State(host)),
  #(State(host), Result(JsValue, JsValue)),
) {
  case keys_left {
    [] -> Ok(#(list.reverse(acc), state))
    [key, ..rest] ->
      case get_keyed(state, padding_option, key) {
        Error(#(thrown, state)) -> Error(close_all_throw(state, opened, thrown))
        Ok(#(v, state)) ->
          zip_keyed_padding_loop(state, padding_option, opened, rest, [v, ..acc])
      }
  }
}

/// Allocate the IteratorZip helper object on %IteratorHelperPrototype%.
fn alloc_zip(
  state: State(host),
  iters: List(#(JsValue, JsValue)),
  mode: ZipMode,
  padding: List(JsValue),
  keys: Option(List(JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let members =
    list.map(iters, fn(rec) { ZipOpen(iter: rec.0, next_method: rec.1) })
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      IteratorZipObject(
        members:,
        mode:,
        padding:,
        keys:,
        done: False,
        running: False,
        started: False,
      ),
      state.builtins.iterator_helper_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

// ============================================================================
// IteratorZip stepping — %IteratorHelperPrototype%.next/.return for zip
// ============================================================================

fn zip_next(
  state: State(host),
  ref: Ref,
  members: List(ZipMember),
  mode: ZipMode,
  padding: List(JsValue),
  keys: Option(List(JsValue)),
  done: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case done, members {
    True, _ -> create_iter_result(state, JsUndefined, True)
    // IteratorZip closure step 1: iterCount = 0 → done forever.
    False, [] -> finish_zip(state, ref)
    False, _ -> zip_round(state, ref, mode, keys, [], members, padding, [])
  }
}

/// One result-tuple round: step every member in order, applying the
/// mode-specific termination rules from the IteratorZip abstract operation.
/// `prev` (reversed) holds processed members; `rest`/`pad_rest` are the
/// unprocessed members and their index-aligned padding values.
fn zip_round(
  state: State(host),
  ref: Ref,
  mode: ZipMode,
  keys: Option(List(JsValue)),
  prev: List(ZipMember),
  rest: List(ZipMember),
  pad_rest: List(JsValue),
  results: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  case rest {
    [] -> zip_emit(state, ref, keys, list.reverse(prev), list.reverse(results))
    [member, ..tail] -> {
      let #(pad_here, pad_tail) = case pad_rest {
        [p, ..ps] -> #(p, ps)
        [] -> #(JsUndefined, [])
      }
      case member {
        // Spec: iters[i] is null → result = padding[i].
        ZipExhausted ->
          zip_round(state, ref, mode, keys, [member, ..prev], tail, pad_tail, [
            pad_here,
            ..results
          ])
        ZipOpen(iter:, next_method:) -> {
          let #(state, step) = iterator_step_value(state, iter, next_method)
          case step {
            Error(thrown) -> {
              let state = zip_mark_done(state, ref)
              close_all_throw(state, open_others(prev, tail), thrown)
            }
            Ok(Some(v)) ->
              zip_round(
                state,
                ref,
                mode,
                keys,
                [member, ..prev],
                tail,
                pad_tail,
                [v, ..results],
              )
            Ok(None) ->
              case mode {
                ZipShortest -> {
                  let state = zip_mark_done(state, ref)
                  let #(state, close_res) =
                    close_all_normal(state, open_others(prev, tail))
                  case close_res {
                    Error(e) -> #(state, Error(e))
                    Ok(Nil) -> create_iter_result(state, JsUndefined, True)
                  }
                }
                ZipStrict ->
                  case prev {
                    // i = 0: every other iterator must be done too.
                    [] -> zip_strict_check(state, ref, tail)
                    _ -> zip_strict_throw(state, ref, open_others(prev, tail))
                  }
                ZipLongest ->
                  case open_others(prev, tail) {
                    // openIters is empty — the generator just completes.
                    [] -> finish_zip(state, ref)
                    _ ->
                      zip_round(
                        state,
                        ref,
                        mode,
                        keys,
                        [ZipExhausted, ..prev],
                        tail,
                        pad_tail,
                        [pad_here, ..results],
                      )
                  }
              }
          }
        }
      }
    }
  }
}

/// IteratorZip "strict" mode, first iterator done: IteratorStep each
/// remaining iterator — all must be done, or TypeError.
fn zip_strict_check(
  state: State(host),
  ref: Ref,
  rest: List(ZipMember),
) -> #(State(host), Result(JsValue, JsValue)) {
  case rest {
    [] -> finish_zip(state, ref)
    // Unreachable: "strict" mode never exhausts members in place.
    [ZipExhausted, ..tail] -> zip_strict_check(state, ref, tail)
    [ZipOpen(iter:, next_method:), ..tail] -> {
      let #(state, step) = iterator_step_done(state, iter, next_method)
      case step {
        Error(thrown) -> {
          let state = zip_mark_done(state, ref)
          close_all_throw(state, open_members(tail), thrown)
        }
        Ok(True) -> zip_strict_check(state, ref, tail)
        // Not done — this iterator is still in openIters and is closed too.
        Ok(False) -> zip_strict_throw(state, ref, [iter, ..open_members(tail)])
      }
    }
  }
}

/// IteratorCloseAll(openIters, ThrowCompletion(TypeError)) for strict-mode
/// length mismatches.
fn zip_strict_throw(
  state: State(host),
  ref: Ref,
  open: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let state = zip_mark_done(state, ref)
  let #(terr, state) =
    state.type_error_value(
      state,
      "Iterator.zip strict mode: iterators have different lengths",
    )
  close_all_throw(state, open, terr)
}

/// §7.4.6 IteratorStep: call next() and read only `done` (never `value`).
fn iterator_step_done(
  state: State(host),
  obj: JsValue,
  next_method: JsValue,
) -> #(State(host), Result(Bool, JsValue)) {
  use _result, done, state <- iterator_step_result(state, obj, next_method)
  #(state, Ok(done))
}

/// Finish a round: persist longest-mode exhaustion transitions, then build
/// the finishResults value (array for zip, null-proto object for zipKeyed).
fn zip_emit(
  state: State(host),
  ref: Ref,
  keys: Option(List(JsValue)),
  members: List(ZipMember),
  results: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let state = zip_write_members(state, ref, members)
  case keys {
    None -> {
      let #(heap, arr) =
        common.alloc_array(state.heap, results, state.builtins.array.prototype)
      create_iter_result(State(..state, heap:), JsObject(arr), False)
    }
    Some(ks) -> {
      let #(heap, obj) = alloc_zip_keyed_result(state.heap, ks, results)
      create_iter_result(State(..state, heap:), JsObject(obj), False)
    }
  }
}

/// zipKeyed finishResults: OrdinaryObjectCreate(null) +
/// CreateDataPropertyOrThrow(obj, keys[i], results[i]) for each column.
fn alloc_zip_keyed_result(
  h: Heap(host),
  keys: List(JsValue),
  results: List(JsValue),
) -> #(Heap(host), Ref) {
  let #(props, sym_props) =
    list.zip(keys, results)
    |> list.fold(#(dict.new(), []), fn(acc, pair) {
      let #(props, syms) = acc
      let #(key, v) = pair
      case key {
        JsSymbol(sym) -> #(
          props,
          list.key_set(syms, sym, value.data_property(v)),
        )
        JsString(s) -> #(
          dict.insert(props, value.canonical_key(s), value.data_property(v)),
          syms,
        )
        // Unreachable: keys are only ever strings or symbols.
        _ -> acc
      }
    })
  heap.alloc(
    h,
    ObjectSlot(
      kind: OrdinaryObject,
      properties: props,
      elements: elements.new(),
      prototype: None,
      symbol_properties: sym_props,
      extensible: True,
    ),
  )
}

/// %IteratorHelperPrototype%.return for zip helpers: close every still-open
/// underlying iterator (IteratorCloseAll, reverse order).
fn zip_return(
  state: State(host),
  ref: Ref,
  members: List(ZipMember),
  done: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case done {
    True -> create_iter_result(state, JsUndefined, True)
    False -> {
      let state = zip_mark_done(state, ref)
      let #(state, close_res) = close_all_normal(state, open_members(members))
      case close_res {
        Error(e) -> #(state, Error(e))
        Ok(Nil) -> create_iter_result(state, JsUndefined, True)
      }
    }
  }
}

/// Open iterator objects (spec's openIters) in order, given the reversed
/// processed prefix and the unprocessed tail — excluding the current member.
fn open_others(prev: List(ZipMember), tail: List(ZipMember)) -> List(JsValue) {
  list.append(open_members(list.reverse(prev)), open_members(tail))
}

fn open_members(members: List(ZipMember)) -> List(JsValue) {
  list.filter_map(members, fn(m) {
    case m {
      ZipOpen(iter:, ..) -> Ok(iter)
      ZipExhausted -> Error(Nil)
    }
  })
}

/// IteratorCloseAll (joint-iteration proposal) with a pending throw: close
/// every iterator in REVERSE list order — errors from .return are swallowed
/// (the original error wins) — then rethrow the original.
fn close_all_throw(
  state: State(host),
  iters: List(JsValue),
  original: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  let state =
    list.fold(list.reverse(iters), state, fn(state, it) {
      // IteratorClose with a throw completion swallows .return errors.
      let #(state, _superseded_by_original) = call_return(state, it)
      state
    })
  #(state, Error(original))
}

/// IteratorCloseAll with a normal/return completion: close in reverse list
/// order; the first abrupt close result wins and the remaining iterators are
/// closed with their errors swallowed.
fn close_all_normal(
  state: State(host),
  iters: List(JsValue),
) -> #(State(host), Result(Nil, JsValue)) {
  list.fold(list.reverse(iters), #(state, Ok(Nil)), fn(acc, it) {
    let #(state, completion) = acc
    case completion {
      Ok(Nil) -> iterator_close_normal(state, it)
      Error(e) -> {
        let #(state, _superseded_by_first_error) = call_return(state, it)
        #(state, Error(e))
      }
    }
  })
}

fn finish_zip(
  state: State(host),
  ref: Ref,
) -> #(State(host), Result(JsValue, JsValue)) {
  create_iter_result(zip_mark_done(state, ref), JsUndefined, True)
}

fn zip_mark_done(state: State(host), ref: Ref) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: IteratorZipObject(..) as kind, ..) ->
          ObjectSlot(..slot, kind: IteratorZipObject(..kind, done: True))
        other -> other
      }
    })
  State(..state, heap:)
}

fn zip_write_members(
  state: State(host),
  ref: Ref,
  members: List(ZipMember),
) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: IteratorZipObject(..) as kind, ..) ->
          ObjectSlot(..slot, kind: IteratorZipObject(..kind, members:))
        other -> other
      }
    })
  State(..state, heap:)
}

// ============================================================================
// Iterator.concat — tc39 iterator-sequencing proposal (stage 4)
// ============================================================================

/// Iterator.concat ( ...items )
fn concat(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  concat_validate(state, args, [])
}

/// Step 2: every item must be an Object with a callable @@iterator method;
/// collect the (openMethod, iterable) records in order.
fn concat_validate(
  state: State(host),
  items: List(JsValue),
  acc: List(#(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case items {
    [] -> {
      let #(heap, ref) =
        common.alloc_wrapper(
          state.heap,
          IteratorConcatObject(
            remaining: list.reverse(acc),
            inner: None,
            done: False,
            running: False,
          ),
          state.builtins.iterator_helper_proto,
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
    [item, ..rest] ->
      case item {
        JsObject(_) -> {
          // GetMethod(item, @@iterator): undefined/null → TypeError,
          // non-callable → TypeError.
          use method, state <- state.try_op(object.get_symbol_value_of(
            state,
            item,
            value.symbol_iterator,
          ))
          case method {
            JsUndefined | JsNull ->
              state.type_error(
                state,
                "Iterator.concat argument is not iterable",
              )
            _ ->
              case is_callable(state.heap, method) {
                True -> concat_validate(state, rest, [#(method, item), ..acc])
                False ->
                  state.type_error(
                    state,
                    "Iterator.concat argument [Symbol.iterator] is not callable",
                  )
              }
          }
        }
        _ ->
          state.type_error(state, "Iterator.concat argument is not an object")
      }
  }
}

/// %IteratorHelperPrototype%.next for concat helpers: pull from the current
/// inner iterator, opening the next iterable as each one is exhausted.
fn concat_next(
  state: State(host),
  ref: Ref,
  remaining: List(#(JsValue, JsValue)),
  inner: Option(#(JsValue, JsValue)),
  done: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case done {
    True -> create_iter_result(state, JsUndefined, True)
    False ->
      case inner {
        Some(#(iter, next_method)) -> {
          let #(state, step) = iterator_step_value(state, iter, next_method)
          case step {
            // Abrupt completion from the closure completes the generator.
            Error(thrown) -> #(concat_mark_done(state, ref), Error(thrown))
            Ok(Some(v)) -> create_iter_result(state, v, False)
            Ok(None) -> {
              let state = concat_write(state, ref, remaining, None)
              concat_open_next(state, ref, remaining)
            }
          }
        }
        None -> concat_open_next(state, ref, remaining)
      }
  }
}

/// Open the next iterable's iterator: iter = Call(openMethod, iterable),
/// require an Object, GetIteratorDirect, then pull its first value.
fn concat_open_next(
  state: State(host),
  ref: Ref,
  remaining: List(#(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case remaining {
    [] -> create_iter_result(concat_mark_done(state, ref), JsUndefined, True)
    [#(method, iterable), ..rest] ->
      case state.call(state, method, iterable, []) {
        Error(#(thrown, state)) -> #(
          concat_mark_done(state, ref),
          Error(thrown),
        )
        Ok(#(iter, state)) ->
          case iter {
            JsObject(_) ->
              case object.get_value_of(state, iter, Named("next")) {
                Error(#(thrown, state)) -> #(
                  concat_mark_done(state, ref),
                  Error(thrown),
                )
                Ok(#(next_method, state)) -> {
                  let state =
                    concat_write(state, ref, rest, Some(#(iter, next_method)))
                  concat_next(
                    state,
                    ref,
                    rest,
                    Some(#(iter, next_method)),
                    False,
                  )
                }
              }
            _ ->
              state.type_error(
                concat_mark_done(state, ref),
                "Result of the Symbol.iterator method is not an object",
              )
          }
      }
  }
}

/// %IteratorHelperPrototype%.return for concat helpers: close the currently
/// open inner iterator (if any).
fn concat_return(
  state: State(host),
  ref: Ref,
  inner: Option(#(JsValue, JsValue)),
  done: Bool,
) -> #(State(host), Result(JsValue, JsValue)) {
  case done {
    True -> create_iter_result(state, JsUndefined, True)
    False -> {
      let state = concat_mark_done(state, ref)
      case inner {
        Some(#(iter, _next_method)) -> {
          let #(state, close_res) = iterator_close_normal(state, iter)
          case close_res {
            Error(e) -> #(state, Error(e))
            Ok(Nil) -> create_iter_result(state, JsUndefined, True)
          }
        }
        None -> create_iter_result(state, JsUndefined, True)
      }
    }
  }
}

fn concat_mark_done(state: State(host), ref: Ref) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: IteratorConcatObject(..) as kind, ..) ->
          ObjectSlot(
            ..slot,
            kind: IteratorConcatObject(..kind, inner: None, done: True),
          )
        other -> other
      }
    })
  State(..state, heap:)
}

fn concat_write(
  state: State(host),
  ref: Ref,
  remaining: List(#(JsValue, JsValue)),
  inner: Option(#(JsValue, JsValue)),
) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: IteratorConcatObject(..) as kind, ..) ->
          ObjectSlot(
            ..slot,
            kind: IteratorConcatObject(..kind, remaining:, inner:),
          )
        other -> other
      }
    })
  State(..state, heap:)
}
