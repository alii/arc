/// ES2025 §27.1 Iteration — the Iterator constructor, Iterator.from, and
/// Iterator.prototype helper methods (map, filter, take, drop, flatMap,
/// toArray, forEach, reduce, some, every, find).
///
/// Prior art: QuickJS quickjs.c js_iterator_* (bellard/quickjs).
import arc/vm/builtins/common.{type BuiltinType}
import arc/vm/builtins/helpers.{first_arg_or_undefined, is_callable}
import arc/vm/builtins/iter_protocol.{IterateStrings, RejectPrimitives}
import arc/vm/builtins/object as builtins_object
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/key.{Named}
import arc/vm/limits
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type ConcatItem, type GeneratorState, type HelperBody, type IteratorHelperKind,
  type IteratorNativeFn, type IteratorRecord, type JsValue, type ObjectKey,
  type Ref, type ZipMember, type ZipMode, ClassicHelper, Completed, ConcatHelper,
  ConcatItem, Dispatch, Executing, Finite, HelperDrop, HelperFilter,
  HelperFlatMap, HelperMap, HelperTake, Infinity, IteratorConcat,
  IteratorConstructor, IteratorFrom, IteratorHelperNext, IteratorHelperObject,
  IteratorHelperReturn, IteratorNative, IteratorProtoGetConstructor,
  IteratorProtoGetToStringTag, IteratorProtoSetConstructor,
  IteratorProtoSetToStringTag, IteratorPrototypeDrop, IteratorPrototypeEvery,
  IteratorPrototypeFilter, IteratorPrototypeFind, IteratorPrototypeFlatMap,
  IteratorPrototypeForEach, IteratorPrototypeMap, IteratorPrototypeReduce,
  IteratorPrototypeSome, IteratorPrototypeTake, IteratorPrototypeToArray,
  IteratorZip, IteratorZipKeyed, JsBool, JsNull, JsObject, JsString, JsUndefined,
  NaN, NegInfinity, ObjectSlot, OrdinaryObject, StringPropKey, SuspendedStart,
  SuspendedYield, SymbolPropKey, WrapForValidIteratorNext,
  WrapForValidIteratorObject, WrapForValidIteratorReturn, ZipExhausted,
  ZipHelper, ZipLongest, ZipOpen, ZipShortest, ZipStrict,
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
      lazy_helper(
        this,
        args,
        state,
        fn(func) { HelperFlatMap(func:, inner: None) },
        "flatMap",
      )
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
      // §10.1.13.1 OrdinaryCreateFromConstructor(NewTarget, %Iterator.prototype%)
      // — step 2's `? Get(newTarget, "prototype")` is a real [[Get]], so a proxy
      // subclass's trap and an accessor `prototype` both fire (and may throw).
      use proto, state <- object.proto_from_new_target(
        state,
        nt,
        state.builtins.iterator.prototype,
      )
      let #(h, ref) = common.alloc_pojo(state.heap, proto, [])
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
  // §27.1.2.1 step 1: GetIteratorFlattenable(O, iterate-string-primitives).
  // The ONE implementation of §7.4.13 lives in `iter_protocol`; `IterateStrings`
  // is what makes `Iterator.from("ab")` legal where `Iterator.zip("ab")` is not.
  use rec, state <- state.try_op(iter_protocol.get_iterator_flattenable(
    state,
    o,
    IterateStrings,
    "Iterator.from argument",
  ))
  // OrdinaryHasInstance(%Iterator%, iterator) — §7.3.22 step 4 walks the
  // chain starting from O's [[Prototype]], NOT from O itself. Starting at
  // O would wrongly treat %Iterator.prototype% ITSELF as an instance.
  // GetIteratorFlattenable guarantees `iter` is an Object.
  let target = state.builtins.iterator.prototype
  let start = case rec.iterator {
    JsObject(iter_ref) ->
      case heap.read(state.heap, iter_ref) {
        Some(ObjectSlot(prototype: proto, ..)) -> proto
        _ -> None
      }
    _ -> None
  }
  case proto_chain_contains(state.heap, start, target) {
    True -> #(state, Ok(rec.iterator))
    False -> {
      let #(heap, ref) =
        common.alloc_wrapper(
          state.heap,
          WrapForValidIteratorObject(
            iterated: rec.iterator,
            next_method: rec.next_method,
          ),
          state.builtins.wrap_for_valid_iterator_proto,
        )
      #(State(..state, heap:), Ok(JsObject(ref)))
    }
  }
}

/// §7.3.22 OrdinaryHasInstance step 4: walk the prototype chain starting at
/// `start` (already the object's [[Prototype]], NOT the object itself)
/// looking for `target`.
fn proto_chain_contains(
  h: Heap(host),
  start: Option(Ref),
  target: Ref,
) -> Bool {
  case start {
    None -> False
    Some(obj) ->
      case obj.id == target.id {
        True -> True
        False ->
          case heap.read(h, obj) {
            Some(ObjectSlot(prototype: proto, ..)) ->
              proto_chain_contains(h, proto, target)
            _ -> False
          }
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
  make_kind: fn(JsValue) -> IteratorHelperKind,
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use rec, func, state <- consumer_with_callback(this, args, state, name)
  alloc_helper(state, make_kind(func), rec)
}

// ============================================================================
// Lazy producers — Iterator.prototype.{take,drop}
// ============================================================================

fn take_or_drop(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
  make_kind: fn(Int) -> IteratorHelperKind,
  name: String,
) -> #(State(host), Result(JsValue, JsValue)) {
  use _this_ref <- require_object_of(
    this,
    state,
    "Iterator.prototype." <> name <> " called on non-object",
  )
  // §27.1.4.10 step 3-6: ToNumber(limit) BEFORE GetIteratorDirect. On any
  // abrupt completion / NaN / negative, close `this` then throw.
  use remaining, state <- state.try_op(coerce_limit(state, this, args, name))
  use rec, state <- state.try_op(get_iterator_direct(state, this, name))
  alloc_helper(state, make_kind(remaining), rec)
}

/// ES2025 §27.1.3.10/12 step 3-6: ToIntegerOrInfinity(ToNumber(limit)) with
/// NaN/negative → RangeError. On any abrupt completion, close `this` first.
fn coerce_limit(
  state: State(host),
  this: JsValue,
  args: List(JsValue),
  name: String,
) -> Result(#(Int, State(host)), #(JsValue, State(host))) {
  let arg = first_arg_or_undefined(args)
  // ToNumber: ToPrimitive(NumberHint) for objects (so valueOf/@@toPrimitive
  // can throw user errors), then primitive → JsNum.
  let prim_result = case arg {
    JsObject(_) -> coerce.to_primitive(state, arg, coerce.NumberHint)
    other -> Ok(#(other, state))
  }
  case prim_result {
    Error(#(thrown, state)) ->
      Error(iter_protocol.close_and_throw(state, this, thrown))
    Ok(#(prim, state)) ->
      case value.to_number(prim) {
        Error(value.BigIntNotConvertible) ->
          Error(close_and_throw_type(
            state,
            this,
            "Cannot convert BigInt to number",
          ))
        Error(value.SymbolNotConvertible) ->
          Error(close_and_throw_type(
            state,
            this,
            "Cannot convert Symbol to number",
          ))
        Ok(NaN) ->
          Error(close_and_throw_range(state, this, name <> " limit is NaN"))
        Ok(Infinity) -> Ok(#(limits.max_safe_integer, state))
        Ok(NegInfinity) ->
          Error(close_and_throw_range(state, this, name <> " limit is negative"))
        Ok(Finite(f)) ->
          case f <. 0.0 {
            True ->
              Error(close_and_throw_range(
                state,
                this,
                name <> " limit is negative",
              ))
            False -> Ok(#(value.float_to_int(f), state))
          }
      }
  }
}

fn alloc_helper(
  state: State(host),
  kind: IteratorHelperKind,
  underlying: IteratorRecord,
) -> #(State(host), Result(JsValue, JsValue)) {
  alloc_helper_body(state, ClassicHelper(kind:, underlying:, counter: 0))
}

/// Allocate a fresh %IteratorHelper% (whatever the flavour) at suspended-start.
fn alloc_helper_body(
  state: State(host),
  body: HelperBody,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(heap, ref) =
    common.alloc_wrapper(
      state.heap,
      IteratorHelperObject(gen_state: SuspendedStart, body:),
      state.builtins.iterator_helper_proto,
    )
  #(State(..state, heap:), Ok(JsObject(ref)))
}

// ============================================================================
// %IteratorHelperPrototype%.next / .return
// ============================================================================

const helper_receiver_err = "Iterator Helper method called on incompatible receiver"

/// §27.1.4.1 %IteratorHelperPrototype%.next: GeneratorResume(this, undefined,
/// "Iterator Helper"). All three helper flavours (classic map/filter/take/
/// drop/flatMap, zip, concat) share the one [[GeneratorState]] machine.
fn helper_next(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, gen_state, body <- require_helper(this, state)
  use state <- resume(state, ref, gen_state)
  case body {
    ClassicHelper(kind:, underlying:, counter:) ->
      classic_helper_next(state, ref, kind, underlying, counter)
    ZipHelper(members:, mode:, keys:) ->
      zip_next(state, ref, members, mode, keys)
    ConcatHelper(remaining:, inner:) ->
      concat_next(state, ref, remaining, inner)
  }
}

/// Unwrap `this` as an %IteratorHelper% — its [[GeneratorState]] and its body.
/// Every helper flavour lands here, so `helper_next`/`helper_return` cannot
/// disagree about which receivers are valid.
fn require_helper(
  this: JsValue,
  state: State(host),
  cont: fn(Ref, GeneratorState, HelperBody) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: IteratorHelperObject(gen_state:, body:), ..)) ->
          cont(ref, gen_state, body)
        _ -> state.type_error(state, helper_receiver_err)
      }
    _ -> state.type_error(state, helper_receiver_err)
  }
}

const helper_running_err = "Iterator Helper is currently being iterated"

/// §27.5.3.3 GeneratorResume — the .next() half of the helper generator's
/// lifecycle, shared by all three helper flavours.
///
/// - `Executing`: GeneratorValidate rejects the reentrant .next() (a callback,
///   or a `next`/`return` method the body invoked, calling back into us).
/// - `Completed`: latched forever — {value: undefined, done: true}, `body`
///   never runs again.
/// - suspended: `body` runs marked `Executing`; if it yielded a value the
///   helper is re-suspended, and if it finished the generator (`mark_done`) it
///   stays `Completed`.
fn resume(
  state: State(host),
  ref: Ref,
  gen_state: GeneratorState,
  body: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case gen_state {
    Executing -> state.type_error(state, helper_running_err)
    Completed -> create_iter_result(state, JsUndefined, True)
    SuspendedStart | SuspendedYield -> {
      let #(state, res) = body(set_gen_state(state, ref, Executing))
      #(map_gen_state(state, ref, suspend_if_executing), res)
    }
  }
}

/// §27.5.3.4 GeneratorResumeAbrupt(·, ReturnCompletion(undefined), ·) — the
/// .return() half. `body` closes the helper's underlying iterator(s) and MUST
/// finish by latching `Completed`.
///
/// The two suspended states differ, and both differences are observable:
/// suspended-start (§27.1.4.2 step 4) completes the generator BEFORE closing,
/// so a reentrant next/return during the close sees a completed generator;
/// suspended-yield resumes the generator body, so the same reentrant call sees
/// `Executing` and throws.
fn resume_abrupt(
  state: State(host),
  ref: Ref,
  gen_state: GeneratorState,
  body: fn(State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case gen_state {
    Executing -> state.type_error(state, helper_running_err)
    Completed -> create_iter_result(state, JsUndefined, True)
    SuspendedStart -> body(set_gen_state(state, ref, Completed))
    SuspendedYield -> body(set_gen_state(state, ref, Executing))
  }
}

fn suspend_if_executing(gen_state: GeneratorState) -> GeneratorState {
  case gen_state {
    Executing -> SuspendedYield
    SuspendedStart | SuspendedYield | Completed -> gen_state
  }
}

/// Overwrite the helper's [[GeneratorState]], whatever flavour of body it holds.
fn set_gen_state(
  state: State(host),
  ref: Ref,
  gen_state: GeneratorState,
) -> State(host) {
  map_gen_state(state, ref, fn(_previous) { gen_state })
}

/// The ONE lifecycle write for every %IteratorHelper%: `gen_state` is a field
/// of `IteratorHelperObject` itself, not of each flavour's body, so there is
/// no per-flavour arm here to forget.
fn map_gen_state(
  state: State(host),
  ref: Ref,
  update: fn(GeneratorState) -> GeneratorState,
) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: IteratorHelperObject(gen_state:, ..) as kind, ..) ->
          ObjectSlot(
            ..slot,
            kind: IteratorHelperObject(..kind, gen_state: update(gen_state)),
          )
        other -> other
      }
    })
  State(..state, heap:)
}

fn classic_helper_next(
  state: State(host),
  ref: Ref,
  kind: IteratorHelperKind,
  underlying: IteratorRecord,
  counter: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  case kind {
    HelperMap(func:) -> step_map(state, ref, underlying, func, counter)
    HelperFilter(func:) -> step_filter(state, ref, underlying, func, counter)
    HelperTake(remaining:) -> step_take(state, ref, underlying, remaining)
    HelperDrop(remaining:) -> step_drop(state, ref, underlying, remaining)
    HelperFlatMap(func:, inner:) ->
      step_flat_map(state, ref, underlying, func, inner, counter)
  }
}

/// §27.1.4.2 %IteratorHelperPrototype%.return.
fn helper_return(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use ref, gen_state, body <- require_helper(this, state)
  use state <- resume_abrupt(state, ref, gen_state)
  case body {
    ClassicHelper(kind:, underlying:, counter: _) ->
      classic_helper_return(state, ref, kind, underlying)
    ZipHelper(members:, mode: _, keys: _) -> zip_return(state, ref, members)
    ConcatHelper(remaining: _, inner:) -> concat_return(state, ref, inner)
  }
}

/// Close body for a classic helper's .return(): the caller (`resume_abrupt`)
/// has already put the generator in the state these closes should observe.
fn classic_helper_return(
  state: State(host),
  ref: Ref,
  kind: IteratorHelperKind,
  underlying: IteratorRecord,
) -> #(State(host), Result(JsValue, JsValue)) {
  // For flatMap, close the inner iterator first (best-effort), then outer.
  let #(state, inner_res) = case kind {
    HelperFlatMap(inner: Some(inner), func: _) ->
      iter_protocol.iterator_close_normal(state, inner.iterator)
    HelperFlatMap(inner: None, func: _)
    | HelperMap(func: _)
    | HelperFilter(func: _)
    | HelperTake(remaining: _)
    | HelperDrop(remaining: _) -> #(state, Ok(Nil))
  }
  let #(state, outer_res) =
    iter_protocol.iterator_close_normal(state, underlying.iterator)
  let state = mark_done(state, ref)
  case inner_res, outer_res {
    Error(e), _ -> #(state, Error(e))
    _, Error(e) -> #(state, Error(e))
    Ok(Nil), Ok(Nil) -> create_iter_result(state, JsUndefined, True)
  }
}

fn step_map(
  state: State(host),
  ref: Ref,
  underlying: IteratorRecord,
  func: JsValue,
  count: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  use step, state <- after_step(state, ref, underlying)
  case step {
    None -> finish(state, ref)
    Some(v) -> {
      let state = write_counter(state, ref, count + 1)
      let counter = value.from_int(count)
      case state.call(state, func, JsUndefined, [v, counter]) {
        Ok(#(mapped, state)) -> create_iter_result(state, mapped, False)
        Error(#(thrown, state)) ->
          close_throw_done(state, ref, underlying, thrown)
      }
    }
  }
}

fn step_filter(
  state: State(host),
  ref: Ref,
  underlying: IteratorRecord,
  func: JsValue,
  count: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  use step, state <- after_step(state, ref, underlying)
  case step {
    None -> finish(state, ref)
    Some(v) -> {
      let state = write_counter(state, ref, count + 1)
      let counter = value.from_int(count)
      case state.call(state, func, JsUndefined, [v, counter]) {
        Error(#(thrown, state)) ->
          close_throw_done(state, ref, underlying, thrown)
        Ok(#(selected, state)) ->
          case value.is_truthy(selected) {
            True -> create_iter_result(state, v, False)
            False -> step_filter(state, ref, underlying, func, count + 1)
          }
      }
    }
  }
}

fn step_take(
  state: State(host),
  ref: Ref,
  underlying: IteratorRecord,
  remaining: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  case remaining <= 0 {
    True -> {
      // §27.1.3.3.11: when remaining is 0, Return ? IteratorClose(iterated,
      // ReturnCompletion(undefined)) and yield done. The close runs INSIDE the
      // generator body, so the helper is still "executing" while it does.
      let #(state, close_res) =
        iter_protocol.iterator_close_normal(state, underlying.iterator)
      let state = mark_done(state, ref)
      case close_res {
        Error(e) -> #(state, Error(e))
        Ok(Nil) -> create_iter_result(state, JsUndefined, True)
      }
    }
    False -> {
      use step, state <- after_step(state, ref, underlying)
      case step {
        None -> finish(state, ref)
        Some(v) -> {
          let state = write_kind(state, ref, HelperTake(remaining - 1))
          create_iter_result(state, v, False)
        }
      }
    }
  }
}

fn step_drop(
  state: State(host),
  ref: Ref,
  underlying: IteratorRecord,
  remaining: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  use step, state <- after_step(state, ref, underlying)
  case step {
    None -> finish(state, ref)
    Some(v) ->
      case remaining > 0 {
        True -> {
          let state = write_kind(state, ref, HelperDrop(remaining - 1))
          step_drop(state, ref, underlying, remaining - 1)
        }
        False -> create_iter_result(state, v, False)
      }
  }
}

fn step_flat_map(
  state: State(host),
  ref: Ref,
  underlying: IteratorRecord,
  func: JsValue,
  inner: Option(IteratorRecord),
  count: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  case inner {
    // Have an active inner iterator — pull from it.
    Some(inner_rec) ->
      case iter_protocol.iterator_step_value(state, inner_rec) {
        #(state, Error(thrown)) ->
          // inner.next() threw → close outer (inner is already broken).
          close_throw_done(state, ref, underlying, thrown)
        #(state, Ok(Some(v))) -> create_iter_result(state, v, False)
        #(state, Ok(None)) -> {
          // Inner exhausted — clear and pull from outer.
          let state = write_kind(state, ref, HelperFlatMap(func:, inner: None))
          step_flat_map(state, ref, underlying, func, None, count)
        }
      }
    // No inner — pull from outer, map, open new inner.
    None -> {
      use step, state <- after_step(state, ref, underlying)
      case step {
        None -> finish(state, ref)
        Some(v) -> {
          let counter = value.from_int(count)
          let state = write_counter(state, ref, count + 1)
          case state.call(state, func, JsUndefined, [v, counter]) {
            Error(#(thrown, state)) ->
              close_throw_done(state, ref, underlying, thrown)
            Ok(#(mapped, state)) ->
              // GetIteratorFlattenable(mapped, reject-strings) — must be Object
              case
                iter_protocol.get_iterator_flattenable(
                  state,
                  mapped,
                  RejectPrimitives,
                  "flatMap callback result",
                )
              {
                Error(#(thrown, state)) ->
                  close_throw_done(state, ref, underlying, thrown)
                Ok(#(inner, state)) -> {
                  let state =
                    write_kind(
                      state,
                      ref,
                      HelperFlatMap(func:, inner: Some(inner)),
                    )
                  step_flat_map(
                    state,
                    ref,
                    underlying,
                    func,
                    Some(inner),
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

/// §27.1.5.2.2 %WrapForValidIteratorPrototype%.return: reuses IteratorClose's
/// GetMethod(iterated, "return") + Call step (`call_return`), so a
/// non-callable `return` is a TypeError. No `return` method synthesizes
/// CreateIterResultObject(undefined, true); otherwise the return method's
/// result is forwarded as-is (the spec does NOT require it to be an Object
/// here — only §7.4.11 IteratorClose does).
fn wrap_return(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use iterated, _next_method <- require_wrap(this, state)
  case iter_protocol.call_return(state, iterated) {
    #(state, Ok(iter_protocol.NoReturnMethod)) ->
      create_iter_result(state, JsUndefined, True)
    #(state, Ok(iter_protocol.Returned(result))) -> #(state, Ok(result))
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

// ============================================================================
// Eager consumers — toArray, forEach, reduce, some, every, find
// ============================================================================

fn to_array(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use rec, state <- state.try_op(get_iterator_direct(state, this, "toArray"))
  to_array_loop(state, rec, [])
}

fn to_array_loop(
  state: State(host),
  rec: IteratorRecord,
  acc: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, step) = iter_protocol.iterator_step_value(state, rec)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> state.ok_array(state, list.reverse(acc))
    Ok(Some(v)) -> to_array_loop(state, rec, [v, ..acc])
  }
}

fn for_each(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use rec, func, state <- consumer_with_callback(this, args, state, "forEach")
  for_each_loop(state, rec, func, 0)
}

fn for_each_loop(
  state: State(host),
  rec: IteratorRecord,
  func: JsValue,
  counter: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, step) = iter_protocol.iterator_step_value(state, rec)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> #(state, Ok(JsUndefined))
    Ok(Some(v)) -> {
      let idx = value.from_int(counter)
      case state.call(state, func, JsUndefined, [v, idx]) {
        Error(#(thrown, state)) ->
          iter_protocol.close_throw(state, rec.iterator, thrown)
        Ok(#(_result, state)) -> for_each_loop(state, rec, func, counter + 1)
      }
    }
  }
}

fn reduce(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use rec, func, state <- consumer_with_callback(this, args, state, "reduce")
  // §27.1.3.9: if initialValue is not present, seed accumulator with first step.
  case args {
    [_, initial, ..] -> reduce_loop(state, rec, func, initial, 0)
    _ -> {
      let #(state, step) = iter_protocol.iterator_step_value(state, rec)
      case step {
        Error(thrown) -> #(state, Error(thrown))
        Ok(None) ->
          state.type_error(
            state,
            "Reduce of empty iterator with no initial value",
          )
        Ok(Some(seed)) -> reduce_loop(state, rec, func, seed, 1)
      }
    }
  }
}

fn reduce_loop(
  state: State(host),
  rec: IteratorRecord,
  func: JsValue,
  acc: JsValue,
  counter: Int,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, step) = iter_protocol.iterator_step_value(state, rec)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> #(state, Ok(acc))
    Ok(Some(v)) -> {
      let idx = value.from_int(counter)
      case state.call(state, func, JsUndefined, [acc, v, idx]) {
        Error(#(thrown, state)) ->
          iter_protocol.close_throw(state, rec.iterator, thrown)
        Ok(#(new_acc, state)) ->
          reduce_loop(state, rec, func, new_acc, counter + 1)
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
  use rec, func, state <- consumer_with_callback(this, args, state, name)
  let #(state, res) = predicate_loop(state, rec, func, 0, match_on)
  #(state, result.map(res, fn(m) { JsBool(option.is_some(m) == match_on) }))
}

/// Shared loop for some/every/find: step iterator, call predicate(v, idx),
/// early-exit (closing iterator) when truthiness == match_on. Some(v) = matched.
fn predicate_loop(
  state: State(host),
  rec: IteratorRecord,
  func: JsValue,
  counter: Int,
  match_on: Bool,
) -> #(State(host), Result(Option(JsValue), JsValue)) {
  let #(state, step) = iter_protocol.iterator_step_value(state, rec)
  case step {
    Error(thrown) -> #(state, Error(thrown))
    Ok(None) -> #(state, Ok(None))
    Ok(Some(v)) -> {
      let idx = value.from_int(counter)
      case state.call(state, func, JsUndefined, [v, idx]) {
        Error(#(thrown, state)) ->
          iter_protocol.close_throw(state, rec.iterator, thrown)
        Ok(#(result, state)) ->
          case value.is_truthy(result) == match_on {
            True -> {
              let #(state, close_res) =
                iter_protocol.iterator_close_normal(state, rec.iterator)
              case close_res {
                Error(e) -> #(state, Error(e))
                Ok(Nil) -> #(state, Ok(Some(v)))
              }
            }
            False -> predicate_loop(state, rec, func, counter + 1, match_on)
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
  use rec, func, state <- consumer_with_callback(this, args, state, "find")
  let #(state, res) = predicate_loop(state, rec, func, 0, True)
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
        // Step 4: CreateDataPropertyOrThrow(this, key, v). Routed through the
        // real [[DefineOwnProperty]] so a Proxy receiver runs its trap and a
        // non-ordinary receiver cannot silently swallow the define.
        False -> {
          let val = first_arg_or_undefined(args)
          let key_val = case which {
            IgnoreSetCtor -> JsString("constructor")
            IgnoreSetTag -> value.JsSymbol(value.symbol_to_string_tag)
          }
          use state <- builtins_object.create_data_property_or_throw(
            state,
            ref,
            key_val,
            val,
          )
          #(state, Ok(JsUndefined))
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

/// §7.4.9 GetIteratorDirect on the receiver of an `Iterator.prototype.<name>`
/// method — the shared §7.4.9 op lives in `iter_protocol`; this only supplies
/// the method-specific TypeError message.
fn get_iterator_direct(
  state: State(host),
  this: JsValue,
  name: String,
) -> Result(#(IteratorRecord, State(host)), #(JsValue, State(host))) {
  iter_protocol.get_iterator_direct(
    state,
    this,
    "Iterator.prototype." <> name <> " called on non-object",
  )
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
  cont: fn(IteratorRecord, JsValue, State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  use _ref <- require_object_of(
    this,
    state,
    "Iterator.prototype." <> name <> " called on non-object",
  )
  let func = first_arg_or_undefined(args)
  case is_callable(state.heap, func) {
    False ->
      iter_protocol.close_throw_type(
        state,
        this,
        name <> " argument is not callable",
      )
    True -> {
      use rec, state <- state.try_op(get_iterator_direct(state, this, name))
      cont(rec, func, state)
    }
  }
}

/// `iter_protocol.close_and_throw` with a freshly-allocated TypeError.
fn close_and_throw_type(
  state: State(host),
  obj: JsValue,
  msg: String,
) -> #(JsValue, State(host)) {
  let #(err, state) = state.type_error_value(state, msg)
  iter_protocol.close_and_throw(state, obj, err)
}

/// `iter_protocol.close_and_throw` with a freshly-allocated RangeError.
fn close_and_throw_range(
  state: State(host),
  obj: JsValue,
  msg: String,
) -> #(JsValue, State(host)) {
  let #(err, state) = state.range_error_value(state, msg)
  iter_protocol.close_and_throw(state, obj, err)
}

// ============================================================================
// Small helpers
// ============================================================================

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

/// A TypeError as the canonical `#(thrown, state)` Error payload — the shape
/// every fallible helper below returns, and the shape `state.try_op` unwraps.
fn err_type(
  state: State(host),
  msg: String,
) -> Result(a, #(JsValue, State(host))) {
  Error(state.type_error_value(state, msg))
}

/// Step the underlying iterator. If next() throws, mark the helper done and
/// propagate WITHOUT calling close (the iterator is already broken).
fn after_step(
  state: State(host),
  ref: Ref,
  rec: IteratorRecord,
  cont: fn(Option(JsValue), State(host)) ->
    #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, step) = iter_protocol.iterator_step_value(state, rec)
  case step {
    Ok(v) -> cont(v, state)
    Error(thrown) -> #(mark_done(state, ref), Error(thrown))
  }
}

/// Complete a helper and yield {value: undefined, done: true}.
fn finish(
  state: State(host),
  ref: Ref,
) -> #(State(host), Result(JsValue, JsValue)) {
  create_iter_result(mark_done(state, ref), JsUndefined, True)
}

/// The generator body's IfAbruptCloseIterator: close `underlying` while the
/// helper is still "executing" (a reentrant next/return during the close
/// therefore throws), then complete the generator and rethrow.
fn close_throw_done(
  state: State(host),
  ref: Ref,
  underlying: IteratorRecord,
  thrown: JsValue,
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, res) =
    iter_protocol.close_throw(state, underlying.iterator, thrown)
  #(mark_done(state, ref), res)
}

/// Latch the helper's [[GeneratorState]] to `Completed` — a completed
/// generator never leaves that state. Works for all three helper kinds.
fn mark_done(state: State(host), ref: Ref) -> State(host) {
  set_gen_state(state, ref, Completed)
}

/// Advance map/filter/flatMap's running element index.
fn write_counter(state: State(host), ref: Ref, counter: Int) -> State(host) {
  use kind, _counter <- update_helper(state, ref)
  #(kind, counter)
}

/// Replace the per-kind payload (take/drop's `remaining`, flatMap's `inner`).
fn write_kind(
  state: State(host),
  ref: Ref,
  kind: IteratorHelperKind,
) -> State(host) {
  use _kind, counter <- update_helper(state, ref)
  #(kind, counter)
}

/// The ONE body write for every %IteratorHelper% flavour. (`gen_state` is a
/// sibling field owned by `map_gen_state`, so a body write can never clobber
/// the generator lifecycle, and vice versa.)
fn map_helper_body(
  state: State(host),
  ref: Ref,
  update: fn(HelperBody) -> HelperBody,
) -> State(host) {
  let heap =
    heap.update(state.heap, ref, fn(slot) {
      case slot {
        ObjectSlot(kind: IteratorHelperObject(body:, ..) as helper, ..) ->
          ObjectSlot(
            ..slot,
            kind: IteratorHelperObject(..helper, body: update(body)),
          )
        other -> other
      }
    })
  State(..state, heap:)
}

/// Rewrite a CLASSIC helper's `kind`/`counter` in place — the zip and concat
/// bodies have neither, so they are left alone.
fn update_helper(
  state: State(host),
  ref: Ref,
  update: fn(IteratorHelperKind, Int) -> #(IteratorHelperKind, Int),
) -> State(host) {
  use body <- map_helper_body(state, ref)
  case body {
    ClassicHelper(kind:, underlying:, counter:) -> {
      let #(kind, counter) = update(kind, counter)
      ClassicHelper(kind:, underlying:, counter:)
    }
    ZipHelper(..) | ConcatHelper(..) -> body
  }
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
  // Steps 2-7: GetOptionsObject + mode (paddingOption rides inside OptLongest).
  use mode, state <- state.try_op(zip_options(state, args, "zip"))
  // Step 10: GetIterator(iterables, sync).
  use input_rec, state <- state.try_op(iter_protocol.get_iterator_sync(
    state,
    iterables,
  ))
  // Step 12: drain the input iterator, flattening each value to an iterator.
  use iters, state <- state.try_op(zip_collect(state, input_rec, []))
  // Steps 13-14: resolve the padding list — only "longest" HAS a paddingOption
  // to read, but every mode carries one entry per iterator so `alloc_zip` can
  // pair them strictly.
  use padding, state <- state.try_op(case mode {
    OptLongest(padding: opt) -> zip_padding_iterated(state, opt, iters)
    OptShortest | OptStrict -> Ok(#(unread_padding(iters), state))
  })
  // Steps 15-16: finishResults = CreateArrayFromList — keys: None.
  alloc_zip(state, iters, zip_mode(mode), padding, None)
}

/// The padding column for a mode that never reads padding: one `undefined` per
/// iterator, so `alloc_zip`'s strict pairing still holds.
fn unread_padding(iters: List(IteratorRecord)) -> List(JsValue) {
  list.map(iters, fn(_iter) { JsUndefined })
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
  use mode, state <- state.try_op(zip_options(state, args, "zipKeyed"))
  // Step 10: allKeys = iterables.[[OwnPropertyKeys]]().
  use all_keys, state <- state.try_op(builtins_object.own_property_keys(
    state,
    iterables_ref,
  ))
  // Steps 11-12: filter to enumerable, non-undefined-valued properties.
  use #(keys, iters), state <- state.try_op(
    zip_keyed_collect(state, iterables, iterables_ref, all_keys, [], []),
  )
  // Step 14: padding read per key from the padding object.
  use padding, state <- state.try_op(case mode {
    OptLongest(padding: opt) -> zip_keyed_padding(state, opt, keys, iters)
    OptShortest | OptStrict -> Ok(#(unread_padding(iters), state))
  })
  // Steps 15-16: finishResults = null-proto object keyed by `keys`.
  alloc_zip(state, iters, zip_mode(mode), padding, Some(keys))
}

/// The parsed `mode` option of Iterator.zip/zipKeyed. `paddingOption` is a
/// field of `OptLongest` — the ONLY mode the spec reads it in — so a
/// shortest/strict path physically cannot reach for a padding value.
type ZipModeOption {
  OptShortest
  OptStrict
  /// `padding` is the raw paddingOption: `undefined` or an Object (validated
  /// at parse time), never any other primitive.
  OptLongest(padding: JsValue)
}

/// The stored [[Mode]] of the zip helper — the option's padding is consumed
/// during construction, so it does not survive into the exotic object.
fn zip_mode(option: ZipModeOption) -> ZipMode {
  case option {
    OptShortest -> ZipShortest
    OptStrict -> ZipStrict
    OptLongest(padding: _) -> ZipLongest
  }
}

/// Steps 2-7 shared by zip/zipKeyed: GetOptionsObject(options), Get "mode",
/// validate it, and Get "padding" when (and only when) mode is "longest".
fn zip_options(
  state: State(host),
  args: List(JsValue),
  name: String,
) -> Result(#(ZipModeOption, State(host)), #(JsValue, State(host))) {
  let options = case args {
    [_, opts, ..] -> opts
    _ -> JsUndefined
  }
  case options {
    // GetOptionsObject: undefined → OrdinaryObjectCreate(null), whose Gets
    // all yield undefined — mode is "shortest" and padding is never read.
    JsUndefined -> Ok(#(OptShortest, state))
    JsObject(_) -> {
      use #(mode_val, state) <- result.try(object.get_value_of(
        state,
        options,
        Named("mode"),
      ))
      // Step 5: no ToString coercion — only undefined or the exact strings.
      case mode_val {
        JsUndefined | JsString("shortest") -> Ok(#(OptShortest, state))
        JsString("strict") -> Ok(#(OptStrict, state))
        JsString("longest") -> {
          use #(padding, state) <- result.try(object.get_value_of(
            state,
            options,
            Named("padding"),
          ))
          case padding {
            JsUndefined | JsObject(_) -> Ok(#(OptLongest(padding:), state))
            _ ->
              err_type(
                state,
                "Iterator." <> name <> " padding is not an object",
              )
          }
        }
        _ ->
          err_type(
            state,
            "Iterator."
              <> name
              <> " mode must be \"shortest\", \"longest\", or \"strict\"",
          )
      }
    }
    _ -> err_type(state, "Iterator." <> name <> " options is not an object")
  }
}

/// Iterator.zip step 12: drain the iterables iterator, converting each value
/// via GetIteratorFlattenable(·, reject-primitives). On abrupt completions,
/// already-collected iterators are closed per IfAbruptCloseIterators.
fn zip_collect(
  state: State(host),
  input_rec: IteratorRecord,
  acc: List(IteratorRecord),
) -> Result(#(List(IteratorRecord), State(host)), #(JsValue, State(host))) {
  let #(state, step) = iter_protocol.iterator_step_value(state, input_rec)
  case step {
    // IfAbruptCloseIterators(next, iters) — the input iterator is broken,
    // close only the collected iterators (in reverse order).
    Error(thrown) ->
      Error(close_all_and_throw(state, collected_iters(acc), thrown))
    Ok(None) -> Ok(#(list.reverse(acc), state))
    Ok(Some(v)) ->
      case
        iter_protocol.get_iterator_flattenable(
          state,
          v,
          RejectPrimitives,
          "Iterator.zip input",
        )
      {
        // IfAbruptCloseIterators(iter, « inputIter » + iters): reverse order
        // closes the collected iterators first, then the input iterator.
        Error(#(thrown, state)) ->
          Error(close_all_and_throw(
            state,
            [input_rec.iterator, ..collected_iters(acc)],
            thrown,
          ))
        Ok(#(rec, state)) -> zip_collect(state, input_rec, [rec, ..acc])
      }
  }
}

/// Iterator-object list (spec order) from a reversed record accumulator.
fn collected_iters(acc: List(IteratorRecord)) -> List(JsValue) {
  list.reverse(acc) |> list.map(fn(rec) { rec.iterator })
}

/// Iterator.zip step 14: the "longest" padding list comes from ITERATING the
/// padding object (unlike zipKeyed, which Gets per key).
fn zip_padding_iterated(
  state: State(host),
  padding_option: JsValue,
  iters: List(IteratorRecord),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  let iter_count = list.length(iters)
  case padding_option {
    JsUndefined -> Ok(#(list.repeat(JsUndefined, iter_count), state))
    _ -> {
      let opened = list.map(iters, fn(rec) { rec.iterator })
      case iter_protocol.get_iterator_sync(state, padding_option) {
        Error(#(thrown, state)) ->
          Error(close_all_and_throw(state, opened, thrown))
        Ok(#(pad_rec, state)) ->
          zip_padding_loop(state, pad_rec, opened, iter_count, [])
      }
    }
  }
}

fn zip_padding_loop(
  state: State(host),
  pad_rec: IteratorRecord,
  opened: List(JsValue),
  remaining: Int,
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case remaining <= 0 {
    True -> {
      // usingIterator is still true — IteratorClose(paddingIter, normal).
      let #(state, close_res) =
        iter_protocol.iterator_close_normal(state, pad_rec.iterator)
      case close_res {
        Error(thrown) -> Error(close_all_and_throw(state, opened, thrown))
        Ok(Nil) -> Ok(#(list.reverse(acc), state))
      }
    }
    False -> {
      let #(state, step) = iter_protocol.iterator_step_value(state, pad_rec)
      case step {
        Error(thrown) -> Error(close_all_and_throw(state, opened, thrown))
        // Padding iterator exhausted: fill the rest with undefined; the
        // padding iterator is NOT closed (usingIterator becomes false).
        Ok(None) ->
          Ok(#(
            list.append(list.reverse(acc), list.repeat(JsUndefined, remaining)),
            state,
          ))
        Ok(Some(v)) ->
          zip_padding_loop(state, pad_rec, opened, remaining - 1, [v, ..acc])
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
  keys_left: List(ObjectKey),
  keys_acc: List(ObjectKey),
  iters_acc: List(IteratorRecord),
) -> Result(
  #(#(List(ObjectKey), List(IteratorRecord)), State(host)),
  #(JsValue, State(host)),
) {
  case keys_left {
    [] -> Ok(#(#(list.reverse(keys_acc), list.reverse(iters_acc)), state))
    [key, ..rest] ->
      case builtins_object.own_property_keyed(state, iterables_ref, key) {
        Error(#(thrown, state)) ->
          Error(close_all_and_throw(state, collected_iters(iters_acc), thrown))
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
                  Error(close_all_and_throw(
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
                    iter_protocol.get_iterator_flattenable(
                      state,
                      v,
                      RejectPrimitives,
                      "Iterator.zipKeyed input",
                    )
                  {
                    Error(#(thrown, state)) ->
                      Error(close_all_and_throw(
                        state,
                        collected_iters(iters_acc),
                        thrown,
                      ))
                    Ok(#(rec, state)) ->
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

/// [[Get]] keyed by an [[OwnPropertyKeys]] element.
fn get_keyed(
  state: State(host),
  obj: JsValue,
  key: ObjectKey,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case key {
    SymbolPropKey(sym) -> object.get_symbol_value_of(state, obj, sym)
    StringPropKey(pkey:, ..) -> object.get_value_of(state, obj, pkey)
  }
}

/// Iterator.zipKeyed step 14: the "longest" padding values are read per key
/// from the padding object.
fn zip_keyed_padding(
  state: State(host),
  padding_option: JsValue,
  keys: List(ObjectKey),
  iters: List(IteratorRecord),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case padding_option {
    JsUndefined -> Ok(#(list.repeat(JsUndefined, list.length(iters)), state))
    _ -> {
      let opened = list.map(iters, fn(rec) { rec.iterator })
      zip_keyed_padding_loop(state, padding_option, opened, keys, [])
    }
  }
}

fn zip_keyed_padding_loop(
  state: State(host),
  padding_option: JsValue,
  opened: List(JsValue),
  keys_left: List(ObjectKey),
  acc: List(JsValue),
) -> Result(#(List(JsValue), State(host)), #(JsValue, State(host))) {
  case keys_left {
    [] -> Ok(#(list.reverse(acc), state))
    [key, ..rest] ->
      case get_keyed(state, padding_option, key) {
        Error(#(thrown, state)) ->
          Error(close_all_and_throw(state, opened, thrown))
        Ok(#(v, state)) ->
          zip_keyed_padding_loop(state, padding_option, opened, rest, [v, ..acc])
      }
  }
}

/// Allocate the IteratorZip helper object on %IteratorHelperPrototype%. This is
/// the ONE place iterators and their padding values are paired up: from here on
/// each `ZipMember` owns its padding, so no downstream code can lose the
/// alignment (or silently substitute `undefined` for a missing entry).
fn alloc_zip(
  state: State(host),
  iters: List(IteratorRecord),
  mode: ZipMode,
  padding: List(JsValue),
  keys: Option(List(ObjectKey)),
) -> #(State(host), Result(JsValue, JsValue)) {
  // Every `padding` producer emits exactly one entry per iterator, so the
  // strict zip cannot fail — and if a future one forgets, it fails loudly here
  // rather than padding a column with `undefined` at yield time.
  let assert Ok(paired) = list.strict_zip(iters, padding)
    as "Iterator.zip padding must have one entry per iterator"
  let members =
    list.map(paired, fn(pair) {
      let #(record, padding) = pair
      ZipOpen(record:, padding:)
    })
  alloc_helper_body(state, ZipHelper(members:, mode:, keys:))
}

// ============================================================================
// IteratorZip stepping — %IteratorHelperPrototype%.next/.return for zip
// ============================================================================

fn zip_next(
  state: State(host),
  ref: Ref,
  members: List(ZipMember),
  mode: ZipMode,
  keys: Option(List(ObjectKey)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case members {
    // IteratorZip closure step 1: iterCount = 0 → done forever.
    [] -> finish_zip(state, ref)
    _ -> zip_round(state, ref, mode, keys, [], members, [])
  }
}

/// One result-tuple round: step every member in order, applying the
/// mode-specific termination rules from the IteratorZip abstract operation.
/// `prev` (reversed) holds processed members, `rest` the unprocessed ones —
/// each member carries its own padding value, so there is no second list to
/// keep in step.
fn zip_round(
  state: State(host),
  ref: Ref,
  mode: ZipMode,
  keys: Option(List(ObjectKey)),
  prev: List(ZipMember),
  rest: List(ZipMember),
  results: List(JsValue),
) -> #(State(host), Result(JsValue, JsValue)) {
  case rest {
    [] -> zip_emit(state, ref, keys, list.reverse(prev), list.reverse(results))
    [member, ..tail] ->
      case member {
        // Spec: iters[i] is null → result = padding[i].
        ZipExhausted(padding:) ->
          zip_round(state, ref, mode, keys, [member, ..prev], tail, [
            padding,
            ..results
          ])
        ZipOpen(record:, padding:) -> {
          let #(state, step) = iter_protocol.iterator_step_value(state, record)
          case step {
            Error(thrown) -> {
              let #(state, res) =
                close_all_throw(state, open_others(prev, tail), thrown)
              #(mark_done(state, ref), res)
            }
            Ok(Some(v)) ->
              zip_round(state, ref, mode, keys, [member, ..prev], tail, [
                v,
                ..results
              ])
            Ok(None) ->
              case mode {
                ZipShortest -> {
                  let #(state, close_res) =
                    close_all_normal(state, open_others(prev, tail))
                  let state = mark_done(state, ref)
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
                        [ZipExhausted(padding:), ..prev],
                        tail,
                        [padding, ..results],
                      )
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
    [ZipExhausted(padding: _), ..tail] -> zip_strict_check(state, ref, tail)
    [ZipOpen(record:, padding: _), ..tail] -> {
      let #(state, step) = iterator_step_done(state, record)
      case step {
        Error(thrown) -> {
          let #(state, res) = close_all_throw(state, open_members(tail), thrown)
          #(mark_done(state, ref), res)
        }
        Ok(True) -> zip_strict_check(state, ref, tail)
        // Not done — this iterator is still in openIters and is closed too.
        Ok(False) ->
          zip_strict_throw(state, ref, [record.iterator, ..open_members(tail)])
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
  let #(terr, state) =
    state.type_error_value(
      state,
      "Iterator.zip strict mode: iterators have different lengths",
    )
  let #(state, res) = close_all_throw(state, open, terr)
  #(mark_done(state, ref), res)
}

/// §7.4.6 IteratorStep: call next() and read only `done` (never `value`).
fn iterator_step_done(
  state: State(host),
  rec: IteratorRecord,
) -> #(State(host), Result(Bool, JsValue)) {
  use _result, done, state <- iter_protocol.iterator_step_result(state, rec)
  #(state, Ok(done))
}

/// Finish a round: persist longest-mode exhaustion transitions, then build
/// the finishResults value (array for zip, null-proto object for zipKeyed).
fn zip_emit(
  state: State(host),
  ref: Ref,
  keys: Option(List(ObjectKey)),
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
  keys: List(ObjectKey),
  results: List(JsValue),
) -> #(Heap(host), Ref) {
  let #(props, sym_props) =
    list.zip(keys, results)
    |> list.fold(#(dict.new(), []), fn(acc, pair) {
      let #(props, syms) = acc
      let #(key, v) = pair
      case key {
        SymbolPropKey(sym) -> #(
          props,
          list.key_set(syms, sym, value.data_property(v)),
        )
        StringPropKey(pkey:, ..) -> #(
          dict.insert(props, pkey, value.data_property(v)),
          syms,
        )
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

/// Close body for a zip helper's .return(): IteratorCloseAll over every
/// still-open underlying iterator (reverse order). `resume_abrupt` has already
/// put the generator in the state those closes should observe.
fn zip_return(
  state: State(host),
  ref: Ref,
  members: List(ZipMember),
) -> #(State(host), Result(JsValue, JsValue)) {
  let #(state, close_res) = close_all_normal(state, open_members(members))
  let state = mark_done(state, ref)
  case close_res {
    Error(e) -> #(state, Error(e))
    Ok(Nil) -> create_iter_result(state, JsUndefined, True)
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
      ZipOpen(record:, ..) -> Ok(record.iterator)
      ZipExhausted(padding: _) -> Error(Nil)
    }
  })
}

/// IteratorCloseAll (joint-iteration proposal) with a pending throw: close
/// every iterator in REVERSE list order — errors from .return are swallowed
/// (the original error wins) — then rethrow the original.
fn close_all_and_throw(
  state: State(host),
  iters: List(JsValue),
  original: JsValue,
) -> #(JsValue, State(host)) {
  let state =
    list.fold(list.reverse(iters), state, fn(state, it) {
      // IteratorClose with a throw completion swallows .return errors.
      let #(state, _superseded_by_original) =
        iter_protocol.call_return(state, it)
      state
    })
  #(original, state)
}

/// `close_all_and_throw` at a dispatch boundary.
fn close_all_throw(
  state: State(host),
  iters: List(JsValue),
  original: JsValue,
) -> #(State(host), Result(a, JsValue)) {
  let #(thrown, state) = close_all_and_throw(state, iters, original)
  #(state, Error(thrown))
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
      Ok(Nil) -> iter_protocol.iterator_close_normal(state, it)
      Error(e) -> {
        let #(state, _superseded_by_first_error) =
          iter_protocol.call_return(state, it)
        #(state, Error(e))
      }
    }
  })
}

fn finish_zip(
  state: State(host),
  ref: Ref,
) -> #(State(host), Result(JsValue, JsValue)) {
  create_iter_result(mark_done(state, ref), JsUndefined, True)
}

fn zip_write_members(
  state: State(host),
  ref: Ref,
  members: List(ZipMember),
) -> State(host) {
  use body <- map_helper_body(state, ref)
  case body {
    ZipHelper(mode:, keys:, members: _) -> ZipHelper(members:, mode:, keys:)
    ClassicHelper(..) | ConcatHelper(..) -> body
  }
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
  acc: List(ConcatItem),
) -> #(State(host), Result(JsValue, JsValue)) {
  case items {
    [] ->
      alloc_helper_body(
        state,
        ConcatHelper(remaining: list.reverse(acc), inner: None),
      )
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
                True ->
                  concat_validate(state, rest, [
                    ConcatItem(open_method: method, iterable: item),
                    ..acc
                  ])
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
  remaining: List(ConcatItem),
  inner: Option(IteratorRecord),
) -> #(State(host), Result(JsValue, JsValue)) {
  case inner {
    Some(inner_rec) -> {
      let #(state, step) = iter_protocol.iterator_step_value(state, inner_rec)
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

/// Open the next iterable's iterator: iter = Call(openMethod, iterable),
/// require an Object, GetIteratorDirect, then pull its first value.
fn concat_open_next(
  state: State(host),
  ref: Ref,
  remaining: List(ConcatItem),
) -> #(State(host), Result(JsValue, JsValue)) {
  case remaining {
    [] -> create_iter_result(concat_mark_done(state, ref), JsUndefined, True)
    [ConcatItem(open_method: method, iterable:), ..rest] ->
      case state.call(state, method, iterable, []) {
        Error(#(thrown, state)) -> #(
          concat_mark_done(state, ref),
          Error(thrown),
        )
        Ok(#(iter, state)) ->
          case
            iter_protocol.get_iterator_direct(
              state,
              iter,
              "Result of the Symbol.iterator method is not an object",
            )
          {
            Error(#(thrown, state)) -> #(
              concat_mark_done(state, ref),
              Error(thrown),
            )
            Ok(#(inner, state)) -> {
              let state = concat_write(state, ref, rest, Some(inner))
              concat_next(state, ref, rest, Some(inner))
            }
          }
      }
  }
}

/// Close body for a concat helper's .return(): close the currently open inner
/// iterator (if any). At suspended-start there is none, so `resume_abrupt`'s
/// two suspended arms only differ once the helper has yielded.
fn concat_return(
  state: State(host),
  ref: Ref,
  inner: Option(IteratorRecord),
) -> #(State(host), Result(JsValue, JsValue)) {
  case inner {
    Some(inner_rec) -> {
      let #(state, close_res) =
        iter_protocol.iterator_close_normal(state, inner_rec.iterator)
      let state = concat_mark_done(state, ref)
      case close_res {
        Error(e) -> #(state, Error(e))
        Ok(Nil) -> create_iter_result(state, JsUndefined, True)
      }
    }
    None -> create_iter_result(concat_mark_done(state, ref), JsUndefined, True)
  }
}

/// Complete the concat generator and drop its inner-iterator reference.
fn concat_mark_done(state: State(host), ref: Ref) -> State(host) {
  let state = mark_done(state, ref)
  use body <- map_helper_body(state, ref)
  case body {
    ConcatHelper(remaining:, inner: _) -> ConcatHelper(remaining:, inner: None)
    ClassicHelper(..) | ZipHelper(..) -> body
  }
}

fn concat_write(
  state: State(host),
  ref: Ref,
  remaining: List(ConcatItem),
  inner: Option(IteratorRecord),
) -> State(host) {
  use body <- map_helper_body(state, ref)
  case body {
    ConcatHelper(..) -> ConcatHelper(remaining:, inner:)
    ClassicHelper(..) | ZipHelper(..) -> body
  }
}
