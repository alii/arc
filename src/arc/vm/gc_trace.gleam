//// GC root tracing for the Arc heap.
////
//// The mark phase's entire view of "what does this heap slot point at" lives
//// here: `refs_in_slot` walks a `HeapSlot` and prepends every `Ref` reachable
//// from it. Every `case` in this module is exhaustive with NO wildcard arm —
//// that is the safety property the whole file exists to hold. Add a
//// ref-carrying variant to any of `value`'s enums and the compiler points at
//// the tracer you forgot, instead of the collector silently freeing a live
//// object at some later date.
////
//// Nothing here touches `Heap` or `State`; the caller supplies `host_refs` so
//// an embedder's opaque host values can be traced too.

import arc/vm/internal/ordered_entries
import arc/vm/internal/tree_array
import arc/vm/internal/tuple_array
import arc/vm/value
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}

/// GC root tracing: prepend the heap ref held by a reaction handler's
/// callable, if any, onto `acc`. Pass-through handlers hold no value.
fn push_reaction_handler_ref(
  handler: value.ReactionHandler,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case handler {
    value.Handler(fun:) -> push_value_ref(fun, acc)
    value.IdentityPassThrough | value.ThrowerPassThrough -> acc
  }
}

/// GC root tracing: prepend heap refs reachable from a Property onto `acc`.
fn push_property_refs(
  prop: value.Property,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case prop {
    value.DataProperty(value: v, ..) -> push_value_ref(v, acc)
    value.AccessorProperty(get:, set:, ..) -> {
      let acc = case get {
        Some(v) -> push_value_ref(v, acc)
        None -> acc
      }
      case set {
        Some(v) -> push_value_ref(v, acc)
        None -> acc
      }
    }
  }
}

/// GC root tracing: prepend the heap ref carried by `value` (if any) onto `acc`.
/// Zero-alloc for primitives.
fn push_value_ref(val: value.JsValue, acc: List(value.Ref)) -> List(value.Ref) {
  case val {
    value.JsObject(ref) -> [ref, ..acc]
    value.JsUndefined
    | value.JsNull
    | value.JsBool(_)
    | value.JsNumber(_)
    | value.JsString(_)
    | value.JsSymbol(_)
    | value.JsBigInt(_)
    | value.JsUninitialized -> acc
  }
}

fn push_option_ref(
  r: Option(value.Ref),
  acc: List(value.Ref),
) -> List(value.Ref) {
  case r {
    Some(ref) -> [ref, ..acc]
    None -> acc
  }
}

/// Both halves of an Iterator Record are heap-reachable: the iterator object
/// itself and its cached [[NextMethod]] (a bound function keeps its target and
/// receiver alive). Exhaustive destructure so a new Ref-carrying field is a
/// compile error, not a silent GC leak.
fn push_iter_record(
  rec: value.IteratorRecord,
  acc: List(value.Ref),
) -> List(value.Ref) {
  let value.IteratorRecord(iterator:, next_method:) = rec
  push_value_ref(next_method, push_value_ref(iterator, acc))
}

/// Every heap reference reachable from an %IteratorHelper%'s body. Exhaustive
/// per variant, so a new helper flavour cannot silently escape the GC.
fn helper_body_refs(
  body: value.HelperBody,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case body {
    value.ClassicHelper(kind:, underlying:, counter: _) -> {
      let acc = push_iter_record(underlying, acc)
      case kind {
        value.HelperTake(remaining: _) | value.HelperDrop(remaining: _) -> acc
        value.HelperMap(func:)
        | value.HelperFilter(func:)
        | value.HelperFlatMap(func:, inner: None) -> push_value_ref(func, acc)
        value.HelperFlatMap(func:, inner: Some(inner)) ->
          push_iter_record(inner, push_value_ref(func, acc))
      }
    }
    // `keys` holds no heap refs (an ObjectKey is a string or a symbol), so
    // only the members (iterator record + its padding value) are traced.
    value.ZipHelper(members:, mode: _, keys: _) ->
      list.fold(members, acc, fn(a, m) {
        case m {
          value.ZipOpen(record:, padding:) ->
            push_value_ref(padding, push_iter_record(record, a))
          value.ZipExhausted(padding:) -> push_value_ref(padding, a)
        }
      })
    value.ConcatHelper(remaining:, inner:) -> {
      let acc =
        list.fold(remaining, acc, fn(a, item) {
          // Full destructure so a new field is a compile error here.
          let value.ConcatItem(open_method:, iterable:) = item
          push_value_ref(iterable, push_value_ref(open_method, a))
        })
      case inner {
        Some(inner) -> push_iter_record(inner, acc)
        None -> acc
      }
    }
  }
}

fn push_option_value(
  v: Option(value.JsValue),
  acc: List(value.Ref),
) -> List(value.Ref) {
  case v {
    Some(val) -> push_value_ref(val, acc)
    None -> acc
  }
}

/// GC root tracing for a [[DisposableResourceStack]]: every resource value,
/// dispose method, callback, and callback argument it holds.
fn push_dispose_resources(
  resources: List(value.DisposeResource),
  acc: List(value.Ref),
) -> List(value.Ref) {
  list.fold(resources, acc, fn(a, r) {
    case r {
      value.SyncDispose(value: v, method: m)
      | value.AsyncFallbackDispose(value: v, method: m) ->
        push_value_ref(m, push_value_ref(v, a))
      value.DisposeCallback(callback:, args:) ->
        list.fold(args, push_value_ref(callback, a), fn(a2, v) {
          push_value_ref(v, a2)
        })
      value.NullDispose -> a
    }
  })
}

/// Root a suspended coroutine frame: locals, operand stack, and the frame's
/// own sloppy-direct-eval var dict — the EnvSlot is reachable ONLY from the
/// frame that owns it, so a suspended coroutine's copy has to be walked or GC
/// would free it out from under a later resume. Exhaustive destructure so a
/// future Ref-carrying `SuspendedFrame` field is a compile error here, not a
/// silent GC leak.
fn push_suspended_frame_refs(
  frame: value.SuspendedFrame,
  acc: List(value.Ref),
) -> List(value.Ref) {
  let value.SuspendedFrame(
    pc: _,
    locals:,
    stack:,
    try_stack: _,
    eval_env:,
    line: _,
  ) = frame
  let acc = push_option_ref(eval_env, acc)
  let acc =
    list.fold(tuple_array.to_list(locals), acc, fn(a, v) {
      push_value_ref(v, a)
    })
  list.fold(stack, acc, fn(a, v) { push_value_ref(v, a) })
}

/// Prepend all refs reachable from a heap slot onto `acc`. 
pub fn refs_in_slot(
  slot: value.HeapSlot(ctx, host),
  host_refs: fn(host) -> List(value.Ref),
) -> List(value.Ref) {
  do_refs_in_slot(slot, host_refs, [])
}

fn do_refs_in_slot(
  slot: value.HeapSlot(ctx, host),
  host_refs: fn(host) -> List(value.Ref),
  acc: List(value.Ref),
) -> List(value.Ref) {
  case slot {
    value.ObjectSlot(
      kind:,
      properties:,
      elements:,
      prototype:,
      symbol_properties:,
      extensible: _,
    ) -> {
      let acc =
        dict.fold(properties, acc, fn(a, _k, prop) {
          push_property_refs(prop, a)
        })
      let acc =
        list.fold(symbol_properties, acc, fn(a, pair) {
          push_property_refs(pair.1, a)
        })
      let acc = case elements {
        value.NoElements -> acc
        value.DenseElements(data) ->
          tree_array.sparse_fold(
            fn(_i, v, a) { push_value_ref(v, a) },
            acc,
            data,
          )
        value.SparseElements(data) ->
          dict.fold(data, acc, fn(a, _i, v) { push_value_ref(v, a) })
      }
      let acc = push_option_ref(prototype, acc)
      case kind {
        value.FunctionObject(env: env_ref, home_object: home, ..) ->
          push_option_ref(home, [env_ref, ..acc])
        // Native functions: every dispatch/call payload is traced by the
        // exhaustive per-enum tracers below (native_fn_refs and friends), so
        // adding a Ref-carrying native-fn variant without teaching the GC
        // about it is a compile error, not a premature-collection bug.
        value.NativeFunction(native:, ..) -> native_fn_refs(native, acc)
        // ShadowRealm instances keep their realm record alive.
        value.ShadowRealmObject(realm_ref:) -> [realm_ref, ..acc]
        value.ProxyObject(slots:, ..) ->
          case slots {
            Some(value.ProxySlots(target:, handler:)) -> [target, handler, ..acc]
            None -> acc
          }
        value.PromiseObject(promise_data:) -> [promise_data, ..acc]
        value.GeneratorObject(generator_data:) -> [generator_data, ..acc]
        value.AsyncGeneratorObject(generator_data:) -> [generator_data, ..acc]
        value.ArrayIteratorObject(source:, ..) -> [source, ..acc]
        // The iterated matcher is the only heap reference the RegExp String
        // Iterator's internal state can hold; the rest is scalar.
        value.RegExpStringIteratorObject(matcher:, ..) -> [matcher, ..acc]
        value.SetIteratorObject(source:, ..)
        | value.MapIteratorObject(source:, ..) -> [source, ..acc]
        value.AsyncFromSyncIteratorObject(sync_iter:, sync_next:) ->
          push_value_ref(sync_next, [sync_iter, ..acc])
        value.IteratorHelperObject(body:, ..) -> helper_body_refs(body, acc)
        value.WrapForValidIteratorObject(iterated:, next_method:)
        | value.IteratorRecordObject(iterated:, next_method:) ->
          push_value_ref(next_method, push_value_ref(iterated, acc))
        value.MapObject(store:) ->
          ordered_entries.fold(store, acc, fn(a, k, v) {
            push_value_ref(v, push_value_ref(value.map_key_to_js(k), a))
          })
        value.SetObject(store:) ->
          ordered_entries.fold(store, acc, fn(a, k, v) {
            push_value_ref(v, push_value_ref(value.map_key_to_js(k), a))
          })
        // Weak keys/members/targets are traced STRONGLY on purpose.
        // heap.alloc recycles freed ids from the free list, and these
        // collections key their Dicts on JsObject(Ref(id)) — dropping a key
        // from the mark set without ALSO pruning it from `data` before sweep
        // lets a fresh alloc reuse the id and produce a false has()/get() hit.
        // An impl MAY decline to collect weak refs (spec-legal over-retention);
        // it MUST NOT return spurious hits. Weak semantics land together with
        // a post-mark ephemeron pass that prunes dead entries before their ids
        // reach heap.free — not before.
        value.WeakMapObject(data:) ->
          dict.fold(data, acc, fn(a, k, v) {
            push_value_ref(v, push_value_ref(k, a))
          })
        value.WeakSetObject(data:) ->
          dict.fold(data, acc, fn(a, k, _v) { push_value_ref(k, a) })
        value.FinalizationRegistryObject(cells:, callback:) ->
          list.fold(cells, push_value_ref(callback, acc), fn(a, cell) {
            let value.FinRegCell(target:, held:, token:) = cell
            // target AND token are both spec-weak (§9.10.3 lets an impl clear
            // [[UnregisterToken]] once unreachable); held is strong. Both are
            // traced strong for now — same prune-pass precondition as above.
            let a = push_value_ref(held, push_value_ref(target, a))
            case token {
              Some(t) -> push_value_ref(t, a)
              None -> a
            }
          })
        value.DisposableStackObject(state: value.Pending(resources:), ..) ->
          push_dispose_resources(resources, acc)
        value.DisposableStackObject(state: value.Disposed, ..) -> acc
        // The namespace's live bindings are BoxSlot refs reachable via exports.
        value.ModuleNamespace(exports:) ->
          dict.fold(exports, acc, fn(a, _name, box_ref) { [box_ref, ..a] })
        // The only heap refs an Intl instance can hold are the cached bound
        // `format`/`compare` function objects; everything else is scalar.
        // Exhaustive on purpose — a new IntlData *variant* that carries a Ref
        // MUST be added here or its target can be collected while reachable.
        // The guarantee stops at the variant: the ref-free arms still spread
        // their state records with `..`/`_`, so adding a Ref-carrying *field*
        // to an existing state (a second cached bound method, say) compiles
        // fine and is silently traced as ref-free. Push such a field's Ref
        // onto `acc` here by hand.
        value.IntlObject(data:) ->
          case data {
            value.CollatorData(value.CollatorState(bound_compare: Some(r), ..))
            | value.NumberFormatData(value.NumberFormatState(
                bound_format: Some(r),
                ..,
              ))
            | value.DateTimeFormatData(value.DateTimeFormatState(
                bound_format: Some(r),
                ..,
              )) -> [r, ..acc]
            // Ref-free: no cached bound function, or no heap slot at all.
            value.CollatorData(value.CollatorState(bound_compare: None, ..))
            | value.NumberFormatData(value.NumberFormatState(
                bound_format: None,
                ..,
              ))
            | value.DateTimeFormatData(value.DateTimeFormatState(
                bound_format: None,
                ..,
              ))
            | value.LocaleData(_)
            | value.PluralRulesData(_)
            | value.ListFormatData(_)
            | value.RelativeTimeFormatData(_)
            | value.SegmenterData(_)
            | value.DisplayNamesData(_)
            | value.DurationFormatData(_)
            | value.SegmentsData(_)
            | value.SegmentIteratorData(_) -> acc
          }
        // Ask the embedder for any engine refs reachable from the opaque
        // host value, so GC traces host objects that point back into the JS
        // heap. Pure host terms (pids, fds) return [] — the default hook.
        value.HostObject(value: h) -> list.append(host_refs(h), acc)
        value.OrdinaryObject
        | value.ErrorObject(_)
        | value.ArrayObject(_)
        | value.ArgumentsObject(_)
        | value.StringObject(_)
        | value.NumberObject(_)
        | value.BooleanObject(_)
        | value.BigIntObject(_)
        | value.SymbolObject(_)
        | value.DateObject(_)
        | value.TemporalDateSlot(..)
        | value.TemporalTimeSlot(..)
        | value.TemporalDateTimeSlot(..)
        | value.TemporalYearMonthSlot(..)
        | value.TemporalMonthDaySlot(..)
        | value.TemporalDurationSlot(..)
        | value.TemporalInstantSlot(..)
        | value.TemporalZonedDateTimeSlot(..)
        | value.RegExpObject(..)
        | value.ArrayBufferObject(..)
        | // The rawJSON box's [[IsRawJSON]] payload is a plain String.
          value.RawJsonObject(_)
        | value.StringIteratorObject(_) -> acc
        // DataView keeps its viewed ArrayBuffer alive.
        value.DataViewObject(buffer:, ..) -> [buffer, ..acc]
        value.TypedArrayObject(buffer:, ..) -> [buffer, ..acc]
      }
    }
    value.EnvSlot(slots:) ->
      list.fold(slots, acc, fn(a, v) { push_value_ref(v, a) })
    value.BoxSlot(value: v) -> push_value_ref(v, acc)
    // A plain Int — no reachable refs.
    value.CounterSlot(count: _) -> acc
    value.EvalEnvSlot(vars:) ->
      dict.fold(vars, acc, fn(a, _k, v) { push_value_ref(v, a) })
    value.ForInIteratorSlot(keys:) ->
      list.fold(keys, acc, fn(a, v) { push_value_ref(v, a) })
    value.PromiseSlot(state:, fulfill_reactions:, reject_reactions:, ..) -> {
      let acc = case state {
        value.PromiseFulfilled(value: v) -> push_value_ref(v, acc)
        value.PromiseRejected(reason:) -> push_value_ref(reason, acc)
        value.PromisePending -> acc
      }
      let push_reactions = fn(reactions, a) {
        list.fold(reactions, a, fn(a, r) {
          // Full destructure so a new field is a compile error here.
          let value.PromiseReaction(child_resolve:, child_reject:, handler:) = r
          a
          |> push_value_ref(child_resolve, _)
          |> push_value_ref(child_reject, _)
          |> push_reaction_handler_ref(handler, _)
        })
      }
      push_reactions(reject_reactions, push_reactions(fulfill_reactions, acc))
    }
    // A completed generator holds no frame at all — its locals / operand stack
    // stopped being roots the moment it finished. A running one still does:
    // a nested drive replaces the live State, so its frame is all that roots
    // the body's values.
    value.GeneratorSlot(gen_state:, env_ref:, ..) ->
      case gen_state {
        value.GenSuspended(frame:, ..) | value.GenExecuting(frame:) ->
          push_suspended_frame_refs(frame, [env_ref, ..acc])
        value.GenCompleted -> [env_ref, ..acc]
      }
    value.AsyncFunctionSlot(
      promise_data_ref:,
      resolve:,
      reject:,
      func_template: _,
      frame:,
    ) -> {
      let acc =
        [promise_data_ref, ..acc]
        |> push_value_ref(resolve, _)
        |> push_value_ref(reject, _)
      push_suspended_frame_refs(frame, acc)
    }
    value.AsyncGeneratorSlot(
      queue: #(queue_front, queue_back),
      env_ref:,
      frame:,
      ..,
    ) -> {
      // Both halves of the two-list FIFO hold live requests — walk both.
      let push_request = fn(a, r) {
        // Full destructure so a new field is a compile error here.
        let value.AsyncGenRequest(completion: _, value: v, resolve:, reject:) =
          r
        a
        |> push_value_ref(v, _)
        |> push_value_ref(resolve, _)
        |> push_value_ref(reject, _)
      }
      let acc = list.fold(queue_front, acc, push_request)
      let acc = list.fold(queue_back, acc, push_request)
      push_suspended_frame_refs(frame, [env_ref, ..acc])
    }
    value.RealmSlot(global_object:, lexical_globals:, symbol_registry: _) ->
      dict.fold(lexical_globals, [global_object, ..acc], fn(a, _k, v) {
        push_value_ref(value.lexical_global_value(v), a)
      })
  }
}

/// GC root tracing for a NativeFunction's payload.
///
/// This family of tracers (native_fn_refs → dispatch_native_fn_refs /
/// call_native_fn_refs → one function per *NativeFn enum) is deliberately
/// written with NO wildcard arms and NO `..` in constructor patterns:
/// exhaustiveness is the point. Adding a native-fn variant — or adding a Ref
/// or JsValue field to an existing one — fails to compile until the new edge
/// is traced (or the variant is explicitly listed as carrying no refs), so a
/// payload can never be silently dropped by the mark phase and collected out
/// from under a live built-in.
fn native_fn_refs(
  native: value.NativeFnSlot(ctx),
  acc: List(value.Ref),
) -> List(value.Ref) {
  case native {
    value.Dispatch(d) -> dispatch_native_fn_refs(d, acc)
    value.Call(c) -> call_native_fn_refs(c, acc)
    // Host closures are opaque to the engine; the embedder owns any values
    // they capture and must keep them alive (same contract as HostObject,
    // which at least exposes a host_refs hook — closures expose nothing).
    value.Host(_) -> acc
  }
}

/// One arm per dispatch module, each delegating to that module's exhaustive
/// per-variant tracer.
fn dispatch_native_fn_refs(
  native: value.NativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case native {
    value.MathNative(f) -> math_native_refs(f, acc)
    value.BooleanNative(f) -> boolean_native_refs(f, acc)
    value.NumberNative(f) -> number_native_refs(f, acc)
    value.StringNative(f) -> string_native_refs(f, acc)
    value.ErrorNative(f) -> error_native_refs(f, acc)
    value.ArrayNative(f) -> array_native_refs(f, acc)
    value.ObjectNative(f) -> object_native_refs(f, acc)
    value.ConsoleNative(f) -> console_native_refs(f, acc)
    value.JsonNative(f) -> json_native_refs(f, acc)
    value.ReflectNative(f) -> reflect_native_refs(f, acc)
    value.MapNative(f) -> map_native_refs(f, acc)
    value.SetNative(f) -> set_native_refs(f, acc)
    value.WeakMapNative(f) -> weak_map_native_refs(f, acc)
    value.WeakSetNative(f) -> weak_set_native_refs(f, acc)
    value.FinalizationRegistryNative(f) ->
      finalization_registry_native_refs(f, acc)
    value.DisposableStackNative(f) -> disposable_stack_native_refs(f, acc)
    value.IteratorNative(f) -> iterator_native_refs(f, acc)
    value.RegExpNative(f) -> regexp_native_refs(f, acc)
    value.DateNative(f) -> date_native_refs(f, acc)
    value.IntlNative(f) -> intl_native_refs(f, acc)
    value.ArrayBufferNative(f) -> array_buffer_native_refs(f, acc)
    value.AtomicsNative(f) -> atomics_native_refs(f, acc)
    value.TypedArrayNative(f) -> typed_array_native_refs(f, acc)
    value.DataViewNative(f) -> data_view_native_refs(f, acc)
    value.TemporalNative(f) -> temporal_native_refs(f, acc)
    value.ShadowRealmNative(f) -> shadow_realm_native_refs(f, acc)
    value.VmNative(f) -> vm_native_refs(f, acc)
  }
}

fn math_native_refs(
  f: value.MathNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.MathPow
    | value.MathAbs
    | value.MathFloor
    | value.MathCeil
    | value.MathRound
    | value.MathTrunc
    | value.MathSqrt
    | value.MathMax
    | value.MathMin
    | value.MathLog
    | value.MathSin
    | value.MathCos
    | value.MathTan
    | value.MathAsin
    | value.MathAcos
    | value.MathAtan
    | value.MathAtan2
    | value.MathExp
    | value.MathLog2
    | value.MathLog10
    | value.MathRandom
    | value.MathSign
    | value.MathCbrt
    | value.MathHypot
    | value.MathFround
    | value.MathClz32
    | value.MathImul
    | value.MathExpm1
    | value.MathLog1p
    | value.MathSinh
    | value.MathCosh
    | value.MathTanh
    | value.MathAsinh
    | value.MathAcosh
    | value.MathAtanh -> acc
  }
}

fn boolean_native_refs(
  f: value.BooleanNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.BooleanConstructor
    | value.BooleanPrototypeValueOf
    | value.BooleanPrototypeToString -> acc
  }
}

fn number_native_refs(
  f: value.NumberNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.NumberConstructor
    | value.NumberIsNaN
    | value.NumberIsFinite
    | value.NumberIsInteger
    | value.NumberPrototypeValueOf
    | value.NumberPrototypeToString
    | value.GlobalParseInt
    | value.GlobalParseFloat
    | value.GlobalIsNaN
    | value.GlobalIsFinite
    | value.NumberIsSafeInteger
    | value.NumberPrototypeToFixed
    | value.NumberPrototypeToPrecision
    | value.NumberPrototypeToExponential -> acc
  }
}

fn string_native_refs(
  f: value.StringNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.StringPrototypeSymbolIterator
    | value.StringPrototypeCharAt
    | value.StringPrototypeCharCodeAt
    | value.StringPrototypeIndexOf
    | value.StringPrototypeLastIndexOf
    | value.StringPrototypeIncludes
    | value.StringPrototypeStartsWith
    | value.StringPrototypeEndsWith
    | value.StringPrototypeSlice
    | value.StringPrototypeSubstring
    | value.StringPrototypeToLowerCase
    | value.StringPrototypeToUpperCase
    | value.StringPrototypeToLocaleLowerCase
    | value.StringPrototypeToLocaleUpperCase
    | value.StringPrototypeTrim
    | value.StringPrototypeTrimStart
    | value.StringPrototypeTrimEnd
    | value.StringPrototypeSplit
    | value.StringPrototypeConcat
    | value.StringPrototypeToString
    | value.StringPrototypeValueOf
    | value.StringPrototypeRepeat
    | value.StringPrototypePadStart
    | value.StringPrototypePadEnd
    | value.StringPrototypeAt
    | value.StringPrototypeCodePointAt
    | value.StringPrototypeNormalize
    | value.StringPrototypeMatch
    | value.StringPrototypeSearch
    | value.StringPrototypeReplace
    | value.StringPrototypeReplaceAll
    | value.StringPrototypeSubstr
    | value.StringPrototypeLocaleCompare
    | value.StringPrototypeMatchAll
    | value.StringPrototypeIsWellFormed
    | value.StringPrototypeToWellFormed
    | value.StringPrototypeAnchor
    | value.StringPrototypeBig
    | value.StringPrototypeBlink
    | value.StringPrototypeBold
    | value.StringPrototypeFixed
    | value.StringPrototypeFontcolor
    | value.StringPrototypeFontsize
    | value.StringPrototypeItalics
    | value.StringPrototypeLink
    | value.StringPrototypeSmall
    | value.StringPrototypeStrike
    | value.StringPrototypeSub
    | value.StringPrototypeSup
    | value.StringRaw
    | value.StringFromCharCode
    | value.StringFromCodePoint -> acc
  }
}

fn error_native_refs(
  f: value.ErrorNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.ErrorConstructor(proto:)
    | value.AggregateErrorConstructor(proto:)
    | value.SuppressedErrorConstructor(proto:)
    | value.ErrorStackSetter(proto:)
    | value.DomExceptionConstructor(proto:) -> [proto, ..acc]
    value.ErrorPrototypeToString
    | value.ErrorCaptureStackTrace
    | value.ErrorStackGetter
    | value.ErrorIsError
    | value.DomExceptionGetCode -> acc
  }
}

fn array_native_refs(
  f: value.ArrayNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.ArrayConstructor
    | value.ArrayIsArray
    | value.ArrayPrototypeJoin
    | value.ArrayPrototypePush
    | value.ArrayPrototypePop
    | value.ArrayPrototypeShift
    | value.ArrayPrototypeUnshift
    | value.ArrayPrototypeSlice
    | value.ArrayPrototypeConcat
    | value.ArrayPrototypeReverse
    | value.ArrayPrototypeFill
    | value.ArrayPrototypeAt
    | value.ArrayPrototypeIndexOf
    | value.ArrayPrototypeLastIndexOf
    | value.ArrayPrototypeIncludes
    | value.ArrayPrototypeForEach
    | value.ArrayPrototypeMap
    | value.ArrayPrototypeFilter
    | value.ArrayPrototypeReduce
    | value.ArrayPrototypeReduceRight
    | value.ArrayPrototypeEvery
    | value.ArrayPrototypeSome
    | value.ArrayPrototypeFind
    | value.ArrayPrototypeFindIndex
    | value.ArrayPrototypeSort
    | value.ArrayPrototypeSplice
    | value.ArrayPrototypeFindLast
    | value.ArrayPrototypeFindLastIndex
    | value.ArrayPrototypeFlat
    | value.ArrayPrototypeFlatMap
    | value.ArrayPrototypeCopyWithin
    | value.ArrayPrototypeToSpliced
    | value.ArrayPrototypeWith
    | value.ArrayPrototypeToSorted
    | value.ArrayPrototypeToReversed
    | value.ArrayPrototypeToString
    | value.ArrayPrototypeToLocaleString
    | value.ArrayPrototypeKeys
    | value.ArrayPrototypeValues
    | value.ArrayPrototypeEntries
    | value.ArrayFrom
    | value.ArrayOf -> acc
  }
}

fn object_native_refs(
  f: value.ObjectNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.ObjectConstructor
    | value.ObjectGetOwnPropertyDescriptor
    | value.ObjectDefineProperty
    | value.ObjectDefineProperties
    | value.ObjectGetOwnPropertyNames
    | value.ObjectKeys
    | value.ObjectValues
    | value.ObjectEntries
    | value.ObjectCreate
    | value.ObjectAssign
    | value.ObjectIs
    | value.ObjectHasOwn
    | value.ObjectGetPrototypeOf
    | value.ObjectSetPrototypeOf
    | value.ObjectFreeze
    | value.ObjectIsFrozen
    | value.ObjectIsExtensible
    | value.ObjectPreventExtensions
    | value.ObjectPrototypeHasOwnProperty
    | value.ObjectPrototypePropertyIsEnumerable
    | value.ObjectPrototypeToString
    | value.ObjectPrototypeValueOf
    | value.ObjectFromEntries
    | value.ObjectSeal
    | value.ObjectIsSealed
    | value.ObjectGetOwnPropertyDescriptors
    | value.ObjectGetOwnPropertySymbols
    | value.ObjectPrototypeIsPrototypeOf
    | value.ObjectPrototypeToLocaleString
    | value.ObjectGroupBy
    | value.ObjectPrototypeDefineGetter
    | value.ObjectPrototypeDefineSetter
    | value.ObjectPrototypeLookupGetter
    | value.ObjectPrototypeLookupSetter
    | value.ObjectPrototypeProtoGetter
    | value.ObjectPrototypeProtoSetter -> acc
  }
}

fn console_native_refs(
  f: value.ConsoleNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.ConsoleLog | value.ConsoleLogError -> acc
  }
}

fn json_native_refs(
  f: value.JsonNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.JsonParse(fn_proto:)
    | value.JsonStringify(fn_proto:)
    | value.JsonRawJson(fn_proto:)
    | value.JsonIsRawJson(fn_proto:) -> [fn_proto, ..acc]
  }
}

fn reflect_native_refs(
  f: value.ReflectNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.ReflectApply
    | value.ReflectConstruct
    | value.ReflectDefineProperty
    | value.ReflectDeleteProperty
    | value.ReflectGet
    | value.ReflectGetOwnPropertyDescriptor
    | value.ReflectGetPrototypeOf
    | value.ReflectHas
    | value.ReflectIsExtensible
    | value.ReflectOwnKeys
    | value.ReflectPreventExtensions
    | value.ReflectSet
    | value.ReflectSetPrototypeOf -> acc
  }
}

fn map_native_refs(
  f: value.MapNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.MapConstructor(proto:) -> [proto, ..acc]
    value.MapPrototypeGet
    | value.MapPrototypeSet
    | value.MapPrototypeHas
    | value.MapPrototypeDelete
    | value.MapPrototypeClear
    | value.MapPrototypeForEach
    | value.MapPrototypeGetSize
    | value.MapPrototypeKeys
    | value.MapPrototypeValues
    | value.MapPrototypeEntries -> acc
  }
}

fn set_native_refs(
  f: value.SetNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.SetConstructor(proto:) -> [proto, ..acc]
    value.SetPrototypeAdd
    | value.SetPrototypeHas
    | value.SetPrototypeDelete
    | value.SetPrototypeClear
    | value.SetPrototypeForEach
    | value.SetPrototypeGetSize
    | value.SetPrototypeUnion
    | value.SetPrototypeIntersection
    | value.SetPrototypeDifference
    | value.SetPrototypeSymmetricDifference
    | value.SetPrototypeIsSubsetOf
    | value.SetPrototypeIsSupersetOf
    | value.SetPrototypeIsDisjointFrom
    | value.SetPrototypeValues
    | value.SetPrototypeEntries -> acc
  }
}

fn weak_map_native_refs(
  f: value.WeakMapNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.WeakMapConstructor(proto:) -> [proto, ..acc]
    value.WeakMapPrototypeGet
    | value.WeakMapPrototypeSet
    | value.WeakMapPrototypeHas
    | value.WeakMapPrototypeDelete
    | value.WeakMapPrototypeGetOrInsert
    | value.WeakMapPrototypeGetOrInsertComputed -> acc
  }
}

fn weak_set_native_refs(
  f: value.WeakSetNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.WeakSetConstructor(proto:) -> [proto, ..acc]
    value.WeakSetPrototypeAdd
    | value.WeakSetPrototypeHas
    | value.WeakSetPrototypeDelete -> acc
  }
}

fn finalization_registry_native_refs(
  f: value.FinalizationRegistryNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.FinalizationRegistryConstructor(proto:) -> [proto, ..acc]
    value.FinalizationRegistryPrototypeRegister
    | value.FinalizationRegistryPrototypeUnregister -> acc
  }
}

fn disposable_stack_native_refs(
  f: value.DisposableStackNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.DisposableStackConstructor(proto:)
    | value.DisposableStackPrototypeMove(proto:)
    | value.AsyncDisposableStackConstructor(proto:)
    | value.AsyncDisposableStackPrototypeMove(proto:) -> [proto, ..acc]
    // disposeAsync continuation handlers keep the remaining resources,
    // pending error, and the capability's resolve/reject alive.
    value.AsyncDisposeContinue(
      remaining:,
      pending:,
      resolve:,
      reject:,
      is_reject: _,
    ) ->
      push_dispose_resources(
        remaining,
        push_value_ref(
          resolve,
          push_value_ref(reject, push_option_value(pending, acc)),
        ),
      )
    // using-declaration disposers keep their method and resource alive.
    value.UsingDisposer(method:, value: v, discard: _) ->
      push_value_ref(method, push_value_ref(v, acc))
    value.DisposableStackPrototypeDispose
    | value.DisposableStackPrototypeUse
    | value.DisposableStackPrototypeAdopt
    | value.DisposableStackPrototypeDefer
    | value.DisposableStackDisposedGetter
    | value.AsyncDisposableStackPrototypeDisposeAsync
    | value.AsyncDisposableStackPrototypeUse
    | value.AsyncDisposableStackPrototypeAdopt
    | value.AsyncDisposableStackPrototypeDefer
    | value.AsyncDisposableStackDisposedGetter -> acc
  }
}

fn iterator_native_refs(
  f: value.IteratorNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.IteratorConstructor
    | value.IteratorFrom
    | value.IteratorZip
    | value.IteratorZipKeyed
    | value.IteratorConcat
    | value.IteratorPrototypeToArray
    | value.IteratorPrototypeForEach
    | value.IteratorPrototypeReduce
    | value.IteratorPrototypeSome
    | value.IteratorPrototypeEvery
    | value.IteratorPrototypeFind
    | value.IteratorPrototypeMap
    | value.IteratorPrototypeFilter
    | value.IteratorPrototypeTake
    | value.IteratorPrototypeDrop
    | value.IteratorPrototypeFlatMap
    | value.IteratorHelperNext
    | value.IteratorHelperReturn
    | value.WrapForValidIteratorNext
    | value.WrapForValidIteratorReturn
    | value.IteratorProtoGetToStringTag
    | value.IteratorProtoSetToStringTag
    | value.IteratorProtoGetConstructor
    | value.IteratorProtoSetConstructor -> acc
  }
}

fn regexp_native_refs(
  f: value.RegExpNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    // Legacy static accessors keep their owning realm's %RegExp% alive.
    value.RegExpLegacyGetter(ctor:, slot: _)
    | value.RegExpLegacyInputSetter(ctor:) -> [ctor, ..acc]
    // The constructor's legacy state is all Strings — no heap edges.
    value.RegExpConstructor(legacy: _)
    | value.RegExpPrototypeTest
    | value.RegExpPrototypeExec
    | value.RegExpPrototypeToString
    | value.RegExpPrototypeCompile
    | value.RegExpGetSource
    | value.RegExpGetFlags
    | value.RegExpGetGlobal
    | value.RegExpGetIgnoreCase
    | value.RegExpGetMultiline
    | value.RegExpGetDotAll
    | value.RegExpGetSticky
    | value.RegExpGetUnicode
    | value.RegExpGetUnicodeSets
    | value.RegExpGetHasIndices
    | value.RegExpSymbolMatch
    | value.RegExpSymbolMatchAll
    | value.RegExpSymbolReplace
    | value.RegExpSymbolSearch
    | value.RegExpSymbolSplit
    | value.RegExpStringIteratorNext -> acc
  }
}

fn date_native_refs(
  f: value.DateNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.DateConstructor(proto:) -> [proto, ..acc]
    value.DateNow
    | value.DateParse
    | value.DateUTC
    | value.DatePrototypeValueOf
    | value.DatePrototypeGetTime
    | value.DatePrototypeGetTimezoneOffset
    | value.DatePrototypeGetFullYear
    | value.DatePrototypeGetUTCFullYear
    | value.DatePrototypeGetMonth
    | value.DatePrototypeGetUTCMonth
    | value.DatePrototypeGetDate
    | value.DatePrototypeGetUTCDate
    | value.DatePrototypeGetDay
    | value.DatePrototypeGetUTCDay
    | value.DatePrototypeGetHours
    | value.DatePrototypeGetUTCHours
    | value.DatePrototypeGetMinutes
    | value.DatePrototypeGetUTCMinutes
    | value.DatePrototypeGetSeconds
    | value.DatePrototypeGetUTCSeconds
    | value.DatePrototypeGetMilliseconds
    | value.DatePrototypeGetUTCMilliseconds
    | value.DatePrototypeSetTime
    | value.DatePrototypeSetMilliseconds
    | value.DatePrototypeSetUTCMilliseconds
    | value.DatePrototypeSetSeconds
    | value.DatePrototypeSetUTCSeconds
    | value.DatePrototypeSetMinutes
    | value.DatePrototypeSetUTCMinutes
    | value.DatePrototypeSetHours
    | value.DatePrototypeSetUTCHours
    | value.DatePrototypeSetDate
    | value.DatePrototypeSetUTCDate
    | value.DatePrototypeSetMonth
    | value.DatePrototypeSetUTCMonth
    | value.DatePrototypeSetFullYear
    | value.DatePrototypeSetUTCFullYear
    | value.DatePrototypeGetYear
    | value.DatePrototypeSetYear
    | value.DatePrototypeToString
    | value.DatePrototypeToDateString
    | value.DatePrototypeToTimeString
    | value.DatePrototypeToISOString
    | value.DatePrototypeToUTCString
    | value.DatePrototypeToLocaleString
    | value.DatePrototypeToLocaleDateString
    | value.DatePrototypeToLocaleTimeString
    | value.DatePrototypeToJSON
    | value.DatePrototypeSymbolToPrimitive -> acc
  }
}

fn intl_native_refs(
  f: value.IntlNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.IntlConstructor(service: _, proto:)
    | value.IntlLocaleMethod(method: _, proto:) -> [proto, ..acc]
    // Bound Intl methods keep their target instance alive.
    value.IntlBoundMethod(service: _, target:) -> [target, ..acc]
    // %SegmentsPrototype% and %SegmentIteratorPrototype% have no other
    // object-graph edge (ECMA-402 exposes no path to them besides these
    // payloads); today they survive GC only because alloc_proto /
    // init_namespace mark them as persistent roots, so scanning here keeps
    // the mark phase honest rather than relying on that rooting policy.
    value.IntlSegmenterSegment(segments_proto:) -> [segments_proto, ..acc]
    value.IntlSegmentsIterator(iter_proto:) -> [iter_proto, ..acc]
    value.IntlGetCanonicalLocales
    | value.IntlSupportedValuesOf
    | value.IntlSupportedLocalesOf(service: _)
    | value.IntlResolvedOptions(service: _)
    | value.IntlBoundGetter(service: _)
    | value.IntlMethod(service: _, method: _)
    | value.IntlHostOverride(which: _)
    | value.IntlLocaleGetter(name: _) -> acc
  }
}

fn array_buffer_native_refs(
  f: value.ArrayBufferNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.ArrayBufferConstructor(proto:)
    | value.SharedArrayBufferConstructor(proto:) -> [proto, ..acc]
    value.ArrayBufferIsView
    | value.ArrayBufferGetByteLength
    | value.ArrayBufferGetDetached
    | value.ArrayBufferGetMaxByteLength
    | value.ArrayBufferGetResizable
    | value.ArrayBufferResize
    | value.ArrayBufferSlice
    | value.ArrayBufferTransfer
    | value.ArrayBufferTransferToFixedLength
    | value.ArrayBufferGetImmutable
    | value.ArrayBufferSliceToImmutable
    | value.ArrayBufferTransferToImmutable
    | value.SharedArrayBufferGetByteLength
    | value.SharedArrayBufferGrow
    | value.SharedArrayBufferGetGrowable
    | value.SharedArrayBufferGetMaxByteLength
    | value.SharedArrayBufferSlice
    | value.DetachArrayBuffer262 -> acc
  }
}

fn atomics_native_refs(
  f: value.AtomicsNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.AtomicsAdd
    | value.AtomicsAnd
    | value.AtomicsCompareExchange
    | value.AtomicsExchange
    | value.AtomicsIsLockFree
    | value.AtomicsLoad
    | value.AtomicsOr
    | value.AtomicsStore
    | value.AtomicsSub
    | value.AtomicsWait
    | value.AtomicsWaitAsync
    | value.AtomicsNotify
    | value.AtomicsPause
    | value.AtomicsXor -> acc
  }
}

fn typed_array_native_refs(
  f: value.TypedArrayNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.TypedArrayConstructor(kind: _, proto:) -> [proto, ..acc]
    value.TypedArrayIntrinsicConstructor
    | value.TypedArrayGetBuffer
    | value.TypedArrayGetByteLength
    | value.TypedArrayGetByteOffset
    | value.TypedArrayGetLength
    | value.TypedArrayGetToStringTag
    | value.TypedArrayPrototypeFill
    | value.TypedArrayPrototypeSet
    | value.TypedArrayPrototypeSubarray
    | value.TypedArrayPrototypeSlice
    | value.TypedArrayPrototypeJoin
    | value.TypedArrayPrototypeIndexOf
    | value.TypedArrayPrototypeIncludes
    | value.TypedArrayPrototypeKeys
    | value.TypedArrayPrototypeValues
    | value.TypedArrayPrototypeEntries
    | value.TypedArrayPrototypeAt
    | value.TypedArrayPrototypeToString
    | value.TypedArrayPrototypeCopyWithin
    | value.TypedArrayPrototypeEvery
    | value.TypedArrayPrototypeSome
    | value.TypedArrayPrototypeForEach
    | value.TypedArrayPrototypeMap
    | value.TypedArrayPrototypeFilter
    | value.TypedArrayPrototypeFind
    | value.TypedArrayPrototypeFindIndex
    | value.TypedArrayPrototypeFindLast
    | value.TypedArrayPrototypeFindLastIndex
    | value.TypedArrayPrototypeLastIndexOf
    | value.TypedArrayPrototypeReduce
    | value.TypedArrayPrototypeReduceRight
    | value.TypedArrayPrototypeReverse
    | value.TypedArrayPrototypeToReversed
    | value.TypedArrayPrototypeSort
    | value.TypedArrayPrototypeToSorted
    | value.TypedArrayPrototypeToLocaleString
    | value.TypedArrayPrototypeWith
    | value.TypedArrayFrom
    | value.TypedArrayOf
    | value.Uint8ArrayPrototypeToBase64
    | value.Uint8ArrayPrototypeToHex
    | value.Uint8ArrayPrototypeSetFromBase64
    | value.Uint8ArrayPrototypeSetFromHex
    | value.Uint8ArrayFromBase64
    | value.Uint8ArrayFromHex -> acc
  }
}

fn data_view_native_refs(
  f: value.DataViewNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.DataViewConstructor(proto:) -> [proto, ..acc]
    value.DataViewGetBuffer
    | value.DataViewGetByteLength
    | value.DataViewGetByteOffset
    | value.DataViewGet(element: _)
    | value.DataViewSet(element: _) -> acc
  }
}

fn temporal_native_refs(
  f: value.TemporalNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    // Every Temporal native carries the eight sibling type prototypes.
    value.TemporalCtor(kind: _, protos:)
    | value.TemporalStatic(kind: _, name: _, protos:)
    | value.TemporalGetterFn(getter: _, protos:)
    | value.TemporalMethod(method: _, protos:)
    | value.TemporalNowFn(name: _, protos:) -> temporal_protos_refs(protos, acc)
  }
}

fn temporal_protos_refs(
  protos: value.TemporalProtos,
  acc: List(value.Ref),
) -> List(value.Ref) {
  let value.TemporalProtos(
    plain_date:,
    plain_time:,
    plain_date_time:,
    plain_year_month:,
    plain_month_day:,
    duration:,
    instant:,
    zoned_date_time:,
  ) = protos
  [
    plain_date,
    plain_time,
    plain_date_time,
    plain_year_month,
    plain_month_day,
    duration,
    instant,
    zoned_date_time,
    ..acc
  ]
}

fn shadow_realm_native_refs(
  f: value.ShadowRealmNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.ShadowRealmConstructor(proto:) -> [proto, ..acc]
    // The prototype methods keep their own realm's %Function.prototype%
    // alive — it's the marker used to recover the caller realm at dispatch.
    value.ShadowRealmEvaluate(fn_proto:)
    | value.ShadowRealmImportValue(fn_proto:) -> [fn_proto, ..acc]
    // Wrapped functions keep their target and both realm records alive.
    value.WrappedFunctionCall(target:, caller_realm:, target_realm:) ->
      push_value_ref(target, [caller_realm, target_realm, ..acc])
  }
}

fn vm_native_refs(
  f: value.VmNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.FunctionConstructor
    | value.GeneratorFunctionConstructor
    | value.AsyncGeneratorFunctionConstructor
    | value.AsyncFunctionConstructor
    | value.FunctionToString
    | value.ReturnThis
    | value.Eval
    | value.DecodeURI
    | value.EncodeURI
    | value.DecodeURIComponent
    | value.EncodeURIComponent
    | value.Escape
    | value.Unescape
    | value.EvalScript
    | value.CreateRealm
    | value.Gc
    | value.BigIntGlobal
    | value.BigIntPrototypeToString
    | value.BigIntPrototypeToLocaleString
    | value.BigIntPrototypeValueOf
    | value.ThrowTypeErrorFn
    | value.FunctionHasInstance
    | value.FunctionPrototypeCall -> acc
  }
}

/// Exhaustive tracer for call-level natives (bound functions, promise
/// reaction closures, async continuations, …). Same no-wildcard rule as
/// dispatch_native_fn_refs.
fn call_native_fn_refs(
  f: value.CallNativeFn,
  acc: List(value.Ref),
) -> List(value.Ref) {
  case f {
    value.BoundFunction(target:, bound_this:, bound_args:) ->
      list.fold(
        bound_args,
        push_value_ref(bound_this, [target, ..acc]),
        fn(a, v) { push_value_ref(v, a) },
      )
    value.PromiseResolveFunction(promise_ref:, data_ref:, already_resolved_ref:)
    | value.PromiseRejectFunction(
        promise_ref:,
        data_ref:,
        already_resolved_ref:,
      ) -> [promise_ref, data_ref, already_resolved_ref, ..acc]
    value.PromiseFinallyFulfill(on_finally:, constructor:)
    | value.PromiseFinallyReject(on_finally:, constructor:) ->
      push_value_ref(on_finally, push_value_ref(constructor, acc))
    value.PromiseCapabilityExecutor(resolve_box:, reject_box:) -> [
      resolve_box,
      reject_box,
      ..acc
    ]
    value.PromiseAllResolveElement(
      index: _,
      remaining_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
      reject:,
    ) ->
      push_value_ref(
        reject,
        push_value_ref(resolve, [
          remaining_ref,
          values_ref,
          already_called_ref,
          ..acc
        ]),
      )
    value.PromiseAllSettledResolveElement(
      index: _,
      remaining_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
    )
    | value.PromiseAllSettledRejectElement(
        index: _,
        remaining_ref:,
        values_ref:,
        already_called_ref:,
        resolve:,
      ) ->
      push_value_ref(resolve, [
        remaining_ref,
        values_ref,
        already_called_ref,
        ..acc
      ])
    value.PromiseAnyRejectElement(
      index: _,
      remaining_ref:,
      errors_ref:,
      already_called_ref:,
      resolve:,
      reject:,
    ) ->
      push_value_ref(
        reject,
        push_value_ref(resolve, [
          remaining_ref,
          errors_ref,
          already_called_ref,
          ..acc
        ]),
      )
    value.PromiseKeyedElement(
      index: _,
      remaining_ref:,
      keys_ref:,
      values_ref:,
      already_called_ref:,
      resolve:,
      status_field: _,
    ) ->
      push_value_ref(resolve, [
        remaining_ref,
        keys_ref,
        values_ref,
        already_called_ref,
        ..acc
      ])
    value.PromiseFinallyValueThunk(value: v) -> push_value_ref(v, acc)
    value.PromiseFinallyThrower(reason:) -> push_value_ref(reason, acc)
    value.AsyncResume(async_data_ref:, is_reject: _) -> [async_data_ref, ..acc]
    value.AsyncGeneratorResume(data_ref:, is_reject: _, kind: _) -> [
      data_ref,
      ..acc
    ]
    value.AsyncFromSyncClose(sync_iter:) -> [sync_iter, ..acc]
    value.ArrayFromAsyncOnNext(ctx:) | value.ArrayFromAsyncOnMapped(ctx:) ->
      from_async_ctx_refs(ctx, acc)
    value.ArrayFromAsyncLikeOnValue(ctx:)
    | value.ArrayFromAsyncLikeOnMapped(ctx:) ->
      from_async_like_ctx_refs(ctx, acc)
    value.ArrayFromAsyncCloseReject(iter:, reject:) ->
      acc
      |> push_value_ref(iter, _)
      |> push_value_ref(reject, _)
    value.ArrayFromAsyncRejectWith(error:, reject:) ->
      acc
      |> push_value_ref(error, _)
      |> push_value_ref(reject, _)
    value.ProxyRevoke(proxy:) -> [proxy, ..acc]
    value.FunctionCall
    | value.FunctionApply
    | value.FunctionBind
    | value.StringConstructor
    | value.PromiseConstructor
    | value.PromiseThen
    | value.PromiseCatch
    | value.PromiseFinally
    | value.PromiseResolveStatic
    | value.PromiseRejectStatic
    | value.PromiseAllStatic
    | value.PromiseRaceStatic
    | value.PromiseAllSettledStatic
    | value.PromiseAnyStatic
    | value.PromiseAllKeyedStatic
    | value.PromiseAllSettledKeyedStatic
    | value.GeneratorNext
    | value.GeneratorReturn
    | value.GeneratorThrow
    | value.ArrayIteratorNext
    | value.SetIteratorNext
    | value.MapIteratorNext
    | value.AsyncGeneratorNext
    | value.AsyncGeneratorReturn
    | value.AsyncGeneratorThrow
    | value.AsyncFromSyncNext
    | value.AsyncFromSyncReturn
    | value.AsyncFromSyncThrow
    | value.AsyncFromSyncUnwrap(done: _)
    | value.ArrayFromAsync
    | value.SymbolConstructor
    | value.ProxyConstructor
    | value.ProxyRevocable
    | value.SymbolFor
    | value.SymbolKeyFor
    | value.SymbolPrototypeToString
    | value.SymbolPrototypeValueOf
    | value.SymbolDescriptionGetter
    | value.SymbolPrototypeToPrimitive -> acc
  }
}

/// Refs held by Array.fromAsync's async-iterator continuation context.
/// Destructured (rather than field-accessed) so a new JsValue field in
/// FromAsyncCtx is a compile error here until it is traced.
fn from_async_ctx_refs(
  ctx: value.FromAsyncCtx,
  acc: List(value.Ref),
) -> List(value.Ref) {
  let value.FromAsyncCtx(
    iter:,
    next_method:,
    map_fn:,
    this_arg:,
    target:,
    k: _,
    resolve:,
    reject:,
  ) = ctx
  acc
  |> push_value_ref(iter, _)
  |> push_value_ref(next_method, _)
  |> push_value_ref(map_fn, _)
  |> push_value_ref(this_arg, _)
  |> push_value_ref(target, _)
  |> push_value_ref(resolve, _)
  |> push_value_ref(reject, _)
}

/// Refs held by Array.fromAsync's array-like continuation context.
fn from_async_like_ctx_refs(
  ctx: value.FromAsyncLikeCtx,
  acc: List(value.Ref),
) -> List(value.Ref) {
  let value.FromAsyncLikeCtx(
    items:,
    map_fn:,
    this_arg:,
    target:,
    k: _,
    len: _,
    resolve:,
    reject:,
  ) = ctx
  acc
  |> push_value_ref(items, _)
  |> push_value_ref(map_fn, _)
  |> push_value_ref(this_arg, _)
  |> push_value_ref(target, _)
  |> push_value_ref(resolve, _)
  |> push_value_ref(reject, _)
}
