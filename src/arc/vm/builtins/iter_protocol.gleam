/// ES2024 §7.4 — the Iterator Record abstract operations, in ONE place.
///
/// This module exists so that EVERY "consume an iterable" site in the VM can
/// funnel through the real iterator protocol instead of reaching into a
/// concrete collection's heap storage. It sits below both `builtins/iterator`
/// (Iterator.prototype helpers, Iterator.zip/concat, Map/Set constructors) and
/// `builtins/object` (Object.fromEntries, Object.groupBy), which cannot import
/// each other — `builtins/iterator` already depends on `builtins/object` for
/// the proxy-aware [[OwnPropertyKeys]] used by Iterator.zipKeyed.
///
/// `builtins/iterator` re-exports the entry points below, so existing callers
/// keep importing it; new consumers may import either module.
import arc/vm/builtins/helpers.{is_callable}
import arc/vm/key.{Index, Named}
import arc/vm/ops/object
import arc/vm/state.{type State}
import arc/vm/value.{type JsValue, JsNull, JsObject, JsUndefined}
import gleam/option.{type Option, None, Some}
import gleam/result

/// state.type_error but polymorphic in the Ok type — for callsites where the
/// surrounding Result's Ok type isn't JsValue (so state.type_error won't unify).
pub fn type_error_any(
  state: State(host),
  msg: String,
) -> #(State(host), Result(a, JsValue)) {
  let #(err, state) = state.type_error_value(state, msg)
  #(state, Error(err))
}

// ============================================================================
// §7.4.2/§7.4.3/§7.4.4 — obtaining an Iterator Record
// ============================================================================

/// §7.4.3 GetIterator(obj, sync): the @@iterator method must be callable and
/// its result must be an Object; caches the next method (GetIteratorDirect).
///
/// Returns the Iterator Record as `#(iterator, next_method)`. The "<obj> is
/// not iterable" TypeError message is relied on by for-of (interpreter
/// GetIterator), ArraySpread (`arc/vm/ops/array`) and Object.fromEntries /
/// Object.groupBy — keep them in sync.
pub fn get_iterator_sync(
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
    True -> get_iterator_from_method(state, obj, method)
  }
}

/// §7.4.4 GetIteratorFromMethod(obj, method): iterator = ? Call(method, obj);
/// the result must be an Object; the `next` method is read once and cached
/// (GetIteratorDirect). Returns the Iterator Record as `#(iterator, next)`.
///
/// Public for consumers that already performed their own
/// GetMethod(obj, @@iterator) — e.g. Array.from, which must fall back to the
/// array-like path when @@iterator is undefined — and so must NOT re-Get it
/// via `get_iterator_sync`.
pub fn get_iterator_from_method(
  state: State(host),
  obj: JsValue,
  method: JsValue,
) -> Result(#(#(JsValue, JsValue), State(host)), #(JsValue, State(host))) {
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

/// §7.4.13 GetIteratorFlattenable's two call sites, as a type: `Iterator.from`
/// accepts a String receiver and iterates its code points; every other caller
/// (Iterator.zip inputs, Iterator.concat inputs, the flatMap callback result)
/// rejects primitives outright.
pub type PrimitiveHandling {
  IterateStrings
  RejectPrimitives
}

/// §7.4.13 GetIteratorFlattenable(obj, primitiveHandling):
///   1. If obj is not an Object: reject-primitives → TypeError; otherwise obj
///      must be a String.
///   2. method = ? GetMethod(obj, @@iterator).
///   3. If method is undefined, iterator = obj; else iterator = ? Call(method, obj).
///   4. If iterator is not an Object, throw a TypeError.
///   5. Return ? GetIteratorDirect(iterator).
///
/// `what` names the value in the two TypeError messages.
pub fn get_iterator_flattenable(
  state: State(host),
  obj: JsValue,
  handling: PrimitiveHandling,
  what: String,
) -> #(State(host), Result(#(JsValue, JsValue), JsValue)) {
  let acceptable = case obj, handling {
    JsObject(_), _ -> True
    value.JsString(_), IterateStrings -> True
    _, _ -> False
  }
  case acceptable {
    False -> type_error_any(state, what <> " is not an object")
    True -> {
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
  }
}

// ============================================================================
// §7.4.5/§7.4.6/§7.4.8 — stepping an Iterator Record
// ============================================================================

/// Shared §7.4.6/§7.4.8 prefix: call next(obj), require the result is an
/// Object, read .done; continue with the result object and the done flag.
pub fn iterator_step_result(
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
///
/// This is the ONE next()/done/value reader shared by every consumer that
/// drains an already-obtained iterator record: the Iterator.prototype helpers,
/// `iterator_rest`, `zip_collect`, ArraySpread's generic path,
/// Object.fromEntries and Object.groupBy. New drain sites must reuse it rather
/// than re-implementing the §7.4.5/§7.4.6 done/value reads.
pub fn iterator_step_value(
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

// ============================================================================
// §7.4.11 IteratorClose
// ============================================================================

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
) -> #(State(host), Result(a, JsValue)) {
  let #(err, state) = state.type_error_value(state, msg)
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
    #(state, Ok(NoReturnMethod)) -> #(state, Ok(Nil))
    #(state, Ok(Returned(JsObject(_)))) -> #(state, Ok(Nil))
    #(state, Ok(Returned(_other))) ->
      type_error_any(state, "Iterator return result is not an object")
    #(state, Error(thrown)) -> #(state, Error(thrown))
  }
}

/// Successful outcome of `call_return`. §7.4.11 IteratorClose and
/// %WrapForValidIteratorPrototype%.return both need to tell "there was no
/// `return` method" apart from "a `return` method ran and produced this value"
/// — a JsUndefined sentinel cannot (a `return` that RAN and returned undefined
/// must be a TypeError under IteratorClose, not a silent success).
pub type ReturnCall {
  /// GetMethod(iterator, "return") was undefined/null: nothing was called.
  NoReturnMethod
  /// The `return` method was called; this is its (unchecked) result.
  Returned(JsValue)
}

/// Shared body of IteratorClose: GetMethod(iterator, "return") and, if
/// present, call it. Callers decide what the §7.4.11 completion rules make of
/// the two `ReturnCall` outcomes.
pub fn call_return(
  state: State(host),
  obj: JsValue,
) -> #(State(host), Result(ReturnCall, JsValue)) {
  use ret_fn, state <- state.try_op(object.get_value_of(
    state,
    obj,
    Named("return"),
  ))
  case ret_fn {
    JsUndefined | JsNull -> #(state, Ok(NoReturnMethod))
    _ ->
      // §7.3.10 GetMethod step 3: a non-callable `return` property is a
      // TypeError. The re-entrant call path silently passes non-callables
      // through (legacy promise-reaction behavior), so check here.
      case is_callable(state.heap, ret_fn) {
        False -> type_error_any(state, "iterator.return is not a function")
        True -> {
          use result, state <- state.try_call(state, ret_fn, obj, [])
          #(state, Ok(Returned(result)))
        }
      }
  }
}

/// Unwrap an op result, or IteratorClose with the thrown error (original
/// error wins over any error from the close itself — §7.4.11).
pub fn or_close(
  res: Result(#(a, State(host)), #(JsValue, State(host))),
  iter: JsValue,
  cont: fn(a, State(host)) -> #(State(host), Result(b, JsValue)),
) -> #(State(host), Result(b, JsValue)) {
  case res {
    Ok(#(v, state)) -> cont(v, state)
    Error(#(thrown, state)) -> close_throw(state, iter, thrown)
  }
}

// ============================================================================
// §7.4.9 AddEntriesFromIterable — the shared entry-drain loop
// ============================================================================

/// The per-entry sink of AddEntriesFromIterable, at the Gleam level. The Map /
/// WeakMap constructors pass a sink that [[Call]]s the user-reachable `set`
/// adder; Object.fromEntries passes one that does CreateDataPropertyOrThrow.
/// Both then share the ONE §7.4.9 loop below (GetIterator, per-entry
/// object check, Get "0"/"1", IteratorClose on any abrupt completion).
pub type EntrySink(host) =
  fn(State(host), JsValue, JsValue) ->
    Result(State(host), #(JsValue, State(host)))

/// §24.1.1.2 AddEntriesFromIterable ( target, iterable, addEntry ) — full
/// iterator protocol: GetIterator, then per entry Get "0"/"1" and run
/// `add_entry`, closing the iterator on any abrupt completion inside the loop.
/// Returns `target` on normal completion.
pub fn add_entries_from_iterable(
  state: State(host),
  target: JsValue,
  iterable: JsValue,
  add_entry: EntrySink(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  use iter_rec, state <- state.try_op(get_iterator_sync(state, iterable))
  let #(iter, next) = iter_rec
  add_entries_loop(state, target, iter, next, add_entry)
}

/// One IteratorStepValue + entry processing per iteration. Abrupt
/// completions from next()/Get(done)/Get(value) propagate without close
/// (§7.4.8 marks the iterator done); abrupt completions from the entry
/// reads or the sink close the iterator first (§24.1.1.2 step 4).
fn add_entries_loop(
  state: State(host),
  target: JsValue,
  iter: JsValue,
  next: JsValue,
  add_entry: EntrySink(host),
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
          case add_entry(state, k, v) {
            Error(#(thrown, state)) -> close_throw(state, iter, thrown)
            Ok(state) -> add_entries_loop(state, target, iter, next, add_entry)
          }
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
