//// The two abstract operations that `String.prototype` and `RegExp.prototype`
//// share, in one place so they cannot drift apart:
////
////   * §7.2.6 IsRegExp — used by the RegExp constructor and by
////     `String.prototype.{includes,startsWith,endsWith,matchAll,replaceAll}`.
////   * The `matchAll`/`replaceAll` "must be a global RegExp" guard
////     (§22.1.3.14 step 2.b / §22.1.3.19 step 2.a).

import arc/vm/heap
import arc/vm/key.{Named}
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type State}
import arc/vm/value.{type JsValue, JsNull, JsObject, JsUndefined}
import gleam/option
import gleam/string

/// ES2024 §7.2.6 IsRegExp ( argument )
///   1. If argument is not an Object, return false.
///   2. Let matcher be ? Get(argument, @@match).
///   3. If matcher is not undefined, return ToBoolean(matcher).
///   4. If argument has a [[RegExpMatcher]] internal slot, return true.
///   5. Return false.
///
/// CPS because step 2 is an observable [[Get]] that can throw:
///
///   use is_re, state <- regexp_ops.is_regexp(state, val)
pub fn is_regexp(
  state: State(host),
  val: JsValue,
  cont: fn(Bool, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    JsObject(ref) -> {
      // Step 2.
      use matcher, state <- state.try_op(object.get_symbol_value(
        state,
        ref,
        value.symbol_match,
        val,
      ))
      case matcher {
        // Step 4.
        JsUndefined ->
          cont(option.is_some(heap.read_regexp(state.heap, ref)), state)
        // Step 3.
        other -> cont(value.is_truthy(other), state)
      }
    }
    // Step 1.
    _ -> cont(False, state)
  }
}

/// `String.prototype.matchAll` step 2.b / `String.prototype.replaceAll`
/// step 2.a, verbatim: the caller has already established that `val` is an
/// IsRegExp object, so
///
///   1. Let flags be ? Get(val, "flags").
///   2. Perform ? RequireObjectCoercible(flags).
///   3. If ! ToString(flags) does not contain "g", throw a TypeError.
///
/// `method` names the caller for the message ("matchAll", "replaceAll").
pub fn require_global_flags(
  state: State(host),
  val: JsValue,
  method: String,
  cont: fn(Nil, State(host)) -> #(State(host), Result(JsValue, JsValue)),
) -> #(State(host), Result(JsValue, JsValue)) {
  case val {
    JsObject(ref) -> {
      // Step 1.
      use flags_val, state <- state.try_op(object.get_value(
        state,
        ref,
        Named("flags"),
        val,
      ))
      case flags_val {
        // Step 2.
        JsUndefined | JsNull ->
          state.type_error(
            state,
            "String.prototype."
              <> method
              <> ": the .flags property of the argument must not be undefined or null",
          )
        // Step 3.
        _ -> {
          use flags_str, state <- coerce.try_to_string(state, flags_val)
          case string.contains(flags_str, "g") {
            True -> cont(Nil, state)
            False ->
              state.type_error(
                state,
                "String.prototype."
                  <> method
                  <> " called with a non-global RegExp argument",
              )
          }
        }
      }
    }
    // IsRegExp is only true for objects, so a non-object never gets here.
    _ -> cont(Nil, state)
  }
}
