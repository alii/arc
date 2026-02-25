/// Shared runtime helpers for builtins.
import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, Finite, JsNumber, JsObject, JsString, JsUndefined, NaN,
  ObjectSlot,
}
import gleam/int
import gleam/option.{type Option, None, Some}

/// ES2024 §7.2.3 IsCallable(argument)
///
/// 1. If argument is not an Object, return false.
/// 2. If argument has a [[Call]] internal method, return true.
/// 3. Return false.
///
/// We check for FunctionObject or NativeFunction object kinds instead of a
/// [[Call]] internal method slot, since our object representation uses tagged
/// kinds rather than method tables.
pub fn is_callable(h: Heap, val: JsValue) -> Bool {
  // Step 1: If argument is not an Object, return false.
  case val {
    JsObject(ref) ->
      case heap.read(h, ref) {
        // Step 2: If argument has a [[Call]] internal method, return true.
        Some(ObjectSlot(kind: value.FunctionObject(..), ..)) -> True
        Some(ObjectSlot(kind: value.NativeFunction(_), ..)) -> True
        // Step 3: Return false.
        _ -> False
      }
    _ -> False
  }
}

/// Get element at index from a list (0-based). O(n).
/// Non-spec utility — used by get_int_arg/get_num_arg for argument access.
fn list_at(lst: List(a), idx: Int) -> Option(a) {
  case idx, lst {
    0, [x, ..] -> Some(x)
    n, [_, ..rest] -> list_at(rest, n - 1)
    _, [] -> None
  }
}

/// Partial implementation of ES2024 §7.1.5 ToIntegerOrInfinity(argument)
/// combined with §7.1.4 ToNumber(argument).
///
/// Spec steps for ToIntegerOrInfinity:
///   1. Let number be ? ToNumber(argument).
///   2. If number is NaN, +0, or -0, return 0.
///   3. If number is +Infinity, return +Infinity.
///   4. If number is -Infinity, return -Infinity.
///   5. Return truncate(number).
///
/// Returns Option(Int) instead of a numeric type — None stands for
/// NaN/Infinity/undefined (callers supply a default). This collapses
/// steps 2-4 differently: NaN/Infinity -> None, finite -> truncated Int.
/// The ToNumber conversion (step 1) is inlined here using the §7.1.4 table:
///   undefined -> NaN, null -> +0, true -> 1, false -> 0, string -> parsed.
/// Objects are not handled (would need ToPrimitive first).
pub fn to_number_int(val: JsValue) -> Option(Int) {
  case val {
    // §7.1.4: Number -> identity; then §7.1.5 step 5: truncate
    JsNumber(Finite(n)) -> Some(value.float_to_int(n))
    // §7.1.5 steps 3-4: Infinity -> None (caller supplies default)
    JsNumber(_) -> None
    // §7.1.4: undefined -> NaN; §7.1.5 step 2: NaN -> None
    JsUndefined -> None
    // §7.1.4: null -> +0
    value.JsNull -> Some(0)
    // §7.1.4: true -> 1, false -> 0
    value.JsBool(True) -> Some(1)
    value.JsBool(False) -> Some(0)
    // §7.1.4: String -> StringToNumber (partial — integer/float parse only)
    JsString(s) ->
      case int.parse(s) {
        Ok(n) -> Some(n)
        Error(_) ->
          case gleam_stdlib_parse_float(s) {
            Ok(f) -> Some(value.float_to_int(f))
            // Non-numeric string -> NaN -> None
            Error(_) -> None
          }
      }
    // Objects would need ToPrimitive — not handled, returns None
    _ -> None
  }
}

/// Non-spec utility: get an integer argument at position `idx`, with a default
/// if missing or not numeric. Uses to_number_int (ToIntegerOrInfinity) internally.
pub fn get_int_arg(args: List(JsValue), idx: Int, default: Int) -> Int {
  case list_at(args, idx) {
    Some(v) -> to_number_int(v) |> option.unwrap(default)
    None -> default
  }
}

/// Non-spec utility: get a numeric (JsNum) argument at position `idx`,
/// defaulting to NaN. The `to_number` callback performs §7.1.4 ToNumber.
pub fn get_num_arg(
  args: List(JsValue),
  idx: Int,
  to_number: fn(JsValue) -> value.JsNum,
) -> value.JsNum {
  case list_at(args, idx) {
    Some(v) -> to_number(v)
    None -> NaN
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)
