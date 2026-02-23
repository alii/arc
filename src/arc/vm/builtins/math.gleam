import arc/vm/heap.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, Finite, Infinity, JsNumber, JsObject, JsString, NaN,
  NativeFunction, NativeMathAbs, NativeMathCeil, NativeMathCos, NativeMathFloor,
  NativeMathLog, NativeMathMax, NativeMathMin, NativeMathPow, NativeMathRound,
  NativeMathSin, NativeMathSqrt, NativeMathTrunc, NegInfinity, ObjectSlot,
  OrdinaryObject,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}

/// Set up the Math global object.
/// Math is NOT a constructor — it's a plain object with static methods.
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  // Allocate Math as a plain ordinary object
  let #(h, math_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: dict.new(),
        elements: dict.new(),
        prototype: Some(object_proto),
      ),
    )
  let h = heap.root(h, math_ref)

  // Add all Math methods
  let methods = [
    #("pow", NativeMathPow, 2),
    #("abs", NativeMathAbs, 1),
    #("floor", NativeMathFloor, 1),
    #("ceil", NativeMathCeil, 1),
    #("round", NativeMathRound, 1),
    #("trunc", NativeMathTrunc, 1),
    #("sqrt", NativeMathSqrt, 1),
    #("max", NativeMathMax, 2),
    #("min", NativeMathMin, 2),
    #("log", NativeMathLog, 1),
    #("sin", NativeMathSin, 1),
    #("cos", NativeMathCos, 1),
  ]

  let h =
    list.fold(methods, h, fn(h, method) {
      let #(name, native, length) = method
      let #(h, fn_ref) =
        alloc_native_fn(h, function_proto, native, name, length)
      add_method(h, math_ref, name, fn_ref)
    })

  // Add Math constants (non-enumerable, non-writable, non-configurable)
  let h = add_constant(h, math_ref, "PI", JsNumber(Finite(3.141592653589793)))
  let h = add_constant(h, math_ref, "E", JsNumber(Finite(2.718281828459045)))

  #(h, math_ref)
}

// ============================================================================
// Math method implementations
// ============================================================================

/// Math.pow(base, exponent)
pub fn math_pow(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let base = get_num_arg(args, 0)
  let exponent = get_num_arg(args, 1)
  #(heap, Ok(JsNumber(num_exp(base, exponent))))
}

/// Math.abs(x)
pub fn math_abs(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) -> Finite(float.absolute_value(n))
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> Infinity
  }
  #(heap, Ok(JsNumber(result)))
}

/// Math.floor(x)
pub fn math_floor(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) -> Finite(ffi_math_floor(n))
    other -> other
  }
  #(heap, Ok(JsNumber(result)))
}

/// Math.ceil(x)
pub fn math_ceil(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) -> Finite(ffi_math_ceil(n))
    other -> other
  }
  #(heap, Ok(JsNumber(result)))
}

/// Math.round(x) — JS round: round half toward +Infinity (NOT banker's rounding)
pub fn math_round(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) -> Finite(js_round(n))
    other -> other
  }
  #(heap, Ok(JsNumber(result)))
}

/// Math.trunc(x)
pub fn math_trunc(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) -> Finite(int.to_float(float_to_int(n)))
    other -> other
  }
  #(heap, Ok(JsNumber(result)))
}

/// Math.sqrt(x)
pub fn math_sqrt(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) ->
      case n <. 0.0 {
        True -> NaN
        False -> Finite(ffi_math_sqrt(n))
      }
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> NaN
  }
  #(heap, Ok(JsNumber(result)))
}

/// Math.max(a, b, ...)
pub fn math_max(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    [] -> #(heap, Ok(JsNumber(NegInfinity)))
    _ -> {
      let result =
        list.fold(args, NegInfinity, fn(acc, arg) {
          let n = to_number(arg)
          case acc, n {
            NaN, _ -> NaN
            _, NaN -> NaN
            Finite(a), Finite(b) ->
              case a >=. b {
                True -> Finite(a)
                False -> Finite(b)
              }
            Infinity, _ -> Infinity
            _, Infinity -> Infinity
            NegInfinity, other -> other
            other, NegInfinity -> other
          }
        })
      #(heap, Ok(JsNumber(result)))
    }
  }
}

/// Math.min(a, b, ...)
pub fn math_min(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  case args {
    [] -> #(heap, Ok(JsNumber(Infinity)))
    _ -> {
      let result =
        list.fold(args, Infinity, fn(acc, arg) {
          let n = to_number(arg)
          case acc, n {
            NaN, _ -> NaN
            _, NaN -> NaN
            Finite(a), Finite(b) ->
              case a <=. b {
                True -> Finite(a)
                False -> Finite(b)
              }
            NegInfinity, _ -> NegInfinity
            _, NegInfinity -> NegInfinity
            Infinity, other -> other
            other, Infinity -> other
          }
        })
      #(heap, Ok(JsNumber(result)))
    }
  }
}

/// Math.log(x)
pub fn math_log(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) ->
      case n <. 0.0 {
        True -> NaN
        False ->
          case n == 0.0 {
            True -> NegInfinity
            False -> Finite(ffi_math_log(n))
          }
      }
    NaN -> NaN
    Infinity -> Infinity
    NegInfinity -> NaN
  }
  #(heap, Ok(JsNumber(result)))
}

/// Math.sin(x)
pub fn math_sin(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) -> Finite(ffi_math_sin(n))
    _ -> NaN
  }
  #(heap, Ok(JsNumber(result)))
}

/// Math.cos(x)
pub fn math_cos(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let x = get_num_arg(args, 0)
  let result = case x {
    Finite(n) -> Finite(ffi_math_cos(n))
    _ -> NaN
  }
  #(heap, Ok(JsNumber(result)))
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Simplified ToNumber for Math operations.
pub fn to_number(val: JsValue) -> value.JsNum {
  case val {
    JsNumber(n) -> n
    value.JsUndefined -> NaN
    value.JsNull -> Finite(0.0)
    value.JsBool(True) -> Finite(1.0)
    value.JsBool(False) -> Finite(0.0)
    JsString("") -> Finite(0.0)
    JsString("Infinity") -> Infinity
    JsString("-Infinity") -> NegInfinity
    JsString(s) ->
      case parse_float(s) {
        Ok(n) -> Finite(n)
        Error(_) ->
          case parse_int(s) {
            Ok(n) -> Finite(int.to_float(n))
            Error(_) -> NaN
          }
      }
    _ -> NaN
  }
}

/// Get numeric arg at position, defaulting to NaN if missing.
fn get_num_arg(args: List(JsValue), idx: Int) -> value.JsNum {
  case list_at(args, idx) {
    Some(v) -> to_number(v)
    None -> NaN
  }
}

fn list_at(lst: List(a), idx: Int) -> option.Option(a) {
  case idx, lst {
    0, [x, ..] -> Some(x)
    n, [_, ..rest] -> list_at(rest, n - 1)
    _, [] -> None
  }
}

/// JS Math.round: round half toward +Infinity.
/// Math.round(-0.5) → 0, Math.round(0.5) → 1
fn js_round(n: Float) -> Float {
  let floored = ffi_math_floor(n)
  case n -. floored >=. 0.5 {
    True -> floored +. 1.0
    False -> floored
  }
}

fn float_to_int(f: Float) -> Int {
  case f <. 0.0 {
    True -> 0 - float.truncate(float.negate(f))
    False -> float.truncate(f)
  }
}

/// Exponentiation with special-value handling (mirrors vm.gleam's num_exp).
fn num_exp(a: value.JsNum, b: value.JsNum) -> value.JsNum {
  case a, b {
    _, Finite(0.0) -> Finite(1.0)
    _, NaN -> NaN
    NaN, _ -> NaN
    Finite(x), Finite(y) -> Finite(float_power(x, y))
    Infinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    NegInfinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    _, Infinity -> NaN
    _, NegInfinity -> NaN
  }
}

// -- FFI --

@external(erlang, "arc_vm_ffi", "float_power")
fn float_power(base: Float, exp: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_sqrt")
fn ffi_math_sqrt(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_log")
fn ffi_math_log(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_sin")
fn ffi_math_sin(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_cos")
fn ffi_math_cos(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_floor")
fn ffi_math_floor(x: Float) -> Float

@external(erlang, "arc_vm_ffi", "math_ceil")
fn ffi_math_ceil(x: Float) -> Float

fn parse_float(s: String) -> Result(Float, Nil) {
  case gleam_stdlib_parse_float(s) {
    Ok(f) -> Ok(f)
    Error(_) -> Error(Nil)
  }
}

@external(erlang, "gleam_stdlib", "parse_float")
fn gleam_stdlib_parse_float(s: String) -> Result(Float, Nil)

fn parse_int(s: String) -> Result(Int, Nil) {
  int.parse(s)
}

/// Allocate a native function object on the heap, root it, and return the ref.
fn alloc_native_fn(
  h: Heap,
  function_proto: Ref,
  native: value.NativeFn,
  name: String,
  length: Int,
) -> #(Heap, Ref) {
  let #(h, ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: NativeFunction(native),
        properties: dict.from_list([
          #("name", value.builtin_property(JsString(name))),
          #(
            "length",
            value.builtin_property(JsNumber(Finite(int.to_float(length)))),
          ),
        ]),
        elements: dict.new(),
        prototype: Some(function_proto),
      ),
    )
  let h = heap.root(h, ref)
  #(h, ref)
}

/// Add a non-enumerable method property to an object on the heap.
fn add_method(h: Heap, obj_ref: Ref, name: String, fn_ref: Ref) -> Heap {
  case heap.read(h, obj_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let new_props =
        dict.insert(properties, name, value.builtin_property(JsObject(fn_ref)))
      heap.write(
        h,
        obj_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }
}

/// Add a non-enumerable, non-writable, non-configurable constant to an object.
fn add_constant(h: Heap, obj_ref: Ref, name: String, val: JsValue) -> Heap {
  case heap.read(h, obj_ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) -> {
      let prop =
        value.DataProperty(
          value: val,
          writable: False,
          enumerable: False,
          configurable: False,
        )
      let new_props = dict.insert(properties, name, prop)
      heap.write(
        h,
        obj_ref,
        ObjectSlot(kind:, properties: new_props, elements:, prototype:),
      )
    }
    _ -> h
  }
}
