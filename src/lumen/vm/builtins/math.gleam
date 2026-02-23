import gleam/dict
import gleam/int
import gleam/option.{Some}
import lumen/vm/heap.{type Heap}
import lumen/vm/value.{
  type JsValue, type Ref, Finite, Infinity, JsNumber, JsObject, JsString, NaN,
  NativeFunction, NativeMathPow, NegInfinity, ObjectSlot, OrdinaryObject,
}

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

  // Math.pow — static method
  let #(h, pow_ref) =
    alloc_native_fn(h, function_proto, NativeMathPow, "pow", 2)
  let h = add_method(h, math_ref, "pow", pow_ref)

  #(h, math_ref)
}

/// Math.pow(base, exponent)
pub fn math_pow(
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue)) {
  let base = case args {
    [b, ..] -> to_number(b)
    [] -> Finite(0.0)
  }
  let exponent = case args {
    [_, e, ..] -> to_number(e)
    _ -> Finite(0.0)
  }
  #(heap, Ok(JsNumber(num_exp(base, exponent))))
}

/// Simplified ToNumber for Math operations.
fn to_number(val: JsValue) -> value.JsNum {
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

@external(erlang, "lumen_vm_ffi", "float_power")
fn float_power(base: Float, exp: Float) -> Float

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
