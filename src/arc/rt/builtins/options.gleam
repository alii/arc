// §9.2.12 getoption family

import arc/bytecode/key.{Named}
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, JFloat, JInt, JNan, JNegInf, JPosInf,
  KHandle, KUndef, StringKey, classify, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

pub fn coerce_options_to_object(
  st: Agent,
  v: JsVal,
) -> #(Option(Handle), Agent) {
  case classify(v) {
    KUndef -> #(None, st)
    _ -> {
      let #(h, st) = rt_val.to_object(st, v)
      #(Some(h), st)
    }
  }
}

pub fn get_options_object(st: Agent, v: JsVal) -> Option(Handle) {
  case classify(v) {
    KUndef -> None
    KHandle(h) -> Some(h)
    _ -> rt_val.throw_type_error(st, "options must be an object or undefined")
  }
}

pub fn get_option(
  st: Agent,
  opts: Option(Handle),
  name: String,
) -> #(JsVal, Agent) {
  case opts {
    None -> #(mk_undefined(), st)
    Some(h) -> rt_obj.get_prop(st, mk_object(h), StringKey(Named(name)))
  }
}

pub fn get_text_opt(
  st: Agent,
  opts: Option(Handle),
  name: String,
  allowed: List(String),
  default: Option(String),
) -> #(Option(String), Agent) {
  let #(v, st) = get_option(st, opts, name)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(s, st) = rt_val.to_string(st, v)
      case allowed == [] || list.contains(allowed, s) {
        True -> #(Some(s), st)
        False ->
          rt_val.throw_range_error(
            st,
            "Value " <> s <> " out of range for options property " <> name,
          )
      }
    }
  }
}

pub fn get_enum_opt(
  st: Agent,
  opts: Option(Handle),
  name: String,
  variants: List(#(String, a)),
  default: a,
) -> #(a, Agent) {
  let #(v, st) = get_option(st, opts, name)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(s, st) = rt_val.to_string(st, v)
      case list.key_find(variants, s) {
        Ok(variant) -> #(variant, st)
        Error(Nil) ->
          rt_val.throw_range_error(
            st,
            "Value " <> s <> " out of range for options property " <> name,
          )
      }
    }
  }
}

pub fn get_bool_opt(
  st: Agent,
  opts: Option(Handle),
  name: String,
  default: Option(Bool),
) -> #(Option(Bool), Agent) {
  let #(v, st) = get_option(st, opts, name)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> #(Some(rt_val.to_boolean(v)), st)
  }
}

pub fn get_num_opt(
  st: Agent,
  opts: Option(Handle),
  name: String,
  min: Int,
  max: Int,
  default: Option(Int),
) -> #(Option(Int), Agent) {
  let #(v, st) = get_option(st, opts, name)
  default_number_option(st, v, min, max, default, name)
}

pub fn default_number_option(
  st: Agent,
  v: JsVal,
  min: Int,
  max: Int,
  default: Option(Int),
  name: String,
) -> #(Option(Int), Agent) {
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(n, st) = rt_val.to_number(st, v)
      let f = case n {
        JInt(i) -> Some(int.to_float(i))
        JFloat(f) -> Some(f)
        JNan | JPosInf | JNegInf -> None
      }
      case f {
        Some(f) ->
          // range check uses the unrounded value (§9.2.17)
          case f >=. int.to_float(min) && f <=. int.to_float(max) {
            True -> #(Some(float.truncate(float.floor(f))), st)
            False ->
              rt_val.throw_range_error(
                st,
                name <> " value is out of range: " <> float.to_string(f),
              )
          }
        None -> rt_val.throw_range_error(st, name <> " value is out of range")
      }
    }
  }
}
