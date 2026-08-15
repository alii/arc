//// The old interpreter's view of what the compiler now produces. The
//// compiler assembles shared-runtime templates with a `JsVal` constant pool;
//// the old interpreter still indexes a `JsValue` pool, so each of its load
//// sites converts once, here, before running anything.

import arc/compiler.{type ExportSeed, SeedUndefined, SeedUninitialized}
import arc/rt/bytecode
import arc/rt/types.{type JsVal}
import arc/vm/internal/tuple_array
import arc/vm/value.{
  type FuncTemplate, type JsValue, CaptureLocal, EvalNameTable, FuncTemplate,
  JsUndefined, JsUninitialized,
}
import gleam/int
import gleam/list
import gleam/option

/// A compiled template in the old interpreter's shape, constant pool and
/// nested function templates included.
pub fn legacy_template(t: bytecode.FuncTemplate) -> FuncTemplate {
  FuncTemplate(
    name: t.name,
    arity: t.arity,
    length: t.length,
    local_count: t.local_count,
    bytecode: t.bytecode,
    constants: tuple_array.to_list(t.constants)
      |> list.map(legacy_constant)
      |> tuple_array.from_list,
    functions: tuple_array.to_list(t.functions)
      |> list.map(legacy_template)
      |> tuple_array.from_list,
    env_descriptors: list.map(t.env_descriptors, fn(c) {
      let bytecode.CaptureLocal(parent_index:) = c
      CaptureLocal(parent_index:)
    }),
    is_strict: t.is_strict,
    is_arrow: t.is_arrow,
    is_derived_constructor: t.is_derived_constructor,
    is_generator: t.is_generator,
    is_async: t.is_async,
    is_constructor: t.is_constructor,
    is_class_constructor: t.is_class_constructor,
    local_names: option.map(t.local_names, fn(n) {
      EvalNameTable(var_env: legacy_var_env(n.var_env), names: n.names)
    }),
    lexical: t.lexical,
    code_kind: t.code_kind,
  )
}

pub fn legacy_var_env(k: bytecode.VarEnvKind) -> value.VarEnvKind {
  case k {
    bytecode.GlobalVarEnv -> value.GlobalVarEnv
    bytecode.FrameVarEnv -> value.FrameVarEnv
  }
}

pub fn shared_var_env(k: value.VarEnvKind) -> bytecode.VarEnvKind {
  case k {
    value.GlobalVarEnv -> bytecode.GlobalVarEnv
    value.FrameVarEnv -> bytecode.FrameVarEnv
  }
}

/// One pool entry back to the old value type. The pool only ever holds
/// primitives and the TDZ sentinel (see `emit.add_constant`).
pub fn legacy_constant(v: JsVal) -> JsValue {
  case types.classify(v) {
    types.KUndef -> JsUndefined
    types.KNull -> value.JsNull
    types.KBool(b) -> value.JsBool(b)
    types.KNum(types.JInt(i)) -> value.JsNumber(value.Finite(int.to_float(i)))
    types.KNum(types.JFloat(f)) -> value.JsNumber(value.Finite(f))
    types.KNum(types.JNan) -> value.JsNumber(value.NaN)
    types.KNum(types.JPosInf) -> value.JsNumber(value.Infinity)
    types.KNum(types.JNegInf) -> value.JsNumber(value.NegInfinity)
    types.KStr(s) -> value.JsString(s)
    types.KBig(n) -> value.JsBigInt(value.BigInt(n))
    types.KTdz -> JsUninitialized
    types.KSym(_) | types.KHandle(_) ->
      panic as "constant pool holds a symbol or heap reference"
  }
}

/// The value the old linker seeds into an exported local's BoxSlot.
pub fn seed_value(seed: ExportSeed) -> JsValue {
  case seed {
    SeedUndefined -> JsUndefined
    SeedUninitialized -> JsUninitialized
  }
}
