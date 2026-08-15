//// `rt_builtins/generator` — %Generator% / %AsyncGenerator% /
//// %GeneratorFunction% / %AsyncGeneratorFunction% / %AsyncFunction% intrinsics
//// (SPEC §7.M6 builtin-control; port of `arc/vm/builtins/generator.gleam` +
//// `arc/vm/builtins/async_generator.gleam` + `common.init_generator_function`).
////
//// `next`/`return`/`throw` bodies live in `rt_async` (`t_gen_next` /
//// `t_gen_return` / `t_gen_throw` and `t_asyncgen_*`) — this module only
//// installs the prototype method objects and routes dispatch.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).**

import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type GeneratorNative, type Handle, type JsVal,
  AsyncFunctionCtor, AsyncGeneratorFunctionCtor, AsyncGeneratorNext,
  AsyncGeneratorReturn, AsyncGeneratorThrow, BuiltinPair, GeneratorFunctionCtor,
  GeneratorN, GeneratorNext, GeneratorReturn, GeneratorThrow, KHandle, KNative,
  NoElements, SObject, TypeErr, classify, mk_object,
} as rt_types
import gleam/dict
import gleam/option.{None, Some}

// ── init: %Generator% + %GeneratorFunction% (§27.3 / §27.5) ─────────────────

/// Set up %GeneratorPrototype% (`.next`/`.return`/`.throw`, inherits
/// %IteratorPrototype%) and the %GeneratorFunction% dynamic-constructor pair.
/// Port of arc `builtins/generator.gleam:15-40`.
pub fn init(
  st: Agent,
  iterator_proto: Handle,
  fn_proto: Handle,
  fn_ctor: Handle,
) -> #(#(BuiltinPair, BuiltinPair), Agent) {
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("next", GeneratorN(GeneratorNext), 1),
      #("return", GeneratorN(GeneratorReturn), 1),
      #("throw", GeneratorN(GeneratorThrow), 1),
    ])
  let #(gen_proto, st) =
    common.init_namespace(st, iterator_proto, "Generator", methods)
  let #(gen_fn, st) =
    init_function_intrinsic(
      st,
      "GeneratorFunction",
      GeneratorN(GeneratorFunctionCtor),
      fn_proto,
      fn_ctor,
      Some(gen_proto),
    )
  #(
    #(
      BuiltinPair(prototype: gen_proto, constructor: gen_fn.constructor),
      gen_fn,
    ),
    st,
  )
}

/// Set up %AsyncGeneratorPrototype% (inherits %AsyncIteratorPrototype%) and
/// the %AsyncGeneratorFunction% pair. Port of arc
/// `builtins/async_generator.gleam:13-40`.
pub fn init_async(
  st: Agent,
  async_iterator_proto: Handle,
  fn_proto: Handle,
  fn_ctor: Handle,
) -> #(#(BuiltinPair, BuiltinPair), Agent) {
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("next", GeneratorN(AsyncGeneratorNext), 1),
      #("return", GeneratorN(AsyncGeneratorReturn), 1),
      #("throw", GeneratorN(AsyncGeneratorThrow), 1),
    ])
  let #(agen_proto, st) =
    common.init_namespace(st, async_iterator_proto, "AsyncGenerator", methods)
  let #(agen_fn, st) =
    init_function_intrinsic(
      st,
      "AsyncGeneratorFunction",
      GeneratorN(AsyncGeneratorFunctionCtor),
      fn_proto,
      fn_ctor,
      Some(agen_proto),
    )
  #(
    #(
      BuiltinPair(prototype: agen_proto, constructor: agen_fn.constructor),
      agen_fn,
    ),
    st,
  )
}

/// §27.7 %AsyncFunction% + %AsyncFunction.prototype% (the [[Prototype]] of
/// async function objects). No `prototype` on fn_proto — async functions are
/// not constructors. Port of arc `common.init_async_function`.
pub fn init_async_function(
  st: Agent,
  fn_proto: Handle,
  fn_ctor: Handle,
) -> #(BuiltinPair, Agent) {
  init_function_intrinsic(
    st,
    "AsyncFunction",
    GeneratorN(AsyncFunctionCtor),
    fn_proto,
    fn_ctor,
    None,
  )
}

/// Shared core of `init` / `init_async` / `init_async_function` — port of arc
/// `common.gleam:113-193 init_function_intrinsic`. Builds a dynamic
/// constructor + the fn_proto that FUNCTION objects use as [[Prototype]]:
///   ctor.[[Prototype]] = %Function%; ctor.prototype = fn_proto {W:F,E:F,C:F}
///   fn_proto.[[Prototype]] = Function.prototype
///   fn_proto.constructor = ctor {W:F,E:F,C:T}; @@toStringTag = name
/// `Some(gp)` additionally: fn_proto.prototype = gp {W:F,E:F,C:T} and
/// gp.constructor is backpatched to fn_proto (§27.5.1.1 / §27.6.1.1).
fn init_function_intrinsic(
  st: Agent,
  name: String,
  ctor_tag: rt_types.NativeToken,
  fn_proto: Handle,
  fn_ctor: Handle,
  generator_proto: option.Option(Handle),
) -> #(BuiltinPair, Agent) {
  // Reserve fn_proto address so ctor can point at it.
  let #(gfn_proto, st) = common.alloc_proto(st, Some(fn_proto), dict.new())
  // Constructor: [[Prototype]] = %Function%, prototype = gfn_proto.
  let #(len_p, st) = common.fn_length_property(st, 1)
  let #(name_p, st) = common.fn_name_property(st, name)
  let #(proto_p, st) = common.fn_prototype_property(st, gfn_proto)
  let #(ctor_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: KNative(tag: ctor_tag, name:, length: 1, constructible: True),
        proto: Some(fn_ctor),
        props: common.named_props([
          #("length", len_p),
          #("name", name_p),
          #("prototype", proto_p),
        ]),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = rt_store.t_pin_root(st, ctor_h)
  // fn_proto body: constructor + optional prototype + @@toStringTag.
  let #(ctor_prop, st) = common.data_prop(st, mk_object(ctor_h))
  let ctor_prop = common.configurable(ctor_prop)
  let #(proto_props, st) = case generator_proto {
    Some(gp) -> {
      let #(gp_prop, st) = common.data_prop(st, mk_object(gp))
      #(
        [
          #("constructor", ctor_prop),
          #("prototype", common.configurable(gp_prop)),
        ],
        st,
      )
    }
    None -> #([#("constructor", ctor_prop)], st)
  }
  let #(tag_pair, st) = common.to_string_tag(st, name)
  let st =
    rt_store.t_cell_update(st, gfn_proto, fn(slot) {
      let assert SObject(..) = slot
      SObject(..slot, props: common.named_props(proto_props), symbol_props: [
        tag_pair,
      ])
    })
  // §27.5.1.1 / §27.6.1.1: gp.constructor = fn_proto object {W:F,E:F,C:T}.
  let st = case generator_proto {
    Some(gp) -> {
      let #(bp, st) = common.data_prop(st, mk_object(gfn_proto))
      common.add_named_property(st, gp, "constructor", common.configurable(bp))
    }
    None -> st
  }
  #(BuiltinPair(prototype: gfn_proto, constructor: ctor_h), st)
}

// ── dispatch ────────────────────────────────────────────────────────────────

/// Route a `GeneratorNative` token to its body. `next/return/throw` delegate
/// to `rt_async.t_gen_*` (sync) / `t_asyncgen_*` (async).
pub fn dispatch(
  st: Agent,
  n: GeneratorNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let arg = first_arg_or_undefined(args)
  case n {
    GeneratorNext -> {
      let #(h, st) = rt_async.t_gen_next(st, require_gen(st, this), arg)
      #(mk_object(h), st)
    }
    GeneratorReturn -> {
      let #(h, st) = rt_async.t_gen_return(st, require_gen(st, this), arg)
      #(mk_object(h), st)
    }
    GeneratorThrow -> {
      let #(h, st) = rt_async.t_gen_throw(st, require_gen(st, this), arg)
      #(mk_object(h), st)
    }
    AsyncGeneratorNext -> {
      let #(h, st) = rt_async.t_asyncgen_next(st, this, arg)
      #(mk_object(h), st)
    }
    AsyncGeneratorReturn -> {
      let #(h, st) = rt_async.t_asyncgen_return(st, this, arg)
      #(mk_object(h), st)
    }
    AsyncGeneratorThrow -> {
      let #(h, st) = rt_async.t_asyncgen_throw(st, this, arg)
      #(mk_object(h), st)
    }
    // Dynamic constructors — reachable only as `(function*(){}).constructor(...)`.
    // arc parity: throw until M19 wires eval (§20.2.1.1 CreateDynamicFunction).
    GeneratorFunctionCtor | AsyncGeneratorFunctionCtor | AsyncFunctionCtor ->
      throw_type_error(st, "dynamic function creation not supported")
  }
}

/// §27.5.1.2 GeneratorValidate brand-check on `this` — must be a Handle. The
/// `SGenerator` cell check itself lives in `rt_async.require_generator`
/// (raises "not a generator object" on mismatch).
fn require_gen(st: Agent, this: JsVal) -> Handle {
  case classify(this) {
    KHandle(h) -> h
    _ ->
      throw_type_error(
        st,
        "Generator.prototype method called on incompatible receiver",
      )
  }
}

fn throw_type_error(st: Agent, msg: String) -> a {
  let js = st.store
  let #(e, st) = js.ops.new_error(st, TypeErr, msg)
  rt_store.t_throw(st, e)
}
