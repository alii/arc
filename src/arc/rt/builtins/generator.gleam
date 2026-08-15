//// `rt_builtins/generator` — %Generator% / %AsyncGenerator% /
//// %GeneratorFunction% / %AsyncGeneratorFunction% / %AsyncFunction% intrinsics
//// (SPEC §7.M6 builtin-control).
////
//// `next`/`return`/`throw` bodies live in `rt_async` (`t_gen_next` /
//// `t_gen_return` / `t_gen_throw` over the data cell `generator_data`
//// brand-checks out of `this`, and `t_asyncgen_*`) — this module only
//// installs the prototype method objects and routes dispatch.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).**

import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/function as b_function
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type GeneratorNative, type Handle, type JsVal,
  AsyncFunctionCtor, AsyncGeneratorFunctionCtor, AsyncGeneratorNext,
  AsyncGeneratorReturn, AsyncGeneratorThrow, BuiltinPair, GeneratorFunctionCtor,
  GeneratorN, GeneratorNext, GeneratorReturn, GeneratorThrow, KNative,
  NoElements, SObject, mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{type Option, None, Some}

// ── init: %Generator% + %GeneratorFunction% (§27.3 / §27.5) ─────────────────

/// Set up %GeneratorPrototype% (`.next`/`.return`/`.throw`, inherits
/// %IteratorPrototype%) and the %GeneratorFunction% dynamic-constructor pair.
/// Port of arc `builtins/generator.gleam:15-40`.
pub fn init(
  st: Agent,
  iterator_proto: Handle,
  fn_proto: Handle,
  fn_ctor: Handle,
  realm: Int,
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
      GeneratorN(GeneratorFunctionCtor(realm:)),
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
  realm: Int,
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
      GeneratorN(AsyncGeneratorFunctionCtor(realm:)),
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
  realm: Int,
) -> #(BuiltinPair, Agent) {
  init_function_intrinsic(
    st,
    "AsyncFunction",
    GeneratorN(AsyncFunctionCtor(realm:)),
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
      let #(h, st) =
        rt_async.t_gen_next(st, rt_async.generator_data(st, this), arg)
      #(mk_object(h), st)
    }
    GeneratorReturn -> {
      let #(h, st) =
        rt_async.t_gen_return(st, rt_async.generator_data(st, this), arg)
      #(mk_object(h), st)
    }
    GeneratorThrow -> {
      let #(h, st) =
        rt_async.t_gen_throw(st, rt_async.generator_data(st, this), arg)
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
    // §27.3.1.1 GeneratorFunction / §27.7.1.1 AsyncFunction / §27.4.1.1
    // AsyncGeneratorFunction ( ...parameterArgs, bodyArg ) under [[Call]]:
    // CreateDynamicFunction with their own kind and NewTarget undefined.
    GeneratorFunctionCtor(realm:)
    | AsyncFunctionCtor(realm:)
    | AsyncGeneratorFunctionCtor(realm:) -> {
      let assert Some(kind) = constructor_kind(n)
      b_function.create_dynamic_function(st, realm, args, kind, mk_undefined())
    }
  }
}

/// [[Construct]] of %GeneratorFunction% / %AsyncFunction% /
/// %AsyncGeneratorFunction% with the original `new.target`.
pub fn dispatch_construct(
  st: Agent,
  n: GeneratorNative,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  case n, constructor_kind(n) {
    GeneratorFunctionCtor(realm:), Some(kind)
    | AsyncFunctionCtor(realm:), Some(kind)
    | AsyncGeneratorFunctionCtor(realm:), Some(kind)
    -> b_function.create_dynamic_function(st, realm, args, kind, new_target)
    _, _ -> rt_val.t_throw_type_error(st, "not a constructor")
  }
}

/// The CreateDynamicFunction kind of a constructor token; `None` for the
/// prototype methods.
fn constructor_kind(
  n: GeneratorNative,
) -> Option(b_function.DynamicFunctionKind) {
  case n {
    GeneratorFunctionCtor(_) -> Some(b_function.DynamicGenerator)
    AsyncFunctionCtor(_) -> Some(b_function.DynamicAsync)
    AsyncGeneratorFunctionCtor(_) -> Some(b_function.DynamicAsyncGenerator)
    GeneratorNext
    | GeneratorReturn
    | GeneratorThrow
    | AsyncGeneratorNext
    | AsyncGeneratorReturn
    | AsyncGeneratorThrow -> None
  }
}
