//// Allocation of interpreted function objects: the `KBytecode` twin of
//// `rt/call.t_new_function`.

import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/obj.{constructor_props, prototype_seq} as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FnFlags, type Handle, BirthPending, DataProperty, FnFlags,
  KBytecode, KHandle, Named, NoElements, Ordinary, SObject, StringKey, classify,
  mk_object,
}
import gleam/dict
import gleam/option.{None, Some}

/// The `FnFlags` a closure over `template` carries. `is_method` is inferred:
/// the only non-arrow, non-constructible, non-coroutine ECMAScript function
/// objects are concise methods and accessors.
pub fn template_flags(template: FuncTemplate) -> FnFlags {
  FnFlags(
    is_constructor: template.is_constructor,
    is_class_constructor: template.is_class_constructor,
    is_derived_constructor: template.is_derived_constructor,
    is_arrow: template.is_arrow,
    is_method: !template.is_arrow
      && !template.is_constructor
      && !template.is_generator
      && !template.is_async,
    is_generator: template.is_generator,
    is_async: template.is_async,
    is_strict: template.is_strict,
  )
}

/// Allocate the function object for `template` closed over `env` (the values
/// captured per `template.env_descriptors`, already packed), created by code
/// of parse `unit`. Shared by the `MakeClosure` opcode and module link-time
/// hoisting. Does NOT root.
///
/// The cell is `KBytecode` with own `length` then `name` (§10.2.9
/// SetFunctionLength precedes §10.2.10 SetFunctionName; named keys enumerate
/// in creation order). [[Prototype]] follows the kind: §27.3.3
/// %GeneratorFunction.prototype%, §27.4.3 %AsyncGeneratorFunction.prototype%,
/// §27.7.3 %AsyncFunction.prototype%, else %Function.prototype%.
///
/// Only constructible functions and (async) generators get an own
/// `prototype` (§10.2.5 MakeConstructor / §27.3.3); arrows, methods,
/// accessors and async functions have none. A plain constructor's, a fresh
/// object inheriting %Object.prototype% with `constructor` → f
/// {W:T,E:F,C:T} under `prototype` {W:T,E:F,C:F}, is left pending along
/// with `length` and `name` (`FnBirth`): `rt_obj` settles them the first
/// time anything could observe them, so this hottest allocation path in
/// the interpreter is one bare cell. A class constructor's `prototype` is
/// allocated here, the same but non-writable (§15.7.14 step 16), and also
/// becomes [[HomeObject]], so `super.x` inside the constructor resolves
/// against the parent prototype; `MakeMethod`/`DefineMethod` re-home
/// concise methods afterwards. A generator's inherits %GeneratorPrototype%
/// / %AsyncGeneratorPrototype% and has no `constructor` (§27.3.3.1). Those
/// two are built directly rather than through [[DefineOwnProperty]] and
/// minted together with the function in one store write, `constructor`
/// back-pointer included.
pub fn t_new_bytecode_function(
  st: Agent,
  template: FuncTemplate,
  env: EnvTuple,
  unit: Int,
) -> #(Handle, Agent) {
  let flags = template_flags(template)
  let realm = st.realm
  case flags.is_generator, flags.is_class_constructor {
    False, False -> {
      let #(fn_proto, prototype_parent) = case
        flags.is_constructor,
        flags.is_async
      {
        True, _ -> #(realm.function.prototype, Some(realm.object.prototype))
        False, True -> #(realm.async_fn.prototype, None)
        False, False -> #(realm.function.prototype, None)
      }
      rt_store.t_cell_new(
        st,
        SObject(
          kind: KBytecode(
            template:,
            env:,
            home_object: None,
            flags:,
            fields_init: None,
            realm: realm.id,
            unit:,
            birth: BirthPending(prototype_parent),
          ),
          proto: Some(fn_proto),
          props: dict.new(),
          symbol_props: [],
          elements: NoElements,
          extensible: True,
        ),
      )
    }
    _, _ -> new_with_prototype(st, template, env, unit, flags)
  }
}

/// `t_new_bytecode_function` for a class constructor or (async) generator:
/// the function and its eager `prototype` object, minted as a pair.
fn new_with_prototype(
  st: Agent,
  template: FuncTemplate,
  env: EnvTuple,
  unit: Int,
  flags: FnFlags,
) -> #(Handle, Agent) {
  let realm = st.realm
  let #(fn_proto, proto_parent, proto_props) = case
    flags.is_generator,
    flags.is_async
  {
    True, True -> #(
      async_generator_fn_prototype(st),
      realm.async_gen.prototype,
      fn(_) { dict.new() },
    )
    True, False -> #(
      realm.generator_fn.prototype,
      realm.generator.prototype,
      fn(_) { dict.new() },
    )
    False, _ -> #(
      realm.function.prototype,
      realm.object.prototype,
      constructor_props,
    )
  }
  let #(h, _, st) = {
    use h, proto <- rt_store.t_cell_new_pair(st)
    let prototype_prop =
      DataProperty(
        value: mk_object(proto),
        writable: !flags.is_class_constructor,
        enumerable: False,
        configurable: False,
        seq: prototype_seq,
      )
    #(
      SObject(
        kind: KBytecode(
          template:,
          env:,
          home_object: Some(proto),
          flags:,
          fields_init: None,
          realm: realm.id,
          unit:,
          birth: BirthPending(None),
        ),
        proto: Some(fn_proto),
        props: dict.from_list([#(Named("prototype"), prototype_prop)]),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
      SObject(
        kind: Ordinary,
        proto: Some(proto_parent),
        props: proto_props(h),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  }
  #(h, st)
}

/// %AsyncGeneratorFunction.prototype%: the realm record keeps only the
/// %AsyncGeneratorFunction% constructor, whose own `prototype` is
/// {W:F, C:F} and so always names the intrinsic.
fn async_generator_fn_prototype(st: Agent) -> Handle {
  let realm = st.realm
  case
    rt_obj.t_ordinary_own_property(
      st,
      realm.async_gen.constructor,
      StringKey(Named("prototype")),
    )
  {
    Some(DataProperty(value:, ..)) ->
      case classify(value) {
        KHandle(p) -> p
        _ -> realm.function.prototype
      }
    _ -> realm.function.prototype
  }
}
