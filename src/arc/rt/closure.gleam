//// Allocation of interpreted function objects: the `KBytecode` twin of
//// `rt/call.t_new_function`. Port of arc `interpreter.make_closure`
//// (`vm/exec/interpreter.gleam:576-709`) minus the lazy-prototype autoinit:
//// the `prototype` object is allocated eagerly.

import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FnFlags, type Handle, type JsVal, type Property, DataProperty,
  FnFlags, JInt, KBytecode, KHandle, Named, NoElements, SObject, StringKey,
  classify, mk_number, mk_object, mk_string,
}
import gleam/bool
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
/// captured per `template.env_descriptors`, already packed). Shared by the
/// `MakeClosure` opcode and module link-time hoisting. Does NOT root.
///
/// The cell is `KBytecode` with own `length` then `name` (§10.2.9
/// SetFunctionLength precedes §10.2.10 SetFunctionName; named keys enumerate
/// in creation order). [[Prototype]] follows the kind: §27.3.3
/// %GeneratorFunction.prototype%, §27.4.3 %AsyncGeneratorFunction.prototype%,
/// §27.7.3 %AsyncFunction.prototype%, else %Function.prototype%.
///
/// Only constructible functions and (async) generators get an own
/// `prototype` (§10.2.5 MakeConstructor / §27.3.3); arrows, methods,
/// accessors and async functions have none. A plain constructor's has the
/// `rt_call.t_make_constructor` shape: fresh object inheriting
/// %Object.prototype% with `constructor` → f {W:T,E:F,C:T}, and `prototype`
/// {W:T,E:F,C:F}. A class constructor's is the same but non-writable
/// (§15.7.14 step 16). A generator's inherits %GeneratorPrototype% /
/// %AsyncGeneratorPrototype% and has no `constructor` (§27.3.3.1). Whenever a
/// `prototype` object exists it also becomes [[HomeObject]], so `super.x`
/// inside a class constructor resolves against the parent prototype;
/// `MakeMethod`/`DefineMethod` re-home concise methods afterwards.
pub fn t_new_bytecode_function(
  st: Agent,
  template: FuncTemplate,
  env: EnvTuple,
) -> #(Handle, Agent) {
  let flags = template_flags(template)
  let realm = st.realm
  let fn_proto = case flags.is_generator, flags.is_async {
    True, False -> realm.generator_fn.prototype
    True, True -> async_generator_fn_prototype(st)
    False, True -> realm.async_fn.prototype
    False, False -> realm.function.prototype
  }
  let #(length_prop, st) = fn_own_prop(st, mk_number(JInt(template.length)))
  let #(name_prop, st) =
    fn_own_prop(st, mk_string(option.unwrap(template.name, "")))
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: KBytecode(
          template:,
          env:,
          home_object: None,
          flags:,
          fields_init: None,
        ),
        proto: Some(fn_proto),
        props: dict.from_list([
          #(Named("length"), length_prop),
          #(Named("name"), name_prop),
        ]),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  use <- bool.guard(!flags.is_constructor && !flags.is_generator, #(h, st))
  let proto_parent = case flags.is_generator, flags.is_async {
    True, True -> realm.async_gen.prototype
    True, False -> realm.generator.prototype
    False, _ -> realm.object.prototype
  }
  let #(proto, st) = rt_obj.t_new_object(st, Some(proto_parent))
  let f = mk_object(h)
  let st = case flags.is_constructor {
    False -> st
    True -> {
      let #(_, st) =
        rt_obj.t_define_own_data(
          st,
          proto,
          StringKey(Named("constructor")),
          f,
          True,
          False,
          True,
        )
      st
    }
  }
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      h,
      StringKey(Named("prototype")),
      mk_object(proto),
      !flags.is_class_constructor,
      False,
      False,
    )
  let st =
    rt_store.t_cell_update(st, h, fn(slot) {
      case slot {
        SObject(kind: KBytecode(..) as k, ..) ->
          SObject(..slot, kind: KBytecode(..k, home_object: Some(proto)))
        _ -> slot
      }
    })
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

/// A §20.2.4 `length`/`name` own property: {W:F, E:F, C:T}, next seq.
fn fn_own_prop(st: Agent, value: JsVal) -> #(Property, Agent) {
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  #(
    DataProperty(
      value:,
      writable: False,
      enumerable: False,
      configurable: True,
      seq:,
    ),
    st,
  )
}
