//// Allocation of interpreted function objects: the `KBytecode` twin of
//// `rt/call.t_new_function`. The `prototype` object is allocated eagerly,
//// not lazily on first read.

import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/call.{fn_own_prop}
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FnFlags, type Handle, type JsSlot, type Property,
  type PropertyKey, DataProperty, FnFlags, JInt, KBytecode, KHandle, Named,
  NoElements, Ordinary, SObject, StringKey, classify, mk_number, mk_object,
  mk_string,
}
import gleam/dict
import gleam/option.{type Option, None, Some}

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
/// accessors and async functions have none. A plain constructor's has the
/// `rt_call.t_make_constructor` shape: fresh object inheriting
/// %Object.prototype% with `constructor` → f {W:T,E:F,C:T}, and `prototype`
/// {W:T,E:F,C:F}. A class constructor's is the same but non-writable
/// (§15.7.14 step 16). A generator's inherits %GeneratorPrototype% /
/// %AsyncGeneratorPrototype% and has no `constructor` (§27.3.3.1). Whenever a
/// `prototype` object exists it also becomes [[HomeObject]], so `super.x`
/// inside a class constructor resolves against the parent prototype;
/// `MakeMethod`/`DefineMethod` re-home concise methods afterwards.
///
/// This is the hottest allocation path in the interpreter, so both objects
/// are built directly rather than through [[DefineOwnProperty]], in one
/// store write: it reserves every birth-prop seq and mints the function cell
/// together with its complete `prototype` object, `constructor` back-pointer
/// included.
pub fn t_new_bytecode_function(
  st: Agent,
  template: FuncTemplate,
  env: EnvTuple,
  unit: Int,
) -> #(Handle, Agent) {
  let flags = template_flags(template)
  let realm = st.realm
  let fn_proto = case flags.is_generator, flags.is_async {
    True, False -> realm.generator_fn.prototype
    True, True -> async_generator_fn_prototype(st)
    False, True -> realm.async_fn.prototype
    False, False -> realm.function.prototype
  }
  let birth_props = fn(seq) {
    [
      #(Named("length"), fn_own_prop(mk_number(JInt(template.length)), seq)),
      #(
        Named("name"),
        fn_own_prop(mk_string(option.unwrap(template.name, "")), seq + 1),
      ),
    ]
  }
  let new_fn = fn(home_object, props) {
    fn_slot(realm.id, unit, template, env, flags, fn_proto, home_object, props)
  }
  case flags.is_constructor || flags.is_generator {
    False -> {
      use seq <- rt_store.t_cell_new_with(st, 2)
      new_fn(None, birth_props(seq))
    }
    True -> {
      let proto_parent = case flags.is_generator, flags.is_async {
        True, True -> realm.async_gen.prototype
        True, False -> realm.generator.prototype
        False, _ -> realm.object.prototype
      }
      let seqs = case flags.is_constructor {
        True -> 4
        False -> 3
      }
      let #(h, _, st) = {
        use seq, h, proto <- rt_store.t_cell_new_pair(st, seqs)
        let prototype_prop =
          DataProperty(
            value: mk_object(proto),
            writable: !flags.is_class_constructor,
            enumerable: False,
            configurable: False,
            seq: seq + 2,
          )
        let proto_props = case flags.is_constructor {
          False -> dict.new()
          True ->
            dict.from_list([
              #(
                Named("constructor"),
                DataProperty(
                  value: mk_object(h),
                  writable: True,
                  enumerable: False,
                  configurable: True,
                  seq: seq + 3,
                ),
              ),
            ])
        }
        #(
          new_fn(Some(proto), [
            #(Named("prototype"), prototype_prop),
            ..birth_props(seq)
          ]),
          SObject(
            kind: Ordinary,
            proto: Some(proto_parent),
            props: proto_props,
            symbol_props: [],
            elements: NoElements,
            extensible: True,
          ),
        )
      }
      #(h, st)
    }
  }
}

fn fn_slot(
  realm: Int,
  unit: Int,
  template: FuncTemplate,
  env: EnvTuple,
  flags: FnFlags,
  fn_proto: Handle,
  home_object: Option(Handle),
  props: List(#(PropertyKey, Property)),
) -> JsSlot {
  SObject(
    kind: KBytecode(
      template:,
      env:,
      home_object:,
      flags:,
      fields_init: None,
      realm:,
      unit:,
    ),
    proto: Some(fn_proto),
    props: dict.from_list(props),
    symbol_props: [],
    elements: NoElements,
    extensible: True,
  )
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
