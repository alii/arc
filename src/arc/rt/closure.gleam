import arc/bytecode/key.{Named}
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/obj.{constructor_props, prototype_seq} as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FnFlags, type Handle, BirthPending, DataProperty, FnFlags,
  KBytecode, KHandle, NoElements, Ordinary, SObject, StringKey, classify,
  mk_object,
}
import gleam/dict
import gleam/option.{None, Some}

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

// does not root the result
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
