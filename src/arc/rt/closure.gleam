import arc/bytecode/names
import arc/rt/bytecode.{type EnvTuple, type FnFlags, type FuncTemplate}
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, BirthPending, BytecodeFn, DataProperty, Ordinary,
  mk_object,
}
import gleam/dict
import gleam/option.{None, Some}

// does not root the result
pub fn new_bytecode_function(
  st: Agent,
  template: FuncTemplate,
  env: EnvTuple,
  unit_id: Int,
) -> #(Handle, Agent) {
  let flags =
    bytecode.flags(
      is_constructor: template.is_constructor,
      is_class_constructor: template.is_class_constructor,
      is_derived_constructor: template.is_derived_constructor,
      is_arrow: template.is_arrow,
      is_generator: template.is_generator,
      is_async: template.is_async,
      is_strict: template.is_strict,
    )
  let realm = st.realm
  case flags.is_generator, flags.is_class_constructor {
    False, False -> {
      let shared = realm.fn_shared
      let #(proto, birth) = case flags.is_constructor, flags.is_async {
        True, _ -> #(shared.proto, shared.constructor_birth)
        False, True -> #(shared.async_proto, BirthPending(None))
        False, False -> #(shared.proto, BirthPending(None))
      }
      let kind =
        BytecodeFn(
          template:,
          env:,
          home_object: None,
          flags:,
          fields_init: None,
          realm: realm.id,
          unit_id:,
          birth:,
        )
      rt_store.cell_new(
        st,
        types.SObject(
          kind: kind,
          proto:,
          props: dict.new(),
          symbol_props: [],
          elements: types.NoElements,
          extensible: True,
        ),
      )
    }
    _, _ -> new_with_eager_prototype(st, template, env, unit_id, flags)
  }
}

fn new_with_eager_prototype(
  st: Agent,
  template: FuncTemplate,
  env: EnvTuple,
  unit_id: Int,
  flags: FnFlags,
) -> #(Handle, Agent) {
  let realm = st.realm
  let #(fn_proto, proto_parent, proto_props) = case
    flags.is_generator,
    flags.is_async
  {
    True, True -> #(
      rt_call.async_generator_fn_prototype(st, st.realm),
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
      rt_obj.constructor_props,
    )
  }
  let #(h, _, st) = {
    use h, proto <- rt_store.cell_new_pair(st)
    let prototype_prop =
      DataProperty(
        value: mk_object(proto),
        writable: !flags.is_class_constructor,
        enumerable: False,
        configurable: False,
        seq: rt_obj.prototype_seq,
      )
    let kind =
      BytecodeFn(
        template:,
        env:,
        home_object: Some(proto),
        flags:,
        fields_init: None,
        realm: realm.id,
        unit_id:,
        birth: BirthPending(None),
      )
    let props = dict.from_list([#(names.prototype, prototype_prop)])
    #(
      types.SObject(
        kind: kind,
        proto: Some(fn_proto),
        props: props,
        symbol_props: [],
        elements: types.NoElements,
        extensible: True,
      ),
      types.SObject(
        kind: Ordinary,
        proto: Some(proto_parent),
        props: proto_props(h),
        symbol_props: [],
        elements: types.NoElements,
        extensible: True,
      ),
    )
  }
  #(h, st)
}
