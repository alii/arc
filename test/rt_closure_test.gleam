import arc/compiler
import arc/internal/tuple_array
import arc/parser
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/closure as rt_closure
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, BirthPending, BirthSettled, DataProperty, JInt,
  KBytecode, KHandle, KNum, KStr, Named, SObject, StringKey, classify,
}
import gleam/list
import gleam/option.{None, Some}
import rt_helpers

fn compile(source: String) -> FuncTemplate {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
  let assert Ok(template) = compiler.compile(body, sb)
  template
}

fn find(t: FuncTemplate, name: String) -> Result(FuncTemplate, Nil) {
  list.find_map(tuple_array.to_list(t.functions), fn(c: FuncTemplate) {
    case c.name == Some(name) {
      True -> Ok(c)
      False -> find(c, name)
    }
  })
}

fn make(source: String, name: String) -> #(Handle, Agent) {
  let assert Ok(t) = find(compile(source), name)
  rt_closure.t_new_bytecode_function(
    rt_helpers.agent(),
    t,
    bytecode.env_from_list([]),
    0,
  )
}

fn own_handle(st: Agent, h: Handle, key: String) -> Result(Handle, Nil) {
  case rt_obj.t_ordinary_own_property(st, h, StringKey(Named(key))) {
    Some(DataProperty(value:, ..)) ->
      case classify(value) {
        KHandle(p) -> Ok(p)
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn proto_of(st: Agent, h: Handle) -> Handle {
  let assert #(Some(p), _) = rt_obj.t_get_prototype_of(st, h)
  p
}

pub fn plain_function_shape_test() {
  let #(f, st) = make("function Foo(a, b) {}", "Foo")
  assert proto_of(st, f) == st.realm.function.prototype
  let assert SObject(kind: KBytecode(birth: BirthPending(Some(parent)), ..), ..) =
    rt_store.t_cell_get(st, f)
  assert parent == st.realm.object.prototype
  assert own_handle(st, f, "prototype") == Error(Nil)
  let #(keys, st) = rt_obj.t_own_keys(st, f)
  assert keys
    == [
      StringKey(Named("length")),
      StringKey(Named("name")),
      StringKey(Named("prototype")),
    ]
  let assert Some(DataProperty(value: len, writable: False, ..)) =
    rt_obj.t_ordinary_own_property(st, f, StringKey(Named("length")))
  assert classify(len) == KNum(JInt(2))
  let assert Some(DataProperty(value: name, ..)) =
    rt_obj.t_ordinary_own_property(st, f, StringKey(Named("name")))
  assert classify(name) == KStr("Foo")
  let assert Some(DataProperty(writable: True, configurable: False, ..)) =
    rt_obj.t_ordinary_own_property(st, f, StringKey(Named("prototype")))
  let assert Ok(proto) = own_handle(st, f, "prototype")
  assert proto_of(st, proto) == st.realm.object.prototype
  assert own_handle(st, proto, "constructor") == Ok(f)
  let assert SObject(
    kind: KBytecode(home_object: None, birth: BirthSettled, flags:, ..),
    ..,
  ) = rt_store.t_cell_get(st, f)
  assert flags.is_constructor && !flags.is_class_constructor
}

pub fn birth_props_precede_later_props_test() {
  let #(f, st) = make("function Foo(a, b) {}", "Foo")
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      f,
      StringKey(Named("sooner")),
      types.mk_undefined(),
      True,
      True,
      True,
    )
  let #(prototype, st) =
    rt_obj.t_get_prop(st, types.mk_object(f), StringKey(Named("prototype")))
  let assert KHandle(proto) = classify(prototype)
  let assert Some(DataProperty(
    writable: True,
    enumerable: False,
    configurable: True,
    ..,
  )) =
    rt_obj.t_ordinary_own_property(st, proto, StringKey(Named("constructor")))
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      f,
      StringKey(Named("later")),
      types.mk_undefined(),
      True,
      True,
      True,
    )
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      proto,
      StringKey(Named("method")),
      types.mk_undefined(),
      True,
      True,
      True,
    )
  let #(keys, st) = rt_obj.t_own_keys(st, f)
  assert keys
    == [
      StringKey(Named("length")),
      StringKey(Named("name")),
      StringKey(Named("prototype")),
      StringKey(Named("sooner")),
      StringKey(Named("later")),
    ]
  let #(pkeys, st) = rt_obj.t_own_keys(st, proto)
  assert pkeys == [StringKey(Named("constructor")), StringKey(Named("method"))]
  let assert Ok(t) = find(compile("function Bar() {}"), "Bar")
  let #(g, st) =
    rt_closure.t_new_bytecode_function(st, t, bytecode.env_from_list([]), 0)
  let #(_, st) =
    rt_obj.t_define_own_data(
      st,
      g,
      StringKey(Named("z")),
      types.mk_undefined(),
      True,
      True,
      True,
    )
  let #(gkeys, _) = rt_obj.t_own_keys(st, g)
  assert gkeys
    == [
      StringKey(Named("length")),
      StringKey(Named("name")),
      StringKey(Named("prototype")),
      StringKey(Named("z")),
    ]
}

pub fn class_constructor_prototype_not_writable_test() {
  let #(c, st) = make("class C { constructor() {} }", "C")
  let assert Some(DataProperty(writable: False, configurable: False, ..)) =
    rt_obj.t_ordinary_own_property(st, c, StringKey(Named("prototype")))
  let assert Ok(proto) = own_handle(st, c, "prototype")
  assert own_handle(st, proto, "constructor") == Ok(c)
  let assert SObject(kind: KBytecode(home_object: Some(home), ..), ..) =
    rt_store.t_cell_get(st, c)
  assert home == proto
}

pub fn generator_function_shape_test() {
  let #(g, st) = make("function* gen() {}", "gen")
  assert proto_of(st, g) == st.realm.generator_fn.prototype
  let assert Some(DataProperty(writable: True, ..)) =
    rt_obj.t_ordinary_own_property(st, g, StringKey(Named("prototype")))
  let assert Ok(proto) = own_handle(st, g, "prototype")
  assert proto_of(st, proto) == st.realm.generator.prototype
  assert own_handle(st, proto, "constructor") == Error(Nil)
}

pub fn async_generator_function_shape_test() {
  let #(g, st) = make("async function* ag() {}", "ag")
  let assert Ok(agen_fn_proto) =
    own_handle(st, st.realm.async_gen.constructor, "prototype")
  assert proto_of(st, g) == agen_fn_proto
  let assert Ok(proto) = own_handle(st, g, "prototype")
  assert proto_of(st, proto) == st.realm.async_gen.prototype
  assert own_handle(st, proto, "constructor") == Error(Nil)
}

pub fn async_function_has_no_prototype_test() {
  let #(a, st) = make("async function af() {}", "af")
  assert proto_of(st, a) == st.realm.async_fn.prototype
  assert rt_obj.t_ordinary_own_property(st, a, StringKey(Named("prototype")))
    == None
  let assert SObject(kind: KBytecode(home_object: None, ..), ..) =
    rt_store.t_cell_get(st, a)
}

pub fn arrow_and_method_have_no_prototype_test() {
  let #(arrow, st) = make("var arrow = () => 1;", "arrow")
  assert rt_obj.t_ordinary_own_property(
      st,
      arrow,
      StringKey(Named("prototype")),
    )
    == None
  let assert SObject(kind: KBytecode(flags: af, ..), ..) =
    rt_store.t_cell_get(st, arrow)
  assert af.is_arrow && !af.is_method
  let #(m, st) = make("var o = { m() {} };", "m")
  assert rt_obj.t_ordinary_own_property(st, m, StringKey(Named("prototype")))
    == None
  let assert SObject(kind: KBytecode(flags: mf, ..), ..) =
    rt_store.t_cell_get(st, m)
  assert mf.is_method && !mf.is_constructor
}
